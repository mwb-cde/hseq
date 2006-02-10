(*-----
   Name: rewrite.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

open Basic
open Term
open Result

(***
 * Rewrite Rules
 ***)


(** Rule ordering *)
type order = (Basic.term -> Basic.term -> bool)

(** Rewrite rules *)
type rule = 
    Rule of term
  | Ordered of (term * order)

(* Rule constructors *)
let rule t = Rule t
let orule t r = Ordered(t, r)

(* Rule destructors *)
let term_of r =
  match r with
    Rule t -> t
  | Ordered (t, _) -> t

let order_of r =
  match r with
    Rule _ -> raise (Failure "Not an ordered rule")
  | Ordered (_, p) -> p

(*** Rewrite control ***)

(** Direction *)

type direction = LeftRight | RightLeft 
let leftright=LeftRight
let rightleft=RightLeft

(** Strategy *)

type strategy = TopDown | BottomUp 
let topdown = TopDown
let bottomup = BottomUp

let is_topdown t = 
  match t with TopDown -> true  | _ -> false

let is_bottonup t = 
  match t with BottomUp -> true  | _ -> false

(*** Control ***)

type control =
    { 
      depth: int option; (** (Some i): maximum number of times to rewrite is i,
			    None : unlimited rewriting (default) *)
      rr_dir: direction;
      rr_strat: strategy
    }

(** Construct a control *)
let control ~dir ~strat ~max = 
  { depth=max; rr_dir=dir; rr_strat=strat }

(** The default control *)
let default_control= 
  control 
    ~strat:TopDown
    ~dir:leftright
    ~max:None

(***
 * Rewrite Engine
 ***)

let limit_reached d = 
  match d with Some 0 -> true | _ -> false

let decr_depth ctrl = 
  match ctrl.depth with 
    None -> ctrl 
  | Some x -> 
      {ctrl with depth=Lib.set_int_option(x-1)}

let is_free_binder qs t= 
  (match t with
    Bound(q) -> List.exists (fun x ->  x == q) qs
  |	_ -> false)

(*** Internal represenation of rewrite rules ***)

type rewrite_rules = 
    (Basic.binders list * Basic.term * Basic.term * order option)

(**
   [dest_lr_rule r]: Destruct for left to right rewriting.

   Break rule [t= !x1..xn: lhs = rhs] 
   into quantifiers [x1..xn], [lhs] and [rhs].

   return ([x1..xn], [lhs], [rhs], [p]).
   where [p] is [None] if [r=Rule t] and [Some x if r= Order(t, x)]
 *)
let dest_lr_rule  r= 
  let dest_term x p=
    let qs, b = strip_qnt Basic.All x
    in 
    let lhs, rhs= Logicterm.dest_equality b
    in 
    (qs, lhs, rhs, p)
  in
  match r with
    Rule(t) -> dest_term t None
  | Ordered(t, x) -> dest_term t (Some x)

(**
   [dest_rl_term t]: Destruct for right to left rewriting.
   Break term [t= !x1..xn: lhs = rhs] 
   into quantifiers [x1..xn], [lhs] and [rhs].
   return ([x1..xn], [rhs], [lhs]).
 *)
let dest_rl_rule r = 
  let dest_term x p=
    let qs, b = strip_qnt Basic.All x
    in 
    let lhs, rhs= Logicterm.dest_equality b
    in 
    (qs, rhs, lhs, p)
  in 
  match r with
    Rule(t) -> dest_term t None
  | Ordered(t, x) -> dest_term t (Some x)


(** make_rewrites: convert list of equalities to db of rewrites *)

let make_rewrites xs = 
  let rec make_rewrites_aux xs net=
    match xs with
      [] -> net
    | ((vs, key, rep, order)::rst) -> make_rewrites_aux rst
	  (Net.add (is_free_binder vs) net key (vs, key, rep, order))
  in 
  make_rewrites_aux (List.rev xs) (Net.empty())

type rewriteDB = 
    Net_rr of rewrite_rules Net.net 
  | List_rr of rewrite_rules list 

(*** Matching functions. ***)

(** 
   [find_match scp ctrl tyenv varp lhs term env]: Try to match [lhs]
   with term in type environment [tyenv] and term environment [env].

   Return a new type environment and term environment if successful.
 *)
let find_match scope ctrl tyenv varp term1 term2 env=
  try 
    Unify.unify_fullenv_rewrite scope tyenv env varp term1 term2 
  with x -> 
    raise 
      (add_error (term_error ("Can't match terms") [term1; term2]) x)

(**
   [match_rewrite scp ctrl tyenv varp lhs rhs order trm]: Match [trm]
   with [lhs]. 

   If successful, return [rhs], the new type environment and the new
   term environment. The type and term environments are obtained by
   unifying [lhs] and [trm] and contain the bindings for unification
   variables in [rhs].
 *)
let match_rewrite scope ctrl qntenv tyenv varp lhs rhs order trm = 
  let env = Term.empty_subst ()
  in 
  try
    (let tyenv1, env1=find_match scope ctrl tyenv varp lhs trm env; 
    in 
    let nt = Term.subst_closed qntenv env1 rhs
    in 
    match order with
      None -> (nt, tyenv1)
    | Some(p) ->  
	if (p nt trm)   (* if nt < trm *)
	then (nt, tyenv1) (* accept nt *) 
	else raise (Failure "No match")) (* reject nt *)
  with x -> 
    (Term.add_term_error "match_rewrite: failed" [lhs; trm] x)

(**
   [match_rr_list scp ctrl tyenv chng rs trm]: Try to rewrite [trm]
   with the rules in [rs]. Calls [match_rewrite] with each of the
   rewrite rules. If any rule succeeds, continues with the replacement
   (rhs) term given by the rule.

   If any rule matches, [chng] is set to [true] otherwise it is unchanged.
 *)
let rec match_rr_list scope ctrl qntenv tyenv chng rs trm = 
  if (limit_reached (ctrl.depth))
  then (trm, tyenv, ctrl)
  else 
    (match rs with
      [] -> (trm, tyenv, ctrl)
    | (qs, lhs, rhs, order)::nxt ->
	let (ntrm, ntyenv), fl = 
	  (try 
	    (match_rewrite scope ctrl qntenv tyenv 
	       (is_free_binder qs) lhs rhs order trm, 
	     true)
	  with _ -> (trm, tyenv), false)
	in 
	if fl 
	then (chng:=true; (ntrm, ntyenv, decr_depth ctrl))
	else 
	  match_rr_list scope ctrl qntenv ntyenv chng nxt trm)

(**
   [match_rewrite_list scp ctrl tyenv chng net trm]: Repeatedly
   rewrite [trm] using rules stored in term-net [net] until no rule
   matches or the limit (given by control [ctrl]) is reached. Note
   that [match_rewrite_list] doesn't descend into the terms' subterms.

   If [trm] rewritten then [chng] is set to [true] otherwise it is
   unchanged.
 *)
let rec match_rewrite_list scope ctrl qntenv tyenv chng net trm =
  if(limit_reached ctrl.depth)
  then (trm, tyenv, ctrl)
  else 
    (let cn=ref false 
    in 
    let rs=
      match net with
	(Net_rr n) -> Net.lookup n trm
      | (List_rr r) -> r 
    in 
    let ntrm, ntyenv, nctrl =
      match_rr_list scope ctrl qntenv tyenv cn rs trm
    in 
    if (!cn) 
    then 
      (chng:=true; 
       match_rewrite_list scope nctrl qntenv ntyenv chng net ntrm)
    else
      (ntrm, ntyenv, nctrl))

(**
   [rewrite_list_topdown scp ctrl tyenv chng net trm]: Rewrite [trm]
   and its sub-terms, top-down. 

   First [trm] is rewritten with the rules in [net]. Each subterm of
   the resulting term is then rewritten (also top-down). Rewriting
   continues up to the limit set by [ctrl.depth].

   If [trm] or any of its subterms are rewritten, [chng] is set to
   [true] other wise it is unchanged.
 *)
let rewrite_list_topdown scope ctrl tyenv chng net trm = 
  let rec rewrite_subterm ctrl qntenv env t=
    if(limit_reached ctrl.depth)
    then (t, env, ctrl)
    else 
      (match t with
	Basic.Qnt(q, b) -> 
	  let qntenv1 = 
	    Term.bind 
	      (Basic.Bound q) (Term.mk_free "" (Gtypes.mk_null()))
	      qntenv
	  in 
	  let nb, benv, bctrl = rewrite_aux ctrl qntenv1 env b
	  in 
	  (Basic.Qnt(q, nb), benv, bctrl)
      |	Basic.App(f, a)->
	  let nf, fenv, fctrl = (rewrite_aux ctrl qntenv env f)
	  in
	  let na, aenv, actrl = (rewrite_aux fctrl qntenv fenv a)
	  in 
	  (Basic.App(nf, na), aenv, actrl)
      | Basic.Typed(tt, ty) -> rewrite_aux ctrl qntenv env tt
      | _ -> (t, env, ctrl))
  and rewrite_aux ctrl qntenv env t = 
    if (limit_reached ctrl.depth)
    then (t, env, ctrl)
    else 
      (let nt, nenv, nctrl= 
	match_rewrite_list scope ctrl qntenv env chng net t
      in 
      rewrite_subterm nctrl qntenv nenv nt)
  in 
  let qntenv = Term.empty_subst()
  in 
  rewrite_aux ctrl qntenv tyenv trm

(**
   [rewrite_list_bottomup scp ctrl tyenv chng net trm]: Rewrite [trm]
   and its sub-terms, bottom-up.

   Each subterm of [trm] is rewritten (bottom-up) with the rules in
   [net] then [trm] (after its subterms are replaced) is
   rewritten. Rewriting continues up to the limit set by [ctrl.depth].

   If [trm] or any of its subterms are rewritten, [chng] is set to
   [true] other wise it is unchanged.
 *)
let rewrite_list_bottomup scope ctrl tyenv chng net trm = 
  let rec rewrite_aux ctrl qntenv env t=
    if(limit_reached (ctrl.depth))
    then (t, env, ctrl)
    else 
      (match t with
	Basic.Qnt(q, b) -> 
	  let qntenv1 = 
	    Term.bind 
	      (Basic.Bound q) (Term.mk_free "" (Gtypes.mk_null()))
	      qntenv
	  in 
	  let nb, benv, nctrl = rewrite_aux ctrl qntenv1 env b
	  in 
	  match_rewrite_list scope nctrl qntenv benv chng net (Qnt(q, nb))
      |	Basic.App(f, a)->
	  (let nf, fenv, fctrl = 
	    (rewrite_aux ctrl qntenv env f)
	  in
	  let na, aenv, actrl= 
	    (rewrite_aux fctrl qntenv fenv a)
	  in 
	  match_rewrite_list scope actrl qntenv aenv chng net 
	    (Basic.App(nf, na)))
      | Basic.Typed(tt, ty) -> 
	  rewrite_aux ctrl qntenv env tt
      | _ -> 
	  match_rewrite_list scope ctrl qntenv env chng net t)
  in 
  let qntenv= Term.empty_subst()
  in 
  rewrite_aux ctrl qntenv tyenv trm

(** 
   [rewrite_list scp ctrl chng tyenv rs trm]: 
   Rewrite [trm] using the list of rules [rs]. 

   Converts the list of rules to a term net then passes off the
   rewriting to one of [rewrite_list_topdown] or
   [rewrite_list_bottomup] depending on the strategy set in
   [ctrl.rr_strat].

   If [trm] or any of its subterms are rewritten, [chng] is set to
   [true] otherwise it is unchanged.
 *)
let rewrite_list scope ctrl chng tyenv rs trm = 
  let nt=Net_rr(make_rewrites rs)
  in 
  let (nt, ntyenv, _) = 
    if(is_topdown (ctrl.rr_strat))
    then 
      rewrite_list_topdown scope ctrl tyenv chng nt trm
    else 
      rewrite_list_bottomup scope ctrl tyenv chng nt trm
  in (nt, ntyenv)


(***
 * Toplevel rewriting functions
 ***)

let rewrite_env scope ctrl tyenv rrl trm=
  let chng = ref false
  in 
  let rs = 
    if ctrl.rr_dir=LeftRight
    then 
      List.map dest_lr_rule rrl
    else 
      List.map dest_rl_rule rrl
  in 
  let rslt = rewrite_list scope ctrl chng tyenv rs trm
  in 
  if(!chng)
  then rslt 
  else raise (term_error "Rewriting failed" [trm])

let rewrite scope ctrl rrl trm =
  let (ret, _) = rewrite_env scope ctrl (Gtypes.empty_subst()) rrl trm
  in ret


(**
   Directed Rewriting
 *)

type orig_rule = rule

exception Quit of exn
exception Stop of exn

module Planned =
  struct

    type rewrite_rule = 
	(Basic.binders list * Basic.term * Basic.term)

(**
   Planned Rewriting

   Rewriting based on a pre-determined plan.

   Rewrites a node [n] by following the direction in a plan [p]. Plan
   [p] specifies the rules to be applied to each node and the order in
   which a node is to be rewritten.

   (For hseq, a node is a term).
 *)

(** {7 Planned rewriting specialised to terms} *)

    type term_key =
	Ident  (** Term [Id] *)
      | BVar   (** Term [Bound] *)
      | FVar   (** Term [Free] *)
      | Appln  (** Term [App] *)
      | Quant  (** Term [Qnt] *)
      | AllQ   (** Term [Qnt] ([All]) *)
      | ExQ   (** Term [Qnt] ([Ex]) *)
      | LamQ   (** Term [Qnt] ([Lambda]) *)
      | Constn (** Term [Const] *)
      | TyTerm (** Term [Typed] *)
      | AnyTerm    (** Any term *)
      | NoTerm   (** No term *)
      | Neg of term_key (** Negate a key *)
      | Alt of term_key * term_key  (** Alternative keys *)

    module TermData =
      struct
	type node = Basic.term
	type rule = rewrite_rule
	type substn = Term.substitution
	type data = 
	    (Scope.t (** Scope *)
	       * Term.substitution    (** Quantifier environment *)
	       * Gtypes.substitution)  (** Type environment *)
(*
	       * Term.substitution)    (** Substitution *) 
*)

	type key = term_key

	let key_of n = 
	  match n with
	    Basic.Id _ -> Ident
	  | Basic.Bound _ -> BVar
	  | Basic.Free _ -> FVar
	  | Basic.App _ -> Appln
	  | Basic.Const _ -> Constn
	  | Basic.Typed _ -> TyTerm
	  | Basic.Qnt(q, b) -> 
	      (match (Basic.binder_kind q) with 
		Basic.All -> AllQ
	      | Basic.Ex -> ExQ
	      | Basic.Lambda -> LamQ
	      | _ -> Quant)

	let rec is_key k n = 
	  match k with
	    AnyTerm -> true
	  | NoTerm -> false
	  | Alt(x, y) -> (is_key x n || is_key y n)
	  | Neg(x) -> not (is_key x n)
	  | Quant -> Term.is_qnt n
	  | _ -> (k = key_of n)

	let num_subnodes n =
	  match n with
	    App(l, r) -> 2
	  | Qnt(_, b) -> 1
	  | Typed(t, _) -> 1
	  | _ -> 0

	let subnodes_of n =
	  match n with
	    App(l, r) -> [l; r]
	  | Qnt(_, b) -> [b]
	  | Typed(t, _) -> [t]
	  | _ -> []

	let set_subnodes n xs =
	  match n with
	    App(_, _) -> 
	      (match xs with 
		[l; r] -> App(l, r)
	      | _ -> raise (Quit (Failure "set_subnodes: App")))
	  | Qnt(q, _) -> 
	      (match xs with 
		[b] -> Qnt(q, b)
	      | _ -> raise (Quit (Failure "set_subnodes: Qnt")))
	  | Typed(_, ty) ->
	      (match xs with 
		[t] -> Typed(t, ty)
	      | _ -> raise (Quit (Failure "set_subnodes: Typed")))
	  | _ -> raise (Quit (Failure "set_subnodes: other"))


	let get_subnode n i =
	  match n with
	    App(l, r) -> 
	      (match i with
		0 -> l 
	      | 1 -> r 
	      | _ -> raise Not_found)
	  | Qnt(_, b) -> 
	      (match i with
		0 -> b 
	      | _ -> raise Not_found)
	  | Typed(t, _) -> 
	      (match i with
		0 -> t 
	      | _ -> raise Not_found)
	  | _ -> raise Not_found

	let set_subnode n i x =
	  match n with
	    App(l, r) -> 
	      (match i with
		0 -> App(x, r) 
	      | 1 -> App(l, x) 
	      | _ -> raise Not_found)
	  | Qnt(q, b) -> 
	      (match i with
		0 ->  Qnt(q, x)
	      | _ -> raise Not_found)
	  | Typed(t, ty) -> 
	      (match i with
		0 -> Typed(x, ty)
	      | _ -> raise Not_found)
	  | _ -> raise Not_found


	let dest_rule r = r

	let matches data rule trm = 
	  let (scope, qntenv, tyenv) = data
	  in 
	  let (qs, lhs, rhs) = dest_rule rule 
	  in
	  let env = Term.empty_subst()
	  in 
	  let varp = is_free_binder qs
	  in 
	  try 
	    let tyenv1, env1 = 
	      Unify.unify_fullenv_rewrite scope tyenv env varp lhs trm
	    in 
	    ((scope, qntenv, tyenv1), env1)
	  with x ->
	    raise 
	      (Rewritekit.Quit
		 (add_error (term_error ("Can't match terms") [lhs; trm]) x))

	let subst data rule env = 
	  let (scope, qntenv, tyenv) = data
	  in 
	  let (qs, lhs, rhs) = dest_rule rule 
	  in
	  let data1 = (scope, qntenv, tyenv)
	  in 
	  (data1, Term.subst_closed qntenv env rhs)

	let add_data data trm = 
	  match trm with 
	    Qnt(q, _) -> 
	      let (scope, qntenv, tyenv) = data
	      in 
	      let qntenv1 = 
		Term.bind 
		  (Basic.Bound q) (Term.mk_free "" (Gtypes.mk_null()))
		  qntenv
	      in 
	      (scope, qntenv1, tyenv)
	  | _ -> data
		
	let drop_data (data1, trm1) (data2, trm2) =
	  match trm1 with 
	    Qnt(q, _) -> 
	      let (scope1, qntenv1, tyenv1) = data1
	      in 
	      let (scope2, qntenv2, tyenv2) = data2
	      in 
	      (scope2, qntenv1, tyenv2)
	  | _ -> data2

      end

    type data = TermData.data
    type key = TermData.key
    type rule = rewrite_rule
    type ('a)plan = (key, 'a)Rewritekit.plan

    module TermPlan=Rewritekit.Make(TermData)

(** {5 Toplevel directed rewriting functions} *)

    let rewrite = TermPlan.rewrite
	(** [rewrite data p t]: Rewrite term [t] with plan [t]. *)

	(** Plan constructors *)
    let mk_node k ps = Rewritekit.Node(k, ps)
    let mk_rules rs = Rewritekit.Rules(rs)
    let mk_subnode i p = Rewritekit.Subnode(i, p)
    let mk_branches ps = Rewritekit.Branches(ps)
    let mk_skip = Rewritekit.Skip

    let mapping = Rewritekit.mapping

    let rec pack pl = 
      match pl with
	Rewritekit.Rules rs -> pack_rules rs
      | Rewritekit.Node(k, ps) -> pack_node k ps
      | Rewritekit.Subnode(i, p) -> pack_subnode i p
      | Rewritekit.Branches(ps) -> pack_branches ps
      | Rewritekit.Skip -> Rewritekit.Skip
    and 
	pack_rules rs = 
      match rs with 
	[] -> Rewritekit.Skip
      | _ -> Rewritekit.Rules(rs)
    and 
	pack_node k ps =
      match ps with 
	[] -> Rewritekit.Skip
      | _ -> 
	  let ps1 = 
	    List.filter (fun x -> not (x=Rewritekit.Skip)) ps
	  in 
	  Rewritekit.Node(k, ps1)
    and 
	pack_branches ps = 
      match ps with
	[] -> Rewritekit.Skip
      | [Rewritekit.Skip] -> Rewritekit.Skip
      | [Rewritekit.Skip; x] -> Rewritekit.Subnode(1, x)
      | [x; Rewritekit.Skip] -> Rewritekit.Subnode(0, x)
      | [x] -> Rewritekit.Subnode(0, x)
      | _ -> Rewritekit.Branches ps
    and 
	pack_subnode i p = 
      match p with
	Rewritekit.Skip -> Rewritekit.Skip
      | _ -> Rewritekit.Subnode(i, p)
	    

	    (** Keys *)
    let key_of = TermData.key_of 

    let anyterm = AnyTerm
    let noterm = NoTerm 
    let alt_key x y = Alt(x, y)
    let neg_key x = Neg(x)
    let ident_key = Ident
    let bvar_key = BVar
    let fvar_key = FVar
    let appln_key = Appln
    let quant_key = Quant
    let allq_key = AllQ
    let exq_key = ExQ
    let lamq_key = LamQ
    let constn_key = Constn
    let tyterm_key = TyTerm

  end


let rec extract_check_rules scp dir pl = 
  let get_test t = 
    let qs, b = Term.strip_qnt Basic.All t
    in 
    let lhs, rhs = Logicterm.dest_equality b
    in 
    if dir = leftright 
    then (qs, lhs, rhs)
    else (qs, rhs, lhs)
  in 
  Planned.mapping get_test pl

let plan_rewrite_env scp ?(dir=leftright) tyenv plan f = 
  let plan1 = extract_check_rules scp dir plan
  in 
  let data = (scp, Term.empty_subst(), tyenv)
  in 
  let (data1, nt) = Planned.rewrite data plan1 f
  in 
  let (scp1, qntenv1, tyenv1) = data1
  in 
  (nt, tyenv1)


let plan_rewrite scp ?(dir=leftright) plan f = 
  let (nt, ntyenv) = 
    plan_rewrite_env scp ~dir:dir (Gtypes.empty_subst()) plan f
  in 
  nt


module type PlannerData =
  sig
    type rule
    type data
    val dest : 
	data -> rule 
      -> (Basic.binders list * Basic.term * Basic.term * order option)
  end

module type PlannerType =
  sig
(** Rewrite plan constructors.

   Two planning functions are provided. The first is for general
   rewriting. The second for rewriting w.r.t a type context.

   Both take rewrite rules as universally quantified equalities of the
   for [!v1 .. vn. lhs = rhs]. The variables [v1 .. vn] are taken as
   variables which can be instantiated by the rewriter. If the rule is
   not an equality then rewriting will fail.

   Rewriting breaks a rule [lhs=rhs] to an equality. If rewriting is
   left-right, the equality is [lhs=rhs]; if rewriting is right-left
   then the equality is [rhs=lhs]. 

   For left-right rewriting, every variable (from [v1 .. vn])
   appearing in [rhs] must also appear in [lhs] otherwise the rule
   cannot be used. Similarly for right-left rewriting.
 *)
    open Planned

    exception No_change

    type a_rule 
    type rule_data

    val make :
	rule_data
	-> Scope.t -> control -> a_rule list 
	  -> Basic.term -> (Basic.term * (a_rule)plan)
(** 
   Make a rewrite plan using a list of universally quantified rewrite
   rules.
 *)
	      
    val make_env : 
	rule_data
	-> Scope.t 
	  -> control 
	  -> Gtypes.substitution
	    -> a_rule list -> Basic.term 
	      -> (Basic.term * Gtypes.substitution * (a_rule)plan)
(**
   [make_env tyenv rules trm]: Make a rewrite plan for [trm] w.r.t type
   environment [tyenv] using [rules]. Return the new term and the type
   environment contructed during rewriting.
 *)

(** {7 Exposed for debugging} *)

    type data = 
	(Scope.t 
	   * Term.substitution  
	   * Gtypes.substitution)

    type internal_rule = 
	(Basic.binders list 
	   * Basic.term 
	   * Basic.term 
	   * order option
	   * a_rule)

    type rewrite_net = internal_rule Net.net 

    val src_of: internal_rule -> a_rule

    val match_rewrite : 
	control
      -> data
	-> internal_rule
	  -> Basic.term 
	    -> (a_rule * Basic.term * Gtypes.substitution)

    val match_rr_list:
	control
      -> data
	-> internal_rule list
	  -> Basic.term
	    -> a_rule list
	      -> (Basic.term * Gtypes.substitution
		    * control * (a_rule)list)

    val match_rewrite_list:
	control
      -> data
	-> rewrite_net
	  -> Basic.term 
	    -> a_rule list
	      -> (Basic.term * Gtypes.substitution
		    * control * (a_rule)list)

    val check_change : ('a)plan -> unit
    val check_change2 : ('a)plan -> ('a)plan -> unit

    val make_list_topdown:
	control 
      -> rewrite_net
	-> data
	  -> Basic.term
	    -> (Basic.term * Gtypes.substitution 
		  * control * (a_rule)plan)

    val make_list_bottomup:
	control 
      -> rewrite_net
	-> data
	  -> Basic.term
	    -> (Basic.term * Gtypes.substitution 
		  * control * (a_rule)plan)

    val make_rewrites: 
	rule_data -> a_rule list -> rewrite_net

    val make_list : 
	rule_data 
      -> control 
      -> Scope.t
	-> Gtypes.substitution
	  -> a_rule list
	    -> Basic.term
	      -> (Basic.term * Gtypes.substitution * (a_rule)plan)

  end

module Planner =
  functor (A: PlannerData) ->
  struct

    open Rewritekit
    open Planned

    type a_rule = A.rule
    type rule_data = A.data

    type data = 
	(Scope.t 
	   * Term.substitution  
	   * Gtypes.substitution)

    type internal_rule = 
	(Basic.binders list 
	   * Basic.term 
	   * Basic.term 
	   * order option
	   * a_rule)

    type rewrite_net = internal_rule Net.net 

    let src_of (_, _, _, _, r) = r


    exception No_change

    let null_term = Term.mk_free "" (Gtypes.mk_null())


(** 
   [find_match scp ctrl tyenv varp lhs term env]: Try to match [lhs]
   with term in type environment [tyenv] and term environment [env].

   Return a new type environment and term environment if successful.
 *)

(**
   [match_rewrite scp ctrl tyenv varp lhs rhs order trm]: Match [trm]
   with [lhs]. 

   If successful, return [rhs], the new type environment and the new
   term environment. The type and term environments are obtained by
   unifying [lhs] and [trm] and contain the bindings for unification
   variables in [rhs].
 *)
    let match_rewrite ctrl data rule trm = 
      let (scope, qntenv, tyenv) = data
      in
      let env = Term.empty_subst ()
      in 
      let (qs, lhs, rhs, order, src) = rule
      in
      let varp x = is_free_binder qs x
      in
      let find_match term1 term2=
	Unify.unify_fullenv_rewrite scope tyenv env varp term1 term2 
      in 
      try
	(let tyenv1, env1=find_match lhs trm
	in 
	let nt = Term.subst_closed qntenv env1 rhs
	in 
	match order with
	  None -> (src, nt, tyenv1)
	| Some(p) ->  
	    if (p nt trm)   (* if nt < trm *)
	    then (src, nt, tyenv1) (* accept nt *) 
	    else raise (Failure "No match")) (* reject nt *)
      with err -> 
	(Term.add_term_error "match_rewrite: failed" [lhs; trm] err)


(**
   [match_rr_list scp ctrl tyenv chng rs trm]: Try to rewrite [trm]
   with the rules in [rs]. Calls [match_rewrite] with each of the
   rewrite rules, returning the first to succeed.

   If any rule succeeds, continues with the replacement (rhs) term
   given by the rule.

   If no rule matches, raise [No_change].
 *)
    let rec match_rr_list ctrl data rules trm rslt = 
      if (limit_reached (ctrl.depth))
      then raise No_change
      else 
	(match rules with
	  [] -> raise No_change 
	| r::nxt ->
	    (match Lib.try_app (match_rewrite ctrl data r) trm with
	      None -> match_rr_list ctrl data nxt trm rslt
	    | Some(rl, ntrm, ntyenv) -> 
		(ntrm, ntyenv, decr_depth ctrl, rl::rslt)))

(**
   [match_rewrite_list scp ctrl tyenv chng net trm]: Repeatedly
   rewrite [trm] using rules stored in term-net [net] until no rule
   matches or the limit (given by control [ctrl]) is reached. Note
   that [match_rewrite_list] doesn't descend into the terms' subterms.

   If [trm] is not rewritten, return an empty list of rules.
 *)
    let rec match_rewrite_list ctrl data net trm rslt =
      let (scope, qntenv, tyenv) = data
      in 
      if(limit_reached ctrl.depth)
      then (trm, tyenv, ctrl, rslt)
      else 
	(let rs= Net.lookup net trm
	in 
	match 
	  (Lib.try_app (match_rr_list ctrl data rs trm) rslt)
	with 
	  None -> (trm, tyenv, ctrl, rslt)
	| Some(ntrm, ntyenv, nctrl, nrslt) ->
	    match_rewrite_list 
	      nctrl (scope, qntenv, ntyenv) net ntrm nrslt)


    let check_change x = 
      match x with
	Skip -> raise No_change
      | _ -> ()

    let check_change2 x y = 
      match (x, y) with
	(Skip, Skip) -> raise No_change
      | _ -> ()

    let rec rewrite_td_subterm ctrl data net t =
      let (scope, qntenv, tyenv) = data
      in 
      if(limit_reached ctrl.depth)
      then raise No_change
      else 
	(match t with
	  Basic.Qnt(q, b) -> 
	    let qntenv1 = 
	      Term.bind (Basic.Bound q) null_term qntenv
	    in 
	    let (nb, benv, bctrl, brslt) = 
	      rewrite_td_term ctrl (scope, qntenv1, tyenv) net b
	    in 
	    check_change brslt;
	    let subplans = pack(mk_subnode 0 brslt)
	    in 
	    (Basic.Qnt(q, nb), benv, bctrl, subplans)
	| Basic.App(f, a)->
	    let nf, fenv, fctrl, fplan = 
	      try (rewrite_td_term ctrl data net f)
	      with No_change -> (f, tyenv, ctrl, mk_skip)
	    in
	    let na, aenv, actrl, aplan = 
	      try (rewrite_td_term fctrl (scope, qntenv, fenv) net a)
	      with No_change -> (a, fenv, fctrl, mk_skip)
	    in 
	    check_change2 fplan aplan;
	    let subplans = pack(mk_branches[fplan; aplan])
	    in 
	    (Basic.App(nf, na), aenv, actrl, subplans)
	| Basic.Typed(tt, ty) -> 
	    rewrite_td_term ctrl data net tt
	| _ -> (t, tyenv, ctrl, mk_skip))
    and 
	rewrite_td_term ctrl data net t = 
      if (limit_reached ctrl.depth)
      then raise No_change
      else 
	let (scope, qntenv, tyenv) = data
	in 
	let (t1, env1, ctrl1, rules) =
	  match_rewrite_list ctrl data net t []
	in 
	let (t2, env2, ctrl2, subplan) =
	  (match 
	    Lib.try_app 
	      (rewrite_td_subterm ctrl1 (scope, qntenv, env1) net) t1 
	  with
	    Some x -> x
	  | None -> (t1, env1, ctrl1, mk_skip))
	in
	(if rules=[] 
	then check_change subplan
	else ());
	let plan1 = pack(mk_rules (List.rev rules))
	in 
	let plan2 = 
	  pack(mk_node (key_of t) [plan1; subplan])
	in 
	(t2, env2, ctrl2, plan2)
    and 
	make_list_topdown ctrl net data trm =
      let (scope, qntenv, tyenv) = data
      in 
      rewrite_td_term ctrl (scope, Term.empty_subst(), tyenv) net trm

(**
   [make_list_bottomup scp ctrl tyenv chng net trm]: Rewrite [trm]
   and its sub-terms, bottom-up.

   Each subterm of [trm] is rewritten (bottom-up) with the rules in
   [net] then [trm] (after its subterms are replaced) is
   rewritten. Rewriting continues up to the limit set by [ctrl.depth].

   If [trm] or any of its subterms are rewritten, [chng] is set to
   [true] other wise it is unchanged.
 *)
    let rec rewrite_bu_subterm ctrl data net t=
      if (limit_reached (ctrl.depth))
      then raise No_change
      else 
	let (scope, qntenv, tyenv) = data
	in 
	match t with
	  Basic.Qnt(q, b) -> 
	    let qntenv1 = 
	      Term.bind (Basic.Bound(q)) null_term qntenv
	    in 
	    let nb, benv, bctrl, brslt = 
	      rewrite_bu_subterm ctrl (scope, qntenv1, tyenv) net b
	    in 
	    let subplans = 
	      try 
		check_change brslt; 
		pack (mk_subnode 0 brslt)
	      with _ -> mk_skip
	    in 
	    rewrite_bu_term ctrl 
	      (scope, qntenv, benv) net (Basic.Qnt(q, nb)) subplans
	| Basic.App(f, a)->
	    let (nf, fenv, fctrl, frslt) = 
	      try rewrite_bu_subterm ctrl data net f
	      with No_change -> (f, tyenv, ctrl, mk_skip)
	    in
	    let (na, aenv, actrl, arslt) = 
	      try rewrite_bu_subterm fctrl (scope, qntenv, fenv) net a
	      with No_change -> (a, fenv, fctrl, mk_skip)
	    in 
	    let subplans = 
	      try 
		check_change2 frslt arslt;
		pack(mk_branches [frslt; arslt])
	      with _ -> mk_skip
	    in 
	    rewrite_bu_term actrl 
	      (scope, qntenv, aenv) net (Basic.App(nf, na)) subplans
	| Basic.Typed(tt, ty) -> 
	    rewrite_bu_subterm ctrl data net tt
	| _ -> 
	    rewrite_bu_term ctrl data net t mk_skip
    and 
	rewrite_bu_term ctrl data net t subrslt =
      let (t1, env1, ctrl1, rslt1) =
	(match_rewrite_list ctrl data net t [])
      in 
      (match (rslt1, subrslt) with
	([], _) -> check_change subrslt
      | _ -> ());
      let plan1= pack(mk_rules (List.rev rslt1))
      in 
      let plan2 = pack(mk_node (key_of t) [subrslt;  plan1])
      in 
      (t1, env1, ctrl1, plan2)
    and
	make_list_bottomup ctrl net data trm =
      let (scope, qntenv, tyenv) = data
      in 
      rewrite_bu_subterm ctrl (scope, Term.empty_subst(), tyenv) net trm


    let make_rewrites rule_data xs = 
      let rec make_rewrites_aux xs net=
	match xs with
	  [] -> net
	| (rl::rst) -> 
	    let (vs, key, rep, order) = A.dest rule_data rl
	    in 
	    let net_data = (vs, key, rep, order, rl)
	    in 
	    make_rewrites_aux rst
	      (Net.add (is_free_binder vs) net key net_data)
      in 
      (make_rewrites_aux (List.rev xs) (Net.empty()))

    let make_list rule_data ctrl scope tyenv rs trm = 
      let net=make_rewrites rule_data rs
      in 
      if (limit_reached ctrl.depth)
      then raise No_change
      else 
	let (ntrm, ntyenv, nctrl, plan) = 
	  let data = (scope, Term.empty_subst(), tyenv)
	  in
	  if (is_topdown (ctrl.rr_strat))
	  then make_list_topdown ctrl net data trm
	  else make_list_bottomup ctrl net data trm
	in 
	(ntrm, ntyenv, plan)

(***
 * Toplevel functions
 ***)

    let make_env rule_data scope ctrl tyenv rrl trm=
      try make_list rule_data ctrl scope tyenv rrl trm
      with 
	No_change -> 
	  raise (term_error "Rewriting failed" [trm])

    let make rule_data scope ctrl rrl trm =
      let (ret, _, plan) = 
	make_env rule_data scope ctrl (Gtypes.empty_subst()) rrl trm
      in (ret, plan)

  end


module TermPlannerData =
  struct
    type rule = Basic.term
    type data = unit

    let dest _ trm = 
      let qs, b = strip_qnt Basic.All trm
      in 
      let lhs, rhs= Logicterm.dest_equality b
      in 
      (qs, lhs, rhs, None)

  end

module TermPlanner = Planner(TermPlannerData)
