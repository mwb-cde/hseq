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

(*
let varp, funp, constp, is_app, is_typed, qntp=
  Term.is_bound, Term.is_fun, Term.is_const, 
  Term.is_app, Term.is_typed, Term.is_qnt 
*)

(*
let eqqnt tyenv s t = 
  let q1, _ = dest_qnt s
  and q2, _ = dest_qnt t
  in
  let qnt1, _, qty1=Basic.dest_binding q1
  and qnt2, _, qty2=Basic.dest_binding q2
  in 
  (Gtypes.matches tyenv qty1 qty2) & (qnt1=qnt2)

let eqqnt_env scp s t tyenv = 
  let q1, _ = dest_qnt s
  and q2, _ = dest_qnt t
  in
  let qnt1, _, qty1=Basic.dest_binding q1
  and qnt2, _, qty2=Basic.dest_binding q2
  in 
  if qnt1=qnt2 then 
    (try 
      (true, Gtypes.unify_env scp qty1 qty2 tyenv)
    with _ -> (false, tyenv))
  else (false, tyenv)
*)

let is_free_binder qs t= 
  (match t with
    Bound(q) -> List.exists (fun x ->  x == q) qs
  |	_ -> false)

(*** Internal represenation of rewrite rules ***)

type rewrite_rules = 
    (Basic.binders list * Basic.term * Basic.term * order option)

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
let match_rewrite scope ctrl tyenv varp lhs rhs order trm = 
  let env = Term.empty_subst ()
  in 
  try
    (let tyenv1, env1=find_match scope ctrl tyenv varp lhs trm env; 
    in 
    let nt = Term.subst env1 rhs
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
let rec match_rr_list scope ctrl tyenv chng rs trm = 
  if (limit_reached (ctrl.depth))
  then (trm, tyenv, ctrl)
  else 
    (match rs with
      [] -> (trm, tyenv, ctrl)
    | (qs, lhs, rhs, order)::nxt ->
	let (ntrm, ntyenv), fl = 
	  (try 
	    (match_rewrite scope ctrl tyenv 
	       (is_free_binder qs) lhs rhs order trm, 
	     true)
	  with _ -> (trm, tyenv), false)
	in 
	if fl 
	then (chng:=true; (ntrm, ntyenv, decr_depth ctrl))
	else 
	  match_rr_list scope ctrl ntyenv chng nxt trm)

(**
   [match_rewrite_list scp ctrl tyenv chng net trm]: Repeatedly
   rewrite [trm] using rules stored in term-net [net] until no rule
   matches or the limit (given by control [ctrl]) is reached. Note
   that [match_rewrite_list] doesn't descend into the terms' subterms.

   If [trm] rewritten then [chng] is set to [true] otherwise it is
   unchanged.
*)
let rec match_rewrite_list scope ctrl tyenv chng net trm =
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
      match_rr_list scope ctrl tyenv cn rs trm
    in 
    if (!cn) 
    then 
      (chng:=true; 
       match_rewrite_list scope nctrl ntyenv chng net ntrm)
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
  let rec rewrite_subterm ctrl env t=
    if(limit_reached ctrl.depth)
    then (t, env, ctrl)
    else 
      (match t with
	Basic.Qnt(q, b) -> 
	  let nb, benv, bctrl = rewrite_aux ctrl env b
	  in 
	  (Basic.Qnt(q, nb), benv, bctrl)
      |	Basic.App(f, a)->
	  let nf, fenv, fctrl = (rewrite_aux ctrl env f)
	  in
	  let na, aenv, actrl = (rewrite_aux fctrl fenv a)
	  in 
	  (Basic.App(nf, na), aenv, actrl)
      | Basic.Typed(tt, ty) -> rewrite_aux ctrl env tt
      | _ -> (t, env, ctrl))
  and rewrite_aux ctrl env t = 
    if(limit_reached ctrl.depth)
    then (t, env, ctrl)
    else 
      (let nt, nenv, nctrl= match_rewrite_list scope ctrl env chng net t
      in 
      rewrite_subterm nctrl nenv nt)
  in 
  rewrite_aux ctrl tyenv trm

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
  let rec rewrite_aux ctrl env t=
    if(limit_reached (ctrl.depth))
    then (t, env, ctrl)
    else 
      (match t with
	Basic.Qnt(q, b) -> 
	  (let nb, benv, nctrl = rewrite_aux ctrl env b
	  in 
	  match_rewrite_list scope nctrl benv chng net (Qnt(q, nb)))
      |	Basic.App(f, a)->
	  (let nf, fenv, fctrl = 
	    (rewrite_aux ctrl env f)
	  in
	  let na, aenv, actrl= 
	    (rewrite_aux fctrl fenv a)
	  in 
	  match_rewrite_list scope actrl aenv chng net (Basic.App(nf, na)))
      | Basic.Typed(tt, ty) -> 
	  rewrite_aux ctrl env tt
      | _ -> 
	  match_rewrite_list scope ctrl env chng net t)
  in 
  rewrite_aux ctrl tyenv trm

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

(*
   rewrite with equality eqtrm: "l=r" -> (l, r) 
   left-right if dir=true, right-left otherwise
 *)
(*
let rewrite_eqs scope ctrl tyenv rrl trm =
  let chng = ref false
  in 
  let r = rewrite_list scope ctrl chng tyenv rrl trm;
  in 
  if !chng 
  then r
  else raise (term_error "Matching" [trm])
*)

(*** Rule destructors *)

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


(***
* Toplevel rewriting functions
***)

(*
let rewrite_env scope ctrl tyenv rrl trm=
  let rs = 
    if ctrl.rr_dir=LeftRight
    then 
      List.map dest_lr_rule rrl
    else 
      List.map dest_rl_rule rrl
  in 
  rewrite_eqs scope ctrl tyenv rs trm
*)

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
