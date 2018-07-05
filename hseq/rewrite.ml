(*----
  Name: rewrite.ml
  Copyright Matthew Wahab 2005-2018
  Author: Matthew Wahab <mwb.cde@gmail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under
  the terms of the Lesser GNU General Public License as published by
  the Free Software Foundation; either version 3, or (at your option)
  any later version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
  License for more details.

  You should have received a copy of the Lesser GNU General Public
  License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
  ----*)

open Basic
open Term
open Report

(*
 * Rewrite Rules
 *)

(** Rule ordering *)
type order = (Term.term -> Term.term -> bool)
type rule = (Term.binders list * Term.term * Term.term)
type orule = (Term.binders list * Term.term * Term.term * order option)

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
  { depth = max; rr_dir = dir; rr_strat = strat }

(** The default control *)
let default_control=
  control ~strat:TopDown ~dir:leftright ~max:None


(*** Internal represenation of rewrite rules ***)

(**
   Directed Rewriting
*)

exception Quit of exn
exception Stop of exn

(**
   Planned Rewriting

   Rewriting based on a pre-determined plan.

   Rewrites a node [n] by following the direction in a plan [p]. Plan
   [p] specifies the rules to be applied to each node and the order in
   which a node is to be rewritten.  (For hseq, a node is a term.)
*)

(** {7 Planned rewriting specialised to terms} *)

type term_key =
  | Ident  (** Term [Id] *)
  | BVar   (** Term [Bound] *)
  | MVar   (** Term [Meta] *)
  | FVar   (** Term [Free] *)
  | Appln  (** Term [App] *)
  | Quant  (** Term [Qnt] *)
  | AllQ   (** Term [Qnt] ([All]) *)
  | ExQ   (** Term [Qnt] ([Ex]) *)
  | LamQ   (** Term [Qnt] ([Lambda]) *)
  | Constn (** Term [Const] *)
  | AnyTerm    (** Any term *)
  | NoTerm   (** No term *)
  | Neg of term_key (** Negate a key *)
  | Alt of term_key * term_key  (** Alternative keys *)

(*** Rewriting utilities ***)
let limit_reached d =
  match d with Some 0 -> true | _ -> false

let decr_depth ctrl =
  match ctrl.depth with
    | None -> ctrl
    | Some x -> {ctrl with depth=Lib.set_int_option(x-1)}

let is_free_binder qs t =
  match t with
    | Atom(Bound(q)) -> List.exists (Term.binder_equality q) qs
    | _ -> false

(*
 *   [TermData]: The data used to instantiate the generic rewriter.
 *)
type a_rule = rule

module TermData =
struct
  type node = Term.term
  type rule = a_rule
  type substn = Term.Subst.t
  type data =
      (Scope.t                (** Scope *)
       * Term.Subst.t    (** Quantifier environment *)
       * Gtype.Subst.t) (** Type environment *)

  type key = term_key

  let key_of_atom n =
    match n with
    | Term.Id _ -> Ident
    | Term.Bound _ -> BVar
    | Term.Meta _ -> MVar
    | Term.Free _ -> FVar
    | Term.Const _ -> Constn

  let key_of_binder q =
    match q with
    | Term.All -> AllQ
    | Term.Ex -> ExQ
    | Term.Lambda -> LamQ
    | _ -> Quant

  let key_of n =
    match n with
      | Term.Atom(a) -> key_of_atom a
      | Term.Qnt(q, _) -> key_of_binder (Term.binder_kind q)
      | Term.App _ -> Appln

  let rec is_key k n =
    match k with
      | AnyTerm -> true
      | NoTerm -> false
      | Alt(x, y) -> (is_key x n || is_key y n)
      | Neg(x) -> not (is_key x n)
      | Quant -> Term.is_qnt n
      | _ -> (k = key_of n)

  let num_subnodes n =
    match n with
      | App(l, r) -> 2
      | Qnt(_, b) -> 1
      | _ -> 0

  let subnodes_of n =
    match n with
      | App(l, r) -> [l; r]
      | Qnt(_, b) -> [b]
      | _ -> []

  let set_subnodes n xs =
    match n with
      | App(_, _) ->
         begin
           match xs with
           | [l; r] -> App(l, r)
           | _ -> raise (Quit (Failure "set_subnodes: App"))
         end
      | Qnt(q, _) ->
         begin
           match xs with
           | [b] -> Qnt(q, b)
           | _ -> raise (Quit (Failure "set_subnodes: Qnt"))
         end
      | _ -> raise (Quit (Failure "set_subnodes: other"))

  let get_subnode n i =
    match n with
      | App(l, r) ->
          (match i with
            | 0 -> l
            | 1 -> r
            | _ -> raise Not_found)
      | Qnt(_, b) ->
        (match i with
          | 0 -> b
          | _ -> raise Not_found)
      | _ -> raise Not_found

  let set_subnode n i x =
    match n with
        App(l, r) ->
          (match i with
            | 0 -> App(x, r)
            | 1 -> App(l, x)
            | _ -> raise Not_found)
      | Qnt(q, b) ->
        (match i with
          | 0 ->  Qnt(q, x)
          | _ -> raise Not_found)
      | _ -> raise Not_found


  let dest_rule r = r

  let matches data rule trm =
    let (scope, qntenv, tyenv) = data in
    let (qs, lhs, rhs) = dest_rule rule in
    let env = Term.Subst.empty() in
    let varp = is_free_binder qs
    in
    try
      let (tyenv1, env1) =
        Unify.matches_rewrite scope tyenv env varp lhs trm
      in
      ((scope, qntenv, tyenv1), env1)
    with x ->
      raise (Rewritekit.Quit
               (add_error (term_error ("Can't match terms") [lhs; trm]) x))

  let subst data rule env =
    let (scope, qntenv, tyenv) = data in
    let (qs, lhs, rhs) = dest_rule rule in
    let data1 = (scope, qntenv, tyenv)
    in
    (data1, Lterm.subst_closed qntenv env rhs)

  let add_data data trm =
    match trm with
      | Qnt(q, _) ->
          let (scope, qntenv, tyenv) = data in
          let qntenv1 =
            Term.Subst.bind
              (Term.mk_bound q)
              (Term.mk_free "" (Gtype.mk_null()))
              qntenv
          in
          (scope, qntenv1, tyenv)
      | _ -> data

  let drop_data (data1, trm1) (data2, trm2) =
    match trm1 with
      | Qnt(q, _) ->
          let (scope1, qntenv1, tyenv1) = data1 in
          let (scope2, qntenv2, tyenv2) = data2
          in
          (scope2, qntenv1, tyenv2)
      | _ -> data2

end

type data = TermData.data
type key = TermData.key

(*
 * The Rewriter
 *)
module TermRewriter=Rewritekit.Make(TermData)

(** {5 Toplevel rewriting functions} *)

type ('a)plan = (key, 'a)Rewritekit.plan

let rewrite = TermRewriter.rewrite
(** [rewrite data p t]: Rewrite term [t] with plan [t]. *)

let rec extract_check_rules scp dir pl =
  let get_test t =
    let qs, b = Term.strip_qnt Term.All t in
    let lhs, rhs = Lterm.dest_equality b
    in
    if dir = leftright
    then (qs, lhs, rhs)
    else (qs, rhs, lhs)
  in
  Rewritekit.mapping get_test pl

let plan_rewrite_env scp ?(dir=leftright) tyenv plan f =
  let plan1 = extract_check_rules scp dir plan in
  let data = (scp, Term.Subst.empty(), tyenv) in
  let (data1, nt) = rewrite data plan1 f in
  let (scp1, qntenv1, tyenv1) = data1
  in
  (nt, tyenv1)

let plan_rewrite scp ?(dir=leftright) plan f =
  let (nt, ntyenv) =
    plan_rewrite_env scp ~dir:dir (Gtype.Subst.empty()) plan f
  in
  nt

(*** Rewrite Plans ***)

let mk_node ps = Rewritekit.Node(ps)
let mk_keyed k ps = Rewritekit.Keyed(k, ps)
let mk_rules rs = Rewritekit.Rules(rs)
let mk_subnode i p = Rewritekit.Subnode(i, p)
let mk_branches ps = Rewritekit.Branches(ps)
let mk_skip = Rewritekit.Skip

let mapping = Rewritekit.mapping

let rec pack pl =
  match pl with
    | Rewritekit.Rules rs -> pack_rules rs
    | Rewritekit.Node(ps) -> pack_node ps
    | Rewritekit.Keyed(k, ps) -> pack_keyed k ps
    | Rewritekit.Subnode(i, p) -> pack_subnode i p
    | Rewritekit.Branches(ps) -> pack_branches ps
    | Rewritekit.Skip -> Rewritekit.Skip
and
    pack_rules rs =
  match rs with
    | [] -> Rewritekit.Skip
    | _ -> Rewritekit.Rules(rs)
and
    pack_node ps =
  match ps with
    | [] -> Rewritekit.Skip
    | _ ->
      let ps1 =
        List.filter (fun x -> not (x=Rewritekit.Skip)) ps
      in
      Rewritekit.Node(ps1)
and
    pack_keyed k ps =
  match ps with
    | [] -> Rewritekit.Skip
    | _ ->
      let ps1 =
        List.filter (fun x -> not (x=Rewritekit.Skip)) ps
      in
      Rewritekit.Keyed(k, ps1)
and
    pack_branches ps =
  match ps with
    | [] -> Rewritekit.Skip
    | [Rewritekit.Skip] -> Rewritekit.Skip
    | [Rewritekit.Skip; x] -> Rewritekit.Subnode(1, x)
    | [x; Rewritekit.Skip] -> Rewritekit.Subnode(0, x)
    | [x] -> Rewritekit.Subnode(0, x)
    | _ -> Rewritekit.Branches ps
and
    pack_subnode i p =
  match p with
    | Rewritekit.Skip -> Rewritekit.Skip
    | _ -> Rewritekit.Subnode(i, p)

(*** Keys ****)

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

module Planner =
struct

  module type Data =
  sig
    type rule
    type data
    val dest : data -> rule
        -> (Term.binders list * Term.term * Term.term * order option)
  end

  module type T =
  sig
    (** Rewrite plan constructors.

        Two planning functions are provided. The first is for general
        rewriting. The second for rewriting w.r.t a type context.

        Both take rewrite rules as universally quantified equalities of the
        for [!v1 .. vn. lhs = rhs]. The variables [v1 .. vn] are taken as
        variables which can be instantiated by the rewriter. If the rule is
        not an equality then rewriting will fail.

        Rewriting breaks a rule [lhs = rhs] to an equality. If rewriting is
        left-right, the equality is [lhs = rhs]; if rewriting is right-left
        then the equality is [rhs=lhs].

        For left-right rewriting, every variable (from [v1 .. vn])
        appearing in [rhs] must also appear in [lhs] otherwise the rule
        cannot be used. Similarly for right-left rewriting.
    *)
    exception No_change

    type a_rule
    type rule_data

    val make :
      rule_data
      -> Scope.t -> control -> a_rule list
      -> Term.term
      -> (Term.term * (a_rule)plan)
    (** Make a rewrite plan using a list of universally quantified
        rewrite rules.  *)

    val make_env :
      rule_data
      -> Scope.t
      -> control
      -> Gtype.Subst.t
      -> a_rule list -> Term.term
      -> (Term.term * Gtype.Subst.t * (a_rule)plan)
    (** [make_env tyenv rules trm]: Make a rewrite plan for [trm]
        w.r.t type environment [tyenv] using [rules]. Return the new
        term and the type environment contructed during rewriting.  *)

    (** {7 Exposed for debugging} *)

    type data =
        (Scope.t
         * Term.Subst.t
         * Gtype.Subst.t)

    type internal_rule =
        (Term.binders list
         * Term.term
         * Term.term
         * order option
         * a_rule)

    type rewrite_net = internal_rule Net.net

    val src_of: internal_rule -> a_rule

    val match_rewrite :
      control
      -> data
      -> internal_rule
      -> Term.term
      -> (a_rule * Term.term * Gtype.Subst.t)

    val match_rr_list:
      control
      -> data
      -> internal_rule list
      -> Term.term
      -> a_rule list
      -> (Term.term * Gtype.Subst.t * control * (a_rule)list)

    val match_rewrite_list:
      control
      -> data
      -> rewrite_net
      -> Term.term
      -> a_rule list
      -> (Term.term * Gtype.Subst.t * control * (a_rule)list)

    val check_change : ('a)plan -> unit
    val check_change2 : ('a)plan -> ('a)plan -> unit

    val make_list_topdown:
      control
      -> rewrite_net
      -> data
      -> Term.term
      -> (Term.term * Gtype.Subst.t * control * (a_rule)plan)

    val make_list_bottomup:
      control
      -> rewrite_net
      -> data
      -> Term.term
      -> (Term.term * Gtype.Subst.t * control * (a_rule)plan)

    val make_rewrites:
      rule_data -> a_rule list -> rewrite_net

    val make_list:
      rule_data
      -> control
      -> Scope.t
      -> Gtype.Subst.t
      -> a_rule list
      -> Term.term
      -> (Term.term * Gtype.Subst.t * (a_rule)plan)

  end
end

module Make =
  functor (A: Planner.Data) ->
struct

  open Rewritekit

  type a_rule = A.rule
  type rule_data = A.data

  type data = (Scope.t
               * Term.Subst.t
               * Gtype.Subst.t)

  type internal_rule = (Term.binders list
                        * Term.term
                        * Term.term
                        * order option
                        * a_rule)

  type rewrite_net = internal_rule Net.net

  let src_of (_, _, _, _, r) = r

  exception No_change

  let null_term = Term.mk_free "" (Gtype.mk_null())

  (** [match_rewrite scp ctrl tyenv varp lhs rhs order trm]: Match
      [trm] with [lhs].

      If successful, return [rhs], the new type environment and the new
      term environment. The type and term environments are obtained by
      unifying [lhs] and [trm] and contain the bindings for unification
      variables in [rhs].
  *)
  let match_rewrite ctrl data rule trm =
    let (scope, qntenv, tyenv) = data in
    let env = Term.Subst.empty () in
    let (qs, lhs, rhs, order, src) = rule in
    let varp x = is_free_binder qs x in
    let find_match term1 term2=
      Unify.matches_rewrite scope tyenv env varp term1 term2
    in
    try
      let tyenv1, env1=find_match lhs trm in
      let nt = Lterm.subst_closed qntenv env1 rhs
      in
      match order with
        | None -> (src, nt, tyenv1)
        | Some(p) ->
          if (p nt trm)   (* if nt < trm *)
          then (src, nt, tyenv1) (* accept nt *)
          else raise (Failure "No match") (* reject nt *)
    with err ->
      raise (Term.add_term_error "match_rewrite: failed" [lhs; trm] err)


  (**
     [match_rr_list scp ctrl tyenv chng rs trm]: Try to rewrite [trm]
     with the rules in [rs]. Calls [match_rewrite] with each of the
     rewrite rules, returning the first to succeed.

     If any rule succeeds, continues with the replacement (rhs) term
     given by the rule.

     If no rule matches, raise [No_change].
  *)
  let rec match_rr_list ctrl data rules trm rslt =
    if limit_reached (ctrl.depth)
    then raise No_change
    else
      (match rules with
        | [] -> raise No_change
        | r::nxt ->
          (match Lib.try_app (match_rewrite ctrl data r) trm with
            | None -> match_rr_list ctrl data nxt trm rslt
            | Some(rl, ntrm, ntyenv) ->
              (ntrm, ntyenv, decr_depth ctrl, rl::rslt)))

  (** [match_rewrite_list scp ctrl tyenv chng net trm]: Repeatedly
      rewrite [trm] using rules stored in term-net [net] until no
      rule matches or the limit (given by control [ctrl]) is
      reached. Note that [match_rewrite_list] doesn't descend into
      the terms' subterms.

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
       match (Lib.try_app (match_rr_list ctrl data rs trm) rslt)
       with
         | None -> (trm, tyenv, ctrl, rslt)
         | Some(ntrm, ntyenv, nctrl, nrslt) ->
           match_rewrite_list nctrl (scope, qntenv, ntyenv) net ntrm nrslt)

  let check_change x =
    match x with
      | Skip -> raise No_change
      | _ -> ()

  let check_change2 x y =
    match (x, y) with
      | (Skip, Skip) -> raise No_change
      | _ -> ()

  let rec rewrite_td_subterm ctrl data net t =
    let (scope, qntenv, tyenv) = data
    in
    if limit_reached ctrl.depth
    then raise No_change
    else
      (match t with
        | Term.Qnt(q, b) ->
            let qntenv1 = Term.Subst.bind (mk_bound q) null_term qntenv in
            let (nb, benv, bctrl, brslt) =
              rewrite_td_term ctrl (scope, qntenv1, tyenv) net b
            in
            check_change brslt;
            let subplans = pack(mk_subnode 0 brslt)
            in
            (Term.Qnt(q, nb), benv, bctrl, subplans)
        | Term.App(f, a)->
          let nf, fenv, fctrl, fplan =
            try rewrite_td_term ctrl data net f
            with No_change -> (f, tyenv, ctrl, mk_skip)
          in
          let na, aenv, actrl, aplan =
            try rewrite_td_term fctrl (scope, qntenv, fenv) net a
            with No_change -> (a, fenv, fctrl, mk_skip)
          in
          check_change2 fplan aplan;
          let subplans = pack(mk_branches[fplan; aplan])
          in
          (Term.App(nf, na), aenv, actrl, subplans)
        | _ -> (t, tyenv, ctrl, mk_skip))
  and
      rewrite_td_term ctrl data net t =
    if limit_reached ctrl.depth
    then raise No_change
    else
      let (scope, qntenv, tyenv) = data in
      let (t1, env1, ctrl1, rules) =
        match_rewrite_list ctrl data net t []
      in
      let (t2, env2, ctrl2, subplan) =
        try rewrite_td_subterm ctrl1 (scope, qntenv, env1) net t1
        with _ -> (t1, env1, ctrl1, mk_skip)
      in
      begin
        if rules = []
        then check_change subplan
        else ()
      end;
      let plan1 = pack (mk_rules (List.rev rules)) in
      let plan2 = pack (mk_node [plan1; subplan])
      in
      (t2, env2, ctrl2, plan2)
  and make_list_topdown ctrl net data trm =
    let (scope, qntenv, tyenv) = data
    in
    rewrite_td_term ctrl (scope, Term.Subst.empty(), tyenv) net trm

  (** [make_list_bottomup scp ctrl tyenv chng net trm]: Rewrite
      [trm] and its sub-terms, bottom-up.

      Each subterm of [trm] is rewritten (bottom-up) with the rules in
      [net] then [trm] (after its subterms are replaced) is
      rewritten. Rewriting continues up to the limit set by [ctrl.depth].

      If [trm] or any of its subterms are rewritten, [chng] is set to
      [true] other wise it is unchanged.
  *)
  let rec rewrite_bu_subterm ctrl data net t=
    if limit_reached (ctrl.depth)
    then raise No_change
    else
      let (scope, qntenv, tyenv) = data
      in
      match t with
        | Term.Qnt(q, b) ->
            let qntenv1 =
              Term.Subst.bind (Term.mk_bound(q)) null_term qntenv
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
              (scope, qntenv, benv) net (Term.Qnt(q, nb)) subplans
        | Term.App(f, a)->
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
            (scope, qntenv, aenv) net (Term.App(nf, na)) subplans
        | _ ->
          rewrite_bu_term ctrl data net t mk_skip
  and
      rewrite_bu_term ctrl data net t subrslt =
    let (t1, env1, ctrl1, rslt1) =
      match_rewrite_list ctrl data net t []
    in
    (match (rslt1, subrslt) with
      | ([], _) -> check_change subrslt
      | _ -> ());
    let plan1= pack(mk_rules (List.rev rslt1)) in
    let plan2 = pack(mk_node [subrslt;  plan1])
    in
    (t1, env1, ctrl1, plan2)
  and
      make_list_bottomup ctrl net data trm =
    let (scope, qntenv, tyenv) = data
    in
    rewrite_bu_subterm ctrl (scope, Term.Subst.empty(), tyenv) net trm

  let make_rewrites rule_data xs =
    let rec make_rewrites_aux xs net =
      match xs with
        | [] -> net
        | (rl::rst) ->
          let (vs, key, rep, order) = A.dest rule_data rl in
          let net_data = (vs, key, rep, order, rl)
          in
          make_rewrites_aux rst
            (Net.add (is_free_binder vs) net key net_data)
    in
    (make_rewrites_aux (List.rev xs) (Net.empty()))

  let make_list rule_data ctrl scope tyenv rs trm =
    let net = make_rewrites rule_data rs
    in
    if (limit_reached ctrl.depth)
    then raise No_change
    else
      let (ntrm, ntyenv, nctrl, plan) =
        let data = (scope, Term.Subst.empty(), tyenv)
        in
        if (is_topdown (ctrl.rr_strat))
        then make_list_topdown ctrl net data trm
        else make_list_bottomup ctrl net data trm
      in
      (ntrm, ntyenv, plan)

  (*
   * Toplevel functions
   *)

  let make_env rule_data scope ctrl tyenv rrl trm=
    try make_list rule_data ctrl scope tyenv rrl trm
    with No_change -> raise (term_error "Rewriting failed" [trm])

  let make rule_data scope ctrl rrl trm =
    let (ret, _, plan) =
      make_env rule_data scope ctrl (Gtype.Subst.empty()) rrl trm
    in (ret, plan)

end

(*** Term Rewrite Planner ***)

module TermPlannerData =
struct
  type rule = Term.term
  type data = unit

  let dest _ trm =
    let qs, b = strip_qnt Term.All trm in
    let lhs, rhs= Lterm.dest_equality b
    in
    (qs, lhs, rhs, None)
end

module TermPlanner = Make(TermPlannerData)
(** The Planner. *)

let make_plan_full = TermPlanner.make_env ()
let make_plan = TermPlanner.make ()
