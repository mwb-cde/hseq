(*----
  Name: simpset.ml
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

open Term
open Lterm
open Simputils

(***
    Simpsets
***)

(*** Utility functions ***)


(** [lt_var varp x y]: Less than ordering of terms for use with
    Net.insert. Makes variables (for which [varp] is true) larger than
    any other term.
*)
let lt_atom x y =
  let bound_lt (q1, n1, _) (q2, n2, _) =  (n1 < n2) && (q1 < q1)
  in
  match (x, y) with
  | (Const c1, Const c2) -> Term.Const.lt c1 c2
  | (Const _ , _ ) -> true
  | (Id _, Const _) -> false
  | (Id(a1, _), Id(a2, _)) -> Ident.lessthan a1 a2
  | (Id _, _) -> true
  | (Meta _, Const _) -> false
  | (Meta _, Id _) -> false
  | (Meta b1, Meta b2) -> bound_lt (Binder.dest b1) (Binder.dest b2)
  | (Meta _, _) -> true
  | (Free _, Const _) -> false
  | (Free _, Id _) -> false
  | (Free _, Meta _) -> false
  | (Free (n1, _), Free (n2, _)) -> n1 < n2

let rec lt_var lvarp rvarp x y =
  let lt_aux t1 t2 =
    let bound_lt (q1, n1, _) (q2, n2, _) =  (n1 < n2) && (q1 < q1)
    in
    match (t1, t2) with
    | (Atom(a1), Atom(a2)) -> lt_atom a1 a2
    | (Atom(_), _) -> true
    | (_, Atom(_)) -> false
    | (Bound b1, Bound b2) -> bound_lt (Binder.dest b1) (Binder.dest b2)
    | (Bound _ , _ ) -> true
    | (_, Bound _) -> false
    | (App(f1, a1), App (f2, a2)) ->
       if lt_var lvarp rvarp f1 f2
       then true
       else
         if lt_var lvarp rvarp f2 f1
         then false
         else lt_var lvarp rvarp a1 a2
    | (App _, _) -> true
    | (Qnt _, App _) -> false
    | (Qnt(q1, b1), Qnt(q2, b2)) ->
       if lt_var lvarp rvarp b1 b2
       then bound_lt (Binder.dest q1) (Binder.dest q2)
       else false
  in
  let (x_is_var, y_is_var) = (lvarp x, rvarp y)
  in
  match x_is_var, y_is_var with
    | (false, true) -> true
    | (true, _) -> false
    | _ -> lt_aux x y

(** [get_rr_order rl]: Get the ordering of rule [rl]. Fail if [rl] is
    not ordered.
*)
let get_rr_order rr =
  match rr with
    | Logic.OAsm(_, p) -> p
    | Logic.ORRThm(_, p) -> p
    | _ -> raise (Invalid_argument "get_rr_order")

(** [set_rr_order rl order]: Set the ordering of rule [rl] to order.
*)
let set_rr_order rr order =
  match rr with
    | Logic.OAsm(t, p) -> Logic.OAsm(t, order)
    | Logic.ORRThm(t, p) -> Logic.ORRThm(t, order)
    | Logic.Asm(t) -> Logic.OAsm(t, order)
    | Logic.RRThm(t) -> Logic.ORRThm(t, order)

(*** Rules ***)

(** [rule]: Variables, optional condition, lhs , rhs , source of rule
*)
type rule =
    (Binder.t list
     * term option * term * term
     * Rewrite.order option
     * Logic.rr_type)

let dest_rule (vs, cnd, lhs, rhs, order, rr) = (vs, cnd, lhs, rhs, order, rr)
let rule_binders (vs, cnd, lhs, rhs, order, rr) = vs
let rule_cond (vs, cnd, lhs, rhs, order, rr) = cnd
let rule_lhs (vs, cnd, lhs, rhs, order, rr) = lhs
let rule_rhs (vs, cnd, lhs, rhs, order, rr) = rhs
let rule_order (vs, cnd, lhs, rhs, order, rr) = order
let rule_src (vs, cnd, lhs, rhs, order, rr) = rr

(** [termnet_lt]: Less-than ordering on simp rules.
*)
let termnet_lt x y =
  let lvarp = is_variable (rule_binders x)
  and rvarp = is_variable (rule_binders y)
  in
  lt_var lvarp rvarp (rule_lhs y) (rule_lhs x)

(** [dest_rr_rule trm]: Split rule [trm] into binders, condition, lhs,
    rhs.  Rules must be in the form [c => (l = r)] or [l = r].
*)
let dest_rr_rule trm =
  (* Get leading quantifiers *)
  let (qs, t1) = Term.strip_qnt Term.All trm  in
  (* test for conditional equalities *)
  let (cnd, rl) =
    if Lterm.is_implies t1
    then (* is conditional *)
      let (_, asm, cncl) = Term.dest_binop t1
      in
      (Some(asm), cncl)
    else (None, t1)
  in
  (* break the equality *)
  if Lterm.is_equality rl
  then
    let (lhs, rhs) = Lterm.dest_equality rl
    in
    (qs, cnd, lhs, rhs, None)
  else
    raise (Failure "Not an equality or a conditional equality\n")

(** [make_rule rl src]: Make rule from theorem or assumption [src] in
    scope [scp].  *)
let make_rule rl trm =
  let (qs, c, l, r, o) = dest_rr_rule trm
  in
  (qs, c, l, r, o, rl)

(** [make_asm_rules ts except goal]

    Make a list of simp rules from the tagged formulas in [ts],
    ignoring those for which [except] is true.
*)
let make_asm_rule tform =
  let tg = Tactics.drop_formula tform
  and trm = Formula.term_of (Tactics.drop_tag tform)
  in
  make_rule (Logic.Asm (Tactics.ftag tg)) trm

let make_asm_rules except forms =
  let rec make_aux xs rslt =
    match xs with
      | [] -> List.rev rslt
      | tf::fs ->
        if except tf
        then make_aux fs rslt
        else
          let nrslt =
            try (make_asm_rule tf)::rslt
            with _ -> rslt
          in
          make_aux fs nrslt
  in
  make_aux forms []

(*** Sets ***)

(** [simpset]: set of simplifier rules
*)
type simpset =
    {
      convs: (Context.t -> Logic.conv) Net.net;
      basic: rule Net.net;          (* global rules *)
      next: simpset option          (* next simpset *)
    }

let empty_set() =
  {
    convs = Net.empty();
    basic=Net.empty();
    next=None
  }

(** [join s t]: Join sets s and t together.  In the set [join s t],
    the set [s] will be searched before set [t]
*)
let rec join s1 s2 =
  match s1.next with
    | None -> {s1 with next = Some(s2)}
    | Some x -> {s1 with next = Some(join x s2)}

(** [split s]: Split simpset [s] into two parts.  Fails if [s] is not
    joined to another set.
*)
let split s1 =
  match s1.next with
    | None -> raise (Failure "split")
    | Some x -> ({s1 with next = None}, x)

(** [add_rule rl set]: Add rule [rl = c => (l = r)] to [set]. If rule
    could lead to looping ([Simpconvs.equals_upto_vars l r] is true)
    then make rule an ordered rewrite (using {!Term.term_lt}).
*)
let add_rule rl s =
  let (vs, cond, l, r, order, src) = rl in
  let varp = is_variable vs in
  if equal_upto_vars (Rewrite.is_free_binder vs) l r
  then
    let order =
      try get_rr_order src
      with _ -> Term.term_lt
    in
    let rl1 = (vs, cond, l, r, Some(order), set_rr_order src order)
    in
    {s with basic = Net.insert termnet_lt varp (s.basic) l rl1}
  else
    {s with basic = Net.insert termnet_lt varp (s.basic) l rl}

(** [add_conv (vars, key) conv set]: Add conversion to [set], to
    rewrite terms matching [key], where [vars] are the variables of
    [key].
*)
let add_conv (vars, key) conv s =
  let varp = is_variable vars
  in
  {s with convs = Net.add varp (s.convs) key conv}

(*** Look-up functions ***)

(** [lookup_conv scp set trm list]: Look up [trm] in the conversions
    of [set]. Raise [Not_found] on failure.
*)
let rec lookup_conv sctxt set trm list =
  let conv_list = Net.lookup set.convs trm
  in
  try
    let thm = Lib.find_first (fun conv -> conv sctxt trm) conv_list in
    let (qs, conc, lhs, rhs, order, src) =
      make_rule thm (Logic.term_of thm)
    in
    (qs, conc, lhs, rhs, order, Logic.RRThm(src))::list
  with _ -> raise Not_found

(** [lookup_all scp set trm lst]: Lookup trm in [set], adding list of
    matches to list. First the conversions of [set] are searched then
    the rewrite rules then the next set in the chain (if any). The
    rules are added, in the order they are found, to [lst].
*)
let rec lookup_all sctxt set term list =
  let list1 =
    try lookup_conv sctxt set term list
    with Not_found -> list
  in
  let list2 =
    try List.rev_append (Net.lookup set.basic term) list1
    with Not_found -> list1
  in
  match set.next with
    | None -> List.rev list2
    | Some(s) -> lookup_all sctxt s term list2


(** [lookup trm set]: find list of possible matches for term [trm] in
    [set].  First the conversions of [set] are searched then the
    rewrite rules then the next set in the chain (if any). The rules
    are returned in the order they are found.
*)
let rec lookup sctxt set trm = lookup_all sctxt set trm []

(*** Adding rules to a simpset ***)

(** [simpset_add_rules scp set rls]: Add (properly formed rules) [rls]
    to set [set] in scope [scp].
*)
let simpset_add_rules sset entries =
  List.fold_left (fun s e-> add_rule e s) sset entries

(*** Theorems ***)

(** [make_thm_rule thm]: Make rule from theorem [thm]. *)
let make_thm_rule thm =
  make_rule (Logic.RRThm thm) (Logic.term_of thm)

(** [thm_to_entries scp thm]: Convert a theorem to a list of simpset
    entries.  *)
let thm_to_entries (sctxt: Context.t) (thm: Logic.thm) =
  let rules = Simpconvs.thm_to_rules sctxt thm
  in
  List.map make_thm_rule rules

(** [simpset_add_thm scp set thm]: Add rewrites from [thm] to simpset
    [set].  *)
let simpset_add_thm ctxt sset thm =
  let entries = thm_to_entries ctxt thm
  in
  simpset_add_rules sset entries

(** [simpset_add_thms scp set thms]: Apply [simpset_add_thm] to each
    theorem in [thms].  *)
let simpset_add_thms sctxt set thms =
  List.fold_left (simpset_add_thm sctxt) set thms

(*** Assumptions ***)

let simpset_add_asm_rule sset tg g=
  let trm =
    Formula.term_of
      (Logic.drop_tag
         (Logic.Sequent.get_tagged_asm tg (Tactics.sequent g)))
  in
  let rule = make_rule (Logic.Asm (Tactics.ftag tg)) trm
  in
  simpset_add_rules sset [rule]

(** [add_context set trm]: Add [trm] as a new assumption in which
    decision procedures operate. This currently does nothing, in the
    future it will be used to notify conversions of new context.  **)
let add_context set trm = set

(*** Printers ***)

(** [print_rule]: Printer for rules *)
let print_rule ppinfo (vars, cond, lhs, rhs, order, src) =
  let trm =
    let trm1 =
      match cond with
          None -> Lterm.mk_equality lhs rhs
        | Some c -> Lterm.mk_implies c (Lterm.mk_equality lhs rhs)
    in
    Term.rebuild_qnt vars trm1
  in
  Format.printf "@[";
  begin
    match src with
      | Logic.Asm _ ->
        Format.printf "Assumption: ";
        Printers.print_term ppinfo trm;
      | Logic.OAsm _ ->
        Format.printf "Ordered assumption: ";
        Printers.print_term ppinfo trm;
      | Logic.RRThm thm ->
        Format.printf "Theorem: ";
        Logic.print_thm ppinfo thm
      | Logic.ORRThm (othm, _) ->
        Format.printf "Ordered theorem: ";
        Logic.print_thm ppinfo othm
  end;
  Format.printf "@]"

(** [print_rule_net]: Printer for rule nets. *)
let print_rule_net ppinfo net =
  let rule_printer r = print_rule ppinfo r; Format.printf "@,"
  in
  Format.printf "@[<v>";
  Net.iter rule_printer net;
  Format.printf "@]"

let rec print_aux ppinfo set =
  print_rule_net ppinfo set.basic;
  match set.next with
    | None -> ()
    | Some set1 ->
      Format.printf "@,";
      print_aux ppinfo set1

(** [print]: Printer for simpsets. *)
let print ppinfo set =
  Format.printf "@[<v>[{";
  print_aux ppinfo set;
  Format.printf "}]@]"
