(*----
  Name: formula.ml
  Copyright Matthew Wahab 2005-2016
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
exception Error

(** The type for formulas *)
type t =  {thy: Scope.marker; term: Basic.term}

let term_of x = x.term
(** Convert a formula to a term. *)

let thy_of x = x.thy
(** Get the theory marker of a formula. *)

(*
 * Error handling
 *)

let print_form_error s ts fmt pinfo =
    Format.fprintf fmt "@[%s@ @[" s;
    Printer.print_sep_list
      ((fun f-> Printers.print_term pinfo (term_of f)), ",") ts;
    Format.fprintf fmt "@]@]"

let error s ts = Report.mk_error(print_form_error s ts)
let add_error s t es = raise (Report.add_error (error s t) es)

(*
 * Conversion from a term
 *)

(** [mk_scoped_formula scp f]: make [f] a formula of [scp].  Currently
    does nothing. Must not be exposed for general use.
*)
let mk_scoped_formula scp f = { thy = Scope.marker_of scp; term = f }

(** Convert a term to a formula *)

let resolve_term ?(strict=false) scp trm =
  Lterm.resolve scp trm

let prepare ?(strict=false) scp tyenv trm =
  let t1, lst = Lterm.resolve scp trm
  in
  if strict && not (lst = [])
  then
    raise
      (Term.term_error
         "Formula.make: Can't make formula, not a closed term" [trm])
  else
    let tyenv1 =
      try Typing.typecheck_top scp tyenv t1 (Gtype.mk_null())
      with x ->
        raise (Report.add_error x
                 (Term.term_error "Formula.make: incorrect types" [t1]))
    in
    (lst, t1, tyenv1)

let make_full ?(strict=false) scp tyenv t =
  let (lst, t1, tyenv1) = prepare ~strict:strict scp tyenv t in
  let t2 =
    List.fold_left
      (fun b (q, _) -> Term.mk_qnt (Term.dest_bound q) b)
      t1 lst
  in
  (mk_scoped_formula scp (Term.retype tyenv1 t2), tyenv1)

let make ?strict ?tyenv scp t =
  let env =
    match tyenv with
      | None -> Gtype.empty_subst()
      | Some(x) -> x
  in
  let (form, _) = make_full ?strict scp env t
  in
  form

(*** Fast conversion to formulas for internal use ***)

(** [mk_subterm_unsafe f t]: Make [t] a formula with the same theory
    marker as [f].  This is only safe if [t] is a subterm of [f] and
    [f] is not a quantifier.
*)
let mk_subterm_unsafe f t = {thy = thy_of f; term = t}

(** [mk_subterm f t]: Make [t] a formula with the same theory marker
    as [f]. Checks that [t] is a closed subterm of [f], fails
    otherwise.
*)
let mk_subterm f t =
  if (Term.is_subterm t (term_of f)) && (Lterm.is_closed [] t)
  then {thy = thy_of f; term = t}
  else
    raise (add_error "Can't make a formula as a subterm of formula" [f]
             (Term.term_error "term isn't a subterm" [t]))

(** [formula_in_scope scp f]: true if formula [f] is in scope
    [scp].  *)
let formula_in_scope scp f = Scope.in_scope_marker scp (thy_of f)

(** [valid_forms scp fs]: Return true if all formulas in [fs] are in
    scope [scp]. Return false otherwise. Used to test whether the
    formulas can be used e.g. with conjunction to make a new formula
    without the expense of going through [make].
*)
let valid_forms scp fs =
  let test() =
    List.iter
      (fun f ->
        if (formula_in_scope scp f)
        then ()
        else raise (error "invalid formula" [f])) fs
  in
  try test(); true
  with _ -> false

(** [fast_make scp fs t]: make a formula without any checks, if
    possible. This function must not be exposed for general use. It is
    only for use by the constructors.

    If [fs] are valid formulas then make [t] a formula of [scp] without
    doing any checks. Otherwise make [t] a formula using [make].
*)
let fast_make ?env scp fs t =
  if valid_forms scp fs
  then mk_scoped_formula scp t
  else make scp t

(*
 * Representation for permanent storage
 *)

type saved_form =  Dbterm.dbterm
let to_save x =  Dbterm.of_term (term_of x)
let from_save scp x = make scp (Dbterm.to_term x)

(*
 * Operations on formulas
 *)

let equals x y = Term.equals (term_of x) (term_of y)

(*** General tests ***)

let in_scope_memo memo scp f =
  if (formula_in_scope scp f) || (Lterm.in_scope memo scp (term_of f))
  then true
  else raise (Term.term_error "Badly formed formula" [term_of f])

let in_scope scp f =
  if (formula_in_scope scp f)
    || (Lterm.in_scope (Lib.empty_env()) scp (term_of f))
  then true
  else raise (Term.term_error "Badly formed formula" [term_of f])

let is_fresh scp f = formula_in_scope scp f

(*** Recognisers ***)

let is_qnt x = Term.is_qnt (term_of x)
let is_app x = Term.is_app (term_of x)
let is_bound x = Term.is_bound (term_of x)
let is_free x = Term.is_free (term_of x)
let is_ident x = Term.is_ident (term_of x)
let is_const x = Term.is_const (term_of x)
let is_fun x = Term.is_fun (term_of x)

let is_true x = Lterm.is_true (term_of x)
let is_false x = Lterm.is_false (term_of x)
let is_neg x = Lterm.is_neg (term_of x)
let is_conj x = Lterm.is_conj (term_of x)
let is_disj x = Lterm.is_disj (term_of x)
let is_implies x = Lterm.is_implies (term_of x)
let is_equality x = Lterm.is_equality (term_of x)

let is_all x = Lterm.is_all (term_of x)
let is_exists x = Lterm.is_exists (term_of x)
let is_lambda x = Lterm.is_lambda (term_of x)

(*** Destructors ***)

let dest_neg f =
  if is_neg f
  then
    let (_, x) = Term.dest_unop (term_of f)
    in
    mk_subterm_unsafe f x
  else raise (error "dest_neg" [f])

let dest_conj f =
  if is_conj f
  then
    let (_, a, b) = Term.dest_binop (term_of f)
    in
    (mk_subterm_unsafe f a, mk_subterm_unsafe f b)
  else raise (error "dest_conj" [f])

let dest_disj f =
  if is_disj f
  then
    let (_, a, b) = Term.dest_binop (term_of f)
    in
    (mk_subterm_unsafe f a, mk_subterm_unsafe f b)
  else raise (error "dest_disj" [f])

let dest_implies f =
  if is_implies f
  then
    let (_, a, b) = Term.dest_binop (term_of f)
    in
    (mk_subterm_unsafe f a, mk_subterm_unsafe f b)
  else raise (error "dest_implies" [f])

let dest_equality f =
  if is_equality f
  then
    let (_, a, b) = Term.dest_binop (term_of f)
    in
    (mk_subterm_unsafe f a, mk_subterm_unsafe f b)
  else raise (error "dest_equality" [f])

let get_binder_name x = Term.get_binder_name (term_of x)
let get_binder_type x = Term.get_binder_type (term_of x)

(*** Constructors ***)

let mk_true scp = make scp Lterm.mk_true
let mk_false scp = make scp Lterm.mk_false
let mk_bool scp b = if b then mk_true scp else mk_false scp

let mk_not scp f =
  fast_make scp [f] (Lterm.mk_not (term_of f))
let mk_and scp a b =
  fast_make scp [a; b] (Lterm.mk_and (term_of a) (term_of b))
let mk_or scp a b =
  fast_make scp [a; b] (Lterm.mk_or (term_of a) (term_of b))
let mk_implies scp a b =
  fast_make scp [a; b] (Lterm.mk_implies (term_of a) (term_of b))
let mk_iff scp a b =
  fast_make scp [a; b] (Lterm.mk_iff (term_of a) (term_of b))
let mk_equality scp a b =
  fast_make scp [a; b] (Lterm.mk_equality (term_of a) (term_of b))

(*
 * Typechecking
 *)

let typecheck_env scp tenv f expty =
  let t = term_of f
  in
  Typing.typecheck_top scp (Gtype.empty_subst()) t expty

let typecheck scp f expty=
  let t = term_of f in
  let tyenv = typecheck_env scp (Gtype.empty_subst()) f expty
  in
  make scp (Term.retype_pretty tyenv t)

let retype scp tenv x = make scp (Term.retype tenv (term_of x))

(**
   [retype_with_check tyenv t]: Reset the types in term [t] using type
   substitution [tyenv].  Substitutes variables with their concrete
   type in [tyenv]. Check that the new types are in scope.
*)
let term_retype_with_check scp tyenv t=
  let memo = Lib.empty_env() in
  let mk_new_type ty =
    let nty = Gtype.mgu ty tyenv
    in
    if (Ltype.in_scope memo scp nty)
    then nty
    else raise
      (Gtype.type_error "Term.retype_with_check: Invalid type" [nty])
  in
  let rec retype_aux t qenv =
    match t with
      | Id(n, ty) -> Id(n, mk_new_type ty)
      | Free(n, ty) -> Free(n, mk_new_type ty)
      | Bound(_) ->
        (try Term.table_find t qenv
         with Not_found -> t)
      | App(f, a) -> App(retype_aux f qenv, retype_aux a qenv)
      | Qnt(q, b) ->
        let (oqnt, oqnm, oqty) = Basic.dest_binding q
        in
        let nty = mk_new_type oqty
        in
        let nq = mk_binding oqnt oqnm nty
        in
        let qenv1 = Term.table_add (Bound(q)) (Bound(nq)) qenv; qenv in
        let new_term = Qnt(nq, retype_aux b qenv1 ) in
        let _ = Term.table_remove (Bound(q)) qenv1; qenv
        in
        new_term
      | Meta(_) -> t
      | Const(_) -> t
  in
  retype_aux t (Term.empty_table())

let retype_with_check scp tenv f =
  let nf =
    try term_retype_with_check scp tenv (term_of f)
    with err -> raise (add_error "Formula.retype_with_check" [f] err)
  in
  fast_make scp [f] nf

let typecheck_retype scp tyenv f expty=
  let tyenv1 = typecheck_env scp tyenv f expty
  in
  try (retype_with_check scp tyenv1 f, tyenv1)
  with err -> (add_error "Formula.typecheck_retype" [f] err)

(*** General Operations ***)

let rec is_closed scp env t =
  match t with
    | Basic.App(l, r) -> is_closed scp env l && is_closed scp env r
    | Basic.Qnt(q, b) ->
      let env1 = (Term.bind (Basic.Bound(q))
                    (Term.mk_free "" (Gtype.mk_null())) env)
      in
      is_closed scp env1 b
    | Basic.Meta (q) -> Scope.is_meta scp q
    | Basic.Bound(q) -> Term.member t env
    | Basic.Free(_) -> Term.member t env
    | _ -> true

let rec subst_closed scp qntenv sb trm =
  try
    let nt = Term.replace sb trm
    in
    if (is_closed scp qntenv nt)
    then subst_closed scp qntenv sb nt
    else raise (Failure "subst_closed: Not closed")
  with Not_found ->
    (match trm with
      | Basic.Qnt(q, b) ->
          let qntenv1 =
            Term.bind (Bound q) (Term.mk_free "" (Gtype.mk_null())) qntenv
          in
          Basic.Qnt(q, subst_closed scp qntenv1 sb b)
      | Basic.App(f, a) ->
        Basic.App(subst_closed scp qntenv sb f, subst_closed scp qntenv sb a)
      | _ -> trm)

let subst scp form lst =
  let env =
    List.fold_left
      (fun e (t, r) -> Term.bind (term_of t) (term_of r) e)
      (Term.empty_subst()) lst
  in
  let nt = Term.subst env (term_of form)
  in
  fast_make scp (List.map snd lst) nt

let subst_equiv scp form lst =
  let repl_list =
    List.map (fun (t, r) -> ((term_of t), (term_of r))) lst
  in
  let nt = Lterm.subst_equiv scp (term_of form) repl_list
  in
  fast_make scp (List.map snd lst) nt

let rename t = mk_subterm_unsafe t (Term.rename (term_of t))

let inst_env scp env f r =
  let t = term_of f
  and r1 = term_of r
  in
  if Term.is_qnt t
  then
    try
      let (q, b) = Term.dest_qnt t in
      let t1 =
        Term.subst
          (Term.bind (Basic.Bound(q)) r1 (Term.empty_subst())) b
      in
      let t2 = fast_make scp [f; r] t1
      in
      typecheck_retype scp env t2 (Gtype.mk_var "inst_ty")
    with err -> raise (add_error "inst: " [r] err)
  else raise (error "inst: not a quantified formula" [f])

let inst scp t r =
  let new_term, _ = inst_env scp (Gtype.empty_subst()) t r
  in
  new_term


(*
 * Unification functions
 *)

let unify scp asmf conclf =
  let asm = term_of asmf
  and concl = term_of conclf
  in
  let (avars, abody) = Term.strip_qnt Basic.All asm
  and (cvars, cbody) = Term.strip_qnt Basic.Ex concl
  in
  let varp x =
    match x with
      | Basic.Bound(q) -> (List.memq q avars) || (List.memq q cvars)
      | _ -> false
  in
  Unify.unify scp varp abody cbody

let unify_env scp tyenv asmf conclf =
  let asm = term_of asmf
  and concl = term_of conclf
  in
  let (avars, abody) = Term.strip_qnt Basic.All asm
  and (cvars, cbody) = Term.strip_qnt Basic.Ex concl
  in
  let varp x =
    match x with
      | Basic.Bound(q) -> (List.memq q avars) || (List.memq q cvars)
      | _ -> false
  in
  Unify.unify_fullenv scp tyenv (Term.empty_subst()) varp abody cbody

(*
 * Logic operations
 *)

(*** Alpha conversion ***)

let alpha_equals scp x y = Lterm.alpha_equals scp (term_of x) (term_of y)

let alpha_equals_match scp tyenv asmf conclf =
  let asm = term_of asmf
  and concl = term_of conclf
  and varp x = false
  in
  let (ret, _) =
    Unify.unify_fullenv scp tyenv (Term.empty_subst()) varp asm concl
  in
  ret

(*** Beta conversion ***)

let beta_convp x = Lterm.beta_convp (term_of x)
let beta_conv scp x =  make scp (Lterm.beta_conv (term_of x))
let beta_reduce scp x = make scp (Lterm.beta_reduce (term_of x))

(** [mk_beta_reduce_eq scp tyenv trm]: Make an equality expressing the
    result of beta-reducing trm.
*)
let mk_beta_reduce_eq scp tyenv trm =
  let (lhst, lst) = resolve_term scp trm in
  let rhst = Lterm.beta_reduce lhst in
  let eqtrm = Lterm.mk_equality lhst rhst
  in
  let rtrm =
    List.fold_left
      (fun b (q, _) -> Term.mk_qnt (Term.dest_bound q) b)
      eqtrm lst
  in
  make_full scp tyenv rtrm

(*** Eta conversion ***)

let eta_conv scp f x =
  make scp (Lterm.eta_conv [term_of f] (term_of x))

(*
 * Rewriting
 *)

let default_rr_control= Rewrite.default_control

(*** Rewriting functions ***)

let rec extract_check_rules scp dir pl =
  let get_test x =
    if (formula_in_scope scp x)
    then
      let t = term_of x in
      let qs, b = Term.strip_qnt Basic.All t in
      let lhs, rhs = Lterm.dest_equality b
      in
      if dir = Rewrite.leftright
      then (qs, lhs, rhs)
      else (qs, rhs, lhs)
    else
      raise (error "Rewrite rule not in scope" [x])
  in
  Rewrite.mapping get_test pl

let rewrite_env scp ?(dir=Rewrite.leftright) tyenv plan f =
  let plan1 = extract_check_rules scp dir plan in
  let data = (scp, Term.empty_subst(), tyenv) in
  let (data1, nt) =
    try Rewrite.rewrite data plan1 (term_of f)
    with
      | Rewritekit.Quit err -> raise err
      | Rewritekit.Stop err -> raise err
      | err -> raise err
  in
  let (scp1, qntenv1, tyenv1) = data1
  in
  (fast_make scp [f] nt, tyenv1)

let rewrite scp ?(dir=Rewrite.leftright) plan f =
  let (new_term, ntyenv) =
    rewrite_env scp ~dir:dir (Gtype.empty_subst()) plan f
  in
  new_term

(** [mk_rewrite_eq scp tyenv plan trm]: Make an equality by rewriting
    a term w.r.t a type context.  Returns [(trm=t, ntyenv)] where [t]
    is the result of rewriting [trm] with [plan] and [ntyenv] is the
    type environment generated during rewriting.
*)
let mk_rewrite_eq scp tyenv plan trm =
  let plan1 = extract_check_rules scp Rewrite.leftright plan in
  let (lhst, lst) = resolve_term scp trm in
  let data = (scp, Term.empty_subst(), tyenv) in
  let (data1, nt) =
    try Rewrite.rewrite data plan1 trm
    with
      | Rewritekit.Quit err -> raise err
      | Rewritekit.Stop err -> raise err
      | err -> raise err
  in
  let (scp1, _, tyenv2) = data1 in
  let eqtrm =  Lterm.mk_equality lhst nt in
  let rtrm =
    List.fold_left
      (fun b (q, _) -> Term.mk_qnt (Term.dest_bound q) b)
      eqtrm lst
  in
  make_full scp tyenv2 rtrm

(*
 * Pretty printing
 *)

let print inf x = Printers.print_term inf (term_of x)
let string_form x = Term.string_term (term_of x)

(*
 * Miscellaneous
 *)
let rec check_term p t =
  if (p t)
  then
    match t with
      | Basic.Qnt(q, b) -> check_term p b
      | Basic.App(f, a) -> check_term p f; check_term p a
      | _ -> ()
  else raise (Term.term_error "Term check failed" [t])
