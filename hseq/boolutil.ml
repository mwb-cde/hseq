(*----
  Copyright (c) 2006-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(*** Utility functions for boolean reasoning ***)

open Commands
open Tactics
open Lib.Ops

(** [find_unifier scp typenv varp trm exclude forms]: Find the
    first formula in forms which unifies with trm. Return the tag of
    the formula and the substitution cosntructed by unification. Ignore
    those formulas for which [?exclude] is true (if it is given).

    [varp] determines what is a bindable variable for unification.
    [typenv] is the type environment, to pass to the unifier.  [scp]
    is the scope, to pass to the unifier.  Raise Not_found if no
    unifiable formula is found.
*)
let find_unifier scp typenv varp trm exclude forms =
  let find_fn form =
    if exclude form
    then raise Not_found
    else
      (Tactics.drop_formula form,
       Unify.unify_typed typenv scp varp trm
         (Formula.term_of (Tactics.drop_tag form)))
  in
  Lib.find_first find_fn forms

(** [is_iff f]: Test whether [f] is a boolean equivalence.
*)
let is_iff f =
  try (fst (Term.dest_fun (Formula.term_of f)) = Lterm.iffid)
  with _ -> false

(** [is_qnt_opt kind pred form]: Test whether [form] satifies [pred].
    The formula may by quantified by binders of kind [kind].
*)
let is_qnt_opt kind pred form =
  Tactics.qnt_opt_of kind pred
    (Formula.term_of (Tactics.drop_tag form))

(** [dest_qnt_opt forms]: Destruct a possibly quantified tagged
    formula.  Returns the binders, the tag and the formula.
*)
let dest_qnt_opt kind tform =
  let tag = Tactics.drop_formula tform
  and form = Tactics.drop_tag tform in
  let (vs, term) = Term.strip_qnt kind (Formula.term_of form)
  in
  (tag, vs, term)

(** [find_qnt_opt kind ?f pred forms]

    Find the first formula in [forms] to satisfy [pred].  The formula
    may by quantified by binders of kind [kind].  Returns the binders,
    the tag and the formula.

    If [f] is given, the formula must be tagged with [f].

    Raises [Not_found] if no formula can be found which satisfies all
    the conditions.
*)
let find_qnt_opt kind pred forms =
  let find_fn tagged_form =
    Tactics.qnt_opt_of kind pred
      (Formula.term_of (Tactics.drop_tag tagged_form))
  in
  let tform = Lib.first find_fn forms in
  let tag = Tactics.drop_formula tform
  and form = Tactics.drop_tag tform in
  let (vs, term) = Term.strip_qnt kind (Formula.term_of form)
  in
  (tag, vs, term)

let fresh_thm scp th =
  Logic.is_fresh scp th

(** [dest_qnt_implies term]: Split a term of the form [! a .. b : asm
    => concl] into [( a .. b, asm, concl)].
*)
let dest_qnt_implies term =
  let thm_vars, body = Term.strip_qnt Term.All term
  in
  if Lterm.is_implies body
  then
    let (_, thm_asm, thm_concl) = Term.dest_binop body
    in
    (thm_vars, thm_asm, thm_concl)
  else
    raise (error "Badly formed theorem")

(** [unify_in_goal varp atrm ctrm goal]: Unify [atrm] with [ctrm] in
    the scope and type environment of [goal].  [varp] identifies the
    variables.
*)
let unify_in_goal varp atrm ctrm goal =
  let tyenv = typenv_of goal
  and scp = scope_of_goal goal
  in
  Unify.unify_typed tyenv scp varp atrm ctrm

(** [close_lambda_app term]: Form term [((% a1 .. an: B) v1 .. vn)],
    return [(% a1 .. an: (!x1 .. xn: B)), [v1; .. ; vn]) where the [x1
    .. xn] close unbound variables in [B].
*)
let close_lambda_app vars term  =
  let t1 = Lterm.gen_term vars term in
  let nvars, t1 = Term.strip_qnt Term.All t1 in
  let c0, cargs = Term.get_fun_args t1 in
  let cvars, c1 =
    let (vs, ct) = Term.strip_qnt Term.Lambda c0
    in
    (List.rev vs, ct)
  in
  let c2 = Term.rebuild_qnt cvars (Term.rebuild_qnt nvars c1)
  in
  (c2, cargs)
