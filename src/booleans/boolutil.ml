(*----
  Name: boolutil.ml
  Copyright M Wahab 2006-2010
  Author: M Wahab  <mwb.cde@googlemail.com>

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

(*** Utility functions for boolean reasoning ***)

open Commands
open Tactics
open Lib.Ops

(** [find_unifier scp typenv varp trm ?exclude ?f forms]: Find the
    first formula in forms which unifies with trm. Return the tag of
    the formula and the substitution cosntructed by unification. Ignore
    those formulas for which [?exclude] is true (if it is given).

    [varp] determines what is a bindable variable for unification.
    [typenv] is the type environment, to pass to the unifier.  [scp]
    is the scope, to pass to the unifier.  Raise Not_found if no
    unifiable formula is found.
*)
let find_unifier scp typenv varp trm ?exclude forms = 
  let not_this = Lib.get_option exclude (fun _ -> false) in 
  let find_fn form =
    if not_this form
    then raise Not_found
    else 
      Tactics.drop_formula form,
      Unify.unify ~typenv:typenv scp varp trm 
	(Formula.term_of (Tactics.drop_tag form))
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

let fresh_thm th = Logic.is_fresh (Global.scope()) th

let get_type_name ty =
  match ty with
    | Basic.Constr (id, _) -> id
    | _ -> failwith "get_type_name"

(** [dest_qnt_implies term]: Split a term of the form [! a .. b : asm
    => concl] into [( a .. b, asm, concl)].
*)
let dest_qnt_implies term = 
  let thm_vars, body = Term.strip_qnt Basic.All term
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
  and scp = scope_of goal
  in 
  Unify.unify ~typenv:tyenv scp varp atrm ctrm

(** [close_lambda_app term]: Form term [((% a1 .. an: B) v1 .. vn)],
    return [(% a1 .. an: (!x1 .. xn: B)), [v1; .. ; vn]) where the [x1
    .. xn] close unbound variables in [B].
*)
let close_lambda_app vars term  = 
  let t1 = Lterm.gen_term vars term in 
  let nvars, t1 = Term.strip_qnt Basic.All t1 in 
  let c0, cargs = Term.get_fun_args t1 in 
  let cvars, c1 = 
    let (vs, ct) = Term.strip_qnt Basic.Lambda c0
    in 
    (List.rev vs, ct)
  in 
  let c2 = Term.rebuild_qnt cvars (Term.rebuild_qnt nvars c1)
  in 
  (c2, cargs)
    

