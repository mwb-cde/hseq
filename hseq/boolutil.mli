(*----
  Name: boolutil.mli
  Copyright Matthew Wahab 2006-2016
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

(** Utility functions for boolean reasoning *)

val find_unifier:
  Scope.t ->  Gtype.Subst.t
  -> (Basic.term -> bool)
  -> Basic.term -> ?exclude:(Logic.tagged_form -> bool)
  -> Logic.tagged_form list
  -> (Logic.ftag_ty * Term.Subst.t)
(** [find_unifier scp typenv varp trm ?exclude forms]: Find the first
    formula in [forms] which unifies with [trm]. Return the tag of the
    formula and the substitution cosntructed by unification. Ignore
    those formulas for which [?exclude] is true (if it is given).

    [varp] determines what is a bindable variable for unification.
    [typenv] is the type environment, to pass to the unifier.  [scp]
    is the scope, to pass to the unifier.  Raise Not_found if no
    unifiable formula is found.
*)

val is_iff: Formula.t -> bool
(** [is_iff f]: Test whether [f] is a boolean equivalence.
*)

val is_qnt_opt:
  Basic.quant -> (Basic.term -> bool)
  -> Logic.tagged_form -> bool
(** [is_qnt_opt kind pred form]: Test whether [form] satifies [pred].
    The formula may by quantified by binders of kind [kind].
*)

val dest_qnt_opt:
  Basic.quant->
  Logic.tagged_form -> (Logic.ftag_ty * Basic.binders list * Basic.term)
(** [dest_qnt_opt forms]: Destruct a possibly quantified tagged
    formula.  Returns the binders, the tag and the formula.
*)

(** [find_qnt_opt kind ?f pred forms]

    Find the first formula in [forms] to satisfy [pred].  The formula
    may by quantified by binders of kind [kind].  Returns the binders,
    the tag and the formula.

    If [f] is given, the formula must be tagged with [f].

    Raises [Not_found] if no formula can be found which satisfies all
    the conditions.
*)
val find_qnt_opt:
  Basic.quant
  -> (Basic.term -> bool)
  -> Logic.tagged_form list
  -> (Logic.ftag_ty * Basic.binders list * Basic.term)

val fresh_thm: Scope.t -> Logic.thm -> bool
(** [fresh_thm th]: Test whether theorem [th] is fresh (a formula of
    the current global scope).
*)

val dest_qnt_implies:
  Basic.term
  -> (Basic.binders list * Basic.term * Basic.term)
(** [dest_qnt_implies term]: Split a term of the form [! a .. b: asm
    => concl] into [( a .. b, asm, concl)].
*)

val unify_in_goal:
  (Basic.term -> bool)
  -> Basic.term -> Basic.term -> Logic.node
  -> Term.Subst.t
(** [unify_in_goal varp atrm ctrm goal]: Unify [atrm] with [ctrm] in
    the scope and type environment of [goal].  [varp] identifies the
    variables.
*)

val close_lambda_app:
  Basic.binders list
  -> Basic.term
  -> (Basic.term * Basic.term list)
(** [close_lambda_app term]: From term [((% a1 .. an: B) v1 .. vn)],
    return [(% a1 .. an: (!x1 .. xn: B)), [v1; .. ; vn]] where the [x1
    .. xn] close unbound variables in [B].
*)
