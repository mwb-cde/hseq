(*----
  Name: boolutil.mli
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

(** Utility functions for boolean reasoning *)

val find_unifier: 
  Scope.t ->  Gtypes.substitution 
  -> (Basic.term -> bool)
  -> Basic.term -> ?exclude:(Logic.tagged_form -> bool)
  -> Logic.tagged_form list 
  -> (Tag.t * Term.substitution)
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
  Logic.tagged_form -> (Tag.t * Basic.binders list * Basic.term)
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
  -> (Tag.t * Basic.binders list * Basic.term)

val fresh_thm: Logic.thm -> bool
(** [fresh_thm th]: Test whether theorem [th] is fresh (a formula of
    the current global scope).
*)

val get_type_name: Basic.gtype -> Hident.t
(** [get_type_name ty]: Get the identifier of the constructor of type
    [ty].
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
  -> Term.substitution
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

(***
val set_info: 
  Logic.info option
  -> (Tag.t list * Tag.t list * Tag.t list * Basic.term list)
  -> unit
(** [set_info info (gs, asms, concls, consts)]: Set [info] to
    [subgoals = gs], [aformulas = asms], [cformulas = concls] and
    [constants=consts]
*)
****)
