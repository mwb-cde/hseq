(*----
  Copyright (c) 2019-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** Skolem constants

   Skolem constants are introduced by quantifier rules, to instantiate a
   quantified formula.

   The generation of skolem constants uses information stored in subgoals. Each
   skolem constant is a meta term, ([Term.Meta]), with a name which is unique in
   the subgoal. The identifier is added to the scope of the subgoal and can be
   used to instantiate a formula in the subgoal.

   A skolem constant is initially assigned a weak type variable which is then
   unified with the type required of the constant.
 *)

(** The record of an individual skolem constant. *)
type skolem_cnst

val get_sklm_name: skolem_cnst -> Ident.t
val get_sklm_indx: skolem_cnst -> int
val get_sklm_type: skolem_cnst -> Gtype.t

(** Information needed to generate a new skolem constant *)
type new_skolem_data =
  {
    name: Ident.t;
    ty: Gtype.t;
    tyenv: Gtype.Subst.t;
    scope: Scope.t;
    skolems: (skolem_cnst)list;
    tylist: (string * int)list
  }
(** [name] The desired name of the skolem constant. The theory part must be
      the theory of the goal. The name part is usually the name of the bound
      variable being instantiated.

      [ty] The type of the bound variable being instantiated.

      [tyenv] The type environment of the subgoal.

      [scope] The scope of the subgoal.

      [tylist] A list of type names already in use (used to generate the name of
     the type variable.  *)

val mk_new_skolem:
  new_skolem_data
  -> (Term.term  * Gtype.t * (skolem_cnst)list
      * Gtype.Subst.t * (string * int) list)

       (** [mk_new_skolem data] constructs a new skolem. Returns [(sv,
    sty, skolems, tyenv, tylist]) where [sv] is the new skolem
    constant, [sty] is the type of the skolem constant, [skolems]
    is the updated skolems record, [tyenv] is the type environment
    updated when making the skolems' type and [tylist] is the
    updated list of type variable names.  *)
