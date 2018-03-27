(*----
  Name: ltype.mli
  Copyright Matthew Wahab 2018
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

(** Manipulating types of the logic. *)

val in_scope:
  (string, bool)Lib.substype -> Scope.t -> Gtypes.t -> bool
(** [in_scope memo scp th ty]: Check that [ty] is in scope by checking
    that every type constructor is decared or defined in scope [scp].

    The function is memoised: if a constructor name is found to be
    in scope, it is added to [memo].
*)

val set_name:
  ?memo:(string, Ident.thy_id)Hashtbl.t
  -> Scope.t -> Gtypes.t -> Gtypes.t
(** [set_name ?strict ?memo scp ty]: set names in type [ty] to their
    long form.

    [memo] is an optional memoisation table.
*)

val unfold: Scope.t -> Gtypes.t -> Gtypes.t
(**
   [unfold scp ty]: Unfold the definition of type [ty] from the scope
   [scp].

   @raise [Not_found] if no definition.
*)

val well_formed_full:
  (Gtypes.t -> (string * Gtypes.t)option)
  -> Scope.t -> Gtypes.t -> bool
(** [well_formed_full pred scp t]: ensure that [t] is well-formed

   See [Gtypes.well_formed_full] for a description
*)

val well_formed: Scope.t -> Gtypes.t -> bool
(** [well_formed scp t]: ensure that [t] is well-formed in scope [scp] *)

val well_defined: Scope.t -> (string)list -> Gtypes.t -> unit
(** [well_defined scp args ty]: Test [ty] for well-definedness. every
    constructor occuring in [ty] must be defined. Variables in [ty]
    must have a name in [args] and weak variables are not permitted in
    [ty].
*)

val check_decl_type: Scope.t -> Gtypes.t -> unit
(** [check_decl_type scp ty]: Ensure type [ty] is suitable for the
    declaration of a term. Fails if [ty] contains a weak variable.
*)

val unify_env:
  Scope.t -> Gtypes.t -> Gtypes.t
  -> Gtypes.substitution -> Gtypes.substitution
(** [unify_env scp ty1 ty2 env]: Unify two types in context [env],
    return a new subsitution.
*)

val unify: Scope.t -> Gtypes.t -> Gtypes.t -> Gtypes.substitution
(** [unify]: unify two types, returning the substitution.
*)

val matching_env:
  Scope.t -> Gtypes.substitution
  -> Gtypes.t -> Gtypes.t -> Gtypes.substitution
(**
   [matching_env scp env t1 t2]: Match type [t1] with type [t2] w.r.t
   context [env]. This unifies [t1] and [t2], but only variables in
   type [t1] can be bound.

   Raises an exception if matching fails.
*)

val matches_env:
  Scope.t -> Gtypes.substitution
  -> Gtypes.t -> Gtypes.t -> Gtypes.substitution
(** [matches_env scp env t1 t2]: Match type [t1] with type [t2] w.r.t
    context [env]. This unifies [t1] and [t2], but only variables in
    type [t1] can be bound.

    Silently returns unchanged substitution on failure.
*)

val matches: Scope.t -> Gtypes.t -> Gtypes.t -> bool
(** Toplevel for [matches_env]. *)
