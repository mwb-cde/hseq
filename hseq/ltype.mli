(*----
  Copyright (c) 20018-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** Manipulating types of the logic. *)

val in_scope: Scope.t -> Gtype.t -> bool

val in_scope_memoized:
  Lib.StringSet.t -> Scope.t -> Gtype.t
  -> (bool * Lib.StringSet.t)
(** [in_scope memo scp th ty]: Check that [ty] is in scope by checking
    that every type constructor is decared or defined in scope [scp].

    The function is memoised: if a constructor name is found to be
    in scope, it is added to [memo].
*)

val set_name: Scope.t -> Gtype.t -> Gtype.t
val set_name_memoized:
  (string)Lib.StringMap.t
  -> Scope.t -> Gtype.t
  -> (Gtype.t * (string)Lib.StringMap.t)
(** [set_name scp ty]: set names in type [ty] to their
    long form.

    [set_name_memoized] is the memoized form
*)

val unfold: Scope.t -> Gtype.t -> Gtype.t
(**
   [unfold scp ty]: Unfold the definition of type [ty] from the scope
   [scp].

   @raise [Not_found] if no definition.
*)

val well_formed_full:
  (Gtype.t -> (string * Gtype.t)option)
  -> Scope.t -> Gtype.t -> bool
(** [well_formed_full pred scp t]: ensure that [t] is well-formed

   See [Gtype.well_formed_full] for a description
*)

val well_formed: Scope.t -> Gtype.t -> bool
(** [well_formed scp t]: ensure that [t] is well-formed in scope [scp] *)

val well_defined: Scope.t -> (string)list -> Gtype.t -> unit
(** [well_defined scp args ty]: Test [ty] for well-definedness. every
    constructor occuring in [ty] must be defined. Variables in [ty]
    must have a name in [args] and weak variables are not permitted in
    [ty].
*)

val check_decl_type: Scope.t -> Gtype.t -> unit
(** [check_decl_type scp ty]: Ensure type [ty] is suitable for the
    declaration of a term. Fails if [ty] contains a weak variable.
*)

val unify_env:
  Scope.t -> Gtype.t -> Gtype.t
  -> Gtype.Subst.t -> Gtype.Subst.t
(** [unify_env scp ty1 ty2 env]: Unify two types in context [env],
    return a new subsitution.
*)

val unify: Scope.t -> Gtype.t -> Gtype.t -> Gtype.Subst.t
(** [unify]: unify two types, returning the substitution.
*)

val matching_env:
  Scope.t -> Gtype.Subst.t
  -> Gtype.t -> Gtype.t -> Gtype.Subst.t
(**
   [matching_env scp env t1 t2]: Match type [t1] with type [t2] w.r.t
   context [env]. This unifies [t1] and [t2], but only variables in
   type [t1] can be bound.

   Raises an exception if matching fails.
*)

val matches_env:
  Scope.t -> Gtype.Subst.t
  -> Gtype.t -> Gtype.t -> Gtype.Subst.t
(** [matches_env scp env t1 t2]: Match type [t1] with type [t2] w.r.t
    context [env]. This unifies [t1] and [t2], but only variables in
    type [t1] can be bound.

    Silently returns unchanged substitution on failure.
*)

val matches: Scope.t -> Gtype.t -> Gtype.t -> bool
(** Toplevel for [matches_env]. *)
