(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** Typing and type-checking of terms *)

(** Type-checking a term [t] means infering the type of [t] and
    checking it against a given type [ty]. The result of type-checking
    is a type substitution in which type variables in [t] are assigned
    the types needed to make [t] well-typed.

    Type-checking tests all types for well-definedness (every
    identifier is in scope) as type inference is carried out.

    Using [typecheck] and [typecheck_top] is safe provided that the
    types of identifiers ([Id]) in terms are always assigned the type
    given to them by the scope before typechecking is carried out.
*)

(** {7 Typing Errors} *)

val typing_error: string -> Term.term -> Gtype.t -> Gtype.t -> exn
(** Construct a type error *)

val add_typing_error:
  string -> Term.term -> Gtype.t -> Gtype.t -> exn -> exn
(** Add a type error to existing errors. *)

(** {5 Typing a term} *)

val typeof_wrt:
  Scope.t -> Gtype.Subst.t -> Term.term -> Gtype.t
(** [typeof_wrt scp env t] Get the type of term [t] in scope [scp] wrt type
   environment [env]. *)

val typeof: Scope.t -> Term.term -> Gtype.t
(** Get the type of a term. *)

(** {5 Type-checking functions} *)

val typecheck: Scope.t -> Term.term -> Gtype.t -> unit
(** [typecheck scp t ty]: Check that term [t] has type [ty] in scope
    [scp].

    Identifiers ([Id(n, ty)]) are assumed to have already been
    assigned their types. The type used is [ty], whatever type [n] has
    in [scp].
*)

val typecheck_top: Scope.t
  -> Gtype.Subst.t
  -> Term.term -> Gtype.t
  -> Gtype.Subst.t
(** [typecheck_top tyenv scp t ty]: Check, w.r.t type context [tyenv],
    that term [t] has type [ty] in scope [scp]. Type variables in [t]
    take their assigned value from [tyenv], if they have one.

    Identifiers ([Id(n, ty)]) are assumed to have already been
    assigned their types. The type used is [ty], whatever type [n] has
    in [scp].
*)

val settype_env:
  Scope.t -> Gtype.Subst.t -> Term.term  -> Gtype.Subst.t
(** [settype_env scp env trm]: Type-check a term getting types from the
    scope. That is, the type of identifier terms [Id(n, ty)] is taken from the
    scope [scp] and [ty] is discarded. (This is a primitive type-checking
    function.) Adds replacements to the substitution [env]. *)

val settype:
  Scope.t -> Term.term  -> Gtype.Subst.t
(** Type-check a term getting types from the scope. That is, the type of
   identifier terms [Id(n, ty)] is taken from the scope [scp] and [ty] is
   discarded. (This is a primitive type-checking function.)  Returns the
   substition that populates the term with its correct types *)

(** {7 Debugging} *)
val typeof_env:
  Scope.t -> (int * Gtype.Subst.t)
  -> Term.term -> (Gtype.t * (int * Gtype.Subst.t))

val settype_aux:
  Scope.t ->
  (int * (Gtype.Subst.t -> Gtype.t -> Term.term -> Gtype.Subst.t))
  -> Gtype.t -> Term.term
  -> ((int * Gtype.Subst.t) -> (int * Gtype.Subst.t))

val test_type:
  Scope.t -> Gtype.Subst.t -> Term.term -> Gtype.t
  -> Gtype.t -> Gtype.Subst.t

type debugType = TermType of (string * Term.term * (Gtype.t list))
val debug_flag: bool ref
val debug_list: (debugType list) ref
