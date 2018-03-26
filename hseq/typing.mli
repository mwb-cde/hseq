(*----
  Name: typing.mli
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

val typing_error: string -> Basic.term -> Gtypes.gtype -> Gtypes.gtype -> exn
(** Construct a type error *)

val add_typing_error:
  string -> Basic.term -> Gtypes.gtype -> Gtypes.gtype -> exn -> exn
(** Add a type error to existing errors. *)

(** {5 Typing a term} *)

val typeof:
  Scope.t
  -> ?env:Gtypes.substitution
  -> Basic.term -> Gtypes.gtype
(** Get the type of a term wrt type environment [env] (if given). *)

(** {5 Type-checking functions} *)

val typecheck: Scope.t -> Basic.term -> Gtypes.gtype -> unit
(** [typecheck scp t ty]: Check that term [t] has type [ty] in scope
    [scp].

    Identifiers ([Id(n, ty)]) are assumed to have already been
    assigned their types. The type used is [ty], whatever type [n] has
    in [scp].
*)

val typecheck_top: Scope.t
  -> Gtypes.substitution
  -> Basic.term -> Gtypes.gtype
  -> Gtypes.substitution
(** [typecheck_top tyenv scp t ty]: Check, w.r.t type context [tyenv],
    that term [t] has type [ty] in scope [scp]. Type variables in [t]
    take their assigned value from [tyenv], if they have one.

    Identifiers ([Id(n, ty)]) are assumed to have already been
    assigned their types. The type used is [ty], whatever type [n] has
    in [scp].
*)

val settype:
  Scope.t -> ?env:Gtypes.substitution
  -> Basic.term  -> Gtypes.substitution
(** Type-check a term. The types of identifier terms (built from
    [Id(n, ty)]) are taken from the scope ([ty] is discarded). (This is
    a primitive type-checking function.)
*)


(** {7 Debugging} *)
val typeof_env:
  Scope.t -> (int * Gtypes.substitution)
  -> Basic.term -> (Gtypes.gtype * (int * Gtypes.substitution))

val settype_aux:
  Scope.t ->
  (int
   * (Gtypes.substitution -> Gtypes.gtype -> Basic.term
      -> Gtypes.substitution))
  -> Gtypes.gtype -> Basic.term ->
  ((int * Gtypes.substitution) -> (int * Gtypes.substitution))

val test_type:
  Scope.t -> Gtypes.substitution -> Basic.term -> Gtypes.gtype
  -> Gtypes.gtype -> Gtypes.substitution

type debugType = TermType of (string * Basic.term * (Gtypes.gtype list))
val debug_flag: bool ref
val debug_list: (debugType list) ref
