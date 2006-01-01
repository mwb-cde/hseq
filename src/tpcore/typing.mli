(*-----
   Name: typing.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(** Typing and type-checking of terms *)

(**
   Type-checking a term [t] means infering the type of [t] and
   checking it against a given type [ty]. The result of type-checking
   is a type substitution in which type variables in [t] are assigned
   the types needed to make [t] well-typed.

   Type-checking tests all types for well-definedness (every
   identifier is in scope) type inference is carried out. 

   Using [typecheck] and [typecheck_top] is safe provided that the
   types of identifiers ([Id]) in terms are always assigned the type
   given to them by the scope before typechecking is carried out.
*)

(** {7 Typing Errors} *)

class typingError: string -> Basic.term -> Basic.gtype -> Basic.gtype ->
  object
    inherit Result.error 
    method get_term: unit -> Basic.term 
    method get_types: unit -> Basic.gtype * Basic.gtype
  end

val typing_error: string -> Basic.term -> Basic.gtype -> Basic.gtype -> exn
(** Construct a type error *)

val add_typing_error: 
    string -> Basic.term -> Basic.gtype -> Basic.gtype -> exn -> exn
(** Add a type error to existing errors. *)

(** {5 Typing a term} *)

val typeof : Scope.t -> Basic.term -> Basic.gtype
(** Get the type of a term *)

(** {5 Type-checking functions} *)

val typecheck: Scope.t -> Basic.term -> Basic.gtype -> unit
(** 
   [typecheck scp t ty]: Check that term [t] has type [ty] in scope [scp]. 

   Identifiers ([Id(n, ty)]) are assumed to have already been assigned
   their types. The type used is [ty], whatever type [n] has in [scp].
*)

val typecheck_top: Scope.t  
  -> Gtypes.substitution
    -> Basic.term -> Basic.gtype 
      -> Gtypes.substitution
(**
   [typecheck_top tyenv scp t ty]: Check, w.r.t type context [tyenv],
   that term [t] has type [ty] in scope [scp]. Type variables in [t]
   take their assigned value from [tyenv], if they have one.

   Identifiers ([Id(n, ty)]) are assumed to have already been assigned
   their types. The type used is [ty], whatever type [n] has in [scp].
*)

val settype: Scope.t -> Basic.term  -> Gtypes.substitution
(**
   Type-check a term. The types of identifier terms (built from [Id(n,
   ty)]) are taken from the scope ([ty] is discarded). (This is a
   primitive type-checking function.)
*)


