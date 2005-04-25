(*-----
   Name: typing.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(* typing and typechecking of terms *)

(* error messages *)

class typingError: string -> Basic.term -> Basic.gtype -> Basic.gtype ->
  object
    inherit Result.error 
    method get_term: unit -> Basic.term 
    method get_types: unit -> Basic.gtype * Basic.gtype
  end
val typing_error: string -> Basic.term -> Basic.gtype -> Basic.gtype -> exn
val add_typing_error: 
    string -> Basic.term -> Basic.gtype -> Basic.gtype -> exn -> exn


(* construct type of a term *)
val typeof : Scope.t -> Basic.term -> Basic.gtype

(*
   typechecking functions:
   the typecheck is deep:  type are tested for well-definedness
   and type inference is carried out.
   This is used e.g. to check that a term (and all its subterms)
   is type-correct in a given scope.
 *)

(* check a given term has the expected type *)
val typecheck: Scope.t -> Basic.term 
  -> Basic.gtype -> unit

(* check a given term has the expected type 
   in a given context/substitution  *) 

val typecheck_env: Scope.t  
  -> Gtypes.substitution
    -> Basic.term   -> Basic.gtype 
      -> Gtypes.substitution
	  
val simple_typecheck_env: Scope.t  
  -> Gtypes.substitution
    -> Basic.term   -> Basic.gtype 
      -> Gtypes.substitution
(*
   Assign the variable types in a term their required type
   to ensure well typed term, returning the required subsititution 
   Also checks that defined types are valid and well defined in the given 
   scope.
 *)
val settype: Scope.t -> Basic.term  -> Gtypes.substitution

(* Assign the variable types in a term their required type
   to ensure well typed term, returning the required subsititution 
   Also checks that defined types are valid and well defined in the given 
   scope.
 *)

(* infer_types is an alternative to settype but not actually used *)

val infer_aux : 
    int ref * (Basic.ident * int, bool) Hashtbl.t 
  -> Scope.t -> Gtypes.substitution -> Basic.term 
    -> (Basic.gtype * Gtypes.substitution )
val infer_types_env: Scope.t -> Gtypes.substitution 
  -> Basic.term -> (Basic.gtype* Gtypes.substitution)

val infer_types: Scope.t -> Basic.term -> Basic.gtype

(* check that types in the term are well defined *)
val check_types: Scope.t -> Basic.term -> unit


