(*-----
 Name: typing.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* typing and typechecking of terms *)

(* error messages *)

(*
class typingError: string -> Basic.term list -> Basic.gtype list ->
  object
    inherit Result.error 
    method get_terms: unit -> Basic.term list
    method get_types: unit -> Basic.gtype list
  end
val typingError: string -> Basic.term list -> Basic.gtype list -> exn
val addtypingError: string -> Basic.term list -> Basic.gtype list -> exn -> 'a
*)
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

(* typechecking functions:
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
(* reset the types in a term using a given context/subsitution *)
(* substitutes variables with their concrete type in the context *)
val retype: Gtypes.substitution -> Basic.term -> Basic.term

(* retype_pretty: 
   as for retype, make substitution for type variables
   but also replace other type variables with new, prettier names 

   retype_pretty_env: 
   like retype_pretty but also return the substitution storing
   from the bindings/replacements generated during retyping.
 *)

val retype_pretty_env: Gtypes.substitution -> Basic.term 
  -> (Basic.term * Gtypes.substitution)
(*
   val retype_pretty: Gtypes.substitution -> Basic.term -> Basic.term
*)

val retype_pretty: Gtypes.substitution -> Basic.term 
  -> Basic.term 
*)

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

(* [set_exact_types scp term]
   get and set the exact types of identifiers (Term.Id) in term [trm]
   Unlike settype, does not do type inference nor does it make 
   copies of the types found in scope [scp].
   The types assigned to an identifier are the exact type of that
   identifier found in the given scope.
   Does not raise errors if identifiers not found in scope.
   This function needed to deal with the type of skolem constants.
 *)
(*
val set_exact_types: Scope.t -> Basic.term -> Basic.term
*)

(*
val assign_types: Scope.t -> Basic.term -> Basic.term
    
val typecheck_aux:
    Scope.t ->
      int ref * (Basic.ident * int, bool) Hashtbl.t ->
	Gtypes.substitution -> Basic.gtype -> Basic.term -> Gtypes.substitution
*)
