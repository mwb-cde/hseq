(* typing and typechecking of terms *)

(* error messages *)

class typingError: string -> Term.term list -> Gtypes.gtype list ->
  object
  inherit Result.error 
  method get_terms: unit -> Term.term list
  method get_types: unit -> Gtypes.gtype list
end
val typingError: string -> Term.term list -> Gtypes.gtype list -> exn
val addtypingError: string -> Term.term list -> Gtypes.gtype list -> exn -> 'a

(* construct type of a term *)
    val typeof : Gtypes.scope -> Term.term -> Gtypes.gtype

(* typechecking functions:
   the typecheck is deep:  type are tested for well-definedness
   and type inference is carried out.
   This is used e.g. to check that a term (and all its subterms)
   is type-correct in a given scope.
*)

(* check a given term has the expected type *)
    val typecheck: Gtypes.scope -> Term.term 
      -> Gtypes.gtype -> unit

(* check a given term has the expected type 
   in a given context/substitution  *) 

val typecheck_env: Gtypes.scope  
  -> Gtypes.substitution
    -> Term.term   -> Gtypes.gtype 
      -> Gtypes.substitution
	  
val simple_typecheck_env: Gtypes.scope  
  -> Gtypes.substitution
    -> Term.term   -> Gtypes.gtype 
      -> Gtypes.substitution
	  
(* reset the types in a term using a given context/subsitution *)
(* substitutes variables with their concrete type in the context *)
val retype: Gtypes.substitution -> Term.term -> Term.term

(* retype_pretty: 
   as for retype, make substitution for type variables
   but also replace other type variables with new, prettier names 
*)

val retype_pretty: Gtypes.substitution -> Term.term -> Term.term

(* Assign the variable types in a term their required type
   to ensure well typed term, returning the required subsititution 
   Also checks that defined types are valid and well defined in the given 
   scope.*)

val settype: Gtypes.scope -> Term.term  -> Gtypes.substitution

(* Assign the variable types in a term their required type
   to ensure well typed term, returning the required subsititution 
   Also checks that defined types are valid and well defined in the given 
   scope.*)

(* infer_types is an alternative to settype but not actually used *)

val infer_aux : 
   int ref * (Basic.fnident * int, bool) Hashtbl.t ->
     Gtypes.scope -> Gtypes.substitution -> Term.term -> Gtypes.gtype
 val infer_types_env: Gtypes.scope -> Gtypes.substitution 
   -> Term.term -> Gtypes.gtype

 val infer_types: Gtypes.scope -> Term.term -> Gtypes.gtype

(* check that types in the term are well defined *)
    val check_types: Gtypes.scope -> Term.term -> unit

(* [set_exact_types scp term]
   get and set the exact types of identifiers (Term.Var) in term [trm]
   Unlike settype, does not do type inference nor does it make 
   copies of the types found in scope [scp].
   The types assigned to an identifier are the exact type of that
   identifier found in the given scope.
   Does not raise errors if identifiers not found in scope.
   This function needed to deal with the type of skolem constants.
 *)

val set_exact_types: Gtypes.scope -> Term.term -> Term.term

val assign_types: Gtypes.scope -> Term.term -> Term.term
	
