(*-----
  Name: boolutil.mli
  Author: M Wahab <mwahab@users.sourceforge.net>
  Copyright M Wahab 2006
  ----*)

(** Utility functions for boolean reasoning *)

val find_unifier: 
    Scope.t ->  Gtypes.substitution 
      -> (Basic.term -> bool)
	-> Basic.term -> ?exclude:(Logic.tagged_form -> bool)
	  -> Logic.tagged_form list 
	    -> (Tag.t * Term.substitution)
(**
   [find_unifier scp typenv varp trm ?exclude forms]: Find the first
   formula in [forms] which unifies with [trm]. Return the tag of the
   formula and the substitution cosntructed by unification. Ignore
   those formulas for which [?exclude] is true (if it is given).

   [varp] determines what is a bindable variable for unification.
   [typenv] is the type environment, to pass to the unifier.
   [scp] is the scope, to pass to the unifier.
   Raise Not_found if no unifiable formula is found.
 *)

val is_qnt_opt:
    Basic.quant -> (Basic.term -> bool)
  -> Logic.tagged_form -> bool
(**
   [is_qnt_opt kind pred form]: Test whether [form] satifies [pred].
   The formula may by quantified by binders of kind [kind]. 
*)

val dest_qnt_opt:
  Basic.quant->
    Logic.tagged_form -> (Tag.t * Basic.binders list * Basic.term)
(**
   [dest_qnt_opt forms]: Destruct a possibly quantified tagged formula.
   Returns the binders, the tag and the 
formula.
*)

(**
   [find_qnt_opt kind ?f pred forms] 

   Find the first formula in [forms] to satisfy [pred].  The formula
   may by quantified by binders of kind [kind].  Returns the binders,
   the tag and the formula.

   if [f] is given, the formula must be tagged with [f]. 

   Raises [Not_found] if no formula can be found which satisfies all the
   conditions.
 *)
val find_qnt_opt: 
    Basic.quant 
  -> (Basic.term -> bool)
    -> Logic.tagged_form list 
      -> (Tag.t * Basic.binders list * Basic.term)

val fresh_thm: Logic.thm -> bool
(**
     [fresh_thm th]: Test whether theorem [th] is fresh (a formula of
     the current global scope).
*)

val get_type_name: Basic.gtype -> Ident.t
(**
    [get_type_name ty]: Get the identifier of the constructor of type
    [ty].
*)
