(*-----
 Name: defn.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** Term and subtype definition *)

(** {5 Term definition and declaration} *)

(** {7 Term declaration} *)

val mk_decln :
    Scope.t ->
      Basic.ident -> Basic.gtype -> (Basic.ident * Basic.gtype)
(* Declare a term identifier [(f:ty)] *)

(** {7 Term definition} *)

val mk_defn :
    Scope.t ->
      Basic.ident->
	Basic.term list -> Basic.term 
	  -> (Basic.ident * Basic.gtype * Formula.form)
(**
   [mk_defn scp id args t]: Construct a definition.

   [scp] is the scope of the definition
   [id] is the identifier being defined.
   [args] are the list of parameters identifiers and types 
   [t] is the body of the definition 
*)

(** {5 Type definition} *)

(** {7 Support functions} *)

val check_args_unique : string list -> unit
(** 
   [check_args_unqiues args]: Ensure that all argument names in [args]
   are unique.
*)

val check_type_name: Scope.t -> Basic.ident -> unit
(**
   [check_type_name scp n]: Make sure that there is no type named [n]
   in scope [scp].
*)

val check_well_defined : Scope.t -> string list -> Basic.gtype -> unit
(** 
   [check_well_defined scp args ty]: Make sure that type [ty] is well
   defined in scope [scp]. Every type variable in [ty] must have have
   a name in [args], weak variables are not permitted and every
   constructor must be declared in [scp].
*)

(** {7 Subtype definition} 

   Define the subtype of a type constructing
   [A, args, T, set:(args)T->bool, rep, abs]
   where 
   {ul
   {- [A] is the name of the subtype.}
   {- [args] are the names of the types' parameters.}
   {- [T] is the representation type.}
   {- [rep] is the name of the representation function
   destructing type [A]}
   {- [abs] is the name of the abstraction function constructing
   type [A].}}

   The types of the constructed functions are
   {ul
   {- representation function:  [rep:(args)T -> A]}
   {- abstraction function: [abs:A-> (args)T]}}
 
   The axioms specifying the abstraction and representation functions are:
   {ul
   {- rep_T: |- !x: set (rep x)}
   {- rep_T_inverse: |- !x: abs (rep x) = x}
   {- abs_T_inverse: |- !x: (set x) => rep (abs x) = x}}

   The names of the type, abstraction and representation functions are
   all parameters to the subtype definition.
*)

type subtype_defn = 
    {
     id: Basic.ident;
     args: string list;
     rep : (Basic.ident* Basic.gtype);
     abs: (Basic.ident* Basic.gtype);
     set: Basic.term;
     rep_T: Basic.term;
     rep_T_inverse: Basic.term;
     abs_T_inverse: Basic.term
   }
(** 
   The result of constructing a subtype:
   {ul
   {- [id]: The name of the new type.}
   {- [args]: The names of the parameters to the type.}
   {- [rep]: The declaration of the representation function.}
   {- [abs]: The declaration of the abstraction function.}
   {- [set]: The term providing the defining predicate of the subtype.}
   {- [rep_T]: A term of the form |- !x: set (rep x)}
   {- [rep_T_inverse]: A term of the form |- !x: abs (rep x) = x}
   {- [abs_T_inverse]: A term of the form |- !x: (set x) => rep
   (abs x) = x}}
*)


val mk_subtype_exists: Basic.term -> Basic.term
(**
   [mk_subtype_exists setp]: Make the term << ?x: setp x >> to be used
   to show that the subtype is not empty.
*)

val make_witness_type: 
    Scope.t -> Basic.gtype -> Basic.term -> Basic.term
(** 
   [make_witness_type scp ty setp]: Make a witness to the
   non-emptyness of the set defined by [setp] (which must be of type
   [ty -> bool]).
*)


val mk_subtype:
    Scope.t -> string -> string list 
      -> Basic.gtype -> Basic.term -> string -> string
	-> subtype_defn
(** 
   [mk_subtype scp n args ty set rep abs]: Make a subtype named [n]
   with parameters [args] from type [ty]. Term [set] is the defining
   set and must be of type [ty -> bool]. Strings [rep] and [abs] are
   the names to use for the representation and abstraction functions.
*)


(*
module HolLike :
sig

(*
 * HOL-like type definition. 
 *  A, args, T, set:(args)T->bool
 * 
 *  make declaration
 *   representation function rep = name:(args)T -> A
 *   and theorem
 *   |- ((!x1 x2: (((rep x1) = (rep x2)) => (x1 = x2)))
 *       and (!x: (P x) = (?x1: x=(rep x1))))
 *
 * Everything needed to use subtyping is derived making this approach
 * the more intellectually rigorous. But this takes a lot of work,
 * so the standard typedef takes the easy way out.
 * 
 *)

(*
   [mk_subtype_prop setp rep]:
   make the term 
   << (!x1 x2: (((rep x1) = (rep x2)) => (x1 = x2)))
      and 
      (!x: (P x) = (?x1: x=(rep x1)))>>
   to be used as the subtype theorem.

   [mk_subtype scp name args d setP rep]:
   - check name doesn't exist already
   - check all arguments in args are unique
   - check def is well defined 
   (all constructors exist and variables are in the list of arguments)
   - ensure setP has type (d -> bool)
   - declare rep as a function of type (d -> n)
   - make subtype property from setp and rep.
*)
val mk_subtype_prop: Basic.term -> Basic.ident -> Basic.term
val mk_subtype:
    Scope.t -> string -> string list 
      -> Basic.gtype -> Basic.term -> Basic.ident
	-> (Basic.gtype * Basic.term * Basic.term)

end
*)
