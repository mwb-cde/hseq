(*-----
 Name: defn.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* Type and term definition and declaration *)

(* Terms *)

(*
   decln: type of declarations. 

   This is stored in theories and its 
   definition must be kept hidden.
*)
(*
type decln 
*)
(*
 defn: the ocaml type of a definition 
   This is used only for prettyprinting and can be.
   (The Logic.thm describing the definition is an abstracted type)
*)
(*
type defn = Defn of (Basic.ident * Basic.gtype * Logic.thm)
*)

(* destructors *)
(*
val dest_decln : decln -> Basic.ident * Basic.gtype
val dest_defn : defn -> (Basic.ident * Basic.gtype * Logic.thm)
*)

(* destruct a term of the form (f a1 a2 ..)=G to (f, [a1; a2; ..]) *)
val get_lhs : Basic.term -> 
  Basic.ident * (Basic.ident * Basic.gtype) list   

(* function declarations of the type (f: ty) *)
val mk_decln :
    Gtypes.scope ->
      Basic.ident -> Basic.gtype -> (Basic.ident * Basic.gtype)

(* make the type of a defined term *)
val mk_defn_type :
    Gtypes.substitution ->
      ('a * Basic.gtype) list ->
	Basic.gtype -> ('a * Basic.gtype) list -> Basic.gtype

(* make a definition *)
(* [mk_defn scp id args t]
   scp is the scope of the definition
   id is the identifier
   args are the list of parameters identifiers and types 
   t is the body of the definition 
 *)
val mk_defn :
    Gtypes.scope ->
      Basic.ident->
	(string * Basic.gtype) list -> Basic.term 
	  -> (Basic.ident * Basic.gtype * Formula.form)


val mk_all_from_list: Gtypes.scope -> Basic.term 
  -> Basic.term list ->  Basic.term


(* Types *)

(* Type definition: subtypes *)


val check_args_unique : string list -> unit

(*
   [mk_subtype_exists setp]:
   make the term << ?x: setp x >>
   to be used to show the subtype is not empty.
*)
val mk_subtype_exists: Basic.term -> Basic.term

val check_type_name: Gtypes.scope -> Basic.ident -> unit
val check_well_defined : Gtypes.scope -> string list -> Basic.gtype -> unit
val make_witness_type: 
    Gtypes.scope -> Basic.gtype -> Basic.term -> Basic.term

val extend_scope_typedef:
    Gtypes.scope -> Basic.ident -> string list -> Gtypes.scope

val extend_scope_identifier:
    Gtypes.scope -> Basic.ident -> Basic.gtype -> Gtypes.scope


(*
 *  Type definition. 
 *  A, args, T, set:(args)T->bool, rep_name, abs_name
 * 
 *  make 
 *  Declarations:
 *   representation function rep = rep_name:(args)T -> A
 *   abstraction function rep = abs_name:A-> (args)T 
 *
 *  Axioms:
 *   Rep_T: |- !x: set (rep_name x)
 *   Rep_T_inverse: |- !x: abs_name (rep_name x) = x
 *   Abs_T_inverse: |- !x: (set x) => rep_name (abs_name x) = x
 *   
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

val mk_subtype:
    Gtypes.scope -> string -> string list 
      -> Basic.gtype -> Basic.term -> string -> string
	-> subtype_defn



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
    Gtypes.scope -> string -> string list 
      -> Basic.gtype -> Basic.term -> Basic.ident
	-> (Basic.gtype * Basic.term * Basic.term)

end
