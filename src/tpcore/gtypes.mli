(*-----
 Name: gtypes.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** {4 Types and their manipulation} *)

open Basic
open Lib

(** {5 Basic Operations} *)

(** [equals]: Syntactic equality between types *)
val equals: gtype -> gtype -> bool

(* Recognisers *)

val is_var : gtype -> bool
val is_constr : gtype  -> bool
val is_base : gtype  -> bool
val is_weak : gtype -> bool

(* Constructors *)

val mk_var : string -> gtype
val mk_weak : string -> gtype
val mk_constr: Basic.typ_const -> gtype list -> gtype
val mk_base: Basic.base_typ -> gtype

(* Destructors *)

val dest_var : gtype -> string  ref
val get_var_name : gtype -> string

val dest_weak : gtype -> string  ref
val get_weak_name : gtype -> string

val dest_constr:  gtype -> (Basic.typ_const * gtype list)
val dest_base:  gtype -> Basic.base_typ

(** {5 Specialised Manipulators} *)

(* Base types *)
val mk_num: gtype
val mk_ind: gtype

(* Variable types *)

(** [is_any_var t]: true if [t] is a variable or a weak variable *)
val is_any_var: gtype -> bool

(** 
   [get_var_names ty]: get names of variables occuring in type.
*)
val get_var_names: gtype -> string list

(**
   [normalize_vars ty]:
   Make all type variables with the same string name
   be the same variable.

   Useful when constructing types from existing types.
*)
val normalize_vars : gtype -> gtype   

(**
   [mk_new_type n]: make a new type variable with a name derived from
   [!n]; increment [n] and return the new type. Different values of
   [!n] make different names.
*)
val mk_typevar: int ref -> gtype

(** 
   [get_var_names ty]: Get the names of variables in [ty]. Ignores
   weak variables. 
*)
val get_var_names: gtype -> string list

(* Unnamed type variables *)

(** [mk_null()]: Make an unnamed type variable *)
val mk_null: unit -> gtype
val is_null: gtype -> bool

(* Named typed constructors *)
val is_def : gtype -> bool
val mk_def: ident -> gtype list -> gtype
val dest_def: gtype -> (ident * gtype list)

(** {5 Type Definitions} *)

(** [typedef_record]: Records for type definitions *)
type typedef_record = Scope.type_record

(**
   [get_typdef scp n]: get definition of type named [n] from scope [scp]
*)
val get_typdef: Scope.t -> ident -> typedef_record

(** {5 Data storage indexed by gtypes} *)

(** [('a)table]: hashtables with gtype as the key *)
module type RHASHKEYS=
  sig 
    type t = gtype
    val equal : t -> t -> bool
    val hash: t -> int
  end
module type RHASH = (Hashtbl.S with type key = (gtype))
module Rhash: RHASH

type ('a)table = ('a)Rhash.t

(** [('a)tree]: Balanced trees indexed by gtypes *)
module TypeTreeData: Treekit.TreeData
(*
    sig
      type key=gtype
      val equals : key -> key -> bool
      val lessthan : key -> key -> bool
    end
*)
module TypeTree:
    sig
      val eql : gtype -> gtype -> bool
      val lessthan : gtype -> gtype -> bool
      type 'a t 
      val nil : 'a t
      val create : (gtype * 'a) list -> 'a t -> 'a t -> 'a t
      val data : 'a t -> (gtype * 'a) list
      val left : 'a t -> 'a t
      val right : 'a t -> 'a t
      val balance : 'a t -> 'a t
      val add : 'a t -> gtype -> 'a -> 'a t
      val replace : 'a t -> gtype -> 'a -> 'a t
      val delete : 'a t -> gtype -> 'a t
      val find : 'a t -> gtype -> 'a
      val find_all : 'a t -> gtype -> 'a list
      val mem : 'a t -> gtype -> bool
      val iter : (gtype -> 'a -> 'b) -> 'a t -> unit
      val to_list : 'a t -> (gtype * 'a) list list
    end 

type ('a)tree = ('a)TypeTree.t

(** {5 Substitution} *)

(** [substitution]: The type of substitutions *)
type substitution

(**
   [empty_subst()]: Make an empty substitution.

   [bind t r env]: Bind [r] to [t] in substitution [env].

   [bind_var t r env]: Bind [r] to [t] in substitution [env] but only
   if [t] is a variable.

   [delete t env]: Delete the binding of [t] in [env].

   [lookup t env]: Get the binding of [t] in [env].

   [member t env]: True if [t] has a binding in [env].

   [subst_iter f env]: Apply function [f] to each binding in [env].

   [subst env t]: Apply substitution [env] to gtype [t].  This is
   simultaneous substitution: the substitution is not pushed into the
   replacement terms. This is therefore unsuitable for forming the
   most general unifier since unification can bind variables in both
   the replaced and the replacement term. 
*)
val empty_subst : unit -> substitution
val bind : gtype -> gtype -> substitution -> substitution
val delete : gtype -> substitution -> substitution
val lookup: gtype -> substitution -> gtype
val member: gtype -> substitution -> bool
val subst_iter: (gtype -> gtype -> unit) -> substitution -> unit
val subst: gtype -> substitution -> gtype

(** {5 Operations which depend on substitution} *)

(**
   Type renaming.

   [rename_type_vars_env]: copy a type, making new variables in the type.    

   [rename_type_vars t]: Make a type equivalent but not equal to
   [t], differing from [t] in the variable names.
*)
val rename_type_vars_env: substitution -> gtype -> (gtype * substitution)
val rename_type_vars: gtype -> gtype

(** {5 Pretty Printing} *)

(** 
   [printer_info]: Pretty printing information for types.

   User defined printers have type [(Basic.type_const * Gtypes list)
   printer].

   [empty_printer_info()]: Make an empty printer information.

   [pplookup ppstate id]: Find the printer record for [id] in [ppstate].
*)
type printer_info=
    { 
      tbl: substitution; (* used to store pretty replacement variable names *)
      ctr: int ref; (* used to generate variable names *)
    }
val empty_printer_info: unit -> printer_info
val pplookup: Printer.ppinfo -> Basic.ident -> Printer.record

(** 
   [print_type_inf ppstate tbl prec ty] Print type [ty] beginning with
   precedence [prec].  Rename type variables, using table [tbl],
   generating consistent pretty variable names.  Update [tbl] with the
   new substitution of new names for old 

   [print]: Toplevel for [print_type_inf].
*)
val print_type : 
    Printer.ppinfo -> (Printer.fixity * int) -> gtype Printer.printer
val print : Printer.ppinfo -> gtype Printer.printer

(* {5 Error reporting} *)

class typeError : string -> gtype list ->
  object
    inherit Result.error
    val trms : gtype list
    method get : unit -> gtype list
  end
val type_error : string -> gtype list -> exn
val add_type_error : string ->gtype list -> exn -> 'a

(**  [string_gtype ty]: Make a string representation of type [ty] *)
val string_gtype :  gtype -> string


(** {5 Type definitions}

*)

(** {6 Consistency tests for type definitions}

   Weak variables are not permitted in any definition (type or term).
*)

(**
   [check_defn l r]: test definition of [l] as alias for [r].

   [check_decln l]: consistency check on declaration of type [l].
*) 
val check_defn : Scope.t -> gtype -> gtype -> bool
val check_decln : gtype  -> bool

(**
   [get_defn scp ty]: get the definition of type [ty] from the scope
   [scp]. Raise Not_found if no definition.
*)
val get_defn : Scope.t -> gtype -> gtype

(**
   [well_defined scp args ty] Test [ty] for well-definednes.  every
   constructor occuring in [ty] must be defined.  Variables in [ty]
   must have a name in [args] and weak variables are not permitted in
   [ty]
 *)
val well_defined : Scope.t -> (string)list -> gtype -> unit

(**
   [quick_well_defined scp tbl ty]:
   Test [ty] to make sure it is well-defined.
   weak variables can occur in [ty].

   [tbl] is memo of found constructors (and the number of their
   parameters
*)
val quick_well_defined : Scope.t -> 
  (ident *int, bool) Hashtbl.t -> gtype -> unit

(**
   [check_decl_type scp ty]: Ensure type [ty] is suitable for
   the declaration of a term.

   Fails if [ty] contains a weak variable.
*)
val check_decl_type: Scope.t -> Basic.gtype -> unit

(** {5 Unification} *)

exception Occurs
exception Unify
exception Match

(**
   Occurs check.

   [lookup_var ty env]: Look-up and chase var [ty] in env [environment].
   
   [occurs t r]: check whether [t] occurs in [r].

   [occurs_env env t r]: Occurs check w.r.t [env].  Chase [t] in [env]
   to get [t'], chase [r] in [env] to get [r']. Check whether [t']
   occurs in [r'].

   Both raise typeError on failure, and are silent on  success.

   [bind_occs t r env]: Bind [r] to [t] in [env]. Fails if [t] occurs in [r].
 *)
val lookup_var : gtype -> substitution -> gtype
val occurs :  gtype -> gtype -> unit
val occurs_env :  substitution-> gtype -> gtype -> unit
val bind_occs : gtype -> gtype -> substitution -> substitution

(** {6 Unification functions} *)

(**
   [unify_env scp ty1 ty2 env]: unify two types in context [env],
   return a new subsitution 

   [unify]: unify two types, returning the substitution

   Raise [type_error] if unification fails

   The result of unifying [x] and [y] is a substitution [env]
   s.t. that [mgu x env] is the same as [mgu y env]. 
*)
val unify_env : 
   Scope.t -> gtype -> gtype 
   -> substitution -> substitution 
val unify : Scope.t -> gtype -> gtype  -> substitution 

(**
   [unify_for_rewrite scp tyl tyr env]: Unify types [tyl'] and
   [tyr] in given context [env], where [tyl' = rename_type_vars tyl]. If [sb]
   is the returned substitution, then the type formed by [mgu tyr sb]
   will not have any type variable in common with [tyl].
   
   [unify_for_rewrite sc l r s] is equivalent, but faster than, to
   [unify_env sc (rename_type_vars l) r s].
   
   This function is used for term-rewriting, where there is a danger
   that the same type may occur in different contexts (e.g. as the type
   of an identifier which occurs in different parts of the term).
*)
val unify_for_rewrite:  
    Scope.t -> gtype -> gtype 
      -> substitution -> substitution

(** {6 Most General Unifiers} *)

(** 
   [mgu ty env]: Construct the most general unifier for type [ty] from
   substitution [env]. This is a version of substitution which pushes
   the substitution into the replacement terms.
*)
val mgu : gtype  -> substitution -> gtype

(**
   [mgu_rename inf env nenv ty] 

   Replace variables in [ty] with their bindings in substitution [env].
   If a variable isn't bound in [env], then it is renamed and bound
   to that name in [nenv] (which is checked before a new name is created).

   [env] is type substitution found e.g. by typechecking
   [nenv] is substitution in which to store the new type variables

   returns the new type and updated nenv
 *)
val mgu_rename_env: int ref -> substitution -> substitution 
  -> gtype -> (gtype * substitution)
val mgu_rename: int ref -> substitution -> substitution 
  -> gtype -> gtype 

(** {6 Matching functions} *)

(**
   [matches_env scp env t1 t2]: Match type [t1] with type [t2] w.r.t
   context [env]. This unifies [t1] and [t2], but only variables in
   type [t1] can be bound.
*)
val matches_env : 
    Scope.t -> substitution 
      -> gtype -> gtype -> substitution
val matches : Scope.t -> gtype -> gtype -> bool


(** {5 More functions} *)

(** 
   [set_name ?strict ?memo scp ty]: set names in type [ty] to their
   long form.

   If [strict] then fail if any type name doesn't occur in scope [scp].

   [memo] is the optional memoisation table.
*)
val set_name : 
    ?strict:bool
  -> ?memo:(string, Basic.thy_id)Hashtbl.t
    -> Scope.t -> gtype -> gtype

(**
   [in_scope memo scp th ty]: Check that [ty] is in scope by checking
   that every type constructor is decared or defined in scope [scp].

   The function is memoised: if a constructor name is found to be 
   in scope, it is added to [memo].
*)
val in_scope: 
    (string, bool)Lib.substype -> Scope.t ->thy_id -> gtype -> bool


(**
   [extract_bindings vars src dst]: extract bindings variables in
   [var] from [src] substitution, store them in [dst] substitution

   Needed by the sequent calculus to determine the bindings made by
   operations.
*)
val extract_bindings: gtype list -> substitution -> substitution 
  -> substitution

(** {5 Saving gtypes to disk storage} *)

(** [stype]: representation of types for storage on disk *)
type stype = ((string * int), Basic.typ_const, Basic.base_typ) pre_typ

(** [stypedef]: representation of typedef_records for disk storage *)
type stypedef_record =
    {sname: string; 
     sargs : string list; 
     salias: stype option;
     scharacteristics: string list}

(** 
   [to_save ty]: Convert [ty] to [stype] storage representation.

   [to_save_env ty env]: Convert [ty] to [stype] storage representation.
   [env] store the names of type variables already encountered.
*)
val to_save: gtype -> stype
val to_save_env: (string ref* (string *int)) list ref 
  -> gtype -> stype

(** 
   [from_save ty]: Convert storage type [ty] to [gtype] representation.

   [from_save_env ty env]: Convert storage [ty] to [gtype] representation.
   [env] store the names of type variables already encountered.
*)
val from_save: stype -> gtype
val from_save_env : 
    ((string * int)* (string ref)) list ref
  -> stype -> gtype

(** 
   [to_save_rec r]: Convert [r] to [stypedef_record] storage representation.

   [from_save_rec r]: Convert storage record [r] to [typedef_record]
   representation.
*)
val to_save_rec: typedef_record -> stypedef_record
val from_save_rec: stypedef_record -> typedef_record

(*
* Debugging support
*)

val print_subst : substitution -> unit

