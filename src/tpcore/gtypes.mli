(*-----
 Name: gtypes.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* Types and their manipulation *)

open Basic
open Lib

(* representation of types for storage on disk *)
type stype = 
    ((string * int), Basic.typ_const, Basic.base_typ) pre_typ

(* records for type definitions *)

type typedef_record = Scope.type_record

type stypedef_record =
    {sname: string; 
     sargs : string list; 
     salias: stype option;
     scharacteristics: string list}

(* record scopes and type definitions (e.g. for typechecking) *)

(* get definition of a type *)
val get_typdef: Scope.t -> ident -> typedef_record

val string_gtype :  gtype -> string

(* conversion to and from disk representation *)
val from_save: stype -> gtype
val from_save_env : 
    ((string * int)* (string ref)) list ref
  -> stype -> gtype

val to_save: gtype -> stype
val to_save_env: (string ref* (string *int)) list ref 
  -> gtype -> stype

val to_save_rec: typedef_record -> stypedef_record
val from_save_rec: stypedef_record -> typedef_record

(* equality between types (uses Dequals test) *)
val equals: gtype -> gtype -> bool

val safe_equal: gtype -> gtype -> bool

(* constructors/destructors/recognisers for types *)

(* basic types *)

val mk_null : unit -> gtype 
val is_null: gtype -> bool

val mk_base: base_typ -> gtype
val mk_bool : gtype
val mk_num : gtype

(* variable types *)

val mk_var : string -> gtype
val dest_var : gtype -> string  ref
val get_var : gtype -> string

(* weak variables *)
val mk_weak : string -> gtype
val dest_weak : gtype -> string  ref
val get_weak : gtype -> string

(* Constructed types *)
val mk_constr: Basic.typ_const -> gtype list -> gtype
val dest_constr:  gtype -> (Basic.typ_const * gtype list)

val dest_constr : gtype -> (Basic.typ_const * gtype list)

(* recognisers *)

val is_var : gtype -> bool
val is_constr : gtype  -> bool
val is_base : gtype  -> bool
val is_weak : gtype -> bool


(* compare types *)

(* Defined types *)

val mk_def: Basic.ident -> gtype list -> gtype
val dest_def: gtype -> (Basic.ident* gtype list)

(* unification and subsitution *)

exception Occurs
exception Unify

(* type of substitutions *)

(* Gtypes.Rhash: hashtables with gtype as the key *)

module type RHASHKEYS=
  sig 
    type t = gtype
    val equal : t -> t -> bool
    val hash: t -> int
  end
module type RHASH = (Hashtbl.S with type key = (gtype))
module Rhash: RHASH

(* Lookup trees with a gtype as key *)
module TypeTreeData:
    sig
      type key=gtype
      val equals : key -> key -> bool
    end

type substitution

(* make a substitution with arbitrary or given size *)
val empty_subst : unit -> substitution
val subst_sz: int -> substitution
val bind : gtype -> gtype -> substitution -> substitution
val delete : gtype -> substitution -> substitution
val lookup: gtype -> substitution -> gtype
val member: gtype -> substitution -> bool
val subst_iter: (gtype -> gtype -> unit) -> substitution -> unit

(* occurs check  
   occurs is a shallow check.
   occurs_env takes context into account 
   and is the version used for unification 
   both raise typeError on failure, 
   and are silent on success.
 *)

val occurs :  gtype -> gtype -> unit
val occurs_env :  substitution-> gtype -> gtype -> unit

val bind_occs : gtype -> gtype -> substitution -> substitution

(**
   [copy_type_env]: copy a type, making new variables in the type.    
   [(copy_type t)] is equivalent but not equal to [t]
*)
val copy_type_env: substitution -> gtype -> (gtype * substitution)
val copy_type: gtype -> gtype

(* Unification 
   
   [unify]: unify two types, returning the substitution
*)
val unify : Scope.t -> gtype -> gtype  -> substitution 

(**
   [unify_env scp ty1 ty2 env]: unify two types in context [env]
   return a new subsitution 
*)
val unify_env : Scope.t -> gtype -> gtype 
  -> substitution -> substitution 

(**
   [unify_env_unique_left scp tyl tyr env]: Unify types [tyl'] and
   [tyr] in given context [env], where [tyl' = copy_type tyl]. If [sb]
   is the returned substitution, then the type formed by [mgu tyr sb]
   will not have any type variable in common with [tyl].
   
   [unify_env_unique_left sc l r s] is equivalent, but faster than, to
   [unify_env sc (copy_type l) r s].
   
   This function is used for rewriting with multiple terms.

   Defunct: use [unify_for_rewrite]
 *)
val unify_env_unique_left:  Scope.t -> gtype -> gtype 
  -> substitution -> substitution 

(* remove bindings from a failed attempt at unification *)
val remove_bindings: gtype list -> substitution -> substitution

(**
   [unify_for_rewrite scp tyl tyr env]: Unify types [tyl'] and
   [tyr] in given context [env], where [tyl' = copy_type tyl]. If [sb]
   is the returned substitution, then the type formed by [mgu tyr sb]
   will not have any type variable in common with [tyl].
   
   [unify_for_rewrite sc l r s] is equivalent, but faster than, to
   [unify_env sc (copy_type l) r s].
   
   This function is used for term-rewriting, where there is a danger
   that the same type may occur in different contexts (e.g. as the type
   of an identifier which occurs in different parts of the term).
*)
val unify_for_rewrite:  
    Scope.t -> gtype -> gtype 
      -> substitution -> substitution

(* get most general unifier for a type and subsitution *)
val mgu : gtype  -> substitution -> gtype

(* matching *)
val matching :Scope.t -> gtype -> gtype -> gtype
val matches_env : 
    Scope.t -> substitution 
      -> gtype -> gtype -> substitution
val matches : Scope.t -> gtype -> gtype -> bool

(* look up types in a subsitution *)
val lookup_var : gtype -> substitution -> gtype
val lookup_ty : gtype -> substitution -> gtype
val lookup : gtype -> substitution -> gtype

(* 
   [extract_bindings vars src dst]
   extract bindings variables in [var] from 
   [src] substitution, store them in [dst] substitution 
 *)
val extract_bindings: gtype list -> substitution -> substitution 
  -> substitution

(**
   [check_defn l r]: test defintion of l as alias for r 

   [check_decln l]: consistency check on declaration of type l 
*) 
val check_defn : Scope.t -> gtype -> gtype -> bool
val check_decln : gtype  -> bool

val print_subst : substitution -> unit

(**
   [get_defn scp ty]: get the definition of type [ty] from the scope
   [scp]. Raise Not_found if no definition.
*)
val get_defn : Scope.t -> gtype -> gtype

(** 
   [get_var_names ty]: 
   get names of variables occuring in type.
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
   [check_decl_type scp ty]: Ensure type [ty] is suitable for
   a declaration.

   Fails if [ty] contains a weak variable.
*)
val check_decl_type: Scope.t -> Basic.gtype -> unit

(**
   [well_defined scp args ty]
   test [ty] for well-definednes.
   every constructor occuring in [ty] must be defined.
   weak variables are not permitted in [ty]
   args: (optional) check variables are in the list of args 
 *)
val well_defined : Scope.t -> (string)list -> gtype -> unit

(**
   [quick_well_defined scp tbl ty]:
   test [ty] to make sure it is well-defined.
   weak variables can occur in [ty].

   [tbl] is memo of found constructors (and the number of their
   parameters
*)
val quick_well_defined : Scope.t -> 
  (ident *int, bool) Hashtbl.t -> gtype -> unit

(* Error reporting *)

class typeError : string -> gtype list ->
  object
    inherit Result.error
    val trms : gtype list
    method get : unit -> gtype list
  end
val type_error : string -> gtype list -> exn
val add_type_error : string ->gtype list -> exn -> 'a

(* pretty printer *)

(** 
   [printer_info]
   Pretty printing information for types.
   Added printers have type [(Basic.type_const * Gtypes list) printer].
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
*)
val print_type : Printer.ppinfo -> int -> gtype Printer.printer

val print : Printer.ppinfo -> gtype Printer.printer

(** [set_name]: set names in a type to their long form *)
val set_name : 
    ?strict:bool
  -> ?memo:(string, Basic.thy_id)Hashtbl.t
    -> Scope.t -> gtype -> gtype

(**
   [in_thy_scope]: check that all types are in Scope.t of given theory
   first argument is for memoisation
*)
val in_scope: 
    (string, bool)Lib.substype -> Scope.t ->thy_id -> gtype -> bool

(* utility functions *)
(**
   [mk_new_type n]: make a new type variable with name [(int_to_name
   !n)]; increment n; return new type
*)
val mk_typevar: int ref -> gtype

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
