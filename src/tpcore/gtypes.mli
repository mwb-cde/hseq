(* Types and their manipulation *)

open Basic
open Lib
(*
type ('idtyp, 'tfun, 'tcons) pre_typ =
    Var of 'idtyp
  | Constr of 'tfun * ('idtyp, 'tfun, 'tcons) pre_typ list
  | Base of 'tcons
  | WeakVar of 'idtyp
(** 
   WeekVar x: binds to anything except a variable.

   Isn't (usually) renamed.

   Is used in a sequent calculus when a variable type x can occur
   in more than one sequent. If x is bound in one sequent, it must
   be have that binding in every sequent in which it occurs. (Like 
   week types in ML)
 *)
(* representation of types *)
type gtype = 
    ((string ref, Basic.typ_const, Basic.base_typ)pre_typ)
*)

(* representation of types for storage on disk *)
type stype = 
    ((string * int), Basic.typ_const, Basic.base_typ) pre_typ

(* records for type definitions *)

type typedef_record =
    {name: string; 
     args : string list; 
     alias: gtype option;
     characteristics: string list}

type stypedef_record =
    {sname: string; 
     sargs : string list; 
     salias: stype option;
     scharacteristics: string list}

(* record scopes and type definitions (e.g. for typechecking) *)

type scope =  (* was typ_env *)
    { curr_thy : thy_id;
      typeof_fn : ident -> gtype; 
	typ_defn: ident -> typedef_record;
	  thy_of : id_selector -> string -> thy_id;
    	    prec_of: id_selector -> ident -> int;
	      thy_in_scope : thy_id -> thy_id -> bool}

val empty_scope : unit -> scope
val add_to_scope: scope -> (ident * gtype) list -> scope
val extend_scope: scope -> (ident -> gtype) -> scope


(* get definition of a type *)
val get_typdef: scope -> ident -> typedef_record

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

(* renamed to equals
   val equality: gtype -> gtype -> bool
 *)

val safe_equal: gtype -> gtype -> bool

(* constructors/destructors/recognisers for types *)

(* basic types *)

val mk_null : unit -> gtype 
val is_null: gtype -> bool

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

(* function types *)
val mk_fun : gtype -> gtype -> gtype
val mk_fun_from_list: gtype list -> gtype -> gtype
val dest_constr : gtype -> (Basic.typ_const * gtype list)

(* recognisers *)

val is_var : gtype -> bool
val is_constr : gtype  -> bool
val is_base : gtype  -> bool
val is_weak : gtype -> bool

(*
   renamed:
   val varp : gtype -> bool  to is_var 
   val constrp : gtype  -> bool to is_constr 
   val basep : gtype  -> bool to is_base
   val weakp : gtype -> bool to is_weak
 *)

(* compare types *)

(*
   val eqvar : gtype -> gtype -> bool 
   val eqbase : gtype -> gtype -> bool
   val eqconstr : gtype -> gtype -> bool
 *)

(* destruct function type *)
val arg_type : gtype -> gtype
val ret_type : gtype -> gtype
val chase_ret_type : gtype -> gtype

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

(* type of primitive constructs *)
val typeof_cnst  : Basic.const_ty -> gtype
val typeof_conn  : Basic.conns_ty -> gtype

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

(* copy a type, making new variables in the type *)
(* ie: (copy_type t) is equivalent but not equal to t *)

val copy_type_env: substitution -> gtype -> (gtype * substitution)
val copy_type: gtype -> gtype

(* unification *)
(* unify two types, returning the substitution*)
val unify : scope -> gtype -> gtype  -> substitution 

(* unify two types in a given context/subsitution, 
   return a new subsitution *)

val unify_env : scope -> gtype -> gtype 
  -> substitution -> substitution 

(*  unify two types in a given context/subsitution, 
   return a new subsitution
   the left type is copied using copy_type.
   i.e. unify_env_unique_left sc l r s
   is equivalent to
   unify_env sc (copy_type l) r s
   this function is used for rewriting with multiple terms.
   Defunct: use unify_for_rewrite
 *)

val unify_env_unique_left:  scope -> gtype -> gtype 
  -> substitution -> substitution 

(* remove bindings from a failed attempt at unification *)
val remove_bindings: gtype list -> substitution -> substitution

(*  unify two types in a given context/subsitution, 
   return a new subsitution
   the left type is copied using copy_type.
   i.e. unify_for_rewrite sc l r s
   is equivalent to
   unify_env sc (copy_type l) r s
   this function is used for rewriting with multiple terms.
 *)

val unify_for_rewrite:  
    scope -> gtype -> gtype 
      -> substitution -> substitution

(* get most general unifier for a type and subsitution *)
val mgu : gtype  -> substitution -> gtype

(* matching *)
val matching :scope -> gtype -> gtype -> gtype
val matches_env : scope -> substitution 
  -> gtype -> gtype -> (bool * substitution)
val matches : scope -> gtype -> gtype -> bool

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

(* check_defn l r: test defintion of l as alias for r *)
(* check_decln l: consistency check on declaration of type l *) 

val check_defn : scope -> gtype -> gtype -> bool
val check_decln : gtype  -> bool

val print_subst : substitution -> unit

(* get the definition of a type from the scope if it exists *)
(* raises Not_found if not definition*)
val get_defn : scope -> gtype -> gtype

(* 
   [well_defined scp args ty]
   test [ty] for well-definednes.
   every constructor occuring in [ty] must be defined.
   weak variables are not permitted in [ty]
 *)
val well_defined : scope -> ?args: (string)list -> gtype -> unit

(* 
   [quick_well_defined scp tbl ty]:
   test [ty] to make sure it is well-defined.
   weak variables can occur in [ty].

   [tbl] is memo of found constructors (and the number of their
   parameters
 *)
val quick_well_defined : scope -> 
  (ident *int, bool) Hashtbl.t -> gtype -> unit

(* Error reporting *)

class typeError : string -> gtype list ->
  object
    inherit Result.error
    val trms : gtype list
    method get : unit -> gtype list
  end
val typeError : string -> gtype list -> exn
val addtypeError : string ->gtype list -> exn -> 'a

(* pretty printer *)

(*
   val print_type_info : Basic.PP.info -> subst -> int -> gtype -> unit
   val print_type : Basic.PP.info -> gtype -> unit
*)

(** 
   [ppinfo]
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
[print_type_inf ppstate tbl prec ty]
Print type [ty] beginning with precedence [prec].
Rename type variables, using table [tbl], generating consistent pretty 
variable names.
Update [tbl] with the new substitution of new names for old 
*)

(* print_type renamed to print *)
val print_type : Printer.ppinfo -> int -> gtype Printer.printer

val print : Printer.ppinfo -> gtype Printer.printer

(* set names in a type to their long form *)

val set_name : scope -> gtype -> gtype

(* in_thy_scope: check that all types are in scope of given theory *)
(* first argument is for memoised *)

val in_thy_scope: (string, bool)Lib.substype
  -> scope ->thy_id -> gtype -> bool

(* utility functions *)
(* mk_new_type n: 
   make a new type variable with name (int_to_name !n);
   increment n;
   return new type
 *)

val mk_typevar: int ref -> gtype

(* [mgu_rename inf env nenv ty] 

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
