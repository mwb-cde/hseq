(*----
  Name: gtypes.mli
  Copyright M Wahab 2005-2009, 2010
  Author: M Wahab  <mwb.cde@googlemail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under
  the terms of the Lesser GNU General Public License as published by
  the Free Software Foundation; either version 3, or (at your option)
  any later version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
  License for more details.

  You should have received a copy of the Lesser GNU General Public
  License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
  ----*)

(** Types and their manipulation *)

open Basic
open Lib

(** {5 Basic Operations} *)

val equals: gtype -> gtype -> bool
(** Syntactic equality between types. *)

(** {7 Recognisers} *)

val is_var: gtype -> bool
val is_constr: gtype -> bool
val is_weak: gtype -> bool

(** {7 Constructors} *)

val mk_var: string -> gtype
val mk_weak: string -> gtype
val mk_constr: Basic.typ_const -> gtype list -> gtype

(** {7 Destructors} *)

val dest_var: gtype -> string ref
val get_var_name: gtype -> string

val dest_weak: gtype -> string ref
val get_weak_name: gtype -> string

val dest_constr: gtype -> (Basic.typ_const * gtype list)

(** {6 Specialised Manipulators} *)

(** {7 Variable types} *)

val is_any_var: gtype -> bool
(** [is_any_var t]: true if [t] is a variable or a weak variable. *)

val get_var_names: gtype -> string list
(** Get names of variables occuring in type. *)

val normalize_vars: gtype -> gtype  
(** Make all type variables with the same string name be the same
    variable. Useful when constructing types from existing types.
*)

val mk_typevar: int -> (int * gtype)
(** [mk_typevar n]: Make a new type variable [t'] with a name derived
    from [n] and return [(n + 1, t')]. Different values of [n] make
    different names.

    This is does the same thing as [mk_typevar] but without side-effects.
*)

val get_var_names: gtype -> string list
(** [get_var_names ty]: Get the names of variables in [ty]. Ignores
    weak variables.
*)

(** {7 Unnamed type variables} *)

val mk_null: unit -> gtype
(** Make an unnamed type variable. *)
val is_null: gtype -> bool
(** Test for an unnamed type variable. *)

(** {7 Named typed constructors} *)

val is_def: gtype -> bool
val mk_def: Ident.t -> gtype list -> gtype
val dest_def: gtype -> (Ident.t * gtype list)

(** {5 Type Definitions} *)

type typedef_record = Scope.type_record
(** Records for type definitions. *)

val get_typdef: Scope.t -> Ident.t -> typedef_record
(** Get definition of type named [n] from scope [scp]. *)

(** {5 Data storage indexed by gtypes} *)

(** {7 Hash tables} *)
module type RHASHKEYS=
sig 
  type t = gtype
  val equal: t -> t -> bool
  val hash: t -> int
end
module type RHASH = (Hashtbl.S with type key = (gtype))
module Rhash: RHASH

type ('a)table = ('a)Rhash.t
(** Hashtables with keys of type gtype. *)


(** {7 Balanced Trees} *)
module TypeTreeData: Treekit.TreeData

module TypeTree:
  (Treekit.BTreeType with type key = gtype)

type ('a)tree = ('a)TypeTree.t
(** Balanced trees indexed by gtypes *)


(** {5 Substitution} *)

type substitution
(** The type of substitutions *)

val empty_subst: unit -> substitution
(** Make an empty substitution. *)
val bind: gtype -> gtype -> substitution -> substitution
(** [bind t r env]: Bind [r] to [t] in substitution [env]. *)
val delete: gtype -> substitution -> substitution
(** [delete t env]: Delete the binding of [t] in [env]. *)
val lookup: gtype -> substitution -> gtype
(** [lookup t env]: Get the binding of [t] in [env]. *)
val member: gtype -> substitution -> bool
(** [member t env]: True if [t] has a binding in [env]. *)
val subst_iter: (gtype -> gtype -> unit) -> substitution -> unit
(** [subst_iter f env]: Apply function [f] to each binding in [env]. *)
val subst_fold: (gtype -> 'a -> gtype -> 'a) -> 'a -> substitution -> 'a
(** [subst_fold f val env]: Fold function [f] over the bindings in [env]. *)
val subst: gtype -> substitution -> gtype
(** [subst env t]: Apply substitution [env] to gtype [t]. This is
    simultaneous substitution: the substitution is not pushed into the
    replacement terms. This is therefore unsuitable for forming the
    most general unifier since unification can bind variables in both
    the replaced and the replacement term.
*)

(** {6 Operations which depend on substitution} *)

val rename_type_vars_env: substitution -> gtype -> (gtype * substitution)
(** copy a type, making new variables in the type. *)
val rename_type_vars: gtype -> gtype
(** [rename_type_vars t]: Make a type equivalent but not equal to [t],
    differing from [t] in the variable names.
*)

(** {5 Pretty Printing} *)

type printer_info=
    { 
      tbl: substitution; (* Replacement variable names for pretty-printing. *)
      ctr: int ref; (* Used to generate variable names. *)
    }
(** Pretty printing information for types. *)
val empty_printer_info: unit -> printer_info
(** Make an empty printer information. *)
val pplookup: Printer.ppinfo -> Ident.t -> Printer.record
(** [pplookup ppstate id]: Find the printer record for [id] in [ppstate].*)

val print_type: 
  Printer.ppinfo -> (Printer.fixity * int) -> gtype Printer.printer
(** [print_type_inf ppstate tbl prec ty] Print type [ty] beginning
    with precedence [prec]. Rename type variables, using table [tbl],
    generating consistent pretty variable names. Update [tbl] with the
    new substitution of new names for old.
*)
val print: Printer.ppinfo -> gtype Printer.printer
(** Toplevel for [print_type_inf]. *)

(* {5 Error reporting} *)

class typeError: string -> gtype list ->
object
  inherit Report.error
  val trms: gtype list
  method get: unit -> gtype list
end
val type_error: string -> gtype list -> exn
val add_type_error: string ->gtype list -> exn -> 'a

val string_gtype: gtype -> string
(** Make a string representation of a type. *)

(** {5 Type definitions} *)

(** {7 Consistency tests for type definitions}

    Weak variables are not permitted in any definition (type or term).
*)

val check_defn: Scope.t -> gtype -> gtype -> bool
(** [check_defn l r]: Test definition of [l] as alias for [r]. *)

val check_decln: gtype -> bool
(**  [check_decln l]: Consistency check on declaration of type [l]. *) 

val unfold: Scope.t -> gtype -> gtype
(**
   [unfold scp ty]: Unfold the definition of type [ty] from the scope
   [scp].

   @raise [Not_found] if no definition.
*)

val well_defined: Scope.t -> (string)list -> gtype -> unit
(** [well_defined scp args ty]: Test [ty] for well-definednes. every
    constructor occuring in [ty] must be defined. Variables in [ty]
    must have a name in [args] and weak variables are not permitted in
    [ty].
*)

val quick_well_defined: Scope.t -> 
  (Ident.t *int, bool) Hashtbl.t -> gtype -> unit
(** [quick_well_defined scp tbl ty]: Test [ty] to make sure it is
    well-defined.  weak variables can occur in [ty].

    [tbl] is memo of found constructors (and the number of their
    parameters.
*)

val check_decl_type: Scope.t -> Basic.gtype -> unit
(** [check_decl_type scp ty]: Ensure type [ty] is suitable for the
    declaration of a term. Fails if [ty] contains a weak variable.
*)

(** {5 Unification} *)

exception Occurs
exception Unify
exception Match

val lookup_var: gtype -> substitution -> gtype
(** [lookup_var ty env]: Look-up and chase var [ty] in env [environment]. *)
val occurs: gtype -> gtype -> unit
(** [occurs t r]: Occurs check.

    @raise [typeError] if [t] occurs in [r], succeed silently otherwise.
*)
val occurs_env: substitution-> gtype -> gtype -> unit
(**
   [occurs_env env t r]: Occurs check w.r.t [env]. Chase [t] in [env]
   to get [t'], chase [r] in [env] to get [r']. 

   @raise [typeError] if [t'] occurs in [r'], succeed silently
   otherwise.
*)
val bind_occs: gtype -> gtype -> substitution -> substitution
(** [bind_occs t r env]: Bind [r] to [t] in [env]. Fails if [t] occurs
    in [r].
*)

(** {6 Unification functions} *)

(** Unification of gtypes [x] and [y] tries to create a substitution
    with bindings for variables of [x] and [y] which make [x] and [y]
    equal.

    The result of unifying [x] and [y] is a substitution [env]
    s.t. that [mgu x env] is the same as [mgu y env].

    The substitution is formed by assigning types to type
    variables. Weak type variables (constructed by [Weak]) can be
    assigned weak variables and non-variables only. Weak type
    variables cannot be assigned type variables formed by [Var]. Type
    variables formed by [Var] can be assigned any type.

    The substitution resulting from the unification functions cannot be
    used with [subst]. Function [mgu] and its derivatives must be used.

    Unification functions raise [type_error] on failure.
*)

val unify_env: 
  Scope.t -> gtype -> gtype 
  -> substitution -> substitution 
(** [unify_env scp ty1 ty2 env]: Unify two types in context [env],
    return a new subsitution.
*)
val unify: Scope.t -> gtype -> gtype -> substitution 
(** [unify]: unify two types, returning the substitution.
*)

(** {7 Most General Unifiers} *)

val mgu: gtype -> substitution -> gtype
(** [mgu ty env]: Construct the most general unifier for type [ty]
    from substitution [env]. This is a version of substitution which
    pushes the substitution into the replacement terms.
*)

val mgu_rename_env: (int * substitution) -> substitution 
  -> gtype -> (gtype * (int * substitution))
(** [mgu_rename_env inf env nenv ty]: Replace variables in [ty] with
    their bindings in substitution [env].  If a variable isn't bound
    in [env], then it is renamed and bound to that name in [nenv]
    (which is checked before a new name is created).

    [env] is type substitution found e.g. by typechecking [nenv] is
    substitution in which to store the new type variables.

    Returns the new type and updated nenv.
*)
val mgu_rename: 
  int -> substitution 
  -> substitution -> gtype 
  -> gtype 

val mgu_rename_simple: int -> substitution -> substitution 
  -> gtype -> (gtype * int *substitution)
(**
   [mgu_rename_simple inf env env nenv typ]: Replace variables in [typ]
   with their bindings in substitution [env].  If a variable isn't bound
   in [env], then it is renamed and bound to that name in [nenv] (which is
   checked before a new name is created).

   This does the same thing as mgu_rename_env except that it takes
   [inf] as a scalar, rather than a reference, and returns a new value
   for [inf].
*)

(** Toplevel for [mgu_rename_env]. *)

(** {6 Matching functions} 

    Matching of types [x] and [y] is the unification of [x] and [y] in
    which only the variables of [x] can be bound.
*)

val matching_env: 
  Scope.t -> substitution 
  -> gtype -> gtype -> substitution
(**
   [matching_env scp env t1 t2]: Match type [t1] with type [t2] w.r.t
   context [env]. This unifies [t1] and [t2], but only variables in
   type [t1] can be bound. 

   Raises an exception if matching fails.
*)

val matches_env: 
  Scope.t -> substitution 
  -> gtype -> gtype -> substitution
(** [matches_env scp env t1 t2]: Match type [t1] with type [t2] w.r.t
    context [env]. This unifies [t1] and [t2], but only variables in
    type [t1] can be bound.

    Silently returns unchanged substitution on failure.
*)

val matches: Scope.t -> gtype -> gtype -> bool
(** Toplevel for [matches_env]. *)

type match_data =
    {
      vars: substitution; (** Unification variables *)
      tyenv: substitution; (** Substitution *)
    }
(** Data for {!Gtypes.matches_rewrite} *)

val matches_rewrite: 
  Scope.t -> gtype -> gtype 
  -> match_data -> match_data
(** [matches_rewrite scp tyl tyr env]: Match type [tyl] with [tyr],
    returning the substitutions for type variables in [tyl]. 
*)

(** [matches_rewrite scp tyl tyr env]: Match type [tyl'] with [tyr] in
    given context [env], where [tyl' = rename_type_vars tyl]. If [sb]
    is the returned substitution, then the type [nty = (mgu tyr sb)]
    will not have any type variable in common with [tyl] (i.e. no type
    variable can occur in both [nyt] and [tyl].)
    
    This function is used for term-rewriting, where there is a danger
    that the same type may occur in different contexts, such as the
    type of an identifier which occurs in different parts of the
    term. 
*)

(**
val copy_set_ty: gtype -> match_data -> (gtype * match_data)
(** [copy_set_ty t d]: Copy type [t], adding any variables in [t] to
    [d.vars].
*)
**)

(** {5 More functions} *)

val set_name: 
  ?strict:bool
  -> ?memo:(string, Ident.thy_id)Hashtbl.t
  -> Scope.t -> gtype -> gtype
(** [set_name ?strict ?memo scp ty]: set names in type [ty] to their
    long form.

    If [strict] then fail if any type name doesn't occur in scope [scp].

    [memo] is the optional memoisation table.
*)

val in_scope: 
  (string, bool)Lib.substype -> Scope.t -> gtype -> bool
(** [in_scope memo scp th ty]: Check that [ty] is in scope by checking
    that every type constructor is decared or defined in scope [scp].

    The function is memoised: if a constructor name is found to be 
    in scope, it is added to [memo].
*)


val extract_bindings: gtype list -> substitution -> substitution 
  -> substitution
(** [extract_bindings vars src dst]: extract bindings variables in
    [var] from [src] substitution, store them in [dst] substitution

    Needed by the sequent calculus to determine the bindings made by
    operations.
*)

(** {5 Saving gtypes to disk storage} *)

type stype = ((string * int), Basic.typ_const) pre_typ
(** Representation of types for storage on disk. *)

type stypedef_record =
    {sname: string; 
     sargs: string list; 
     salias: stype option;
     scharacteristics: string list}
(** Representation of typedef_records for disk storage. *)

type to_stype_env = (string ref * (string *int)) list
(** Data needed to construct a type storage representation. *)

val to_save_env: to_stype_env -> gtype -> (stype * to_stype_env)
(** [to_save_env ty env]: Convert [ty] to [stype] storage
    representation.  [env] store the names of type variables already
    encountered.
*)

val to_save: gtype -> stype
(** Toplevel for [to_save_env]. *)

type from_stype_env = ((string * int) * string ref) list
(** Data needed to construct a type storage representation. *)

val from_save_env: 
  from_stype_env -> stype -> (gtype * from_stype_env)
(** [from_save_env ty env]: Convert storage [ty] to [gtype]
    representation.  [env] store the names of type variables already
    encountered.
*)

val from_save: stype -> gtype
(** Toplevel for [from_save_env]. *)

val to_save_rec: typedef_record -> stypedef_record
(** [to_save_rec r]: Convert [r] to [stypedef_record] storage
    representation.
*)

val from_save_rec: stypedef_record -> typedef_record
(** [from_save_rec r]: Convert storage record [r] to [typedef_record]
    representation.
*)

(*
 * Debugging support
 *)

val print_subst: substitution -> unit

module Retired: 
sig

  val unify_for_rewrite: 
    Scope.t -> gtype -> gtype 
    -> substitution -> substitution
(** [unify_for_rewrite scp tyl tyr env]: Unify types [tyl'] and [tyr]
    in given context [env], where [tyl' = rename_type_vars tyl]. If
    [sb] is the returned substitution, then the type formed by [mgu tyr
    sb] will not have any type variable in common with [tyl].
    
    [unify_for_rewrite sc l r s] is equivalent, but faster than, to
    [unify_env sc (rename_type_vars l) r s].
    
    This function is used for term-rewriting, where there is a danger
    that the same type may occur in different contexts (e.g. as the type
    of an identifier which occurs in different parts of the term).
*)

end
