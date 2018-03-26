(*----
  Name: gtypes.mli
  Copyright Matthew Wahab 2005-2018
  Author: Matthew Wahab <mwb.cde@gmail.com>

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

open Lib

(** {5 Base representation of logic types} *)

type gtype_id
(** Type identifers. *)

val mk_gtype_id: string -> gtype_id
(** Make a type identifier. *)
val gtype_id_string: gtype_id -> string
(** Get a string representation of a type identifier. *)
val gtype_id_copy: gtype_id -> gtype_id
(** Make a new, unique with the same string representation as another. *)
val gtype_id_equal: gtype_id -> gtype_id -> bool
(** Equality of gtype_id. *)
val gtype_id_greaterthan: gtype_id -> gtype_id -> bool
val gtype_id_lessthan: gtype_id -> gtype_id -> bool
val gtype_id_compare: gtype_id -> gtype_id -> Order.t
(** Orderings on gtype_id.

    Maintains the invariant:
    - [gtype_id_equal x y]
      =>
      [not (gtype_id_lessthan x y)] and [not (gtype_id_greaterthan x y)].
    - [gtype_id_lessthan x y = not(gtype_id_greaterthan x y)]
*)

(** The base representation of types. *)
type ('a) pre_typ =
  | Atom of 'a
  (** Atomic variables. *)
  | TApp of (('a) pre_typ * ('a) pre_typ)
  (** Applications *)

type atomtype =
  | Var of gtype_id
  | Weak of gtype_id
  | Ident of Ident.t
(** [atomtype] Kinds of atomic type

   [Var(v)] is type variable. Can be bound (in a type environment) to any other
   type.

   [Weak(v)] is a weak type variable. Can bind to anything except a non-weak
   variable. They are used in a sequent calculus when a variable type 'x can
   occur in more than one sequent. If 'x is bound in one sequent, then it must
   have that binding in every sequent in which it occurs.

   [Ident(i)] is the name of a type constructor.
 *)

type gtype = (atomtype)pre_typ
(** The actual representation of types. *)

val mk_vartype: gtype_id -> gtype
val mk_weakvartype: gtype_id -> gtype
val mk_identtype: Ident.t -> gtype
val mk_apptype: gtype -> gtype -> gtype

val flatten_apptype: gtype -> (gtype)list
(**
   [flatten_apptype ty]: flatten an application in [ty] to a list of
   types.  [flatten_apptype (((f a1) a2) a3)] is [[f; a1; a2; a3]] and
   [flatten_apptype (((f a1) (g a2)) a3)] is [[f; a1; (g a2); a3]]

   If [ty] is not an applictaion then returns [[ty]].
*)

val split_apptype: gtype -> (gtype *(gtype)list)
(** Split an application [x a1 .. an] into [(x, [a1; .. an])] *)

val map_atomtype: (('a)pre_typ -> ('a)pre_typ) -> ('a)pre_typ -> ('a)pre_typ
(* [map_atomtype f ty] Apply [f] to each [Atom] in [ty] returning the resulting
   type. *)

val iter_atomtype: (('a)pre_typ -> unit) -> ('a)pre_typ -> unit
(* [iter_atomtype f ty] Apply [f] to each [Atom(x)] in [ty]. *)

val fold_atomtype: ('a -> ('b)pre_typ -> 'a) -> 'a -> ('b)pre_typ -> 'a
(* [fold_atomtype f z ty] Fold [f] over each [Atom(x)] in [ty] returning the
   result. The fold is top-down, left-to-right *)

val exists_atomtype: (('a)pre_typ -> bool) -> ('a)pre_typ -> bool
(* [exists_atomtype p ty] Apply [p] to each [Atom(x)] in [ty], return [true] if
   any [Atom(x)] satisfies [p]. The check is top-down, left-to-right *)

val exists_type: (('a)pre_typ -> bool) -> ('a)pre_typ -> bool
(* [exists_type p ty] Apply [p] to each sub-type of [ty], return [true] if any
   satisfies [p]. The check is top-down, left-to-right *)

val exists_type_data:
  (('a)pre_typ -> (bool * ('a)option)) -> ('a)pre_typ -> (bool * ('a)option)
(* [exists_type_data p ty] Apply [p] to each sub-type of [ty], return [true] if
   any satisfies [p]. The check is top-down, left-to-right *)

(** {5 Basic Operations} *)

val compare: gtype -> gtype -> Order.t
(** Total order on types: Var < Constr < WeakVar. *)

val equals: gtype -> gtype -> bool
(** Syntactic equality between types. *)

(** {7 Recognisers} *)

val is_var: gtype -> bool
val is_weak: gtype -> bool
val is_ident: gtype -> bool
val is_constr: gtype -> bool
val is_app: gtype -> bool

(** {7 Constructors} *)

val mk_var: string -> gtype
val mk_weak: string -> gtype
val mk_ident: Ident.t -> gtype
val mk_constr: Ident.t -> gtype list -> gtype
val mk_app: gtype -> gtype -> gtype

(** {7 Destructors} *)

val dest_var: gtype -> gtype_id
val get_var_name: gtype -> string

val dest_weak: gtype -> gtype_id
val get_weak_name: gtype -> string

val dest_constr: gtype -> (Ident.t * gtype list)
val get_type_name: gtype -> Ident.t
(** [get_type_name ty]: Get the identifier of the constructor of type
    [ty].
*)

val map_atom: (gtype -> gtype) -> gtype -> gtype
val dest_app: gtype -> (gtype * gtype)
val flatten_app: gtype -> (gtype)list

val split_app: gtype -> (gtype *(gtype)list)
(** Split an application [x a1 .. an] into [(x, [a1; .. an])] *)

val map_atom: (gtype -> gtype) -> gtype -> gtype
(* [map_atom f ty] Apply [f] to each [Atom(x)] in [ty] returning the resulting
   type. *)

val fold_atom: ('a -> gtype -> 'a) -> 'a -> gtype -> 'a
(* [fold_atom f z ty] Fold [f] over each [Atom(x)] in [ty] returning the
   result. The fold is top-down, left-to-right *)

(** {6 Specialised Manipulators} *)

(** {7 Variable types} *)

val is_any_var: gtype -> bool
(** [is_any_var t]: true if [t] is a variable or a weak variable. *)

val normalize_vars: gtype -> gtype
(** Make all type variables with the same string name be the same
    variable. Useful when constructing types from existing types.
*)

val mk_typevar: int -> (int * gtype)
(** [mk_typevar n]: Make a new type variable [t'] with a name derived
    from [n] and return [(n + 1, t')]. Different values of [n] make
    different names. Names are constructed as sequences of alphabetic
    characters.
*)

val mk_plain_typevar: int -> (int * gtype)
(** [mk_typevar n]: Make a new type variable [t'] with a name derived
    from [n] and return [(n + 1, t')]. Different values of [n] make
    different names. Names are constructed as numbers prefixed by a
    string.
*)

(** {7 Unnamed type variables} *)

val mk_null: unit -> gtype
(** Make an unnamed type variable. *)
val is_null: gtype -> bool
(** Test for an unnamed type variable. *)

(** {7 Named typed constructors} *)

val mk_def: Ident.t -> gtype list -> gtype

(** {5 Type Definitions} *)

module TypeScope:
sig

  (** Records for type definitions *)
  type type_record =
    {
      name: string;               (** Type name *)
      args : string list;         (** Arguments appearing in the definition *)
      alias: gtype option;        (** The definition *)
    }

  (** Scope for type definitions *)
  type t =
    {
      type_defn: Ident.t -> type_record;  (** Get the type definition *)
      type_thy: string -> Ident.thy_id    (** Get the defining theory *)
    }

  (** The empty scope *)
  val empty: unit -> t

  (** Constructor *)
  val make:
    (Ident.t -> type_record)
    -> (string -> Ident.thy_id)
    -> t

  (** [defn_of scp i] Get the definition of [i] *)
  val defn_of: t -> Ident.t -> type_record

  (** [thy_of scp i] Get the theory defining of type [i] *)
  val thy_of: t -> string -> Ident.thy_id

  (** Extend a scope with a list of type definitions [[(I1, D1); ...;
    (In, Dn)]]. Each identifier [Ii] has definition [Di].
   *)
  val add_defns: t -> (Ident.t * type_record) list -> t

  (** Extend a scope with a list of type declarations [[(I1, A1); ...;
    (In, An)]]. Each identifier [Ii] has arguments [Ai], but no
    definition.
   *)
  val add_declns: t -> (Ident.t * (string) list) list -> t
end

type typedef_record = TypeScope.type_record
(** Records for type definitions. *)

val get_typdef: TypeScope.t -> Ident.t -> typedef_record
(** Get definition of type named [n] from scope [scp]. *)

(** {5 Data storage indexed by gtypes} *)

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
val subst_fold: (gtype -> gtype -> 'a -> 'a) -> substitution -> 'a -> 'a
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

val rename_index: int -> substitution -> gtype -> (gtype * int * substitution)
(** [rename_index t]: Make a type equivalent but not equal to [t],
    differing from [t] in the variable names. Use an integer to
    generate the type names.
*)

(* {5 Error reporting} *)

type error = { msg: string; typs: (gtype)list; next: (exn)option }

exception Error of error

val type_error: string -> gtype list -> exn
val add_type_error: string ->gtype list -> exn -> exn

val string_gtype: gtype -> string
(** Make a string representation of a type. *)

(** {5 Type definitions} *)

(** {7 Consistency tests for type definitions}

    Weak variables are not permitted in any definition (type or term).
*)

val check_decln: gtype -> bool
(**  [check_decln l]: Consistency check on declaration of type [l]. *)

val unfold: TypeScope.t -> gtype -> gtype
(**
   [unfold scp ty]: Unfold the definition of type [ty] from the scope
   [scp].

   @raise [Not_found] if no definition.
*)

val well_formed_full:
  (gtype -> (string * gtype)option)
  -> TypeScope.t -> gtype -> bool
(** [well_formed_full pred scp t]: ensure that [t] is well-formed

    [pred t] should return [None] for success and [Some(msg, errty)] for
    failure, where [msg] is an error message and [errty] is the type causing the
    error.

    - [Atom(Var(v))] at depth [d = 0]

    - [Atom(Weak(v))] at depth [d = 0]

    - [Atom(Ident(f))] and
    - [f] is in scope and
    - [d] is the arity of [f]

    - [TApp(l, r)] and
    - [l] is well-defined at depth [d + 1] and
    - [r] is  well-defined at depth [0]

    At type constructor [F/n] has arity [n]. With arguments [a_0, .., an], the
    type [(a_0, .., an)F] is formed with [TApp] by making [F] the left-most
    element with the [a_i] as the right branches. For [(a_0, .., an)F], this is
    [Tapp(..(TApp(Atom(Ident(F), a_0), a_1), ..), a_n)].

    Specific constructors formed by [TApp]:

    - [()F = Atom(Ident(F))]: [F] has arity [0] and is well-defined at depth
    [0].

    - [(a)F = TApp(Atom(Ident(f)), a)] [F] has arity [1] and is well-defined at
    depth [0].
*)

val well_formed: TypeScope.t -> gtype -> bool
(** [well_formed scp t]: ensure that [t] is well-formed in scope [scp] *)


(** [well_formed_full scp pred t]: ensure that [t] is well-formed declared.

    A type is well-formed at depth [d] if it satisifes [pred] and is one of:

    - [Atom(Var(v))] at depth [d = 0]

    - [Atom(Weak(v))] at depth [d = 0]

    - [Atom(Ident(f))] and
    - [f] is in scope and
    - [d] is the arity of [f]

    - [TApp(l, r)] and
    - [l] is well-defined at depth [d + 1] and
    - [r] is  well-defined at depth [0]

    At type constructor [F/n] has arity [n]. With arguments [a_0, .., an], the
    type [(a_0, .., an)F] is formed with [TApp] by making [F] the left-most
    element with the [a_i] as the right branches. For [(a_0, .., an)F], this is
    [Tapp(..(TApp(Atom(Ident(F), a_0), a_1), ..), a_n)].

    Specific constructors formed by [TApp]:

    - [()F = Atom(Ident(F))]: [F] has arity [0] and is well-defined at depth
    [0].

    - [(a)F = TApp(Atom(Ident(f)), a)] [F] has arity [1] and is well-defined at
    depth [0].
*)

val well_defined: TypeScope.t -> (string)list -> gtype -> unit
(** [well_defined scp args ty]: Test [ty] for well-definedness. every
    constructor occuring in [ty] must be defined. Variables in [ty]
    must have a name in [args] and weak variables are not permitted in
    [ty].
*)

val check_decl_type: TypeScope.t -> gtype -> unit
(** [check_decl_type scp ty]: Ensure type [ty] is suitable for the
    declaration of a term. Fails if [ty] contains a weak variable.
*)

(** {5 Unification} *)

val lookup_var: gtype -> substitution -> gtype
(** [lookup_var ty env]: Look-up and chase var [ty] in env [environment]. *)

val occurs: gtype -> gtype -> bool
(** [occurs t r]: Occurs check.

   return [true] iff atomic type [t] occurs in [r]
 *)

(**
   [occurs_env env t r]: Occurs check w.r.t [env]. Chase [t] in [env]
   to get [t'], chase [r] in [env] to get [r'].

   @raise [typeError] if [t'] is not atomic or if [t'] occurs in [r'], succeed
   silently otherwise.
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
  TypeScope.t -> gtype -> gtype
  -> substitution -> substitution
(** [unify_env scp ty1 ty2 env]: Unify two types in context [env],
    return a new subsitution.
*)
val unify: TypeScope.t -> gtype -> gtype -> substitution
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
  TypeScope.t -> substitution
  -> gtype -> gtype -> substitution
(**
   [matching_env scp env t1 t2]: Match type [t1] with type [t2] w.r.t
   context [env]. This unifies [t1] and [t2], but only variables in
   type [t1] can be bound.

   Raises an exception if matching fails.
*)

val matches_env:
  TypeScope.t -> substitution
  -> gtype -> gtype -> substitution
(** [matches_env scp env t1 t2]: Match type [t1] with type [t2] w.r.t
    context [env]. This unifies [t1] and [t2], but only variables in
    type [t1] can be bound.

    Silently returns unchanged substitution on failure.
*)

val matches: TypeScope.t -> gtype -> gtype -> bool
(** Toplevel for [matches_env]. *)

(** {5 More functions} *)

val set_name:
  ?memo:(string, Ident.thy_id)Hashtbl.t
  -> TypeScope.t -> gtype -> gtype
(** [set_name ?strict ?memo scp ty]: set names in type [ty] to their
    long form.

    If [strict] then fail if any type name doesn't occur in scope [scp].

    [memo] is the optional memoisation table.
*)

(*
val in_scope:
  (string, bool)Lib.substype -> TypeScope.t -> gtype -> bool
(** [in_scope memo scp th ty]: Check that [ty] is in scope by checking
    that every type constructor is decared or defined in scope [scp].

    The function is memoised: if a constructor name is found to be
    in scope, it is added to [memo].
*)
*)

val extract_bindings: gtype list -> substitution -> substitution
  -> substitution
(** [extract_bindings vars src dst]: extract bindings variables in
    [var] from [src] substitution, store them in [dst] substitution

    Needed by the sequent calculus to determine the bindings made by
    operations.
*)

(** {5 Saving gtypes to disk storage} *)

type satom =
  | SVar of (string * int) (* Variables *)
  | SIdent of Ident.t      (* Identifier *)
type stype = (satom) pre_typ
(** Representation of types for storage on disk. *)

type stypedef_record =
  {
    sname: string;
    sargs: string list;
    salias: stype option
  }
(** Representation of typedef_records for disk storage. *)

type to_stype_env = (gtype_id * (string *int)) list
(** Data needed to construct a type storage representation. *)

val to_save_env: to_stype_env -> gtype -> (stype * to_stype_env)
(** [to_save_env ty env]: Convert [ty] to [stype] storage
    representation.  [env] store the names of type variables already
    encountered.
*)

val to_save: gtype -> stype
(** Toplevel for [to_save_env]. *)

type from_stype_env = ((string * int) * gtype_id) list
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

(** Debugging information *)
val unify_aux:
  TypeScope.t -> gtype -> gtype
  -> substitution -> substitution
(** [unify_env scp ty1 ty2 env]: Unify two types in context [env],
    return a new subsitution.
*)
