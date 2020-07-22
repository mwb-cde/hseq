(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

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
  | App of (('a) pre_typ * ('a) pre_typ)
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

type t = (atomtype)pre_typ
(** The actual representation of types. *)

val mk_vartype: gtype_id -> t
val mk_weakvartype: gtype_id -> t
val mk_identtype: Ident.t -> t
val mk_apptype: t -> t -> t

val flatten_apptype: t -> (t)list
(**
   [flatten_apptype ty]: flatten an application in [ty] to a list of
   types.  [flatten_apptype (((f a1) a2) a3)] is [[f; a1; a2; a3]] and
   [flatten_apptype (((f a1) (g a2)) a3)] is [[f; a1; (g a2); a3]]

   If [ty] is not an applictaion then returns [[ty]].
*)

val split_apptype: t -> (t *(t)list)
(** Split an application [x a1 .. an] into [(x, [a1; .. an])] *)

val map: (('a)pre_typ -> ('a)pre_typ) -> ('a)pre_typ -> ('a)pre_typ
(* [map f ty] Apply [f] to each subterm [t] of [ty]. *)

val iter_up: (('a)pre_typ -> unit) -> ('a)pre_typ -> unit
(* [iter_down f ty] Apply [f] to each subterm [t] of [ty], starting at the
   bottom and working up. *)

val iter_down: (('a)pre_typ -> unit) -> ('a)pre_typ -> unit
(* [map_down f ty] Apply [f] to each subterm [t] of [ty], starting at the
   top and working down. *)

val fold_up: ('a -> ('b)pre_typ -> 'a) -> 'a -> ('b)pre_typ -> 'a
(* [fold_up f ty] Apply [f] to each subterm in [ty], working from the bottom
   up. ([fold_down] is more efficient) *)

val fold_down: ('a -> ('b)pre_typ -> 'a) -> 'a -> ('b)pre_typ -> 'a
(* [fold_down f ty] Apply [f] to each subterm in [ty], working from the top
   down *)

val exists: (('a)pre_typ -> bool) -> ('a)pre_typ -> bool
(* [exists p ty] Apply [p] to each sub-type of [ty], return [true] if any
   satisfies [p]. The check is top-down, left-to-right *)

val exists_data:
  (('a)pre_typ -> (bool * ('a)option)) -> ('a)pre_typ -> (bool * ('a)option)
(* [exists_data p ty] Apply [p] to each sub-type of [ty], return [true] if
   any satisfies [p]. The check is top-down, left-to-right *)

(** {5 Basic Operations} *)

val compare: t -> t -> Order.t
(** Total order on types: Var < Constr < WeakVar. *)

val equals: t -> t -> bool
(** Syntactic equality between types. *)

(** {7 Recognisers} *)

val is_atom: t -> bool
val is_var: t -> bool
val is_weak: t -> bool
val is_ident: t -> bool
val is_constr: t -> bool
val is_app: t -> bool

(** {7 Constructors} *)

val mk_var: string -> t
val mk_weak: string -> t
val mk_ident: Ident.t -> t
val mk_constr: Ident.t -> t list -> t
val mk_app: t -> t -> t

(** {7 Destructors} *)

val dest_var: t -> gtype_id
val get_var_name: t -> string

val dest_weak: t -> gtype_id
val get_weak_name: t -> string

val dest_constr: t -> (Ident.t * t list)
val get_type_name: t -> Ident.t
(** [get_type_name ty]: Get the identifier of the constructor of type
    [ty].
*)

val dest_app: t -> (t * t)
val flatten_app: t -> (t)list

val split_app: t -> (t *(t)list)
(** Split an application [x a1 .. an] into [(x, [a1; .. an])] *)

(*
val map_atom: (t -> t) -> t -> t
(* [map_atom f ty] Apply [f] to each [Atom(x)] in [ty] returning the resulting
   type. *)
 *)
(*
val fold_atom: ('a -> t -> 'a) -> 'a -> t -> 'a
(* [fold_atom f z ty] Fold [f] over each [Atom(x)] in [ty] returning the
   result. The fold is top-down, left-to-right *)
 *)

(** {6 Specialised Manipulators} *)

(** {7 Variable types} *)

val is_any_var: t -> bool
(** [is_any_var t]: true if [t] is a variable or a weak variable. *)

val normalize_vars: t -> t
(** Make all type variables with the same string name be the same
    variable. Useful when constructing types from existing types.
*)

val mk_typevar: int -> (int * t)
(** [mk_typevar n]: Make a new type variable [t'] with a name derived
    from [n] and return [(n + 1, t')]. Different values of [n] make
    different names. Names are constructed as sequences of alphabetic
    characters.
*)

val mk_plain_typevar: int -> (int * t)
(** [mk_typevar n]: Make a new type variable [t'] with a name derived
    from [n] and return [(n + 1, t')]. Different values of [n] make
    different names. Names are constructed as numbers prefixed by a
    string.
*)

(** {7 Unnamed type variables} *)

val mk_null: unit -> t
(** Make an unnamed type variable. *)
val is_null: t -> bool
(** Test for an unnamed type variable. *)

(** {7 Named typed constructors} *)

val mk_def: Ident.t -> t list -> t

(** {5 Type Definitions} *)

module TypeScope:
sig

  (** Records for type definitions *)
  type type_record =
    {
      name: string;            (** Type name *)
      args : string list;      (** Arguments appearing in the definition *)
      alias: (t)option;        (** The definition = (Gtype.t)option *)
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
    (Ident.t -> type_record) -> (string -> Ident.thy_id) -> t

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

(** {5 Data storage indexed by ts} *)

(** {7 Balanced Trees} *)
module TypeTreeData: Treekit.TreeData

module TypeTree:
  (Treekit.BTreeType with type key = t)

type ('a)tree = ('a)TypeTree.t
(** Balanced trees indexed by ts *)


(** {5 Substitution} *)
module Subst:
sig
  (** The type of substitutions *)
  type ty = t (* Gtype.t *)
  type t

  val empty: unit -> t
  (** Make an empty substitution. *)
  val bind: ty -> ty -> t -> t
  (** [bind t r env]: Bind [r] to [t] in substitution [env]. *)
  val delete: ty -> t -> t
  (** [delete t env]: Delete the binding of [t] in [env]. *)
  val lookup: ty -> t -> ty
  (** [lookup t env]: Get the binding of [t] in [env]. *)
  val member: ty -> t -> bool
  (** [member t env]: True if [t] has a binding in [env]. *)
  val subst_iter: (ty -> ty -> unit) -> t -> unit
  (** [subst_iter f env]: Apply function [f] to each binding in [env]. *)
  val subst_fold: (ty -> ty -> 'a -> 'a) -> t -> 'a -> 'a
  (** [subst_fold f val env]: Fold function [f] over the bindings in [env]. *)
end

val subst: t -> Subst.t -> t
(** [subst env t]: Apply substitution [env] to t [t]. This is
    simultaneous substitution: the substitution is not pushed into the
    replacement terms. This is therefore unsuitable for forming the
    most general unifier since unification can bind variables in both
    the replaced and the replacement term.
*)

(** {6 Operations which depend on substitution} *)

val rename_type_vars_env: Subst.t -> t -> (t * Subst.t)
(** copy a type, making new variables in the type. *)
val rename_type_vars: t -> t
(** [rename_type_vars t]: Make a type equivalent but not equal to [t],
    differing from [t] in the variable names.
*)

val rename_index: int -> Subst.t -> t -> (t * int * Subst.t)
(** [rename_index t]: Make a type equivalent but not equal to [t],
    differing from [t] in the variable names. Use an integer to
    generate the type names.
*)

(* {5 Error reporting} *)

type error = { msg: string; typs: (t)list; next: (exn)option }

exception Error of error

val type_error: string -> t list -> exn
val add_type_error: string ->t list -> exn -> exn

val string_gtype: t -> string
(** Make a string representation of a type. *)

(** {5 Type definitions} *)

(** {7 Consistency tests for type definitions}

    Weak variables are not permitted in any definition (type or term).
*)

val check_decln: t -> bool
(**  [check_decln l]: Consistency check on declaration of type [l]. *)

val unfold: TypeScope.t -> t -> t
(**
   [unfold scp ty]: Unfold the definition of type [ty] from the scope
   [scp].

   @raise [Not_found] if no definition.
*)

val well_formed_full:
  (t -> (string * t)option)
  -> TypeScope.t -> t -> bool
(** [well_formed_full pred scp t]: ensure that [t] is well-formed

    [pred t] should return [None] for success and [Some(msg, errty)] for
    failure, where [msg] is an error message and [errty] is the type causing the
    error.

    - [Atom(Var(v))] at depth [d = 0]

    - [Atom(Weak(v))] at depth [d = 0]

    - [Atom(Ident(f))] and
    - [f] is in scope and
    - [d] is the arity of [f]

    - [App(l, r)] and
    - [l] is well-defined at depth [d + 1] and
    - [r] is  well-defined at depth [0]

    A type constructor [F/n] has arity [n]. With arguments [a_0, .., an], the
    type [(a_0, .., an)F] is formed with [App] by making [F] the left-most
    element with the [a_i] as the right branches. For [(a_0, .., an)F], this is
    [Tapp(..(App(Atom(Ident(F), a_0), a_1), ..), a_n)].

    Specific constructors formed by [App]:

    - [()F = Atom(Ident(F))]: [F] has arity [0] and is well-defined at depth
    [0].

    - [(a)F = App(Atom(Ident(f)), a)] [F] has arity [1] and is well-defined at
    depth [0].
*)

val well_formed: TypeScope.t -> t -> bool
(** [well_formed scp t]: ensure that [t] is well-formed in scope [scp] *)


(** [well_formed_full scp pred t]: ensure that [t] is well-formed declared.

    A type is well-formed at depth [d] if it satisifes [pred] and is one of:

    - [Atom(Var(v))] at depth [d = 0]

    - [Atom(Weak(v))] at depth [d = 0]

    - [Atom(Ident(f))] and
    - [f] is in scope and
    - [d] is the arity of [f]

    - [App(l, r)] and
    - [l] is well-defined at depth [d + 1] and
    - [r] is  well-defined at depth [0]

    At type constructor [F/n] has arity [n]. With arguments [a_0, .., an], the
    type [(a_0, .., an)F] is formed with [App] by making [F] the left-most
    element with the [a_i] as the right branches. For [(a_0, .., an)F], this is
    [Tapp(..(App(Atom(Ident(F), a_0), a_1), ..), a_n)].

    Specific constructors formed by [App]:

    - [()F = Atom(Ident(F))]: [F] has arity [0] and is well-defined at depth
    [0].

    - [(a)F = App(Atom(Ident(f)), a)] [F] has arity [1] and is well-defined at
    depth [0].
*)

val well_defined: TypeScope.t -> (string)list -> t -> unit
(** [well_defined scp args ty]: Test [ty] for well-definedness. every
    constructor occuring in [ty] must be defined. Variables in [ty]
    must have a name in [args] and weak variables are not permitted in
    [ty].
*)

val check_decl_type: TypeScope.t -> t -> unit
(** [check_decl_type scp ty]: Ensure type [ty] is suitable for the
    declaration of a term. Fails if [ty] contains a weak variable.
*)

(** {5 Unification} *)

val lookup_var: t -> Subst.t -> t
(** [lookup_var ty env]: Look-up and chase var [ty] in env [environment]. *)

val occurs: t -> t -> bool
(** [occurs t r]: Occurs check.

   return [true] iff atomic type [t] occurs in [r]
 *)

(**
   [occurs_env env t r]: Calculate [occurs t (subst r env)]

   @raise [typeError] if [t] is not atomic or if [t] occurs in [(subst r env)],
   succeed silently otherwise.  *)

val bind_occs: t -> t -> Subst.t -> Subst.t
(** [bind_occs t r env]: Bind [r] to [t] in [env]. Fails if [t] occurs
    in [subst r env].
*)

(** {6 Unification functions} *)

(** Unification of ts [x] and [y] tries to create a substitution
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
  TypeScope.t -> t -> t
  -> Subst.t -> Subst.t
(** [unify_env scp ty1 ty2 env]: Unify two types in context [env],
    return a new subsitution.
*)
val unify: TypeScope.t -> t -> t -> Subst.t
(** [unify]: unify two types, returning the substitution.
*)

(** {7 Most General Unifiers} *)

val mgu: t -> Subst.t -> t
(** [mgu ty env]: Construct the most general unifier for type [ty]
    from substitution [env]. This is a version of substitution which
    pushes the substitution into the replacement terms.
*)

val mgu_rename_env: (int * Subst.t) -> Subst.t
  -> t -> (t * (int * Subst.t))
(** [mgu_rename_env inf env nenv ty]: Replace variables in [ty] with
    their bindings in substitution [env].  If a variable isn't bound
    in [env], then it is renamed and bound to that name in [nenv]
    (which is checked before a new name is created).

    [env] is type substitution found e.g. by typechecking [nenv] is
    substitution in which to store the new type variables.

    Returns the new type and updated nenv.
*)
val mgu_rename:
  int -> Subst.t
  -> Subst.t -> t
  -> t

val mgu_rename_simple: int -> Subst.t -> Subst.t
  -> t -> (t * int *Subst.t)
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
  TypeScope.t -> Subst.t
  -> t -> t -> Subst.t
(**
   [matching_env scp env t1 t2]: Match type [t1] with type [t2] w.r.t
   context [env]. This unifies [t1] and [t2], but only variables in
   type [t1] can be bound.

   Raises an exception if matching fails.
*)

val matches_env:
  TypeScope.t -> Subst.t
  -> t -> t -> Subst.t
(** [matches_env scp env t1 t2]: Match type [t1] with type [t2] w.r.t
    context [env]. This unifies [t1] and [t2], but only variables in
    type [t1] can be bound.

    Silently returns unchanged substitution on failure.
*)

val matches: TypeScope.t -> t -> t -> bool
(** Toplevel for [matches_env]. *)

(** {5 More functions} *)

(** [set_name memo scp ty]: set names in type [ty] to their
    long form.

    [set_name_memoized] is the memoized version
*)
val set_name: TypeScope.t -> t -> t
val set_name_memoized:
  (string)Lib.StringMap.t
  -> TypeScope.t -> t
  -> (t * (string)Lib.StringMap.t)

val extract_bindings: t list -> Subst.t -> Subst.t
  -> Subst.t
(** [extract_bindings vars src dst]: extract bindings variables in
    [var] from [src] substitution, store them in [dst] substitution

    Needed by the sequent calculus to determine the bindings made by
    operations.
*)

(** {5 Saving ts to disk storage} *)

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

type to_stype_env = (gtype_id * (string * int)) list
(** Data needed to construct a type storage representation. *)

val to_save_env: to_stype_env -> t -> (stype * to_stype_env)
(** [to_save_env ty env]: Convert [ty] to [stype] storage
    representation.  [env] store the names of type variables already
    encountered.
*)

val to_save: t -> stype
(** Toplevel for [to_save_env]. *)

type from_stype_env = ((string * int) * gtype_id) list
(** Data needed to construct a type storage representation. *)

val from_save_env:
  from_stype_env -> stype -> (t * from_stype_env)
(** [from_save_env ty env]: Convert storage [ty] to [t]
    representation.  [env] store the names of type variables already
    encountered.
*)

val from_save: stype -> t
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

val print_subst: Subst.t -> unit

(** Debugging information *)
val unify_aux:
  TypeScope.t -> t -> t
  -> Subst.t -> Subst.t
(** [unify_env scp ty1 ty2 env]: Unify two types in context [env],
    return a new subsitution.
*)
