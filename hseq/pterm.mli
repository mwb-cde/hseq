(*----
  Name: pterm.mli
  Copyright Matthew Wahab 2005-2019
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

(** Parsed terms

    The term representation constructed by parsers. Differs from the
    standard representation in providing a typing constructor to
    specify the type of a term.

    A parser constructs a term as element of type {!Pterm.t} which is
    then converted to the representation {!Basic.term} used by the
    theorem prover by removing explicit type specifications and using
    them to infer the types required for identifiers and variables.

    In addition, operator overloading is supported, in module
    {!Pterm.Resolver}, by allowing symbols to be resolved to identifiers
    based on the expected type of the identifier.
*)

open Basic

(** The representation of a parsed term *)
type t =
  | PId of Ident.t * Gtype.t   (** Identifiers *)
  | PBound of binders     (** Bound variables *)
  | PFree of string * Gtype.t  (** Free variables *)
  | PMeta of binders       (** Meta variables (use for skolem constants) *)
  | PApp of t * t    (** Function application *)
  | PQnt of binders * t (** Binding terms *)
  | PConst of const_ty     (** Constants *)
  | PTyped of t * Gtype.t  (** Typed terms *)


(** {5 Operations on terms} *)

(** {7 Recognisers} *)

val is_qnt: t -> bool
val is_app: t -> bool
val is_bound: t -> bool
val is_free: t -> bool
val is_ident: t -> bool
val is_typed: t -> bool
val is_const: t -> bool

(** {7 Constructors} *)

val mk_qnt: binders -> t -> t
val mk_bound: binders -> t
val mk_free: string -> Gtype.t -> t
val mk_app: t -> t -> t
val mk_typed: t -> Gtype.t -> t
val mk_const: Basic.const_ty -> t
val mk_typed_ident: Ident.t -> Gtype.t -> t
val mk_ident: Ident.t -> t
val mk_short_ident: string -> t

(** {7 Destructors} *)

val dest_qnt: t -> (binders * t)
val dest_bound: t -> binders
val dest_free:t -> (string * Gtype.t)
val dest_app: t -> (t * t)
val dest_typed: t -> (t * Gtype.t)
val dest_const: t -> Basic.const_ty
val dest_ident: t -> (Ident.t * Gtype.t)

(** {6 Specialised Manipulators} *)

(** {7 Meta variables} *)

val mk_meta: string -> Gtype.t -> t
val is_meta: t -> bool
val dest_meta: t -> binders

(** {7 Constants} *)

val destbool: t -> bool
val mk_bool: bool -> t

val mk_comb: t -> t list -> t
(** [mk_comb x y]: Make a function application from [x] and [y].
    [mk_comb f [a1;a2;...;an]] is [((((f a1) a2) ...) an)]
*)

val mk_fun: Ident.t -> t list -> t
(** [mk_fun f args]: make function application [f args]. *)

(** Operator Overloading *)
module Resolver:
sig
  (** {7 Overloading}

      Operator overloading works by maintaining a list of identifiers
      which have the same symbol together with their types. When the
      symbol occurs in a term, as a short name, a type is inferred for
      the name and the list of identifiers is searched for a matching
      type. The first matching identifier is used. If there is no
      match, the first identifier in the list is chosen.

      The standard table for operator overloading is
      {!Parser.overload_table}, which maintains a list of identifiers
      and types for each overloaded symbols. Identifiers are normally
      added to the front of the list but a position can be passed, to
      prefer one identifier over others. (The search begins from the
      front of the list.)

      The toplevel for operator overloading is
      {!Pterm.Resolver.resolve_term} which takes a function which
      carries out the search for an identifier with a matching
      type. Function {!Pterm.Resolver.make_lookup} constructs a
      suitable search function, from a symbol look-up table.  *)

  val resolve_term:
    Scope.t
    -> (string -> Gtype.t -> (Ident.t * Gtype.t))
    -> t
    -> (Basic.term * Gtype.Subst.t)
  (** [resolve_term scp env t]: Resolve the symbols in term [t].

      For each free variable [Free(s, ty)] in [t], lookup
      [s] in [env] to get long identifier [id].  If not
      found, use [Free(s, ty)].  If found, replace
      [Free(s, ty)] with the identifier [Id(id, ty)].

      [env] should return an identifier-type pair where
      type matches (in some sense) [ty].

      [env] must raise Not_found if [s] is not found.
  *)

  val make_lookup:
    Scope.t
    -> (string -> (Ident.t * Gtype.t) list)
    -> (string -> Gtype.t -> (Ident.t * Gtype.t))
  (** [make_lookup scp db]: Make an environment suitable for
      {!Pterm.Resolver.resolve_term} from table [db].

      [db] must raise [Not_found] when items are not found.

      [make_lookup db s ty]: returns the identifier-type
      pair associated by [db] with [s] for which [ty] is
      unifies with type in scope [scp].

      [make_lookup db s ty] raise Not_found if [s] is not
      found in [db].
  *)

  (** {7 Debugging} *)

  val default:
    string -> Gtype.t -> (Ident.t * Gtype.t) list
    -> (Ident.t * Gtype.t) option

  type resolve_memo =
      {
        types: (Ident.t, Gtype.t)Hashtbl.t;
        idents: (string, Ident.t)Hashtbl.t;
        symbols: (string, Ident.t)Hashtbl.t;
        type_names: (string, Ident.thy_id)Hashtbl.t
      }

  type resolve_arg =
      {
        scp: Scope.t;
        inf: int;
        memo: resolve_memo;
        qnts: Term.Subst.t;
        lookup: (string -> Gtype.t -> (Ident.t * Gtype.t))
      }

  val resolve_aux:
    resolve_arg
    -> Gtype.Subst.t
    -> Gtype.t
    -> t
    -> (Basic.term * Gtype.t * Gtype.Subst.t * resolve_arg)

  val memo_find:
    ('a, 'b)Hashtbl.t
    -> ('a -> 'c -> 'b)
    -> 'c
    -> 'a -> 'b

  val find_type:
    Scope.t
    -> string
    -> Gtype.t -> (Ident.t * Gtype.t) list
    -> (Ident.t * Gtype.t)

end

(** {5 Conversion to-from terms} *)

val from_term: term -> t
(** [from_term trm]: Construct the represententation of [trm].
*)

val to_term: t -> term
(** [to_term pt]: Construct the term represented by [pt].  The typing
    constructers ([PTyped]) in [pt] are removed and used to infer the
    expected types of the identifiers and variables in the result.
*)

(** {7 Conversion with overloading} *)

val resolve:
  Scope.t
  -> (string -> Gtype.t -> (Ident.t * Gtype.t))
  -> t
  -> (Basic.term * Gtype.Subst.t)
(** [resolve scp env pt]: Construct the term represented by [pt],
    resolving overloaded operators and returning the type environment
    built up. The typing constructers ([PTyped]) in [pt] are removed
    and used to infer the expected types of the identifiers and
    variables in the result.

    This is the same as {!Pterm.Resolver.resolve_term}.  For each free
    variable [Free(s, ty)] in [t], lookup [s] in [env] to get long
    identifier [id].  If not found, use [Free(s, ty)].  If found,
    replace [Free(s, ty)] with the identifier [Id(id, ty)].

    [env] should return an identifier-type pair where type matches (in
    some sense) [ty].

    [env] must raise [Not_found] if [s] is not found.
*)

val make_lookup:
  Scope.t
  -> (string -> (Ident.t * Gtype.t) list)
  -> (string -> Gtype.t -> (Ident.t * Gtype.t))
(** [make_lookup scp db]: Make an environment suitable for
    {!Pterm.resolve} from table [db].

    This is the same as {!Pterm.make_lookup}.

    [make_lookup db s ty]: returns the identifier-type pair associated
    by [db] with [s] for which [ty] is unifies with type in scope
    [scp].

    [db] must raise [Not_found] when items are not found.

    [make_lookup db s ty] raises [Not_found] if [s] is not found in
    [db].
*)
