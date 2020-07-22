(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(**
    Scope of terms and types.

    A scope stores information about the term and type identifiers
    which are available for use, the type of each declared identifier,
    the definition of type aliases and whether a theory is in scope.

    Each scope is associated with a theory [th] and a theory [x] is in
    scope if it is [th] or is a parent of [th].
*)


(** {5 Data structures} *)

(** Theory markers. *)
type marker = (string)Tag.t      (** The unique identifier of the theory. *)

val mk_marker: string -> marker
(** Marker constructor *)
val marker_name : marker -> string
(** Marker destructor *)

type meta_db  (* = (Term.binders)Treekit.StringTree.t  *)
(** Meta variables

    A record of the meta variables in a particular scope. These are
    only needed by goal scopes.
*)

val empty_meta_db: unit -> meta_db
(** The empty [meta_db]. *)

val meta_db_add: string -> Term.Binder.t -> meta_db -> meta_db
(** Add a binder with the given name to a [meta_db].  Replaces any
    previous binder for the name.
*)

val meta_db_find: string -> meta_db -> Term.Binder.t
(** Find the binder for the given name. Raise [Not_found] if no such
    binder.
*)

(** Records for type definitions *)
type type_record = Gtype.typedef_record

val mk_type_record:
  string -> string list -> (Gtype.t)option -> type_record
val dest_type_record:
  type_record -> (string * (string)list * (Gtype.t)option)

(** Scope records. *)
type t =
    {
      curr_thy : marker;
      (** The marker of the current theory. *)
      term_type : Ident.t -> Gtype.t;
      (** The type of a term identifier. *)
      term_thy : string -> Ident.thy_id;
      (** The theory in which a term is declared. *)
      types: Gtype.TypeScope.t;
      (** The types scope *)
      thy_in_scope : Ident.thy_id -> bool ;
      (** Whether a theory is in scope (identified by name). *)
      marker_in_scope : marker -> bool ;
      (** Whether a theory is in scope (identified by marker). *)
      meta_vars: meta_db
    (** A record of meta variables in this scope. *)
    }
(** All lookup functions raise [Not_found] on failure. *)

(** {6 Operations on scopes} *)

val empty_scope : unit -> t
(** Construct an empty scope. *)

val marker_of : t -> marker
(** [thy_of scp]: Get the theory marker of scope [scp]. *)

val thy_of : t -> Ident.thy_id
(** [thy_of scp]: Get the theory name of scope [scp]. *)

val type_of : t -> Ident.t -> Gtype.t
(** Lookup the type of an identifier. *)

val thy_of_term: t -> string -> Ident.thy_id
(** Lookup the theory of an identifier. *)

val types_scope: t -> Gtype.TypeScope.t
(** Get the types scope *)

val defn_of: t -> Ident.t -> type_record
(** Get the definition of a type. *)

val thy_of_type: t -> string -> Ident.thy_id
(** Lookup the theory of a type. *)

val in_scope : t -> Ident.thy_id -> bool
(** Test whether a theory is in scope (by name). *)

val in_scope_marker : t -> marker -> bool
(** Test whether a theory is in scope (by marker). *)

(** {5 Extending scopes} *)

val extend_with_terms: t -> (Ident.t * Gtype.t) list -> t
(** Extend a scope with a list of identifiers [[(I1, T1); ...; (In,
    Tn)]]. Each identifier [Ii] is given type [Ti].
*)

val extend_with_typedefs: t -> (Ident.t * type_record) list -> t
(** Extend a scope with a list of type definitions [[(I1, D1); ...;
    (In, Dn)]]. Each identifier [Ii] has definition [Di].
*)

val extend_with_typedeclns: t -> (Ident.t * (string) list) list -> t
(** Extend a scope with a list of type declarations [[(I1, A1); ...;
    (In, An)]]. Each identifier [Ii] has arguments [Ai], but no
    definition.
*)

val new_local_scope: t -> t
(** Introduce a new local scope, derived from the current theory
    marker.  The new marker is set as the current theory.
*)

val add_meta: t -> Term.Binder.t -> t
(** [add_meta scp v]: Add [v] as a new meta variable. Fails if there
    is already a meta variable with the same name as [v].
*)

val find_meta: t -> string -> Term.Binder.t
(** [find_meta scp n]: Find the meta-variable named [n].

    @raise [Not_found] if no meta-variable named [n].
*)

val is_meta: t -> Term.Binder.t -> bool
(** [is_meta scp v]: Test whether [v] is a meta variable in scope
    [scp].
*)

(** Generate a scope that will associate any unknown identifier with the current
    theory. (Only needed for [Gtype.set_names]) *)
val relaxed: t -> t
