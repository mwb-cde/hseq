(*-----
   Name: scope.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(** 
   Scope of terms and types. 

   A scope stores information about the term and type identifiers
   which are available for use, the type of each declared identifier,
   the definition of type aliases and whether a theory is in scope.

   Each scope is associated with a theory [th] and a theory [x] is in
   scope if it is [th] or is a parent of [th].
*)

open Basic

(** {5 Data structures} *)

(** Theory markers. *)
type marker = Tag.t      (** The unique tag of the theory. *)

val mk_marker: string -> marker
(** Marker constructor *)
val marker_name : marker -> string
(** Marker destructor *)

(** Records for type definitions *)
type type_record =
    {
     name: string; 
     args : string list; 
     alias: gtype option;
     characteristics: string list
   }

(** Scope records. *)
type t=
    { 
      curr_thy : marker;
(** The marker of the current theory *)
      term_type : Ident.t -> gtype; 
	(** The type of a term identifier. *)
	term_thy : string -> Ident.thy_id;
	  (** The theory in which a term is declared. *)
	  type_defn: Ident.t -> type_record;
	    (** The definition (if any) of a type *)
	    type_thy : string -> Ident.thy_id;
	      (** The theory in which a type is declared *)
	      thy_in_scope : Ident.thy_id -> bool ;
		  (** Whether a theory is in scope (identified by name). *)
		marker_in_scope : marker -> bool 
		  (** Whether a theory is in scope (identified by marker). *)
    }
(** All lookup functions raise [Not_found] on failure. *)

(** {6 Operations on scopes} *)

val empty_scope : unit -> t
(** Construct an empty scope *)

val marker_of : t -> marker
(** [thy_of scp]: Get the theory marker of scope [scp] *)

val thy_of : t -> Ident.thy_id
(** [thy_of scp]: Get the theory name of scope [scp] *)

val type_of : t -> Ident.t -> gtype
(** Lookup the type of an identifier *)

val thy_of_term: t -> string -> Ident.thy_id
(** Lookup the theory of an identifier *)

val defn_of: t -> Ident.t -> type_record
(** Get the definition of a type. *)

val thy_of_type: t -> string -> Ident.thy_id
(** Lookup the theory of a type. *)

val in_scope : t -> Ident.thy_id -> bool
(** Test whether a theory is in scope (by name) *)

val in_scope_marker : t -> marker -> bool
(** Test whether a theory is in scope (by marker) *)

(** {5 Extending scopes} *)

val extend_with_terms: t -> (Ident.t * gtype) list -> t 
(** 
   Extend a scope with a list of identifiers [[(I1, T1); ...; (In,
   Tn)]]. Each identifier [Ii] is given type [Ti].
*)

val extend_with_typedefs: t -> (Ident.t * type_record) list -> t
(** 
   Extend a scope with a list of type definitions [[(I1, D1); ...;
   (In, Dn)]]. Each identifier [Ii] has definition [Di].
*)

val extend_with_typedeclns: t -> (Ident.t * (string) list) list -> t
(** 
   Extend a scope with a list of type declarations [[(I1, A1); ...;
   (In, An)]]. Each identifier [Ii] has arguments [Ai], but
   no definition.
*)


