(*-----
   Name: scope.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(* Scope of terms and types *)

open Basic

(* records for type definitions *)
type type_record =
    {
     name: string; 
     args : string list; 
     alias: gtype option;
     characteristics: string list
   }

type t=
    { 
      curr_thy : thy_id;
      term_type : ident -> gtype; 
	term_thy : string -> thy_id;
	  type_defn: ident -> type_record;
	    type_thy : string -> thy_id;
(*	      thy_in_scope : thy_id -> thy_id -> bool *)
	      thy_in_scope : thy_id -> bool 
    }

val empty_scope : unit -> t

val thy_of : t -> thy_id
val type_of : t -> ident -> gtype
val thy_of_term: t -> string -> thy_id
val defn_of: t -> ident -> type_record
val thy_of_type: t -> string -> thy_id
(*
val in_scope_of : t-> thy_id -> thy_id -> bool
*)
val in_scope : t -> thy_id -> bool

val extend_with_terms: t -> (ident * gtype) list -> t
val extend_with_typedefs: t -> (ident * type_record) list -> t
val extend_with_typedeclns: t -> (ident * (string) list) list -> t


