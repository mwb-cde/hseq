(* Global environment and utility functions *)

(* destruct a long identifier *)
val dest_name : string -> string * string

(* default theory and theoryDB*)

val empty_thy_name : string
val base_thy_name : string

val base_thy : unit -> Theory.thy
val thdb : unit -> Thydb.thydb


(* current theoryDB *)
val theories : Thydb.thydb ref
(* get/set theoryDB and current theory *)
val get_theories : unit -> Thydb.thydb
val set_theories : Thydb.thydb -> unit
val get_cur_thy : unit -> Theory.thy
val get_cur_name : unit -> string

(* get current scope *)

(*
val stdtypenv : unit -> Gtypes.scope
*)

val set_cur_thy : Theory.thy -> unit

(* filenames and paths for theory files *)

val thy_suffix: string
val get_thy_path : unit -> string list
val add_thy_path : string -> unit
val set_thy_path: string list -> unit
val remove_from_path: string -> unit
val find_thy_file: string -> string

val get_cdir : unit -> string

(* current scope (synonym for stdtypenv()) *)
(*
val typenv : unit -> Gtypes.scope
*)
val scope : unit -> Gtypes.scope

(* build PP record *)
(*
val build_type_info : unit -> unit
val build_id_info : unit -> unit
*)

(* function to invoke when loading theories from disk *)

val on_load_thy: Theory.thy -> unit

(* parsing functions *)
(* does error handling (from exception to Result.Error)
   so all calls to the parser should go through here *)

val mkterm : Gtypes.scope -> Term.term -> Term.term
val mkterm_unchecked : Gtypes.scope -> Term.term -> Term.term
val read : string -> Term.term
val read_unchecked : string -> Term.term
val read_defn :
    string -> (string * (string * Gtypes.gtype) list) * Term.term
val read_type_defn : string -> string * string list * Gtypes.gtype option
val read_type : string -> Gtypes.gtype
val read_fulltype : string -> Gtypes.gtype
val read_identifier: string -> Basic.ident

(* PP information *)

(* 
   [tp_pp_info]
   The system PP information store (of size Basic.PP.default_info_size).
 *)
val tp_pp_info: Basic.PP.info ref

(**
   [pp_info()]
   Get the system PP information store
 *)
val pp_info : unit -> Basic.PP.info

(**
   [pp_reset()]
   Reset the system PP information store
 *)
val pp_reset: unit -> unit

(** 
   [pp_set info]
   Set the system PP information store to [info]
 *)
val pp_set : Basic.PP.info -> unit

(** get/set/remove PP information for identifiers*)

(** PP information for terms *)

(*
   [get_term_pp id]
   get parsing and pretty printing information for identifer occuring 
   in a term.
   @param id identifier to look up.

   @return [(prec, fixity, repr)]
   where 
   [prec] is precedence
   [fixity] is fixity
   [repr] is representation to use (if any)

   @return [(default_term_prec, default_term_fixity, None)] if id is not found.
 *)
val get_term_pp : Basic.ident -> (int * Basic.PP.fixity * string option)   

(**
   [add_term_pp id prec fixity repr]
   add parsing and pretty printing information for identifer 
   occuring in a term.
   @param id identifier to add.
   @param prec precedence.
   @param fixity fixity.
   @param repr representation (if any).
 *)
val add_term_pp : 
    Basic.ident -> int -> Basic.PP.fixity 
      -> string option -> unit

(**
   [add_term_pp_record id rcrd]
   add parsing and pretty printing record for identifer occuring in a term.
   @param id identifier to add.
   @param rcrd PP record
 *)
val add_term_pp_record : 
    Basic.ident -> Basic.PP.record -> unit

(**
   [remove_term_pp id]
   remove parsing and pretty printing information for identifer 
   occuring in a term.
   @param id identifier to remove.
   @param prec precedence.
   @param fixity fixity.
   @param repr representation (if any).
 *)
val remove_term_pp : Basic.ident -> unit

(**
   [get_type_pp id]
   get parsing and pretty printing information for identifer occuring 
   in a type.
   @param id identifier to look up.

   @return [(prec, fixity, repr)]
   where 
   [prec] is precedence
   [fixity] is fixity
   [repr] is representation to use (if any)
 *)
val get_type_pp : Basic.ident -> (int * Basic.PP.fixity * string option)

(**
   [add_type_pp id prec fixity repr]
   add parsing and pretty printing information for identifer 
   occuring in a type.
   @param id identifier to add.
   @param prec precedence.
   @param fixity fixity.
   @param repr representation (if any).
 *)

val add_type_pp : 
    Basic.ident -> int -> Basic.PP.fixity -> string option -> unit

(**
   [add_type_pp_record id rcrd]
   add parsing and pretty printing record for identifer occuring in a type.
   @param id identifier to add.
   @param rcrd PP record
 *)
val add_type_pp_record : 
    Basic.ident -> Basic.PP.record -> unit

(**
   [remove_type_pp id]
   remove parsing and pretty printing information for identifer 
   occuring in a type.
   @param id identifier to remove.
   @param prec precedence.
   @param fixity fixity.
   @param repr representation (if any).
 *)
val remove_type_pp : Basic.ident -> unit

(* initialising functions *)

val init_theoryDB : unit -> unit

(* list of initialising functions *)

val init_list: (unit -> unit) list ref
val add_init: (unit-> unit) -> unit

(* function to call to initialise system *)
val init : unit -> unit
