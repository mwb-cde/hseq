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
    val stdtypenv : unit -> Gtypes.scope
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
    val typenv : unit -> Gtypes.scope

(* build PP record *)
val build_type_info : unit -> unit
val build_id_info : unit -> unit

val prec_of:  Basic.id_selector -> Basic.fnident ->  int
val is_infix: Basic.id_selector -> Basic.fnident ->  bool

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
val read_identifier: string -> Basic.fnident

(* PP information *)
type pp_info 
val empty_pp_info : unit -> pp_info
val base_pp_state : unit -> Corepp.pp_state
val get_pp : Basic.fnident -> pp_info ->  Corepp.pp_rec 
val add_pp :  Basic.fnident -> Corepp.pp_rec -> pp_info -> unit
val remove_pp : Basic.fnident -> pp_info -> unit

val type_pp_info : pp_info
val id_pp_info : pp_info

val get_id_info : Basic.fnident ->  Corepp.pp_rec
val add_id_info : Basic.fnident -> Corepp.pp_rec -> unit
val remove_id_info : Basic.fnident  ->  unit
val get_type_info : Basic.fnident ->  Corepp.pp_rec
val add_type_info : Basic.fnident -> Corepp.pp_rec -> unit
val remove_type_info : Basic.fnident  -> unit

val build_type_info : unit -> unit
val build_id_info : unit -> unit

(* initialising functions *)

val init_theoryDB : unit -> unit

(* list of initialising functions *)

val init_list: (unit -> unit) list ref
val add_init: (unit-> unit) -> unit

(* function to call to initialise system *)
val init : unit -> unit
