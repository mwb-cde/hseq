(* database of theories *)
(* has a hashtable of theories indexed by name,
   a current theory and that theories dependency list *)

    exception Importing

    type thydb 
(*=
      { db: (string, Theory.thy) Hashtbl.t;
        mutable curr: Theory.thy;
        mutable importing: string list 
      }
*)
    val emptydb : Theory.thy -> thydb

(* get current theory/database of theories *)
    val getcur : thydb -> Theory.thy
    val getdb : thydb -> (string, Theory.thy) Hashtbl.t

(* tests on imported and in memory *)
    val imported : string -> thydb -> bool
    val is_loaded : string -> thydb -> bool

(* add/remove/extract theories and their properties *)
    val addthy : thydb -> Theory.thy -> Theory.thy
    val remove_thy : thydb -> string -> unit
    val getthy : thydb -> string -> Theory.thy
    val get_parents : thydb -> string -> string list
    val filter : ('a -> bool) -> 'a list -> 'a list

    val add_importing : string list -> thydb -> unit

(* set the current theory *)
    val setcur : thydb -> string -> thydb
    val setcur_thy : thydb -> Theory.thy -> thydb

(* load a theory from disc into the database.
   applies a given function the theory once it is loaded
   applies a second function to construct the filename of the theory 
   bool: true => theory and its parents should be protected
         false => it doesn't matter
*)

val load_theory : thydb -> string -> bool -> (Theory.thy -> unit) 
      -> (string -> string) -> string list

(* build the importing list of the current theory *)
    val mk_importing : thydb -> string list
    val set_importing : thydb -> thydb

(* add/extract components to the current theory *)
    val add_axiom : string -> Logic.thm -> thydb -> unit
    val add_thm : string -> Logic.thm -> thydb -> unit
(*
    val add_type_rec: string -> Gtypes.typedef_record -> thydb->unit
*)
    val add_type_rec: Logic.cdefn -> thydb->unit

    val add_defn : string -> Basic.gtype -> Logic.thm -> thydb -> unit
    val add_decln_rec :Defn.decln -> int -> thydb -> unit
    val add_decln :Defn.decln -> thydb -> unit
    val add_defn_rec : string-> Basic.gtype -> Logic.thm option 
       -> bool -> int -> thydb -> unit

(* get the type record of a type in a named theory *)
    val get_type_rec: string -> string -> thydb -> Gtypes.typedef_record

(* add/extract PP records *)

val add_pp_rec: 
    Basic.id_selector  -> string -> Printer.record
      -> thydb  -> unit
val get_pp_rec: 
    Basic.id_selector -> string  -> string  -> thydb
      -> Printer.record
val remove_pp_rec : 
    Basic.id_selector -> string -> string 
      -> thydb -> unit

val get_pplist: Basic.id_selector -> string -> thydb 
  -> (Basic.ident * Printer.record) list
  
(* find a theory and apply a function *)
    val find : (Theory.thy -> 'a) -> thydb -> 'a
    val quick_find : (Theory.thy -> 'a) -> string -> thydb -> 'a
    val find_apply : (Theory.thy -> 'a) -> thydb -> 'a

(* extract components of named theories *)
    val get_axiom : string -> string -> thydb -> Logic.thm
    val get_theorem : string -> string -> thydb -> Logic.thm
    val get_defn_rec : string -> string -> thydb -> Theory.id_record
    val get_lemma : string -> string -> thydb -> Logic.thm
    val get_defn : string -> string -> thydb -> Logic.thm
    val get_id_type : string -> string -> thydb -> Basic.gtype

    val id_is_infix : string -> string -> thydb -> bool
    val get_id_prec : string -> string -> thydb -> int
    val id_exists : string -> string -> thydb -> bool

type memos
val empty_memo : unit -> memos

(* test that second theory is in scope of first *)
val thy_in_scope: string -> string -> thydb -> bool
(* find the theory of a given identifier *)
val thy_of: string -> string -> thydb -> string 

(* empty a database *)
val expunge: thydb -> Theory.thy-> unit
