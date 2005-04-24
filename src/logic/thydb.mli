(*-----
 Name: thydb.mli
 Author: M Wahab <Mwahab@Users.Sourceforge.Net>
 Copyright M Wahab 2005
----*)

(* database of theories *)
(* has a hashtable of theories indexed by name,
   a current theory and that theories dependency list *)

    exception Importing

    type thydb 
    val emptydb : Theory.thy -> thydb

(* get current theory/database of theories *)
    val getcur : thydb -> Theory.thy
    val getdb : thydb -> (string, Theory.thy) Hashtbl.t

(* tests on imported and in memory *)
    val imported : string -> thydb -> bool
    val is_loaded : string -> thydb -> bool

(* add/remove/extract theories and their properties *)
    val add_thy : thydb -> Theory.thy -> Theory.thy
    val remove_thy : thydb -> string -> unit
    val get_thy : thydb -> string -> Theory.thy
    val get_parents : thydb -> string -> string list
    val filter : ('a -> bool) -> 'a list -> 'a list

    val add_importing : string list -> thydb -> unit

(* set the current theory *)
    val setcur : thydb -> string -> thydb
    val setcur_thy : thydb -> Theory.thy -> thydb

(*
   [load_theory db n prot f loadfn buildfn]

 load a theory from disc into the database.
   apply function [f] to the theory once it is loaded

   apply function [loadfn] to construct the filename of the theory 

   apply function [buildfn] to build a theory if it can't be loaded.

   [prot]: true => theory and its parents should be protected
         false => it doesn't matter

   return the list of importings for the new theory.
*)
val load_parents : 
    (thydb * (Theory.contents -> unit) * (string -> string) 
       * (string -> unit) * string) 
    -> float  -> (string list) -> string list -> string list

val load_theory : thydb -> string -> bool -> (Theory.contents -> unit) 
      -> (string -> string) -> (string -> unit) -> string list

(* build the importing list of the current theory *)
    val mk_importing : thydb -> string list
    val set_importing : thydb -> thydb

(* add/extract components to the current theory *)
    val add_axiom : 
	string -> Logic.thm -> Theory.property list -> thydb -> unit
    val add_thm : 
	string -> Logic.thm -> Theory.property list -> thydb -> unit

    val add_type_rec: Logic.cdefn -> thydb->unit

    val add_defn : 
	string -> Basic.gtype -> Logic.thm -> Theory.property list 
	  -> thydb -> unit
    val add_decln_rec :Logic.cdefn -> int -> Theory.property list
      -> thydb -> unit
    val add_decln :Logic.cdefn
      -> Theory.property list -> thydb -> unit
    val add_defn_rec : string-> Basic.gtype -> Logic.thm option 
       -> bool -> int -> Theory.property list -> thydb -> unit

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

val get_term_pp_rec : string -> string -> thydb -> Printer.record
val get_type_pp_rec : string -> string -> thydb -> Printer.record
    val id_exists : string -> string -> thydb -> bool

type memos
val empty_memo : unit -> memos

(* test that second theory is in scope of first *)
val thy_in_scope: string -> string -> thydb -> bool

(* find the theory of a given identifier *)
val thy_of: string -> string -> thydb -> string 

(* empty a database *)
val expunge: thydb -> Theory.thy-> unit

(* debugging info *)

val load_thy: 
    bool -> float -> (('a -> string) * (Theory.contents -> unit))
	-> 'a -> thydb -> Theory.thy

val build_thy: 
    float -> ( string -> unit)
	-> string -> thydb -> Theory.thy
