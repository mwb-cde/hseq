(*-----
 Name: settings.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* Directory and files  *)

let base_dir = ref "/home/mw/src/tp/src"
let get_base_dir () = !base_dir
let set_base_dir d = base_dir := d

let make_filename f = (get_base_dir())^"/"^f
let make_directory f = (get_base_dir())^"/"^f

let init_file = "fm.ml"

let base_dir_var = "HSEQ"
let include_dir () = make_directory "include"
let libs_dir () = make_directory "libs"
let thys_dir () = make_directory "thys"


(* suffixes *)

let thy_suffix="tho"
let script_suffix="Script.ml"


(* Sequent display *)

(** 
   [set_nice_sequent x]
   if [x] is [true], print sequents with assumption indices prefixed by 
   [!nice_sequent_prefix].
   if [x] is false, print sequent assumption indices as negative numbers.
*)
let nice_sequent_prefix=ref "~"
let nice_sequent = ref true
let set_nice_sequent x = nice_sequent:=x
let get_nice_sequent () = !nice_sequent


