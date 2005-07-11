(*-----
 Name: settings.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(***
* File and directory settings
***)

let base_dir_var = "HSEQ"

let base_dir = ref Defaults.basedir
let get_base_dir () = !base_dir
let set_base_dir d = base_dir := d

let make_filename f = (get_base_dir())^"/"^f
let make_directory f = (get_base_dir())^"/"^f

let init_file = "fm.ml"

let include_dir () = make_directory "include"
let libs_dir () = make_directory "libs"
let thys_dir () = make_directory "thys"

(*** File suffixes ***)

let thy_suffix="tho"
let script_suffix="Script.ml"

(***
* Sequent display 
***)

(** 
   [set_nice_sequent x]
   if [x] is [true], print sequents with assumption indices prefixed by 
   [!nice_sequent_prefix].
   if [x] is false, print sequent assumption indices as negative numbers.
*)
let nice_sequent_prefix=ref "~"
let nice_sequent = ref true

(* Gtypes display *)

let long_identifier=ref false
let print_type_level=ref 1


