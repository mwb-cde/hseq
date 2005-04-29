(*-----
 Name: settings.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* 
   Settings:
   Put installation dependent settings in here 

*)

(**
   [base_dir_var]: the environment variable to test for the
   systems base_dir. 
   Initially "HSEQ"
*)
val base_dir_var: string

(* File and directory settings *)
(* 
   base_dir:
   base directory of installation.
   all other directories are set relative to this
*)

val get_base_dir: unit -> string
val set_base_dir: string -> unit


(* 
   [make_filename f]: make file name f relative to the base directory
   [make_directory f]: make directory path f relative to the base
   directory
*)
val make_filename: string -> string
val make_directory: string -> string

(* 
   [init_file]: name of file to execute to initialise the system
   the file name used will be contructed as base_dir^init_file
*)
val init_file: string

(* 
   [include_dir ()]: Directory to search for headers.
   [lib_dir ()]: Directory to search for library files.
*)
val include_dir : unit -> string
val libs_dir : unit -> string
val thys_dir : unit -> string

(* 
   File suffixes:
   suffixes to add to a name to form a file name
*)

(* thy_suffix: suffix of stored theories *)
val thy_suffix: string

(* 
   script_suffix = "Script.ml": 
   string to append to theory name to get 
   theory building script.
*)
val script_suffix: string


(** 
   [set_nice_sequent x]
   if [x] is [true], print sequents with assumption indices prefixed by 
   [!nice_sequent_prefix].
   if [x] is false, print sequent assumption indices as negative numbers.
*)
val nice_sequent_prefix : string ref
val set_nice_sequent: bool -> unit
val get_nice_sequent: unit -> bool

(**
   [long_identifier]: whether to print a long or short identifier
*)
val long_identifier: bool ref
val print_type_level: int ref

