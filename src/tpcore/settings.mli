(*-----
 Name: settings.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** Installation specific and modifiable settings. *)


(** {5 File and directory settings} *)

(**
 Directories are set relative to the base directory.
*)

val base_dir_var: string
(**
   The environment variable to test at start-up for the systems base
   directory. Initially "HSEQ".
*)

val get_base_dir: unit -> string
(** Get the base directory. *)

val set_base_dir: string -> unit
(** Set the base directory. *)

val make_filename: string -> string
(** [make_filename f]: make file name f relative to the base directory. *)

val make_directory: string -> string
(** [make_directory f]: make directory path f relative to the base directory.*)


val init_file: string
(**
   Name of the file to execute to initialise the system
   the file name used will be contructed as [base_dir^init_file].
*)

val include_dir : unit -> string
(** Directory to search for headers. *)

val libs_dir : unit -> string
(**  Directory to search for library files. *)

val thys_dir : unit -> string
(**  Directory to search for theory files. *)

(** 
   {7 File suffixes} 

   Suffixes to add to a name to form a file name
*)

val thy_suffix: string
(** [thy_suffix="tho"]: Suffix of stored theories. *)

val script_suffix: string
(**
   [script_suffix = "Script.ml"]: 
   String to append to theory name to get 
   theory building script.
*)

(** {5 Settings} *)

val nice_sequent_prefix : string ref
(** 
   If [!nice_sequent] is [true], print sequents with the
   assumption indices prefixed by [nice_sequent_prefix].
*)

val nice_sequent: bool ref
(**
   if [true], print sequents with assumption indices prefixed by
   [!nice_sequent_prefix] otherwise print assumption indices
   as negative numbers.
*)

val long_identifier: bool ref
(**
   [long_identifier]: whether to print a long or short identifier
*)

val print_type_level: int ref
(**
   How many types to print when printing terms. Higher values of
   [print_type_level] correspond to more types. Initially [1].
*)
