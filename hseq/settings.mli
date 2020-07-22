(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** Installation specific and modifiable settings. *)

(** {5 File and directory settings} *)

(** Directories are set relative to the base directory.
*)

val base_dir_var: string
(** The environment variable to test at start-up for the systems base
    directory. Default: "HSEQ".
*)

val get_base_dir: unit -> string
(** Get the base directory. *)

val set_base_dir: string -> unit
(** Set the base directory. *)

val make_filename: (string)option -> string -> string
(** [make_filename dir f]: make file name [f] relative to the
    directory [dir]. If [dir] is not giiven, use the base directory.
*)

val make_directory: string -> string
(** [make_directory f]: make directory path [f] relative to the base
    directory.
*)

val libs_dir : unit -> string
val set_libs_dir : string -> unit
(**  Directory of libraries (loadable modules). *)

val thys_dir : unit -> string
val set_thys_dir : string -> unit
(**  Theories directory. *)

val include_dirs : unit -> string list
val set_include_dirs : string list -> unit
(** Directories to search for modules to include. Default: [ [libs_dir] ]. *)

val init_file: string
(** Name of the file to execute to initialise the system the file name
    used will be contructed as [libs_dir^init_file].
*)

(**
    {7 File suffixes}

    Suffixes to add to a name to form a file name
*)

val thy_suffix: string
(** [thy_suffix = ".tho"]: Suffix of stored theories. *)

val script_suffix: string
(** [script_suffix = "Script.ml"]: String to append to theory name to
    get theory building script.
*)

(** {5 Settings} *)

val nice_sequent_prefix : string ref
(** If [!nice_sequent] is [true], print sequents with the assumption
    indices prefixed by [nice_sequent_prefix].
*)

val nice_sequent: bool ref
(** If [true], print sequents with assumption indices prefixed by
    [!nice_sequent_prefix] otherwise print assumption indices as
    negative numbers.
*)

val long_identifier: bool ref
(** [long_identifier]: whether to print a long or short identifiers.
*)

val print_type_level: int ref
(** How many types to print when printing terms. Higher values of
    [print_type_level] correspond to more types. Initially [1].
*)

val load_thy_level: int ref
(** Amount of information to emit when loading a theory. Initially
    [0].
*)
