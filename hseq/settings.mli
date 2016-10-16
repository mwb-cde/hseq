(*----
  Name: settings.mli
  Copyright Matthew Wahab 2005-2016
  Author: Matthew Wahab <mwb.cde@gmail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under
  the terms of the Lesser GNU General Public License as published by
  the Free Software Foundation; either version 3, or (at your option)
  any later version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
  License for more details.

  You should have received a copy of the Lesser GNU General Public
  License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
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

val make_filename: ?dir:string -> string -> string
(** [make_filename ?dir f]: make file name [f] relative to the
    directory [dir]. If [?dir] is not given, use the base directory.
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
