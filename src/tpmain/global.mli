(*----
  Name: global.mli
  Copyright M Wahab 2005-2010
  Author: M Wahab  <mwb.cde@googlemail.com>

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

(** The global environment *)

(** Hooks for interacting with the system *)
module Hooks:
sig
  (** Values in this module depend on the operating environment of the
      system. Settings for an interactive system (built with
      [ocamlmktop]) will be different to those for a system built as a
      library.

      All values should  be set as part of the initialisation
      sequence for the system and before {!Global.init} (which sets up
      the threom prover) is called.

      All functions defined here are allowed to fail arbitrarily. 
  *)

  val load_file: (string -> unit) ref
  (** [load_file f]: Load a byte-code file [f] into memory.
      
      Used, when loading theories, to load any associated byte-code
      libraries.

      For interactive systems, this can be set to [Topdirs.dir_load
      Format.std_formatter].

      For libraries, a possible value is [Dynlink.loadfile].
  *)

  val use_file: (?silent:bool -> string -> unit) ref
(** [use_file ?silent f]: Read file [f] as a script.  If
    [silent=true], do not report any information.

    Used, when loading theories, to run any associated script.
    
    Used if a missing theory is to be built from a script.

    For interactive systems, this can be set to 
    [Toploop.use_file].
*)

end

(** {5 Theories} *)
module Thys:
sig

  val empty_thy_name: string
  (** The name of the anonymous theory. *)

  val anon_thy: unit -> Theory.thy
  (** Make an anonymous theory. *)

  val base_name: (string)option ref
  (** The name of the base theory [[default: base]]. *)

  val get_base_name: unit -> string
  (** Get the name of the base theory. 

      @raise Not_Found if no base theory set.  
  *)

  val set_base_name: string -> unit
  (** Set the name of the base theory. *)
  val clear_base_name: unit -> unit
  (** Clear the base name. *)

  (** {5 The theory database} *)

  val theoryDB: Thydb.thydb ref
  (** The theory database. *)

  val get_theories: unit -> Thydb.thydb
  (** Get the theory database. *)
  val set_theories: Thydb.thydb -> unit
  (** Set the theory database. *)

  val current: unit -> Theory.thy
  (** Get the current theory. @raise [Not_found] if no current
      theory. *)
  val current_name: unit -> string
  (** The name of the current theory.
      @raise [Not_found] if no current theory.  *)

  val set_current: Theory.thy -> unit
(** Set the current theory. *)
end

(** {7 Toplevel theory functions} *)

val theories: unit -> Thydb.thydb
(** Short cut to {!Thys.get_theories.} *)
val current: unit -> Theory.thy
(** Short cut to {!Thys.current.} *)
val current_name: unit -> string
(** Short cut to {!Thys.current_name.} *)

val scope: unit -> Scope.t
(** The global scope. Constructed from the theory database. *)

(** {5 Printing and Parsing} 

    Global printer tables and functions to add, query and remove
    combined printer-parser information.
*)
module PP:
sig

  val info: unit -> Printer.ppinfo
  (** Get the global printer information table. *)
  val set: Printer.ppinfo -> unit
  (** Set the global PP information table. *)

  val init: unit -> unit
  (** Initialise the printer and parser tables. *)

  (** {7 Terms} *)

  val get_term_pp: 
    Ident.t -> (int * Printer.fixity * string option)
  (** Get PP information for term identifer.  Returns
      [(default_term_prec, default_term_fixity, None)] if not
      found.  *)

  val add_term_pp: 
    Ident.t -> int -> Printer.fixity 
    -> string option -> unit
  (** Add printer information for term identifer. *)

  val add_term_pp_record: 
    Ident.t -> Printer.record -> unit
  (** Add printer record for term identifer. *)

  val remove_term_pp: Ident.t -> unit
  (** Remove PP information for term identifer occuring in a term. *)

  (** {7 Types} *)

  val get_type_pp: Ident.t -> (int * Printer.fixity * string option)
  (** Get PP information for type identifer. *)

  val add_type_pp: 
    Ident.t 
    -> int -> Printer.fixity -> string option 
    -> unit
  (** Add PP information for type identifer. *)

  val add_type_pp_record: 
    Ident.t -> Printer.record -> unit
  (** Add PP record for type identifer. *)

  val remove_type_pp: Ident.t -> unit
  (** Remove PP record for type identifer. *)

  (** {6 User-defined printers} *)

  val get_term_printer:
    Ident.t -> 
    (Printer.fixity * int 
     -> (Basic.term * (Basic.term list)) Printer.printer)
  (** Get printer for terms. *)

  val add_term_printer: 
    Ident.t -> 
    (Printer.ppinfo 
     -> (Printer.fixity * int) 
     -> (Basic.term * (Basic.term list)) Printer.printer) -> unit
  (** [add_term_printer id p]: Add printer p for terms. The
      printer is keyed by term identifier and triggered on a
      function application (id args) (where args may be an empty
      list). Printer p is invoked as (p info (fix, prec) (f,
      args)) where info is the PP information, fix the fixity and
      prec the precedence active when the printer is called and f
      is the identifier term.  *)

  val remove_term_printer: Ident.t -> unit
  (** Remove printer for terms. *)

  val get_type_printer:
    Ident.t 
    -> (Printer.fixity * int 
	-> (Ident.t * (Basic.gtype list)) Printer.printer)
  (** Get printer for types *)

  val add_type_printer: 
    Ident.t -> 
    (Printer.ppinfo 
     -> (Printer.fixity * int) 
     -> (Ident.t * (Basic.gtype list)) Printer.printer) -> unit
  (** [add_type_printer id p]: Add printer p for types. The
      printer is keyed by type identifier and triggered on a
      constructor expression (args)id (where args may be an empty
      list). Printer p is invoked as (p info (fix, prec) (id,
      args)) where info is the PP information, fix the fixity and
      prec the precedence active when the printer is called.  *)

  val remove_type_printer: Ident.t -> unit
  (** Remove printer for types *)

  (** {6 Parsing} *)

  val overload_lookup: string -> (Ident.t * Basic.gtype) list
  (** [overload_lookup s]: Find the list of identifiers which may
      be overloaded on [s]. String [s] may be a symbol or the
      short name of a term.  *)

  val expand_term: 
    Scope.t -> Pterm.t -> Basic.term
  (** Resolve symbols and short names in terms and types, replacing
      them with long identifiers where possible. Also retype the term
      if possible. Intended to make a parsed term suitable for passing
      to {!Formula.make}. Never fails but resulting term may be
      inconsistently typed.  *)

  val expand_type_names: 
    Scope.t -> Basic.gtype -> Basic.gtype
  (** Replace symbols and short names in a type with the long
      identifier, were possible.  *)
  val expand_typedef_names: 
    Scope.t -> Parser.typedef_data -> Defn.Parser.typedef
  (** Resolve symbols and short names in a type definition.  *)

  val mk_term: Scope.t -> Pterm.t -> Basic.term
  (** Resolve symbols and short names in a term, making a parsed term
      suitable for use.
      
      Replaces short names and symbols with their long identifiers where
      possible. Also retypes the term if possible. Never fails but
      resulting term may be inconsistently typed.
  *)

  val read: string -> Basic.term
  (** Parse a string as a term, resolving short names and
      symbols. *)

  val read_unchecked: string -> Basic.term
  (** Parse a string as a term, return the term as is, without
      expanding terms and resolving symbols.  *)

  val read_defn:
    string -> ((string * Basic.gtype) * Basic.term list) * Basic.term
  (** Parse a string as a term definition. *)

  val read_type: string -> Basic.gtype
  (** Parse a string a type, resolving short names and symbols where
      possible.  *)
  val read_type_defn: string -> Defn.Parser.typedef
  (** Parse a string as a type definition. *)

  val read_identifier: string -> Ident.t
(** Parse a string as an identifier. *)
end

(** {7 Toplevel parsing functions} *)

val read: string -> Basic.term
(** Read a term. *)
val read_type: string -> Basic.gtype
(** Read a type. *)
val read_identifier: string -> Ident.t
(** Read an identifier. *)

val read_defn:
  string -> ((string * Basic.gtype) * Basic.term list) * Basic.term
(** Read a term definition. *)

val read_type_defn: string -> Defn.Parser.typedef
(** Read a type definition. *)

val mk_term: Pterm.t -> Basic.term
(** Resolve the names and symbols in a parsed term, making it suitable
    for passing to formula constructors.
*)

(** {5 File-Handling} *)

(** Filenames and paths for theory files *)
module Files: 
sig

  val get_cdir: unit -> string
  (** The current working directory. *)

  val object_suffix:  string list ref
  (** The suffixes of file types which should be treated as byte-code
      files (=[".cmo"; ".cmi"]).  *)

  val load_use_file: ?silent:bool -> string -> unit
  (** [load_use_file ?silent name]: If name is an object file (with a
      suffix in [object_suffix]) then call {!Global.Hooks.load_file}[
      name] otherwise call {!Global.Hooks.use_file} [?silent name].  *)

  (** {7 Paths} *)

  val get_path: string list ref -> string list
  (** Get a list of directories from a path. *)
  val set_path: string list -> string list ref -> unit
  (** Set a path. *)
  val add_path: string -> string list ref -> unit
  (** Add a directory to a path. *)
  val remove_path: string -> string list ref -> unit
  (** Remove a directory from a path.*)

  val get_thy_path: unit -> string list
  (** The path for theory files. *)

  val add_thy_path: string -> unit
  (** Add a directory to the theory path. *)

  val set_thy_path: string list -> unit
  (** Set the theory path. *)

  val remove_from_path: string -> unit
  (** Remove a directory from the theory path. *)

  val find_file: string -> string list -> string
  (** [find_file f p]: Find file [f] in the path [p].  Returns the
      full path to the file, raises [Not_found] if not found.  If [f]
      is an absolute file name, just returns [f].  *)

  (** {7 Theory files} *)

  val file_of_thy: string -> string
  (** [file_of_thy th]: Make the name of the file of theory [th]. *)

  val script_of_thy: string -> string
  (** [file_of_thy th]: Make the name of the script to build theory
      [th]. *)

  val find_thy_file: string -> string
  (** Find the a theory file. *)

  (** {7 Theory loading and building} *)

  val build_thy_file: Thydb.thydb -> string -> Thydb.thydb
  (** Function to build a theory from a script. *)

  val load_thy_file: Thydb.Loader.info -> Theory.saved_thy
  (** Function to load a theory from a file. *)

  val load_use_theory_files: Theory.contents -> unit
  (** Load or use the files named by a theory. This is only called
      when a theory is loaded from a file, not went it is built from a
      script.  Files are searched for in the theory path
      [get_thy_path()].  *)

  (** {7 Theory load functions}

      Functions invoked when a theory is loaded from disk. When a
      theory is loaded from disk, these functions are invoked on the
      theory contents, e.g. to set-up the print and parse information
      and pass informatin to proof tools.  *)

  val load_functions: (Theory.contents -> unit) list ref
  (** The list of functions to invoke on a theory. *)
  val add_load_fn: (Theory.contents -> unit) -> unit
  (** Add an inspection function. *)

  val init_load_functions: unit -> unit
  (** Initialise the inspection functions. *)

  val on_load_thy: Thydb.thydb -> Theory.contents -> unit
  (** The top-level inspection functions. Simply iterates through the
      list of functions !load_functions in the reverse order that they
      were added.  *)

  (** {7 Miscellaneous} *)
  val loader_data: Thydb.Loader.data
  (** Default information needed for the theory database loader. *)

  (** {7 Debugging} *)
  val forbidden:  Lib.StringSet.t ref
  val init_forbidden: unit -> unit 
  val add_forbidden: string -> unit 
  val drop_forbidden: string -> unit 
  val is_forbidden: string -> bool
    
end

(** {7 Toplevel file functions} *)

val get_thy_path: unit -> string list
(** Get the list of directories to search for theory files. *)

val add_thy_path: string -> unit
(** Add to the list of directories to search for theory files. *)

(** {5 Initialising functions} *)
module Init:
sig

  val set_base_thy_builder: (unit -> unit) -> unit
  (** Set the function to use if the base theory must be rebuilt. *)
  val get_base_thy_builder: unit -> (unit -> unit) option
  (** Get the function to use if the base theory must be
      rebuilt. *)
  val load_base_thy: unit -> unit
  (** Try to load the base theory and make it the current theory.  If
      unsuccessful use an empty theory as the current theory.  then if
      [get_base_thy_builder()=Some(f)], call [f], otherwise clear the
      base theory name ([clear_base_name()]). *)

  val init_theoryDB: unit -> unit
  (** Initialise the theory database and try to load the base theory. *)

  val init_list: (unit -> unit) list ref
  (** The list of functions to call to initialise the system. *)
  val add_init: (unit-> unit) -> unit
  (** Add an initialising function. Functions are invoked in the
      reverse order that they were added.  *)

  val init: unit -> unit
  (** Initialise the system. *)


  val reset_list: (unit -> unit) list ref
  (** The functions to call when the system is to be reset. *)
  val add_reset: (unit-> unit) -> unit
  (** Add a reset function. *)

  val reset: unit -> unit
(** Reset the system. Calls all functions in !reset_list then calls
    [init()].
*)
end

(** {7 Toplevel initialising functions} *)

val init: unit -> unit
(** Initialise the theorem prover. This should be called before the
    theorem prover is first used.
*)
  
val reset: unit -> unit
(** Reset the system. (This is the same as [init()].) *)
