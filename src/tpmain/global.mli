(*-----
   Name: global.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(** The global environment *)

(** {5 Theories} *)
module Thys:
    sig

      val empty_thy_name : string
(** The name of the anonymous theory *)

      val anon_thy : unit -> Theory.thy
(** Make an anonymous theory *)

      val base_name : (string)option ref
(** The name of the base theory [[default: base]] *)

      val get_base_name: unit -> string
(** 
   Get the name of the base theory. Raise Not_Found if no base theory
   set.
*)

      val set_base_name: string -> unit
(**  Set the name of the base theory. *)
      val clear_base_name: unit -> unit
(** clear the base name. *)

(** {5 The theory database} *)

      val theoryDB : Thydb.thydb ref
(** The theory database *)

      val get_theories : unit -> Thydb.thydb
	  (** Get the theory database *)
      val set_theories : Thydb.thydb -> unit
	  (** Set the theory database *)

      val current : unit -> Theory.thy
	  (** Get the current theory, raise [Not_found] if no theory. *)
      val current_name : unit -> string
	  (** 
	     The name of the current theory, raises [Not_found] if no
	     theory.
	   *)
      val set_current : Theory.thy -> unit
	  (** Set the current theory. *)
    end

(** {7 Toplevel theory functions} *)

val theories: unit -> Thydb.thydb
(** Short cut to Thys.get_theories. *)
val current: unit -> Theory.thy
(** Short cut to Thys.current. *)
val current_name: unit -> string
(** Short cut to Thys.current_name. *)

val scope : unit -> Scope.t
(** The global scope. Constructed from the theory database. *)

(** {5 Printing and Parsing} 

   Global printer tables and functions to add, query and remove
   combined printer-parser information.
*)
module PP:
    sig

      val info : unit -> Printer.ppinfo
(** Get the global printer information table *)
      val set : Printer.ppinfo -> unit
(** Set the global PP information table. *)

      val init: unit -> unit
(** Initialise the printer and parser tables. *)

(** {7 Terms} *)

      val get_term_pp : 
	  Basic.ident -> (int * Printer.fixity * string option)
(**
   Get PP information for term identifer.
   Returns [(default_term_prec, default_term_fixity, None)] if not found.
*)

      val add_term_pp : 
	  Basic.ident -> int -> Printer.fixity 
	    -> string option -> unit
(** Add printer information for term identifer. *)

      val add_term_pp_record : 
	  Basic.ident -> Printer.record -> unit
(** Add printer record for term identifer. *)

      val remove_term_pp : Basic.ident -> unit
(** Remove PP information for term identifer occuring in a term. *)

(** {7 Types} *)

      val get_type_pp : Basic.ident -> (int * Printer.fixity * string option)
(** Get PP information for type identifer. *)

      val add_type_pp : 
	  Basic.ident 
	-> int -> Printer.fixity -> string option 
	  -> unit
(** Add PP information for type identifer. *)

      val add_type_pp_record : 
	  Basic.ident -> Printer.record -> unit
(** Add PP record for type identifer. *)

      val remove_type_pp : Basic.ident -> unit
(** Remove PP record for type identifer. *)

(** {6 User-defined printers} *)

      val get_term_printer:
	  Basic.ident -> 
	    (Printer.fixity * int 
	     -> (Basic.term * (Basic.term list)) Printer.printer)
(** Get printer for terms *)

      val add_term_printer : 
	  Basic.ident -> 
	    (Printer.ppinfo 
	     -> (Printer.fixity * int) 
	       -> (Basic.term * (Basic.term list)) Printer.printer) -> unit
(** 
   [add_term_printer id p]: Add printer p for terms. The printer is
   keyed by term identifier and triggered on a function application
   (id args) (where args may be an empty list). Printer p is invoked
   as (p info (fix, prec) (f, args)) where info is the PP information,
   fix the fixity and prec the precedence active when the printer is
   called and f is the identifier term.
*)

      val remove_term_printer : Basic.ident -> unit
(** Remove printer for terms. *)

      val get_type_printer:
	  Basic.ident 
	-> (Printer.fixity * int 
	    -> (Basic.ident * (Basic.gtype list)) Printer.printer)
(** Get printer for types *)

      val add_type_printer : 
	  Basic.ident -> 
	    (Printer.ppinfo 
	     -> (Printer.fixity * int) 
	       -> (Basic.ident * (Basic.gtype list)) Printer.printer) -> unit
(** 
   [add_type_printer id p]: Add printer p for types. The printer is
   keyed by type identifier and triggered on a constructor expression
   (args)id (where args may be an empty list). Printer p is invoked as
   (p info (fix, prec) (id, args)) where info is the PP information,
   fix the fixity and prec the precedence active when the printer is
   called.
*)
      val remove_type_printer : Basic.ident -> unit
(** Remove printer for types *)

(** {6 Parsing} *)

      val expand_term : 
	  Scope.t -> Basic.term -> Basic.term
(** 
   Resolve symbols and short names in terms and types, replacing them
   with long identifiers where possible. Also retype the term if
   possible. Intended to make a parsed term suitable for passing to
   {!Formula.make}. Never fails but resulting term may be
   inconsistently typed.
*)
      val expand_type_names: 
	  Scope.t -> Basic.gtype -> Basic.gtype
(**
   Replace symbols and short names in a type with the long identifier,
   were possible.
*)
      val expand_typedef_names: 
	  Scope.t -> Parser.typedef_data -> Parser.typedef_data
(**
   Resolve symbols and short names in a type definition. 
*)

      val mk_term : Scope.t -> Basic.term -> Basic.term
(** 
   Resolve symbols and short names in a term, making a parsed term
   suitable for use.
   
   Replaces short names and symbols with their long identifiers where
   possible. Also retypes the term if possible. Never fails but
   resulting term may be inconsistently typed.
*)

      val read : string -> Basic.term
(** Parse a string as a term, resolving short names and symbols. *)

      val read_unchecked : string -> Basic.term
(** 
   Parse a string as a term, return the term as is, without expanding terms
   and resolving symbols. 
*)
      val read_defn :
	  string -> (string * (string * Basic.gtype) list) * Basic.term
(** Parse a string as a term definition. *)

      val read_type : string -> Basic.gtype
(** 
   Parse a string a type, resolving short names and symbols where possible. 
*)
      val read_type_defn : string -> Parser.typedef_data
(** Parse a string as a type definition. *)

      val read_identifier: string -> Basic.ident
(** Parse a string as an identifier. *)
    end

(** {7 Toplevel parsing functions} *)

val read : string -> Basic.term
(** Read a term. *)
val read_type : string -> Basic.gtype
(** Read a type. *)
val read_identifier: string -> Basic.ident
(** Read an identifier. *)

val read_defn :
    string -> (string * (string * Basic.gtype) list) * Basic.term
(** Read a term definition. *)
val read_type_defn : string -> Parser.typedef_data
(** Read a type definition. *)

val mk_term : Basic.term -> Basic.term
(** 
   Resolve the names and symbols in a parsed term, making it suitable
   for passing to formula constructors.
 *)

(** {5 File-Handling} *)

(** Filenames and paths for theory files *)
module Files : 
    sig

      val get_cdir : unit -> string
(** The current working directory. *)

(** {7 Paths} *)

      val get_path : string list ref -> string list
(** Get a list of directories from a path. *)
      val set_path : string list -> string list ref -> unit
(** Set a path. *)
      val add_path : string -> string list ref -> unit
(** Add a directory to a path. *)
      val remove_path : string -> string list ref -> unit
(** Remove a directory from a path.*)

      val get_thy_path : unit -> string list
(** The path for theory files. *)

      val add_thy_path : string -> unit
(** Add a directory to the theory path. *)

      val set_thy_path: string list -> unit
(** Set the theory path. *)

      val remove_from_path: string -> unit
(** Remove a directory from the theory path. *)

      val find_file : string -> string list -> string
(** 
   [find_file x p]: Find file [x] in the path [p].
   Returns the full path to the file, raises [Not_found] if not found.
 *)

(** {7 Theory files} *)

      val file_of_thy: string -> string
(** [file_of_thy th]: Make the name of the file of theory [th]. *)

      val script_of_thy: string -> string
(** [file_of_thy th]: Make the name of the script to build theory [th]. *)

      val find_thy_file: string -> string
(** Find the a theory file. *)

(** {7 Theory loading and building} *)

      val build_thy_file: Thydb.thydb -> string -> Thydb.thydb
(** Function to build a theory from a script. *)

      val load_thy_file: Thydb.Loader.info -> Theory.saved_thy
(** Function to load a theory from a file. *)

      val load_use_theory_files: Theory.contents -> unit
(** 
   Load or use the files named by a theory. This is only called when
   a theory is loaded from a file, not went it is built from a script.
*)

(** {7 Theory load functions}

   Functions invoked when a theory is loaded from disk. When a theory
   is loaded from disk, these functions are invoked on the theory
   contents, e.g. to set-up the print and parse information and pass
   informatin to proof tools.
*)

      val load_functions : (Theory.contents -> unit) list ref
(** The list of functions to invoke on a theory. *)
      val add_load_fn : (Theory.contents -> unit) -> unit
(** Add an inspection function. *)

      val init_load_functions: unit -> unit
(** Initialise the inspection functions. *)

      val on_load_thy: Theory.contents -> unit
(** 
   The top-level inspection functions, simply iterates through the
   list of functions !load_functions in the reverse order that they
   were added.
*)

(** {7 Miscellaneous} *)
      val loader_data : Thydb.Loader.data
(** Default information needed for the theory database loader. *)

    end

(** {7 Toplevel file functions} *)
val get_thy_path: unit -> string list
(** Get the list of directories to search for theory files *)

val add_thy_path: string -> unit
(** Add to the list of directories to search for theory files *)

(** {5 Initialising functions} *)
module Init:
    sig

      val set_base_thy_builder : (unit -> unit) -> unit
(** Set the function to use if the base theory must be rebuilt. *)
      val get_base_thy_builder : unit -> (unit -> unit) option
(** Get the function to use if the base theory must be rebuilt. *)
      val load_base_thy: unit -> unit
(**
   Try to load the base theory and make it the current theory.
   If unsuccessful use an empty theory as the current theory.
   then if [get_base_thy_builder()=Some(f)], call [f],
   otherwise clear the base theory name ([clear_base_name()])
 *)

      val init_theoryDB : unit -> unit
(** Initialise the theory database and try to load the base theory. *)

      val init_list: (unit -> unit) list ref
(** The list of functions to call to initialise the system. *)
      val add_init: (unit-> unit) -> unit
(** 
   Add an initialising function. Functions are invoked in the reverse
   order that they were added.
*)

      val init : unit -> unit
(** Initialise the system. *)


      val reset_list: (unit -> unit) list ref
(** The functions to call when the system is to be reset. *)
      val add_reset: (unit-> unit) -> unit
(** Add a reset function. *)

      val reset : unit -> unit
(**
   Reset the system. Calls all functions in !reset_list then calls
   init().
*)
    end

(** {7 Toplevel initialising functions} *)

val init : unit -> unit
(** Initialise the system *)
val reset : unit -> unit
(** Reset the system. (This is the same as init()). *)
