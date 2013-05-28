(*----
  Name: context.mli
  Copyright M Wahab 2012
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

(** Default values. *)
module Default:
sig

  (** {6 File handling} *)

  (** [load n]: Load file [n]. *)
  val load : string -> unit

  (** [use ?silent n]: Use script [n]. *)
  val use : ?silent:bool -> string -> unit

  (** {6 Theories} *)

  (** The empty theory string. *)
  val empty_thy_name: string

  (** The name of the base theory. *)
  val base_thy_name:string
end

(** The theorem prover context. *)
(****
     module Context:
     sig
***)
(** Types *)

(** File handling functions *)
type file_t =
  {
    (** [load f]: Load a byte-code file [f] into memory. *)
    load_f: string -> unit;

    (** [use ?silent f]: Read file [f] as a script.  If
        [silent=true], do not report any information. *)
    use_f: ?silent:bool -> string -> unit;

    (** [build ?silent th]: Build theory [th] from a script.
        [silent=true], do not report any information.  @raise
        Failure on failure. *)
    build_f: ?silent:bool -> string -> unit;

    (** [path]: List of directories to search for theories,
        libraries and scripts.*)
    path_f: string list;

    (** suffix: List of possible suffixes for an object file. *)
    obj_suffix_f: string list;

    (** thy_suffix: Suffix for a theory file. *)
    thy_suffix_f: string;

    (** script_suffix: Suffix for a script file. *)
    script_suffix_f: string;
  }

(* The default value for [file_t]. *)
val empty_file_t: unit -> file_t

(** Theory data *)
type thy_t =
  {
    (** Name of the theory on which all user theories are based *)
    base_name_f: string option;

    (** The theory data-base. *)
    thydb_f: Thydb.thydb;

    (** Information needed for the theory database loader. *)
    loader_data_f: Thydb.Loader.data
  }

(** [empty_thy_t()]: The default value for [thy_t]. *)
val empty_thy_t: unit -> thy_t

(** Pretty printer *)
type pp_t =
  {
    pp_info_f: Printer.ppinfo ref;
  }

  (** Parsers *)
type parser_t =
  {
    parser_info_f: Parser.Table.t ref;
  }

(** [empty_pp_t()]: The default value for [pp_t]. *)
val empty_pp_t: unit -> pp_t

(** Top-level context *)
type t = 
  {
    (** Hooks for file handling functions *)
    file_f: file_t;

    (** Theory data *)
    thys_f: thy_t;

    (** Pretty printer *)
    pp_f: pp_t;

    (** Parsers *)
    parser_f: parser_t;
    
    (** A list of functions to invoke on a theory when it is added
        to the data-base. *)
    load_functions_f: (t -> Theory.contents -> t) list;

    (** Theorems caches *)
    thm_cache_f: (Ident.t, Logic.thm) Hashtbl.t;

    (** Scope attached to this context. *)
    scope_f: Scope.t;
  }

(** [empty()]: The empty context. *)
val empty: unit -> t

(** {6 Scoped contexts} *)
(**
type scoped = (t * Scope.t)
*)
type scoped
(** The type of scoped contexts *)

val scoped: t -> Scope.t -> scoped
(** Constructor for scoped contexts *)
val scope_of: scoped -> Scope.t
(** Get the scope *)
val context_of: scoped -> t
(** Get the context *)

val set_scope: scoped -> Scope.t -> scoped
  (** Set the scope *)
val set_context: scoped -> t -> scoped
(** Set the context *)

(** {5 Accessor Functions} *)

(** {6 File handling} *)

val set_load : t -> (string -> unit) -> t
(** [set_load f t]: Set the file-loading function in context [t] to [f]. *)

val load : t -> (string -> unit)
(** [load t]: Get the file-loading function of context [t]. *)

val set_use : t -> (?silent:bool -> string -> unit) -> t
(** [set_use f t]: Set the script-loading function in context [t] to [f]. *)

val use : t -> (?silent:bool -> string -> unit)
(** [use t]: Get the script-loading function of context [t]. *)

val set_build : t ->  (?silent:bool -> string -> unit) -> t
(** [set_use f t]: Set the script-loading function in context [t] to [f]. *)

val build : t -> (?silent:bool -> string -> unit)
(** [use t]: Get the script-loading function of context [t]. *)

val set_path : t-> string list -> t
(** [set_path p t]: Set the path in context [t] to [p]. *)

val path : t -> string list
(** [path t]: Get the path of context [t]. *)

val set_obj_suffix : t -> string list -> t
(** [set_obj_suffix sl t]: Set the object suffix list in context [t] to [sl]. *)

val obj_suffix : t -> string list
(** [obj_suffix t]: Get the object suffix list of context [t]. *)

val set_thy_suffix : t -> string -> t
(** [set_obj_suffix sl t]: Set the theory suffix in context [t] to [sl]. *)

val thy_suffix : t -> string
(** [obj_suffix t]: Get the theory suffix  of context [t]. *)

val set_script_suffix : t-> string -> t
(** [set_obj_suffix sl t]: Set the script suffix in context [t] to [sl]. *)

val script_suffix : t -> string
(** [obj_suffix t]: Get the script suffix  of context [t]. *)

(** {6 Theory handling} *)

val set_base_name : t -> string -> t
(** [set_base_name n t]: Set the theory base name in context [t] to [n]. *)

val base_name : t -> string
(** [base_name t]: Get the theory base name in context [t]. *)

val has_base_name : t -> bool
(** [has_base_name t]: Test whether a theory base name is set in context [t]. *)

val clear_base_name : t -> t
(** [clear_base_name t]: Clear the theory base name in context [t]. *)

val set_thydb : t-> Thydb.thydb -> t
(** [set_thydb db t]: Set the theory database in context [t] to [db]. *)

val thydb : t -> Thydb.thydb
(** [thydb t]: Get the theory database of context [t]. *)

val set_loader_data : t-> Thydb.Loader.data -> t 
(** [set_thydb db t]: Set the theory database in context [t] to [db]. *)

val loader_data : t -> Thydb.Loader.data
(** [thydb t]: Get the theory database of context [t]. *)

val set_load_functions : t-> (t -> Theory.contents -> t) list -> t
(** [set_load_functions fl t]: Set the load functions in context
    [t] to [fl]. *)

val load_functions : t -> (t -> Theory.contents -> t) list
(** [load_functions t]: Get the load functions of context [t]. *)

(** {6 Pretty-printer handling} *)

val set_ppinfo : t -> Printer.ppinfo -> t
(** [set_ppinfo n t]: Update the PP data in context [t] to [n]. *)

val ppinfo : t -> Printer.ppinfo
(** [ppinfo t]: Get the PP data in context [t]. *)

val set_parsers : t-> Parser.Table.t -> t
(** [set_ppinfo n t]: Update the PP data in context [t] to [n]. *)

val parsers : t -> Parser.Table.t
(** [ppinfo t]: Get the PP data in context [t]. *)

val cache_thm: t -> Ident.t -> Logic.thm -> t
(** Cache a theorem *)

val lookup_thm: scoped -> Ident.t -> Logic.thm
(** Lookup a cached theorem, raising Not_found if not found. *)

val find_thm: scoped-> Ident.t -> (scoped -> Logic.thm) -> Logic.thm
(** Lookup a cached theorem, creating and caching it if not found. *)

(***
    end
***)

(** {5 Theories} *)
module Thys:
sig

  (** [empty_thy_name]: The name of the anonymous theory. *)
  val empty_thy_name: string

  (** Make an anonymous theory. *)
  val anon_thy: unit -> Theory.thy

  (** Get the name of the base theory. 

      @raise Not_Found if no base theory set.  
  *)
  val get_base_name: t -> string

  val set_base_name: t -> string -> t
  (** Set the name of the base theory. *)

  (** Clear the base name. *)
  val clear_base_name: t -> t

  (** {5 The theory database} *)

  (** Get the theory database. *)
  val theories: t -> Thydb.thydb
  val get_theories: t -> Thydb.thydb

  (** Set the theory database. *)
  val set_theories: t-> Thydb.thydb -> t

  (** Get the current theory. @raise [Not_found] if no current
      theory. *)
  val current: t -> Theory.thy
  val curr_theory: t -> Theory.thy

  (** The name of the current theory.
      @raise [Not_found] if no current theory.  *)
  val current_name: t -> string

  (** Set the current theory. *)
  val set_current: t -> Theory.thy -> t

end

(** {5 File-Handling} *)

module Files: 
sig

  (** The current working directory. *)
  val get_cdir: unit -> string

  (** [load_use_file ?silent name]: Load or use file [n]. *)
  val load_use_file: ?silent:bool -> t -> string -> unit

  (** {7 Paths} *)

  val set_path: t -> string list -> t
  (** Set the search path. *)
  val get_path: t -> string list
  (** Get the search path. *)
  val add_path: t -> string -> t
  (** Add a directory to a path. *)
  val remove_path: t -> string -> t
  (** Remove a directory from a path.*)

  val get_thy_path: t -> string list
  (** The path for theory files. *)

  val add_thy_path: t -> string -> t
  (** Add a directory to the theory path. *)
  val set_thy_path: t -> string list -> t
  (** Set the theory path. *)
  val remove_from_path: t -> string -> t
  (** Remove a directory from the theory path. *)

  val find_file: string -> string list -> string
  (** [find_file f p]: Find file [f] in the path [p].  Returns the
      full path to the file, raises [Not_found] if not found.  If [f]
      is an absolute file name, just returns [f].  *)
    
  (** {7 Theory files} *)

  val file_of_thy: t -> string -> string
  (** [file_of_thy th]: Make the name of the file of theory [th]. *)

  val script_of_thy: t -> string -> string
  (** [file_of_thy th]: Make the name of the script to build theory
      [th]. *)

  val find_thy_file: t -> string -> string
  (** Find the a theory file. *)

  (** {7 Theory loading and building} *)

  val build_thy_file: t -> string -> unit
  (** Function to build a theory from a script. *)

  val load_thy_file: t -> Thydb.Loader.info -> Theory.saved_thy
  (** Function to load a theory from a file. *)

  val load_use_theory_files: t -> Theory.contents -> unit
(** Load or use the files named by a theory. This is only called
    when a theory is loaded from a file, not went it is built from a
    script.  Files are searched for in the theory path
    [get_thy_path()].  *)

end


(** {5 Printing and Parsing}

    Global printer tables and functions to add, query and remove
    combined printer-parser information.
*)
module NewPP:
sig

  (** {7 Terms} *)

  val add_term_parser: 
    t -> (string) Lib.position -> string 
    -> (Grammars.parser_info -> Pterm.t Grammars.phrase) 
    -> t
  (** Add a term parser *)

  val remove_term_parser: t -> string  -> t
  (** Remove a term parser, rasing Not_found if no such parser *)

  val get_term_pp: t ->
    Ident.t -> (int * Printer.fixity * string option)
  (** Get PP information for term identifer.  Returns
      [(default_term_prec, default_term_fixity, None)] if not
      found.  *)

  val add_term_pp: t ->
    Ident.t -> int -> Printer.fixity 
    -> string option -> t
  (** Add printer information for term identifer. *)

  val add_term_pp_record: t ->
    Ident.t -> Printer.record -> t
  (** Add printer record for term identifer. *)

  val remove_term_pp: t -> Ident.t -> t
  (** Remove PP information for term identifer occuring in a term. *)

  (** {7 Types} *)

  val add_type_parser: 
    t -> (string) Lib.position -> string 
    -> (Grammars.parser_info -> Basic.gtype Grammars.phrase) 
    -> t
  (** Add a type parser *)

  val remove_type_parser: t -> string  -> t
  (** Remove a type parser, rasing Not_found if no such parser *)

  val get_type_pp:
    t -> Ident.t -> (int * Printer.fixity * string option)
  (** Get PP information for type identifer. *)

  val add_type_pp: 
    t -> Ident.t 
    -> int -> Printer.fixity -> string option 
    -> t
  (** Add PP information for type identifer. *)

  val add_type_pp_record: 
    t -> Ident.t -> Printer.record -> t
  (** Add PP record for type identifer. *)

  val remove_type_pp: t -> Ident.t -> t
  (** Remove PP record for type identifer. *)

  (** {6 User-defined printers} *)

  val get_term_printer:
    t -> Ident.t -> Printer.term_printer
  (** Get printer for terms. *)

  val add_term_printer: 
    t -> Ident.t -> Printer.term_printer -> t
  (** [add_term_printer id p]: Add printer p for terms. The
      printer is keyed by term identifier and triggered on a
      function application (id args) (where args may be an empty
      list). Printer p is invoked as (p info (fix, prec) (f,
      args)) where info is the PP information, fix the fixity and
      prec the precedence active when the printer is called and f
      is the identifier term.  *)

  val remove_term_printer: t -> Ident.t -> t
  (** Remove printer for terms. *)

  val get_type_printer:
    t -> Ident.t -> Printer.gtype_printer
  (** Get printer for types *)

  val add_type_printer: 
    t -> Ident.t -> Printer.gtype_printer
    -> t
  (** [add_type_printer id p]: Add printer p for types. The
      printer is keyed by type identifier and triggered on a
      constructor expression (args)id (where args may be an empty
      list). Printer p is invoked as (p info (fix, prec) (id,
      args)) where info is the PP information, fix the fixity and
      prec the precedence active when the printer is called.  *)

  val remove_type_printer: t -> Ident.t -> t
  (** Remove printer for types *)

  (** {7 Overloading} *)

  val overload_lookup:
    t -> string -> (Ident.t * Basic.gtype) list
  (** [overload_lookup s]: Find the list of identifiers which may
      be overloaded on [s]. String [s] may be a symbol or the
      short name of a term.  *)

  val add_overload: 
    t -> string -> Theory.sym_pos -> (Ident.t * Basic.gtype) 
    -> t
  (** 
      [add_overload sym pos (id, ty)]: Overload identifier [id], with
      type [ty] on symbol [sym]. Put [id] in position [pos]. 
  *)

  val remove_overload: t -> string -> Ident.t -> t
  (** 
      [remove_overload sym id]: Remove [id] from the list of identifiers
      overloading symbol [sym].
  *)

  (** {6 Parsing} *)

  val expand_term: 
    scoped -> Pterm.t -> Basic.term
  (** Resolve symbols and short names in terms and types, replacing
      them with long identifiers where possible. Also retype the term
      if possible. Intended to make a parsed term suitable for passing
      to {!Formula.make}. Never fails but resulting term may be
      inconsistently typed.  *)

  val expand_type_names: 
    scoped -> Basic.gtype -> Basic.gtype
  (** Replace symbols and short names in a type with the long
      identifier, were possible.  *)
  val expand_typedef_names: 
    scoped -> Parser.typedef_data -> Defn.Parser.typedef
  (** Resolve symbols and short names in a type definition.  *)

  val mk_term: scoped -> Pterm.t -> Basic.term
  (** Resolve symbols and short names in a term, making a parsed term
      suitable for use.
      
      Replaces short names and symbols with their long identifiers where
      possible. Also retypes the term if possible. Never fails but
      resulting term may be inconsistently typed.
  *)

  val read: scoped -> string -> Basic.term
  (** Parse a string as a term, resolving short names and
      symbols. *)

  val read_unchecked: t -> string -> Basic.term
  (** Parse a string as a term, return the term as is, without
      expanding terms and resolving symbols.  *)

  val read_defn:scoped -> string 
    -> ((string * Basic.gtype) * Basic.term list) * Basic.term
  (** Parse a string as a term definition. *)

  val read_type: scoped -> string -> Basic.gtype
  (** Parse a string a type, resolving short names and symbols where
      possible.  *)
  val read_type_defn: scoped -> string -> Defn.Parser.typedef
  (** Parse a string as a type definition. *)

  val read_identifier: t -> string -> Ident.t
(** Parse a string as an identifier. *)
end
