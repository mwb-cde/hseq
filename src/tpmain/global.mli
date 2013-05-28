(*----
  Name: global.mli
  Copyright M Wahab 2005-2010, 2012
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

(** {3 Toplevel state} *)

(** The default context. *)
val default_context: unit -> Context.t

(** Global state *)
val state : unit -> Context.t
val set_state: Context.t -> unit

(** Short cut to {!Thys.get_theories.} *)
val theories: unit -> Thydb.thydb

(** Short cut to {!Thys.current.} *)
val current: unit -> Theory.thy

(** Short cut to {!Thys.current_name.} *)
val current_name: unit -> string

(** The global scope. Constructed from the theory database. *)
val scope: unit -> Scope.t

(** {5 Printing and Parsing}

    Global printer tables and functions to add, query and remove
    combined printer-parser information.
*)
module PP:
sig

  val info: unit -> Printer.ppinfo
  (** Get the global printer information table. *)

(***
  val set: Printer.ppinfo -> unit
  (** Set the global PP information table. *)
***)

(***
  val init: unit -> unit
  (** Initialise the printer and parser tables. *)
***)

  (** {7 Terms} *)

  val get_term_pp: 
    Ident.t -> (int * Printer.fixity * string option)
  (** Get PP information for term identifer.  Returns
      [(default_term_prec, default_term_fixity, None)] if not
      found.  
  *)

(***
  val add_term_pp: 
    Ident.t -> int -> Printer.fixity 
    -> string option -> unit
  (** Add printer information for term identifer. *)

  val add_term_pp_record: 
    Ident.t -> Printer.record -> unit
  (** Add printer record for term identifer. *)

  val remove_term_pp: Ident.t -> unit
  (** Remove PP information for term identifer occuring in a term. *)
***)

  (** {7 Types} *)

  val get_type_pp: Ident.t -> (int * Printer.fixity * string option)
  (** Get PP information for type identifer. *)

(***
  val add_type_pp: 
    Context.t -> Ident.t -> int -> Printer.fixity -> string option 
    -> Context.t
  (** Add PP information for type identifer. *)

  val add_type_pp_record: 
    Context.t -> Ident.t -> Printer.record -> Context.t
  (** Add PP record for type identifer. *)

  val remove_type_pp: 
    Context.t -> Ident.t -> Context.t
  (** Remove PP record for type identifer. *)
***)

  (** {6 User-defined printers} *)

  val get_term_printer: Ident.t -> Printer.term_printer
  (** Get printer for terms. *)

(***
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
***)

  val get_type_printer: Ident.t -> Printer.gtype_printer
  (** Get printer for types *)

(***
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
***)
(*****

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
****)

end

(*****
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
****)

(*****
(** Convenience module, so that readers are available *)
module Read:
sig

  val term: string -> Basic.term
  (** Read a term. *)

  val ltype: string -> Basic.gtype
  (** Read a type. *)

  val identifier: string -> Ident.t
  (** Read an identifier. *)

  val defn:
    string -> ((string * Basic.gtype) * Basic.term list) * Basic.term
  (** Read a term definition. *)

  val typedef: string -> Defn.Parser.typedef
  (** Read a type definition. *)

end
****)

