(*----
  Name: parser.mli
  Copyright Matthew Wahab 2005-2016
  Author: Matthew Wahab <mwb.cde@gmail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under the
  terms of the Lesser GNU General Public License as published by the Free
  Software Foundation; either version 3, or (at your option) any later
  version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public License for
  more details.

  You should have received a copy of the Lesser GNU General Public License
  along with HSeq.  If not see <http://www.gnu.org/licenses/>.
  ----*)

(** Parsers for terms and types.

   Top-down parsers for terms, types and their definitions using the
   ParserKit constructors, instantiated in the module {!Grammars.Pkit}.
   A parser applies the rules of a grammar to the token stream
   constructed by the functions in modules {!Lexer}.  A grammar is
   described as a {!Parser.phrase}, and a phrase can also be
   built up from one or more phrases. A parser is built from a phrase
   using function {!Parserkit.T.parse}.

   The grammars are grouped around terms and types, with some
   miscellaneous utlity parsers. The term parsers include the grammars
   for term definitions. The type parsers include the grammars for
   type definitions.

   This module holds the standard symbol tables and token information
   needed for the toplevel parsers. The toplevel function, for
   parsing a string, is {!Parser.read}, which is a specialisation of
   {!Lexer.reader} using the standard symbol table.

   The terms returned by the term parsers construct are represented by
   {!Pterm.t}. These can be converted to a {!Basic.term} using
   {!Pterm.to_term}.  Operator overloading is supported using
   {!Pterm.resolve}.

   The toplevel term parser is made up of two parts: the first parses
   a string to construct an initial term. The second resolves
   names in this term, expanding short names by finding the theory
   of an identifier with a matching type. The name resolution
   compares the type inferred for the name with the actual type of
   the identifiers, chosing the first to succeed.
*)

(** {5 Token information} *)

type associativity = Grammars.associativity
(** Token associativity (exactly the same as used by the lexer) *)
val left_assoc : associativity
(** Left associative *)
val right_assoc : associativity
(** Right associative *)
val non_assoc : associativity
(** Non-associative *)

type fixity = Grammars.fixity
(** Token fixity (exactly the same as used by the lexer) *)
val nonfix : fixity
(** Non-fix *)
val infix : associativity -> fixity
(** Infix of given associativity *)
val prefix : fixity
(** Prefix *)
val suffix : fixity
(** Suffix *)

(** {7 Default token information} *)

val default_term_prec: int
    (** Default term precedence *)
val default_term_assoc: associativity
    (** Default term associativity *)
val default_term_fixity: fixity
    (** Default term fixity *)

val default_type_prec: int
    (** Default type precedence *)
val default_type_assoc: associativity
    (** Default type associativity *)
val default_type_fixity: fixity
    (** Default type fixity *)

(** {7 Types used by the parsers. } *)

type input = Grammars.input
(** The input stream *)
type 'a parse = 'a Grammars.parse
(** A parser *)
type 'a phrase = 'a Grammars.phrase
(** A grammar *)

type typedef_data = Grammars.typedef_data
(**
   Information returned by the type definition parsers.
*)

exception ParsingError of string
(** Exception for reporting parse errors *)


(** {5 Parser Data}

   The standard lexers and parsers use tables of symbols and
   tokens. The symbol table is used by the lexer and maps strings to
   tokens. Two token tables hold fixity and precedence information
   about tokens which can appearing in term and type parsers.

   Symbols should be added to the symbol table if they are to
   recognised by the lexer. This includes any symbol used in a
   user-defined parser or as a symbolic representation of an
   identifier. A token should be added to the token table, together
   with its fixity information, if it represents an identifier. Note
   that there are

   For example, if the symbol "@" is to be used in a user-defined
   parser then it should added to the symbol table, possibly mapping to
   the token [Sym (Other "@")].

   For another example, assume the identifier "Set.in" is to be
   represented as the infix operator "in". First the symbol "in" is
   added to the symbol table, mapping to the token [Sym (Other
   "in")]. Then the token [Sym (Other "in")] is added to the token
   table, with appropriate fixity and precedence.
 *)

type symbol_table = Lexer.symtable
val default_symtable_size: int
(** The default size of the table of symbols *)
val core_symbols : (string * Lexer.tok) list
(** The list of builtin symbols *)

type sym_pos = Ident.t Lib.position
(** The precedence of a term identifier overloaded on a
    symbol. (Default [First].)
*)

module OverloadTree : Treekit.SimpleTreeType with type key = string
type overload_table_t = ((Ident.t * Gtypes.t) list) OverloadTree.t

val default_overload_table_size : int
(** The default size of the overloading table. *)

(** {5 Parser Tables} *)
module Table:
sig
  (** Parser tables *)
  type t =
      {
        tokens_f: Grammars.token_table;
        type_tokens_f: Grammars.token_table;
        symbols_f: Lexer.symtable;
        overloads_f: overload_table_t;
        term_parsers_f:
          (string,
           Grammars.parser_info -> Pterm.t phrase) Lib.named_list;
        type_parsers_f:
          (string,
           Grammars.parser_info -> (Gtypes.t phrase)) Lib.named_list;
      }

  (** Default sizes *)
  val default_size: (int * int * int * int)

  (** The empty table, of given size *)
  val empty: (int * int * int * int) -> t

  (** Initialize a table *)
  val init: t -> t

  (** Accessors *)
  val tokens: t -> Grammars.token_table
  val set_tokens: t -> Grammars.token_table -> t
  val type_tokens: t -> Grammars.token_table
  val set_type_tokens: t -> Grammars.token_table -> t
  val symbols: t -> Lexer.symtable
  val set_symbols: t -> Lexer.symtable -> t
  val overloads: t -> overload_table_t
  val set_overloads: t -> overload_table_t -> t
  val term_parsers:
    t -> (string, Grammars.parser_info -> Pterm.t phrase) Lib.named_list
  val set_term_parsers:
    t -> (string, Grammars.parser_info -> Pterm.t phrase) Lib.named_list
    -> t
  val type_parsers:
    t -> (string, Grammars.parser_info -> Gtypes.t phrase) Lib.named_list
  val set_type_parsers:
    t
    -> (string, Grammars.parser_info -> Gtypes.t phrase) Lib.named_list
    -> t
end

(** {7 Toplevel symbol and token functions} *)

val add_token:
  Table.t -> Ident.t -> string -> fixity -> int -> Table.t
(**
   [add_token id sym fix prec]: Add symbol [sym] as representation for
   term identifier [id], with fixity [fix] and precedence [prec].
   Updates term symbol and token tables.
*)

val remove_token: Table.t -> string -> Table.t
(**
   [remove_token sym]: Remove [sym] and associated token information
   from the term symbol and token tables.
*)

val add_type_token:
  Table.t -> Ident.t -> string -> fixity -> int -> Table.t
(**
   [add_type_token id sym fix prec]: Add symbol [sym] as representation for
   type identifier [id], with fixity [fix] and precedence [prec].
   Updates type symbol and token tables.
*)

val remove_type_token : Table.t -> string -> Table.t
(**
   [remove_type_token sym]: Remove [sym] and associated token information
   from the type symbol and token tables.
*)

val add_overload:
  Table.t
  -> string -> sym_pos -> (Ident.t * Gtypes.t)
  -> Table.t
(**
   [add_overload sym pos (id, ty)]: Overload identifier [id], with
   type [ty] on symbol [sym]. Put [id] in position [pos].
*)
val get_overload_list:
  Table.t-> string -> (Ident.t * Gtypes.t) list
(**
   [get_overload_list sym]: Get the list of identifiers overloaded on
   symbol [sym].
*)
val remove_overload:
  Table.t -> string -> Ident.t -> Table.t
(**
   [remove_overload sym id]: Remove [id] from the list of identifiers
   overloading symbol [sym].
*)


val mk_info: Table.t -> Grammars.parser_info
(** Make parser information ([parser_info)] from a table.  *)

val print_overloads: Table.t -> Printer.ppinfo -> unit
(** Print the overloads table. *)


(** {5 Initialising functions} *)

val init_symbols: Table.t -> Table.t
(** Initialise the symbols *)

val init_parsers : Table.t -> Table.t
(** Initialise the type and term parsers *)

val init : unit -> Table.t
(**
   Initialise the parser tables (symbols, tokens and overloading).
   This must be called before any of the parsers are used.
*)


(** {5 Toplevel Parser functions}

   Parsers to read a phrase followed by an end of file/string.
*)

val parse : 'a phrase -> 'a parse
(** Make a parser from a phrase *)

val identifier_parser: Table.t -> input -> Ident.t
(** Read a possibly long identifier *)

val type_parser : Table.t -> input -> Gtypes.t
(** Read a type. *)

val typedef_parser : Table.t -> input -> typedef_data
(** Read a type definition *)

val term_parser : Table.t -> input -> Pterm.t
(** Read a term *)

val defn_parser :
  Table.t -> input -> ((string * Gtypes.t) * Pterm.t list) * Pterm.t
(** Read a term definition *)

(** {7 Symbols} *)

val add_symbol: Table.t -> string -> Lexer.tok -> Table.t
val find_symbol: Table.t -> string -> Lexer.tok
val remove_symbol: Table.t -> string -> Table.t

(** {7 User defined parsers} *)

val term_parser_list :
    Table.t -> (string, Grammars.parser_info -> Pterm.t phrase) Lib.named_list
(**
   The list of user defined term parsers. Parsers added to this list are
   used by the term parser {!Grammars.form}.
 *)

val add_term_parser:
  Table.t -> (string) Lib.position -> string
  -> (Grammars.parser_info -> Pterm.t phrase)
  -> Table.t
(**
   [add_term_parser pos n ph]: Add the term parser [ph] at position
   [pos] with name [n] to {!Parser.term_parser_list}.
*)

val remove_term_parser: Table.t -> string -> Table.t
(**
   [remove_term_parser s]: Remove the term parser named [s] from
   {!Parser.term_parser_list}. Raise [Not_found] if not present.
 *)

val type_parser_list :
  Table.t
  -> (string, Grammars.parser_info -> Gtypes.t phrase) Lib.named_list
(**
   The list of user defined type parsers. Parsers added to this list are
   used by the term parser {!Grammars.types}.
 *)

val add_type_parser:
  Table.t -> (string)Lib.position -> string
  -> (Grammars.parser_info -> Gtypes.t phrase)
  -> Table.t
(**
   [add_type_parser pos n ph]: Add the type parser [ph] at position
   [pos] with name [n] to {!Parser.type_parser_list}.
*)

val remove_type_parser: Table.t -> string -> Table.t
(**
   [remove_type_parser s]: Remove the type parser named [s] from
   {!Parser.type_parser_list}. Raise [Not_found] if not present.
 *)

(** {7 Readers}

   Read and parse a string
*)

val read: Table.t -> (Table.t -> ('a)parse) -> string -> 'a
(** [read ph str]: Parse string [str] with parser [ph]. *)

val read_term : Table.t -> string -> Pterm.t
(** [read_term str]: Parse string [str] using the standard term parser. *)

val read_type : Table.t -> string -> Gtypes.t
(** [read_type str]: Parse string [str] using the standard term parser. *)

(** {7 Debugging} *)

val test_lex : Table.t -> string -> Lexer.tok Parserkit.Input.t
val test : Table.t -> string -> Pterm.t
