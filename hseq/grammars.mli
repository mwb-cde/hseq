(*----
  Name: grammars.mli
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

(** Parser constructors for types and terms

    Top-down parsers for terms, types and their definitions using the
    ParserKit constructors, instantiated in the module
    {!Grammars.Pkit}.  A parser applies the rules of a grammar to the
    token stream constructed by the functions in modules {!Lexer}.  A
    grammar is described as a {!Grammars.phrase}, and a phrase can
    also be built up from one or more phrases. A parser is built from
    a phrase using function {!Parserkit.T.parse}.

    The grammars are grouped around terms and types, with some
    miscellaneous utlity parsers. The term parsers include the
    grammars for term definitions. The type parsers include the
    grammars for type definitions.
*)

(** {5 Token information} *)

type associativity = Parserkit.Info.associativity
(** Token associativity (exactly the same as used by the lexer) *)
val left_assoc: associativity
(** Left associative *)
val right_assoc: associativity
(** Right associative *)
val non_assoc: associativity
(** Non-associative *)

type fixity = Parserkit.Info.fixity
(** Token fixity (exactly the same as used by the lexer) *)
val nonfix: fixity
(** Non-fix *)
val infix: associativity -> fixity
(** Infix of given associativity *)
val prefix: fixity
(** Prefix *)
val suffix: fixity
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

(** {5 Parser grammars} *)

(** Parser constructors specialised to tokens from {!Lexer}. *)
module Pkit: (Parserkit.T with type token = Lexer.tok)

(** Useful parser constructors  *)
module Utility:
sig
  open Pkit

  val (?$): Lexer.tok -> Pterm.t phrase
  (** [?$ sym]: Utility function for building term parsers using
      {!Parserkit.T.seq}.  Parse symbol [sym], return term
      [Pterm.mk_free (Lexer.string_of_token)].  *)

  val (?%): Lexer.tok -> Basic.gtype phrase
(** [?% sym]: Utility function for building type parsers using
    {!Parserkit.T.seq}. Parse symbol [sym], return term [Gtypes.mk_var
    (Lexer.string_of_token)].
*)
end

(** {7 Basic types used by the parsers. } *)

type input = Pkit.input
(** The input stream *)
type 'a parse = input -> 'a
(** A parser *)
type 'a phrase = 'a Pkit.phrase
(** A grammar *)

exception ParsingError of string

(** Information returned by the type definition parsers.
*)
type typedef_data =
  | NewType of (string * (string list))
  (** A new type: the type name and its arguments. *)
  | TypeAlias of (string * (string list) * Basic.gtype)
  (** A type alias: the type name, its arguments and the type it
      aliases *)
  | Subtype of (string * (string list)
                * Basic.gtype * Pterm.t)
(**
    Subtype definition: The type name, its arguments, the type it
    subtypes and the defining predicate
*)

(** {7 Parser Grammars} *)

(** {7 Utility types and functions} *)

type token_info = (Ident.t * fixity * int) option

val string_of_tok: Lexer.tok -> string
val string_tokens: Lexer.tok list -> string

(** {7 Token tables}

    A token table stores information about tokens. A token in the
    table is the respresentation of a symbol which can occur in an
    input stream.
*)

module TokenTree: Treekit.SimpleTreeType with type key = Pkit.token

type token_table
(** Token tables *)
val default_table_size: int
(** Default token table size *)
val token_table_new: int -> token_table
(** Make a token table *)
val token_table_reset: token_table -> token_table
(** Reset a token table *)
(*
val token_table_add: token_table -> Lexer.tok -> token_info -> unit
*)
val token_table_add:
  token_table -> Lexer.tok -> token_info -> token_table
(** Add a token to token table *)
val token_table_find: token_table -> Lexer.tok-> token_info
(** Lookup a token in a token table *)
(*
val token_table_remove: token_table -> Lexer.tok -> unit
*)
val token_table_remove: token_table -> Lexer.tok -> token_table
(** Remove a token from a token table *)

(** {7 Parser information}  *)

(** Information used by the parsers. Includes a record of the bound
    names found in a term and the names found in terms and types
*)
type parser_info =
    {
      (* Term information *)
      bound_names: (string* Pterm.t) list ref;
      (** Names found in a term and the term they are to be replaced
          with *)
      token_info: Pkit.token -> token_info;
      (** Get the information for a token found in a term *)
      term_parsers:
        (string, parser_info -> Pterm.t phrase) Lib.named_list;
      (** Extra term parsers *)

      (* Type information *)
      typ_indx: int ref; (** Counter to generate type names *)
      typ_names: (string* Basic.gtype)list ref;
      (** Names found in a type and their replacements *)
      type_token_info: Pkit.token -> token_info;
      (** Get the information for a token found in a type *)
      type_parsers:
        (string, parser_info -> (Basic.gtype phrase)) Lib.named_list;
      (** Extra type parsers *)
    }

val mk_info:
  (token_table * token_table
     * (string, parser_info -> Pterm.t phrase) Lib.named_list
     * (string, parser_info -> (Basic.gtype phrase)) Lib.named_list)
  ->
  parser_info
(** [mk_inf tbl type_tbl]: Make parsing information from tables [tbl]
    and [type_tbl] of term and type token information.
*)

val lookup_name: string -> parser_info -> Pterm.t
(** [lookup_name n inf]: Look up [n] in [inf.bound_names].  raise
    [Not_found] if not found.
*)

val add_name: string -> Pterm.t -> parser_info -> unit
(** [add_name n trm inf]: Associate [trm] with name [n] in
    [inf.bound_names].  Used to associate variable name with a bound
    variable,

    For example, at the top of a binding term [!x. t], [add_name x
    (Bound b) inf] is called to associate [x] with [Bound b] when
    parsing [t].
*)

val drop_name: string -> parser_info -> unit
(** [drop_name n inf]: Remove [n] from the list of bound names.  For
    example, after parsing the body [t] of a binding term [! x. t].
*)

val get_term: string -> parser_info -> Pterm.t
(** [get_term n inf]: Get the term associated with bound name [n].  if
    there is no term associated with [n] then return a short identifier
    made from [n] (as [mk_free n (Gtype.mk_null())]).
*)

val clear_names: parser_info -> parser_info
(** [clear_names inf]: Clear the bound names of [inf].
*)

val get_type_indx: parser_info -> int
(** [mk_vartyp inf]: Get and increment the type index [inf.typ_indx].
*)

val mk_vartyp: parser_info -> Basic.gtype
(** [mk_vartyp inf]: Make a new, uniquely named, type variable.
    Increments [inf.typ_indx].
*)

val lookup_type_name: string -> parser_info -> Basic.gtype
(** [lookup_type_name n inf]: Lookup type variable name [n].  If not
    found, raise [Not_found].
*)

val add_type_name: string -> Basic.gtype -> parser_info -> Basic.gtype
(** [add_type_name n ty inf]: Add [n] as the string representation of
    gtype [ty].
*)

val get_type: string -> parser_info -> Basic.gtype
(** [get_type n inf]: Get the type variable represented by name [n].
    If not found, create a type variable [ty], with a unique name,
    associate [n] with [ty] in [inf] and return [ty].
*)

val clear_type_names: parser_info -> parser_info
(** [clear_type_names inf]: Clear the record of type variable names.
*)

(** {7 Token information utility functions} *)

val mk_token_info: token_info -> Pkit.token_info
(** [mk_token_info x]: Extract the precedence and fixity information
    from term token [x], if any. If not, return the default term fixity
    and precedence. For use with the {!Parserkit.T.operators}
    constructor.
*)

val mk_type_token_info: token_table -> Lexer.tok -> Pkit.token_info
(** [mk_type_token_info x] Extract the precedence and fixity
    information from type token [x], if any. If not, return the default
    type fixity and precedence.  Used with the Parserkit.operator
    parser.
*)

(** {5 Utility parsers} *)

val message: string -> 'a phrase
(** [message m]: Fail, raising [ParsingError m]  *)

val error: string -> 'a phrase
(** [error msg]: Fail, using [msg] as the error message  *)

val term_error: string -> 'a phrase
(** [term_error msg]: Fail, using [msg] as the error message  *)

val type_error: string -> 'a phrase
(** [type_error msg]: Fail, using [msg] as the error message  *)

val comma_list: 'a phrase -> 'a list phrase
(** [comma_list ph]: Parse a comma seperated list of phrases [ph] *)

val repeat_term: 'a phrase -> 'b phrase -> 'a list phrase
(** [repeat_term ph1 ph2]: Repeatedly parse phrase [ph1], terminated
    by final phrase [ph2].
*)

(** {5 Identifier Parsers} *)

val id_parser: (Lexer.tok -> token_info) -> Ident.t phrase
(** [id_parser info inp]: General identifier parser.  Matches
    identifiers and symbols which translate to identifiers.
*)

val id_strict: (Lexer.tok -> token_info) -> Ident.t phrase
(** [id_strict info inp]: Strict identifier parser.  Matches (possibly
    qualified) identifiers only, not symbols.  *)

val id_relaxed: parser_info -> Ident.t phrase
(** [id_relaxed info inp]: Relaxed identifier parser.  Matches
    (possibly qualified) identifiers and symbols.  *)

val named_id:
  parser_info
  -> (parser_info -> Ident.t phrase)
  -> Ident.t -> Ident.t phrase
(** [named_id info ph name inp]: Parse an identifier [name].  Fail if
    token doesn't match the given name. Uses parser [ph inf] to parse
    the identifier.
*)

val short_id: (parser_info -> Ident.t phrase)
  -> parser_info -> string phrase
(** [short_id ph inf toks]: Parse a short (unqualified)
    identifier. Uses parser [ph inf] to parse the identifier.
*)

val long_id: (parser_info -> Ident.t phrase)
  -> parser_info -> Ident.t phrase
(** [short_id ph inf toks]: Parse a long (possibly qualified)
    identifier. Uses parser [ph inf] to parse the identifier.
*)

val mk_short_id: (parser_info -> Ident.t phrase)
  -> parser_info -> string phrase
(** [mk_short_id ph inf]: Parse a possibly qualified identifer with
    [ph inf], make it a short identifier.  *)

(**
    {5 Type parsers}

    Types are parsed according to the following rules:

    {[
    types ::= inner_types

    inner_types ::= operators(atomic_types)

    operators(atom) ::=
    op atom
    | atom op atom
    | atom op

    atomic_types ::=
    type_parsers
    | error

    type_parsers ::=
    primed_id
    | num_type
    | bool_type
    | type_constructor =
    [ '(' inner_types [ ',' inner_types ]* ')' ] long_id
    | bracketed_type = '(' inner_types ')'
    | User added parsers

    primed_id ::= PrimedID token (matched by lexer)
    num_type ::= "num"
    bool_type ::= "bool"
    ]}
*)

(** {7 Utility functions} *)

val mk_type_binary_constr:
  parser_info -> Lexer.tok -> Basic.gtype -> Basic.gtype -> Basic.gtype
(** [mk_type_binary_constr inf tok l r]: Make a gtype from binary type
    from constructor [tok], left argument [l] and right argument [r].
*)

val mk_type_unary_constr:
  parser_info -> Lexer.tok -> Basic.gtype -> Basic.gtype
(** [mk_type_unary_constr inf tok a]: Make a gtype from a unary type
    from constructor [tok] and argument [a].
*)

(** {7 The parsers} *)

val type_id: parser_info -> Ident.t phrase
(** [type_id]: Parse a type identifier. *)

val primed_id: parser_info -> Basic.gtype phrase
(** [primed_id inf]: Read a type variable name. *)

val bool_type: parser_info -> Basic.gtype phrase
(** [bool_type info]: Parse type "bool" *)
val num_type: parser_info -> Basic.gtype phrase
(** [num_type info]: Parse type "num" *)

val type_constructor_parser: parser_info -> Basic.gtype phrase
(** [type_constructor_parser inf]: Parse "['(' <args> ')']<constructor>" *)

val bracketed_type_parser: parser_info -> Basic.gtype phrase
(** [bracketed_type_parser info]: Parse "'(' <type> ')'" *)


val inner_types: parser_info -> Basic.gtype Pkit.phrase
(** Parse infix/prefix/suffix type operators *)

val atomic_types: parser_info -> Basic.gtype phrase
(** The atomic types (num, bool, etc) *)

val type_parsers_list:
  parser_info ->
  (string, parser_info -> (Basic.gtype phrase)) Lib.named_list
(** A record of the type parsers used by {!Grammars.inner_types}.  Can
    be extended with user-defined parsers.
*)

val type_parsers: parser_info -> Basic.gtype phrase
(** The parser made from {!Grammars.inner_types}) *)

val types: parser_info -> Basic.gtype phrase
(** The main type parser. *)

val core_type_parsers:
  (string, parser_info -> (Basic.gtype phrase)) Lib.named_list
(** The built-in type parsers *)

val init_type_parsers:
  parser_info -> parser_info
(** Initialise the type parsers *)

(** {7 Support for adding parsers}

    Functions to add to and remove from the list of type parsers
    {!Grammars.type_parsers_list} used by {!Grammars.types}.

    Allows the use of user-defined type parsers.
*)

val add_type_parser:
  parser_info -> (string)Lib.position -> string
  -> (parser_info -> Basic.gtype phrase)
  -> parser_info
(** [add_type_parser pos n ph]: Add type parser [ph] at position [pos]
    with name [n].
*)

val remove_type_parser: parser_info -> string -> parser_info
(* [remove_type_parser n]: Remove the type parser named [n], raise
   [Not_found] if not present.
*)

(** {5 Term parsers}

    Terms are parsed according to the following grammar:

    {[
    form ::= formula [ formula* ]

    formula ::= operators(typed_primary)

    operators(atom) ::=
    op atom
    | atom op atom
    | atom op

    typed_primary ::= primary optional_type

    primary ::=
    term_parsers
    | error

    term_parsers ::=
    id
    | number
    | boolean
    | bracketed_term = '(' form ')'
    | forall = ALL (short_id optional_type)+ ':' form
    | exists = EX (short_id optional_type)+ ':' form
    | lambda = LAM (short_id optional_type)+ ':' form
    | User added parsers
    ]}

    Note that the parser for terms is called 'form'
    ({!Grammars.form}).
*)

(** {7 Utility functions} *)

val mk_conn :
  parser_info -> Lexer.tok -> Pterm.t -> Pterm.t -> Pterm.t
(** [mk_conn inf tok l r]: Make a binary operator term from token
    [tok] with left argument [l] and right argument [r].
*)

val mk_prefix : parser_info -> Lexer.tok -> Pterm.t -> Pterm.t
(** [mk_prefix inf tok a]: Make a binary operator term from token
    [tok] with argument [a].
*)

val qnt_setup_bound_names:
  parser_info -> Basic.quant -> (string * Basic.gtype) list
  -> (string * Pterm.t) list
(** [qnt_setup_bound_names inf qnt xs]: Make bound variables from the
    name-type pairs in [xs], add them to [inf.bound_names].  [qnt] is
    the quantifier type (All, Ex or Lambda).
*)

val qnt_term_remove_names:
  parser_info -> (string * Pterm.t) list
  -> Pterm.t -> Pterm.t
(** [qnt_term_remove_names inf xs body]: Use bound names in [xs] to
    form a quantified term, with body as the initial term.

    Simplified example: [[!x, ?y, !z] t] produces [(!x: (?y: (!z: t)))].

    Removes each name in [xs] from [inf.bound_names] as it is used.
*)

(** [make_term_remove_names info wrapper vs body]: Remove the
    variables in [vs] from [info].  Return the term constructed by
    quantifying [body] with the variables [vs], applying [wrapper] to
    each constructed term.
*)
val make_term_remove_names:
  parser_info
  -> (Pterm.t -> Pterm.t)
  -> (string * Pterm.t) list
  -> Pterm.t -> Pterm.t

(** {7 The parsers} *)

(** Read a number constant. *)
val boolean : bool phrase
(** Read a boolean constant. *)

val optional_type : parser_info -> Basic.gtype option phrase
(** Parse an optional type.

    [ optional_type ::= [ ':' types ] ]
*)

val id: parser_info -> Ident.t phrase
(** [id]: Parse identifiers which occur in terms *)

val id_type_opt:
  (parser_info -> 'a Pkit.phrase) ->
  parser_info -> ('a * Basic.gtype) phrase
(** Parse an optionally typed identifier.

    [ id_type_opt ::= id optional_type ]
*)

val term_identifier: parser_info -> Pterm.t phrase
(** [term_identifer inf]: Parse an identifier that might appear in a
    quantified term. Use [id_type_opt inf] to get an identifier.
    Look-up identifier in [inf], to check if it is a bound variable.
    If not, it is a free variable.
*)

val form: parser_info -> Pterm.t phrase
(** The main term parser *)
val formula: parser_info -> Pterm.t phrase
(** Parse infix/prefix/suffix operators *)
val typed_primary: parser_info -> Pterm.t phrase
(** Parse a possibly typed atomic term *)

val term_parsers_list:
  parser_info -> (string, parser_info -> (Pterm.t)phrase) Lib.named_list
(** The list of atomic term parsers. Can be extended with user-defined
    parsers.
*)
val term_parsers: parser_info ->Pterm.t phrase
(** The parser built from the list of atomic term parsers *)
val primary: parser_info -> Pterm.t phrase
(** Parser for the atomic terms *)

val core_term_parsers:
  (string, parser_info -> (Pterm.t phrase)) Lib.named_list
(** The built-in term parsers *)

val init_term_parsers: parser_info -> parser_info
(** Initialise the term parsers *)

(** {7 Support functions} *)

val add_parser:
  parser_info -> string Lib.position
  -> string -> (parser_info -> Pterm.t phrase) -> parser_info
(** [add_parser pos n ph]: Add term parser [ph] with name [n] in
    position [pos] to {!Grammars.term_parsers_list}.
*)

val remove_parser: parser_info -> string -> parser_info
(** [remove_parser n]: Remove the term parser named [n] from
    {!Grammars.term_parsers_list}.
*)

val parse_as_binder:
  Ident.t -> string -> parser_info -> Pterm.t phrase
(** [parse_as_binder f sym]: Construct a grammar to parse function
    applications of the form [f (%x: P)] as [sym x: P].

    Symbol [sym] should be added to the lexer seperately.
    (e.g. using [add_symbol sym (Lexer.Sym(Lexer.OTHER sym))]).
*)

(** {5 Definitions} *)

(** {7 Type definitions}

    A type definition is either a simple type definition (declaring a
    new type); an aliasing definition or a subtyping definition.

    {[
    typedef ::=
    simple_typedef
    | subtypedef

    simple_typedef ::=
    [ '(' primed_id  (',' primed_id)* ')' ] type_id [ '=' types ]

    subtypedef ::=
    [ '(' primed_id  (',' primed_id)* ')' ] type_id
    '=' types ':' form
    ]}
*)

val simple_typedef:
  parser_info ->
  (string * string list option * Basic.gtype option)  phrase
(** Parse a type declaration or alias. If the rhs of the definition is
    given, it is an aliasing definition.
*)
val subtypedef:
  parser_info ->
  (string * string list option * Basic.gtype * Pterm.t)  phrase
(** Parse a subtyping definition. The term is the defining predicate
    for the type.
*)

val typedef: parser_info -> (typedef_data)  phrase
(** Parse a type definition. *)

(** {7 Term definitions}

    Term definitions.

    {[
    defn ::= short_id short_id* '=' form
    ]}

    In the form [ f a1 ... an = rhs ], [f] is the identifier being
    defined, the [a1 ... an] are the parameters and the form [rhs] is
    the definition of [f]. Occurences of the parameters [a1 .. an] in
    [rhs] are treated as short identifiers.

    Each of the arguments [ai] is converted to a bound variable [bi]
    (of kind [Basic.All]) and replaced in [rhs] with [bi].

    The parser returns the name [f], [b1 .. bi] and the body [rhs].
*)

val defn:
  parser_info ->
  (((string * Basic.gtype) * Pterm.t list) * Pterm.t) phrase
(** Parse a term definition. *)
