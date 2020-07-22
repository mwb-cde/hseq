(*----
  Copyright (c) 2006-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** Printer-Parser for Boolean functions. *)

open Grammars
open Pkit
open Lexer

val negation_pprec: Printkit.record
(** Printer for negation (base.not). Prints [ << base.not x >> ] as
    [~x] rather than [~ x].
*)

val ifthenelse_id: Ident.t
(** [ifthenelse_id]: Identifier for the conditional.
*)

val ifthenelse_pprec: Printkit.record
(**
   [ifthenelse_prec]: Precedence/fixity/associativity of the conditional.
*)

val ifthenelse_parser: parser_info -> Pterm.t phrase
(** Parser for the conditional. The conditional has syntax [<< if b
    then t else f >>].
*)

val ifthenelse_printer:
  Printers.ppinfo
  -> (Printkit.fixity * int)
  -> (Term.term * Term.term list) Printkit.printer
(** Printer for the conditional. *)

val choice_ident: Ident.t
(** Identifier for choice (the Hilbert epsilon) *)

val choice_sym: string
(** The symbol denoting the choice quantifier ([choice_sym = "@"])
*)

val choice_pp: Printkit.fixity * int
(** Precedence and fixity of the choice operator.
*)

val choice_parser: parser_info -> Pterm.t phrase
(** Parser for the choice operator. Syntax [<< @ x: P >>]
*)

val choice_printer:
  Printers.ppinfo
  -> (Printkit.fixity * int)
  -> (Term.term * Term.term list) Printkit.printer
(** Printer for the choice operator. *)

type symbol = (Ident.t * int * Printkit.fixity * string option)

val basethy_type_symbols: symbol list
val basethy_term_symbols: symbol list
val quote_type_symbols: symbol list
val quote_term_symbols: symbol list

val init_bool_parsers: Parser.Table.t -> Parser.Table.t
val init_bool_printers: Printers.ppinfo -> Printers.ppinfo
val init_bool_ppinfo:
  Printers.ppinfo -> ((symbol list) * (symbol list)) -> Printers.ppinfo
val init_bool_tokens:
  Parser.Table.t -> ((symbol list) * (symbol list)) -> Parser.Table.t

(** {7 Minimal printer and parser information} *)
val ppinfo: unit -> Printers.ppinfo

(** {7 OCaml Quotations support} *)

val basethy_context: unit -> Context.t
val quote_context: Context.t
val read: string -> Term.term
(** Parse a string as a term, resolving short names and
    symbols. *)
val read_unchecked: string -> Term.term
(** Parse a string as a term, resolving short names and
    symbols. *)
val read_defn:
  string -> ((string * Gtype.t) * Term.term list) * Term.term
(** Parse a string as a term definition. *)
val read_type: string -> Gtype.t
(** Parse a string a type, resolving short names and symbols where
    possible.  *)
val read_type_defn : string -> Defn.Parser.typedef
(** Parse a string as a type definition. *)
val read_identifier: string -> Ident.t
(** Parse a string as an identifier. *)
