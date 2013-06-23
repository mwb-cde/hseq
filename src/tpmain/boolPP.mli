(*----
  Name: boolPP.mli
  Copyright M Wahab 2006-2010
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

(** Printer-Parser for Boolean functions. *)

open Grammars
open Pkit
open Lexer

val negation_pprec: Printer.record
(** Printer for negation (base.not). Prints [ << base.not x >> ] as
    [~x] rather than [~ x].
*)

val ifthenelse_id: Ident.t
(** [ifthenelse_id]: Identifier for the conditional.
*)

val ifthenelse_pprec: Printer.record
(**
   [ifthenelse_prec]: Precedence/fixity/associativity of the conditional.
*)

val ifthenelse_parser: parser_info -> Pterm.t phrase
(** Parser for the conditional. The conditional has syntax [<< if b
    then t else f >>].
*)

val ifthenelse_printer: 
  Printer.ppinfo
  -> (Printer.fixity * int) 
  -> (Basic.term * Basic.term list) Printer.printer
(** Printer for the conditional. *)

val choice_ident: Ident.t
(** Identifier for choice (the Hilbert epsilon) *)

val choice_sym: string
(** The symbol denoting the choice quantifier ([choice_sym = "@"])
*)

val choice_pp: Printer.fixity * int
(** Precedence and fixity of the choice operator.
*)

val choice_parser: parser_info -> Pterm.t phrase
(** Parser for the choice operator. Syntax [<< @ x: P >>]
*)

val choice_printer: 
  Printer.ppinfo
  -> (Printer.fixity * int) 
  -> (Basic.term * Basic.term list) Printer.printer
(** Printer for the choice operator. *)

(** {7 Minimal printer and parser information} *)
val ppinfo: unit -> Printer.ppinfo

(** {7 OCaml Quotations support} *)

val read: string -> Basic.term
(** Parse a string as a term, resolving short names and
    symbols. *)
val read_unchecked: string -> Basic.term
(** Parse a string as a term, resolving short names and
    symbols. *)
val read_defn:
  string -> ((string * Basic.gtype) * Basic.term list) * Basic.term
(** Parse a string as a term definition. *)
val read_type: string -> Basic.gtype
(** Parse a string a type, resolving short names and symbols where
    possible.  *)
val read_type_defn : string -> Defn.Parser.typedef
(** Parse a string as a type definition. *)

val read_identifier: string -> Ident.t
(** Parse a string as an identifier. *)
