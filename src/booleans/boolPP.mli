(*-----
   Name: boolPP.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2006
   ----*)


(** Printer-Parser for Boolean functions. *)

open Grammars
open Pkit
open Lexer

val negation_pprec : Printer.record
  (**
     Printer for negation (base.not). Prints [ << base.not x >> ] 
     as [~x] rather than [~ x].
  *)

val ifthenelse_id: Ident.t
  (**
     [ifthenelse_id]: Identifier for the conditional.
  *)

val ifthenelse_pprec : Printer.record
  (**
     [ifthenelse_prec]: Precedence/fixity/associativity of the conditional.
  *)

val ifthenelse_parser: parser_info -> Basic.term phrase
  (**
     Parser for the conditional. The conditional has syntax [<< if b then
     t else f >>].
  *)

val ifthenelse_printer: 
  Printer.ppinfo
  -> (Printer.fixity * int) 
  -> (Basic.term * Basic.term list) Printer.printer
  (**
     Printer for the conditional
  *)

val choice_ident: Ident.t
  (** Identifier for choice (the Hilbert epsilon) *)

val choice_sym : string
  (**
     The symbol denoting the choice quantifier ([choice_sym = "@"])
  *)

val choice_pp: Printer.fixity * int
  (**
     Precedence and fixity of the choice operator.
  *)

val choice_parser: parser_info -> Basic.term phrase
  (**
     Parser for the choice operator. Syntax [<< @ x: P >>]
  *)

val choice_printer: 
  Printer.ppinfo
  -> (Printer.fixity * int) 
  -> (Basic.term * Basic.term list) Printer.printer
  (** Printer for the choice operator. *)


(** {7 Initialising functions } *)

val init_printers : unit -> unit
  (** Initialise printers. *)   

val init_parsers : unit -> unit
  (**
     Initialise parsers.
  *)

val init: unit -> unit
  (** Initialise printers and parsers. *)
