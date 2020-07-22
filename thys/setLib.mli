(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(*
   setLib.mli:

   Support code for setScript.ml.

   Adds a parser and printer for set terms.

   Parse and print [SET (%x: P)] as [{x: P}].

   Parse [{} ] as [empty].  The standard printer handles the empty set
   correctly.

   compile with [compile [] "setLib.mli"; compile [] "setLib.ml"]
*)

open HSeq

module SetPP:
sig

  val set_thy: string
(**
   [set_thy]: The name of the theory of sets.
*)

  val set_id : Ident.t
(**
   [set_id] The identifier of the set constructor
*)

  val set_data:(Printkit.fixity * int)
(**
   [set_data] The precedence and fixity of the set identifier
*)

(**
   [empty_id] The identifier of the set constructor

   [empty_data] PP data for the empty set.
*)
  val empty_id : Ident.t
  val empty_data:(Printkit.fixity* int)

  val ocb_sym: string
  val ccb_sym: string

  val main_parser:
    (Grammars.parser_info -> Pterm.t Parser.phrase)
  val set_list:
    (Grammars.parser_info -> Pterm.t Parser.phrase)
  val set_body:
    (Grammars.parser_info -> Pterm.t Parser.phrase)

  val set_parser:
    (Grammars.parser_info -> Pterm.t Parser.phrase)
  val set_printer :
    unit
    -> Printers.ppinfo -> (Printkit.fixity * int)
    -> (Term.term * Term.term list) Printkit.printer

  val init_set_parser : unit -> unit
  val init_set_printer : unit -> unit
end
