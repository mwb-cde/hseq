(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** Top-level pretty printers *)

val print_term: Printers.ppinfo -> Term.term -> unit
val print_formula: Printers.ppinfo -> Formula.t -> unit
val print_type: Printers.ppinfo -> Gtype.t -> unit
val print_theory: Printers.ppinfo -> Theory.thy -> unit

val print_sqnt: Printers.ppinfo -> Logic.Sequent.t -> unit
val print_node: Printers.ppinfo -> Logic.node -> unit
val print_branch: Printers.ppinfo -> Logic.branch -> unit

val print_thm: Printers.ppinfo -> Logic.thm -> unit
val print_defn: Printers.ppinfo -> Logic.Defns.cdefn -> unit

val print_prf: Printers.ppinfo -> Goals.Proof.t -> unit
val print_prfstk: Printers.ppinfo -> Goals.ProofStack.t -> unit

val print_fnident: Ident.t -> unit

val print_subst: ('a, 'a)Hashtbl.t -> ('a -> string) -> unit
val fprint_error: Format.formatter -> Printers.ppinfo -> Report.error -> unit
val print_error: Printers.ppinfo -> Report.error -> unit
val print_report: Printers.ppinfo -> int -> exn -> unit
val print_type_error: Printers.ppinfo -> Gtype.error -> unit
