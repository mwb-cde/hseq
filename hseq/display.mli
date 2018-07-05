(*----
  Name: display.mli
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

(** Top-level pretty printers *)

open Basic

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
