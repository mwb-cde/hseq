(*----
  Name: display.mli
  Copyright M Wahab 2005-2010
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

(** Top-level pretty printers *)

open Basic

val print_term: Basic.term -> unit
val print_formula: Formula.t -> unit
val print_type: Basic.gtype -> unit
val print_theory: Theory.thy -> unit

val print_sqnt: Logic.Sequent.t -> unit
val print_node: Logic.node -> unit
val print_branch: Logic.branch -> unit

val print_thm: Logic.thm -> unit
val print_defn: Logic.Defns.cdefn -> unit

val print_prf: Goals.Proof.t -> unit
val print_prfstk: Goals.ProofStack.t -> unit

val print_fnident: Ident.t -> unit

val print_subst: ('a, 'a)Hashtbl.t -> ('a -> string) -> unit
val print_error: Report.error -> unit
