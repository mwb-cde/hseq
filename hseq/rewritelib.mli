(*----
  Name: rewritelib.mli
  Copyright Matthew Wahab 2006-2019
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

(** Generalised Rewriting

    Tactics, conversions and rules for rewriting with a list of
    theorems and assumptions. Combines rewrite planners and rewriting
    and allows the direction of rewriting (left-right/right-left) to
    be specified.
*)

module Rewriter:
sig
  (** This module implements a usable interface to rewriting, allowing
      the direction of rewriting to be specified and using theorems and
      assumptions as the rewrite rules. The conversions and tactics
      provided here should not be used directly, instead the
      conversions and the tactics of the main {!Rewritelib} module
      (e.g. {!Rewritelib.rewrite_tac} and {!Rewritelib.rewrite_conv})
      should be used.  *)

  val gen_rewrite_conv:
    Context.t -> Rewrite.control -> Tactics.rule list
    -> Logic.conv
  (** [gen_rewrite_conv scp ctrl rules trm]: rewrite term [trm] with rules
      [rrl] in scope [scp].

      Returns |- trm = X
      where [X] is the result of rewriting [trm]

      Discards any rule which is not a theorem or an ordered theorem.

      This conversion could be written using the rewriting tactics but
      this would require two sets of rewriting. The first to construct
      the term [X] on the rhs of the equality and the second when the
      rewrite tactic is invoked. By contrast, [rewrite_conv] only does
      one set of rewriting.  *)

  val gen_rewrite_rule:
    Context.t
    -> Rewrite.control
    -> Logic.rr_type list
    -> Logic.thm -> Logic.thm
  (** [gen_rewrite_rule scp ctrl rules thm]: rewrite theorem [thm] with
      rules [rrl] in scope [scp].

      Returns |- X where [X] is the result of rewriting [thm].
  *)

  (** {7 Tactics} *)

  val map_sym_tac:
    Tactics.rule list -> (Tactics.rule list)Tactics.data_tactic
  (** [map_sym_tac ret rules goal]: Apply [eq_sym] to each rule in
      [rules], returning the resulting list in [ret]. The list in
      [ret] will be in reverse order of [rules].

      Used to set assumptions and theorems in the right form for
      right-left rewriting.
  *)

  val gen_rewrite_tac:
    Rewrite.control
    -> Tactics.rule list -> Logic.label -> Tactics.tactic
(** [rewrite_tac ctrl rules l sq]: Rewrite formula [l] with
    [rules].

    If [l] is in the conclusions then call [rewriteC_tac]
    otherwise call [rewriteA_tac].
*)
end

val gen_rewrite_conv:
  Context.t -> Rewrite.control -> Logic.thm list -> Logic.conv
(** [rewrite_conv scp ctrl rules trm]: Rewrite term [trm] with
    theorems [rules] in scope [scp].

    Returns [ |- trm = X ]
    where [X] is the result of rewriting [trm]
*)

val rewrite_conv:
  Context.t -> Logic.thm list -> Logic.conv
(** Appiies [gen_rewrite_conv] with [Rewrite.default] *)

val gen_rewrite_rule:
  Context.t -> Rewrite.control -> Logic.thm list -> Logic.thm -> Logic.thm
(** [rewrite_rule scp ctrl rules thm]: Rewrite theorem [thm] with
    theorems [rules] in scope [scp].

    Returns [ |- X ] where [X] is the result of rewriting [thm]
*)

val rewrite_rule:
  Context.t -> Logic.thm list -> Logic.thm -> Logic.thm
(** Appiies [gen_rewrite_rule] with [Rewrite.default] *)

val gen_rewrite_asm_tac:
  Rewrite.control -> (Logic.label)option -> Logic.rr_type list
  -> Tactics.tactic
(** [gen_rewrite_asm_tac ctrl f rules]: General assumption rewriting tactic.

    Rewrite assumption [f] with list of theorems and assumptions given in
    [rules].

    If [f] is not given, rewrite all assumptions and in the
    sequent.
*)

val gen_rewrite_concl_tac:
  Rewrite.control -> (Logic.label)option -> Logic.rr_type list
  -> Tactics.tactic
(** [gen_rewrite_concl_tac ctrl f rules]: General conclusion rewriting tactic.

    Rewrite conclusion [f] with list of theorems and assumptions given in
   [rules].

    If [f] is not given, rewrite all conclusions in the sequent.  *)

val gen_rewrite_tac:
  Rewrite.control -> (Logic.label)option -> Logic.rr_type list
  -> Tactics.tactic
(** [gen_rewrite_tac ctrl rules f]: General rewriting tactic.

    Rewrite formula [f] with list of theorems and assumptions given in
   [rules].

   If [f] is not given, rewrite all assumptions and conclusions in in
   sequent. If [f] is not given then rewrite both assumptions and conclusions
   in the sequent.  *)

val rewrite_at:
  Logic.thm list -> Logic.label -> Tactics.tactic
val rewrite_tac:
  Logic.thm list -> Tactics.tactic
val once_rewrite_at:
  Logic.thm list -> Logic.label -> Tactics.tactic
val once_rewrite_tac:
  Logic.thm list -> Tactics.tactic
(**
    [rewrite_at thms f]: Rewrite formula [f] with theorems [thms].

    [rewrite_tac thms]:  Rewrite all formulas.

    [once_rewrite_at thms f]: Rewrite formula [f] with theorems [thms] once.

    [once_rewrite_tac thms]:  Rewrite all formulas.
*)

val rewriteC_tac: Logic.thm list -> Tactics.tactic
val rewriteC_at: Logic.thm list -> Logic.label -> Tactics.tactic
val once_rewriteC_tac: Logic.thm list -> Tactics.tactic
val once_rewriteC_at:
  Logic.thm list -> Logic.label -> Tactics.tactic
(**
    [rewriteC_at thms c]: Rewrite conclusion [c] with theorems [thms]

    [rewriteC_tac thms]:  Rewrite all conclusions

    [once_rewriteC_at thms c]: Rewrite conclusion [c] with theorems [thms] once

    [once_rewriteC_tac thms]:  Rewrite all conclusions once
*)

val rewriteA_tac: Logic.thm list -> Tactics.tactic
val rewriteA_at: Logic.thm list -> Logic.label -> Tactics.tactic
val once_rewriteA_tac: Logic.thm list -> Tactics.tactic
val once_rewriteA_at:
  Logic.thm list -> Logic.label -> Tactics.tactic
(**
    [rewriteA_at thms c]: Rewrite assumption [a] with theorems [thms]

    [rewriteA_tac thms]:  Rewrite all assumptions

    [once_rewriteA_at thms c]: Rewrite assumption [a] with theorems [thms] once

    [once_rewriteA_tac thms]:  Rewrite all assumptions once
*)

val gen_replace_tac:
  Rewrite.control -> Logic.label list
  -> (Logic.label)option -> Tactics.tactic
(** [gen_replace_tac ctrl asms f]: Rewrite formula [f] with the
    assumptions in list [asms].  If [f] is not given, rewrite all
    formulas in sequent. If [asms] is not given, use all assumptions of
    the form [l=r] or [!x1 .. xn: l = r]. Doesn't rewrite the
    assumptions used as rewrite rules.
*)

val replace_tac: Logic.label list -> Tactics.tactic
val replace_at: Logic.label list -> Logic.label -> Tactics.tactic
(**
   [replace_at asms f]: Rewrite formula [f] with assumptions in
   list [asms]

   [replace_tac asms]: Rewrite all formulas with assumptions in
   list [asms]

   If [asms] is empty, use all assumptions of the form [l=r] or [!x1
   .. xn: l = r].  Doesn't rewrite the used assumptions. *)

val replace_rl_tac: Logic.label list -> Tactics.tactic
val replace_rl_at: Logic.label list -> Logic.label -> Tactics.tactic
(** [replace_rl_at asms f]: Rewrite, right to left, formula [f] with
   assumptions in list [asms]

   [replace_rl_tac asms: Rewrite, right to left, all formulas with
   assumptions in list [asms]

   If [asms] is empty, use all assumptions of the form [l=r] or [!x1
   .. xn: l = r].  Doesn't rewrite the used assumptions. *)

val once_replace_tac: Logic.label list -> Tactics.tactic
val once_replace_at: Logic.label list -> Logic.label -> Tactics.tactic
(**
   [once_replace_at asms f]: Rewrite formula [f] with assumptions in
   list [asms] once.

   [once_replace_tac asms f]: Rewrite all formulas with assumptions in
   list [asms] once.

   If [asms] is empty, use all assumptions of the form [l=r] or [!x1
   .. xn: l = r].  Doesn't rewrite the used assumptions. *)


val unfold_at: string -> Logic.label -> Tactics.tactic
(** [unfold_at n f]: Unfold the definition of [n] at formula [?f].

    info: [aforms=[f'], cforms=[]] or [aforms=[], cforms=[f']]
    depending on whether [f] is in the assumptions or conclusions.
    [f'] is the tag of the formula resulting from rewriting.
*)

val unfold: string -> Tactics.tactic
(** [unfold_at f n]: Unfold the definition of [n] at formula [?f]. *)
