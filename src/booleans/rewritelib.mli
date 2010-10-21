(*----
  Name: rewritelib.mli
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

  val rewrite_conv: 
    ?ctrl:Rewrite.control -> Logic.rr_type list -> Logic.conv
  (** [rewrite_conv scp ctrl rules trm]: rewrite term [trm] with rules
      [rrl] in scope [scp].

      Returns |- trm = X 
      where [X] is the result of rewriting [trm]

      Discards any rule which is not a theorem or an ordered theorem.

      This conversion could be written using the rewriting tactics but
      this would require two sets of rewriting. The first to construct
      the term [X] on the rhs of the equality and the second when the
      rewrite tactic is invoked. By contrast, [rewrite_conv] only does
      one set of rewriting.  *)

  val rewrite_rule:
    Scope.t 
    -> ?ctrl:Rewrite.control 
    -> Logic.rr_type list 
    -> Logic.thm -> Logic.thm
  (** [rewrite_rule scp ctrl rules thm]: rewrite theorem [thm] with
      rules [rrl] in scope [scp].

      Returns |- X where [X] is the result of rewriting [thm].
  *)

  (** {7 Tactics} *)

  val map_sym_tac:
    (Tactics.rule list) ref -> Tactics.rule list -> Tactics.tactic
  (** [map_sym_tac ret rules goal]: Apply [eq_sym] to each rule in
      [rules], returning the resulting list in [ret]. The list in
      [ret] will be in reverse order of [rules].

      Used to set assumptions and theorems in the right form for
      right-left rewriting.
  *)
  val rewriteA_tac: 
    ?info:Tactics.Info.t
    -> ?ctrl:Rewrite.control
    -> Tactics.rule list -> Logic.label -> Tactics.tactic
  (** [rewriteA_tac ctrl rules l]: Rewrite the assumption at label [l]
      with [rules], passing [ctrl] to the rewriter.

      {L
      A{_ l}, asms |- concls

      ----> (B is the rewritten assumption)

      B{_ l}, asms |- concls
      }

      info: [goals = [], aforms=[l], cforms=[], terms = []]
  *)

  val rewriteC_tac: 
    ?info:Tactics.Info.t
    -> ?ctrl:Rewrite.control
    -> Tactics.rule list -> Logic.label -> Tactics.tactic
  (** [rewriteC_tac ctrl rules l]: Rewrite the conclusion at label [l]
      with [rules], passing [ctrl] to the rewriter.

      {L
      asms |- A{_ l}, concls

      ----> (B is the rewritten conclusion)

      asms |- B{_ l}, concls
      }

      info: [goals = [], aforms=[], cforms=[l], terms = []]
  *)
    
  val rewrite_tac: 
    ?info:Tactics.Info.t
    -> ?ctrl:Rewrite.control
    -> Tactics.rule list -> Logic.label -> Tactics.tactic
(** [rewrite_tac ?info ctrl rules l sq]: Rewrite formula [l] with
    [rules].
    
    If [l] is in the conclusions then call [rewriteC_tac]
    otherwise call [rewriteA_tac].
*)

end


val rewrite_conv: 
  ?ctrl:Rewrite.control -> Logic.thm list -> Logic.conv
(** [rewrite_conv scp ctrl rules trm]: Rewrite term [trm] with
    theorems [rules] in scope [scp].

    Returns [ |- trm = X ]
    where [X] is the result of rewriting [trm]
*)

val rewrite_rule:
  Scope.t 
  -> ?ctrl:Rewrite.control -> Logic.thm list 
  -> Logic.thm -> Logic.thm
(** [rewrite_rule scp ctrl rules thm]: Rewrite theorem [thm] with
    theorems [rules] in scope [scp].

    Returns [ |- X ] where [X] is the result of rewriting [thm]
*)

val gen_rewrite_tac: 
  ?info: Tactics.Info.t 
  -> ?asm:bool
  -> Rewrite.control
  -> ?f:Logic.label 
  -> Logic.rr_type list 
  -> Logic.tactic
(** [gen_rewrite_tac ?info ?asm ctrl rules f]: General rewriting
    tactic.

    Rewrite formula [f] with list of theorems and assumptions given in
    [rules]. 

    If [f] is not given, rewrite all assumptions and conclusions in in
    sequent. If [f] is not given and [asm] is given then if [asm] is
    true, rewrite only the assumptions, if [asm] is false then rewrite
    only the conclusions. If neither [f] nor [asm] is given, the
    rewrite both assumptions and conclusions in the sequent.
*)

val rewrite_tac: 
  ?info:Tactics.Info.t 
  -> ?dir:Rewrite.direction
  -> ?f:Logic.label
  -> Logic.thm list 
  -> Logic.tactic
(** [rewrite_tac info dir f thms]: Rewrite formula [f] with list of
    theorems [thms]. If [f] is not given, rewrite all formulas in
    sequent.

    [dir = leftright] by default.
*)

val once_rewrite_tac: 
  ?info:Tactics.Info.t -> ?dir:Rewrite.direction -> 
  ?f:Logic.label -> Logic.thm list -> Logic.tactic
(** [once_rewrite_tac info dir f thms]: Rewrite formula [f] once.  If
    [f] is not given, rewrite all formulas in sequent.

    [dir = leftright] by default.
*)

val rewriteC_tac: 
  ?info:Tactics.Info.t 
  -> ?dir:Rewrite.direction
  -> ?c:Logic.label
  -> Logic.thm list 
  -> Logic.tactic
(** [rewriteC_tac info dir c thms]: Rewrite conclusion [c] with list
    of theorems [thms]. If [c] is not given, rewrite all conclusions
    in sequent.

    [dir = leftright] by default.
*)

val once_rewriteC_tac: 
  ?info:Tactics.Info.t -> ?dir:Rewrite.direction -> 
  ?c:Logic.label -> Logic.thm list -> Logic.tactic
(** [once_rewrite_tac info dir c thms]: Rewrite conclusion [c] once.
    If [c] is not given, rewrite all conclusions in sequent.

    [dir = leftright] by default.
*)

val rewriteA_tac: 
  ?info:Tactics.Info.t 
  -> ?dir:Rewrite.direction
  -> ?a:Logic.label
  -> Logic.thm list 
  -> Logic.tactic
(** [rewrite_tac info dir a thms]: Rewrite assumption [a] with list of
    theorems [thms]. If [a] is not given, rewrite all assumptions in
    sequent. 

    [dir = leftright] by default.
*)

val once_rewriteA_tac: 
  ?info:Tactics.Info.t -> ?dir:Rewrite.direction -> 
  ?a:Logic.label -> Logic.thm list -> Logic.tactic
(** [once_rewrite_tac info dir a thms]: Rewrite assumption [a] once.
    If [a] is not given, rewrite all assumptions in sequent.

    [dir = leftright] by default.
*)


val gen_replace_tac: 
  ?info:Tactics.Info.t -> ?ctrl:Rewrite.control
  -> ?asms:Logic.label list 
  -> ?f:Logic.label -> Logic.tactic
(** [gen_replace_tac info ctrl asms f]: Rewrite formula [f] with the
    assumptions in list [asms].  If [f] is not given, rewrite all
    formulas in sequent. If [asms] is not given, use all assumptions of
    the form [l=r] or [!x1 .. xn: l = r]. Doesn't rewrite the
    assumptions used as rewrite rules.
*)

val replace_tac: 
  ?info:Tactics.Info.t -> ?dir:Rewrite.direction
  -> ?asms:Logic.label list 
  -> ?f:Logic.label -> Logic.tactic
(** [replace_tac info dir asms f]: Rewrite formula [f] with
    assumptions in list [asms].  If [f] is not given, rewrite all
    formulas in sequent.  If [asms] is not given, use all assumptions
    of the form [l=r] or [!x1 .. xn: l = r].  Doesn't rewrite the used
    assumptions.

    [dir = leftright] by default.
*)

val once_replace_tac: 
  ?info:Tactics.Info.t -> ?dir:Rewrite.direction
  -> ?asms:Logic.label list 
  -> ?f:Logic.label -> Logic.tactic
(** [once_replace_tac info dir asms f]: Rewrite formula [f] with
    assumptions in list [asms] once. If [f] is not given, rewrite all
    formulas in sequent.  If [asms] is not given, use all assumptions
    of the form [l=r] or [!x1 .. xn: l = r].  Doesn't rewrite the used
    assumptions.

    [dir = leftright] by default.
*)


val unfold: 
  ?info:Tactics.Info.t -> ?f:Logic.label -> string -> Tactics.tactic
(** [unfold ?f n]: Unfold the definition of [n] at formula [?f].

    info: [aforms=[f'], cforms=[]] or [aforms=[], cforms=[f']]
    depending on whether [f] is in the assumptions or conclusions.
    [f'] is the tag of the formula resulting from rewriting.
*)
