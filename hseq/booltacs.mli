(*----
  Copyright (c) 2006-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** General support for boolean reasoning *)

(** {5 Boolean equivalence} *)

val iff_def: Context.t -> Logic.thm
(** Get the definition of [iff]. *)

val iffA_at: Logic.label -> Tactics.tactic
val iffA: Tactics.tactic
(** [iffA l sq]: Elminate the equivalance at assumption [l].

    {L
    g:\[(A iff B){_ l}, asms |- concl\]

    ---->

    g1:\[A{_ l1}, asms |- B{_ l2}, concl\];
    g2:\[B{_ l3}, asms |- A{_ l4}, concl\];
    }

    info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
*)

val iffC_at: Logic.label -> Tactics.tactic
val iffC: Tactics.tactic
(** [iffC l sq]: Elminate the equivalence at conclusion [l]

    {L
    g:\[asms |- (A iff B){_ l}, concl\]

    ---->

    g1:\[asms |- (A=>B){_ l}, concl\];
    g2:\[asms |- (B=>A){_ l}, concl\];
    }

    info: [goals = [g1; g2], aforms=[], cforms=[l], terms = []]
*)

val iffE_at: Logic.label -> Tactics.tactic
(** [iffE l sq]: Fully elminate the equivalence at conclusion [l]

    {L
    g:\[asms |- (A iff B){_ l}, concl\]

    ---->

    g1:\[A{_ l1}, asms |- B{_ l2}, concl\];
    g2:\[B{_ l3}, asms |- A{_ l4}, concl\];
    }

    info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
*)

val iffE: Tactics.tactic
(** Applies [iffE] to the first [iff] forumula in the conclusions *)

(** {5 Splitting subgoals}

    A subgoal formula constructed from an operator [op] {e split} is
    split if eliminating the operator results in more than one subgoal
    (or proves the subgoal).
*)

val split_asm_rules:
  (Logic.label -> Tactics.tactic) list
(** The rules used by {!Booltacs.split_tac} to split assumptions *)
val split_concl_rules:
  (Logic.label -> Tactics.tactic) list
(** The rules used by {!Booltacs.split_tac} to conclusions assumptions *)

val split_asms_tac:
  Logic.label -> Tactics.tactic
(** Eliminate operators in the assumptions which introduce new
    subgoals. Uses the same rules as {!Booltacs.split_tac}.
*)
val split_concls_tac:
  Logic.label -> Tactics.tactic
(** Eliminate operators in the conclusions which introduce new
    subgoals. Uses the same rules as {!Booltacs.split_tac}.
*)

val split_at: Logic.label -> Tactics.tactic
(** Eliminate operators in the assumptions and conclusions which
    introduce new subgoals. Resulting tag information may contain
    duplicates.

    In the assumptions, eliminates [false], disjunction ([|]),
    implication ([=>]).

    In the conclusions, eliminates [true], conjunction ([&]).
*)

val split_tac: Tactics.tactic
(** Apply {!split_at} to all formulas in a sequent *)

(** {5 Flattening subgoals}

    A subgoal formula constructed from an operator [op] is {e
    flattened} if eliminating the operator results in at most one
    subgoal (or proves the subgoal).
*)

val flatter_asm_rules:
  (Logic.label -> Tactics.tactic) list
(** The rules used by {!Booltacs.flatten_tac} to flatten assumptions.
*)
val flatter_concl_rules:
  (Logic.label -> Tactics.tactic) list
(** The rules used by {!Booltacs.flatten_tac} to flatten conclusions.
*)

val flatter_asms_tac:
  Logic.label -> Tactics.tactic
(** Eliminate operators in the assumptions which don't introduce new
    subgoals. Uses the same rules as {!Booltacs.flatten_tac}.
*)
val flatter_concls_tac:
  Logic.label -> Tactics.tactic
(** Eliminate operators in the conclusions which don't introduce new
    subgoals. Uses the same rules as {!Booltacs.flatten_tac}.
*)

val flatten_at: Logic.label -> Tactics.tactic
(** Eliminate operators in a formula which don't
    introduce new subgoals. Resulting tag information may contain
    duplicates.

    In the assumptions, eliminates [false], conjunction ([&]) and
    existential quantification ([?]).

    In the conclusions, eliminates [true], negation ([not]),
    disjunction ([|]), implication ([=>]), universal quantification
    ([!]).

    Doesn't eliminate negation in the assumptions (to avoid
    introducing trivial conclusions).
*)

val flatten_tac: Tactics.tactic
(** Apply {!flatten_at} to all formulas in a sequent *)

(** {5 Scatter subgoals}

    Split and flatten subgoals.
*)

val scatter_asm_rules:
  (Logic.label -> Tactics.tactic) list
(** Rules used by {!Booltacs.scatter_tac} to scatter assumptions.
*)
val scatter_concl_rules:
  (Logic.label -> Tactics.tactic) list
(** Rules used by {!Booltacs.scatter_tac} to scatter conclusions.
*)

val scatter_at: Logic.label -> Tactics.tactic
(** Eliminate boolean operators in a formulat

    In the assumptions, eliminates [false], negation ([not]),
    conjunction ([&]) and existential quantification ([?]),
    disjunction ([|]), implication ([=>]).

    In the conclusions, eliminates [true], negation ([not]),
    disjunction ([|]), implication ([=>]), universal quantification
    ([!]), conjunction ([&]) and boolean equivalence ([iff]).

    Resulting tag information may contain duplicates.
*)

val scatter_tac: Tactics.tactic
(** Apply {!scatter_at} to all formulas in a sequent *)

val blast_asm_rules:
  (Logic.label -> Tactics.tactic) list
(** Rules used by {!Booltacs.blast_tac}. *)
val blast_concl_rules:
  (Logic.label -> Tactics.tactic) list
(** Rules used by {!Booltacs.blast_tac}. *)

val blast_at: Logic.label -> Tactics.tactic
(** Eliminate boolean operators in a formula
    then try to solve subgoals.

    In the assumptions, eliminates [false], negation ([not]),
    conjunction ([&]) and existential quantification ([?]),
    disjunction ([|]), implication ([=>]) then calls {!Tactics.basic}.

    In the conclusions, eliminates [true], negation ([not]),
    disjunction ([|]), implication ([=>]), universal quantification
    ([!]), conjunction ([&]) and boolean equivalence ([iff]) then
    calls {!Tactics.basic}.

    This is like {!Booltacs.scatter_tac}, followed by {!Tactics.basic}.
*)

val blast_tac: Tactics.tactic
(** Apply {!blast_at} to all formulas in a sequent *)

(** {5 Cases} *)

val cases_thm: Context.t -> Logic.thm
(** [cases_thm]: |- !P: (~P) | P *)

val cases_tac: Term.term -> Tactics.tactic
(** [cases_tac x g]: Cases tactic.

    Add formula [x] to assumptions of [g] and create new subgoal in
    which to prove [x].

    {L
    g:\[asms |- concls\]

    --->

    g1:\[asms |- x{_ t}, concls\]; g2:\[x{_ t}, asms |- concls\]
    }

    info: [goals = [g1; g2], aforms=[t], cforms=[t], terms = []]
*)

val show_tac:
  Term.term -> Tactics.tactic -> Tactics.tactic
(** [show_tac trm tac]: Use [tac] to show that [trm] is true,
    introducing [trm] as a new assumption. If [tac] fails to prove
    [trm], introduces [trm] as the conclusion of a new subgoal.
*)

val show:
  Term.term -> Tactics.tactic -> Tactics.tactic
(** [show trm tac]: Use [tac] to show that [trm] is true, introducing
    [trm] as a new assumption. If [tac] fails to prove [trm],
    introduces [trm] as the conclusion of a new subgoal.

    {!Boollib.show} is a synonym for {!Boollib.show_tac}.
*)

val cases_of: Term.term -> Tactics.tactic
(** [cases_of trm]: Try to introduce a case split based on the type of
    term [trm]. The theorem named ["T_cases"] is used as the cases theorem,
    where [T] is the name of the type of [trm].  *)

val cases_with: Logic.thm -> Term.term -> Tactics.tactic
(** Apply {!cases_of} wih the given theorem. *)


(** {5 Modus Ponens} *)

val mp_search: (Logic.label)option -> (Logic.label)option -> Tactics.tactic
val mp_at: Logic.label -> Logic.label -> Tactics.tactic
val mp_tac: Tactics.tactic
(** Modus ponens

    {L g:\[(A=>B){_ a}, A{_ h}, asms |- concls\]

    --->

    g:\[B{_ t}, A{_ h}, asms |- concls\] }

    info: [goals = [], aforms=[t], cforms=[], terms = []]

    [mp_at a h]: If [a] is [! x1 .. xn: A => B] and [h] is [l], try to
   instantiate all of the [x1 .. xn] with values from [h] (found by
   unification).

   [mp_tac] Each (possibly quantified) implication in the assumptions
   is tried, starting with the first and the assumptions are searched
   for a suitable formula.

   [mp_search a h] If either [a] or [h] is None then search for suitable
   assumpitons to use. If either is given, then use it
 *)

val cut_mp_at: Term.term list-> Logic.thm -> Logic.label -> Tactics.tactic
val cut_mp_tac: Term.term list-> Logic.thm -> Tactics.tactic
(** Cut theorem for Modus ponens.

    {L g:\[A{_ a}, asms |- concls\]; thm: |- A => B

    --->

    g:\[B{_ t}, A{_ a}, asms |- concls\] }

    info: [goals = [], aforms=[t], cforms=[], terms = []]

    If [inst] is given, instantiate [thm] with the given terms.

    If [thm] is [! x1 .. xn: A => B] and [a1] is [l], try to instantiate
   all of the [x1 .. xn] with values from [a] (found by unification).

    [cut_mp_at inst a]: Apply modus-ponens to assumption [a].

    [cut_mp_tac inst a]: Apply modus-ponens to the first (possibly
   quantified) suitable assumption.  *)

val back_search: (Logic.label)option -> (Logic.label)option -> Tactics.tactic
val back_at: Logic.label -> Logic.label -> Tactics.tactic
val back_tac: Tactics.tactic
(** Match, backward tactic.

    {L g:\[(A=>B){_ a}, asms |- B{_ c}, concls\]

    --->

    g1:\[asms |- A{_ t}, concls\] }

    info: [goals = [g1], aforms=[], cforms=[t], terms = []]

    [back_at a c] If [a] is [! x1 .. xn: A => B] and [a1] is [l], try to
   instantiate all of the [x1 .. xn] with values from [c] (found by
   unification).

    [back_tac] Each (possibly quantified) implication in the assumptions
   is tried with each of the conclusions

    [back_search a c] If [a] is not given then try each of the
   assumptions, starting with the first. If [c] is not given then try
   the assumption against each of the conclusions, starting with the
   first.
 *)

val cut_back_at: Term.term list -> Logic.thm -> Logic.label -> Tactics.tactic
val cut_back_tac: Term.term list -> Logic.thm -> Tactics.tactic
(** Match, backward tactic.

    {L
    g:\[asms |- B{_ c}, concls\]; thm: |- A => B

    --->

    g1:\[asms |- A{_ t}, concls\]
    }

    info: [goals = [g1], aforms=[], cforms=[t], terms = []]

    If [inst] is given, instantiate [thm] with the given terms.

    If [thm] is [! x1 .. xn: A => B] and [a1] is [l], try to
    instantiate all of the [x1 .. xn] with values from [c] (found by
    unification).

    [cut_back_at inst thm c]: Use assumption [c]

    [cut_back_tac inst thm]: The assumptions are searched for a suitable
    formula.
*)
