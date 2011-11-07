(*----
  Name: booltacs.mli
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

(** General support for boolean reasoning *)

(** {5 Boolean equivalence} *)

val iff_def: unit -> Logic.thm
(** Get the definition of [iff]. *)

val iffA: ?a:Logic.label -> Tactics.tactic
(** [iffA l sq]: Elminate the equivalance at assumption [l].

    {L
    g:\[(A iff B){_ l}, asms |- concl\]

    ---->

    g1:\[A{_ l1}, asms |- B{_ l2}, concl\]; 
    g2:\[B{_ l3}, asms |- A{_ l4}, concl\]; 
    }

    info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
*)

val iffC: ?c:Logic.label -> Tactics.tactic
(** [iffC l sq]: Elminate the equivalence at conclusion [l]

    {L
    g:\[asms |- (A iff B){_ l}, concl\]

    ---->

    g1:\[asms |- (A=>B){_ l}, concl\]; 
    g2:\[asms |- (B=>A){_ l}, concl\]; 
    }

    info: [goals = [g1; g2], aforms=[], cforms=[l], terms = []]
*)

val iffE: ?info:Tactics.Info.t -> ?c:Logic.label -> Tactics.tactic
(** [iffE l sq]: Fully elminate the equivalence at conclusion [l]

    {L
    g:\[asms |- (A iff B){_ l}, concl\]

    ---->

    g1:\[A{_ l1}, asms |- B{_ l2}, concl\]; 
    g2:\[B{_ l3}, asms |- A{_ l4}, concl\]; 
    }

    info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
*)

(** {5 Splitting subgoals}

    A subgoal formula constructed from an operator [op] {e split} is
    split if eliminating the operator results in more than one subgoal
    (or proves the subgoal).  For example, {!Tactics.conjC} [~c:f]
    will split formula [f].
*)

val split_asm_rules:
  (Logic.label -> Tactics.tactic) list
(** The rules used by {!Booltacs.split_tac} to split assumptions *)
val split_concl_rules:
  (Logic.label -> Tactics.tactic) list
(** The rules used by {!Booltacs.split_tac} to conclusions assumptions *)

val split_asms_tac: 
  ?info:Tactics.Info.t -> Logic.label -> Tactics.tactic
(** Eliminate operators in the assumptions which introduce new
    subgoals. Uses the same rules as {!Booltacs.split_tac}.
*)
val split_concls_tac: 
  ?info:Tactics.Info.t -> Logic.label -> Tactics.tactic
(** Eliminate operators in the conclusions which introduce new
    subgoals. Uses the same rules as {!Booltacs.split_tac}.
*)

val split_tac: 
  ?info:Tactics.Info.t -> ?f:Logic.label -> Tactics.tactic
(** Eliminate operators in the assumptions and conclusions which
    introduce new subgoals. Resulting tag information, in [?info], may
    contain duplicates.

    In the assumptions, eliminates [false], disjunction ([|]),
    implication ([=>]).

    In the conclusions, eliminates [true], conjunction ([&]).
*)

(** {5 Flattening subgoals}

    A subgoal formula constructed from an operator [op] is {e
    flattened} if eliminating the operator results in at most one
    subgoal (or proves the subgoal). For example, {!Tactics.conjA}
    [~a:f] flattens formal [f].
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
  ?info:Tactics.Info.t -> Logic.label -> Tactics.tactic
(** Eliminate operators in the assumptions which don't introduce new
    subgoals. Uses the same rules as {!Booltacs.flatten_tac}.
*)
val flatter_concls_tac: 
  ?info:Tactics.Info.t -> Logic.label -> Tactics.tactic
(** Eliminate operators in the conclusions which don't introduce new
    subgoals. Uses the same rules as {!Booltacs.flatten_tac}.
*)

val flatten_tac: 
  ?info:Tactics.Info.t -> ?f:Logic.label -> Tactics.tactic
(** Eliminate operators in the assumptions and conclusions which don't
    introduce new subgoals. Resulting tag information, in [?info], may
    contain duplicates.

    In the assumptions, eliminates [false], conjunction ([&]) and
    existential quantification ([?]).

    In the conclusions, eliminates [true], negation ([not]),
    disjunction ([|]), implication ([=>]), universal quantification
    ([!]).
    
    Doesn't eliminate negation in the assumptions (to avoid
    introducing trivial conclusions).
*)

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

val scatter_tac: 
  ?info:Tactics.Info.t -> ?f:Logic.label -> Tactics.tactic
(** Eliminate boolean operators in the assumptions and conclusions.

    In the assumptions, eliminates [false], negation ([not]),
    conjunction ([&]) and existential quantification ([?]),
    disjunction ([|]), implication ([=>]).

    In the conclusions, eliminates [true], negation ([not]),
    disjunction ([|]), implication ([=>]), universal quantification
    ([!]), conjunction ([&]) and boolean equivalence ([iff]).

    Resulting tag information, in [?info], may contain duplicates.
*)

val blast_asm_rules:
  (Logic.label -> Tactics.tactic) list
(** Rules used by {!Booltacs.blast_tac}.
*)
val blast_concl_rules:
  (Logic.label -> Tactics.tactic) list
(** Rules used by {!Booltacs.blast_tac}.
*)

val blast_tac: 
  ?info:Tactics.Info.t -> ?f:Logic.label -> Tactics.tactic
(** Eliminate boolean operators in the assumptions and conclusions
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

(** {5 Cases} *)

val cases_thm: unit -> Logic.thm
(** [cases_thm]: |- !P: (~P) | P *)

val cases_tac: ?info:Tactics.Info.t -> Basic.term -> Tactics.tactic
(** [cases_tac ?info x g]: Cases tactic.

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
  ?info:Tactics.Info.t
  -> Basic.term -> Tactics.tactic -> Tactics.tactic
(** [show_tac trm tac]: Use [tac] to show that [trm] is true,
    introducing [trm] as a new assumption. If [tac] fails to prove
    [trm], introduces [trm] as the conclusion of a new subgoal.
*)

val show: 
  ?info:Tactics.Info.t
  -> Basic.term -> Tactics.tactic -> Tactics.tactic
(** [show trm tac]: Use [tac] to show that [trm] is true, introducing
    [trm] as a new assumption. If [tac] fails to prove [trm],
    introduces [trm] as the conclusion of a new subgoal.

    {!Boollib.show} is a synonym for {!Boollib.show_tac}.
*)


val cases_of: 
  ?info:Tactics.Info.t 
  -> ?thm:Logic.thm -> Basic.term 
  -> Tactics.tactic
(** [cases_of ?info ?thm trm]: Try to introduce a case split based on
    the type of term [trm]. If [thm] is given, it is used as the cases
    theorem. If [thm] is not given, the theorem named ["T_cases"] is
    used, where [T] is the name of the type of [trm].
*)


(** {5 Modus Ponens} *)

val mp_tac: 
  ?info:Tactics.Info.t
  -> ?a:Logic.label -> ?h:Logic.label -> Tactics.tactic
(** [mp_tac ?a ?h]: Modus ponens.

    {L
    g:\[(A=>B){_ a}, A{_ h}, asms |- concls\]

    ---> 

    g:\[B{_ t}, A{_ h}, asms |- concls\]
    }

    info: [goals = [], aforms=[t], cforms=[], terms = []]

    If [a] is [! x1 .. xn: A => B] and [h] is [l], try to instantiate
    all of the [x1 .. xn] with values from [h] (found by unification).

    If [?a] is not given, each (possibly quantified) implication in
    the assumptions is tried, starting with the first. If [?h] is not
    given, the assumptions are searched for a suitable formula.
*)

val cut_mp_tac:
  ?info:Tactics.Info.t 
  -> ?inst:Basic.term list
  -> Logic.thm 
  -> ?a:Logic.label -> Tactics.tactic
(** [cut_mp_tac ?info ?inst ?a ]: Cut theorem for Modus ponens.

    {L
    g:\[A{_ a}, asms |- concls\]; thm: |- A => B

    ---> 

    g:\[B{_ t}, A{_ a}, asms |- concls\]
    }

    info: [goals = [], aforms=[t], cforms=[], terms = []]

    If [inst] is given, instantiate [thm] with the given terms.

    If [thm] is [! x1 .. xn: A => B] and [a1] is [l], try to
    instantiate all of the [x1 .. xn] with values from [a] (found by
    unification).

    If [?a] is not given, the first (possibly quantified) implication
    in the assumptions is used. If [?a1] is not given, the assumptions
    are searched for a suitable formula.
*)

val back_tac: 
  ?info:Tactics.Info.t 
  -> ?a:Logic.label -> ?c:Logic.label -> Tactics.tactic
(** [back_tac ~a ~c]: Match, backward tactic.

    {L
    g:\[(A=>B){_ a}, asms |- B{_ c}, concls\]

    ---> 

    g1:\[asms |- A{_ t}, concls\]
    }

    info: [goals = [g1], aforms=[], cforms=[t], terms = []]

    If [a] is [! x1 .. xn: A => B] and [a1] is [l], try to instantiate
    all of the [x1 .. xn] with values from [c] (found by unification).

    If [a] is not given, each (possibly quantified) implication in the
    assumptions is tried, starting with the first. If [c] is not
    given, the assumptions are searched for a suitable formula.
*)

val cut_back_tac:
  ?info:Tactics.Info.t -> ?inst:Basic.term list 
  -> Logic.thm -> ?c:Logic.label -> Tactics.tactic
(** [cut_back_tac ?inst thm ~c]: Match, backward tactic.

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

    If [c] is not given, the assumptions are searched for a suitable
    formula.
*)
