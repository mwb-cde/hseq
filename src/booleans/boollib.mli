(*----
  Name: boollib.mli
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

(** Support for boolean proofs.

    Many (but not all) of the values here are front-ends to
    values defined elsewhere. 
*)


(** {5 Theorems, Rules and Conversions} *)

module Thms:
sig
  (** Theorems used by tactics. A theorem [n] is accessed by
      calling function [n_thm()]. This returns the theorem,
      proving it if necessary. If [n_thm()] has to prove the
      theorem, it stores the result so that subsequent calls to
      [n_thm()] do not have to carry out the proof again. Some
      theorems have a function [make_n_thm()] which actually
      carries out the proof.  *)

  val false_def: unit -> Logic.thm
  (** The definition of [false]. *)
  val iff_def: unit -> Logic.thm
  (** The definition of [iff]. *)

  (** [iff_equals_thm]: |- !x y: (x iff y) = (x = y) *)
  val make_iff_equals_thm: unit -> Logic.thm
  val iff_equals_thm: unit -> Logic.thm

  (** [equals_iff_thm]: |- !x y: (x = y) = (x iff y) *)
  val equals_iff_thm: unit -> Logic.thm


  (** [bool_eq_thm]: |- !x y: x iff y = ((x => y) and (y=>x)) *)
  val make_bool_eq_thm: unit -> Logic.thm
  val bool_eq_thm: unit -> Logic.thm

  (** [double_not_thm]: |- ! x: x = (not (not x)) *)
  val make_double_not_thm: unit -> Logic.thm
  val double_not_thm: unit -> Logic.thm

  (** [rule_true_thm]: |- !x: x = (x=true) *)
  val make_rule_true_thm: unit -> Logic.thm
  val rule_true_thm: unit -> Logic.thm

  (** rule_false_thm: !x: (not x) = (x=false) *)
  val make_rule_false_thm: unit -> Logic.thm
  val rule_false_thm: unit -> Logic.thm

  val bool_cases_thm: unit -> Logic.thm
  (** [bool_cases_thm]: [! (x:bool): (x=true) | (x=false)] *)

  val cases_thm: unit -> Logic.thm
  (** [cases_thm]: |- !P: (~P) | P *)

  val eq_refl_thm: unit -> Logic.thm
  (** [eql_refl]: [!x: (x = x)] *)

  val eq_sym_thm: unit -> Logic.thm
(** [eql_sym]: [!x y: (x = y) = (y = x)] *)

end

module Rules:
sig
  (** Functions to construct theorems from other theorems.  *)

  val once_rewrite_rule:
    Scope.t -> Logic.thm list -> Logic.thm -> Logic.thm
  (** [once_rewrite_rule scp rules thm]: Rewrite [thm] with [rules]
      once.  *)

  val conjunctL: Scope.t -> Logic.thm -> Logic.thm 
  (** [conjunctL scp thm]: Get the left hand side of conjunct [thm].
      [conjunctL scp << l and r >> = l] *)

  val conjunctR: Scope.t -> Logic.thm -> Logic.thm 
  (** [conjunctR scp thm]: Get the right hand side of conjunct [thm].
      [conjunctL scp << l and r >> = r] *)

  val conjuncts: Scope.t -> Logic.thm -> Logic.thm list 
(** [conjuncts scp thm]: Break theorem [thm] into the list of
    conjuncts.  [conjuncts scp << f1 and f2 and .. and fn>> = [f1; f2;
    ..; fn]]
*)

end

module Convs:
sig
  (** Conversions on boolean operators.  *)

  (** [neg_all_conv]: |- (not (!x..y: a)) = ?x..y: not a *)
  val neg_all_conv: Scope.t -> Basic.term -> Logic.thm

  (** [neg_exists_conv]: |- (not (?x..y: a)) = !x..y: not a *)
  val neg_exists_conv: Scope.t -> Basic.term -> Logic.thm

end

(** {5 Tactics} *)

val falseA: ?info:Logic.info -> ?a:Logic.label -> Tactics.tactic
(** Solve a goal of the form \[ false{_ a}, A |- C \].  [info] is
    unchanged.
*)

val trivial: ?info:Logic.info -> ?f:Logic.label -> Tactics.tactic
(** Solve a goal of the form \[ false{_ f}, A |- C \] or \[ A |-
    true{_ f}, C \].  [info] is unchanged.
*)

val cut_thm: 
  ?info:Logic.info -> ?inst:Basic.term list 
  -> string -> Tactics.tactic
(** Cut a named theorem, with optional instantiation. *)

(** {7 Basic equality reasoning} *)

val eq_sym_rule: Scope.t -> Logic.thm -> Logic.thm
(** [eq_sym_rule scp thm]: If the body of [thm] is [ |- x = y], return
    [ |- y=x ].
*)

val eq_symA: ?info:Logic.info -> Logic.label -> Tactics.tactic
(** [eq_symA a]: Rewrite assumption [a] with [eq_sym_thm] once.
*)

val eq_symC: ?info:Logic.info -> Logic.label -> Tactics.tactic
(** [eq_symA a]: Rewrite conclusion [c] with [eq_sym_thm] once.
*)

val eq_sym_tac: ?info:Logic.info -> Logic.label -> Tactics.tactic
(** [eq_sym_tac f]: Try to apply [eq_symA f], if that fails, try
    [eq_symC f].
*)

val eq_tac:  ?info:Logic.info -> ?c:Logic.label -> Tactics.tactic
(** Prove goals of the form \[A|- x=x{_ c}, C\].  [info] is unchanged.
*)

(** {5 Generalised Rewriting}

    Tactics, conversions and rules for rewriting with a list of
    theorems and assumptions. Combines rewrite planners and rewriting
    and allows the direction of rewriting (left-right/right-left) to
    be specified.
*)

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
  ?info: Logic.info 
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
  ?info:Logic.info 
  -> ?dir:Rewrite.direction
  -> ?f:Logic.label
  -> Logic.thm list 
  -> Logic.tactic
(** [rewrite_tac info dir f thms]: Rewrite formula [f] with list of
    theorems [thms]. If [f] is not given, rewrite all formulas in
    sequent. [dir=leftright] by default.
*)

val once_rewrite_tac: 
  ?info:Logic.info -> ?dir:Rewrite.direction -> 
  ?f:Logic.label -> Logic.thm list -> Logic.tactic
(** [once_rewrite_tac info dir f thms]: Rewrite formula [f] once.  If
    [f] is not given, rewrite all formulas in sequent.
    [dir=leftright] by default.
*)

val rewriteC_tac: 
  ?info:Logic.info 
  -> ?dir:Rewrite.direction
  -> ?c:Logic.label
  -> Logic.thm list 
  -> Logic.tactic
(** [rewriteC_tac info dir c thms]: Rewrite conclusion [c] with list
    of theorems [thms]. If [c] is not given, rewrite all conclusions
    in sequent. [dir=leftright] by default.
*)

val once_rewriteC_tac: 
  ?info:Logic.info -> ?dir:Rewrite.direction -> 
  ?c:Logic.label -> Logic.thm list -> Logic.tactic
(** [once_rewrite_tac info dir c thms]: Rewrite conclusion [c] once.
    If [c] is not given, rewrite all conclusions in sequent.
    [dir=leftright] by default.
*)

val rewriteA_tac: 
  ?info:Logic.info 
  -> ?dir:Rewrite.direction
  -> ?a:Logic.label
  -> Logic.thm list 
  -> Logic.tactic
(** [rewriteA_tac info dir a thms]: Rewrite assumption [a] with list
    of theorems [thms]. If [a] is not given, rewrite all assumptions
    in sequent.  [dir=leftright] by default.
*)

val once_rewriteA_tac: 
  ?info:Logic.info -> ?dir:Rewrite.direction -> 
  ?a:Logic.label -> Logic.thm list -> Logic.tactic
(** [once_rewrite_tac info dir a thms]: Rewrite assumption [a] once.
    If [a] is not given, rewrite all assumptions in sequent.
    [dir=leftright] by default.
*)


val gen_replace_tac: 
  ?info:Logic.info -> ?ctrl:Rewrite.control
  -> ?asms:Logic.label list 
  -> ?f:Logic.label -> Logic.tactic
(** [gen_replace_tac info ctrl asms f]: Rewrite formula [f] with the
    assumptions in list [asms].  If [f] is not given, rewrite all
    formulas in sequent. If [asms] is not given, use all assumptions of
    the form [l=r] or [!x1 .. xn: l = r]. Doesn't rewrite the
    assumptions used as rewrite rules.
*)

val replace_tac: 
  ?info:Logic.info -> ?dir:Rewrite.direction
  -> ?asms:Logic.label list 
  -> ?f:Logic.label -> Logic.tactic
(** [replace_tac info dir asms f]: Rewrite formula [f] with
    assumptions in list [asms].  If [f] is not given, rewrite all
    formulas in sequent.  If [asms] is not given, use all assumptions
    of the form [l=r] or [!x1 .. xn: l = r].  Doesn't rewrite the used
    assumptions.  [dir=leftright] by default.
*)

val once_replace_tac: 
  ?info:Logic.info -> ?dir:Rewrite.direction
  -> ?asms:Logic.label list 
  -> ?f:Logic.label -> Logic.tactic
(** [once_replace_tac info dir asms f]: Rewrite formula [f] with
    assumptions in list [asms] once. If [f] is not given, rewrite all
    formulas in sequent.  If [asms] is not given, use all assumptions
    of the form [l=r] or [!x1 .. xn: l = r].  Doesn't rewrite the used
    assumptions.  [dir=leftright] by default.
*)


val unfold: 
  ?info:Logic.info -> ?f:Logic.label -> string -> Tactics.tactic
(** [unfold ?f n]: Unfold the definition of [n] at formula [?f].

    info: [aforms=[f'], cforms=[]] or [aforms=[], cforms=[f']]
    depending on whether [f] is in the assumptions or conclusions.
    [f'] is the tag of the formula resulting from rewriting.
*)

(** {7 Boolean equivalence} *)

val iffA: ?info:Logic.info -> ?a:Logic.label -> Tactics.tactic
(** [iffA l sq]: Elminate the equivalance at assumption [l].

    {L
    g:\[(A iff B){_ l}, asms |- concl\]

    ---->

    g1:\[A{_ l1}, asms |- B{_ l2}, concl\]; 
    g2:\[B{_ l3}, asms |- A{_ l4}, concl\]; 
    }

    info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
*)

val iffC: ?info:Logic.info -> ?c:Logic.label -> Tactics.tactic
(** [iffC l sq]: Elminate the equivalence at conclusion [l]

    {L
    g:\[asms |- (A iff B){_ l}, concl\]

    ---->

    g1:\[asms |- (A=>B){_ l}, concl\]; 
    g2:\[asms |- (B=>A){_ l}, concl\]; 
    }

    info: [goals = [g1; g2], aforms=[], cforms=[l], terms = []]
*)

val iffE: ?info:Logic.info -> ?c:Logic.label -> Tactics.tactic
(** [iffE l sq]: Fully elminate the equivalence at conclusion [l]

    {L
    g:\[asms |- (A iff B){_ l}, concl\]

    ---->

    g1:\[A{_ l1}, asms |- B{_ l2}, concl\]; 
    g2:\[B{_ l3}, asms |- A{_ l4}, concl\]; 
    }

    info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
*)

(**  {5 Eliminating boolean operators}  *)

val direct_alt: 
  (Logic.info -> Logic.label -> Tactics.tactic) list 
  ->  Logic.info -> Logic.label -> Tactics.tactic
(** [direct_alt tacs info l]: Directed alt. Like {!Tactics.alt} but
    pass [info] and [l] to each tactic in [tacs].  **)

val direct_map_some: 
  (Logic.label -> Tactics.tactic)
  -> Logic.label list ref -> Logic.label list -> Tactics.tactic
(** [direct_map_some tac lst l]: Directed map_some. Like
    {!Tactics.map_some} but pass [info] and [l] to [tac]. If [tac]
    fails for [l], then [lst:=l::!lst].  **)

val asm_elim_rules_tac:
  ?info:Logic.info 
  -> ((Logic.info -> Logic.label -> Tactics.tactic) list
      * (Logic.info -> Logic.label -> Tactics.tactic) list)
  -> Logic.label
  -> Tactics.tactic
(** [asm_elim_rules ?info (arules, crules) f goal]: Apply elimination
    rules to assumption [f] and to all resulting assumptions and
    conclusions. Assumptions are eliminated with [arules], conclusions
    with [crules]. Any new tag which can't be eliminated are stored in
    [?info] (in arbitrary order and may contain duplicates).
*)

val concl_elim_rules_tac:
  ?info:Logic.info 
  -> ((Logic.info -> Logic.label -> Tactics.tactic) list
      * (Logic.info -> Logic.label -> Tactics.tactic) list)
  -> Logic.label
  -> Tactics.tactic
(** [concl_elim_rules ?info (arules, crules) f goal]: Apply
    elimination rules to conclusion [f] and to all resulting
    assumptions and conclusions. Assumptions are eliminated with
    [arules], conclusions with [crules]. The tag of any new formula
    for which the elimination rules fails is stored in [?info] (in
    arbitrary order and may contain duplicates).
*)


val elim_rules_tac:
  ?info:Logic.info 
  -> ((Logic.info -> Logic.label -> Tactics.tactic) list
      * (Logic.info -> Logic.label -> Tactics.tactic) list)
  -> Logic.label list -> Logic.label list
  -> Tactics.tactic
(** [elim_rules_tac ?info (arules, crules) albls clbls]: Apply
    elimination rules to all assumptions with a label in [albls] and
    all conclusions with a label in [clbls] and with to all resulting
    assumptions and conclusions. The tag of any new formula for which
    the elimination rules fails is stored in [?info] (in arbitrary
    order and may contain duplicates).
*)

val apply_elim_tac:
  (?info:Logic.info 
   -> Logic.label list -> Logic.label list
   -> Tactics.tactic)
  -> ?info:Logic.info 
  -> ?f:Logic.label
  -> Tactics.tactic
(** [apply_elim_tac tac ?info ?f]: Apply elimination tactic [tac] to
    formula [?f]. If [?f] is not given, use all formulas in the
    sequent. The tag of any new formula for which the elimination rules
    fails is stored in [?info] (in arbitrary order and may contain
    duplicates).

    [apply_elim_tac] is intended to be used to wrap
    {!Boollib.elim_rules_tac}.
*)

(** {7 Splitting subgoals}

    A subgoal formula constructed from an operator [op] {e split} is
    split if eliminating the operator results in more than one subgoal
    (or proves the subgoal).  For example, {!Tactics.conjC} [~c:f]
    will split formula [f].
*)

val split_asms_tac: 
  ?info:Logic.info -> Logic.label -> Tactics.tactic
(** Eliminate operators in the assumptions which introduce new
    subgoals. Uses the same rules as {!Boollib.split_tac}.
*)
val split_concls_tac: 
  ?info:Logic.info -> Logic.label -> Tactics.tactic
(** Eliminate operators in the conclusions which introduce new
    subgoals. Uses the same rules as {!Boollib.split_tac}.
*)

val split_tac: 
  ?info:Logic.info -> ?f:Logic.label -> Tactics.tactic
(** Eliminate operators in the assumptions and conclusions which
    introduce new subgoals. Resulting tag information, in [?info], may
    contain duplicates.

    In the assumptions, eliminates [false], disjunction ([|]),
    implication ([=>]).

    In the conclusions, eliminates [true], conjunction ([&]).
*)

(** {7 Flattening subgoals}

    A subgoal formula constructed from an operator [op] is {e
    flattened} if eliminating the operator results in at most one
    subgoal (or proves the subgoal). For example, {!Tactics.conjA}
    [~a:f] flattens formal [f].
*)

val flatter_asms_tac: 
  ?info:Logic.info -> Logic.label -> Tactics.tactic
(** Eliminate operators in the assumptions which don't introduce new
    subgoals. Uses the same rules as {!Boollib.flatten_tac}.
*)
val flatter_concls_tac: 
  ?info:Logic.info -> Logic.label -> Tactics.tactic
(** Eliminate operators in the conclusions which don't introduce new
    subgoals. Uses the same rules as {!Boollib.flatten_tac}.
*)

val flatten_tac: 
  ?info:Logic.info -> ?f:Logic.label -> Tactics.tactic
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

(** {7 Scatter subgoals}

    Split and flatten subgoals.
*)

val scatter_tac: 
  ?info:Logic.info -> ?f:Logic.label -> Tactics.tactic
(** Eliminate boolean operators in the assumptions and conclusions.

    In the assumptions, eliminates [false], negation ([not]),
    conjunction ([&]) and existential quantification ([?]), disjunction
    ([|]), implication ([=>]).

    In the conclusions, eliminates [true], negation ([not]),
    disjunction ([|]), implication ([=>]), universal quantification
    ([!]), conjunction ([&]) and boolean equivalence ([iff]).

    Resulting tag information, in [?info], may contain duplicates.
*)


val blast_tac: 
  ?info:Logic.info -> ?f:Logic.label -> Tactics.tactic
(** Eliminate boolean operators in the assumptions and conclusions
    then try to solve subgoals.

    In the assumptions, eliminates [false], negation ([not]),
    conjunction ([&]) and existential quantification ([?]),
    disjunction ([|]), implication ([=>]) then calls {!Tactics.basic}.

    In the conclusions, eliminates [true], negation ([not]),
    disjunction ([|]), implication ([=>]), universal quantification
    ([!]), conjunction ([&]) and boolean equivalence ([iff]) then
    calls {!Tactics.basic}.

    This is like {!Boollib.scatter_tac}, followed by {!Tactics.basic}.
*)

(** {5 Cases} *)

val cases_tac: ?info:Logic.info -> Basic.term -> Tactics.tactic
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
  ?info:Logic.info
  -> Basic.term -> Tactics.tactic -> Tactics.tactic
(** [show_tac trm tac]: Use [tac] to show that [trm] is true,
    introducing [trm] as a new assumption. If [tac] fails to prove
    [trm], introduces [trm] as the conclusion of a new subgoal.
*)

val show: 
  ?info:Logic.info
  -> Basic.term -> Tactics.tactic -> Tactics.tactic
(** [show trm tac]: Use [tac] to show that [trm] is true, introducing
    [trm] as a new assumption. If [tac] fails to prove [trm],
    introduces [trm] as the conclusion of a new subgoal.

    {!Boollib.show} is a synonym for {!Boollib.show_tac}.
*)


val cases_of: 
  ?info:Logic.info 
  -> ?thm:Logic.thm -> Basic.term 
  -> Tactics.tactic
(** [cases_of ?info ?thm trm]: Try to introduce a case split based on
    the type of term [trm]. If [thm] is given, it is used as the cases
    theorem. If [thm] is not given, the theorem named ["T_cases"] is
    used, where [T] is the name of the type of [trm].
*)


(** {5 Modus Ponens} *)

val mp_tac: 
  ?info:Logic.info
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
  ?info:Logic.info 
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
  ?info:Logic.info 
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
  ?info:Logic.info -> ?inst:Basic.term list 
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

(** {5 More tactics} *)

val equals_tac: ?info:Logic.info -> ?f:Logic.label -> Tactics.tactic
(** Convert boolean equality to iff *)

(** {7 Induction tactics} *)

val asm_induct_tac:
  ?info:Logic.info 
  -> Logic.label -> Logic.label -> Tactics.tactic
(** [asm_induct_tac ?info a c]: Apply the induction scheme of
    assumption [a] to conclusion [c].

    See {!Boollib.induct_tac} for details about the form of the
    induction scheme.
*)

val induct_tac: 
  ?info:Logic.info
  -> ?c:Logic.label -> Logic.thm -> Tactics.tactic
(** [induct_tac ?c thm]: Apply induction theorem [thm] to conclusion
    [c] (or the first conclusion to succeed).

    Theorem [thm] must be in the form:
    {L ! P a .. b: (thm_asm P a .. b) => (thm_concl P a .. b)}
    where
    {L 
    thm_concl P d .. e = (! x .. y: (pred x .. y) => (P d .. e x .. y))
    }   
    The order of the outer-most bound variables is not relevant. 
    
    The conclusion must be in the form:
    {L ! a .. b f .. g: (pred a .. b) => (C a .. b f ..g) }
*)


val induct_on: 
  ?info:Logic.info
  -> ?thm:Logic.thm -> ?c:Logic.label
  -> string -> Tactics.tactic
(** [induct_on ?info ?thm ?c n]: Apply induction to the first
    universally quantified variable named [n] in conclusion [c] (or the
    first conclusion to succeed). The induction theorem is [thm], if
    given or the theorem [thm "TY_induct"] where [TY] is the name of
    the type constructor of [n].

    Theorem [thm] must be in the form:
    {L ! P a .. b: (thm_asm P a .. b) => (thm_concl P a .. b)}
    where
    {L 
    thm_concl P a .. b= (! x: (P x a .. b))
    }   
    The order of the outer-most bound variables is not relevant. 
    
    The conclusion must be in the form:
    {L ! n f .. g: (C n f ..g) }
    [n] does not need to be the outermost quantifier.
*)


