(*----
  Name: tactics.mli
  Copyright Matthew Wahab 2005-2019
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

open Rewrite

(** Tactics, Tacticals  and Conversions *)

type tactic = Context.t -> Logic.tactic
(** A tactic is a function of type [Logic.node -> Logic.branch] *)

type ('a)data_tactic = Context.t -> Logic.node -> ('a * Logic.branch)
(** A data tactic is a tactic that returns additional data. *)

type conv = Context.t -> Logic.conv
(** A conversion is a function of type [term -> thm] *)

(** {5 Support functions} *)

val scope_of: Context.t -> Scope.t
(** Get the scope. *)

val set_scope: Context.t -> Scope.t -> Context.t
(** Set the scope. *)

val goal_context: Context.t -> Logic.node -> Context.t
(** Make scoped context from the scope of a goal. *)

val context_tac : (Context.t -> Context.t) -> tactic -> tactic
(** [context_tac f tac]: Set the context of [tac] to [f ctxt]. *)

(** {7 Error reporting} *)

val error: string -> exn
(** [error s]: Make a {!Report.Error} exception with message [s]. *)

val add_error : string -> exn -> exn
(** [add_error s err]: Add [error s] to exception [err]. *)

(** {7 Accessing elements of a list}

    Simpler versions of {!Lib.get_one} and {!Lib.get_two}. Both raise
    exception [Failure msg] on failure, with [msg] an optional
    argument.
*)

val msg_get_one: string -> 'a list -> 'a
(** [msg_get_one msg lst] Get the first element of list [l], failing with [msg]
    if not possible. *)

val msg_get_two: string -> 'a list -> ('a * 'a)
(** [msg_get_two msg lst] Get the first two elements of list [l], failing with
    [msg] if not possible. *)

(** {7 Formulas} *)

val drop_tag : Logic.tagged_form -> Formula.t
(** Get the formula of a tagged formula. *)

val drop_formula: Logic.tagged_form -> Logic.ftag_ty
(** Get the tag of a tagged formula. *)

(** {7 Formula labels} *)

val fnum: int -> Logic.label
(** Make a label from an integer. *)

val ftag: Logic.ftag_ty -> Logic.label
(** Make a label from a tag. *)

val fname: string -> Logic.label
(** Make a label from a string. *)

val (!!): int -> Logic.label
(** Formula index to label. [!! x] is [fnum x]. *)

val (!~): int -> Logic.label
(**  Assumption index to label.  [!~ x] is [fnum (-x)]. *)

val (!$): string -> Logic.label
(** Formula name to label. [!$ x] is [fname x]. *)

(** {7 Sequents} *)

val asms_of: Logic.Sequent.t -> Logic.tagged_form list
(** Get the assumptions of a sequent. *)

val concls_of: Logic.Sequent.t -> Logic.tagged_form list
(** Get the conclusions of a sequent. *)

val sqnt_tag: Logic.Sequent.t -> Logic.ftag_ty
(** Get the tag of a sequent. *)

(** {7 Nodes} *)

val sequent: Logic.node -> Logic.Sequent.t
(** Get sequent of a node. *)

val scope_of_goal: Logic.node -> Scope.t
(** Get the scope of a node. *)

val typenv_of: Logic.node -> Gtype.Subst.t
(** Get the type environment of a node. *)

val node_tag: Logic.node -> Logic.ftag_ty
(** Get the tag of a node. *)

val get_tagged_asm: Logic.label -> Logic.node -> Logic.tagged_form
(** Get an assumption and its tag by label.*)

val get_tagged_concl: Logic.label -> Logic.node -> Logic.tagged_form
(** Get a conclusion and its tag by label.*)

val get_asm: Logic.label -> Logic.node -> Formula.t
(** Get an assumption by label.*)

val get_concl: Logic.label -> Logic.node -> Formula.t
(** Get a conclusion by label.*)

val get_form: Logic.label -> Logic.node -> Formula.t
(** Get a formula by label. First tries [get_concl] then [get_asm]. *)

(** {7 Branches} *)

val branch_tyenv: Logic.branch -> Gtype.Subst.t
(** Type environment of a branch. *)

val branch_subgoals: Logic.branch -> Logic.Sequent.t list
(** Subgoals of a branch. *)

val has_subgoals: Logic.branch -> bool
(** Test whether a branch has subgoals. *)

val num_subgoals: Logic.branch -> int
(** Get the number of subgoals in a branch. *)

(** {7 Information records} *)

module Info:
sig
  type t = Changes.t

  val changes: Logic.node -> t
  (** Get the changes record of a node. Used to extract the changes
      before a tactic is applied. *)

  val branch_changes: Logic.branch -> t
  (** Get the changes record of a branch. Used to extract the changes
      after a tactic is applied. *)

  val subgoals: t -> Logic.ftag_ty list
  (** [subgoals info]: Get subgoal tags of [info]. *)

  val aformulas: t -> Logic.ftag_ty list
  (** [aformulas info]: Get tags of assumption formula tags from [info]. *)

  val cformulas: t -> Logic.ftag_ty list
  (** [cformulas info]: Get tags of conclusion formula tags from [info]. *)

  val constants: t -> Term.term list
  (** [constants info]: Get constants from [info]. *)
end

val record_changes_tac: (Info.t -> Info.t) -> tactic -> tactic
(** Tactial to set the changes made by tactic.

    [record_changes_tac setter tac g] applies [(tac g)], updating the
    resulting goal with the change record obtained by [(setter
    (branch_changes (tac g)))].
*)

val set_changes_tac: Info.t -> tactic
(** Tactic to record changes in a goal and behave like [skip]. *)

val add_changes_tac: Info.t -> tactic
(** Tactic to add to changes in a goal and behave like [skip]. *)

val append_changes_tac: Info.t -> tactic
(** Tactic to add changes after goal changes. *)


(** {7 Utility functions} *)

val extract_consts:
  Term.Binder.t list -> Term.Subst.t -> Term.term list
(** [extract_consts qs sb]: [extract_consts qs sb] extracts the
    bindings for each of the binders in [qs] from substitution [sb],
    returning the terms in the same order as the binders. [qs] is
    typically obtained by stripping the binders from a formula. [sb]
    is typically constructed by unification.
*)

val qnt_opt_of:
  Term.quant -> (Term.term -> bool) -> Term.term -> bool
(** [qnt_opt_of k pred t]: Apply predicate [pred] to the body of
    possibly quantified term [t]. The outermost quantifiers of kind [k]
    are stripped off before [pred] is applied.

    Returns [pred body] where [(_, body) = strip_qnt k t].
*)

val first_asm:
  (Logic.tagged_form -> bool) -> Logic.Sequent.t
  -> Logic.tagged_form
(** Get the first assumption in a sequent which satisfies a predicate.

    @raise [Not_found] if no assumption found.
*)

val first_concl:
  (Logic.tagged_form -> bool) -> Logic.Sequent.t
  -> Logic.tagged_form
(**
    Get the first conclusion in a sequent which satisfies a predicate.

    @raise [Not_found] if no conclusion found.
*)

val first_form:
  (Logic.tagged_form -> bool) -> Logic.Sequent.t
  -> Logic.tagged_form
(**
    Get the first formula in a sequent which satisfies a
    predicate. Searches the assumptions
    then the conclusions of the sequent.

    @raise [Not_found] if no formula found.
*)

val first_asm_label: (Formula.t -> bool) -> Logic.node -> Logic.label
(** [first_asm_label pred sq]: Get the label of the first assumption whose
   formula satisifies [pred]. Raise Not_found if no such assumption.
 *)

val first_concl_label: (Formula.t -> bool) -> Logic.node -> Logic.label
(** [first_concl_label pred sq]: Get the label of the first conclusion whose
   formula satisifies [pred]. Raise Not_found if no such conclusion.
 *)

(** {5 Basic tacticals and tactics}

    Primitive tactics and tacticals needed by the tacticals.
*)

val foreach: tactic -> Context.t -> Logic.branch -> Logic.branch
(** [foreach tac br]: Apply [tac] to each subgoal of branch [br]. *)

val skip: tactic
(** The tactic that does nothing. Alway succeeds. Preserves the
    incoming change record. *)

val pass: tactic
(** The tactic that does nothing. Alway succeeds. Clears the
    change record. *)

val fail: exn -> tactic
(** [fail err]: The tactic that always fails. Raises [err]. *)

(** {5 Tacticals} *)

val data_tac: (Logic.node -> 'a) -> tactic -> ('a) data_tactic
(** [data_tac f tac g]: Form a data tactic from [((f g), tag g)]. *)

val inject_tac: 'a -> tactic -> 'a data_tactic
(** [inj_tac f tac g]: Form the data_tactic [(f, tac g)]. *)

val (>+): 'a -> tactic -> 'a data_tactic
(** [(d >- tac)]: Synonym for [inj_tac d tac]. *)

val (+<): tactic -> 'a -> 'a data_tactic
(** [(tac +< d)]: Synonym for [inj_tac d tac]. *)

val permute_tac:
  ('a -> 'b) -> ('a) data_tactic
  -> ('b) data_tactic
(** [permute_tac p tac g]:  *)

val (>/):
  ('a) data_tactic -> ('a -> 'b)
  -> ('b) data_tactic
(** [tac >/ p] is [permute_tac p tac]:  *)

val try_tac: tactic -> (bool) data_tactic
(** [try_tac tac g]: Return [(true, tac g)] or [(false, skip g)] if
    [tac g] fails. *)

val query_tac: (Info.t -> tactic) -> tactic
(** [query_tac tac g]: Apply a tactic after extracting the change
    data from a goal. Forms [tac (changes g) g].
*)

val (?>): (Info.t -> tactic) -> tactic
(** ?>tac g]: Prefix notation for [query_tac]. Apply a tactic after
    extracting the change data from a goal. Forms [tac (changes g) g].
*)

val apply_tac: ('a)data_tactic -> ('a -> tactic) -> tactic
(** [apply_tac tac g]: Apply a tactic after extracting the change
    data from a goal. Forms [tac (changes g) g].
*)

val seq: tactic list -> tactic
(** [seq tacl]: Apply each tactic in [tacl] in sequence to the
    subgoals resulting from the previous tactic. Fails if [tacl] is
    empty.
*)

val (++): tactic -> tactic -> tactic
(** [tac1 ++ tac2]: Apply [tac1] then, if there are subgoals, apply
    [tac2] to the subgoals. [tac1 ++ tac2] is [seq [tac1; tac2]].
*)

val alt:  tactic list -> tactic
(** [alt tacl]: Apply each tactic in [tacl], in sequence, until one
    succeeds.  Fails if no tactic succeeds.
*)

val (//): tactic -> tactic -> tactic
(** [tac1 // tac2]: Apply [tac1], if that fails, apply [tac2].  [tac1
    // tac2] is [alt [tac1; tac2]].
*)

val fold_seq: 'a -> ('a -> ('a)data_tactic) list -> ('a)data_tactic
(** [fold_seq d tacl g]: Fold the data returning tactics in [tacl].
*)

val fold_data:
  ('a -> 'b -> ('a) data_tactic) -> 'a -> 'b list -> ('a) data_tactic
(** [fold tac a blist  g]: Fold the data returning tactic [tac] over
    the list [blist] with initial value [a].
*)

val alt_data:
  'a -> ('a -> ('b) data_tactic) list -> ('b) data_tactic
(** [alt_data d tacl]: Try [tac d] for each tactic [tac] in [tacl], in
    sequence, until one succeeds.  Fails if no tactic succeeds.
*)

val thenl: tactic ->  tactic list -> tactic
(** [thenl tac tacl]: Apply tactic [tac] then pair each of the tactics
    in [tacl] with a resulting subgoal. If [tag n] results in subgoals
    [g1, g2, .. gn], the tactics are matched up to produce [tac1 g1,
    tac2 g2, .. , tacn gn]. Excess tactics in [tacl] are silently
    discarded. Excess subgoals are appended to the result of the
    tactics.
*)

val (--): tactic ->  tactic list -> tactic
(** [tac -- tacl]: Synonym for [thenl tac tacl].
*)

val repeat: tactic -> tactic
(** [repeat tac]: Apply [tac] at least once then repeat until it fails
    or there are no more subgoals.
*)

val cond: (Logic.node -> bool) -> tactic -> tactic -> tactic
(** [cond pred ttac ftac g]: Conditional application.

    If [pred g] then [ttac g] else [ftac g].
*)

val (-->): (Logic.node -> bool) -> tactic -> tactic
(** One-armed conditional.  [pred --> tac] is [cond pred tac skip],
    applying [tac] if condition [pred] is met.
*)

val restrict: (Logic.branch -> bool) -> tactic -> tactic
(** [restrict pred tac g]: Restrict the result of applying a tactic.
    Fails if [pred (tac g)] is false otherwise behaves as [(tac g)].
*)

val map_every: ('a -> tactic) -> 'a list -> tactic
(** [map_every tac xs]: Sequentially apply the tactics formed by [(tac
    x)], for each [x] in [xs].  [map_every tac [y1; y2; .. ; yn]] is
    [(tac y1) ++ (tac y2) ++ .. ++ (tac yn)].

    Fails if function [(tac x)] fails for any [x] in [xs] or if any of
    the tactics [(tac x)] fail. Does nothing if [xs] is initially
    empty.
*)

val map_first: ('a -> tactic) -> 'a list -> tactic
(** [map_first tac xs]: Apply the first tactic formed by [(tac x)],
    for each [x] in [xs], to succeed.  [map_first tac [y1; y2; .. ;
    yn]] is [(tac y1) // (tac y2) // .. // (tac yn)].

    Fails if function [(tac x)] fails for every [x] in [xs]. Fails if
    [xs] is initially empty.
*)

val map_some: ('a -> tactic) -> 'a list -> tactic
(** [map_some tac xs]: Sequentially apply the tactics formed by [(tac
    x)], for each [x] in [xs], allowing some tactics to fail.

    Fails if function [(tac x)] fails for any [x] in [xs] or if all of
    the tactics [(tac x)] fail. Fails if [xs] is initially empty.
*)

val seq_some: tactic list -> tactic
(** [seq_some tacl xs]: Sequentially apply the tactics in [tacl],
    allowing some tactics to fail.

    Fails if every tactic in [tacl] fails or if [tacl] is initially empty.

    [seq_some tacl] is equivalent to [map_some tacl (fun x -> x)]
*)

val seq_any: tactic list -> tactic
(** [seq_any tacl xs]: Sequentially apply the tactics in [tacl],
    allowing some tactics to fail.

    Fails if every tactic in [tacl] fails or if [tacl] is initially empty.
*)

val foreach_asm: (Logic.label -> tactic) -> tactic
(** [foreach_asm tac goal]: Sequentially apply [tac l] to each
    assumption in [goal], beginning with the first assmuption, where
    [l] is the label of each assumption considered.

    Fails if no instance of [tac l] succeeds.
*)

val foreach_concl: (Logic.label -> tactic) -> tactic
(** [foreach_concl tac goal]: Sequentially apply [tac l] to each
    conclusion in [goal], beginning with the first assmuption, where
    [l] is the label of each assumption considered.

    Fails if no instance of [tac l] succeeds.
*)

val foreach_form: (Logic.label -> tactic) -> tactic
(** Apply {!Tactics.foreach_asm} then {!Tactics.foreach_concl},
    failing if both fail.
*)

(** {5 Tactics}

    The tactics in this module which abstract from those defined in
    {!Logic.Tactics} should be prefered to those in {!Logic.Tactics}.
*)

val rotateA: tactic
(** Rotate the assumptions. *)

val rotateC: tactic
(** Rotate the conclusions. *)

val copyA: Logic.label -> tactic
(** Copy an assumption.*)

val copyC: Logic.label -> tactic
(** Copy a conclusion. *)

val liftA: Logic.label -> tactic
(** [liftA a]: Lift assumption [a] to the top of the list.
*)
val liftC: Logic.label -> tactic
(** [liftC c]: Lift conclusion [c] to the top of the list.
*)

val lift: Logic.label -> tactic
(** Move a formula to the top of the list of assumptions/conclusions.
*)

val deleteA: Logic.label -> tactic
(** [deleteA l]: Delete the assumption labelled [l]. *)

val deleteC: Logic.label -> tactic
(** [deleteC l]: Delete the conclusion labelled [l]. *)

val delete: Logic.label -> tactic
(** [delete l]: Delete the formula labelled [l]. *)

val deleten: Logic.label list -> tactic
(**  [deleten ls]: Delete the formulas identified by the labels in [ls]. *)

(** {7 Logic rules}

    Apply the basic rules to the first assumption/conclusion which
    will succeed

    If assumption [a], conclusion [c] or formula [f] is not found,
    these will search for a suitable assumption/conclusion.
    (Parameter [f] is used for formulas which can be either in the
    assumptions or the conclusions).

    Tag information provided by the rules is as in {!Logic.Tactics}.
*)

val trueC_at: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.trueC}. *)
val trueC: tactic
(** Apply {!Logic.Tactics.conjC} to the first instance of [true] in the
    conclusions. *)

val conjC_at: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.conjC}. *)
val conjC: tactic
(** Apply {!Logic.Tactics.conjC} to the first conjunction in the conclusions. *)

val conjA_at: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.conjA}. *)
val conjA: tactic
(** Apply {!Logic.Tactics.conjA} to the first conjunction in the assumptions. *)

val disjC_at: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.disjC}. *)
val disjC: tactic
(** Apply {!Logic.Tactics.disjC} to the first disjunction in the conclusions. *)

val disjA_at: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.disjA}. *)
val disjA: tactic
(** Apply {!Logic.Tactics.disjA} to the first diisjunction in the
    assumptions. *)

val negC_at: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.negC}. *)
val negC: tactic
(** Apply {!Logic.Tactics.negC} to the first negation in the conclusions. *)

val negA_at: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.negA}. *)
val negA: tactic
(** Apply {!Logic.Tactics.negA} to the first negation in the assumptions. *)

val implC_at: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.implC}. *)
val implC: tactic
(** Apply {!Logic.Tactics.implC} to the first implication in the conclusions. *)

val implA_at: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.implA}. *)
val implA: tactic
(** Apply {!Logic.Tactics.implA} to the first implication in the assumptions. *)

val existC_at: Term.term -> Logic.label -> tactic
(** Entry point to {!Logic.Tactics.existC}. *)
val existC: Term.term -> tactic
(** Apply {!Logic.Tactics.existC} to the first existentially quantified
    conclusion. *)

val existA_at: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.existA}. *)
val existA: tactic
(** Apply {!Logic.Tactics.existA} to the first existentially quantified
   assumption. *)

val allC_at: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.allC}. *)
val allC: tactic
(** Apply {!Logic.Tactics.allC} to the first universally quantified
    conclusion. *)

val allA_at: Term.term -> Logic.label -> tactic
(** Entry point to {!Logic.Tactics.allA}. *)
val allA: Term.term -> tactic
(** Apply {!Logic.Tactics.allA} to the first universally quantified
    assumption. *)

val nameC:
  string -> Logic.label -> tactic
(** Entry point to {!Logic.Tactics.nameC}. *)
val nameA:
  string -> Logic.label -> tactic
(** Entry point to {!Logic.Tactics.nameA}. *)

val instA_at: Term.term list -> Logic.label -> tactic
(** Instantiate a universally quantified assumption. Generalises
    [allA] to a list of terms. [instA a trms] applies [allA a t] for
    each [t] in [trms]. Fails if there are more terms then variables.
*)
val instA: Term.term list -> tactic
(** Apply [instA_at] to the first universially quantified assumption *)

val instC_at: Term.term list -> Logic.label -> tactic
(** Instantiate an existentially quantified conclusion. Generalises
    [existC] to a list of terms. [instc a trms] applies [existC a t]
    for each [t] in [trms]. Records the result of the last
    instantiation. Fails if there are more terms then variables.
*)
val instC: Term.term list -> tactic
(** Apply [instC_at] to the first universially quantified conclusion *)

val inst_at: Term.term list -> Logic.label -> tactic
(** Instantiate a specific formula. Tries {!Tactics.instA_at} then
   {!Tactics.instC_at}. *)

val inst_tac: Term.term list -> tactic
(** Instantiate a formula. Tries {!Tactics.instA} then {!Tactics.instC}. *)

val cut: Term.term list -> Logic.thm -> tactic
(** [cut trms th]: Cut [th] into the sequent. The top-most variables of the
   theorem are instantiated with [trms]. Entry point to {!Logic.Tactics.cut}.
   *)

val betaA_at: Logic.label -> tactic
(** [betaA l sq]: beta conversion of assumption [l]

    {L
    F((%x.P x) c){_ l}, asm |- concl
    ---->
    F(P c){_ l}, asm |- concl
    }

    @raise [Not_found] if assumption not found.

    info: [goals = [], aforms=[l], cforms=[], terms = []]
*)
val betaA: tactic
(** Apply [betaA_at] to the first lambda quantified assumption *)

val betaC_at: Logic.label -> tactic
(** [betaC l sq]: beta conversion of conclusion [l]

    {L
    asms |- F((%x.P x) c){_ l}, concls
    ---->
    asms |- F(P c){_ l}, concls
    }

    @raise [Not_found] if conclusion not found.

    info: [goals = [], aforms=[l], cforms=[], terms = []]
*)
val betaC: tactic
(** Apply [betaC_at] to the first lambda quantified conclusion *)

val beta_tac: tactic
(** [beta_tac]: Apply beta conversion to a formula in the goal. Beta convert
    conclusions and then the assumptions. Fails if no change is made.  *)

val beta_at :Logic.label -> tactic
(** [beta_tac]: Apply beta conversion to a specific formula in the goal. *)

val name_tac: string -> Logic.label -> tactic
(** [name_tac n lbl]: Name formula [lbl] with [n].  Entry point
    to {!Logic.Tactics.nameA} and {!Logic.Tactics.nameC}.
*)

val find_basic:
  (Logic.label)option -> (Logic.label)option -> Logic.Subgoals.node
  -> (Logic.label * Logic.label)
(** Find an assumption and conclusion that make the goal basic. Raises
   [Not_found] if there is no such pair *)

val basic_at: Logic.label -> Logic.label -> tactic
(** [basic_at a c] Proves the goal \[A{_ a}, asms |- B{_ c}, concls\] if A is
   alpha-equal to B.  Entry point to {!Logic.Tactics.basic}.  *)

val basic: tactic
(** Tries to find an assumption and conclusion that can be solved using
   {!Logic.Tactics.basic}.  *)

val unify_at: Logic.label -> Logic.label -> tactic
(** [unify_at a c g]: Try to unify assumption [a] with conclusion [c].

   Assumption [a] may be universally quantified.  Conclusion [c] may be
   existentially quantified. Toplevel universal/existential quantifiers will be
   stripped, and the bound variables treated as variable for the unification. If
   unification succeeds, the toplevel quantifiers are instantiated with the
   terms found by unification.

   Final action is to apply [basic] to solve the goal if the terms are
   alpha-equal.
*)

val unify_tac: tactic
(** Like {!unify_at}, tries to find an assumption and conclusion that
    can be unified.  *)

val substA: Logic.label list -> Logic.label -> tactic
(** Entry point to {!Logic.Tactics.substA}. *)

val substC: Logic.label list -> Logic.label -> tactic
(** Entry point to {!Logic.Tactics.substC}. *)

(** {5 Derived tactics and tacticals} *)

val named_tac:
  tactic
  -> string list -> string list
  -> tactic
(** [named_tac tac anames cnames]: Apply [tac], renaming the
    assumptions and conclusions produced with the names in [anames]
    and [cnames] respecatively. The number of names does not have to
    match the number of assumptions/conclusions produced.

    Actions: apply [tac ~info:inf goal], rename each of
    [Drule.aformulas inf] with a name from [anames], rename each of
    [Drule.cformulas inf] with a name from [cnames], in order. Set
    [info=inf'] where [inf'] is [inf], with the formula tag produced
    by renaming.  *)

(** {7 Pattern matching tacticals} *)

(** {8 Support functions} *)

val find_match_formulas:
  Gtype.Subst.t
  -> Scope.t -> (Term.term -> bool)
  -> Term.term -> Logic.tagged_form list -> Logic.label
(** [find_match_formulas scp varp t fs]: Find a match for a term in
    list of tagged formulas.  Return the tag of the first formula in
    [fs] to unify with term [t] in scope [scp].  [varp] determines
    which terms can be bound by unification.  raise Not_found if no
    match.

    Only free variables are bound in the matching process.
    e.g. in [<< !x. y and x >>] only [y] is a bindable variable
    for the match.
*)

val find_match_asm:
  Gtype.Subst.t
  -> Term.term -> Logic.Sequent.t -> Logic.label
(** [find_match_asm tyenv t sq]: Find a match for [t] in the
    assumptions of [sq].  Return the tag of the first formula in the
    assumptions to unify with term [t] in the scope of sequent [sq].
    raise Not_found if no match.
*)

val find_match_concl:
  Gtype.Subst.t
  -> Term.term -> Logic.Sequent.t -> Logic.label
(** [match_concl t sq]: Find a match for [t] in the assumptions of
    [sq].  Return the tag of the first formula in the assumptions to
    unify with term [t] in the scope of sequent [sq].  raise Not_found
    if no match.
*)

(** {8 Tacticals} *)

val match_asm: Term.term -> (Logic.label -> tactic) -> tactic
(** [match_asm trm tac g]: Apply a tactic to the assumption matching
    term.

    Find the label [l] of the first assumption, in the first subgoal
    of [g], which matches [trm] then apply tactic [tac l] to [g].
    Fails if [tac l] fails or if there is no matching assumption.

    Free variables in trm may be bound in the matching process.
    e.g. in [<< !x. y and x >>] only [y] is a bindable variable for
    the match.
*)

val match_concl: Term.term -> (Logic.label -> tactic) -> tactic
(** [match_concl trm tac g]: Apply a tactic to the conclusion matching
    a term.

    Find the label [l] of the first conclusion, in the first subgoal
    of [g], which matches [trm] then apply tactic [tac l] to [g].
    Fails if [tac l] fails or if there is no matching conclusion.

    Free variables in [trm] may be bound during matching.
    e.g. in [<< !x. y and x >>] only [y] is a bindable variable for
    the match.
*)

val match_formula: Term.term -> (Logic.label -> tactic) -> tactic
(** [match_formula trm tac g]: Apply a tactic the assumption or
    conclusion matching a term.

    Find the label [l] of the first formula, in the first subgoal of
    [g], which matches [trm] then apply tactic [tac l] to [g].  The
    match is carried out first on the assumptions then on the
    conclusions. Fails if [tac l] fails or if there is no matching
    formula in the subgoal.

    Free variables in trm may be bound in the matching process.
    e.g. in [<< !x. y and x >>] only [y] is a bindable variable for
    the match.
*)

val specA_at: Logic.label -> tactic
(** Specialize an existentially quantified assumption. [specA_at a] repeatedly
   applies [existA_at a], failing if [a] is not an existentially
   quantified formula.

   info: [aforms=[tg], constants = cs] where [tg] is the tag of the specialised
   assumption and [cs] are the constants generated by [existA], in the order
   they were generated.  *)

val specA: tactic
(** Specialize an existentially quantified assumption. [specA a trms]
    repeatedly applies [existA], failing if [a] is not an existentially
    quantified formula.

    info: [aforms=[tg], constants = cs] where [tg] is the tag of the
    specialised assumption and [cs] are the constants generated by
    [existA], in the order they were generated.
*)

val specC_at: Logic.label -> tactic
(** Specialize a universally quantified assumption. [specC_at c]
    repeatedly applies [allC], failing if [c] is not universally
    quantified.

    info: [cforms=[tg], constants = cs] where [tg] is the tag of the
    specialised conclusion and [cs] are the constants generated by
    [allC], in the order they were generated.
*)

val specC: tactic
(** Specialize a universally quantified assumption. [specC a trms]
    repeatedly applies [allC], failing if [c] is not universally
    quantified.

    info: [cforms=[tg], constants = cs] where [tg] is the tag of the
    specialised conclusion and [cs] are the constants generated by
    [allC], in the order they were generated.
*)

val spec_at: Logic.label -> tactic
(** Specialize a specfici formula. Tries {!Tactics.specC} then
    {!Tactics.specA}.

    info: [cforms=[tg], constants = cs] or [aforms=[tg], constants =
    cs] where [tg] is the tag of the specialised formula and [cs] are
    the new constants in the order they were generated.
*)

val spec_tac: tactic
(** Specialize a formula. Tries {!Tactics.specC} then
    {!Tactics.specA}.

    info: [cforms=[tg], constants = cs] or [aforms=[tg], constants =
    cs] where [tg] is the tag of the specialised formula and [cs] are
    the new constants in the order they were generated.
*)

(** {5 Rewriting} *)

type ('a)plan = ('a)Rewrite.plan
(** Rewrite plans. *)
type rule = Logic.rr_type
(** Rewrite rules. *)

val leftright: Rewrite.direction
(** Left to right rewriting. *)
val rightleft: Rewrite.direction
(** Right to left rewriting. *)

val rewrite_control:
  ?max:int -> ?strat:Rewrite.strategy
  -> Rewrite.direction -> Rewrite.control
(** [rewrite_control max strat dir]: Make a rewrite control. Default
    strategy is top-down ([?strat = Rewrite.topdown]).
*)

val conv_rule:
  Context.t -> conv -> Logic.thm -> Logic.thm
(** [conv_rule ctxt conv thm]: Apply conversion [conv] to theorem [thm] *)

(** {7 Tactics} *)

val pure_rewriteA:
  ?term:Term.term -> (rule)plan -> Logic.label
  -> tactic
(** [pure_rewriteA info p l]: Rewrite assumption [l] with plan
    [p]. This is a front end to [rewrite_intro]/[substA].  If [term]
    is given, assumption [l] is replaced with the result of rewriting
    [term] otherwise it is the assumption that is rewritten.

    {L
    A{_ l}, asms |- concl
    ---->
    B{_ l}, asms|- concl
    }

    info: [goals = [], aforms=[l], cforms=[], terms = []]
*)

val pure_rewriteC:
  ?term:Term.term -> (rule)plan -> Logic.label
  -> tactic
(** [pure_rewriteC info p l]: Rewrite conclusion [l] with plan
    [p]. This is a front end to [rewrite_intro]/[substC]. If [term] is
    given, conclusion [l] is replaced with the result of rewriting
    [term] otherwise it is the conclusion that is rewritten.

    {L
    asms |- A{_ l}, concl
    ---->
    asms|- B{_ l}, concl
    }

    info: [goals = [], aforms=[], cforms=[l], terms = []]
*)

val pure_rewrite_tac:
  ?term:Term.term -> (rule)plan -> Logic.label
  -> tactic
(** [pure_rewrite info p l]: Combination of [pure_rewriteC] and
    [pure_rewriteA]. First tries [pure_rewriteC] then tries
    [pure_rewriteA].
*)

val pure_rewrite_conv: (Logic.thm) plan -> conv
(** [pure_rewrite_conv plan ctxt trm]: rewrite term [trm] according to
    [plan] in scope [ctxt]. This is an interface to
    {!Logic.Conv.rewrite_conv}.

    Returns [|- trm = X] where [X] is the result of rewriting [trm]
*)

val pure_rewrite_rule:
  (Logic.thm) plan -> Context.t -> Logic.thm -> Logic.thm
(** [pure_rewrite_rule plan scp thm]: rewrite theorem [thm] according
    to [plan] in scope [scp].

    Returns [|- X] where [X] is the result of rewriting [trm]
*)

(** {7 Rewrite planner}

    Rewrite planner for use with tactics and conversions.
*)

module PlannerData:
  (Rewrite.Planner.Data
   with type rule = Logic.rr_type
   and type data = Logic.node option)
(** Data for the rewrite planner *)

module Planner:
  (Rewrite.Planner.T with type a_rule = PlannerData.rule
                     and type rule_data = PlannerData.data)
(** The general rewriter planner. Constructs plans based on
    assumptions and theorems.
*)

val mk_plan:
  ?ctrl:Rewrite.control -> Logic.node
  -> rule list -> Term.term -> rule plan
(** The rewrite planner, for use with tactics.

    [mk_plan scp ?ctrl ?goal rules term]: Make a plan to rewrite [term]
    using theorems and assumptions in [rules]. If [goal] is given, it
    is the source of the assumptions in [rules].

    N.B. The [rr_dir] field of [ctrl] is ignored.
*)

val mk_thm_plan:
  Context.t -> ?ctrl:Rewrite.control
  -> rule list -> Term.term -> Logic.thm plan
(** The theorem rewrite planner, for use with conversions and rules.

    [mk_thm_plan scp ?ctrl ?goal rules term]: Make a plan to rewrite [term]
    using theorems in [rules].

    N.B. The [rr_dir] field of [ctrl] is ignored.
*)
