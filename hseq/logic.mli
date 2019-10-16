(*----
  Name: logic.mli
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

(** Theorems and rules of the logic.

    The rules of the logic are based on a sequent calculus.  A {e
    theorem} is a formula which has been proved true, by applying rules
    to a {e goal}. A goal is made up of {e subgoals} to be proved. If a
    goal has no subgoals, it can be converted to a theorem. The rules
    of the logic are implemented as {e tactics} which reduce a subgoal
    or replace it with a list of subgoals.

    The logic includes a number of features which are experimental or
    are intended to improve efficiency. These include some functions
    which construct theorems without passing through the rules of the
    logic. In particular, beta reduction is available as a {e
    conversion} (a function constructing a theorem to state an
    equality [|- t=x] from term [t]). In addition, term and type
    definitions are currently handled as part of the logic.

    {7 Goals, subgoals and tactics}

    A goal has type {!Logic.goal} and consists of the formula to be
    proved, a list of subgoals and a type environment. The type
    environment holds bindings for weak type variables occurring in
    any of the subgoals. This means that when a weak type variable is
    bound to a type constant (e.g. by unification) in one subgoal, the
    binding is propagated to all the other subgoals. This is necessary
    for the soundness of the logic.

    A subgoal (also called a {e sequent}) has type {!Logic.Sequent.t}
    and consists of a tag, a scope, possibly empty lists of {e
    assumptions} and {e conclusions} and information used (by the
    quantifier rules) to generate terms. The tag of a subgoal is
    unique and can be used to identify the subgoal.  Subgoals are
    implemented in module {!Logic.Sequent}.

    The assumptions and conclusions of a subgoal are collectively
    referred to as the {e formulas} of the subgoal. Each subgoal has
    at least one formula. Each formula of a subgoal has a {e tag}
    which is unique in the subgoal. A {e label} identifies a formula
    either by the formulas' tag or by an integer giving the formulas'
    position in the list of assumptions or conclusions. A negative
    integer {i -i} identifies the {i i}th assumption with the first
    assumption at position -1. A positive integer {i i} identifies the
    {i i}th conclusion with the first conclusion at position 1. Tags
    are intended for use in tactics, to identify a formula regardless
    of its position (which can change).

    Integer labels are intended for interactive proof, where it is
    more convenient to use the position of a formula as the
    identifier. For writing tactics and in other programming uses,
    integer labels should be converted to formula tags. Tactics
    written using tags to identify formulas will be more robust and
    efficient than those using positional identifiers since the tag of
    a formula in a subgoal always stays the same while the position of
    the formula can change.

    A formula can be labelled with a string (its {e name}) which can
    then be used in interactive proof in the same way as any other
    label. The name is independent of the position of the formula and
    is therefore more robust than positional labels. The assignment of
    a name is handled by tactics {!Logic.Tactics.nameA} or
    {!Logic.Tactics.nameC}.

    The application of tactics to subgoals and the integration of the
    result into the goal is handled by a subgoal package (module
    {!Logic.Subgoals}). This package manages the proof tree contructed
    during a proof and allows tactics to be chained together, applying
    each tactic in a sequence to the subgoals resulting from the
    previous tactic.

    A tactic is a function from a {e node} to a {e branch} of the
    proof tree. A node has type {!Logic.Subgoals.node} and consists of
    a single subgoal and the type environment of the goal. A branch
    has type {!Logic.Subgoals.branch} and consists of a list of
    subgoals and the new type environment of the goal. The subgoals
    and type environment of a node or a branch can be examined,
    e.g. to determine what action to take. Nodes and branches can only
    by constructed by functions in module {!Logic}. A new tactic must
    use the logic rules implemented by module {!Logic.Tactics} to
    construct a branch from a node. The subgoal package tags nodes and
    constructed branches to ensure that the branch {e b} returned by a
    tactic applied to node {e n} was constructed from {e n}. This is
    intended to make it impossible to return an arbitrary branch for a
    given node. The correct working of this mechanism is required to
    ensure the soundness of the logic.

    Each of the standard tactics provides information about subgoals
    and formulas created or affected and constants introduced by the
    tactic. This information is packaged using type {!Logic.info} and
    contains the tags of subgoals, formulas and terms generated by a
    tactic. Generally only the tags of subgoals or formula affected or
    generated by a tactic will be recorded but the documentation for
    each tactic should be consulted for specific details. Note that,
    because every subgoal is uniquely tagged, the standard tactics
    only record the subgoal tags if more than one subgoal is
    produced. The terms recorded are typically generated by quantifier
    rules, which instantiate quantifiers with arbitrary {e Skolem}
    constants. These are automatically generated identifiers which are
    unique to the scope of a subgoal. The generation of Skolem
    constants is handled by module {!Logic.Skolem}.

    {7 Notation}

    A theorem is written |- t

    A subgoal is written
    {L g:\[A{_ ta{_ 1}}, ... , A{_ ta{_ n}} |-
    C{_ tc{_ 1}}, ..., C{_ tc{_ n}}\]}
    where
    {ul {- g is the tag of the subgoal}
    {- ta{_ i} is the tag of assumption A{_ ta{_ i}}}
    {- tc{_ i} is the tag of conclusion C{_ tc{_ i}}}
    {- A{_ i} are the assumptions}
    {- C{_ i} are the conclusions}
    {- A{_ ta{_ 1}} is the first assumption}
    {- C{_ tc{_ 1}} is the first conclusion}}
    When the subgoal tag or a formula tag is irrelevant, they may be
    omitted. When the subgoal tag is omitted, the enclosing
    brackets ('\[' and '\]') may also be omitted.

    A list of subgoals is written as a semi-colon seperated list:
    [sg1; sg2; ...; sg3].

    Where there is no ambiguity, labels and tags will be treated as
    the same.

    The information provided by tactics is written
    {L [info: goals = goal_tags, aforms=form_tags, cforms=form_tags,
    terms = term_list]}
    where
    {ul {- [goal_tags] is a list of tags which identify subgoals.}
    {- [aform_tags] is a list of tags which identify subgoal assumptions.}
    {- [form_tags] is a list of tags which identify subgoal conclusions.}
    {- [term_list] is a list of terms which have been introduced by
    the tactic.}}
*)

(** {5 Theorems} *)

type thm
(** The type of theorems. *)

val mk_axiom: Formula.t -> thm
(** Make an axiom. (Axioms do not need to be proved true.) *)

val formula_of: thm -> Formula.t
(** Convert a theorem to a formula. *)
val term_of: thm -> Term.term
(** Convert a theorem to a term. *)

val is_fresh: Scope.t -> thm -> bool
(** [is_fresh scp t]: Check that the theory marker of [t] is still
    valid.
*)

(** {7 Permanent storage} *)

type saved_thm
(** The representation for saving on permanent storage. *)
val to_save: thm -> saved_thm
(** Convert to the representation for saving on permanent storage. *)
val from_save: Scope.t -> saved_thm -> thm
(** Convert from the representation for saving on permanent storage. *)

(** {7 Pretty printing} *)

val print_thm: Printers.ppinfo -> thm -> unit
(** Pretty printing of a theorem. *)

val string_thm: thm -> string
(** String representation of a theorem (using [print_thm] instead). *)

(** {5 Error reporting} *)

val logic_error: string -> Formula.t list -> exn
(** Make a logic error. *)
val add_logic_error: string -> Formula.t list -> exn -> 'a
(** Add a logic error to an existing list of errors. *)

val sqntError: string ->  exn
(** Make a sequent error. *)
val addsqntError: string -> exn -> 'a
(** Add a sequent error to an existing list of errors. *)


(** {5 Subgoals} *)

(** {7 Types used in subgoals} *)


(**
    Labels for identifying sequent formulas.

    [FNum i]: The formula at position [i]. If [i] is negative, [i=-j],
    the formula is the assumption at position [j]. If [i] is positive,
    the formula is the conclusion at position [i].

    [FTag t]: The formula with tag [t].

    [FName n]: The formula with named [n] (Experimental).
*)

type ftag_ty = (string)Tag.t
(** Tags for formulas in a goal. *)

type label =
    FNum of int
  | FTag of ftag_ty
  | FName of string

type tagged_form = (ftag_ty * Formula.t)
(** Tagged formulas. Each formula in a subgoal has a tag. *)

val form_tag: tagged_form -> ftag_ty
(** The tag of a formula. *)
val drop_tag: tagged_form -> Formula.t
(** Drop the tag of a formula. *)


(** {6 Sequents} *)

(** {7 Utility functions}

    Utility functions for use with sequents.
*)

val join_up: 'a list -> 'a list -> 'a list
(** [join_up l r] is [List.rev_append l r]. Reverses [l] and appends
    it to [r].
*)

val split_at_tag:
  ftag_ty -> (ftag_ty * 'a) list
  -> ((ftag_ty * 'a) list * (ftag_ty * 'a) * (ftag_ty * 'a) list)
(** [split_at_tag t x]: Split [x] into [(l, c, r)] so that [x = join_up
    l (c::r)] and [c] is the first element of [x] tagged with [t].
*)

val split_at_label:
  label -> (ftag_ty * 'a) list
  -> ((ftag_ty * 'a) list * (ftag_ty * 'a) * (ftag_ty * 'a) list)
(** [split_at_label t x]: Split [x] into [(l, c, r)] so that [x =
    join_up l (c::r)] and [c] is the formula in [x] labelled [l].
*)

val split_at_asm:
  label -> (ftag_ty * 'a) list
  -> ((ftag_ty * 'a) list * (ftag_ty * 'a) * (ftag_ty * 'a) list)
(** [split_at_asm lbl x]: Split [x] into [(l, c, r)] so that
    [x=join_up l (c::r)] and [c] is the assumption in [x] labelled [l].

    @raise Not_found if [lbl = FNum i] and i >= 0.
*)

val split_at_concl:
  label -> (ftag_ty * 'a) list
  -> ((ftag_ty * 'a) list * (ftag_ty * 'a) * (ftag_ty * 'a) list)
(** [split_at_concl lbl x]: Split [x] into [(l, c, r)] so that [x =
    join_up l (c::r)] and [c] is the conclusion in [x] labelled [l].

    @raise Not_found if [lbl = FNum i] and i<0.
*)

(** Sequents and their components *)
module Sequent:
sig
  (** A sequent is made up of a unique tag, a scope, a list of
      assumptions, a list of conclusions and information about
      skolem constants and weak types. *)

  type t
  (** The type of sequents *)

  (** {7 Components of a sequent} *)

  val asms: t -> tagged_form list
  (** The assumptions. *)
  val concls: t -> tagged_form list
  (** The conclusions. *)
  val scope_of: t -> Scope.t
  (** The scope of a sequent. *)
  val sklm_cnsts: t -> Skolem.skolem_cnst list
  (** The skolem constants of the sequent. *)
  val sqnt_tyvars: t -> Gtype.t list
  (** All weak type variables that were generated in the sequent. *)
  val sqnt_tag: t->ftag_ty
  (** The tag of the sequent. (This is the tag of the subgoal.) *)


  (** {7 Operations on formulas} *)

  val get_asm: int -> t -> tagged_form
  (** Get an assumption by position. *)
  val get_cncl: int -> t -> tagged_form
  (** Get a conclusion by position. *)

  val get_tagged_asm: ftag_ty -> t -> tagged_form
  (** Get an assumption by tag. *)
  val get_tagged_cncl: ftag_ty -> t -> tagged_form
  (** Get a conclusion by tag. *)
  val get_tagged_form: ftag_ty -> t -> tagged_form
  (** Get a formula by tag. *)

  val get_named_asm: string -> t -> tagged_form
  (** Get an assumption by name. *)
  val get_named_cncl: string -> t -> tagged_form
  (** Get a conclusion by name. *)
  val get_named_form: string -> t -> tagged_form
  (** Get a formula by name. *)

  val delete_asm: label -> t -> t
  (** Delete an assumption by label. *)
  val delete_cncl: label -> t -> t
  (** Delete a conclusion by label. *)

  val tag_to_index: ftag_ty -> t -> int
  (** Get the position of a formula from its tag. *)
  val index_to_tag: int -> t -> ftag_ty
  (** Get the tag of a formula from its position. *)
  val name_to_tag: string -> t -> ftag_ty
(** Get the tag of a formula from its name. *)

end

(** {7 Operations on sequent formulas} *)

val label_to_tag: label -> Sequent.t -> ftag_ty
(** Convert a label to the tag of the formula it identifies. *)
val label_to_index: label -> Sequent.t -> int
(** Convert a label to the position of the formula it identifies. If
    the label identifies an assumption, the position will be a
    negative integer.
*)

val get_label_asm: label -> Sequent.t -> tagged_form
(** Get the assumption identified by a label. *)
val get_label_cncl: label -> Sequent.t -> tagged_form
(** Get the conclusion identified by a label. *)
val get_label_form: label -> Sequent.t -> tagged_form
(** Get the formula identified by a label. *)

(** {5 Goals} *)

(**
   A goal is made up of:
   {ul
   {- The sub-goals still to be proved.}
   {- A type environment: the bindings of the shared type
   variables which occur in the goals sequents (all of these are weak
   type variables).}
   {- A formula: the theorem which is to be proved.}}
*)
type goal

val get_goal: goal -> Formula.t
(** The formula to be proved. *)
val get_subgoals: goal -> Sequent.t list
(** The subgoals of the goal. *)
val goal_tyenv: goal -> Gtype.Subst.t
(** The type environment of the goal. *)
val goal_changes: goal -> Changes.t
(** The changes made to the goal. *)

val has_subgoals: goal -> bool
(** Whether a goal has subgoals. *)
val num_of_subgoals: goal -> int
(** The number of subgoals in a goal. *)

val mk_goal: Scope.t -> Formula.t -> goal
(** [mk_goal info scp f]: Make formula [f] a goal to be proved in
    scope [scp]. If [info] is given, the tag of the new subgoal and
    conclusion are stored in it.
*)

val mk_thm: goal -> thm
(** Make a theorem from a goal with no subgoals.  Fails if the goal
    has subgoals.
*)

(** {7 Manipulating goals} *)

val goal_focus: ftag_ty-> goal -> goal
(** Put the tagged sqnt at the front. @raise [Not_found] if not found. *)

val rotate_subgoals_left: int -> goal -> goal
(** [rotate_subgoals_left n]: Rotate subgoals left.

    @raise [Failure] if no subgoals.
*)
val rotate_subgoals_right: int -> goal -> goal
(** [rotate_subgoals_right n]: Rotate subgoals right [n] places.

    @raise [Failure] if no subgoals.
*)

(** {7 Applying Rules to Subgoals} *)

(** The subgoal package. Manages the application of rules to the
    subgoals of a goal. Ensures that the result of applying a tactics
    to a subgoal is the result of the functions, defined in module
    {!Logic}, which implement the rules of the logic.

    The packages provides a type {!Logic.Subgoals.node} of nodes and a
    type {!Logic.Subgoals.branch} of branches, representing nodes and
    branches of the proof tree. A tactic has type [node -> branch], a
    node contains the subgoal which is the argument to the tactic and
    a branch has the list of subgoals resulting from the tactic. Each
    node and branch has a unique tag. When a tactic [tac] is applied
    to a node [n], a unique tag (the ticket) is generated and set as
    the tag of [n]. If branch [b=tac n], the tag of [b] must be the
    same as the ticket otherwise the result is invalid and will be
    discarded. If the result is valid, the tag of [b] is set to the
    original tag of [n] and [b] is the result of the tactic.
    Initially, the tag of node [n] is the tag of the subgoal to be
    solved. Because each subgoal is uniquely tagged, the branch
    resulting from applying a tactic to the node is unique to that
    subgoal.

    Since only functions in module {!Logic} can change the tag of a
    node or branch, tactics external to {!Logic} must use the tactics
    defined in {!Logic} to construct branches from nodes. The system
    provides for tacticals by allowing tactics to be chained together,
    the branch resulting from the last tactic having the same tag as
    the node applied to the first tactic.

    The functions in this module deal with the application of tactics
    to nodes and goals. The main function is {!Logic.Subgoals.apply}
    which implements the ticket holding mechanism. Other functions
    allow the result of applying tactics to be reported (e.g. by
    printing) and also provide different ways of applying tactics to
    lists of subgoals (all of which are based around
    {!Logic.Subgoals.apply}).
*)

exception No_subgoals
(** No subgoals. A tactic was applied to a goal with no sub-goals. *)

module Subgoals:
sig

  (** {7 Notification of Result} *)

  (** {7 Nodes and branches} *)

  (** [node]: A node holds the subgoal to be solved and the type
      environment of the goal. A node also holds a tag, which is not
      visible.  *)
  type node

  val node_tyenv: node -> Gtype.Subst.t
  (** The type environment of the goal. *)
  val node_sqnt: node -> Sequent.t
  (** The subgoal to be proved. *)
  val node_changes: node -> Changes.t
  (** The subgoals of the branch. *)

  (** [branch]: A branch is a list of subgoals and the type
      environment of the goal. A branch also holds the tag of the
      node from which it was produced.  *)
  type branch

  val branch_tyenv: branch -> Gtype.Subst.t
  (** The type environment of the branch. *)
  val branch_sqnts: branch -> Sequent.t list
  (** The subgoals of the branch. *)
  val branch_changes: branch -> Changes.t
  (** The subgoals of the branch. *)

  (** {7 Utility functions} *)

  val merge_tyenvs:
    Gtype.Subst.t
    -> Gtype.Subst.t
    -> Gtype.Subst.t
  (** [merge tyenv1 tyenv2]: Merge type environments.

      Create a type environment [env3] which has the binding of each weak
      variable in [env1 + env2].

      Used to combine the type environment resulting from the application
      of a tactic with the original type environment of a goal.

      raise [Failure] if a variable ends up bound to itself.
  *)

  (** {7 Applying tactics} *)

  val apply: (node -> branch) -> node -> branch
  (** [apply tac node]: Apply tactic [tac] to [node].

      This is the work-horse for applying tactics. All other functions
      should call this to apply a tactic.

      Approach:
      {ol
      {- Create a new tag [ticket] and make it the tag of [node].}
      {- Apply tac to [node] getting branch [b].}
      {- If the tag of [b] is not [ticket] then fail.}
      {- Merge the type environment of [b] with [n']. (This may be
      unnecessary.) (Almost certainly unnecessary so not done.)}
      {- Make the original tag of [node] the tag of [b].}
      {- Return the branch [b].}}

      @raise [logicError] on failure.

      The ticket passing method should ensure that it is not possible
      to return an arbitrary branch as the result of a tactic. A
      tactic can only succeed if it returns a branch produced from the
      original node by passing through [apply]. Since the tags of
      nodes and branches can only be set from functions in module
      {!Logic}, this should ensure that a tactic cannot fake a
      result.  *)

  val apply_to_node:
    (node -> branch -> unit)option -> (node -> branch) -> node -> branch
  (** [apply_to_node report tac n]: A wrapper around [apply] to allow
      reporting of the argument and result of a tactic.

      Evaluate [apply tac n] to get a branch [b] then,
      if [report] is given, evaluate [report n b]. Return [b].
  *)

  val apply_to_first:
    (node -> branch -> unit)option
    -> (node -> branch) -> branch -> branch
  (** [apply_to_first report tac (Branch(tg, tyenv, sqnts))]:
      Apply a tactic to the first subgoal in a branch.

      Apply tactic [tac] to [List.hd sqnts] using [apply_to_node].
      Replace original sequent with resulting branch to form the result.

      If [report] is given, apply to first subgoal and the branch
      resulting from application of [tac]. (This is to allow interactive
      proof support to print result of the tactic).

      @raise [No_subgoals] if [sqnts] is empty.
  *)

  val apply_to_each: (node->branch) -> branch -> branch
  (** [apply_to_each tac (Branch(tg, tyenv, sqnts))] Apply tactic
      [tac] to each subgoal in a branch.

      Apply tactic [tac] to each subgoal in [sqnts] using
      [apply_to_node].  Collapse the resulting branches, merging the type
      environments, to form the branch which is returned.

      @raise [No_subgoals] if [sqnts] is empty.
  *)

  val apply_zip: (node -> branch) list -> branch -> branch
  (** [apply_zip tacl branch]: Apply each of the tactics in [tacl] to the
      corresponding subgoal in branch.

      [zip [t1;t2;..;tn] (Branch [g1;g2; ..; gm])] is [Branch([t1 g1; t2
      g2; .. ;tn gn])] (with [t1 g1] first and [tn gn] last). The type
      environment from (t{_ i} g{_ i}) is used when evaluating (t{_ i+1}
      g{_ i+1}). The type environment of the returned branch is the type
      environment of [(tn gn)].

      If there are more subgoals than tactics (n < m) then untreated
      subgoals are attached to the end of the new branch. If there are
      more tactics then subgoals (m < n) then the unused tactics are silently
      discarded.
  *)

  val apply_fold:
    ('a -> node -> ('a * branch))  -> 'a -> branch -> ('a * branch)
(** [apply_fold rl i (Branch(tg, tyenv, sqnts))]: Apply tactic [tac] to
    each subgoal in a branch, folding the initial value across the
    nodes in the branch. Returns [(x, g)] where [x] is the result of
    the fold and [g] the new goal. The new goal is produced in the
    same way as [apply_to_each].

    @raise [No_subgoals] if [sqnts] is empty.
*)

end

type node = Subgoals.node
type branch = Subgoals.branch

val apply_to_goal:
  (node -> branch -> unit)option -> (node -> branch) -> goal -> goal
(** [apply_to_goal report tac goal]: Apply a tactic to a goal.

    Apply tactic [tac] to first subgoal of [goal] using
    [apply_to_first].  Replace original list of subgoals with resulting
    subgoals and merge the type environments.

    If [report] is given, apply to first subgoal and the branch
    resulting from application of [tac]. (This is to allow interactive
    proof support to print result of the tactic).

    @raise [logicError "Invalid Tactic"] if tactic is invalid.
*)

(** {5 Tactics} *)

type tactic = node -> branch

val foreach: tactic -> branch -> branch
(** A synonym for {!Logic.Subgoals.apply_to_each}. [foreach tac b]
    applies [tac] to each subgoal in [b], returning all subgoals.
*)

val first_only: tactic -> branch -> branch
(** A synonym for {!Logic.Subgoals.apply_to_first}.  [first_only tac
    b] applies [tac] to the first subgoal of [b], merging the
    resulting subgoals with the other subgoals of [b].
*)

(** {7 Support for rewriting} *)

(** The type of rules to use with rewriting in the logic.
*)
type rr_type =
  | RRThm of thm   (** A theorem *)
  | ORRThm of thm * Rewrite.order (** An ordered theorem *)
  | Asm of label  (** The label of an assumption *)
  | OAsm of label * Rewrite.order
(** The label of an ordered assumption *)

type plan = rr_type Rewrite.plan
(** The type of rewrite plans *)

(** {7 Tactics and Conversions} *)

(** Tactics which implement the rules of the logic. *)
module Tactics:
sig

  val changes: node -> Changes.t
  (** Utility function to extract changes from a node for use in tactics. *)

  val branch_changes: branch -> Changes.t
  (** Utility function to extract changes from a node for use in tactics. *)

  val set_changes: branch -> Changes.t -> branch
  (** Utility function to set the change information reported by a branch. *)

  (** {5 Manipulating Assumptions and Conclusions} *)

  val lift_asm: label -> tactic
  (** [lift_asm l sqnt]: Move assumption with label [l] to top of
      the assumptions of subgoal [sqnt].

      @raise [Not_found] if no assumption has label [l].

      {L
      asms1, A{_ l}, asms2 |- c
      ---->
      A{_ l}, asms1, asms2 |- c
      }

      info: [goals = [], aforms=[l], cforms=[], terms = []]
  *)

  val lift_concl: label -> tactic
  (** [lift_concl l sqnt]: Move conclusion with label [l] to top
      of the conclusions of subgoal [sqnt].

      @raise [Not_found] if no conclusion has label [l].

      {L
      a |- concls1, C{_ l}, concls2
      ---->
      a |- C{_ l}, concls1, concls2
      }

      info: [goals = [], aforms=[], cforms=[l], terms = []]
  *)

  val lift: label -> tactic
  (** [lift l sqnt]: Lift formula with label [l] to the top of
      assumption/conclusions. [lift] tries [lift_asm] then tries
      [lift_concl]. Doesn't change the formula tag.  *)

  val copy_asm: label -> tactic
  (** [copy_asm l]: Copy assumption [l].

      {L
      A{_ l}, asms|- C
      ---->
      A{_ l'}, A{_ l}, asms |- C
      }

      info: [goals = [], aforms=[l'], cforms=[], terms = []]
  *)

  val copy_cncl: label -> tactic
  (** [copy_cncl l]: Copy conclusion [l]

      {L
      A |- C{_ l}, concls
      ---->
      A |- C{_ l'}, C{_ l}, concls
      }

      info: [goals = [], afroms=[], cforms=[l'], terms = []]
  *)

  (*** rotate assumptions conclusions ***)

  val rotate_asms: tactic
  (** [rotate_asm]: Rotate assumptions.

      {L
      A1, asms |- C   ---->  asms, A1 |- C
      }

      info: [goals = [], aforms=[], cforms=[], terms = []]
  *)

  val rotate_cncls: tactic
  (** [rotate_cncls]: Rotate conclusions.

      {L
      A|- C1, concls
      ---->
      A|-  concls, C1
      }

      info: [goals = [], aforms=[], cforms=[], terms = []]
  *)

  val deleteA: label -> tactic
  (** [deleteA l sq]: Delete assumption [l] from subgoal [sq].

      {L
      A{_ l}, asms |- concls ----> asms |- concls
      }

      info: [goals = [], aforms=[], cforms=[], terms = []]
  *)

  val deleteC: label -> tactic
  (** [delete l sq]: Delete conclusion [l] from subgoal [sq].

      If [l] is a conclusion
      {L
      asms |- C{_ l}, concls ----> asms |- concls
      }

      info: [goals = [], aforms=[], cforms=[], terms = []]
  *)

  (** {5 Logic Rules}  *)

  val skip: tactic
  (** [skip]: The do nothing tactic.

      {L
      asms |- concls ----> asms |- concls
      }

      info: [goals = [], aforms=[], cforms=[], terms = []]

      Useful for turning a node into a branch (e.g. for recursive
      functions using {!Logic.foreach})
  *)

  val cut: thm -> tactic
  (** [cut th sq]: add theorem [th] to assumptions of [sq].

      {L
      asms |- concls ----> th{_ l}, asms |- concls
      }

      info: [goals = [], aforms=[l], cforms=[], terms = []]
  *)

  val basic: label -> label -> tactic
  (** [basic i j sq]: solve the subgoal if assumption [i] is
      alpha-equal to conclusion [j].

      {L
      A{_ i}, asms |- C{_ j}, concls

      ----> (if A{_ i} =alpha= C{_ j})

      |- true
      }

      info: [goals = [], aforms=[], cforms=[], terms = []]
  *)

  val conjA: label -> tactic
  (** [conjA l sq]: Eliminate the conjunction at assumption [l].

      {L
      (A/\B){_ l1}, asm |- concl
      ---->
      A{_ l1}, B{_ l2}, asm |- concl
      }

      info: [goals = [], aforms=[l1, l2], cforms=[], terms = []]
  *)

  val conjC: label -> tactic
  (** [conjC l sq]: Eliminate the conjunction at conclusion [l].


      {L
      g:\[asms |- (A/\B){_ l}, concls\]

      ---->

      g1:\[asms |- A{_ l}, concls\]; g2:\[asms |- B{_ l}, concls\]
      }

      info: [goals = [g1; g2], aforms=[], cforms=[l], terms = []]
  *)

  val disjA: label -> tactic
  (** [disjA l sq]: Eliminate the disjunction at assumption [l].


      {L
      g:\[(A\/B){_ l}, asms |- concls\]

      ---->

      g1:\[A{_ l}, asms |- concls\]; g2:\[asms |- B{_ l}, concls\]
      }

      info: [goals = [g1; g2], aforms=[l], cforms=[], terms = []]
  *)

  val disjC: label -> tactic
  (** [disjC l sq]: Eliminate the disjunction at conclusion [l].

      {L
      (asm |- A\/B){_ l1}, concl
      ---->
      asm |- A{_ l1}, B{_ l2}, concl
      }

      info: [goals = [], aforms=[], cforms=[l1, l2], terms = []]
  *)

  val negA: label -> tactic
  (** [negA l sq]: Elminate the negation at assumption [l].

      {L
      (~A){_ l}, asms |- concl
      ---->
      asms |- A{_ l}, concl
      }

      info: [goals = [], aforms=[], cforms=[l], terms = []]
  *)

  val negC: label -> tactic
  (** [negC l sq]: Elminate the negation at conclusion [l].

      {L
      asms |- (~A){_ l}, concl
      ---->
      A{_ l}, asms |- concl
      }

      info: [goals = [], aforms=[l], cforms=[], terms = []]
  *)


  val implA: label -> tactic
  (** [implA l sq]: Eliminate the implication at assumption [l].

      {L
      g:\[(A => B){_ l}, asms |- concls\]

      ---->

      g1:\[asms |- A{_ l}, concls\]; g2:\[B{_ l}, asms |- concls\]
      }

      info: [goals = [g1; g2], aforms=[l], cforms=[l], terms = []]
  *)

  val implC: label -> tactic
  (** [implC l sq]: Elminate the implication at conclusion [l].

      {L
      asms |- (A => B){_ l}, concl
      ---->
      A{_ l1}, asms |- B{_ l}, concl
      }

      info: [goals = [], aforms=[l1], cforms=[l], terms = []]
  *)

  val allA: Term.term -> label -> tactic
  (** [allA t l sq]: Elminate the universal quantifier at
      assumption [l], instantiating it with [t].


      {L
      (! x. A){_ l}, asms |- concls

      ---->

      (A\[t/x\]){_ l}, asms |- concls
      }

      info: [goals = [], aforms=[l], cforms=[], terms = []]
  *)

  val allC: label -> tactic
  (** [allC l sq]: Elminate the universal quantifier at conclusion
      [l].


      {L
      asms |- (! x. A){_ l}, concls

      ----> (create a skolem constant c)

      asms |- (A\[c/x\]){_ l}, concls
      }

      info: [goals = [], aforms=[], cforms=[l], terms = [c]]
  *)

  val existA: label -> tactic
  (** [existA l sq]: Elminate the existential quantifier at
      assumption [l].

      {L
      (? x. A){_ l}, asms |- concls

      ----> (create a skolem constant c)

      (A\[c/x\]){_ l}, asms |- concls
      }

      info: [goals = [], aforms=[l], cforms=[], terms = [c]]
  *)

  val existC: Term.term -> label -> tactic
  (** [existC t l sq]: Elminate the existential quantifier at
      conclusion [l], instantiating it with [t].

      {L
      asms |- (? x. A){_ l}, concls

      ---->

      asms |- (A\[t/x\]){_ l}, concls
      }

      info: [goals = [], aforms=[], cforms=[l], terms = []]
  *)

  val trueC: label -> tactic
  (** [trueC i sq]: Truth solves the goal.

      {L
      asms |- true, concls
      ---->
      |- true
      }

      info: [goals = [], aforms=[], cforms=[], terms = []]
  *)


  val rewrite_intro:
    (rr_type)Rewrite.plan -> Term.term -> tactic
  (** [rewrite_intro ctrl plan trm sq]: Introduce an equality
      established by rewriting term [trm] with [plan].

      The rewriting plan is made up of theorems or the labels of the
      assumptions to rewrite with. The tactic fails if any of the rules
      are out of scope, a label to a non-existant assumption or not an
      equality.

      {L
      asms |- concl
      ---->
      (trm = T){_ l}, asms|- concl
      }

      info: [goals = [], aforms=[l], cforms=[], terms = []]
  *)

  val substA: label list -> label -> tactic
  (** [substA eqs l sq]: Substitute, using the assumptions
      in [eq], into the assumption [l].  The assumptions in [eq]
      must all be equalities of the form [L=R]. The substitution is
      A{_ l}\[R1, R2, ..., Rn/L1, L2, ..., Rn\]. The substitution
      is based on alpha-equality rather than syntactic equality.

      {L
      A{_ l}, asms |- concl

      ---->

      (A\[R1, R2, ..., Rn/L1, L2, ..., Rn\]){_ l}, asms|- concl
      }

      info: [goals = [], aforms=[l], cforms=[], terms = []]

      Silently discards the labels of non-existant assumptions in [eqs].
  *)

  val substC: label list -> label -> tactic
  (** [substC eqs l sq]: Substitute, using the assumptions
      in [eq], into the conclusion [l].  The assumptions in [eq]
      must all be equalities of the form [L=R]. The substitution is
      C{_ l}\[R1, R2, ..., Rn/L1, L2, ..., Rn\]. The substitution
      is based on alpha-equality rather than syntactic equality.

      {L
      asms |- C{_ l}, concl

      ---->

      asms|- (C\[R1, R2, ..., Rn/L1, L2, ..., Rn\]){_ l}, concl
      }

      info: [goals = [], aforms=[], cforms=[l], terms = []]

      Silently discards the labels of non-existant assumptions in [eqs].
  *)

  val nameA: string -> label -> tactic
  (** [nameA l name sq]: Rename the assumption labelled [l]
      as [name].  The previous name and tag of [l] are both
      discarded.

      {L
      A{_ l1}, asms |- concl

      ----> l2 a tag created from name

      A{_ l2}, asms|- concl
      }

      info: [goals = [], aforms=[l2], cforms=[], terms = []]

      Fails if an assumption or conclusion is already named [name].
  *)

  val nameC: string -> label -> tactic
(** [nameC name l sq]: Rename the conclusion labelled [l] as
    [name].  The previous name and tag of [l] are both discarded.

    {L
    asms |- C{_ l1}, concl

    ----> l2 a tag created from name

    asms|- C{_ l2}, concl
    }

    info: [goals = [], aforms=[], cforms=[l2], terms = []]

    Fails if an assumption or conclusion is already named [name].
*)

end

(** Conversions are functions constructing theorems which express an
    equality.

    Although the logic is based on tactics, some rules are better
    described as equalities (which can be used for rewriting).

    Module {!Logic.Conv} also provides a very small number of basic
    conversions which are generally useful and which would be too
    inefficient if written in terms of tactics.
*)
(*
type conv = Scope.t -> Term.term -> thm
*)
type conv = Term.term -> thm

module Conv:
sig

  val beta_conv: Scope.t -> conv
  (** [beta_conv scp term]: Apply beta reduction to [term].

      Returns |- ((%x: F) y) = F'
      where F' = F\[y/x\]

      Note that this beta reduces throughout the term, not just the top
      level.
  *)

  val rewrite_conv: (thm) Rewrite.plan -> Scope.t -> conv
(** [rewrite_conv plan scp trm]: rewrite term [trm] according to
    [plan] in scope [scp].

    Returns |- trm = X
    where [X] is the result of rewriting [trm]

    Discards any rule which is not a theorem or an ordered theorem.

    This conversion could be written using the rewriting tactics but
    this would require two sets of rewriting. The first to construct
    the term [X] on the rhs of the equality and the second when the
    rewrite tactic is invoked. By contrast, [rewrite_conv] only does
    one set of rewriting.
*)
end

(** {5 Declarations and Definitions} *)

(** Support for defining terms and subtypes. *)
module Defns:
sig

  (** {7 Representation of Declarations and definitions} *)

  type cdefn
  (** Checked term definitions. Elements of [cdefn] can be assumed to be
      correctly defined.
  *)

  (** Checked subtype definitions. Elements of [ctypedef] can be
      assumed to be correctly defined.  *)
  type ctypedef =
      {
        type_name: Ident.t;       (* name of new type *)
        type_args: string list;   (* arguments of new type *)
        type_base: Gtype.t;   (* the base type *)
        type_rep: cdefn;          (* representation function *)
        type_abs: cdefn;          (* abstraction function *)
        type_set: Formula.t;      (* defining set *)
        rep_type: thm;
        rep_type_inverse: thm;
        abs_type_inverse: thm
      }

(** {7 Representation for permanent storage} *)

  (** The representation of a checked definition for permanent storage.
  *)
  type saved_cdefn

  (** The representation of a checked subtype definition for permanent
      storage.  *)
  type saved_ctypedef =
      {
        stype_name: Ident.t;             (* name of new type *)
        stype_args: string list;         (* arguments of new type *)
        stype_base: Gtype.stype;
        stype_rep: saved_cdefn;          (* representation function *)
        stype_abs: saved_cdefn;          (* abstraction function *)
        stype_set: Formula.saved_form;   (* defining set *)
        srep_type: saved_thm;
        srep_type_inverse: saved_thm;
        sabs_type_inverse: saved_thm
      }

  val to_saved_cdefn: cdefn -> saved_cdefn
  (** Convert a definition to the representation for permanent
      storage. *)
  val from_saved_cdefn: Scope.t -> saved_cdefn -> cdefn
  (** Convert a definition from the representation for permanent
      storage. *)

  (** {7 Term definition and declaration} *)

  val is_termdef: cdefn -> bool
  (** Recogniser for term definitions. *)

  val dest_termdef: cdefn ->
    Ident.t * Gtype.t * thm
  (** Get the components of a certified definition. *)

  val mk_termdef:
    Scope.t
    -> (Ident.t * Gtype.t)
    -> Term.term list -> Term.term
    -> cdefn
  (** [mk_termdef scp i args trm]: Make a certified definition.

      Constructs the definition [! args. (i args) = trm].
  *)

  val is_termdecln: cdefn -> bool
  (** Recogniser for term declarations. *)

  val dest_termdecln: cdefn
    -> Ident.t * Gtype.t
  (** Get the components of a term declaration.  *)

  val mk_termdecln:
    Scope.t -> string -> Gtype.t -> cdefn
  (** [mk_termdecln scp name ty]: Declare identifier [name] of type
      [ty] in scope [scp].  Fails if identifier [name] is already
      defined in [scp] or if [ty] is not well defined.  *)

  (** {7 Type definition: Declarations and Aliases}

      Type declarations and aliases are handled together since both
      just introduce names of types. The only difference between an
      declaration and alias is that an alias has a definition (the
      type being aliased) while a declaration does not.  *)

  val is_typealias: cdefn -> bool
  (** Recogniser for definition of a type declaration or alias. *)
  val dest_typealias: cdefn ->
    Ident.t * string list * Gtype.t option
  (** Get the components of a type declaration or alias.  *)
  val mk_typealias: Scope.t
    -> string -> string list -> Gtype.t option -> cdefn
  (** [mk_typealias scp n args d]: Make a type declaration or alias.

      {ul
      {- Check n doesn't exist already.}
      {- check all arguments in args are unique.}
      {- if [d = Some x] then [n] is being defined as an alias for [x].
      so check x is well defined (all constructors exist and variables
      are in the list of arguments)}}
  *)

  (** {7 Type definition: Subtypes} *)

  val is_subtype: cdefn -> bool
  (** Recognisers for subtype definition. *)
  val dest_subtype: cdefn -> ctypedef
  (** Get the components of a subtype definition. *)

  val prove_subtype_exists:
    Scope.t -> Term.term -> thm -> thm
  (** [prove_subtype_exists scp setp thm]: Prove the existence
      theorem of a subtype.  Use [thm] to prove the goal <<
      ?x. setp x >> (built by {!Defn.mk_subtype_exists}.  *)
  val mk_subtype:
    Scope.t -> string -> string list
    -> Gtype.t -> Term.term -> string -> string
    -> thm
    -> cdefn
(**
   [mk_subtype scp name args d setP rep abs]: Define a subtype.

   {ul
   {- Check name doesn't exist already.}
   {- Check all arguments in args are unique.}
   {- Check def is well defined (all constructors exist and variables
   are in the list of arguments).}
   {- Ensure setP has type (d -> bool).}
   {- Declare rep as a function of type (d -> n).}
   {- make subtype property from setp and rep.}}
*)

(** {7 Pretty Printing} *)

  val print_cdefn: Printers.ppinfo -> cdefn -> unit
(** Print a definition *)

end

(** {5 Pretty Printing} *)

val print_sqnt: Printers.ppinfo -> Sequent.t -> unit
(** [print_sqnt ppinfo sq]: Print sequent [sq] using PP info [ppinfo].
*)

val print_node: Printers.ppinfo -> node -> unit
(** [print_node ppinfo n]: Print node [n] using PP info [ppinfo].
*)

val print_branch: Printers.ppinfo -> branch -> unit
(** [print_branch ppinfo branch]: Print branch [branch] using PP info
    [ppinfo].
*)
