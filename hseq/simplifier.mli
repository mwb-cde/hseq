(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** The simplifier engine. *)

open Tactics
open Rewritekit
open Rewrite

(** {5 Errors} *)

val error: string -> Term.term list -> exn
val add_error: string -> Term.term list -> exn -> exn

exception No_change
(** Raised if the simplifier makes no changes. *)

(** {5 Simplifier Data} *)

type control = Rewrite.control
(** Passed to the simplifier to control rewriting. *)

(** Data used by the simplifier. *)
module Data:
sig

  type loopDB = Term.term Net.net
  (** Structure used to store terms for looping rewriting
      detection *)

  (** Information needed during simplification. *)
  type t = {
    simpset: Simpset.simpset;
    (** The simpset being used. Assumptions may be added to this
        during the course of simplification.  *)

    cond_tac: t -> Logic.ftag_ty -> Tactics.tactic;
    (** Tactic used to prove conditions of rewrite rules. Default:
        [skip].  *)

    control: Rewrite.control;
    (** Rewrite control ([direction] is ignored). *)

    conds: int option;
    (** Max. no. of conditions to try and prove at once. *)

    rr_depth: int option;
    (** Max. no. of rewrite rules to apply at one level. *)

    asms: Logic.ftag_ty list;
    (** Assumptions generated during the course of simplification. *)

    visited: Logic.ftag_ty list;
    (** Formulas visited during the course of simplification *)

    exclude: Logic.ftag_ty list;
    (** Formulas not to be used as a rewrite rule. *)

    loopdb: loopDB
  (** loopdb: Terms already rewritten. *)
  }

  val make:
    (Simpset.simpset
     * (t -> Logic.ftag_ty -> Tactics.tactic)
     * control
     * int option * int option
     * Logic.ftag_ty list * Logic.ftag_ty list
     * Logic.ftag_ty list
     * Logic.rr_type list)
    -> t
  (** Make simp data. *)

  val set_simpset: t -> Simpset.simpset -> t
  (** Set the simpset. *)

  val set_tactic: t -> (t -> Logic.ftag_ty -> Tactics.tactic) -> t
  (** Set the condition prover tactic. *)

  val set_conds: t -> int option -> t
  (** Set the maximum number of conditions to try in one go. *)

  val set_conds_val: t -> int -> t
  (** Set the maximum number of conditions to try in one go. *)

  val set_rr_depth: t -> int option -> t
  (** Set the maximum number of rewrite rules to apply at one level. *)

  val set_rr_depth_val: t -> int -> t
  (** Set the maximum number of rewrite rules to apply at one level. *)

  val set_control: t -> control -> t
  (** Set the rewriting control. *)

  val set_asms: t -> Logic.ftag_ty list -> t
  (** Set the assumptions list. *)

  val set_visited: t -> Logic.ftag_ty list -> t
  (** Set the visited formulas list. *)

  val set_exclude: t -> Logic.ftag_ty list -> t
  (** Set the excluded formulas list. *)

  val get_simpset: t -> Simpset.simpset
  (** Get the simpset to use. *)

  val set_loopdb: t -> loopDB -> t
  (** Set the loopdb. *)

  val get_loopdb: t -> loopDB
  (** Get the loopdb *)

  val add_loopdb: t -> Term.term -> t
  (** Add a term to the loopdb *)

  val mem_loopdb: Scope.t -> t -> Term.term -> bool
  (** Test whether a term is alpha-equal to a term in the loopdb *)

  val get_tactic: t -> (t -> Logic.ftag_ty -> Tactics.tactic)
  (** Get the condition prover tactic. *)

  val get_control: t -> control
  (** Get the rewriting control. *)

  val get_asms: t -> Logic.ftag_ty list
  (** Get the list of assumptions. *)

  val get_visited: t -> Logic.ftag_ty list
  (** Get the list of visited formulas. *)

  val get_exclude: t -> Logic.ftag_ty list
  (** Get the exclusion list. *)

  val add_asm: t -> Logic.ftag_ty -> t
  (** Add an assumption. *)

  val dec_cond_depth: t -> t
  (** Decrement the condition limit. *)

  val add_simp_rule: t -> Simpset.rule -> t
  (** Add a simp rule as a rewrite rule. *)

  val default_cond_depth: int option ref
  (** The default condition depth **)

  val default_rr_depth: int option ref
  (** The default rewrite depth **)

  val default: t
(** Make the default simp data. *)
end

(** {5 Utility functions} *)

val is_conditional: Simpset.rule -> bool
(** Test whether a rule is conditional. *)

val is_none: 'a option -> bool
(** [is_none x]: true if [x = None], false otherwise. *)

val is_excluded: Logic.ftag_ty list -> Logic.Sequent.t -> Logic.rr_type -> bool
(** [is_excluded excluded sqnt rl]: True if rewrite rule [rl] is an
    assumption in the excluded list.
*)

(** {5 Utility Tactics} *)

val cleanup: bool ref
(** Whether the clean_up_tac should do anything. Useful for debugging. *)

val clean_up_tac: Data.t -> Tactics.tactic
(** [cleanup_tac]: If [!cleanup=true], assumptions added to a goal by
    the simplifier will be removed after simplification.
*)

val copyA_inst_tac:
  Term.term list -> Logic.label
  -> Tactics.tactic
(** [copyA_inst_tac info vals x]: Copy assumption [x], instantiate the
    copy with [vals]. info: aformulas = [x1], where [x1] is the tag of
    the new assumption.  Fails if there are more terms in [vals] then
    variables in [x].
*)

val cut_rr_rule:
  Term.term list -> Logic.rr_type -> tactic
(** [cut_rr_rule info vals t g]: Cut rule [t] into goal [g],
    instantiating with [vals].  If [t] is a theorem, it is cut into
    the goal.  If [t] is an assumption, it is copied. Fails if there
    are more terms in [vals] then there are quantifiers in [t].

    info: aforms = [[x]] where [x] is the tag of the new assumption.
*)

val simp_rewrite_tac:
  bool -> Logic.rr_type Rewrite.plan
  -> Term.term -> Logic.label
  -> tactic
(** [simp_rewrite_tac is_concl plan term lbl]: Local interface
    to the main rewriting tactics. If [is_concl] is true, call
    [Tactics.pure_rewriteC plan ~term:trm lbl goal] otherwise
    call [Tactics.pure_rewriteA plan ~term:trm lbl goal].
*)

(** {5 Conditional rule tactics} *)

type tag_pair = (Logic.ftag_ty * Logic.ftag_ty)
val prep_cond_tac:
  Data.t -> Term.term list -> Logic.rr_type
  -> (Data.t * tag_pair * tag_pair) Tactics.data_tactic
(** [prep_cond_tac cntrl ret values thm g]: Prepare [thm], which is
    assumed to be a conditional rules, for proof of condition and use.

    Cut [thm] into the sequent, instantiate with [values].  Apply
    [implA] to get two subgoals, tagged [(cnd_gltg, rl_gltg)] with
    condition in [cnd_gltg] tagged [cnd_ftg] and rewrite-rule in
    [rl_gltg] tagged [rl_ftg].  Add [rl_ftg] to [cntrl], getting
    [ncntrl].

    Returns [ret=(ncntrl, (cnd_gltg, rl_gltg), (cnd_ftg, rl_ftg))]
*)

val prove_cond_tac:
  Data.t -> Term.term list -> Simpset.rule
  -> (Data.t * Logic.rr_type)Tactics.data_tactic
(** [prove_cond_tac cntrl tac values entry g]: Prepare a conditional
    simp rule [entry] for use in rewriting.

    Use [prep_cond_tac] add the rule to the goal to create a subgoal
    for the condition. Use tactic [cntrl.cond_tac] to prove the
    condition, failing if it fails to prove the condition.

    Return [ret=(ncntrl, rl)] where [ncntrl] is the new simp data and
    [rl] the rewrite rule built from the new theorem/assumption.
*)

(** {5 Simplifier functions} *)

type match_data =
    {
      (** Simplifier data *)
      cntrl: Data.t;
      (** Type environment *)
      tyenv: Gtype.Subst.t;
      (** Quantifier environment *)
      qntenv: Term.Subst.t;
    }

(** Data passed between the simplifier functions *)

val match_rewrite:
  Scope.t
  -> Gtype.Subst.t
  -> Term.Subst.t
  -> Simpset.rule
  -> Term.term
  -> (Logic.rr_type * Gtype.Subst.t * Term.Subst.t * Term.term)
(** [match_rewrite scp tyenv qntenv trmenv rule trm]: Try to match lhs
    of [rule] with [trm] in type envivornment [tyenv] and term bindings
    [trmenv]. Return rhs of [rule], instantiated with the binding from
    the match, and the type and term environments that made the match
    successful. Raise [Failure] on failure.
*)

val find_basic:
  match_data -> Simpset.rule -> Term.term ->
  (match_data * Term.term * Logic.rr_type) Tactics.data_tactic
(** [find_basic ret data rl trm g]: Try to match simp rule [rl] with
    term [trm] in goal [g], with [data = (cntrl, tyenv, qntenv)]. If
    [rl] matches but is conditional, try to prove the condition using
    tactic [cntrl.cond_tac], adding the rule to the goal assumptions.

    Returns [ret = (ndata, ntrm, rr)] where [ndata = (ncntrl, ntyenv,
    qntenv)], [ncntrl] is the updated simplifier data, [nytenv] is the
    type environment made by the matching, [ntrm] is the result of
    rewriting [trm] and [rl] the rewrite rule to add to the list being
    compiled.
*)

val find_match_tac:
  match_data -> Term.term ->
  (match_data * Term.term * Logic.rr_type) Tactics.data_tactic
(**
   [find_match_tac ret data trm g]: Find a rule in simpset [set] which
   matches term [trm] in goal [g], with [data = (cntrl, tyenv,
   qntenv)]. If found, rewrite [trm] with the rule.

   Returns [ret = (ndata, ntrm, rr)] where [ndata = (ncntrl, ntyenv,
   qntenv)], [ncntrl] is the updated data, [nytenv] is the type
   environment made by the matching, [ntrm] is the result of rewriting
   [trm] with [rl] and [rl] the rewrite rule to add to the list being
   compiled.

   @raise [No_change] and set [ret:=None] if no matches.
*)

val find_all_matches_tac:
  match_data -> Term.term ->
  (match_data * Term.term * Logic.rr_type list)
    Tactics.data_tactic
(** [find_all_matches ret (cntrl, tyenv, qntenv) trm g]: Find all
    rules in simpset [cntrl.set] which can be used to rewrite term
    [trm] in goal [g].

    Returns new simp data, the new type environment and the rewritten
    term. The new simp data is built by adding the rules used to
    rewrite the term, in the order they are applied.
*)

val find_term_bu_tac:
  match_data -> Term.term
  -> (match_data * Term.term
      * Logic.rr_type Rewrite.plan) Tactics.data_tactic
(** [find_term_bu_tac ret (ctrl, tyenv, qntenv) trm g]: Traverse term
    [trm], bottom-up, constructing a rewrite plan.

    Return [ret = (ncntrl, ntyenv, ntrm, plan)], where [ncntrl] is the
    new simp data, [ntyenv] the new type-environment, [ntrm] the term
    resulting from simplification and [plan] the constructed rewriting
    plan.
*)

val find_subterm_bu_tac:
  match_data-> Term.term
  -> (match_data * Term.term
      * Logic.rr_type Rewrite.plan) Tactics.data_tactic
(** [find_subterm_bu_tac ret (ctrl, tyenv, qntenv) trm g]: Make a plan
    to rewrite, bottom-up, the subterms of [trm].

    Return [ret = (ncntrl, ntyenv, ntrm, plan)], where [ncntrl] is the
    new simp data, [ntyenv] the new type-environment, [ntrm] the term
    resulting from simplification and [plan] the constructed rewriting
    plan.

    This is a companion function to {!Simplifier.find_term_bu_tac}.
*)

val find_term_td_tac:
  match_data -> Term.term
  -> (match_data * Term.term
      * Logic.rr_type Rewrite.plan)Tactics.data_tactic
(** [find_term_td_tac ret (ctrl, tyenv, qntenv) trm g]: Traverse term
    [trm], top-down, constructing a rewrite plan.

    Return [ret=(ncntrl, ntyenv, ntrm, plan)], where [ncntrl] is the
    new simp data, [ntyenv] the new type-environment, [ntrm] the term
    resulting from simplification and [plan] the constructed rewriting
    plan.
*)

val find_subterm_td_tac:
  match_data-> Term.term
  -> (match_data * Term.term
      * Logic.rr_type Rewrite.plan)Tactics.data_tactic
(** [find_subterm_td_tac ret (ctrl, tyenv, qntenv) trm g]: Make a plan
    to rewrite, top-down, the subterms of [trm].

    Return [ret = (ncntrl, ntyenv, ntrm, plan)], where [ncntrl] is the
    new simp data, [ntyenv] the new type-environment, [ntrm] the term
    resulting from simplification and [plan] the constructed rewriting
    plan.

    This is a companion function to {!Simplifier.find_term_bu_tac}.
*)

val basic_simp_tac:
  Data.t -> Logic.ftag_ty -> (Data.t)Tactics.data_tactic
(** [basic_simp_tac data ret tag goal]: Main interface to the basic
    simplifier functions.

    Simplify formula tagged [tag] in [goal]:
    {ul
    {- Descend top-down or bottom-up into formula, at each level collect
    rewrite rules which can be used to rewrite the term.}
    {- Use collected rules to rewrite the formula.}}

    Doesn't clean up afterwards.

    Returns [ret = (ndata, ntag)] where [ndata] is [data] updated with
    the rules used to rewrite the formula and [ntag] is the new tag of
    the formula.

    raise [No_change] if no rules can be found.
*)

(** {5 Derived simplifier functions} *)

val simp_prep_tac:
  Data.t -> Logic.label -> (Data.t)Tactics.data_tactic
(** [simp_prep_tac data ret lbl g]: Prepare goal [g] for simplifying
    formula [lbl].

    Returns [ret = ncontrol] where [ncontrol] is the new control
    recording formulas added/modified by simp_prep_tac

    Currently this does nothing except strip the quantifiers off
    formula [lbl].

    Always succeeds.
*)

val cond_prover_tac:
  Data.t -> Logic.ftag_ty -> Tactics.tactic
(** [cond_prover_tac ctrl tg g]: The tactic used to prove the
    conditions of rewrite rules.

    Apply [simp_prep_tac] then [basic_simp_tac] then apply
    [Logic.Tactics.trueR] to solve goal.  *)

(** {5 Debugging information} *)

val check_add_loop:
  Scope.t -> Data.t -> Term.term ->  Data.t
(** [check_add_loop scp cntrl trm]: Test whether term [trm] is in
    [cntrl.loopDB]. If it is, raise [Failure] otherwise add it to
    [cntrl.loopDB].

    Used to test for loops during simplification.
*)

val check_change: ('a)plan -> unit
(**
    [check_change p]: Test whether plan [p] does anything. Raise
    [No_change] if it does not.
*)

val check_change2: ('a)plan -> ('a)plan -> unit
(** [check_change2 p1 p2]: Test either plan [p1] or plan [p2] does
    anything. Raise [No_change] if both do nothing.
*)

val cond_prover_trueC:
  Logic.label -> Tactics.tactic
val cond_prover_worker_tac:
  Data.t -> Logic.ftag_ty -> (Data.t)Tactics.data_tactic
val is_excluded:
  Logic.ftag_ty list -> Logic.Sequent.t -> Logic.rr_type -> bool
