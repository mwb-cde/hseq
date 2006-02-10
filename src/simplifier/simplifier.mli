(*-----
   Name: simplifier.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(**
   The simplifier engine.
 *)

open Tactics

(** {5 Errors} *)

class simpError :
    string ->
      Basic.term list ->
	object
          val trms : Basic.term list
          method get : unit -> Basic.term list
          method msg : unit -> string
          method print : Printer.ppinfo -> unit
	end
val error : string -> Basic.term list -> exn
val add_error : string -> Basic.term list -> exn -> exn


(** {5 Utility functions} *)

(*
   val strip_rrs : Logic.rr_type list -> Basic.term list
 *)
(**
   [strip_rrs]: Prepare for direct rewriting of term. (For tests only.)
 *)

val is_conditional: Simpset.rule -> bool
(** Test whether a rule is conditional. *)

val is_none: 'a option -> bool
(** [is_none x]: true if [x=None], false otherwise. *)

val is_excluded: Tag.t list -> Logic.Sequent.t -> Logic.rr_type -> bool
(**
   [is_excluded excluded sqnt rl]: True if rewrite rule [rl] is an
   assumption in the excluded list.
 *)

(** {5 Simplifier Data} *)

exception No_change
(** Raised if the simplifier makes no changes. *)

type control = Rewrite.control
(** Passed to the simplifier to control rewriting. *)

(** Data used by the simplifier. *)
module Data:
    sig

      type loopDB = Basic.term Net.net
    (** Structure used to store terms for looping rewriting detection *)

(** Information needed during simplification. *)
      type t = {
	  simpset: Simpset.simpset;
(**
   The simpset being used. Assumptions may be added to this during the
   course of simplification.
 *)

	  cond_tac: t -> Tag.t -> Tactics.tactic;
(** 
   Tactic used to prove conditions of rewrite rules. Default: [skip].
 *)

	    control: Rewrite.control;
(** Rewrite control ([direction] is ignored). *)

	    conds : int option;
(** Max. no. of conditions to try and prove at once. *)

	    rr_depth : int option;
(** Max. no. of rewrite rules to apply at one level. *)

	    asms : Tag.t list;
(** Assumptions generated during the course of simplification. *)

	    visited: Tag.t list;
(** Formulas visited during the course of simplification *)

(***
(** asm_pairs: 
   tags of original formulas and the new modified formula
   (in (a, b) a is the tag of the original assumption,
   b is the tag of the formula used as a rewrite rule
 *)
   asm_pairs: (Tag.t*Tag.t) list;
(** concl_pairs: 
   tags of original formulas and the new modified formula
   (in (a, b) a is the tag of the original conclusion
   b is the tag of the formula used as a rewrite rule
 *)
   concl_pairs: (Tag.t*Tag.t) list;
 ***)

	    exclude: Tag.t list;
(** Formulas not to be used as a rewrite rule. *)

	    rules : Logic.rr_type list;
(**  Rewrite rules chosen by the simplifier. *)

	    loopdb: loopDB
(** loopdb: Terms already rewritten. *)
	}

      val make: 
	  (Simpset.simpset
	     * (t -> Tag.t -> Tactics.tactic)
	     * control
	     * int option * int option 
	     * Tag.t list * Tag.t list 
	     * Tag.t list 
	     * Logic.rr_type list)
	-> t
(** Make simp data. *)

      val set_simpset : t -> Simpset.simpset -> t
(** Set the simpset. *)

      val set_tactic : t -> (t -> Tag.t -> Tactics.tactic) -> t
(** Set the condition prover tactic. *)

      val set_conds : t -> int option -> t
(** Set the maximum number of conditions to try in one go. *)

      val set_conds_val : t -> int -> t
(** Set the maximum number of conditions to try in one go. *)

      val set_rr_depth : t -> int option -> t
(** Set the maximum number of rewrite rules to apply at one level. *)

      val set_rr_depth_val : t -> int -> t
(** Set the maximum number of rewrite rules to apply at one level. *)

      val set_control: t -> control -> t
(** Set the rewriting control. *)

      val set_asms : t -> Tag.t list -> t
(** Set the assumptions list. *)

      val set_visited : t -> Tag.t list -> t
(** Set the visited formulas list. *)

      val set_exclude : t -> Tag.t list -> t
(** Set the excluded formulas list. *)

      val set_rules : t -> Logic.rr_type list -> t
(** Set the rewrite rules list. *)

      val get_rules : t -> Logic.rr_type list 
(** Get the rewrite rules list. *)

      val get_simpset : t -> Simpset.simpset
(** Get the simpset to use. *)

      val set_loopdb : t -> loopDB -> t
(** Set the loopdb. *)

      val get_loopdb : t -> loopDB
(** Get the loopdb *)

      val add_loopdb: t -> Basic.term -> t
(** Add a term to the loopdb *)

      val mem_loopdb: Scope.t -> t -> Basic.term -> bool
(** 
   Test whether a term is alpha-equal to a term in the loopdb
*)

      val get_tactic : t -> (t -> Tag.t -> Tactics.tactic)
(** Get the condition prover tactic. *)

      val get_control: t -> control
(** Get the rewriting control. *)

      val get_asms : t -> Tag.t list 
(** Get the list of assumptions. *)

      val get_visited : t -> Tag.t list 
(** Get the list of visited formulas. *)

      val get_exclude : t -> Tag.t list 
(** Get the exclusion list. *)

      val add_asm : t -> Tag.t -> t
(** Add an assumption. *)

      val dec_cond_depth : t -> t
(** Decrement the condition limit. *)

      val add_rule : t -> Logic.rr_type -> t
(** Add a rewrite rule. *)

      val add_simp_rule : t -> Simpset.rule -> t
(** Add a simp rule as a rewrite rule. *)

      val default_cond_depth: int option ref
(** The default condition depth **)

      val default_rr_depth: int option ref
(** The default rewrite depth **)

      val default : t
(** Make the default simp data. *)
    end 

(** {5 Utility Tactics} *)

val cleanup: bool ref
(** Whether the clean_up_tac should do anything. Useful for debugging. *)

val clean_up_tac: Data.t -> Tactics.tactic
(**
   [cleanup_tac]: If [!cleanup=true], assumptions added to a goal by
   the simplifier will be removed after simplification. 
 *)

val copyA_inst_tac: 
    ?info:Logic.info -> Basic.term list -> Logic.label
      -> Tactics.tactic
(**
   [copyA_inst_tac info vals x]: Copy assumption [x], instantiate the
   copy with [vals]. info: aformulas = [x1], where [x1] is the tag of
   the new assumption.  Fails if there are more terms in [vals] then
   variables in [x].
 *)

val cut_rr_rule :
    ?info:Logic.info -> Basic.term list -> Logic.rr_type -> tactic
(** 
   [cut_rr_rule info vals t g]: Cut rule [t] into goal [g],
   instantiating with [vals].  If [t] is a theorem, it is cut into the
   goal.  If [t] is an assumption, it is copied. Fails if there are
   more terms in [vals] then there are quantifiers in [t].

   info: aforms = [[x]] where [x] is the tag of the new assumption.
 *)


(** {5 Conditional rewriting} *)

val prep_cond_tac :
    Data.t 
  -> (Data.t * (Tag.t * Tag.t) * (Tag.t * Tag.t)) option ref
    -> Basic.term list -> Logic.rr_type 
      -> Tactics.tactic
(** 
   [prep_cond_tac cntrl ret values thm g]: Prepare [thm], which is
   assumed to be a conditional rules, for proof of condition and use.

   Cut [thm] into the sequent, instantiate with [values].  Apply
   [implA] to get two subgoals, tagged [(cnd_gltg, rl_gltg)] with
   condition in [cnd_gltg] tagged [cnd_ftg] and rewrite-rule in [rl_gltg]
   tagged [rl_ftg].  Add [rl_ftg] to [cntrl], getting [ncntrl].

   Returns [ret=(ncntrl, (cnd_gltg, rl_gltg), (cnd_ftg, rl_ftg))]
 *)


val prove_cond_tac:
    Data.t -> (Data.t * Logic.rr_type) option ref 
      -> Basic.term list -> Simpset.rule
	-> Tactics.tactic
(** 
   [prove_cond_tac cntrl tac values entry g]: Prepare a
   conditional simp rule [entry] for use in rewriting.
   
   Use [prep_cond_tac] add the rule to the goal to create a subgoal
   for the condition. Use tactic [cntrl.cond_tac] to prove the
   condition, failing if it fails to prove the condition.

   Return [ret=(ncntrl, rl)] where [ncntrl] is the new simp data and
   [rl] the rewrite rule built from the new theorem/assumption.
 *)


(** {5 Rewriting terms} *)

val match_rewrite :
    Scope.t 
  -> Gtypes.substitution 
    -> Term.substitution 
      -> Term.substitution 
	-> Simpset.rule
	  -> Basic.term 
	    -> Gtypes.substitution * Term.substitution * Basic.term
(**
   [match_rewrite scp tyenv qntenv trmenv varp lhs rhs trm]: Try to match
   [lhs] with [trm] in type envivornment [tyenv] and term bindings
   [trmenv]. Return [rhs], instantiated with the binding from the
   match, and the type and term environments that made the match
   successful. Raise [Failure] on failure.
 *)

val find_basic :
    Data.t 
  -> (Data.t * Gtypes.substitution * Basic.term * Logic.rr_type) option ref 
    -> Gtypes.substitution 
      -> Term.substitution 
	-> Simpset.rule
	  -> Basic.term 
	    -> Tactics.tactic
(** 
   [find_basic cntrl ret tyenv tac rl trm g]: Try to match simp rule
   [rl] with term [trm] in goal [g]. If [rl] matches but is
   conditional, try to prove the condition using tactic [tac], adding
   the rule to the goal assumptions.

   Returns [ret=(ncntrl, ntyenv, ntrm, rr)] where [ncntrl] is the
   updated data, [nytenv] is the type environment made by the
   matching, [ntrm] is the result of rewriting [trm] with [rl] and
   [rl] the rewrite rule to add to the list being compiled.
 *)

val find_match_tac :
    Data.t 
  -> Gtypes.substitution 
    -> Term.substitution 
      -> (Data.t 
	    * Gtypes.substitution 
	    * Basic.term 
	    * Logic.rr_type) option ref 
	-> Basic.term 
	  -> Tactics.tactic
(**
   [find_match_tac cntrl tyenv ret trm g]: Find a rule in simpset
   [set] which matches term [trm] in goal [g]. If found, rewrite [trm]
   with the rule.

   Returns [ret=(ncntrl, ntyenv, ntrm, rr)] where [ncntrl] is the
   updated data, [nytenv] is the type environment made by the
   matching, [ntrm] is the result of rewriting [trm] with [rl] and
   [rl] the rewrite rule to add to the list being compiled.

   Raise [No_change] and set [ret:=None] if no matches.
 *)

val find_all_matches_tac :
    Data.t 
  -> (Data.t * Gtypes.substitution * Basic.term) option ref
    -> Gtypes.substitution 
      -> Term.substitution 
	-> Basic.term 
	  -> Tactics.tactic
(** 
   [find_all_matches cntrl ret tyenv qntenv trm g]: Find all rules in simpset
   [cntrl.set] which can be used to rewrite term [trm] in goal [g].

   Returns new simp data, the new type environment and the rewritten
   term. The new simp data is built by adding the rules used to
   rewrite the term, in the order they are applied.
 *)

(** {5 Term traversal} *)

val find_rrs_bottom_up_tac:
    Data.t
  -> (Data.t * Gtypes.substitution 
	* Basic.term) option ref
    -> Gtypes.substitution
      -> Term.substitution 
	-> Basic.term
	  -> Tactics.tactic
(**
   [find_rrs_bottom_up ctrl ret tyenv qntenv trm g]: Traverse term [trm],
   bottom-up, finding rewrite rules to apply. 

   Return [ret=(ncntrl, ntyenv, ntrm)], where [ncntrl] is the new simp
   data (with the rewrite rules to apply), [ntyenv] the new
   type-environment and [ntrm] the term resulting from simplification.
 *)

val find_rrs_top_down_tac : 
    Data.t
  -> (Data.t * Gtypes.substitution 
	* Basic.term) option ref
    -> Gtypes.substitution
      -> Term.substitution 
	-> Basic.term
	  -> Tactics.tactic
(**
   [find_rrs_top_down ctrl ret tyenv qntenv trm g]: Traverse term [trm],
   top-down, finding rewrite rules to apply. This is the default.

   Return [ret=(ncntrl, ntyenv, ntrm)], where [ncntrl] is the new simp
   data (with the rewrite rules to apply), [ntyenv] the new
   type-environment and [ntrm] the term resulting from simplification.
 *)


(** {5 Simplifier tactics} *)

val basic_simp_tac :
    Data.t -> Data.t option ref -> Tag.t -> Tactics.tactic
(**
   [basic_simp_tac data ret tag goal]:

   Simplify formula tagged [tag] in [goal]: 
   {ul
   {- Descend top-down or bottom-up into formula, at each level collect
   rewrite rules which can be used to rewrite the term.}
   {- Use collected rules to rewrite the formula.}}

   Doesn't clean up afterwards.

   Returns [ret = ndata] where [ndata] is [data] updated with the
   rules used to rewrite the formula.

   raise [No_change] if no rules can be found.
 *)

val cond_prover_tac:
    Data.t -> Tag.t -> Tactics.tactic
(**
   [cond_prover_tac ctrl tg g]: The tactic used to prove the conditions of
   rewrite rules.

   Apply [simp_prep_tac] then [basic_simp_tac].
   Then apply [Logic.Tactics.trueR] to solve goal.
 *) 

val simp_prep_tac :
    'a -> 'a option ref -> Logic.label -> Tactics.tactic
(**
   [simp_prep_tac data ret lbl g]: Prepare goal [g] for simplifying
   formula [lbl].

   Returns [ret = ncontrol] where [ncontrol] is the new control
   recording formulas added/modified by simp_prep_tac

   Currently this does nothing except strip the quantifiers off
   formula [lbl].

   Always succeeds.
 *)

(** {5 Debugging information} *)

val cond_prover_trueC: 
    Logic.info option -> Logic.label -> Tactics.tactic
val cond_prover_worker_tac: 
    Data.t -> Data.t option ref -> Tag.t -> Tactics.tactic
val is_excluded: 
    Tag.t list -> Logic.Sequent.t -> Logic.rr_type -> bool

val simp_fail: ?err:exn -> Tactics.tactic

(** {5 Planned rewriting} *)

module Planner :
    sig

      val log : string -> 'a -> unit

      open Rewritekit
      open Rewrite.Planned

      val check_change : ('a)plan -> unit
      val check_change2 : ('a)plan -> ('a)plan -> unit

      val null_term : Basic.term

      val plan_rewrite:
	  ?info:Logic.info 
	-> bool
	  -> Logic.rr_type Rewrite.Planned.plan
	    -> Basic.term 
	      -> Logic.label
		-> tactic

      type data = 
	  (Data.t  (** Scope *)
	     * Gtypes.substitution (** Type environment *)
	     * Term.substitution)   (** Quantifier environment *)

      val match_rewrite :
	  Scope.t 
	-> Gtypes.substitution 
	  -> Term.substitution 
	    -> Simpset.rule
	      -> Basic.term 
		-> (Logic.rr_type 
		      * Gtypes.substitution 
		      * Term.substitution 
		      * Basic.term)

(**
   [match_rewrite scp tyenv qntenv trmenv varp lhs rhs trm]: Try to match
   [lhs] with [trm] in type envivornment [tyenv] and term bindings
   [trmenv]. Return [rhs], instantiated with the binding from the
   match, and the type and term environments that made the match
   successful. Raise [Failure] on failure.
 *)

      val find_basic :
	  (Data.t 
	     * Gtypes.substitution 
	     * Basic.term 
	     * Logic.rr_type) option ref 
	-> data
	  -> Simpset.rule
	    -> Basic.term 
	      -> Tactics.tactic
(** 
   [find_basic cntrl ret tyenv tac rl trm g]: Try to match simp rule
   [rl] with term [trm] in goal [g]. If [rl] matches but is
   conditional, try to prove the condition using tactic [tac], adding
   the rule to the goal assumptions.

   Returns [ret=(ncntrl, ntyenv, ntrm, rr)] where [ncntrl] is the
   updated data, [nytenv] is the type environment made by the
   matching, [ntrm] is the result of rewriting [trm] with [rl] and
   [rl] the rewrite rule to add to the list being compiled.
 *)

      val find_match_tac :
	  (Data.t 
	     * Gtypes.substitution 
	     * Basic.term 
	     * Logic.rr_type) option ref 
	-> data 
	  -> Basic.term 
	    -> Tactics.tactic
(**
   [find_match_tac cntrl tyenv ret trm g]: Find a rule in simpset
   [set] which matches term [trm] in goal [g]. If found, rewrite [trm]
   with the rule.

   Returns [ret=(ncntrl, ntyenv, ntrm, rr)] where [ncntrl] is the
   updated data, [nytenv] is the type environment made by the
   matching, [ntrm] is the result of rewriting [trm] with [rl] and
   [rl] the rewrite rule to add to the list being compiled.

   Raise [No_change] and set [ret:=None] if no matches.
 *)

      val find_all_matches_tac :
	  (Data.t 
	     * Gtypes.substitution 
	     * Basic.term
	     * Logic.rr_type list) option ref
	-> data
	  -> Basic.term 
	    -> Tactics.tactic
(** 
   [find_all_matches cntrl ret tyenv qntenv trm g]: Find all rules in simpset
   [cntrl.set] which can be used to rewrite term [trm] in goal [g].

   Returns new simp data, the new type environment and the rewritten
   term. The new simp data is built by adding the rules used to
   rewrite the term, in the order they are applied.
 *)
      val extract_rules: 
	  Data.t -> (Logic.rr_type list * Data.t)

      val find_term_bu_tac:
	  (Data.t * Gtypes.substitution 
	     * Basic.term
	     * Logic.rr_type Rewrite.Planned.plan) option ref
	-> data
	  -> Basic.term
	    -> Tactics.tactic
      val find_subterm_bu_tac:
	  (Data.t * Gtypes.substitution 
	     * Basic.term
	     * Logic.rr_type Rewrite.Planned.plan) option ref
	-> data
	  -> Basic.term
	    -> Tactics.tactic
(**
   [find_rrs_bottom_up ctrl ret tyenv qntenv trm g]: Traverse term [trm],
   bottom-up, finding rewrite rules to apply. 

   Return [ret=(ncntrl, ntyenv, ntrm)], where [ncntrl] is the new simp
   data (with the rewrite rules to apply), [ntyenv] the new
   type-environment and [ntrm] the term resulting from simplification.
 *)

      val find_term_td_tac : 
	  (Data.t * Gtypes.substitution 
	     * Basic.term
	     * Logic.rr_type Rewrite.Planned.plan) option ref
	-> data
	  -> Basic.term
	    -> Tactics.tactic

      val find_subterm_td_tac : 
	  (Data.t * Gtypes.substitution 
	     * Basic.term
	     * Logic.rr_type Rewrite.Planned.plan) option ref
	-> data
	  -> Basic.term
	    -> Tactics.tactic

(**
   [find_rrs_top_down ctrl ret tyenv qntenv trm g]: Traverse term [trm],
   top-down, finding rewrite rules to apply. This is the default.

   Return [ret=(ncntrl, ntyenv, ntrm)], where [ncntrl] is the new simp
   data (with the rewrite rules to apply), [ntyenv] the new
   type-environment and [ntrm] the term resulting from simplification.
 *)

      val basic_simp_tac :
	  Data.t -> Data.t option ref -> Tag.t -> Tactics.tactic
(**
   [basic_simp_tac data ret tag goal]:

   Simplify formula tagged [tag] in [goal]: 
   {ul
   {- Descend top-down or bottom-up into formula, at each level collect
   rewrite rules which can be used to rewrite the term.}
   {- Use collected rules to rewrite the formula.}}

   Doesn't clean up afterwards.

   Returns [ret = ndata] where [ndata] is [data] updated with the
   rules used to rewrite the formula.

   raise [No_change] if no rules can be found.
 *)

(** {7 Debugging information} *)
      val check_add_loop: 
	  Scope.t -> Data.t -> Basic.term
	    ->  Data.t
    end
