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

(** {5 Simplifier Data} *)

exception No_change
(** Raised if the simplifier makes no changes. *)

type control = Rewrite.control
(** Passed to the simplifier to control rewriting. *)

(** Data used by the simplifier. *)
module Data:
    sig

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

	    conds : int;
(** Max. no. of conditions to try and prove at once. *)

	    rr_depth : int;
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

	    rules : Logic.rr_type list
(**  Rewrite rules chosen by the simplifier. *)

	}

      val make: 
	  (Simpset.simpset
	  * (t -> Tag.t -> Tactics.tactic)
	  * control
	  * int * int 
	  * Tag.t list * Tag.t list 
	  * Tag.t list 
	  * Logic.rr_type list)
	-> t
(** Make simp data. *)

      val set_simpset : t -> Simpset.simpset -> t
(** Set the simpset. *)

      val set_tactic : t -> (t -> Tag.t -> Tactics.tactic) -> t
(** Set the condition prover tactic. *)

      val set_conds : t -> int -> t
(** Set the maximum number of conditions to try in one go. *)

      val set_rr_depth : t -> int -> t
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

      val get_simpset : t -> Simpset.simpset
(** Get the simpset to use. *)

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

      val default : t
(** Make the default simp data. *)
    end 

(** {5 Utility Tactics} *)

val copyA_inst: 
    ?info:Logic.info -> Basic.term list -> Logic.label
      -> Tactics.tactic
(**
   [copyA_inst info vals x]: Copy assumption [x], instantiate the copy
   with [vals]. info: aformulas = [x1], where [x1] is the tag of the
   new assumption.  Fails if there are more terms in [vals] then
   variables in [x].
*)

val cut_rr_rule :
    ?info:Logic.info -> Basic.term list -> Logic.rr_type -> tactic


val prep_cond_tac :
    Data.t 
  -> (Data.t * (Tag.t * Tag.t) * (Tag.t * Tag.t)) option ref
    -> Basic.term list -> Logic.rr_type 
      -> Tactics.tactic

val prove_cond_tac:
    Data.t 
  -> (Data.t * Logic.rr_type) option ref 
    -> Basic.term list 
      -> Simpset.rule
	-> Tactics.tactic


val match_rewrite :
    Scope.t 
  -> Gtypes.substitution 
    -> Term.substitution 
      -> (Basic.term -> bool) 
	-> Basic.term 
	  -> Basic.term 
	    -> Basic.term 
	      -> Gtypes.substitution * Term.substitution * Basic.term

val find_basic :
    Data.t 
  -> (Data.t * Gtypes.substitution * Basic.term * Logic.rr_type) option ref 
    -> Gtypes.substitution 
      -> (Basic.binders list 
	    * 'a option 
	    * Basic.term 
	    * Basic.term 
	    * Logic.rr_type) 
	-> Basic.term 
	  -> Tactics.tactic

val find_match_tac :
    Data.t 
  -> Gtypes.substitution 
    -> (Data.t 
	  * Gtypes.substitution 
	  * Basic.term 
	  * Logic.rr_type) option ref 
      -> Basic.term 
	-> Tactics.tactic

val find_all_matches_tac :
    Data.t 
  -> (Data.t * Gtypes.substitution * Basic.term) option ref
  -> Gtypes.substitution 
    -> Basic.term 
	-> Tactics.tactic

val find_rrs_top_down_tac : 
    Data.t
  -> (Data.t
	* Gtypes.substitution 
	* Basic.term) option ref
  -> Gtypes.substitution
    -> Basic.term
	-> Tactics.tactic

val simp_prep_tac :
    'a -> 'a option ref -> Tag.t -> Logic.node -> Logic.Subgoals.branch

val is_true : Basic.term -> bool
val simp_asm_elims :
    ((Formula.form -> bool) * (Logic.label -> Tactics.tactic)) list
val simp_conc_elims :
    ((Formula.form -> bool) * (Logic.label -> Tactics.tactic)) list
val initial_flatten_tac :
    Tag.t list -> tactic



(**
   [basic_simp_tac data set tag goal]:

   Simplify formula tagged [tag] in [goal]: 
   - Descend top-down or bottom-up into formula, at each level collect
   rewrite rules which can be used to rewrite the term.
   - Use collected rules to rewrite the formula.

   Doesn't clean up afterwards.

   Returns updated data set.

   raise [No_change] if no rules can be found.
 *)
val basic_simp_tac :
    Data.t -> Data.t option ref -> Tag.t -> Tactics.tactic


val clean_up_tac: Data.t -> Tactics.tactic
val cleanup: bool ref
(**
   [cleanup]: If [true], assumptions added to a goal by the simplifier
   will be removed after simplification. This is intended to help with
   debuggin. Default: [true]
*)

val cond_prover_tac:
    Data.t -> Tag.t -> Tactics.tactic
(**
   [cond_prover_tac ctrl tg g]: The tactic used to prove the conditions of
   rewrite rules.

   Apply [simp_prep_tac] then [basic_simp_tac].
   Then apply [Logic.Tactics.trueR] to solve goal.
 *) 

(*
(**
   {7 User-level functions }
*)

val make_asm_entries_tac : 
    ((Tag.t * Tag.t) list * (Tag.t * Formula.form) list) ref 
  -> Tag.t list -> (Tag.t -> bool)
    -> Tactics.tactic

val make_concl_entries_tac : 
    ((Tag.t * Tag.t) list * (Tag.t * Formula.form) list) ref 
  -> Tag.t list -> (Tag.t -> bool)
    -> Tactics.tactic

val cond_prover_tac:
    Data.t -> Tag.t 
      -> Tactics.tactic

(**
   [once_simp_tac cntrl set l g]

   Simplify formula [label] with [set], once.
 *)
val once_simp_engine_tac :
(Data.t  * Data.t option ref
   * (Tag.t -> bool)
   * (Logic.tagged_form list))
-> Tag.t -> Tactics.tactic

val once_simp_tac: 
    Data.t
-> bool -> (Tag.t -> bool)
-> Logic.label option -> Tactics.tactic

(**
   [simp_tac cntrl set l g]

   Simplify formula [label] with [set], repeat until nothing more can
   be done.
*)

val simp_engine_tac :
(Data.t  * Data.t option ref
   * (Tag.t -> bool)
   * (Logic.tagged_form list))
-> Tag.t -> Tactics.tactic

val simp_tac: 
    Data.t
-> bool -> (Tag.t -> bool)
-> Logic.label option -> Tactics.tactic

(** [full_simp_tac]: not implemented *)
(*
val full_simp_tac :
    Data.t -> Simpset.simpset -> Tag.t -> tactic
*)

val dfst: ('a * 'b) -> 'a
val dsnd: ('a * 'b) -> 'b

*)

(* Debugging information *)

val cond_prover_trueC: 
    Logic.info option -> Logic.label -> Tactics.tactic
val cond_prover_worker_tac: 
    Data.t -> Data.t option ref -> Tag.t -> Tactics.tactic

val is_excluded: 
    Tag.t list -> Logic.Sequent.t -> Logic.rr_type -> bool
