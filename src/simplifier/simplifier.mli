(*-----
 Name: simplifier.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Tactics

(**
   [cleanup]

   if [true], assumptions added to a goal by the simplifier 
   will be removed after simplification.

   It is useful to set this to [false] for debugging.

   default: [true]
*)
val cleanup: bool ref

class simpError :
    string ->
      Basic.term list ->
	object
          val trms : Basic.term list
          method get : unit -> Basic.term list
          method msg : unit -> string
          method print : Printer.ppinfo -> unit
	end
val mk_error : string -> Basic.term list -> exn
val add_error : string -> Basic.term list -> exn -> exn
exception No_change

type control = Rewrite.control

module Data:
    sig
      type t = {
(**
   [simpset]: the simpset being used. Assumptions may be added to this
   during the course of simplification
 *)
	  simpset: Simpset.simpset;

(** 
   [cond_tac]: the tactic used to prove conditions of rewrite rules.
   Default: [skip].
 *)
	  cond_tac: t -> Tag.t -> Tactics.tactic;

(** [control]: rewrite control ([direction] is ignored) *)
	    control: Rewrite.control;

(** conds: max. no. of conditions to try and prove at once *)
	    conds : int;

(** rr_depth: max. no. of rr rules to apply at one level *)
	    rr_depth : int;

(** asms: assumptions generated during the course of simplification *)
	    asms : Tag.t list;

(** visited: formulas visited during the course of simplification *)
	   visited: Tag.t list;
(*
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
*)
(** exclude: formulas not to use as a rewrite rule *)
	   exclude: Tag.t list;

(** rules: 
   rewrite rules to pass to the rewriter (the result of the simplifier)
 *)
	    rules : Logic.rr_type list

	}

      val make: 
	  Simpset.simpset
	-> (t -> Tag.t -> Tactics.tactic)
	  -> control
	    -> int -> int 
	      -> Tag.t list 
		-> Tag.t list 
		  -> Tag.t list 
(*
		    -> (Tag.t*Tag.t) list 
		      -> (Tag.t*Tag.t) list 
*)
			-> Logic.rr_type list 
			  -> t

      val set_simpset : t -> Simpset.simpset -> t
      val set_tactic : t -> (t -> Tag.t -> Tactics.tactic) -> t
      val set_conds : t -> int -> t
      val set_rr_depth : t -> int -> t
      val set_control: t -> control -> t
      val set_asms : t -> Tag.t list -> t
      val set_visited : t -> Tag.t list -> t
(*
      val set_asm_pairs : t -> (Tag.t*Tag.t) list -> t
      val set_concl_pairs : t -> (Tag.t*Tag.t) list -> t
*)
      val set_exclude : t -> Tag.t list -> t
      val set_rules : t -> Logic.rr_type list -> t
      val get_simpset : t -> Simpset.simpset
      val get_tactic : t -> (t -> Tag.t -> Tactics.tactic)
      val get_control: t -> control
      val get_asms : t -> Tag.t list 
      val get_visited : t -> Tag.t list 
(*
      val get_asm_pairs : t -> (Tag.t*Tag.t) list 
      val get_concl_pairs : t -> (Tag.t*Tag.t) list 
*)
      val get_exclude : t -> Tag.t list 
      val add_asm : t -> Tag.t -> t
      val dec_cond_depth : t -> t
      val add_rule : t -> Logic.rr_type -> t
      val add_simp_rule : t -> Simpset.rule -> t
      val default : t
    end 

val strip_rrs : Logic.rr_type list -> Basic.term list
val cut_rr_rule :
    Logic.info option ->  Logic.rr_type -> tactic
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
    Data.t ->
    Data.t option ref -> Tag.t -> Tactics.tactic

(**
   {6C User-level functions }
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


(* Debugging information *)

val cond_prover_trueC: 
    Logic.info option -> Logic.label -> Tactics.tactic
val cond_prover_worker_tac: 
    Data.t -> Data.t option ref -> Tag.t -> Tactics.tactic

val is_excluded: 
    Tag.t list -> Logic.Sequent.t -> Logic.rr_type -> bool
