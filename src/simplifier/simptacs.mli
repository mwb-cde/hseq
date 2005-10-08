(*-----
   Name: simptacs.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(**
   Tactical interface to the simplifier engine. 
 *)

open Simplifier

(** 
   Simplification tactics. 

   [make_asm_entries_tac]/[make_concl_entries_tac]: Tactics to prepare
   assumptions/conclusions for use as simp rules.

   [simp_tac]: The standard simplification tactic. Repeatedly
   simplifies formulas until nothing else can be done.
 *)

(** {5 Rule-forming tactics} *)

type rule_data = 
    { 
      rule_asms: (Tag.t * Tag.t) list ;
      rule_forms: (Logic.tagged_form) list
    }
	

val make_asm_entries_tac : 
    rule_data ref 
  -> Tag.t list -> (Tag.t -> bool)
    -> Tactics.tactic
(**
   Make simp rules from identified assumptions.

   [make_asm_entries_tac ret tags except goal]: Copy, prepare the
   assumptions in [tags] for use as simp rules. Ignore the assumptions
   in for which [except] is true.

   Return [ret = (asm_tags, rules)] where [asm_tags] is a list of
   pairs [(s, a)], with [a] a new assumption formed from [s] and
   [rules] is the list of tagged formulas to be used as simp rules.
*)

val make_concl_entries_tac : 
    rule_data ref 
  -> Tag.t list -> (Tag.t -> bool)
    -> Tactics.tactic
(** 
   Make simp rules from identified conclusions.

   [make_concl_entries_tac ret tags except goal]: Copy, lift, prepare
   the conclusions in [tags] for use as simp rules.  Ignore the
   conclusions for which [except] is true.

   Return [ret = (asm_tags, rules)] where [asm_tags] is a list of
   pairs [(c, a)], with [a] is a new assumption formed from [c] and
   [rules] is the list of tagged formulas to be used as simp rules.
*)

(** {5 Simplifier tactics} *)

val simp_engine_tac :
    (Data.t * Data.t option ref * (Tag.t -> bool))
  -> Tag.t -> Tactics.tactic
(** The engine for [simp_tac]. 

   [simp_engine_tac cntrl asms l goal]:
   {ul 
   {- Eliminate toplevel universal quantifiers of [l].}
   {- If [asms=true], put conclusions other than [l] into assumptions
   and make simp rules.}
   {- If [asms=true], make simp rules from assumptions.}
   {- Simplify.}
   {- Delete temporary assumptions.}}
*)

val simp_tac: 
    Data.t
  -> bool -> (Tag.t -> bool)
    -> Logic.label option -> Tactics.tactic
(**
   Simplify formulas.

   [simp_tac cntrl asms except ?l goal]: 
   {ul
   {- Eliminate toplevel universal quantifiers of [l].}
   {- If [asms=true], put conclusions, other then those to be
   simplified, into assumptions and make simp rules from them.}
   {- If [asms=true], make simp rules from assumptions, other than
   those to be simplified.} 
   {- Simplify.}  
   {- Delete temporary assumptions.}}

   If [l] is not given, simplify each conclusion.
   Ignore formulas for which [except] is true.
 *)




(** [full_simp_tac]: not implemented *)
(*
   val full_simp_tac :
   Data.t -> Simpset.simpset -> Tag.t -> tactic
 *)


(* Debugging information *)

(*
   val cond_prover_trueC: 
   Logic.info option -> Logic.label -> Tactics.tactic
   val cond_prover_worker_tac: 
   Data.t -> Data.t option ref -> Tag.t -> Tactics.tactic

   val is_excluded: 
   Tag.t list -> Logic.Sequent.t -> Logic.rr_type -> bool
 *)

(*** RETIRED
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
 *)
