(*-----
   Name: simptacs.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(**
   Tactical interface to the simplifier engine. 
 *)

open Simplifier

(** {5 Rule-forming tactics} *)

val make_asm_entries_tac : 
    Simpconvs.rule_data list ref 
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
    Simpconvs.rule_data list ref 
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

(** Arguments for the simplifier *)
type simp_args=
      {
       use_asms: bool; 
      (** Whether to use the assumptions (and conclusions) as simp rules. *)
       exclude: (Tag.t -> bool)
      (** Test on whether a formula is to be ignored (not simplified
      or used as a simp rule. *)
     }

val mk_args : bool -> (Tag.t -> bool) -> simp_args
(** Make arguments for the simplifier. *)

val simp_engine_tac :
    Data.t 
  -> (Data.t option) ref
    -> Tag.t 
      -> Tactics.tactic
(** The engine for [simp_tac]. 

   [simp_engine_tac ret cntrl l goal]:

   {ul 
   {- Eliminate toplevel universal quantifiers of [l].}
   {- Simplify [l], using {!Simplifier.basic_simp_tac}}
   {- Solve trivial goals}
   {- Repeat until nothing works}}

   Returns the updated simp data in [ret].
*)

val simp_tac: 
    Data.t
  -> simp_args
    -> Logic.label option 
      -> Tactics.tactic
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

(** {5 Alternative approach} *)

val add_rule_data:
    Data.t -> Simpconvs.rule_data list -> Data.t
(** 
   [add_rule_data data rules]: Update [data] with assumption
   [rules]. [rules] should be as provided by {!Simpconvs.prepare_asm}.
*)

val add_asms_tac:
    Data.t option ref
    -> Tag.t list
      -> Tactics.tactic
(**
   [add_asms_tac data tags g]: Prepare the assumptions in [tags] for
   use as simp-rules. Add them to [data].
*)

val add_concls_tac:
    Data.t option ref
    -> Tag.t list
      -> Tactics.tactic
(**
   [add_concls_tac data tags g]: Prepare the conclusions in [tags] for
   use as simp-rules. Add them to [data].
*)

val simpC0_tac :
    Data.t 
    -> Data.t option ref 
      -> Logic.label -> Tactics.tactic
(**
   [simpC0_tac cntrl ret l goal]: Simplify conclusion [l], returning
   the updated data in [ret]. Doesn't clean-up.
*) 

val simpC1_tac :
    Data.t 
    -> Data.t option ref 
      -> Tactics.tactic
(** 
   [simpC1_tac cntrl ret goal]: Simplify conclusions.

   Simplify each conclusion, starting with the last, adding it to the
   assumptions after it is simplified.

   Doesn't clean-up.
*)

val simpC_tac :
    Data.t 
      -> ?c:Logic.label -> Tactics.tactic
(** 
   [simpC1_tac cntrl goal]: Simplify conclusions.

   Simplify each conclusion, starting with the last, adding it to the
   assumptions after it is simplified.
*)


val simpA0_tac :
    Data.t 
    -> Data.t option ref 
      -> Logic.label -> Tactics.tactic
(**
   [simpA0_tac cntrl ret l goal]: Simplify assumption [l], returning
   the updated data in [ret]. Doesn't clean-up.
*) 

val simpA1_tac :
    Data.t 
    -> Data.t option ref 
      -> Tactics.tactic
(** 
   [simpA1_tac cntrl ret goal]: Simplify assumptions

   Simplify each assumptions, starting with the last, adding it to the
   simpset rules after it is simplified.

   Doesn't clean-up.
*)

val simpA_tac :
    Data.t 
  -> ?a:Logic.label -> Tactics.tactic
(** 
   [simpA_tac cntrl goal]: Simplify assumptions

   Simplify each assumption, starting with the last, adding it to the
   simpset rules after it is simplified.
*)


val full_simp0_tac:
    Data.t 
    -> Data.t option ref 
      -> Tactics.tactic
(** 
   [full_simp0_tac cntrl ret goal]: Simplify subgoal

   {ul
   {- Simplify each assumption, starting with the last, adding it to the
   simpset rules after it is simplified.}
   {- Simplify each conclusion, starting with the last, adding it to the
   simpset rules after it is simplified.}}

   Doesn't clean-up.
*)
 
val full_simp_tac:
    Data.t -> Tactics.tactic
(** 
   [full_simp_tac cntrl ret goal]: Simplify subgoal

   {ul
   {- Simplify each assumption, starting with the last, adding it to the
   simpset rules after it is simplified.}
   {- Simplify each conclusion, starting with the last, adding it to the
   simpset rules after it is simplified.}}

   Doesn't clean-up.
*)
 



(** Debugging information **)

val log : string -> Data.t -> unit
