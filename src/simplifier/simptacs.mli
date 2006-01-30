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
    (Data.t) 
  -> ((Data.t option) ref * (Tag.t -> bool))
      -> Tag.t -> Tactics.tactic
(** The engine for [simp_tac]. 

   [simp_engine_tac cntrl (ret, exclude) l goal]:
   {ul 
   {- Eliminate toplevel universal quantifiers of [l].}
   {- If [asms=true], put conclusions other than [l] into assumptions
   and make simp rules.}
   {- If [asms=true], make simp rules from assumptions.}
   {- Simplify.}
   {- Delete temporary assumptions.}}

   Ignores all formulas for which [exclude] is true. Returns the
   updated simp data in [ret].
*)

val simp_tac: 
    Data.t
  -> simp_args
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


(** Debugging information **)

