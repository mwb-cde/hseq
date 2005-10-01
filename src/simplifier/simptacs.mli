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

(*
val cond_prover_tac:
    Data.t -> Tag.t 
      -> Tactics.tactic
*)

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


(* Debugging information *)

(*
val cond_prover_trueC: 
    Logic.info option -> Logic.label -> Tactics.tactic
val cond_prover_worker_tac: 
    Data.t -> Data.t option ref -> Tag.t -> Tactics.tactic

val is_excluded: 
    Tag.t list -> Logic.Sequent.t -> Logic.rr_type -> bool
*)
