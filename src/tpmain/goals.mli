(* Proof stacks *)
(* user level manipulation of goals *)

(* proof are a list of goals *)
type prf = Logic.goal 


(* function to call when a proof command is successful 
   (for user interfaces) *)

val save_hook :(unit -> unit) ref 
val set_hook : (unit -> unit) -> unit

(*    val prflist : prf list ref *)

(* current index/goal *)
(*    val curr_indx : prf -> int*)
val curr_goal : prf -> Logic.goal

(* the current proof attempt *)
val top : unit -> prf

(** [drop()]

   Drop the current proof.
 *)
val drop : unit -> unit

(* go back *)
val pop_plist : unit -> prf

(* the current sequent *)
val curr_sqnt : prf -> Logic.Sequent.t

(* start a proof attempt *)
val goal : Basic.term -> prf

(** [apply ?report tac goal]

   Apply tactic [tac] to [goal] using 
   [Logic.Subgoals.apply_to_goal ?report].

   Applies [tac] to the first subgoal [n] of [goal]. Returns the goal 
   with the subgoals [tac n] appended to the remaining subgoals of goal.
*)
val apply: ?report:(Logic.node -> Logic.branch -> unit) 
  -> Tactics.tactic -> Logic.goal -> Logic.goal

(* apply a tactic *)
val by_com : Tactics.tactic -> prf

(* go back *)
val undo : unit -> prf
(* claim that proof is completed *)
val result: unit-> Logic.thm

(* try on a different subgoal *)
(* val postpone : unit -> prf *)

(**
   [prove_goal scp trm tac]: Prove the goal formed from [trm] using
   tactic [tac] in scope [scp].

   [prove scp trm tac]: Prove the goal formed from [trm] using
   tactic [tac] in the standard scope.
   (equivalent to [prove_goal (Tpenv.scope()) trm tac]).
*)
val prove_goal: Gtypes.scope -> Basic.term -> Tactics.tactic -> Logic.thm 
val prove: Basic.term -> Tactics.tactic -> Logic.thm

(**
   [by_list trm tacl]: Apply the list of tactics [tacl] to the goal formed 
   from term [trm] in the standard scope.

   Unlike [prove_goal], this allows each tactic in the list applied to
   the first subgoal of the goal. This is similar to the way that
   interactive proof are built up.
*)
val by_list : Basic.term -> Tactics.tactic list -> Logic.thm

(* user-level utility functions *)
val get_asm: int -> (Tag.t * Basic.term)
val get_concl: int -> (Tag.t * Basic.term)
