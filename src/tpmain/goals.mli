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
(* go back *)
    val pop_plist : unit -> prf

(* the current sequent *)
    val curr_sqnt : prf -> Logic.sqnt

(* start a proof attempt *)
    val goal : Basic.term -> prf
(*
    val goal_string : string -> prf
*)
(* apply a tactic *)
    val by_com : Tactics.tactic -> prf

(* go back *)
    val undo : unit -> prf
(* try on a different subgoal *)
    val postpone : unit -> prf
(* claim that proof is completed *)
    val result: unit-> Logic.thm

(* prove a goal with a list of tactics *)

    val prove_goal: Basic.term -> Tactics.tactic -> Logic.thm

    val by_list : Basic.term -> Tactics.tactic list -> Logic.thm

(*
    val prove_goal_string: string -> Tactics.tactic -> Logic.thm

    val by_list_string : string -> Tactics.tactic list -> Logic.thm
*)


(* user-level utility functions *)
val get_asm: int -> (Tag.t * Basic.term)
val get_concl: int -> (Tag.t * Basic.term)
