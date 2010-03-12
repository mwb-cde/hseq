(*----
 Name: goals.mli
 Copyright M Wahab 2005-2010
 Author: M Wahab  <mwb.cde@googlemail.com>

 This file is part of HSeq

 HSeq is free software; you can redistribute it and/or modify it under
 the terms of the Lesser GNU General Public License as published by
 the Free Software Foundation; either version 3, or (at your option)
 any later version.

 HSeq is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
 License for more details.

 You should have received a copy of the Lesser GNU General Public
 License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
----*)

(** Goals and Proofs

   Support functions for interactive and batch proof.
 *)


(**
   Support for interactive proof.

   A proof is a list of goals with the current goal at the head of the
   list.  and earlier goals following in order. Each goal is produced
   from its predecessor by applying a tactic. A tactic is undone by
   popping the top goal off the list.  *)
module Proof: 
sig
  type t = Logic.goal list

  val push : Logic.goal -> t -> t
  val top : t -> Logic.goal
  val pop : t -> t

end

(**
   Proof stacks for interactive proof. A proof stack is a list of
   proofs. The top proof is the active proof, on which work is done.
*)
module ProofStack :
    sig
      type t (* = Proof.t list *)
      val empty: unit -> t
      val push : Proof.t -> t -> t
      val top : t -> Proof.t
      val pop : t -> t

      val rotate: t -> t
      val lift: int -> t -> t

      val push_goal : Logic.goal -> t -> t
      val top_goal : t -> Logic.goal
      val pop_goal : t -> t

    end 

(** {7 General operations} *)

val proofs : unit -> ProofStack.t
(** Get the stack of proofs. *)

val top : unit -> Proof.t
(** The current proof attempt *)

val top_goal : unit -> Logic.goal
(** The current goal. *)

val drop : unit -> unit
(** Drop the current proof.  *)

val goal : ?info:Logic.info -> Basic.term -> Proof.t
(** 
   Start a proof attempt. Creates a goal and pushes it on the top of
   the proof stack.  If [?info] is given, the tag of the goal and
   conclusion ([trm]) are stored in it. This allows the tag of the
   conclusion formed from term [trm] to be determined.
*)

val postpone: unit -> Proof.t
(**
   Postpone the current proof, pushing it to the bottom of the stack.
*)

val lift: int -> Proof.t
  (** [lift n]: Focus on the nth proof to the top of the stack, making
      it the current proof attempt. Fails if there is no nth proof.
  *)

val undo : unit -> Proof.t
  (** 
      Go back. Pop the top goal off the proof. Fails if there is only one
      goal in the proof.
  *)

val result: unit-> Logic.thm
(** 
   Claim that proof is completed. Make a theorem from the proof, fail
   if the current goal has subgoals and is therefore not a theorem.
*)

val apply: 
    ?report:(Logic.node -> Logic.branch -> unit) 
  -> Tactics.tactic -> Logic.goal -> Logic.goal
(** 
   [apply ?report tac goal]: Apply tactic [tac] to [goal] using
   {!Logic.Subgoals.apply_to_goal}.

   Applies [tac] to the first subgoal [n] of [goal]. Returns the goal 
   with the subgoals [tac n] appended to the remaining subgoals of goal.
*)

(** {7 Batch proofs} *)

val prove_goal: 
    ?info:Logic.info -> 
      Scope.t -> Basic.term -> Tactics.tactic 
	-> Logic.thm
(**
   [prove_goal ?info scp trm tac]: Prove the goal formed from [trm]
   using tactic [tac] in scope [scp]. Used for batch proofs. If
   [?info] is given, the tag of the goal and conclusion ([trm]) are
   stored in it before the tactic [tac] is applied.
*)

(*
val prove: Basic.term -> Tactics.tactic -> Logic.thm
(**
   [prove trm tac]: Prove the goal formed from [trm] using tactic
   [tac] in the standard scope. Equivalent to [prove_goal
   (Global.scope()) trm tac]. Used for batch proofs.
*)
*)

(** {7 Interactive proofs} *)

val by_com : Tactics.tactic -> Proof.t
(** 
   Apply a tactic to the current goal. If the tactic succeeds, call
   [!save_hook]. Used for interactive proofs.
*)

val by_list : 
    ?info:Logic.info 
  -> Basic.term -> Tactics.tactic list -> Logic.thm
(**
   [by_list ?info trm tacl]: Apply the list of tactics [tacl] to the
   goal formed from term [trm] in the standard scope.

   [by_list] applies each tactic in the list to the first subgoal of
   the goal, in the same way as an interactive proof is built up by
   applying tactics, one at a time, to a goal. This allows the list of
   tactics used during an interactive proof to be packaged for a batch
   proof. By contrast, {!Goals.prove_goal} requires a structured
   proof, a tactic which completely solves the goal, to be constructed
   from the tactics used in an interactive proof.

   If [?info] is given, the tag of the goal and conclusion ([trm]) are
   stored in it before the tactics are applied. This allows the
   tactics to determine the tag of the conclusion formed from term
   [trm].
*)

(** {7 Support for proof recording} *)

val save_hook: (unit -> unit) ref 
(**
   User interface hook called when an application a proof command is
   successful.

   The proof commands which invoke [save_hook] are {!Goals.by_com} and
   {!Goals.goal}.
*)

val set_hook : (unit -> unit) -> unit
(** Set the user interface hook to a given function. *)

(** {7 Miscellaneous} *)

val curr_sqnt : unit -> Logic.Sequent.t
(** The current sequent. *)

val goal_scope: unit -> Scope.t
(** The scope of the current subgoal. *)

val get_asm: int -> (Tag.t * Basic.term)
(** Get an assumption from the current sequent. *)

val get_concl: int -> (Tag.t * Basic.term)
(** Get a conclusion from the current sequent. *)


(** {7 Debugging} *)
