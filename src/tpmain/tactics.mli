(* tactics and tacticals *)
(* user level rules *)

type tactic = Logic.rule

(* conversion from rule to tactic *)

val rule_tac :
    Logic.rule -> tactic

val rotateA : tactic
val rotateC : tactic

val copy_asm : int -> tactic
val copy_concl : int -> tactic

val trivial : tactic
val skip : tactic
val basic : tactic
val postpone: tactic

val cut : Logic.thm -> tactic
val unify_tac : int -> int -> tactic


(*
   [lift id sqnt]
   Move assumption/conclusion with identifier [id] to 
   to top of the assumptions/conclusions of sequent sqnt.

   Raise Not_found if identified formula is not in 
   assumptions/conclusions.
*)
val lift : int -> tactic

(* 
   the following apply the basic rules to the first assumption/conclusion
   which will succeed 
*)

val conjI : tactic
val conjE : tactic
val disjI : tactic
val disjE : tactic
val negA : tactic
val negC : tactic
val implI : tactic
val implE : tactic
val mp_tac : tactic
val existI : tactic
val existE : string -> tactic
val allI : tactic
val allE : string -> tactic

(* beta conversion 
   to given asumption/conclusion
   or search for suitable assumption/conclusion
*)
val beta:  int -> tactic
val beta_tac :  tactic

(* rewrite from asumption *)
val replace: int -> int -> tactic
val replace_rl: int -> int -> tactic

(* delete assumption or conclusion *)
val delete: int -> tactic 

(* tacticals *)

type tactical = tactic 

val repeat : tactic -> tactic
val (++) : tactic -> tactic -> tactic
val (||) : tactic -> tactic -> tactic
val orelseF : tactic -> tactic -> tactic
val firstF : tactic list -> tactic      
val thenl : tactic list -> tactic
val apply_list : tactic list -> tactic
val orl: tactic list -> tactic
