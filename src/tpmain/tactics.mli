(* tactics and tacticals *)
(* user level rules *)

type tactic = Logic.rule

(* fident functions *)

val fnum: int -> Logic.fident
val ftag: Tag.t -> Logic.fident

val (!!): int -> Logic.fident

(* rewriting direction *)
val leftright : Rewrite.direction
val rightleft : Rewrite.direction

(* conversion from rule to tactic *)

val rotateA : (?info: Logic.info) -> tactic
val rotateC : (?info: Logic.info) -> tactic

val copy_asm : (?info: Logic.info) -> Logic.fident -> tactic
val copy_concl : (?info: Logic.info) -> Logic.fident -> tactic

(* delete a list of assumptions/conclusions *)
val deleten: ?info:Logic.info -> Logic.fident list -> Logic.rule

val trivial : ?info: Logic.info -> (?c:Logic.fident) -> tactic
val skip : tactic
val basic : ?info:Logic.info -> tactic
val postpone: tactic

val cut : (?info: Logic.info) -> Logic.thm -> tactic

(*
   [unify_tac a c g]
   unify assumption [a] with conclusion [c]
*)
val unify_tac : ?info: Logic.info ->  
  ?a:Logic.fident -> ?c:Logic.fident -> Logic.rule
(*
val unify_tac : Logic.fident -> Logic.fident -> tactic
*)
(*
   [lift id sqnt]
   Move assumption/conclusion with identifier [id] to 
   to top of the assumptions/conclusions of sequent sqnt.

   Raise Not_found if identified formula is not in 
   assumptions/conclusions.
*)
val lift : ?info:Logic.info -> Logic.fident -> tactic

(* 
   the following apply the basic rules to the first assumption/conclusion
   which will succeed 
*)

val conjI : ?info: Logic.info -> (?c: Logic.fident) -> tactic
val conjE : ?info: Logic.info -> (?a: Logic.fident) -> tactic
val disjI : ?info: Logic.info -> (?a: Logic.fident) -> tactic
val disjE : ?info: Logic.info -> (?c: Logic.fident) -> tactic
val negA : ?info: Logic.info -> (?a: Logic.fident) -> tactic
val negC : ?info: Logic.info -> (?c: Logic.fident) -> tactic
val implI : ?info: Logic.info -> (?c: Logic.fident) -> tactic
val implE : ?info: Logic.info -> (?a: Logic.fident) -> tactic
val existI : ?info: Logic.info -> (?a: Logic.fident) -> tactic
val existE : ?info: Logic.info -> (?c: Logic.fident) -> Basic.term -> tactic 
val allI : ?info: Logic.info -> (?c: Logic.fident) -> tactic
val allE : ?info: Logic.info -> (?a: Logic.fident) -> Basic.term -> tactic

(*
   beta conversion 
   to given asumption/conclusion
   or search for suitable assumption/conclusion
*)
val beta_tac : ?info: Logic.info -> (?f:Logic.fident) -> tactic

(* rewrite with a list of theorems *)
val rewrite_thm: 
    ?info: Logic.info -> 
      Logic.thm list -> ?dir:Rewrite.direction
	-> Logic.fident -> Logic.rule

(* rewrite from asumption *)
val replace: ?info: Logic.info -> Logic.fident -> Logic.fident -> tactic
val replace_rl: ?info: Logic.info -> Logic.fident -> Logic.fident -> tactic

(* delete assumption or conclusion *)
val delete: ?info:Logic.info -> Logic.fident -> tactic 

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
