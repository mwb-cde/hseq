(* tactics and tacticals *)
(* user level rules *)

type tactic = Logic.rule

(* label functions *)

val fnum: int -> Logic.label
val ftag: Tag.t -> Logic.label

val (!!): int -> Logic.label

(* rewriting direction *)
val leftright : Rewrite.direction
val rightleft : Rewrite.direction

(* conversion from rule to tactic *)

val rotateA : (?info: Logic.info) -> tactic
val rotateC : (?info: Logic.info) -> tactic

val copy_asm : (?info: Logic.info) -> Logic.label -> tactic
val copy_concl : (?info: Logic.info) -> Logic.label -> tactic

(* delete a list of assumptions/conclusions *)
val deleten: ?info:Logic.info -> Logic.label list -> Logic.rule

val trivial : ?info: Logic.info -> (?c:Logic.label) -> tactic
val skip : tactic
val basic : ?info:Logic.info -> tactic
val postpone: tactic

val cut : (?info: Logic.info) -> Logic.thm -> tactic

(*
   [unify_tac a c g]
   unify assumption [a] with conclusion [c]
*)
val unify_tac : ?info: Logic.info ->  
  ?a:Logic.label -> ?c:Logic.label -> Logic.rule
(*
val unify_tac : Logic.label -> Logic.label -> tactic
*)
(*
   [lift id sqnt]
   Move assumption/conclusion with identifier [id] to 
   to top of the assumptions/conclusions of sequent sqnt.

   Raise Not_found if identified formula is not in 
   assumptions/conclusions.
*)
val lift : ?info:Logic.info -> Logic.label -> tactic

(* 
   the following apply the basic rules to the first assumption/conclusion
   which will succeed 
*)

val conjI : ?info: Logic.info -> (?c: Logic.label) -> tactic
val conjE : ?info: Logic.info -> (?a: Logic.label) -> tactic
val disjI : ?info: Logic.info -> (?a: Logic.label) -> tactic
val disjE : ?info: Logic.info -> (?c: Logic.label) -> tactic
val negA : ?info: Logic.info -> (?a: Logic.label) -> tactic
val negC : ?info: Logic.info -> (?c: Logic.label) -> tactic
val implI : ?info: Logic.info -> (?c: Logic.label) -> tactic
val implE : ?info: Logic.info -> (?a: Logic.label) -> tactic
val existI : ?info: Logic.info -> (?a: Logic.label) -> tactic
val existE : ?info: Logic.info -> (?c: Logic.label) -> Basic.term -> tactic 
val allI : ?info: Logic.info -> (?c: Logic.label) -> tactic
val allE : ?info: Logic.info -> (?a: Logic.label) -> Basic.term -> tactic

(*
   beta conversion 
   to given asumption/conclusion
   or search for suitable assumption/conclusion
*)
val beta_tac : ?info: Logic.info -> (?f:Logic.label) -> tactic


(* delete assumption or conclusion *)
val delete: ?info:Logic.info -> Logic.label -> tactic 

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



(** [gen_rewrite_tac info dir rules f]
   rewrite formula [f] with list of theorems and assumptions given
   in [rules].
   if f is not given, rewrite all formulas in sequent.
*)
val gen_rewrite_tac: 
    ?info: Logic.info 
  -> Rewrite.control
      -> Logic.rr_type list 
	-> ?f:Logic.label -> Logic.rule

(** [rewrite_tac info dir thms f]
   rewrite formula [f] with list of theorems [thms].
   if f is not given, rewrite all formulas in sequent.
*)
val rewrite_tac: 
    ?info: Logic.info -> ?dir:Rewrite.direction
      -> Logic.thm list -> ?f:Logic.label -> Logic.rule


(** [replace_tac info dir asms f]
   rewrite formula [f] with assumptions in list [asms].
   if f is not given, rewrite all formulas in sequent.
   Doesn't rewrite formulas in [asms].
*)
val replace_tac: 
    ?info: Logic.info -> ?dir:Rewrite.direction
      -> Logic.label list 
	-> ?f:Logic.label -> Logic.rule

