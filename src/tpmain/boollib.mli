(* derived tactics, some of which depend on theorems already having been
   proved *)

(* prove goals of the form A|- x=x, C *)
    val eq_tac :  Tactics.tactic

(* unfold a definition *)
    val unfold : string -> int -> Tactics.tactic

(* test and rules for Iff *)
val is_iff: Formula.form -> bool

(* val iffI_rule: Logic.label -> Logic.rule *)
val iffI: ?c:Logic.label -> Tactics.tactic

val asm_elims : 
    unit -> ((Formula.form->bool) * (Logic.label -> Logic.rule)) list
val conc_elims : 
    unit -> ((Formula.form->bool) * (Logic.label -> Logic.rule)) list

val false_rule:  ?a:Logic.label -> Logic.rule

(* [flatten_tac]

   flattens formulas in a subgoal without creating new subgoals.

   apply negC, disjE, implI, allI
   to conclusions then apply negA, conjE, existI 
   to assumptions.
*)
    val flatten_tac : Tactics.tactic

(* [split_tac]

   split conjunctions and iff in the conclusions
   and disjunctions and implications in the assumptions
   creating new subgoals.
*)
    val split_tac : Tactics.tactic

(* [inst_tac f consts] 
   instantiate formula [f] with terms [consts]
*)
val inst_tac: Logic.label -> Basic.term list -> Tactics.tactic 
val inst_asm : ?a:Logic.label -> Basic.term list -> Tactics.tactic
val inst_concl : ?c:Logic.label -> Basic.term list -> Tactics.tactic

(* cases tactics *)

val cases_tac: Basic.term -> Tactics.tactic

(* convert boolean equality to iff *)
val equals_tac: ?f:Logic.label -> Tactics.tactic

(** [false_tac]
   solve the subgoal if it has [false] in the assumptions
*)
val false_tac: Tactics.tactic

(** [bool_tac]
   solve the subgoal if it has [false] in the assumptions or [true]
   in the conclusions.
*)
val bool_tac:  Tactics.tactic

(* match_mp_tac *)

val match_mp_tac: Logic.thm -> ?c:Logic.label -> Tactics.tactic

val back_mp_tac: a:Logic.label -> c:Logic.label -> Tactics.tactic
