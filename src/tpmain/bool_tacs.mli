(* derived tactics, some of which depend on theorems already having been
   proved *)

(* prove goals of the form A|- x=x, C *)
    val eq_tac :  Tactics.tactic
(* cut a named theorem *)
    val cut_thm : string -> Tactics.tactic
(* unfold a definition *)
    val unfold : string -> int -> Tactics.tactic

(* rewrite with a named theorem *)
    val rewrite_thm : string -> Logic.fident -> Tactics.tactic

(* rewrite with a named theorem *)
    val rewrite_dir : Rewrite.direction
      -> Logic.thm list -> Logic.fident -> Tactics.tactic

(* rewrite left-right/right-left with a named theorem *)

val rewrite_rl: string -> Logic.fident -> Tactics.tactic
val rewrite_lr: string -> Logic.fident -> Tactics.tactic

(* rewrite with a given theorem *)
    val rewrite : Logic.thm -> Logic.fident -> Tactics.tactic

(* test and rules for Iff *)
val is_iff: Formula.form -> bool
val iffI_rule: Logic.fident -> Logic.rule
val iffI: ?c:Logic.fident -> Tactics.tactic


val asm_elims : 
    unit -> ((Formula.form->bool) * (Logic.fident -> Logic.rule)) list
val conc_elims : 
    unit -> ((Formula.form->bool) * (Logic.fident -> Logic.rule)) list

val false_rule0: Logic.fident -> Logic.rule
val false_rule:  ?a:Logic.fident -> Logic.rule

(* flatten_tac: logical simplification of a sequent without
   creating new sequents *)
    val flatten_tac : Tactics.tactic

(* split_tac: logical simplification of a sequent
   creating new sequents *)
    val split_tac : Tactics.tactic

(* [inst_tac i consts] instantiate assumption (if [i<0]) or conclusion
   with list of constants *)

val inst_tac: Logic.fident -> Basic.term list -> Tactics.tactic 
val inst_asm : ?a:Logic.fident -> Basic.term list -> Tactics.tactic
val inst_concl : ?c:Logic.fident -> Basic.term list -> Tactics.tactic

(* cases tactics *)

val cases_tac0: Basic.term -> Tactics.tactic
val cases_tac: string -> Tactics.tactic

(* convert boolean equality to iff *)
val equals_tac: ?f:Logic.fident -> Tactics.tactic

(* tactics for trivial boolean *)
val false_tac: Tactics.tactic
val bool_tac:  Tactics.tactic

(* match_mp_tac *)

val match_mp_tac: Logic.thm -> int -> Tactics.tactic

val back_mp_tac: int -> int -> Tactics.tactic
