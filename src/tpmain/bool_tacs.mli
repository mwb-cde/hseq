(* derived tactics, some of which depend on theorems already having been
   proved *)

(* prove goals of the form A|- x=x, C *)
    val eq_tac :  Tactics.tactic
(* cut a named theorem *)
    val cut_thm : string -> Tactics.tactic
(* unfold a definition *)
    val unfold : string -> int -> Tactics.tactic

(* rewrite with a named theorem *)
    val rewrite_thm : string -> int -> Tactics.tactic

(* rewrite with a named theorem *)
    val rewrite_dir : bool -> Logic.thm list -> int -> Tactics.tactic

(* rewrite left-right/right-left with a named theorem *)

val rewrite_rl: string -> int -> Tactics.tactic
val rewrite_lr: string -> int -> Tactics.tactic

(* rewrite with a given theorem *)
    val rewrite : Logic.thm -> int -> Tactics.tactic

(* test and rules for Iff *)
val is_iff: Formula.form -> bool
val iffI_rule: int -> Logic.rule
val iffI: Tactics.tactic


val asm_elims : unit -> ((Formula.form->bool) * (int -> Logic.rule)) list
val conc_elims : unit -> ((Formula.form->bool) * (int -> Logic.rule)) list

val false_rule0: int -> Logic.rule
val false_rule:  Logic.rule

(* flatten_tac: logical simplification of a sequent without
   creating new sequents *)
    val flatten_tac : Tactics.tactic

(* split_tac: logical simplification of a sequent
   creating new sequents *)
    val split_tac : Tactics.tactic

(* [inst_tac i consts] instantiate assumption (if [i<0]) or conclusion
   with list of constants *)

val inst_rule : string list -> int -> Logic.rule
val inst_term_rule : Basic.term list -> int -> Logic.rule
val inst_tac: string list -> int -> Tactics.tactic 
val inst_asm : string list -> Tactics.tactic
val inst_concl : string list -> Tactics.tactic

(* cases tactics *)

val cases_tac0: Basic.term -> Tactics.tactic
val cases_tac: string -> Tactics.tactic

(* convert boolean equality to iff *)
val equals_tac: int -> Tactics.tactic

(* tactics for trivial boolean *)
val false_tac: Tactics.tactic
val bool_tac:  Tactics.tactic

(* match_mp_tac *)

val match_mp_tac: Logic.thm -> int -> Tactics.tactic

val back_mp_tac: int -> int -> Tactics.tactic

