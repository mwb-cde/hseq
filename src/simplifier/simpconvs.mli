(**
   [cond_rule_true_ax] : |- !x y: (x=>y) = (x => (y=true))
*)
val make_cond_rule_true_ax : unit -> Logic.thm
val cond_rule_true_ax : Logic.thm option ref
val get_cond_rule_true_ax : unit -> Logic.thm

(**
   [cond_rule_false_ax]: |- !x y: (x=>~y) = (x => (y=false))
 *)
val make_cond_rule_false_ax : unit -> Logic.thm
val cond_rule_false_ax : Logic.thm option ref
val get_cond_rule_false_ax : unit -> Logic.thm


val simple_rewrite_conv :
    Gtypes.scope -> Logic.thm -> Basic.term -> Logic.thm
val simple_rewrite_rule :
    Gtypes.scope -> Logic.thm -> Logic.thm -> Logic.thm
val simple_asm_rewrite_tac :
    Logic.thm -> Logic.label -> Logic.node -> Logic.Subgoals.branch
val asm_rewrite :
    Logic.thm -> Tag.t -> Logic.node -> Logic.Subgoals.branch
val many_conj_conv : Logic.thm -> Logic.thm list
val negate_concl :
    Logic.info option ->
      Logic.label -> Logic.Subgoals.node -> Logic.Subgoals.branch


val is_many_conj : Logic.thm -> bool
val is_iffterm : 'a * 'b * Basic.term -> bool
val is_negation : 'a * 'b * Basic.term -> bool
val is_equality : 'a * 'b * Basic.term -> bool
val find_variables :
    (Basic.binders -> bool) ->
      Term.substitution -> Basic.term -> Term.substitution
val check_variables :
    (Basic.binders -> bool) -> Term.substitution -> Basic.term -> unit
val is_rr_rule :
    Basic.binders list ->
      Basic.term option ->
	Basic.term -> Basic.term option -> bool option * bool option
val strip_qnt_cond :
    Basic.term -> Basic.binders list * Basic.term option * Basic.term

val is_constant : Basic.binders list * Basic.term option * Basic.term -> bool
val is_neg_all:  Basic.binders list * Basic.term option * Basic.term -> bool
val is_neg_exists: Basic.binders list * Basic.term option * Basic.term -> bool
val is_rr_equality: Basic.binders list * Basic.term option * Basic.term -> bool

val apply_first : ('a -> 'b) list -> 'a -> 'b

val accept_all_thms :
    (Gtypes.scope 
       * Logic.thm 
       * (Basic.binders list * Basic.term option * Basic.term))
     -> Logic.thm
val do_rr_equality :
    (Gtypes.scope 
       * Logic.thm 
       * (Basic.binders list * Basic.term option * Basic.term))
     -> Logic.thm
val do_fact_rule :
    Gtypes.scope * Logic.thm *
    (Basic.binders list * Basic.term option * Basic.term) -> Logic.thm
val do_neg_rule :
    Gtypes.scope * Logic.thm *
    (Basic.binders list * Basic.term option * Basic.term) -> Logic.thm

val do_neg_all_rule :
    Gtypes.scope * Logic.thm *
    (Basic.binders list * Basic.term option * Basic.term) -> Logic.thm

val do_neg_exists_rule :
    Gtypes.scope * Logic.thm *
    (Basic.binders list * Basic.term option * Basic.term) -> Logic.thm

val single_thm_to_rules : Gtypes.scope -> Logic.thm -> Logic.thm
val do_conj_rule : 'a -> Logic.thm -> Logic.thm list
val apply_get_list : ('a -> 'a list) -> 'a list -> 'a list -> 'a list
val app_first : ('a -> 'b) list -> 'a -> 'b
val multi_thm_to_rules : 'a -> Logic.thm -> Logic.thm list
val thm_to_rules : Gtypes.scope -> Logic.thm -> Logic.thm list
val make_thm_rule :
    Logic.thm ->
      Simpset.rule
val thm_to_entries :
    Gtypes.scope ->
      Logic.thm ->
	Simpset.rule list
(** 
   [add_simp_rule set rules]: Add [rules] to simpset [set].
   [simp_add_thm scp set thms]: Add theorem [thm] to simpset [set].
   [simp_add_thms scp set thms]: Add theorems [thms] to simpset [set].
*)
val add_simp_rule :
    Simpset.simpset -> Simpset.rule list -> Simpset.simpset
val simpset_add_thm :
    Gtypes.scope -> Simpset.simpset -> Logic.thm -> Simpset.simpset
val simpset_add_thms :
    Gtypes.scope -> Simpset.simpset -> Logic.thm list -> Simpset.simpset



val accept_asm :
    Tag.t *
    (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
val rr_equality_asm :
    Tag.t *
    (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
val fact_rule_asm :
    Tag.t *
    (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
val neg_rule_asm :
    Tag.t *
    (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
val neg_all_rule_asm :
    Tag.t *
    (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
val neg_exists_rule_asm :
    Tag.t *
    (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic

val single_asm_to_rule : Tag.t -> Tactics.tactic
val asm_to_rules : Tag.t -> Tag.t list ref -> Tactics.tactic

val simpset_add_asm: 
    Simpset.simpset -> Tag.t -> Logic.node -> Simpset.simpset

val prepare_concl :
  (Tag.t * Tag.t) list ref ->
  (Tag.t -> bool) -> Tag.t -> Logic.Subgoals.node -> Logic.Subgoals.branch 
val prepare_concls :
  (Tag.t * Tag.t) list ref ->
  Tag.t list ->
  (Tag.t -> bool) -> Logic.Subgoals.node -> Logic.Subgoals.branch 

val prepare_asm :
  (Tag.t * Tag.t) list ref ->
  (Tag.t -> bool) -> Tag.t -> Logic.Subgoals.node -> Logic.Subgoals.branch

val prepare_asms :
  (Tag.t * Tag.t) list ref ->
  Tag.t list ->
  (Tag.t -> bool) -> Logic.Subgoals.node -> Logic.Subgoals.branch

val make_simp_asm :
  Tag.t ->
  Logic.node ->
  Tag.t *
      Simpset.rule
val make_simp_asms :
  Tag.t list ->
  (Tag.t -> bool) ->
  Logic.node ->
  (Tag.t *
     Simpset.rule ) list

val make_simp_asm_rule :
  Tag.t 
  -> Formula.form
      -> Simpset.rule
val make_simp_asm_rules :
    (Tag.t * Formula.form -> bool) 
  -> (Tag.t * Formula.form) list 
      -> Simpset.rule list


(* OLD STUFF *)

(*
val test_apply_tac :
    'a ->
      ((Basic.term -> bool) * ('a -> Tag.t -> Logic.node -> 'b)) list ->
	Tag.t -> Logic.node -> 'b
val is_cond_true_fact : Basic.term -> bool
val is_cond_false_fact : Basic.term -> bool
val is_false_fact : Basic.term -> bool
val is_true_fact : 'a -> bool
val cond_true_asm_to_rule :
    'a -> Tag.t -> Logic.node -> Logic.Subgoals.branch
val cond_false_asm_to_rule :
    'a -> Tag.t -> Logic.node -> Logic.Subgoals.branch
val false_asm_to_rule :
    'a -> Tag.t -> Logic.node -> Logic.Subgoals.branch
val true_asm_to_rule : 'a -> Tag.t -> Logic.node -> Logic.Subgoals.branch
val asm_rule_makers :
    ((Basic.term -> bool) *
       ('a -> Tag.t -> Logic.node -> Logic.Subgoals.branch))
    list
val prep_asm_rule : 'a -> Tag.t -> Logic.node -> Logic.Subgoals.branch
val term_cond_rewrite :
    Gtypes.scope -> Logic.thm -> Basic.term -> Basic.term
val form_cond_rewrite :
    Gtypes.scope -> Logic.thm -> Formula.form -> Formula.form
	
*)
