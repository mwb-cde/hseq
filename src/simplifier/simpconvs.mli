(*-----
 Name: simpconvs.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

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


(**
   [simple_rewrite_conv scp rule trm]

   Form an equality from term [trm=!x .. y: body] and [rule=(l=r)] by
   descending through topmost universal quantifiers of [trm] and
   applying rewrite once only to the body of [trm]. Return the theorem
   stating the equality [(!x..y: body)=(!x.. y: (body=r))].

   e.g 
   [simple_rewrite_conv  |- ! a: a = true,   << !x y z: (f x y z) >> ]
   ->
   [ |- (!x y z: f x y z) = (! x y z: (f x y z = true))  ]
 *)
val simple_rewrite_conv :
    Scope.t -> Logic.thm -> Basic.term -> Logic.thm

(**
   [simple_rewrite_rule scp rule thm]: 
   Apply [simple_rewrite_conv] to theorem [thm].
 *)
val simple_rewrite_rule :
    Scope.t -> Logic.thm -> Logic.thm -> Logic.thm

(**
   [simple_asm_rewrite_tac rule asm]

   Rewrite assumption [asm] with [rule] by descending through topmost
   universal quantifiers and applying rewrite once only to the body of
   [asm].  i.e.
   
   rule=|- lhs = rhs
   asm:lhs, A |- C 
   -->
   asm:rhs, A |- C
 *)
val simple_asm_rewrite_tac :
    Logic.thm -> Logic.label -> Logic.node -> Logic.Subgoals.branch

(** 
   [asm_rewrite thm tg g]:

   Rewrite assumption [tg] with rule [thm] = |- a=b

   tg:a, asms |- concl
   -->
   tg:b, asms |- concl

 *)
val asm_rewrite :
    Logic.thm -> Tag.t -> Logic.node -> Logic.Subgoals.branch


(** [negate_concl info t g]:
   Negate conclusion [t], making it assumption tagged [t'].

   asms|- t:c, cncl
   -->
   t':~c, asms |- cncl
   info [] [t'] []
 *)
val negate_concl :
    Logic.info option ->
      Logic.label -> Logic.Subgoals.node -> Logic.Subgoals.branch

(**
   [is_many_conj thm]: true if [thm] is of the form |- a and b and ... and z 

   [is_iffterm (vars, cnd, main)]: true if [main] is of the for [a iff b] 

   [is_negation (var, cnd, main): true if [main] is of the form [not a]

   [is_equality (var, cnd, main)]: true if [main] is of the form a=b 
 *)
val is_many_conj : Logic.thm -> bool
val is_iffterm : 'a * 'b * Basic.term -> bool
val is_negation : 'a * 'b * Basic.term -> bool
val is_equality : 'a * 'b * Basic.term -> bool

(** {6c Utility functions} *)

(** [find_variables is_var vars trm]
   find all subterms [t] of [trm] s.t. [(is_var t)] is true.
   add [t] to [vars]
   return [vars]

[check_variables is_var vars trm]
   check that all subterms [t] of [trm] s.t. [is_var t] 
   is in [vars]

 *)
val find_variables :
    (Basic.binders -> bool) ->
      Term.substitution -> Basic.term -> Term.substitution
val check_variables :
    (Basic.binders -> bool) -> Term.substitution -> Basic.term -> unit

(**  [is_rr_rule qs c l r]: 

   Check that [c=>(l=r)] is a rewrite rule.

   All variables (in [qs]) occuring in [c] or [r] must also occur in [l]
   return: [(cnd, rhs)]
   where 
   [cnd]= [Some(true)] iff all variables in [c] occur in [l]
   = [None] if no condition
   [rhs]= [Some(true)] iff all variables in [r] occur in [l]
   = [None] if no [rhs]
 *)
val is_rr_rule :
    Basic.binders list ->
      Basic.term option ->
	Basic.term -> Basic.term option -> bool option * bool option

(** [strip_qnt_cond trm]
   split rule [trm] into variable binders, condition, equality
   rules are of the form:
   a=>c
   c
 *)
val strip_qnt_cond :
    Basic.term -> Basic.binders list * Basic.term option * Basic.term


(** {6c Functions to make simp rules from theorems} *)

(**
   [thm_to_rule scp thm]: convert theorem [thm] to a list of theorems suitable 
   for rewriting.

   Conversion:
   |- l=r   ->  no change, if all variables in [r] also occur in [l])
   -> |- (l=r)=true, otherwise

   |- c => l = r -> no change, if all variables in [r] and [c] 
   also occur in [l]
   -> |- (c=> l = r)=true, otherwise

   |- a -> |- a=true
   |- c=> a -> |- c => a=true
   |- not a ->  |- a=false
   |- c=> not a -> |- c => a = false
   |- a and b -> |- a; |- b
   |- false -> not true
 *)


(** 
   [apply_first lst x]

   Apply each function in [lst], return the result of the first to 
   succeed.

   Fail if all functions in [lst] fail.
 *)
val apply_first : ('a -> 'b) list -> 'a -> 'b



(** Functions to test and convert a specific type of theorem 

   The tests.

   [is_constant t]: [t] is a boolean constant (true/false)
   or in the form [l=r] where [r] is a boolean constant.

   [is_neg_all t]: [t] is in the form [not ! a: b]
   
   [is_neg_exists t]: [t] is in the form [not ? a: b]

   [is_rr_equality t]: [t] is a proper rewrite rule, in the form 
   |- a=b or in the form |- c=>a=b
   where all free variables in c and b also occur in a.
*)
val is_constant : Basic.binders list * Basic.term option * Basic.term -> bool
val is_neg_all:  Basic.binders list * Basic.term option * Basic.term -> bool
val is_neg_exists: Basic.binders list * Basic.term option * Basic.term -> bool
val is_rr_equality: Basic.binders list * Basic.term option * Basic.term -> bool

(**
   The conversion functions.

   [accept_all_thms]: convert |- a to |- a=true 

   [do_rr_equality]: accept |- l=r or |= c=> l=r 

   [do_fact_rule]: 
   convert |- a to |- a=true 
   and  |- c=> a to |- c => a=true

   [do_neg_rule]: convert |- not a to |- a=false 
   and |- c=> not a to |- c=> a=false

   [do_neg_all_rule]: convert |- not (!a: b) to |- ?a: not b
   then convert the new theorem.

   [do_neg_exists_rule]: convert |- not (?a: b) to |- !a: not b
   then convert the new theorem.

   [do_conj_rule]: convert  |- a and b to |- a and |- b.

   [single_thm_to_rules scp thm]:
   convert a theorem stating a single fact to a rewrite-rule.

   [multi_thm_to_rules scp thm]:
   convert a theorem stating many facts to a list of rewrite-rules.

   [thm_to_rules scp thm]: Toplevel conversion function.
   Convert theorem [thm] to a list of rules
 *)
val accept_all_thms :
    (Scope.t 
       * Logic.thm 
       * (Basic.binders list * Basic.term option * Basic.term))
     -> Logic.thm
val do_rr_equality :
    (Scope.t 
       * Logic.thm 
       * (Basic.binders list * Basic.term option * Basic.term))
     -> Logic.thm
val do_fact_rule :
    Scope.t * Logic.thm *
    (Basic.binders list * Basic.term option * Basic.term) -> Logic.thm
val do_neg_rule :
    Scope.t * Logic.thm *
    (Basic.binders list * Basic.term option * Basic.term) -> Logic.thm

val do_neg_all_rule :
    Scope.t * Logic.thm *
    (Basic.binders list * Basic.term option * Basic.term) -> Logic.thm

val do_neg_exists_rule :
    Scope.t * Logic.thm *
    (Basic.binders list * Basic.term option * Basic.term) -> Logic.thm

val single_thm_to_rules : Scope.t -> Logic.thm -> Logic.thm
val do_conj_rule : Scope.t -> Logic.thm -> Logic.thm list
val apply_get_list : ('a -> 'a list) -> 'a list -> 'a list -> 'a list
val app_first : ('a -> 'b) list -> 'a -> 'b
val multi_thm_to_rules : Scope.t -> Logic.thm -> Logic.thm list
val thm_to_rules : Scope.t -> Logic.thm -> Logic.thm list


(** {6c Converting assumptions to rewrite rules} *)


(** 

   Functions to convert an assumption

   [accept_asm]: convert |- a to |- a=true 

   [rr_equality_asm]: accept |- l=r or |= c=> l=r 

   [fact_rule_asm]: 
   convert |- a to |- a=true 
   and  |- c=> a to |- c => a=true

   [neg_rule_asm]: convert |- not a to |- a=false 
   and |- c=> not a to |- c=> a=false

   [neg_all_rule_asm]: convert |- not (!a: b) to |- ?a: not b
   then convert the new theorem.

   [neg_exists_rule_asm]: convert |- not (?a: b) to |- !a: not b
   then convert the new theorem.

   [conj_rule_asm]: convert  |- a and b to |- a and |- b.

   [single_asm_to_rules l g]:
   convert an assumption stating a single fact to a rewrite-rule.

   [asm_to_rules tg ret g]: Toplevel conversion function.  Convert
   assumption [tg] of goal [g] to one or more rules.  The tag of each
   rule (including [tg]) generated from [tg] is stored in [ret].
 *)
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


(**
   [prepare_concls data cs except g]

   Copy and lift the conclusions labelled with a tag in [cs] into the
   assumptions (by negation) then prepare the assumption for use as a
   simp rule.  Ignore conclusions for which [except] is true. Set
   [!data] to the list of pairs [(c, a)] where [c] is the tag of the
   original conclusion and [a] is the tag of the new assumption (which
   is to be used as a simp rule).
 *)
val prepare_concl :
  (Tag.t * Tag.t) list ref ->
  (Tag.t -> bool) -> Tag.t -> Logic.Subgoals.node -> Logic.Subgoals.branch 
val prepare_concls :
  (Tag.t * Tag.t) list ref ->
  Tag.t list ->
  (Tag.t -> bool) -> Logic.Subgoals.node -> Logic.Subgoals.branch 

(**
   [prepare_asms data cs except g]

   Copy the assumptions labelled with a tag in [cs] then prepare the
   copy for use as a rewrite rule.  Ignore assumptions for which
   [except] is true. Set [!data] to the list of pairs [(a, na)] where
   [a] is the tag of the original assumption and [na] is the tag of the
   new assumption generated from [a].
 *)
val prepare_asm :
  (Tag.t * Tag.t) list ref ->
  (Tag.t -> bool) -> Tag.t -> Logic.Subgoals.node -> Logic.Subgoals.branch
val prepare_asms :
  (Tag.t * Tag.t) list ref ->
  Tag.t list ->
  (Tag.t -> bool) -> Logic.Subgoals.node -> Logic.Subgoals.branch


