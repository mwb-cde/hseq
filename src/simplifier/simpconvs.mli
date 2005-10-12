(*-----
 Name: simpconvs.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** Theorems, rules and conversions for the simplifier. *)


(** {5 Theorems} *)

(**
   [cond_rule_true_thm] : |- !x y: (x=>y) = (x => (y=true))
*)

val make_cond_rule_true_thm : unit -> Logic.thm
val cond_rule_true_thm : unit -> Logic.thm

(**
   [cond_rule_false_thm]: |- !x y: (x=>~y) = (x => (y=false))
 *)

val make_cond_rule_false_thm : unit -> Logic.thm
val cond_rule_false_thm : unit -> Logic.thm


(** {5 Rewriting conversions and tactics} *)

val simple_rewrite_conv :
    Scope.t -> Logic.thm -> Basic.term -> Logic.thm
(**
   [simple_rewrite_conv scp rule trm]

   Form an equality from term [trm=!x .. y: body] and [rule=(l=r)] by
   descending through topmost universal quantifiers of [trm] and
   applying rewrite once only to the body of [trm]. Return the theorem
   stating the equality [(!x..y: body)=(!x.. y: (body=r))].

   Example: 
   [simple_rewrite_conv  |- ! a: a = true,   << !x y z: (f x y z) >> ]
   ->
   [ |- (!x y z: f x y z) = (! x y z: (f x y z = true))  ]
 *)

val simple_rewrite_rule :
    Scope.t -> Logic.thm -> Logic.thm -> Logic.thm
(**
   [simple_rewrite_rule scp rule thm]: 
   Apply [simple_rewrite_conv] to theorem [thm].
 *)

val simple_asm_rewrite_tac :
    ?info:Logic.info
    -> Logic.thm -> Logic.label -> Tactics.tactic
(**
   [simple_asm_rewrite_tac rule l asm]

   Rewrite assumption [asm] with [rule] by descending through topmost
   universal quantifiers and applying rewrite once only to the body of
   [asm].  i.e.
   
   rule = [ |- lhs = rhs]

   {L
   lhs{_ l}, A |- C 
   -->
   rhs{_ l1}, A |- C

   info: [goals = [], aforms=[l1], cforms=[l], terms = []]
   }

 *)


val negate_concl_tac :
    ?info: Logic.info ->
      Logic.label -> Logic.Subgoals.node -> Logic.Subgoals.branch
(** 
   [negate_concl_tac info t g]:
   Negate conclusion [t], making it assumption tagged [t'].

   {L
   asms|- c{_ t}, cncl

   -->

   ~c{_ t1}, asms |- cncl
   }

   info: [goals = [], aforms=[t1], cforms=[], terms = []]
 *)

(** 
   {5 Preparing simplifier rules} 

   Simplification rules come from a theorem or assumption [f]. 

   An unconditional formula [f] is transformed to [T(f)] as follows:

   {L
   [T(x and y)] = [T(x), T(y)]

   [T(x iff y)] = [T(x=y)], if this results in a rewrite rule

   [T(not x)] = [x=false]

   [T(x)] = [x=true]
   }

   A conditional formula [f] is of the form [c=>x] and
   [T(c=>x)] is as for unconditional formulas 
   except [T(c=>(x and y))] = [c=>(x and y)].

   If [f] is a rewrite rule, of the form [x=y], its transformation is
   [T(x=y)] = [x=y], if all variables in [y] also occur in [x].
   [T(x=y)] = [(x=y)=true], otherwise.

   [T(c=>x=y)] = [c=>(x=y)], if all variables in [y] and [c] occur in [x].
   [T(c=>x=y)] = [c=>((x=y)=true)], if variables in [y] don't occur in [x].
   and all variables in [c] occur in [x].
   [T(c=>x=y)] = [(c=>x=y)=true] otherwise
*)

(** {7 Tests on terms and theorems} **)

val is_many_conj : Logic.thm -> bool
(**
   [is_many_conj thm]: true if [thm] is of the form
   [ |- a and b and ... and z ].
*)

val is_iffterm : 'a * 'b * Basic.term -> bool
(**
   [is_iffterm (vars, cnd, main)]: true if [main] is of the for [a iff b].
*)

val is_negation : 'a * 'b * Basic.term -> bool
(**
   [is_negation (var, cnd, main)]: true if [main] is of the form [not a].
*)

val is_equality : 'a * 'b * Basic.term -> bool
(**
   [is_equality (var, cnd, main)]: true if [main] is of the form [a=b]. 
*)

val is_constant : 
  Basic.term list 
  -> Basic.binders list * Basic.term option * Basic.term -> bool
(** 
   [is_constant lst t]: [t] is in the list (of constants) lst
   or in the form [l=r] where [r] is a boolean constant.
*)

val is_constant_true : 
  Basic.binders list * Basic.term option * Basic.term -> bool
(** 
   [is_constant_true t]: [t] is boolean true.
*)

val is_constant_bool : 
  Basic.binders list * Basic.term option * Basic.term -> bool
(** 
   [is_constant_bool t]: [t] is boolean true or false.
*)


val is_neg_all:  Basic.binders list * Basic.term option * Basic.term -> bool
(**  [is_neg_all (var, cnd, trm)]: [trm] is in the form [not ! a: b] *)

val is_neg_exists: Basic.binders list * Basic.term option * Basic.term -> bool
(** [is_neg_exists (var, cnd, trm)t]: [trm] is in the form [not ? a: b] *)

val is_rr_rule :
    (Basic.binders list * Basic.term option 
     * Basic.term * Basic.term option)
  -> bool option * bool option
(**  
   [is_rr_rule (qs, c, l, r)]: Check that [c=>(l=r)] is a rewrite rule.

   All variables (in [qs]) occuring in [c] or [r] must also occur in [l]
   return: [(cnd, rhs)]

   where 
   [cnd = Some(true)] iff all variables in [c] occur in [l].
   [cnd = None] if no condition.
   [rhs = Some(true)] iff all variables in [r] occur in [l].
   [rhs = None] if no [rhs].
 *)


val is_rr_equality: Basic.binders list * Basic.term option * Basic.term -> bool
(** 
   [is_rr_equality (var, cnd, trm)]: [trm] is a proper rewrite rule,
   in the form [|- a=b] or in the form [|- c=>a=b] where all free
   variables in [c] and [b] also occur in [a].
*)


(** {7 Rules from theorems}

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

val accept_all_thms :
    (Scope.t 
       * Logic.thm 
       * (Basic.binders list * Basic.term option * Basic.term))
     -> Logic.thm
(** [accept_all_thms]: Convert [|- a] -> [|- a=true] *)

val do_rr_equality :
    (Scope.t 
       * Logic.thm 
       * (Basic.binders list * Basic.term option * Basic.term))
     -> Logic.thm
(** [do_rr_equality]: Accept [|- l=r] or [|= c=> l=r] *)

val do_fact_rule :
    Scope.t * Logic.thm *
    (Basic.binders list * Basic.term option * Basic.term) -> Logic.thm
(**
   [do_fact_rule]: Convert [|- a] -> [|- a=true]
   and [|- c=>a] -> [|- c => a=true].
*)

val do_neg_rule :
    Scope.t * Logic.thm *
    (Basic.binders list * Basic.term option * Basic.term) -> Logic.thm
(**
   [do_neg_rule]: Convert [|- not a] -> [|- a=false]
   and [|- c=> not a] -> [|- c=> a=false].
*)

val do_neg_all_rule :
    Scope.t * Logic.thm *
    (Basic.binders list * Basic.term option * Basic.term) -> Logic.thm
(**
   [do_neg_all_rule]: Convert [|- not (!a: b)] -> [|- ?a: not b]
   then convert the new theorem.
*)

val do_neg_exists_rule :
    Scope.t * Logic.thm *
    (Basic.binders list * Basic.term option * Basic.term) -> Logic.thm
(**
   [do_neg_exists_rule]: Convert [|- not (?a: b)] -> [|- !a: not b]
   then convert the new theorem.
*)

val do_conj_rule : Scope.t -> Logic.thm -> Logic.thm list
(**
   [do_conj_rule]: Convert [|- a and b] -> [|- a] and [|- b].
*)

val single_thm_to_rules : Scope.t -> Logic.thm -> Logic.thm
(**
   [single_thm_to_rules scp thm]: Convert a theorem stating a single
   fact to a rewrite-rule.
*)

val multi_thm_to_rules : Scope.t -> Logic.thm -> Logic.thm list
(**
   [multi_thm_to_rules scp thm]: Convert a theorem stating many facts
   to a list of rewrite-rules.
*)

val thm_to_rules : Scope.t -> Logic.thm -> Logic.thm list
(**
   [thm_to_rules scp thm]: Toplevel conversion function.
   Convert theorem [thm] to a list of rules.
*)

(** {7 Rules from assumptions} *)

val asm_rewrite_tac : ?info:Logic.info -> 
    Logic.thm -> Tag.t -> Logic.node -> Logic.Subgoals.branch
(** 
   [asm_rewrite thm tg g]:

   Rewrite assumption [tg] with rule [thm] = |- a=b

   tg:a, asms |- concl
   -->
   tg:b, asms |- concl

 *)

val add_asm_tac:
  Logic.tagged_form list ref
  -> Tag.t -> Tactics.tactic
(**
   [add_asm_tac ret tg g]: Add the assumption labelled [tg] to
   [ret].
   
   If g = [ a{_ tg}, asms |- concl ]
   then return [ret = [a]::!(reg)]
*)

val asm_rewrite_add_tac : 
  ?info:Logic.info 
  -> Logic.tagged_form list ref
  -> Logic.thm -> Tag.t -> Tactics.tactic
(** 
   [asm_rewrite_add_tac ret thm tg g]:

   Rewrite assumption [tg] with rule [thm] = |- a=b

   a{_ tg}, asms |- concl
   -->
   b{_ tg}, asms |- concl

   Return [ret = [b]::!(reg)]
 *)


val accept_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   [accept_asm]: Convert [|- a] -> [|- a=true]
*)

val rr_equality_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   [rr_equality_asm]: Accept [|- l=r] or [|= c=> l=r]
*)

val fact_rule_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   [fact_rule_asm]: Convert [|- a] -> [|- a=true]
   and [|- c=>a] to [|- c =>a=true]
*)

val neg_rule_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   [neg_rule_asm]: Convert [|- not a] to [|- a=false]
   and [|- c => not a] to [|- c => a=false].
*)

val conj_rule_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   [conj_rule_asm]: convert |- (a & b)  to |- a and |- b
*)

val neg_all_rule_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   [neg_all_rule_asm]: Convert [|- not (!a: b)] -> [|- ?a: not b]
   then convert the new theorem.
*)

val neg_exists_rule_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   [neg_exists_rule_asm]: Convert [|- not (?a: b)] -> [|- !a: not b]
   then convert the new theorem.
*)

val single_asm_to_rule : 
  Logic.tagged_form list ref -> Tag.t -> Tactics.tactic
 (**
  [single_asm_to_rules f g]: Convert the assumption [f] stating a single
  fact to a rewrite-rule. Formula [f] must be an assumption of [g].
*)

val asm_to_rules : 
  Logic.tagged_form list ref -> Tag.t -> Tactics.tactic
(**
   [asm_to_rules tg ret g]: Toplevel conversion function.  Convert
   assumption [tg] of goal [g] to one or more rules.  The (tagged)
   formula of each rule generated from [tg] is stored in [ret]. Note
   that the assumption identified by [tg] will be one of the rules in
   [ret].
*)


(** {7 Rules from assumptions and conclusions} *)

(** Information about rules created from a sequent formula. **)
type rule_data = 
    { 
      src: Logic.tagged_form; 
      (** The source of the rules. **)
      new_asm: Formula.form;
            (**
      	       The new assumption formed from the formula (e.g. when a
      	       conclusion is lifted into the assumptions) to add to the
      	       simp set (for information). 
	    **)
      new_rules: (Logic.tagged_form) list 
      	(** The rules formed fro the source. *)
    }

val mk_rule_data:
  Logic.tagged_form -> Formula.form
  -> Logic.tagged_form list -> rule_data
(** Constructor for rule data. *)

val unpack_rule_data: 
  rule_data list -> 
  (Logic.tagged_form list * Formula.form list * Logic.tagged_form list)
(**
    [unpack_rule_data rd]: Unpack a list of rule data in a list of
    sources, a list of new assumptions and a list of new rules.
*)


(** {7 Rules from assumptions and conclusions} *)

val prepare_asm :
  rule_data list ref 
  -> (Tag.t -> bool) -> Tag.t 
    -> Tactics.tactic
(**
   [prepare_asm data except a goal]: Prepare assumption labelled [a]
   for use as a simp rule. 

   Does nothing if [except a] is true. Solves the goal if [a] is
   [false]. Otherwise, returns the list of new assumptions formed from
   [a] which are to be used as simp rules.

   {ul
   {- Copy the assumption to get new assumption [a1].}
   {- Call [asm_to_rules] on [a1] to get [rules].}
   {- For each new assumption [r], store the result as the pair
   [\{ src = a; new_asm = a1; new_rules = rules \}]}.
   {- Return the result in [data]. }
   }
*)


val prepare_asms :
  rule_data list ref 
  -> Tag.t list 
    -> (Tag.t -> bool) 
      -> Tactics.tactic
(**
   [prepare_asms data asm except g]: Apply [prepare_asm] to each
   assumption in the list [asms]. Return the cumulative results.
 *)


val prepare_concl :
  rule_data list ref 
  -> (Tag.t -> bool) -> Tag.t 
    -> Tactics.tactic
(**
   [prepare_concl data except c goal]: Prepare conclusion labelled [a]
   for use as a simp rule. 

   Does nothing if [except c] is true. Solves the goal if [c] is
   [true]. Otherwise, returns the list of new assumptions formed from
   [c] which are to be used as simp rules.

   {ul 
   {- Copy the conclusion and lift it into the assumptions (by
   negation) to get new assumption [a].}
   {- Call [asm_to_rules] on [a] to get [rules].}
   {- For each new assumption [r], store the result as the pair
   [\{ src = c; new_asm = a; new_rules = rules \}]}.
   {- Return the result in [data]. }
   }
*)

val prepare_concls :
  rule_data list ref ->
  Tag.t list ->
  (Tag.t -> bool) -> Tactics.tactic
(**
   [prepare_concls data concls except g]: Apply [prepare_concl] to each
   assumption in the list [concls]. Return the cumulative results.
 *)

      








(**** RETIRED

val prepare_asm :
  (Tag.t * Tag.t) list ref 
  -> (Tag.t -> bool) -> Tag.t 
    -> Tactics.tactic
(**
   [prepare_asm data except a goal]: Prepare assumption labelled [a]
   for use as a simp rule. 

   Does nothing if [except a] is true. Solves the goal if [a] is
   [false]. Otherwise, returns the list of new assumptions formed from
   [a] which are to be used as simp rules.

   {ul 
   {- Copy the assumption to get new assumption [a1].}
   {- Call [asm_to_rules] on [a1].}
   {- For each new assumption [r], store the result as the pair
   [(a, r)].}
   {- Return the list of pairs in [data]. }
   }

   Note that [(a, a1)] is stored in [data].
*)

val prepare_asms :
  (Tag.t * Tag.t) list ref 
  -> Tag.t list 
    -> (Tag.t -> bool) 
      -> Tactics.tactic
(**
   [prepare_asms data asm except g]: Apply [prepare_asm] to each
   assumption in the list [asms]. Return the cumulative results.
 *)


val prepare_concl :
  (Tag.t * Tag.t) list ref 
  -> (Tag.t -> bool) -> Tag.t 
    -> Tactics.tactic
(**
   [prepare_concl data except c goal]: Prepare conclusion labelled [a]
   for use as a simp rule. 

   Does nothing if [except c] is true. Solves the goal if [c] is
   [true]. Otherwise, returns the list of new assumptions formed from
   [c] which are to be used as simp rules.

   {ul 
   {- Copy the conclusion and lift it into the assumptions (by
   negation) to get new assumption [a].}
   {- Call [asm_to_rules] on [a].}
   {- For each new assumption [r], store the result as the pair
   [(c, r)].}
   {- Return the list of pairs in [data]. }
   }

   Note that [(c, a)] is stored in [data].
*)

val prepare_concls :
  (Tag.t * Tag.t) list ref ->
  Tag.t list ->
  (Tag.t -> bool) -> Logic.Subgoals.node -> Logic.Subgoals.branch 
(**
   [prepare_concls data concls except g]: Apply [prepare_concl] to each
   assumption in the list [concls]. Return the cumulative results.
 *)

****)
