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

(**
   [cond_rule_imp_false_thm]: |- !x: (x=>false) = (not x)
 *)

val make_cond_rule_imp_false_thm : unit -> Logic.thm
val cond_rule_imp_false_thm : unit -> Logic.thm

(**
   [cond_rule_imp_not_true_thm]: |- !x: (x=>not true) = (not x)
 *)

val make_cond_rule_imp_not_true_thm : unit -> Logic.thm
val cond_rule_imp_not_true_thm : unit -> Logic.thm

(**
   [neg_disj]: |- not (a | b) = ((not a) & (not b))
 *)

val make_neg_disj_thm : unit -> Logic.thm
val neg_disj_thm : unit -> Logic.thm

(**
   [neg_eq_sym]: |- not (a = b) = not (b = a)
 *)

val make_neg_eq_sym_thm : unit -> Logic.thm
val neg_eq_sym_thm : unit -> Logic.thm

(**
   [cond_neg_eq_sym]: |- (c => not (a = b)) = (c => not (b = a))
 *)

val make_cond_neg_eq_sym_thm : unit -> Logic.thm
val cond_neg_eq_sym_thm : unit -> Logic.thm

(**
   [cond_eq_sym]: |- (c => (a = b)) = (c => (b = a))
 *)

val make_cond_eq_sym_thm : unit -> Logic.thm
val cond_eq_sym_thm : unit -> Logic.thm

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

   [T(not (x or y))] = [T((not x) and (not y))]

   [T(x iff y)] = [T(x=y)], if this results in a rewrite rule (not
   implemented).

   [T(x = y)] = [((x=y) = true), ((y=x) = true)] if [x=y] is not a
   rewrite rule.

   [T(not(x=y))] = [((x=y) = false), ((y=x)=false)]
   
   [T(c=>not(x=y))] = [T(c=> ((x=y) = false)), T(c=> ((y=x)=false))]
   
   [T(not x)] = [x=false]

   [T(x)] = [x=true]
   }

   A conditional formula [f] is of the form [c=>x] and
   [T(c=>x)] is transformed as:
   {L
    [T(c=>false)] = [T(not c)]
   
    [T(c=>not true)] = [T(not c)]
   
    [T(c=>(x and y))] = [c=>(x and y)]
   
    [T(c=>x)] = [(c => T(x))]
    }

    If [f] is a rewrite rule, of the form [x=y], its transformation is
    [T(x=y)] = [x=y], if all variables in [y] also occur in [x].
    
    A conditional rewrite rule is transformed:
    {L
    [T(c=>x=y)] = [c=>(x=y)], if all variables in [y] and [c] occur in [x].
      
    [T(c=>x=y)] = [c=>((x=y)=true), c=>((y=x)=true)], if variables in
    [y] don't occur in [x].  and all variables in [c] occur in [x].
       
    [T(c=>x=y)] = [(c=>x=y)=true] otherwise
    }
       *)

(** {7 Tests on terms and theorems} *)

val is_many_conj : Logic.thm -> bool
(**
   [is_many_conj thm]: true if [thm] is of the form
   [ |- a and b and ... and z ].
*)

val is_neg_disj : Logic.thm -> bool
(**
   [is_neg_disj thm]: true if [thm] is of the form
   [ |- not (a or b) ].
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

val is_constant_false : 
  Basic.binders list * Basic.term option * Basic.term -> bool
(** 
   [is_constant_true t]: [t] is boolean false.
*)

val is_constant_bool : 
  Basic.binders list * Basic.term option * Basic.term -> bool
(** 
   [is_constant_bool t]: [t] is boolean true or false.
*)

val is_neg_all:  Basic.binders list * Basic.term option * Basic.term -> bool
(**  [is_neg_all (var, cnd, trm)]: [trm] is [not ! a: b] *)

val is_neg_exists: Basic.binders list * Basic.term option * Basic.term -> bool
(** [is_neg_exists (var, cnd, trm)t]: [trm] is in the form [not ? a: b] *)

val is_rr_rule :
    (Basic.binders list * Basic.term option 
     * Basic.term * Basic.term option)
  -> bool option * bool option
(**  
   [is_rr_rule (qs, c, l, r)]: Check that [c=>(l=r)] is a rewrite rule.

   All variables (in [qs]) occuring in [c] or [r] must occur in [l].
   
   Return: [(cnd, rhs)]

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
   {L
    [|- l=r]: Unchanged, if all variables in [r] also occur in [l]
    and [|- (l=r)=true], otherwise.

    [|- c=>l=r]:  no change, if all variables in [r] and [c] 
    also occur in [l] and [|- (c=> l=r)=true], otherwise

    [|- a] is [|- a=true]
   
    [|- c=> false] is [|- not c] and [|- c=> not true] is [|- not c]
   
    [|- c=>a] is  [|- c => a=true]
    
    [|- not (a = b)] is [|- (a=b)=false] and [|- (b=a) = false]
   
    [|- not a] is  [|- a=false]
   
    [ |- c=> not a] is  [|- c => a = false]
   
    [|- not (a or b)]  is  [|- (not a) and (not b)]

    [|- a and b]  is  [|- a] and [|- b]
   
    [|- true] is ignored
    
    [|- a] is [|- a = true], in all other cases
    }
*)
module Thms :
sig

val accept_all_thms :
  Logic.thm list -> 
    Scope.t * Logic.thm 
  * (Basic.binders list * Basic.term option * Basic.term) 
  -> Logic.thm list
(** 
    Convert [|- a] -> [|- a=true] but ignore [|-true]. Always succeeds.
*)

val do_rr_equality :
  Logic.thm list -> 
    Scope.t * Logic.thm 
  * (Basic.binders list * Basic.term option * Basic.term) 
  -> Logic.thm list
(** Accept [|- l=r] or [|= c=> l=r] *)

val do_eq_rule :
  Logic.thm list -> 
    Scope.t * Logic.thm 
  * (Basic.binders list * Basic.term option * Basic.term) 
  -> Logic.thm list
   (**
   Convert [|- (a = b)] to [|- (a = b) = true] and [|-
   (b=a)=true].  Convert [|- c=>(a = b)] to [|- c=>(a = b)
   =true] and [|- c=> (b=a)=true]
   *)
       
val do_fact_rule :
  Logic.thm list -> 
    Scope.t * Logic.thm 
  * (Basic.binders list * Basic.term option * Basic.term) 
  -> Logic.thm list
(**
   Convert [|- a] -> [|- a=true] and [|- c=>a] -> [|- c => a=true]
   and [|- c => false] -> [|- not c].
   Ignore [|- true] and [|- c=>true].
*)

val do_neg_eq_rule :
  Logic.thm list -> 
    Scope.t * Logic.thm 
  * (Basic.binders list * Basic.term option * Basic.term) 
  -> Logic.thm list
   (**
   Convert [|- not (a = b)] to [|- (a = b) =false] and [|-
   (b=a)=false].  Convert [|- c=>not (a = b)] to [|- c=>(a = b)
   =false] and [|- c=> (b=a)=false]
   *)
   
val do_neg_rule :
  Logic.thm list -> 
    Scope.t * Logic.thm 
  * (Basic.binders list * Basic.term option * Basic.term) 
  -> Logic.thm list
(**
   Convert [|- not a] to [|- a=false] and [|- c=> not a] to [|- c=>
   a=false] and [|- c=> not true ] to [|- not c] and [not (a or b)] to
   [(not a) and (not b)].
*)

val do_neg_all_rule :
  Logic.thm list -> 
    Scope.t * Logic.thm 
  * (Basic.binders list * Basic.term option * Basic.term) 
  -> Logic.thm list
(**
   Convert [|- not (!a: b)] -> [|- ?a: not b] then convert the new
   theorem. (Not used).
*)

val do_neg_exists_rule :
  Logic.thm list -> 
    Scope.t * Logic.thm 
  * (Basic.binders list * Basic.term option * Basic.term) 
  -> Logic.thm list
(**
   Convert [|- not (?a: b)] -> [|- !a: not b] then convert the new
   theorem.
*)

val do_neg_disj_rule :
  Logic.thm list -> 
    Scope.t * Logic.thm 
  * (Basic.binders list * Basic.term option * Basic.term) 
  -> Logic.thm list
(**
   Convert [|- not (a | b)] -> [|- (not a) & (not b)] then convert the new
   theorem.
*)

val do_conj_rule : 
  Logic.thm list -> 
    Scope.t * Logic.thm 
  * (Basic.binders list * Basic.term option * Basic.term) 
  -> Logic.thm list
(**
   Convert [|- a and b] -> [|- a] and [|- b].
*)

val single_thm_to_rules : 
  Logic.thm list -> Scope.t -> Logic.thm -> Logic.thm list
(**
   Convert a theorem to rewrite rules suitable for the simplifier.
*)

end

val thm_to_rules : Scope.t -> Logic.thm -> Logic.thm list
(**
   Toplevel conversion function.  Convert theorem [thm] to a list of
   rules.
*)

(** {7 Rules from assumptions} *)

val asm_rewrite_tac : ?info:Logic.info -> 
    Logic.thm -> Tag.t -> Logic.node -> Logic.Subgoals.branch
(** 
   [asm_rewrite thm tg g]:

   Rewrite assumption [tg] with rule [thm] = [|- a=b]

   a{_ tg}, asms |- concl
   -->
   b{_ tg}, asms |- concl
 *)


val qnt_asm_rewrite_tac : ?info:Logic.info -> 
    Logic.thm -> Tag.t -> Logic.node -> Logic.Subgoals.branch
(** 
   [qnt_asm_rewrite_tac thm tg g]: Rewrite a possibly quantified
   assumption.

   Descend through topmost quantifiers and 
   rewrite assumption [tg] with rule [thm] 
 
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


val solve_not_true_tac: Tag.t -> Tactics.tactic
(** 
  [solve_not_true_tac]: Solve goals of the form [(not true){_ tg} |- C].
*)

(** 
   Functions to convert an assumption

   [accept_asm]: Convert [a] to [a=true] and delete [true]

   [rr_equality_asm]: Accept [l=r] or [c=> l=r].

   [eq_asm]:
    Convert [a=b] to [(a=b) = true] and [(b=a)=true]
    and [c=> (a=b)] to [c=>((a=b) = true)] and [c=>((b=a)=true)]

   [fact_rule_asm]: 
   Convert [a] to [a=true]
   and [c=> false] to [(not c)]
   and [c=> a] to [c => a=true]
   pass [(a=b)] and [c=>(a=b)] to [eq_asm]
   and solve [false |- C]

   [neg_eq_asm]:
    Convert [not (a=b)] to [(a=b) = false] and [(b=a)=false]
    and [c=>not (a=b)] to [c=>((a=b) = false)] and [c=>((b=a)=false)]

   [neg_rule_asm]: 
    Convert [c=> not true] to [not c]
    and [not (a or b)] to [(not a) & (not b)]
    and [not a] to [a=false]
    and [c=> not a] to [c=> a=false]
    pass [not (a=b)] and [c=>not(a=b)] to [neg_eq_asm]
    and solve  [not true |- C]

   [conj_rule_asm]: Convert [a & b] to [a] and [b]

   [neg_all_rule_asm]: Convert [not (!a: b)] to [?a: not b]
   then convert the new theorem. (Not used)

   [neg_exists_rule_asm]: Convert [not (?a: b)] to [!a: not b]
   then convert the new theorem.

   [single_asm_to_rules l g]: convert an assumption stating a single
   fact to a rewrite-rule.

   [asm_to_rules tg ret g]: Toplevel conversion function.  Convert
   assumption [tg] of goal [g] to one or more rules.  The tag of each
   rule (including [tg]) generated from [tg] is stored in [ret].
   Solves trivial goals involving [false] or [not true] in the
   assumptions.
*)
module Asms :
sig

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
   Convert [a] to [a=true] and delete [true]. Always succeeds.
*)

val rr_equality_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   Accept [l=r] or [c=> l=r].
*)

val eq_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
    Convert [a=b] to [(a=b) = true] and [(b=a)=true]
    and [c=> (a=b)] to [c=>((a=b) = true)] and [c=>((b=a)=true)]
*)

val fact_rule_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   Convert [a] to [a=true]
   and [c=> false] to [(not c)]
   and [c=> a] to [c => a=true]
   pass [(a=b)] and [c=>(a=b)] to [eq_asm]
   and solve [false |- C].
*)

val neg_disj_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   Convert [not (a or b)] to [(not a) and (not b)]
*)
       
val neg_eq_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
    Convert [not (a=b)] to [(a=b) = false] and [(b=a)=false]
    and [c=>not (a=b)] to [c=>((a=b) = false)] and [c=>((b=a)=false)]
*)

val neg_rule_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   Convert 
   and [c=> not true] to [not c]
   and [not a] to [a=false]
   and [c=> not a] to [c=> a=false]
   pass [not (a=b)] and [c=>not(a=b)] to [neg_eq_asm]
   and solve  [not true |- C]
*)

val conj_rule_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   Convert [a & b] to [a] and [b].
*)

val neg_all_rule_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   Convert [not (!a: b)] to [?a: not b] then convert the new
   theorem. (Not used)
*)

val neg_exists_rule_asm :
  Logic.tagged_form list ref 
  -> Tag.t * (Basic.binders list * Basic.term option * Basic.term) 
  -> Tactics.tactic
(**
   Convert [not (?a: b)] to [!a: not b] then convert the new theorem.
*)

val single_asm_to_rule : 
  Logic.tagged_form list ref -> Tag.t -> Tactics.tactic
(**
  [single_asm_to_rules f g]: Convert the assumption [f] stating a single
  fact to a rewrite-rule. Formula [f] must be an assumption of [g].
*)
end

val asm_to_rules : 
  Logic.tagged_form list ref -> Tag.t -> Tactics.tactic
(**
   [asm_to_rules tg ret g]: Toplevel conversion function.  Convert
   assumption [tg] of goal [g] to one or more rules.  Solves trivial
   goals involving [false] or [not true] in the assumptions. The
   (tagged) formula of each rule generated from [tg] is stored in
   [ret]. Note that the assumption identified by [tg] will be one of
   the rules in [ret].
*)

(** {7 Rules from assumptions and conclusions} *)

(** Information about rules created from a sequent formula. **)
type rule_data = 
    { 
      src: Logic.tagged_form; 
      (** The source of the rules. *)
      new_asm: Formula.t;
            (**
      	       The new assumption formed from the source (e.g. when a
      	       conclusion is lifted into the assumptions).
	    *)
      new_rules: (Logic.tagged_form) list 
      	(** The rules formed fro the source. *)
    }

val mk_rule_data:
  Logic.tagged_form -> Formula.t
  -> Logic.tagged_form list -> rule_data
(** Constructor for rule data. *)

val unpack_rule_data: 
  rule_data list -> 
  (Logic.tagged_form list * Formula.t list * Logic.tagged_form list)
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


