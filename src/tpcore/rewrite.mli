(*-----
Name: rewrite.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(** Term rewriting *)


(** {5 Rewrite Rules} *)

(**
   Rewrite rules are presented to the rewriter as terms of the form
   [x=y]. Rules can be ordered by a given predicate [order]. Ordered
   rewrite rules are only applied to a term [t] if [order x t] is
   true, where [order x t] is interpreted as [x] is less-than [t].
 *)

type order = (Basic.term -> Basic.term -> bool)
(** 
   Ordering predicates for rewrite rules. [order x y] is interpreted
   as true if [x] is less-than [y].
 *)

(** Rewrite rules.

   [Rule t]: An unordered rewrite rule.

   [Order t p]: A rewrite rule ordered by [p]. 
 *)
type rule = 
    Rule of Basic.term 
  | Ordered of (Basic.term * order)


val rule : Basic.term -> rule
(** Make an unordered rewrite rule. *)
val orule : Basic.term -> order -> rule
(** Make a unordered rewrite rule. *)

val term_of : rule -> Basic.term 
(** Destructor for rewrite rules: get the term of the rule. *)
val order_of : rule -> order
(** 
   Destructor for ordered rewrite rules: get the ordering of the rule. 
   raise [Failure] if rule is not ordered.
 *)

(** {5 Controlling rewriting} *)

(**
   The direction of rewriting (left to right or right to left) and
   whether rewriting is top-down or bottom-up are both controlled by
   options to the rewriter. 

   It is also possible to limit the number of times the rewriter makes
   changes. This is mainly used to ensure that the rewriter always
   terminates. It is also used to apply the rewriter once, usually to
   ensure that only one occurence of term is rewritten.

   The options apply to the rewriter and therefore affect all rewrite
   rules.
 *)

type direction
(** The direction of rewriting. *)
val leftright: direction
(** Rewrite from left to right (this is the usual direction) *)
val rightleft: direction
(** Rewrite from right to left (this reverses rewrite rules) *)

type strategy = TopDown | BottomUp 
(** The rewriting strategy *)
val topdown : strategy
(** 
   Rewrite from the top-most term down (the default). This is fast but
   less through than bottom-up.
 *)
val bottomup : strategy
(**
   Rewrite from lowest-terms up. This is thorough but can be
   inefficient since the result of rewriting at one level can be
   discarded by rewriting at a higher level.
 *)

(** How the options are passed to the rewriter. *)
type control =
    { 
      depth: int option; 
      rr_dir: direction;
      rr_strat: strategy
    }

val control : 
    dir:direction 
  -> strat : strategy 
    -> max:int option
      -> control
(** Make a rewrite control. 

   [strat] is the strategy to use.

   [dir] is the direction of rewriting.

   [max] is the maximum number of times to rewrite in the term.
 *)

val default_control: control
(**
   The default rewrite control. 

   [default_control= control ~strat:TopDown ~dir:leftright ~max:None]
 *)


(** {5 Rewriting functions} *)

(**
   Two rewriting functions are provided. The first is for general
   rewriting. The second for rewriting w.r.t a type context.

   Both take rewrite rules as universally quantified equalities of the
   for [!v1 .. vn. lhs = rhs]. The variables [v1 .. vn] are taken as
   variables which can be instantiated by the rewriter. If the rule is
   not an equality then rewriting will fail.

   Rewriting breaks a rule [lhs=rhs] to an equality. If rewriting is
   left-right, the equality is [lhs=rhs]; if rewriting is right-left
   then the equality is [rhs=lhs]. 

   For left-right rewriting, every variable (from [v1 .. vn])
   appearing in [rhs] must also appear in [lhs] otherwise the rule
   cannot be used. Similarly for right-left rewriting.
 *)

val rewrite :
    Scope.t -> 
      control -> rule list -> Basic.term ->  Basic.term
(** 
   Rewrite using a list of universally quantified rewrite rules.
 *)
	  
val rewrite_env : 
    Scope.t -> 
      control 
      -> Gtypes.substitution
	-> rule list -> Basic.term 
	  -> (Basic.term * Gtypes.substitution)
(**
   [rewrite_env tyenv rules trm]
   Rewrite [trm] w.r.t type environment [tyenv] using [rules]
   Return the new term and the type environment contructed during rewriting.
 *)


(** {5 Utility functions} *)

val is_free_binder : Basic.binders list -> Basic.term -> bool
(** 
   Utility function to construct a predicate which tests for variables
   in unification. [is_free_binder qs t] is true if [t] is a bound
   variable [Bound b] and [b] occurs in [qs].
 *)

type rewrite_rules = 
    (Basic.binders list * Basic.term * Basic.term * order option)
(** 
   The representation used by the rewriting engine. An rule of the
   form [! v1 .. vn. lhs=rhs] for left-right rewriting is broken to
   [([v1; .. ; vn], lhs, rhs, opt_order)]. If the rule is unordered,
   then [opt_order] is [None], if the rule is ordered by [p] then
   [opt_order] is [Some p]. For right-left rewriting, the rule is
   broken to [([v1; ..; vn], rhs, lhs, opt_order)].
 *)

val dest_lr_rule: 
    rule 
  -> (Basic.binders list * Basic.term * Basic.term * order option)
(**
   Rule destructor for left to right rewriting.
   Break rule [t= !x1..xn: lhs = rhs] 
   into quantifiers [x1..xn], [lhs] and [rhs].

   Return ([x1..xn], [lhs], [rhs], [p])
   where [p] is [None] if [r=Rule t] and [Some x if r= Order(t, x)]

   Raises [term_error] if the rule is not an equality.
 *)

val dest_rl_rule: 
    rule 
  -> (Basic.binders list * Basic.term * Basic.term * order option)
(**
   Rule destructor for right to left rewriting.
   Break rule [t= !x1..xn: lhs = rhs] 
   into quantifiers [x1..xn], [rhs] and [lhs].

   Return ([x1..xn], [lhs], [rhs], [p])
   where [p] is [None] if [r=Rule t] and [Some x if r= Order(t, x)]

   Raises [term_error] if the rule is not an equality.
 *)

val limit_reached: int option -> bool
(**
   [limit_reached d] is [true] iff [d=Some 0]
 *)

(** {7 Debugging information} *)


type rewriteDB = 
    Net_rr of rewrite_rules Net.net 
  | List_rr of rewrite_rules list


val match_rewrite : 
    Scope.t -> 
      control -> 
	Term.substitution  -> 
	  Gtypes.substitution ->
	    (Basic.term -> bool) -> Basic.term -> 
	      Basic.term -> order option -> Basic.term -> 
		(Basic.term* Gtypes.substitution)


val rewrite_list : 
    Scope.t -> 
      control -> bool ref 
	-> Gtypes.substitution
	  -> (Basic.binders list 
		* Basic.term * Basic.term * order option) list 
	    ->  Basic.term -> (Basic.term * Gtypes.substitution)


val match_rr_list: 
    Scope.t -> control 
      -> Term.substitution  
	-> Gtypes.substitution
	  -> bool ref 
	    -> (Basic.binders list * Basic.term * Basic.term * order option) list 
	      -> Basic.term
		-> (Basic.term * Gtypes.substitution * control)
		    

val match_rewrite_list: 
    Scope.t -> control 
      -> Term.substitution  
	-> Gtypes.substitution
	  -> bool ref 
	    -> rewriteDB 
	      -> Basic.term 
		-> (Basic.term * Gtypes.substitution * control)

val rewrite_list_topdown:
    Scope.t -> control -> Gtypes.substitution
      -> bool ref 
	-> rewriteDB 
	  -> Basic.term 
	    -> (Basic.term * Gtypes.substitution * control)

val rewrite_list_bottomup:
    Scope.t -> control 
      -> Gtypes.substitution
	-> bool ref 
	  -> rewriteDB 
	    -> Basic.term 
	      -> (Basic.term * Gtypes.substitution * control)


(** {5 Directed Rewriting} *)

exception Quit of exn
exception Stop of exn

type orig_rule = rule

module Planned :
    sig

      type rewrite_rule = (Basic.binders list * Basic.term * Basic.term)

(**
   Planned Rewriting

   Rewriting based on a pre-determined plan.

   Rewrites a node [n] by following the direction in a plan [p]. Plan
   [p] specifies the rules to be applied to each node and the order in
   which a node is to be rewritten.

   (For hseq, a node is a term).
 *)


(** {7 Planned rewriting specialised to terms} *)

      type term_key =
	  Ident  (** Term [Id] *)
	| BVar   (** Term [Bound] *)
	| FVar   (** Term [Free] *)
	| Appln  (** Term [App] *)
	| Quant  (** Term [Qnt] *)
	| AllQ   (** Term [Qnt] ([All]) *)
	| ExQ   (** Term [Qnt] ([Ex]) *)
	| LamQ   (** Term [Qnt] ([Lambda]) *)
	| Constn (** Term [Const] *)
	| TyTerm (** Term [Typed] *)
	| AnyTerm    (** Any term *)
	| NoTerm   (** No term *)
	| Neg of term_key (** Negate a key *)
	| Alt of term_key * term_key  (** Alternative keys *)

      module TermData : (Rewritekit.Data with 
      type data = 
	  (Scope.t (** Scope *)
	     * Term.substitution    (** Quantifier environment *)
	     * Gtypes.substitution)  (** Type environment *)
(*
	     * Term.substitution    (** Substitution *) )
*)
      and type rule = rewrite_rule
      and type node = Basic.term
      and type substn = Term.substitution
      and type key = term_key)

      type data = TermData.data
      type key = TermData.key
      type rule = rewrite_rule

      module TermPlan: 
	  (Rewritekit.Kit 
      with type data = 
	  (Scope.t (** Scope *)
	     * Term.substitution    (** Quantifier environment *)
	     * Gtypes.substitution)  (** Type environment *)
(*
	     * Term.substitution    (** Substitution *) )
*)
      and type node = TermData.node
      and type substn = Term.substitution
      and type rule = rewrite_rule
      and type key = term_key)


(** {5 Toplevel directed rewriting functions} *)

      type ('a)plan = (key, 'a)Rewritekit.plan

      val rewrite :
	  data -> (rule)plan
	    -> Basic.term -> (data * Basic.term)

		(** [rewrite data p t]: Rewrite term [t] with plan [t]. *)

		(** {7 Plan constructors} *)
      val mk_node: key -> ('a)plan list -> ('a)plan
      val mk_rules : 'a list -> ('a)plan
      val mk_branch: int -> ('a)plan -> ('a)plan
      val mk_branches: ('a)plan list -> ('a)plan
      val mk_skip: ('a)plan

      val mapping: ('a -> 'b) -> ('a)plan -> ('b)plan
	  (** [mapping f p]: Map function [f] to each rule in [p]. *)

      val pack: ('a)plan -> ('a)plan
	  (** [pack p]: Pack plan [p], removing redundant directions. *)

      val pack: ('a)plan -> ('a)plan
      val pack_rules: ('a)list -> ('a)plan
      val pack_node: key -> ('a)plan list -> ('a)plan
      val pack_branches: ('a)plan list -> ('a)plan
      val pack_branch : int -> 'a plan -> 'a plan

  	  (** {7 Keys} *)
      val key_of : Basic.term -> key
      val anyterm : key
      val noterm : key
      val alt_key : key -> key -> key
      val neg_key : key -> key
      val ident_key : key
      val fvar_key : key
      val bvar_key : key
      val appln_key : key
      val quant_key: key
      val allq_key : key
      val exq_key : key
      val lamq_key : key
      val constn_key : key
      val tyterm_key : key

    end

val plan_rewrite : 
    Scope.t -> ?dir:direction
      -> Basic.term Planned.plan
	-> Basic.term -> Basic.term
(** Rewrite a formula *)

val plan_rewrite_env : 
    Scope.t -> ?dir:direction
      -> Gtypes.substitution 
	-> Basic.term Planned.plan
	  -> Basic.term -> (Basic.term * Gtypes.substitution)
(** Rewrite a formula w.r.t a type context. *)


module type PlannerData =
  sig
    type rule
    type data 
    val dest : 
	data -> rule 
      -> (Basic.binders list * Basic.term * Basic.term * order option)
  end


module type PlannerType =
  sig
(** Rewrite plan constructors.

   Two planning functions are provided. The first is for general
   rewriting. The second for rewriting w.r.t a type context.

   Both take rewrite rules as universally quantified equalities of the
   for [!v1 .. vn. lhs = rhs]. The variables [v1 .. vn] are taken as
   variables which can be instantiated by the rewriter. If the rule is
   not an equality then rewriting will fail.

   Rewriting breaks a rule [lhs=rhs] to an equality. If rewriting is
   left-right, the equality is [lhs=rhs]; if rewriting is right-left
   then the equality is [rhs=lhs]. 

   For left-right rewriting, every variable (from [v1 .. vn])
   appearing in [rhs] must also appear in [lhs] otherwise the rule
   cannot be used. Similarly for right-left rewriting.
 *)
    open Planned

    exception No_change

    type a_rule 
    type rule_data

    val make :
	rule_data -> 
	  Scope.t -> control -> a_rule list 
	    -> Basic.term -> (Basic.term * (a_rule)plan)
(** 
   Make a rewrite plan using a list of universally quantified rewrite
   rules.
 *)
	      
    val make_env : 
       rule_data 
	-> Scope.t 
	  -> control 
	  -> Gtypes.substitution
	    -> a_rule list -> Basic.term 
	      -> (Basic.term * Gtypes.substitution * (a_rule)plan)
(**
   [make_env tyenv rules trm]: Make a rewrite plan for [trm] w.r.t type
   environment [tyenv] using [rules]. Return the new term and the type
   environment contructed during rewriting.
 *)

(** {7 Exposed for debugging} *)

    type data = 
	(Scope.t  (** Scope *)
	   * Term.substitution   (** Quantifier environment *)
	   * Gtypes.substitution) (** Type environment *)

    type internal_rule = 
	(Basic.binders list 
	   * Basic.term 
	   * Basic.term 
	   * order option
	   * a_rule)

    type rewrite_net = internal_rule Net.net 

    val src_of: internal_rule -> a_rule

    val match_rewrite : 
	control
      -> data
	-> internal_rule
	  -> Basic.term 
	    -> (a_rule * Basic.term * Gtypes.substitution)

    val match_rr_list:
	control
      -> data
	-> internal_rule list
	  -> Basic.term
	    -> a_rule list
	      -> (Basic.term * Gtypes.substitution
		    * control * (a_rule)list)

    val match_rewrite_list:
	control
      -> data
	-> rewrite_net
	  -> Basic.term 
	    -> a_rule list
	      -> (Basic.term * Gtypes.substitution
		    * control * (a_rule)list)

    val check_change : ('a)plan -> unit
    val check_change2 : ('a)plan -> ('a)plan -> unit

    val make_list_topdown:
	control 
      -> rewrite_net
	-> data
	  -> Basic.term
	    -> (Basic.term * Gtypes.substitution 
		  * control * (a_rule)plan)


    val make_list_bottomup:
	control 
      -> rewrite_net
	-> data
	  -> Basic.term
	    -> (Basic.term * Gtypes.substitution 
		  * control * (a_rule)plan)

    val make_rewrites: 
	rule_data -> a_rule list -> rewrite_net

    val make_list : 
	rule_data
	-> control 
	  -> Scope.t
	    -> Gtypes.substitution
	      -> a_rule list
		-> Basic.term
		  -> (Basic.term * Gtypes.substitution * (a_rule)plan)

  end

module Planner : 
functor (A: PlannerData) -> 
  (PlannerType with type a_rule = A.rule
  and type rule_data = A.data)

module TermPlannerData :  
    (PlannerData with type rule = Basic.term 
    and type data = unit)

module TermPlanner : 
    (PlannerType with type a_rule = TermPlannerData.rule 
    and type rule_data = unit)
