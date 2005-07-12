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
      control -> Gtypes.substitution ->
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
    Scope.t -> control -> Gtypes.substitution
      -> bool ref 
	-> (Basic.binders list * Basic.term * Basic.term * order option) list 
	  -> Basic.term
	    -> (Basic.term * Gtypes.substitution * control)
		

val match_rewrite_list: 
    Scope.t -> control -> Gtypes.substitution
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
    Scope.t -> control -> Gtypes.substitution
      -> bool ref 
	-> rewriteDB 
	  -> Basic.term 
	    -> (Basic.term * Gtypes.substitution * control)
