(* term rewriting *)

(** [type rule] 

   Rewrite rules:

   [Rule t]: simple rewrite rule.
   [Ordered t p]: Rewrite rule with ordering [p].

   Constructors:

   [rule t]: make simple rule from term [t].
   [orule t p]: make ordered rule from term [t] and order [p].


   Destructors:

   [term_of r]: Term of rule [r].
   [rule_of r]: Ordering of rule [r]. raise [Failure] if [r] is not ordered.
*)

type order = (Basic.term -> Basic.term -> bool)

type rule = 
    Rule of Basic.term
  | Ordered of (Basic.term * order)

val rule : Basic.term -> rule
val orule : Basic.term -> order -> rule

val term_of : rule -> Basic.term 
val order_of : rule -> order

(* rewrite control, rules and databases *)
(*
type rewrite_rules = (Basic.binders list * Basic.term * Basic.term)
*)
type rewrite_rules = 
    (Basic.binders list * Basic.term * Basic.term * order option)
type rewriteDB = 
    Net_rr of rewrite_rules Net.net 
  | List_rr of rewrite_rules list


type direction  (* = LeftRight | RightLeft *)
val leftright: direction
val rightleft: direction

type strategy = TopDown | BottomUp 

val topdown : strategy
val bottomup : strategy

type control =
    { 
      depth: int option; (** (Some i): maximum number of times to rewrite is i,
			    None : unlimited rewriting (default) *)
      rr_dir: direction;
      rr_strat: strategy
    }

val control : 
    dir:direction 
  -> strat : strategy 
    -> max:int option
      -> control

val default_control: control

(**
   [limit_reached d]
   [true] iff d=Some 0
 *)
val limit_reached: int option -> bool

(** [match_rewrite]

   Try to rewrite a term using a given rewrite rule
   Returns new term and updated type environment (substitution)
 *)
val match_rewrite : 
    Gtypes.scope -> 
      control -> Gtypes.substitution ->
	(Basic.term -> bool) -> Basic.term -> 
	  Basic.term -> order option -> Basic.term -> 
	    (Basic.term* Gtypes.substitution)

(** [is_free_binder]
   utility function to construct a predicate which tests 
   for variables in unification 
 *)
val is_free_binder : Basic.binders list -> Basic.term -> bool

(** [rewrite_list]
   rewrite using a list of deconstructed rewrite rules 
   return type environment built up during rewriting 
 *)
val rewrite_list : 
    Gtypes.scope -> 
      control -> bool ref 
	-> Gtypes.substitution
	  -> (Basic.binders list 
		* Basic.term * Basic.term * order option) list 
	    ->  Basic.term -> (Basic.term * Gtypes.substitution)

(** [rewrite_eqs]
   rewrite using a list of partialy deconstructed rewrite rules 
   return type environment built up during rewriting 
 *)
val rewrite_eqs : 
    Gtypes.scope -> 
      control
      -> Gtypes.substitution
	-> (Basic.binders list * Basic.term * Basic.term * order option)list 
	  -> Basic.term -> (Basic.term * Gtypes.substitution )

(** [rewrite]
   rewrite using a list of universally quantified rewrite rules.

   [rewrite_env tyenv rules trm]
   rewrite [trm] using [rules] in type environment [tyenv]
   return new term and the new type environment contructed during rewriting.
 *)
val rewrite :
    Gtypes.scope -> 
      control -> rule list -> Basic.term ->  Basic.term

val rewrite_env : 
    Gtypes.scope -> 
      control 
      -> Gtypes.substitution
	-> rule list -> Basic.term 
	  -> (Basic.term * Gtypes.substitution)


val dest_lr_rule: 
    rule 
  -> (Basic.binders list * Basic.term * Basic.term * order option)

val dest_rl_rule: 
    rule 
  -> (Basic.binders list * Basic.term * Basic.term * order option)



(** [rewrite_net]
   rewrite using a database of rewrite rules 
 *)
(*
   val rewrite_net: control -> rewriteDB -> Basic.term -> Basic.term

   val rewrite_net_env: 
   control 
   -> Gtypes.substitution 
   -> rewriteDB 
   -> Basic.term 
   -> (Basic.term * Gtypes.substitution)
 *)


val match_rr_list: 
    Gtypes.scope -> control -> Gtypes.substitution
      -> bool ref 
	-> (Basic.binders list * Basic.term * Basic.term * order option) list 
	  -> Basic.term
	    -> (Basic.term * Gtypes.substitution * control)
		

val match_rewrite_list: 
    Gtypes.scope -> control -> Gtypes.substitution
      -> bool ref 
	-> rewriteDB 
	  -> Basic.term 
	    -> (Basic.term * Gtypes.substitution * control)

val rewrite_list_topdown:
    Gtypes.scope -> control -> Gtypes.substitution
      -> bool ref 
	-> rewriteDB 
	  -> Basic.term 
	    -> (Basic.term * Gtypes.substitution * control)

val rewrite_list_bottomup:
    Gtypes.scope -> control -> Gtypes.substitution
      -> bool ref 
	-> rewriteDB 
	  -> Basic.term 
	    -> (Basic.term * Gtypes.substitution * control)
