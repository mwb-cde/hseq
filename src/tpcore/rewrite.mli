(* term rewriting *)
(* two versions with a single core. 
   The first uses lists of rewrite rules, the second term nets *)


(* rewrite control, rules and databases *)
type rewrite_rules = (Basic.binders list * Basic.term * Basic.term)

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
(*      scope: Gtypes.scope; *)
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
	Basic.term -> Basic.term -> 
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
    -> (Basic.binders list * Basic.term * Basic.term) list ->
      Basic.term -> (Basic.term * Gtypes.substitution)

(** [rewrite_eqs]
   rewrite using a list of partialy deconstructed rewrite rules 
   return type environment built up during rewriting 
 *)
val rewrite_eqs : 
    Gtypes.scope -> 
    control
  -> Gtypes.substitution
    -> (Basic.binders list * Basic.term)list 
      -> Basic.term -> (Basic.term * Gtypes.substitution )

(** [rewrite]
   rewrite using a list of universally quantified rewrite rules.

   [rewrite_env tyenv rules trm]
   rewrite [trm] using [rules] in type environment [tyenv]
   return new term and the new type environment contructed during rewriting.
 *)
val rewrite :
    Gtypes.scope -> 
    control -> Basic.term list -> Basic.term ->  Basic.term

val rewrite_env : 
    Gtypes.scope -> 
    control 
  -> Gtypes.substitution
    -> Basic.term list -> Basic.term 
      -> (Basic.term * Gtypes.substitution)


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
