(* term rewriting *)
(* two versions with a single core. 
   The first uses lists of rewrite rules, the second term nets *)


(* rewrite control, rules and databases *)
type rewrite_rules = (Basic.binders list * Basic.term * Basic.term)

type rewriteDB = 
    Net_rr of rewrite_rules Net.net 
  | List_rr of rewrite_rules list

type direction  (* = LeftRight | RightLeft *)
type control =
    { 
      depth: int option; (** (Some i): maximum number of times to rewrite is i,
			   None : unlimited rewriting (default) *)
      dir: direction
    }

val control : int option -> direction -> control

(**
   [limit_reached d]
   [true] iff d=Some 0
*)
val limit_reached: int option -> bool

(** [find_match]
   Try to match a term with the LHS of a rewrite rule
   Because lists of rewrites are used, the type contexts is important 
*)
    val find_match :
      Gtypes.scope -> Gtypes.substitution ->
      (Basic.term -> bool) ->
      Basic.term -> Basic.term -> Term.substitution 
	-> Gtypes.substitution

(** [match_rewrite]

   Try to rewrite a term using a given rewrite rule
   Returns new term and updated type environment (substitution)
*)
    val match_rewrite : 
	Gtypes.scope -> Gtypes.substitution ->
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
    val rewrite_list :Gtypes.scope -> bool ref 
      -> Gtypes.substitution
	  -> (Basic.binders list * Basic.term * Basic.term) list ->
	      Basic.term -> (Basic.term * Gtypes.substitution)

(** [rewrite_eqs]
   rewrite using a list of partialy deconstructed rewrite rules 
   return type environment built up during rewriting 
*)
    val rewrite_eqs :  Gtypes.scope -> bool 
      -> Gtypes.substitution
	  -> (Basic.binders list * Basic.term)list 
	    -> Basic.term -> (Basic.term * Gtypes.substitution )

(** [rewrite_univs]
   rewrite using a list of universally quantified rewrite rules 
*)
    val rewrite_univs : 
	Gtypes.scope -> ?dir:bool -> ?simple:bool ->
	  Basic.term list -> Basic.term ->  Basic.term

    val rewrite_univs_env : 
	Gtypes.scope -> ?dir:bool -> ?simple:bool 
	  -> Gtypes.substitution
	    -> Basic.term list -> Basic.term 
	      -> (Basic.term * Gtypes.substitution)

(** [rewrite_net]
 rewrite using a database of rewrite rules 
*)
val rewrite_net: Gtypes.scope -> rewriteDB -> Basic.term -> Basic.term

val rewrite_net_env: 
    Gtypes.scope 
      -> Gtypes.substitution 
	-> rewriteDB 
	  -> Basic.term 
	    -> (Basic.term * Gtypes.substitution)
