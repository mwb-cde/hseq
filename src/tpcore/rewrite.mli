(* term rewriting *)
(* two versions with a single core. 
   The first uses lists of rewrite rules, the second term nets *)

(*
    exception Error of string
*)

(* rewrite rules and databases *)
type rewrite_rules = (Term.binders list * Term.term * Term.term)
type rewriteDB = 
    Net_rr of rewrite_rules Net.net 
  | List_rr of rewrite_rules list

(* try to match a term with the LHS of a rewrite rule *)
(* because lists of rewrites are used, the type contexts is important *)
    val find_match :
      Gtypes.scope -> Gtypes.substitution ->
      (Term.term -> bool) ->
      Term.term -> Term.term -> Term.substitution 
	-> Gtypes.substitution

(* try to rewrite a term using a given rewrite rule
   returns new term and updated type environment (substitution)
*)
    val match_rewrite : 
	Gtypes.scope -> Gtypes.substitution ->
	  (Term.term -> bool) -> Term.term -> 
	    Term.term -> Term.term -> 
	      (Term.term* Gtypes.substitution)

(* utility function to construct a predicate which tests 
   for variables in unification *)
    val is_free_binder : Term.binders list -> Term.term -> bool

(* rewrite using a list of deconstructed rewrite rules *)
    val rewrite_list :Gtypes.scope -> bool ref ->  
      (Term.binders list * Term.term * Term.term) list ->
       Term.term -> Term.term

(* rewrite using a list of partialy deconstructed rewrite rules *)
    val rewrite_eqs :  Gtypes.scope -> bool ->
      (Term.binders list * Term.term)list -> 
	Term.term -> Term.term

(* rewrite using a universally quantified rewrite rule *)
(* dir: true for left to right, false for right to left *)
(* simple: true for toplevel replacment only, 
           false for full rewrite, recursing into term (default) *)
(* simple rewriting: rewrite topmost term once only, 
   recurse through quantifiers if necessary *)

val rewrite_univ : Gtypes.scope ->  ?dir:bool -> ?simple:bool->
    Term.term -> Term.term -> Term.term

(* rewrite using a list of universally quantified rewrite rules *)
    val rewrite_univs : Gtypes.scope -> ?dir:bool -> ?simple:bool ->
      Term.term list -> Term.term ->  Term.term

(* rewrite using a database of rewrite rules *)
val rewrite_net: Gtypes.scope -> rewriteDB -> Term.term -> Term.term
