(* Unification of terms *)
(* various flavours  *)

(* [unify scp p l r]
   unify terms l and r in scope scp
   use predicate p to determine what counts as a variable for unification *)

val unify: Gtypes.scope -> (Term.term -> bool) 
  -> Term.term -> Term.term -> Term.substitution

(* unify_env: unify terms in a given context 
   if unification fails, any bindings made are removed *)

val unify_env: Gtypes.scope  -> Term.substitution
  -> (Term.term -> bool) -> Term.term -> Term.term 
    -> Term.substitution 

(* unify_fullenv:  unify terms in a given type and term context.
   if unification fails, any bindings made are removed *) 

val unify_fullenv: 
    Gtypes.scope 
  -> Gtypes.substitution
    -> Term.substitution 
      -> (Term.term -> bool) -> Term.term -> Term.term 
	-> (Gtypes.substitution * Term.substitution)

(*
   unify_fullenv_rewrite:
   A version of unify_fullenv for rewriting.
   [unify_fullenv_rewrite scp tenv env varp trm1 trm2]
   is equivalent to
   [unify_fullenv scp tenv env varp trm1' trm2]
   where [trm1'] is obtained from [trm1] by applying [Gtypes.copy_type]
   to each type in [trm1].
   usage: trm1 is normally the lhs of a rewrite rule which is to be
   applied to trm2. 
   returns both type and term substitutions
 *)

val unify_fullenv_rewrite: 
    Gtypes.scope -> Gtypes.substitution  -> Term.substitution 
    -> (Term.term -> bool) 
      -> Term.term -> Term.term 
	-> (Gtypes.substitution * Term.substitution)

(* simple wrapper for unify_fullenv_rewrite which constructs 
   and empty type substitution *)

val unify_env_rewrite: Gtypes.scope 
  -> Term.substitution -> (Term.term -> bool) 
    -> Term.term -> Term.term -> Term.substitution
