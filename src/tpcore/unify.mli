(*-----
 Name: unify.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* Unification of terms *)
(* various flavours  *)

(* [unify scp p l r]
   unify terms l and r in scope scp
   use predicate p to determine what counts as a variable for unification *)

val unify: ?typenv:Gtypes.substitution -> ?initial:Term.substitution
  -> Scope.t -> (Basic.term -> bool) 
    -> Basic.term -> Basic.term -> Term.substitution

(* unify_env: unify terms in a given context 
   if unification fails, any bindings made are removed *)

val unify_env: 
    ?typenv:Gtypes.substitution
  -> Scope.t  -> Term.substitution
  -> (Basic.term -> bool) -> Basic.term -> Basic.term 
    -> Term.substitution 

(* unify_fullenv:  unify terms in a given type and term context.
   if unification fails, any bindings made are removed *) 

val unify_fullenv: 
    Scope.t 
  -> Gtypes.substitution
    -> Term.substitution 
      -> (Basic.term -> bool) -> Basic.term -> Basic.term 
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
    Scope.t -> Gtypes.substitution  -> Term.substitution 
    -> (Basic.term -> bool) 
      -> Basic.term -> Basic.term 
	-> (Gtypes.substitution * Term.substitution)

(* simple wrapper for unify_fullenv_rewrite which constructs 
   and empty type substitution *)

val unify_env_rewrite: Scope.t 
  -> Term.substitution -> (Basic.term -> bool) 
    -> Basic.term -> Basic.term -> Term.substitution
