(*-----
 Name: unify.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** Unification of terms.

   Various flavours of unification function, to suit general
   unification, unification w.r.t to a context and unification for rewriting.

   The unification functions take a parameter [varp:term->bool] to
   determine what is considered a unification variable (to which an
   assignment can be made by the unifier).

   The result of unification is a substitution, possibly with a type
   substitution, from which the most general unifier can be
   constructed. If terms [x] and [y] are unified and unification
   variables only occur in [x], the most general unifier can be
   obtained using [Term.subst]. If unification variables occur in both
   [x] and [y] then [Term.subst_mgu] must be used.

*)

(** {5 General Unification} *)

val unify_fullenv: 
    Scope.t 
  -> Gtypes.substitution
    -> Term.substitution 
      -> (Basic.term -> bool) -> Basic.term -> Basic.term 
	-> (Gtypes.substitution * Term.substitution)
(** 
   [unify_fullenv scp tyenv env varp t1 t2]: Unify terms [t1] and [t2]
   in scope [scp] w.r.t to given type and term contexts. 

   Type substitution [tyenv] gives type context (pre-determined
   bindings for the type variables in [x] and [y]). Term substitution
   [env] provides the term context (pre-determined bindings for
   unification variables in [x] and [y].

   The result of the function [(tyenv', env')] are type and term
   substitutions. Only term substitution [env'] is needed to construct
   the mgu (which will be type-correct in [tyenv']). Type substitution
   [tyenv'] contains the bindings assigned to type variables to make
   the unification type-correct. It is provided to allow continued
   operations w.r.t the updated type context.
*) 

val unify_env: 
    ?typenv:Gtypes.substitution
  -> Scope.t  -> Term.substitution
  -> (Basic.term -> bool) -> Basic.term -> Basic.term 
    -> Term.substitution 
(**
   Unify terms in a given context.
   
   This is equivalent to calling [unify_fullenv] with an empty type
   context.
*)


val unify: ?typenv:Gtypes.substitution -> ?initial:Term.substitution
  -> Scope.t -> (Basic.term -> bool) 
    -> Basic.term -> Basic.term -> Term.substitution
(**
   [unify scp varp l r]: Unify terms [l] and [r] in scope scp.

   Toplevel for [unify_fullenv] which discards the updated type context.

   If [?typenv] is not given, the empty type substitution is used.
   If [?initial] is not given, the empty term substitution is used.
*)

(** {5 Unification for rewriting} *)

val unify_fullenv_rewrite: 
    Scope.t -> Gtypes.substitution  -> Term.substitution 
    -> (Basic.term -> bool) 
      -> Basic.term -> Basic.term 
	-> (Gtypes.substitution * Term.substitution)
(**
   A version of [unify_fullenv] which renames type variables.

   [unify_fullenv_rewrite scp tyenv env varp trm1 trm2] is
   [unify_fullenv scp tyenv env varp trm1' trm2] where [trm1'] is
   obtained from [trm1] by applying [Gtypes.copy_type] to each type in
   [trm1]. This ensures that the type variables in [trm1] are all
   unique. This means that it is safe to repeatedly unify [trm1] in
   type context constructed from [tyenv] since each instance of
   [trm1] will have its own set of unique type variables.

   Usage: [trm1] is normally the left hand side of a rewrite rule
   which is to be applied to [trm2]. Only used because renaming types
   in [trm1] before unification would take at least two traversals of
   the term (once to rename and once to unify).

   Returns both type and term substitutions
*)

val unify_env_rewrite: Scope.t 
  -> Term.substitution -> (Basic.term -> bool) 
    -> Basic.term -> Basic.term -> Term.substitution
(**
   Top-level unification for rewriting.
   Calls [unify_fullenv_rewrite] with an empty type substitution.
*)
