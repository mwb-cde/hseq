(*-----
 Name: formula.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(**
   Well formed formulas.

   A term is a well-formed formula if
   - it is type-correct 
   - it is closed: all bound variables occur within their binders
   and there are no free variables.

   Each formula is associated with a theory, identified by a theory
   marker. The formula is type-correct in the scope of its theory.
*)

type t
(** The type of formulas *)

val term_of: t -> Basic.term
(** The term of a formula *)

val thy_of : t -> Scope.marker
(** The theory  of a formula *)


(** {5 Error Reporting} *)

class formError : string -> t list ->
  object
    inherit Report.error 
    val forms: t list
    method get : unit -> t list
  end
val error : string -> t list -> exn
val add_error : string -> t list -> exn -> 'a

(** {5 Conversion from a term} *)

val make_full: 
  ?strict:bool
  -> Scope.t 
  -> Gtypes.substitution 
  -> Basic.term -> (t * Gtypes.substitution)
(**
   [make_full ?(strict=false) scp tyenv trm]: Make a formula from term
   [trm] in scope [scp] w.r.t type environment [tyenv]. The theory of
   the formula is the theory currently in scope. Return the new
   formula and the updated type environment.
   
   {ol 
   {- Replace each free variable [Var(x, _)] in [trm] with the term
   associated with [x] in scope [scp]. }
   {- if [strict=true]: Fail if any bound variable in [trm] occurs
   outside its binding term or any free variable doesn't resolve to an
   identifier. }
   {- if [strict=false]: Replace bound variables occuring outside
   their binder and free variables which don't resolve to an
   identifier with a universally quantified variable.}
   {- Fail if any identifier is not in scope.}
   {- Typecheck resulting term, to set correct types. Passing [tyenv]
   to the typechecker.}
   {- Return resulting formula built from resulting term with the type
   substitution obtained from typechecking.}}
   *)


val make: 
   ?strict:bool
  -> ?tyenv:Gtypes.substitution 
  -> Scope.t 
  -> Basic.term -> t
(**
   [make ?strict ?tyenv scp trm]: Make a formula from term [trm] in scope
   [scp] w.r.t type environment [tyenv] if given. If [tyenv] is not
   given, an empty type environment is used. The theory of the formula
   is the theory currently in scope.
   
   {ol
   {- if [strict=true]: Fail if any bound variable in [trm] occurs
   outside its binding term or any free variable doesn't resolve to an
   identifier. }
   {- if [strict=false]: Replace bound variables occuring outside
   their binder and free variables which don't resolve to an
   identifier with a universally quantified variable.}}
   
   This is a front-end to {!Formula.make_full}.
*)

(** {5 Representation for permanent storage} *)

type saved_form 
(** The representation of formulas for permanent storage. *)

val to_save: t -> saved_form
(** Convert to the saveable representation. *)
val from_save : Scope.t -> saved_form -> t
(** Convert from the saveable representation. *)

(** {5 Operations on formulas} *)

val equals : t -> t -> bool
(** Equality *)

(** {7 General tests} *)

val in_scope:  Scope.t -> t -> bool
(** Check that a formula is in scope. *)

val in_scope_memo: 
    (string, bool) Lib.substype ->
      Scope.t ->  t -> bool
(** Memoised version of [in_scope]. *)

val is_fresh:  Scope.t -> t -> bool
(** 
   [is_fresh scp t]: Check that the theory marker of [t] is still valid.
   true iff [Scope.in_scope_marker scp (thy_of t)] is true.
*)

(** {7 Recognisers} *)

val is_qnt : t -> bool 
val is_app : t -> bool
val is_bound: t -> bool
val is_free : t -> bool
val is_ident : t -> bool
val is_const : t -> bool
val is_fun: t -> bool

val is_true :t-> bool
val is_false : t -> bool
val is_neg: t -> bool
val is_conj: t -> bool
val is_disj: t -> bool
val is_implies: t -> bool
val is_equality: t -> bool

val is_all: t-> bool
val is_exists : t -> bool
val is_lambda: t-> bool

(** 
   {7 Destructors} 

   The theory of a subterm of a formula [f] is the theory of
   [f]. There are no destructors for binding terms since these would
   not result in closed terms. Use {!Term.dest_qnt} to destruct
   binding terms or {!Formula.inst} to form a formula from a binding term.
*)

val dest_num : t -> Num.num
val dest_neg: t -> t
val dest_conj: t -> (t * t)
val dest_disj: t -> (t * t)
val dest_implies: t -> (t * t)
val dest_equality: t -> (t * t)

val get_binder_name : t -> string
val get_binder_type: t -> Basic.gtype

(** {7 Constructors} *)

val mk_true: Scope.t -> t
val mk_false : Scope.t -> t
val mk_bool : Scope.t -> bool -> t
val mk_not: Scope.t -> t -> t
val mk_and: Scope.t -> t -> t -> t
val mk_or: Scope.t -> t -> t -> t
val mk_implies: Scope.t -> t -> t -> t
val mk_iff: Scope.t -> t -> t -> t
val mk_equality: Scope.t -> t -> t -> t


(** {5 Typechecking} *)

val typecheck: Scope.t -> t -> Basic.gtype ->t
(** [typecheck scp f ty]: Check that [f] has type [ty] in scope [scp]. *)

val typecheck_env : Scope.t -> Gtypes.substitution 
  -> t -> Basic.gtype -> Gtypes.substitution
(** 
   [typecheck_env scp tyenv f ty]: Check that [f] has type [ty] in
   scope [scp] w.r.t type context [tyenv]. Returns [tyenv] updated
   with binding made during the typechecking.
*)

val retype: Scope.t -> Gtypes.substitution -> t -> t
(** [retype tyenv f]: Retype [f] with using type context [tyenv]. *)

val typecheck_retype: 
    Scope.t -> Gtypes.substitution 
      -> t -> Basic.gtype
	-> (t * Gtypes.substitution)
(** 
   [typecheck_retype scp tyenv f ty]: Check that [f] is correctly
   typed and has type [ty] w.r.t type context [tyenv].  Retype [f]
   with the updated type context. Return the retyped formula and the
   updated type context.
*)

(** {7 General operations} *)

val subst : Scope.t -> t -> (t*t) list -> t 
(** 
   [subst scp [(t1, r1); ...; (tn, rn)] f]: Simultaneous substitution.
   Substitutes the [ri] for the [ti] in formula [f].
*)

val subst_equiv : Scope.t -> t -> (t*t) list -> t 
(**
   Substition of equivalents under alpha-conversion. [subst scp f
   [(t1, r1); ... ; (tn, rn)]]: Substitute [ri] for terms alpha-equal
   to [ti] in [f]. Slower than {!Formula.subst} but less sensitive to
   binder renaming. Note that this is not syntactic substitution.
*)

val rename: t -> t
(** Rename bound variables *)

val inst_env : Scope.t -> Gtypes.substitution
  -> t -> t -> (t* Gtypes.substitution)
(**
   Instantiation w.r.t a type substitution.
   Instantiate a quantified formula with a given term 
   succeeds only if the result is a formula.
*)

val inst : Scope.t -> t -> t -> t
(**
   Instantiate a quantified formula with a given term
   succeeds only if the result is a formula.
*)

(** {5 Unification functions} *)

val unify: 
    Scope.t -> t -> t -> Term.substitution
(**
   [unify scp asm concl]: Unify [asm] with [concl] in scope
   [scp]. Formula [asm] is normally the assumption of some sub-goal and
   [concl] is the conclusion.
*)

val unify_env: 
    Scope.t 
  -> Gtypes.substitution
    -> t -> t -> (Gtypes.substitution * Term.substitution)
(**
   [unify_env tyenv scp asm concl]: Unify [asm] with [concl] in scope
   [scp] w.r.t type context [tyenv]. Formula [asm] is normally the
   assumption of some sub-goal and [concl] is the conclusion. Returns
   the type context updated with binding made during unification.
*)

(** {5 Logic operations} *)

(** {7 Alpha conversion} *)

val alpha_equals : Scope.t -> t -> t -> bool 
(** Equality under alpha conversion *)

val alpha_equals_match : 
    Scope.t -> Gtypes.substitution 
      -> t -> t -> Gtypes.substitution
(** Equality under alpha-conversion w.r.t a type environment *)

(** {7 Beta conversion} *)

val beta_convp:  t -> bool
(** A formula has the form [((%x. F) a)]. *)

val beta_conv: Scope.t -> t -> t
(** Reduce a formula of the form [((%x. F) a)] to [F[a/x]]. *)

val beta_reduce : Scope.t -> t -> t
(** Reduce all sub-terms of the form [((%x. F) a)] to [F[a/x]]. *)

val mk_beta_reduce_eq : 
  Scope.t -> Gtypes.substitution 
  -> Basic.term -> (t * Gtypes.substitution)
(**
    [mk_beta_reduce_eq scp tyenv scp]: Make an equality expressing the
    result of beta-reducing [trm].
*)

(** {7 Eta conversion} *)

val eta_conv: Scope.t -> t -> t -> t
(** Eta abstract a formula. *)

(** {5 Rewriting} *)

val default_rr_control : Rewrite.control
(** The default rewrite control. *)

val rewrite : 
    Scope.t -> ?dir:Rewrite.direction
      -> t Rewrite.plan
	-> t -> t
(** Rewrite a formula *)

val rewrite_env : 
    Scope.t -> ?dir:Rewrite.direction
      -> Gtypes.substitution 
	-> t Rewrite.plan
	  -> t -> (t * Gtypes.substitution)
(** Rewrite a formula w.r.t a type context. *)

val mk_rewrite_eq : 
    Scope.t
      -> Gtypes.substitution 
	-> t Rewrite.plan
	  -> Basic.term -> (t * Gtypes.substitution)
(** 
   [mk_rewrite_eq scp tyenv plan trm]: Make an equality by rewriting a
   term w.r.t a type context.  Returns [(trm=t, ntyenv)] where [t] is
   the result of rewriting [trm] with [plan] and [ntyenv] is the type
   environment generated during rewriting.
*)

(** {5 Pretty printing} *)

val print : Printer.ppinfo -> t -> unit 
(** Print a formula in a given PP state *)

val string_form : t -> string
