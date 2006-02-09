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

type form 
(** The type of formulas *)

val term_of: form -> Basic.term
(** The term of a formula *)

val thy_of : form -> Scope.marker
(** The theory  of a formula *)


(** {5 Error Reporting} *)

class formError : string -> form list ->
  object
    inherit Result.error 
    val forms: form list
    method get : unit -> form list
  end
val error : string -> form list -> exn
val add_error : string -> form list -> exn -> 'a

(** {5 Conversion from a term} *)

val make: ?env:Gtypes.substitution ref -> Scope.t -> Basic.term -> form
(**
   [make ?env scp trm]: Make a formula from term [trm] in scope [scp].
   The theory of the formula is the theory currently in scope.
   
   {ol 
   {- Replace each free variable [Var(x, _)] in [trm] with the term
   associated with [x] in scope [scp]. Fail if [x] is not in scope [scp].}
   {- Fail if any bound variable in [trm] occurs outside its binding term.}
   {- Fail if any identifier is not in scope.}
   {- Typecheck resulting term, to set correct types. If [?env] is
      given, pass it to the typechecker.}
   {- return resulting formula built from resulting term.  If [?env]
      is given, set it to the type substitution obtained from typechecking.}}
*)

(** {5 Representation for permanent storage} *)

type saved_form 
(** The representation of formulas for permanent storage. *)

val to_save: form -> saved_form
(** Convert to the saveable representation. *)
val from_save : Scope.t -> saved_form -> form
(** Convert from the saveable representation. *)

(** {5 Operations on formulas} *)

val equals : form -> form -> bool
(** Equality *)

(** {7 General tests} *)

val in_scope:  Scope.t -> form -> bool
(** Check that a formula is in scope. *)

val in_scope_memo: 
    (string, bool) Lib.substype ->
      Scope.t ->  form -> bool
(** Memoised version of [in_scope]. *)

(** {7 Recognisers} *)

val is_qnt : form -> bool 
val is_app : form -> bool
val is_bound: form -> bool
val is_free : form -> bool
val is_var : form -> bool
val is_typed : form -> bool
val is_const : form -> bool
val is_fun: form -> bool

val is_true :form-> bool
val is_false : form -> bool
val is_neg: form -> bool
val is_conj: form -> bool
val is_disj: form -> bool
val is_implies: form -> bool
val is_equality: form -> bool

val is_all: form-> bool
val is_exists : form -> bool
val is_lambda: form-> bool

(** 
   {7 Destructors} 

   The theory of a subterm of a formula [f] is the theory of
   [f]. There are no destructors for binding terms since these would
   not result in closed terms. Use {!Term.dest_qnt} to destruct
   binding terms or {!Formula.inst} to form a formula from a binding term.
*)

val dest_num : form -> Num.num
val dest_neg: form -> form
val dest_conj: form -> (form * form)
val dest_disj: form -> (form * form)
val dest_implies: form -> (form * form)
val dest_equality: form -> (form * form)

val get_binder_name : form -> string
val get_binder_type: form -> Basic.gtype

(** {7 Constructors} *)

val mk_true: Scope.t -> form
val mk_false : Scope.t -> form
val mk_bool : Scope.t -> bool -> form
val mk_not: Scope.t -> form -> form
val mk_and: Scope.t -> form -> form -> form
val mk_or: Scope.t -> form -> form -> form
val mk_implies: Scope.t -> form -> form -> form
val mk_iff: Scope.t -> form -> form -> form
val mk_equality: Scope.t -> form -> form -> form


(** {7 General operations} *)

val inst_env : Scope.t -> Gtypes.substitution
  -> form -> form -> (form* Gtypes.substitution)
(**
   Instantiation w.r.t a type substitution.
   Instantiate a quantified formula with a given term 
   succeeds only if the result is a formula.
*)

val inst : Scope.t -> form -> form -> form
(**
   Instantiate a quantified formula with a given term
   succeeds only if the result is a formula.
*)

val subst : Scope.t -> form -> (form*form) list -> form 
(** 
   [subst scp [(t1, r1); ...; (tn, rn)] f]: Simultaneous substitution.
   Substitutes the [ri] for the [ti] in formula [f].
*)

val subst_equiv : Scope.t -> form -> (form*form) list -> form 
(**
   Substition of equivalents under alpha-conversion. [subst scp f
   [(t1, r1); ... ; (tn, rn)]]: Substitute [ri] for terms alpha-equal
   to [ti] in [f]. Slower than {!Formula.subst} but less sensitive to
   binder renaming. Note that this is not syntactic substitution.
*)

val rename: form -> form
(** Rename bound variables *)

(** {5 Unification functions} *)

val unify: Scope.t 
  -> form 
    -> form 
      -> Term.substitution
(**
   [unify scp asm concl]: Unify [asm] with [concl] in scope
   [scp]. Formula [asm] is normally the assumption of some sub-goal and
   [concl] is the conclusion.
*)

val unify_env: Scope.t 
  -> Gtypes.substitution
    -> form 
      -> form 
	-> (Gtypes.substitution * Term.substitution)
(**
   [unify_env tyenv scp asm concl]: Unify [asm] with [concl] in scope
   [scp] w.r.t type context [tyenv]. Formula [asm] is normally the
   assumption of some sub-goal and [concl] is the conclusion. Returns
   the type context updated with binding made during unification.
*)

(** {5 Typechecking} *)

val typecheck: Scope.t -> form -> Basic.gtype ->form
(** [typecheck scp f ty]: Check that [f] has type [ty] in scope [scp]. *)

val typecheck_env : Scope.t -> Gtypes.substitution 
  -> form -> Basic.gtype -> Gtypes.substitution
(** 
   [typecheck_env scp tyenv f ty]: Check that [f] has type [ty] in
   scope [scp] w.r.t type context [tyenv]. Returns [tyenv] updated
   with binding made during the typechecking.
*)

val retype: Scope.t -> Gtypes.substitution -> form -> form
(** [retype tyenv f]: Retype [f] with using type context [tyenv]. *)

val typecheck_retype: 
    Scope.t -> Gtypes.substitution 
      -> form -> Basic.gtype
	-> (form * Gtypes.substitution)
(** 
   [typecheck_retype scp tyenv f ty]: Check that [f] is correctly
   typed and has type [ty] w.r.t type context [tyenv].  Retype [f]
   with the updated type context. Return the retyped formula and the
   updated type context.
*)

(** {5 Logic operations} *)

(** {7 Alpha conversion} *)

val alpha_equals : Scope.t -> form -> form -> bool 
(** Equality under alpha conversion *)

val alpha_equals_match : 
    Scope.t -> Gtypes.substitution 
      -> form -> form -> Gtypes.substitution
(** Equality under alpha-conversion w.r.t a type environment *)

(** {7 Beta conversion} *)

val beta_convp:  form -> bool
(** A formula has the form [((%x. F) a)]. *)

val beta_conv: Scope.t -> form -> form
(** Reduce a formula of the form [((%x. F) a)] to [F[a/x]]. *)

val beta_reduce : Scope.t -> form -> form
(** Reduce all sub-terms of the form [((%x. F) a)] to [F[a/x]]. *)

(** {7 Eta conversion} *)

val eta_conv: Scope.t -> form -> Basic.gtype -> form -> form
(** Eta abstract a formula. *)

(** {5 Rewriting} *)

type rule = 
    Rule of form
  | Ordered of (form * Rewrite.order)

val rule : form -> rule
(** Make an unordered rewrite rule. *)
val orule : form -> Rewrite.order -> rule
(** Make a unordered rewrite rule. *)

val rule_to_form : rule -> form
(** Get the formula of a rule. *)

val to_rewrite_rule: rule -> Rewrite.rule
(** Convert to a Rewrite.rule. *)

val default_rr_control : Rewrite.control
(** The default rewrite control. *)

val rewrite : 
    Scope.t -> ?ctrl:Rewrite.control
      -> rule list-> form -> form
(** Rewrite a formula *)

val rewrite_env : 
    Scope.t -> ?ctrl:Rewrite.control
      -> Gtypes.substitution 
	-> rule list-> form -> (form * Gtypes.substitution)
(** Rewrite a formula w.r.t a type context. *)

val plan_rewrite : 
    Scope.t -> ?dir:Rewrite.direction
      -> form Rewrite.Planned.plan
	-> form -> form
(** Rewrite a formula *)

val plan_rewrite_env : 
    Scope.t -> ?dir:Rewrite.direction
      -> Gtypes.substitution 
	-> form Rewrite.Planned.plan
	  -> form -> (form * Gtypes.substitution)
(** Rewrite a formula w.r.t a type context. *)

val mk_rewrite_eq : 
    Scope.t
      -> Gtypes.substitution 
	-> form Rewrite.Planned.plan
	  -> Basic.term -> (form * Gtypes.substitution)
(** Make an equality by rewriting a term w.r.t a type context. *)

(** {5 Pretty printing} *)

val print : Printer.ppinfo -> form -> unit 
(** Print a formula in a given PP state *)

val string_form : form -> string
