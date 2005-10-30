(*-----
 Name: tactics.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** Tactics and Tacticals *)

type tactic = Logic.tactic
(** A tactic is a function of type [Logic.node -> Logic.branch] *)

(** {5 Support functions} *)

(** {7 Error reporting} *)

val error: string -> exn
(** [error s]: Make a Result.Error exception with message [s]. *)

val add_error : string -> exn -> exn
(** [add_error s err]: Add [error s] to exception [err]. *)

(** 
   {7 Accessing elements of a list} 

   Simpler versions of {!Lib.get_one} and {!Lib.get_two}. Both raise
   exception [Failure msg] on failure, with [msg] an optional
   argument.
*)

val get_one: ?msg:string -> 'a list -> 'a
(** Get the first element of a list. *)

val get_two: ?msg:string -> 'a list -> ('a * 'a)
(** Get the first two elements of a list. *)

(** {7 Formulas} *)

val drop_tag : Logic.tagged_form -> Formula.form
(** Get the formula of a tagged formula. *)

val drop_formula: Logic.tagged_form -> Tag.t
(** Get the tag of a tagged formula. *)

(** {7 Formula labels} *)

val fnum: int -> Logic.label
(** Make a label from an integer. *)

val ftag: Tag.t -> Logic.label
(** Make a label from a tag. *)

val fname: string -> Logic.label
(** Make a label from a string. *)

val (!!): int -> Logic.label
(** Formula index to label. [!! x] is [fnum x]. *)

val (!~): int -> Logic.label
(**  Assumption index to label.  [!~ x] is [fnum (-x)]. *)

val (!$): string -> Logic.label
(** Formula name to label. [!$ x] is [fname x]. *)

(** {7 Sequents} *)

val asms_of : Logic.Sequent.t -> Logic.tagged_form list
(** Get the assumptions of a sequent. *)
val concls_of : Logic.Sequent.t -> Logic.tagged_form list
(** Get the conclusions of a sequent. *)
val sqnt_tag : Logic.Sequent.t -> Tag.t
(** Get the tag of a sequent. *)

(** {7 Nodes} *)

val sequent : Logic.node -> Logic.Sequent.t
(** Get sequent of a node. *)
val scope_of : Logic.node -> Scope.t
(** Get the scope of a node. *)
val typenv_of : Logic.node -> Gtypes.substitution
(** Get the type environment of a node. *)
val node_tag: Logic.node -> Tag.t
(** Get the tag of a node. *)

val get_tagged_asm: Logic.label -> Logic.node -> Logic.tagged_form
(** Get an assumption and its tag by label.*)
val get_tagged_concl: Logic.label -> Logic.node -> Logic.tagged_form
(** Get a conclusion and its tag by label.*)

val get_asm: Logic.label -> Logic.node -> Formula.form
(** Get an assumption by label.*)
val get_concl: Logic.label -> Logic.node -> Formula.form
(** Get a conclusion by label.*)

(** {7 Branches} *)

val branch_tyenv: Logic.branch -> Gtypes.substitution
(** Type environment of a branch. *)

val branch_subgoals: Logic.branch -> Logic.Sequent.t list
(** Subgoals of a branch. *)

val has_subgoals : Logic.branch -> bool
(** Test whether a branch has subgoals. *)

val num_subgoals : Logic.branch -> int
(** Get the number of subgoals in a branch. *)

(** {7 Information records} *)

val mk_info: unit -> Logic.info
(** Make an empty information record. *)

val empty_info: Logic.info -> unit
(** 
   [empty_info info]: Empty the information record [info].
   Equivalent to [info:=mk_info()].
 *)

val subgoals: Logic.info -> Tag.t list
(** 
   [subgoals info]: Get subgoals of [info].
   Equivalent to [(!info).goals]
 *)

val aformulas: Logic.info -> Tag.t list
(** 
   [aformulas info]: Get tags of assumption formulas from [info].
   Equivalent to [(!info).aforms]
*)

val cformulas: Logic.info -> Tag.t list
(** 
   [cformulas info]: Get tags of conclusion formulas from [info].
   Equivalent to [(!info).cforms]
*)

val constants: Logic.info -> Basic.term list
(** 
   [constants info]: Get constants from [info].  Equivalent to
   [(!info).terms]
 *)

val set_info : 
    Logic.info option -> 
      (Tag.t list * Tag.t list * Tag.t list * Basic.term list)
      -> unit
(** 
   A version of {!Logic.add_info}, packaged for use, in tactics, with
   {!Tactics.data_tac}.
*)

(** {7 Utility functions} *)

val extract_consts: 
    Basic.binders list -> Term.substitution -> Basic.term list
(** 
   [extract_consts qs sb]: [extract_consts qs sb] extracts the
   bindings for each of the binders in [qs] from substitution [sb],
   returning the terms in the same order as the binders. [qs] is
   typically obtained by stripping the binders from a formula. [sb] is
   typically constructed by unification.
 *)

val qnt_opt_of: 
    Basic.quant_ty -> (Basic.term -> bool) -> Basic.term -> bool
(**
   [qnt_opt_of k pred t]: Apply predicate [pred] to the body of
   possibly quantified term [t]. The outermost quantifiers of kind [k]
   are stripped off before [pred] is applied.  Returns [pred body]
   where [(_, body)=strip_qnt k t].
*)

val first_asm : 
    (Logic.tagged_form -> bool) -> Logic.Sequent.t
      -> Logic.tagged_form
(** 
   Get the first assumption in a sequent which satisfies a predicate. 
   Raise Not_found if no such assumption.
*)

val first_concl : 
    (Logic.tagged_form -> bool) -> Logic.Sequent.t
      -> Logic.tagged_form
(** 
   Get the first conclusion in a sequent which satisfies a predicate. 
   Raise Not_found if no such assumption.
*)

val first_form : 
    (Logic.tagged_form -> bool) -> Logic.Sequent.t
      -> Logic.tagged_form
(** 
   Get the first formula in a sequent which satisfies a
   predicate. Raise [Not_found] if no such assumption. Searches the
   assumptions then the conclusions of the sequent.
*)

val first_asm_label : 
    Logic.label option -> (Formula.form -> bool) 
      -> Logic.node -> Logic.label
(** 
   [first_asm_label ?c pred sq]: If a is [Some(x)] then return [x].
   Otherwise, get the label of the first assumption whose formula
   satisifies [pred]. Raise Not_found if no such assumption.

   Mostly used to unpack the argument to a tactic, where [a] is the
   optional label identifying a formula and [pred] is used if [a] is
   not given.
*)

val first_concl_label : 
    Logic.label option -> (Formula.form -> bool) 
      -> Logic.node -> Logic.label
(** 
   [first_concl_label ?c pred sq]: If c is [Some(x)] then return [x].
   Otherwise, get the label of the first conclusion whose formula
   satisifies [pred]. Raise Not_found if no such conclusion.

   Mostly used to unpack the argument to a tactic, where [c] is the
   optional label identifying a formula and [pred] is used if [c] is
   not given.
*)

(** 
   {5 Basic tacticals and tactics} 

   Primitive tactics and tacticals needed by the tacticals.
*)

val foreach: tactic -> Logic.branch -> Logic.branch
(** [foreach tac br]: Apply [tac] to each subgoal of branch [br]. *)

val skip : tactic
(** The tactic that does nothing. Alway succeeds. *)

val fail : ?err:exn -> tactic
(** The tactic that always fails. Raises [Failure] or [?err] if given. *)

val data_tac: ('a -> unit) -> 'a -> tactic
(** 
   Evaluate an expression. [data_tac f data g] evaluates [(f data)]
   then behaves like {!Tactics.skip}.
*)

(** {5 Tacticals} *)

val seq : tactic list -> tactic 
(**
   [seq tacl]: Apply each tactic in [tacl] in sequence to the subgoals
   resulting from the previous tactic. Fails if [tacl] is empty.
*)

val (++): tactic -> tactic -> tactic
(**
   [tac1 ++ tac2]: Apply [tac1] then, if there are subgoals, apply [tac2]
   to the subgoals. [tac1 ++ tac2] is [seq [tac1; tac2]].
*)

val alt:  tactic list -> tactic 
(**
   [alt tacl]: Apply each tactic in [tacl], in sequence, until one succeeds.
   Fails if no tactic succeeds. 
*)

val (//) : tactic -> tactic -> tactic
(**
   [tac1 // tac2]: Apply [tac1], if that fails, apply [tac2].
   [tac1 // tac2] is [alt [tac1; tac2]].
*)

val thenl : tactic ->  tactic list -> tactic 
(**
   [thenl tac tacl]: Apply tactic [tac] then pair each of the tactics
   in [tacl] with a resulting subgoal. If [tag n] results in subgoals
   [g1, g2, .. gn], the tactics are matched up to produce [tac1 g1,
   tac2 g2, .. , tacn gn]. Excess tactics in [tacl] are silently
   discarded. Excess subgoals are appended to the result of the tactics.
*)

val (--) : tactic ->  tactic list -> tactic 
(**
   [tac -- tacl]: Synonym for [thenl tac tacl].
*)

val repeat : tactic -> tactic
(**
   [repeat tac]: Apply [tac] at least once then repeat until it fails
   or there are no more subgoals.
*)

val cond : (Logic.node -> bool) -> tactic -> tactic -> tactic
(**
   [cond pred ttac ftac g]:  Conditional application.
   If [pred g] is true then [ttac g] else [ftac g].
*)

val (-->) : (Logic.node -> bool) -> tactic -> tactic 
(** 
   One-armed conditional.  [pred --> tac] is [cond pred tac skip],
   applying [tac] if condition [pred] is met.
*)

val restrict : (Logic.branch -> bool) -> tactic -> tactic
(**
   [restrict pred tac g]:  Restrict the result of applying a tactic.
   Fails if [pred (tac g)] is false otherwise behaves as [(tac g)].
*)

val notify_tac : ('a -> unit) -> 'a -> tactic -> tactic
(**
   [notify_tac f x tac g]: Notify [tac g] succeeded.
   Applies [tac g] then, if the tactic suceeded, apply [f x].
   Fails if [tac g] fails.
*)

val map_every: ('a -> tactic) -> 'a list -> tactic
(**
   [map_every tac xs]: Sequentially apply the tactics formed by [(tac
   x)], for each [x] in [xs].  [map_every tac [y1; y2; .. ; yn]] is
   [(tac y1) ++ (tac y2) ++ .. ++ (tac yn)].

   Fails if function [(tac x)] fails for any [x] in [xs] or if any of
   the tactics [(tac x)] fail. Does nothing if [xs] is initially empty.
*)

val map_first: ('a -> tactic) -> 'a list -> tactic
(**
   [map_first tac xs]: Apply the first tactic formed by [(tac
   x)], for each [x] in [xs].  [map_every tac [y1; y2; .. ; yn]] is
   [(tac y1) ++ (tac y2) ++ .. ++ (tac yn)].

   Fails if function [(tac x)] fails for any [x] in [xs] or if all of the
   resulting tactics fail. Fails if [xs] is initially empty.
*)

val map_some: ('a -> tactic) -> 'a list -> tactic
(**
   [map_some tac xs]: Sequentially apply the tactics formed by [(tac
   x)], for each [x] in [xs], allowing some tactics to fail.

   Fails if function [(tac x)] fails for any [x] in [xs] or if all of
   the tactics [(tac x)] fail. Fails if [xs] is initially empty.
*)

val foreach_asm: (Logic.label -> tactic) -> tactic
(**
   [foreach_asm tac goal]: Sequentially apply [tac l] to each
   assumption in [goal], beginning with the first assmuption, where [l]
   is the label of each assumption considered.

   Fails if no instance of [tac l] succeeds.
*)

val foreach_concl: (Logic.label -> tactic) -> tactic
(**
   [foreach_concl tac goal]: Sequentially apply [tac l] to each
   conclusion in [goal], beginning with the first assmuption, where [l]
   is the label of each assumption considered.

   Fails if no instance of [tac l] succeeds.
*)

val foreach_form: (Logic.label -> tactic) -> tactic
(**
   Apply {!Tactics.foreach_asm} then {!Tactics.foreach_concl}, failing
   if both fail.
*)

(** 
   {5 Tactics}

   The tactics in this module which abstract from those defined in
   {!Logic.Tactics} should be prefered to those in {!Logic.Tactics}.
   Where tactics take an argument [?info] and the tag details aren't
   given, they can be found in the equivalent tactic in
   {!Logic.Tactics}.
*)

val rotateA : ?info:Logic.info -> tactic
(** Rotate the assumptions. *)

val rotateC : ?info:Logic.info -> tactic
(** Rotate the conclusions. *)

val copyA : ?info:Logic.info -> Logic.label -> tactic
(** Copy an assumption.*)

val copyC : ?info:Logic.info -> Logic.label -> tactic
(** Copy a conclusion. *)

val lift : ?info:Logic.info -> Logic.label -> tactic
(** 
   Move a formula to the top of the list of assumptions/conclusions.
*)

val delete: ?info:Logic.info -> Logic.label -> tactic 
(** [delete l]: Delete the formula labelled  [l]. *)

val deleten: Logic.label list -> Logic.tactic
(**  [deleten ls]: Delete the formulas identified by a label in [ls]. *)

(** {7 Logic rules}

   Apply the basic rules to the first assumption/conclusion
   which will succeed 

   If assumption [a], conclusion [c] or formula [f] is not
   found, these will search for a suitable assumption/conclusion.
   (Parameter [f] is used for formulas which can be either 
   in the assumptions or the conclusions).

   Tag information provided by the rules is as in {!Logic.Tactics}.
*)

val trueC : ?info:Logic.info -> ?c:Logic.label -> tactic
(** Entry point to {!Logic.Tactics.trueC}. *)
val conjC : ?info:Logic.info -> ?c: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.conjC}. *)
val conjA : ?info:Logic.info -> ?a: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.conjA}. *)
val disjC : ?info:Logic.info -> ?c: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.disjC}. *)
val disjA : ?info:Logic.info -> ?a: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.disjA}. *)
val negC : ?info:Logic.info -> ?c: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.negC}. *)
val negA : ?info:Logic.info -> ?a: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.negA}. *)
val implC : ?info:Logic.info -> ?c: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.implC}. *)
val implA : ?info:Logic.info -> ?a: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.implA}. *)
val existC : ?info:Logic.info -> ?c: Logic.label -> Basic.term -> tactic 
(** Entry point to {!Logic.Tactics.existC}. *)
val existA : ?info:Logic.info -> ?a: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.existA}. *)
val allC : ?info:Logic.info -> ?c: Logic.label -> tactic
(** Entry point to {!Logic.Tactics.allC}. *)
val allA : ?info:Logic.info -> ?a: Logic.label -> Basic.term -> tactic
(** Entry point to {!Logic.Tactics.allA}. *)

val instA: ?info:Logic.info
    -> ?a:Logic.label -> Basic.term list -> tactic
(**
   Instantiate a universally quantified assumption. Generalises
   [allA] to a list of terms. [instA a trms] applies [allA a t] for
   each [t] in [trms]. [?info] is set to the result of the last
   instantiation. Fails if there are more terms then variables.
*)

val instC: ?info:Logic.info
    -> ?c:Logic.label -> Basic.term list -> tactic
(**
   Instantiate an existentially quantified conclusion. Generalises
   [existC] to a list of terms. [instc a trms] applies [existC a t]
   for each [t] in [trms]. [?info] is set to the result of the last
   instantiation. Fails if there are more terms then variables.
*)

val inst_tac: ?info:Logic.info
    -> ?f:Logic.label -> Basic.term list -> tactic
(**
   Instantiate a formula. Tries {!Tactics.instA} then {!Tactics.instC}.
*)

val cut: ?info:Logic.info 
  -> ?inst:Basic.term list -> Logic.thm -> tactic
(** 
   [cut th]: Cut [th] into the sequent. If [~inst:trms] is given then the
   top-most variables of the theorem are instantiated with [trms]. 
   Entry point to {!Logic.Tactics.cut}. 
*)

val beta_tac : ?info:Logic.info -> ?f:Logic.label -> tactic
(** 
   [beta_tac]: Apply beta conversion. Entry point to
   {!Logic.Tactics.beta}.
*)

val name_tac: ?info:Logic.info -> string -> Logic.label -> tactic
(** 
   [name_tac ?info n lbl]: Name formula [lbl] with [n]. 
   Entry point to {!Logic.Tactics.nameA} and {!Logic.Tactics.nameC}. 
*)

val basic : 
    ?info:Logic.info -> ?a:Logic.label -> ?c:Logic.label -> tactic
(** 
   Proves the goal \[A{_ a}, asms |- B{_ c}, concls\] if A is
   alpha-equal to B.  Entry point to {!Logic.Tactics.basic}.
*)

val unify_tac : ?info: Logic.info ->  
  ?a:Logic.label -> ?c:Logic.label -> Logic.tactic
(**
   [unify_tac a c g]: Try to unify assumption [a] with conclusion [c].

   Assumption [a] may be universally quantified.  Conclusion [c] may
   be existentially quantified. Toplevel universal/existential
   quantifiers will be stripped, and the bound variables treated as
   variable for the unification. If unification succeeds, the
   toplevel quantifiers are instantiated with the terms found by
   unification.
   
   Final action is to apply [basic] to solve the goal if the terms
   are alpha-equal.

   Defaults: [a=(fnum -1)], [c=(fnum 1)].
*)

(** {7 Rewriting tactics} *)

val leftright : Rewrite.direction
(** Left to right rewriting. *)
val rightleft : Rewrite.direction
(** Right to left rewriting. *)

val rewrite_control: 
    ?max:int -> ?strat:Rewrite.strategy 
      -> Rewrite.direction -> Rewrite.control
(**
   [rewrite_control max strat dir]: Make a rewrite control. Default
   strategy is top-down ([?strat=Rewrite.topdown]).
*)

val is_rewrite_formula: Basic.term -> bool
(** 
   [is_rewrite_formula f]: Test whether [f] is an equality or a
   universally quantified equality (e.g. of the form [l=r] or [! x1
   .. x2 : l = r]).
*)

val gen_rewrite_tac: 
    ?info: Logic.info 
  -> Rewrite.control
    -> Logic.rr_type list 
      -> ?f:Logic.label -> Logic.tactic
(** 
   [gen_rewrite_tac info ctrl rules f]: General rewriting tactic.

   Rewrite formula [f] with list of theorems and assumptions given in
   [rules]. If [f] is not given, rewrite all formulas in sequent.

   This tactic is the entry-point for {!Logic.Tactics.rewrite}.
*)

val rewrite_tac: 
    ?info:Logic.info -> ?dir:Rewrite.direction
    -> Logic.thm list -> ?f:Logic.label -> Logic.tactic
(** 
   [rewrite_tac info dir thms f]: Rewrite formula [f] with list of
   theorems [thms]. If [f] is not given, rewrite all formulas in
   sequent.
*)

val once_rewrite_tac: 
    ?info:Logic.info -> ?dir:Rewrite.direction -> 
    Logic.thm list -> ?f:Logic.label -> Logic.tactic
(** 
   [once_rewrite_tac info dir thms f]: Rewrite formula [f] once.
   If [f] is not given, rewrite all formulas in sequent.
*)

val gen_replace_tac: 
    ?info:Logic.info -> ?ctrl:Rewrite.control
  -> ?asms:Logic.label list 
    -> ?f:Logic.label -> Logic.tactic
(**
   [gen_replace_tac info ctrl asms f]: Rewrite formula [f] with the
   assumptions in list [asms].  If [f] is not given, rewrite all
   formulas in sequent. If [asms] is not given, use all assumptions of
   the form [l=r] or [!x1 .. xn: l = r]. Doesn't rewrite the
   assumptions used as rewrite rules.
*)

val replace_tac: 
    ?info:Logic.info -> ?dir:Rewrite.direction
  -> ?asms:Logic.label list 
    -> ?f:Logic.label -> Logic.tactic
(** 
   [replace_tac info dir asms f]: Rewrite formula [f] with assumptions
   in list [asms].  If [f] is not given, rewrite all formulas in
   sequent.  If [asms] is not given, use all assumptions of the form
   [l=r] or [!x1 .. xn: l = r].  Doesn't rewrite the used assumptions.
*)

val once_replace_tac: 
    ?info:Logic.info -> ?dir:Rewrite.direction
  -> ?asms:Logic.label list 
    -> ?f:Logic.label -> Logic.tactic
(** 
   [once_replace_tac info dir asms f]: Rewrite formula [f] with
   assumptions in list [asms] once. If [f] is not given, rewrite all
   formulas in sequent.  If [asms] is not given, use all assumptions
   of the form [l=r] or [!x1 .. xn: l = r].  Doesn't rewrite the used
   assumptions.
*)

(** {5 Derived tactics and tacticals} *)

val named_tac : 
    ?info: Logic.info -> (info:Logic.info -> tactic) 
      -> string list -> string list 
	-> tactic
(** 
   [named_tac tac anames cnames]: Apply [tac], renaming the
   assumptions and conclusions produced with the names in [anames] and
   [cnames] respecatively. The number of names does not have to match
   the number of assumptions/conclusions produced.

   Actions: apply [tac ~info:inf goal], rename each of
   [Drule.aformulas inf] with a name from [anames], rename each of
   [Drule.cformulas inf] with a name from [cnames], in order. Set
   [info=inf'] where [inf'] is [inf], with the formula tag produced by
   renaming.
*) 

(** {7 Pattern matching tacticals} *)

(** {8 Support functions} *)

val find_match_formulas: 
    Gtypes.substitution
  -> Scope.t -> (Basic.term -> bool) 
    -> Basic.term -> Logic.tagged_form list -> Logic.label
(**
   [find_match_formulas scp varp t fs]: Find a match for a term in list
   of tagged formulas.  Return the tag of the first formula in [fs]
   to unify with term [t] in scope [scp].  [varp] determines which
   terms can be bound by unification.  raise Not_found if no match.

   Only free variables are bound in the matching process.
   e.g. in [<< !x. y and x >>] only [y] is a bindable variable 
   for the match.
 *)

val find_match_asm : 
    Gtypes.substitution
  -> Basic.term -> Logic.Sequent.t -> Logic.label
(** 
   [find_match_asm tyenv t sq]: Find a match for [t] in the assumptions of
   [sq].  Return the tag of the first formula in the assumptions to
   unify with term [t] in the scope of sequent [sq].
   raise Not_found if no match.
 *)

val find_match_concl :     
    Gtypes.substitution
  -> Basic.term -> Logic.Sequent.t -> Logic.label
(** 
   [match_concl t sq]: Find a match for [t] in the assumptions of
   [sq].  Return the tag of the first formula in the assumptions to
   unify with term [t] in the scope of sequent [sq].  raise Not_found
   if no match.
*)

(** {8 Tacticals} *)

val match_asm: Basic.term -> (Logic.label -> tactic) -> tactic
(** 
   [match_asm trm tac g]: Apply a tactic to the assumption matching 
   term.

   Find the label [l] of the first assumption, in the first subgoal of
   [g], which matches [trm] then apply tactic [tac l] to [g].  Fails
   if [tac l] fails or if there is no matching assumption.

   Free variables in trm may be bound in the matching process.
   e.g. in [<< !x. y and x >>] only [y] is a bindable variable 
   for the match.
*)

val match_concl: Basic.term -> (Logic.label -> tactic) -> tactic
(** 
   [match_concl trm tac g]: Apply a tactic to the conclusion matching
   a term.

   Find the label [l] of the first conclusion, in the first subgoal of
   [g], which matches [trm] then apply tactic [tac l] to [g].  Fails
   if [tac l] fails or if there is no matching conclusion.

   Free variables in trm may be bound in the matching process.
   e.g. in [<< !x. y and x >>] only [y] is a bindable variable 
   for the match.
*)

val match_formula: Basic.term -> (Logic.label -> tactic) -> tactic
(** 
   [match_formula trm tac g]: Apply a tactic the assumption or
   conclusion matching a term.

   Find the label [l] of the first formula, in the first subgoal of
   [g], which matches [trm] then apply tactic [tac l] to [g].  The
   match is carried out first on the assumptions then on the
   conclusions. Fails if [tac l] fails or if there is no matching
   formula in the subgoal.

   Free variables in trm may be bound in the matching process.
   e.g. in [<< !x. y and x >>] only [y] is a bindable variable 
   for the match.
*)




