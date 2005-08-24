(*-----
 Name: tactics.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** Tactics and Tacticals *)

type tactic = Logic.tactic

(** {5 Support functions} *)

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

val (||) : tactic -> tactic -> tactic
(**
   [tac1 || tac2]: Apply [tac1], if that fails, apply [tac2].
   [tac1 || tac2] is [alt [tac1; tac2]].
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
   One-armed conditional.
   [pred --> tac] is [cond pred tac skip]. 
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
   resulting tactics fail. Does nothing if [xs] is initially empty.
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

val trueR : ?info:Logic.info -> ?c:Logic.label -> tactic
val conjC : ?info:Logic.info -> ?c: Logic.label -> tactic
val conjA : ?info:Logic.info -> ?a: Logic.label -> tactic
val disjC : ?info:Logic.info -> ?c: Logic.label -> tactic
val disjA : ?info:Logic.info -> ?a: Logic.label -> tactic
val negC : ?info:Logic.info -> ?c: Logic.label -> tactic
val negA : ?info:Logic.info -> ?a: Logic.label -> tactic
val implC : ?info:Logic.info -> ?c: Logic.label -> tactic
val implA : ?info:Logic.info -> ?a: Logic.label -> tactic
val existC : ?info:Logic.info -> ?c: Logic.label -> Basic.term -> tactic 
val existA : ?info:Logic.info -> ?a: Logic.label -> tactic
val allC : ?info:Logic.info -> ?c: Logic.label -> tactic
val allA : ?info:Logic.info -> ?a: Logic.label -> Basic.term -> tactic

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
*)

val beta_tac : ?info:Logic.info -> ?f:Logic.label -> tactic
(** [beta_tac]: Apply beta conversion. *)

val name_tac: ?info:Logic.info -> string -> Logic.label -> tactic
(**  [name_tac ?info n lbl]: Name formula [lbl] with [n]. *)

val basic : ?info:Logic.info -> tactic
(** 
   Prove the goal \[A, asms |- A, concls\].  Formula [A] can occur in
   any position in the assumptions and conclusions, not just the
   first.
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
   [gen_replace_tac info ctrl asms f]: Rewrite formula [f] with
   assumptions in list [asms].  If [f] is not given, rewrite all
   formulas in sequent.  If [asms] is not given, use all assumptions
   of the form [l=r] or [!x1 .. xn: l = r].  Doesn't rewrite the used
   assumptions.
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

val match_asm: Basic.term -> (Logic.label -> tactic) -> tactic
(** 
   [match_asm trm tac g]: Apply a tactic to the assumption matching a term.

   Find the label [l] of the first assumption, in the first subgoal of
   [g], which matches [trm] then apply tactic [tac l] to [g].  Fails
   if [tac l] fails or if there is no matching assumption.
*)

val match_concl: Basic.term -> (Logic.label -> tactic) -> tactic
(** 
   [match_concl trm tac g]: Apply a tactic to the conclusion matching
   a term.

   Find the label [l] of the first conclusion, in the first subgoal of
   [g], which matches [trm] then apply tactic [tac l] to [g].  Fails
   if [tac l] fails or if there is no matching conclusion.
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
*)

