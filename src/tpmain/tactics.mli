(* Tactics and Tacticals *)

type tactic = Logic.rule

(* label functions *)

val fnum: int -> Logic.label
val ftag: Tag.t -> Logic.label

(* [!~]: 

   Integer to label.
   [!~ x] is equivalent to [fnum  (-x)].
   Used for identifying assumptions in a sequent.
 *)
val (!~): int -> Logic.label


(* [!+]: 

   Integer to label.
   [!+ x] is equivalent to [fnum x].
   Used for identifying conclusions in a sequent.
 *)
val (!~): int -> Logic.label

(* [!!]: 
   Integer to label.
   [!! x] is equivalent to [fnum x].
   Used for assumptions and conclusions of a sequent.
 *)
val (!!): int -> Logic.label

(* rewriting direction *)
val leftright : Rewrite.direction
val rightleft : Rewrite.direction

(** [rotateA]/[rotateC]

   Rotate the assumptions/conclusions
*)
val rotateA : tactic
val rotateC : tactic

(** [copy_asm l]/[copy_concl l]

   Make a copy of assumption/conclusion labelled [l].
*)
val copy_asm : Logic.label -> tactic
val copy_concl : Logic.label -> tactic

(**
   [delete l]/[deleten ls]

   Delete assumption/conclusion labelled by [l] ([delete])
   or by a label in [ls] ([deleten]).
*)
val delete: Logic.label -> tactic 
val deleten: Logic.label list -> Logic.rule

val postpone: Logic.goal -> Logic.goal

(**
   [skip]: Do nothing. Useful for converting a node to a branch.
   (=[Drule.skip]).

   [foreach tac b]: Apply tactic [tac] to each node of branch [b].
   (=[Drule.foreach]).

   [fail ?e]: Fail, raising [e] if given 
   and (Result.error "failed") otherwise.
*)
val skip : tactic
val foreach: tactic -> Logic.branch -> Logic.branch
val fail: ?err:exn -> tactic 

(** [add_info_tac f info g]

   Tactic to add information to [info].

   Apply [f info] then [skip].
*)
val add_info_tac:
    ('a -> unit) -> 'a  -> tactic

(**
   [trivial]: Prove the sequent [  A |-C_{1}, true, C_{n} ].

   [basic]: Prove the sequent [  A_{1}, x, A_{m}  |-C_{1}, y, C_{n} ],
   where [x] and [y] are alpha-equal.
*)
val trueR : ?c:Logic.label -> tactic

val basic : tactic

(** 
   [cut th]: Cut [th] into the sequent.
*)
val cut : Logic.thm -> tactic

(*
   [unify_tac a c g]

   Try to unify assumption [a] with conclusion [c].

   Assumption [a] may be universally quantified.
   Conclusion [c] may be existentially quantified.
   Toplevel universal/existential quantifiers will be stripped, and the
   bound variables treated as variable for the unification.
   If unification succeeds, the toplevel quantifiers are instantiated
   with the terms found by unification.
   
   Final action is to apply [basic] to solve the goal if the terms
   are alpha-equal.

   Defaults: [a=(fnum -1)], [c=(fnum 1)].
*)
val unify_tac : ?info: Logic.info ->  
  ?a:Logic.label -> ?c:Logic.label -> Logic.rule

(**
   [lift id sqnt]

   Move assumption/conclusion with identifier [id] to 
   to top of the assumptions/conclusions of sequent sqnt.

   Raise Not_found if identified formula is not in 
   assumptions/conclusions.
 *)
val lift : Logic.label -> tactic

(**
   User level tactics

   Apply the basic rules to the first assumption/conclusion
   which will succeed 

   If assumption [a], conclusion [c] or formula [f] is not
   found, these will search for a suitable assumption/conclusion.
   (Parameter [f] is used for formulas which can be either 
   in the assumptions or the conclusions).
*)
val conjC : ?c: Logic.label -> tactic
val conjA : ?a: Logic.label -> tactic
val disjC : ?c: Logic.label -> tactic
val disjA : ?a: Logic.label -> tactic
val negC : ?c: Logic.label -> tactic
val negA : ?a: Logic.label -> tactic
val implC : ?c: Logic.label -> tactic
val implA : ?a: Logic.label -> tactic
val existA : ?a: Logic.label -> tactic
val existC : ?c: Logic.label -> Basic.term -> tactic 
val allC : ?c: Logic.label -> tactic
val allA : ?a: Logic.label -> Basic.term -> tactic

(**
   [beta_tac]: Apply beta conversion.
 *)
val beta_tac : ?f:Logic.label -> tactic


(* tacticals *)

type tactical = tactic 

(**
   [repeat tac]: Apply [tac] at least one then repeat until it fails
   or there are no more subgoals.
   
   [tac1 ++ tac2]: Apply [tac1] then, if there are subgoals, apply [tac2]
   to the subgoals. ([tac1 ++ tac2 = Drule.seq tac1 tac2]).

   [seq tacl]: Apply each tactic in [tacl] in sequence to the subgoals
   resulting from the previous tactic.
   Fails if [tacl] is empty.

   [orl tacl]: Apply each tactic in [tacl], in sequence, until one succeeds.
   Fails if no tactic succeeds.

   [tac1 || tac2]: Apply [tac1], if that fails, apply [tac2].
   (=[orl [tac1; tac2]]

   [thenl tac tacl]: Apply tactic [tac] then pair each of the tactics
   in [tacl] with a resulting subgoal. If [tag n] results in subgoals
   [g1, g2, .. gn], the tactics are matched up to produce [tac1 g1,
   tac2 g2, .. , tacn gn]. Excess tactics in [tacl] are silently
   discarded. Excess subgoals are appended to the result of the tactics.

   [tac -- tacl]: Synonym for [thenl tac tacl].

   [apply_if pred tac]: Apply [tac] to a node if predicate [pred] is
   satisfied by the node.

   [pred --> tac]: Synonym for [apply_if pred tac].   

   [each xs tac]: Apply [tac] to each [x] in [xs] in order.
   For [xs = [y1; y2; .. ; yn]], 
   returns [(tac y1) ++ (tac y2) ++ .. ++ (tac yn)]
*)
val repeat : tactic -> tactic
val (++) : tactic -> tactic -> tactic
val seq : tactic list -> tactic 
val orl:  tactic list -> tactic 
val (||) : tactic -> tactic -> tactic

val thenl : tactic ->  tactic list -> tactic 
val (--) : tactic ->  tactic list -> tactic 

val apply_if : (Logic.node -> bool) -> tactic -> tactic
val (-->) : (Logic.node -> bool) -> tactic -> tactic

val each: 'a list -> ('a -> tactic) -> tactic

(** [gen_rewrite_tac info dir rules f]

   Rewrite formula [f] with list of theorems and assumptions given
   in [rules].
   If [f] is not given, rewrite all formulas in sequent.
*)
val gen_rewrite_tac: 
    ?info: Logic.info 
  -> Rewrite.control
    -> Logic.rr_type list 
      -> ?f:Logic.label -> Logic.rule

(** [rewrite_tac info dir thms f]

   Rewrite formula [f] with list of theorems [thms].
   If [f] is not given, rewrite all formulas in sequent.
*)
val rewrite_tac: 
    ?ctrl:Rewrite.control
  -> Logic.thm list -> ?f:Logic.label -> Logic.rule


(** [once_rewrite_tac info dir thms f]

   Rewrite formula [f] once.
   If [f] is not given, rewrite all formulas in sequent.
*)
val once_rewrite_tac: 
    Logic.thm list -> ?f:Logic.label -> Logic.rule


(** [replace_tac info dir asms f]

   Rewrite formula [f] with assumptions in list [asms].
   If [f] is not given, rewrite all formulas in sequent.
   If [asms] is not given, use all assumptions 
   of the form [l=r] or [!x1 .. xn: l = r].
   Doesn't rewrite the used assumptions.

   [once_replace_tac info asms f]

   replace exactly once.
*)
val replace_tac: 
    ?ctrl:Rewrite.control
  -> ?asms:Logic.label list 
    -> ?f:Logic.label -> Logic.rule

val once_replace_tac: 
    ?asms:Logic.label list 
  -> ?f:Logic.label -> Logic.rule

(** [is_rewrite_formula f] 
   test whether [f] is an equality or a universally quantified 
   equality.
   (e.g. of the form [l=r] or [! x1 .. x2 : l = r]
 *)
val is_rewrite_formula: Basic.term -> bool

(** 
   [match_asm trm tac g]

   Find the label [l] of the first assumption, in the first subgoal of [g],
   which matches [trm] then apply tactic [tac l] to [g].
   Fails if [tac l] fails or if there is no matching assumption.
 *)
val match_asm: Basic.term -> (Logic.label -> tactic) -> tactic

(** 
   [match_concl trm tac g]

   Find the label [l] of the first conclusion, in the first subgoal of [g],
   which matches [trm] then apply tactic [tac l] to [g].
   Fails if [tac l] fails or if there is no matching conclusion.
 *)
val match_concl: Basic.term -> (Logic.label -> tactic) -> tactic

(** 
   [match_formula trm tac g]

   Find the label [l] of the first formula, in the first subgoal of [g],
   which matches [trm] then apply tactic [tac l] to [g].
   The match is carried out first on the assumptions then on the conclusions.
   Fails if [tac l] fails or if there is no matching formula in the subgoal.
 *)
val match_formula: Basic.term -> (Logic.label -> tactic) -> tactic

(* Tacticals for dealing with information returned by tactics
   (Probably pointless)
*)

(** [itactic]
   Information passing tactics.
 *)
type itactic = 
    Logic.info 
  -> (Tag.t list * Tag.t list * Basic.term list)
    -> tactic

(**
   [itac tac (gs, fs, vs) g]

   Handle information passing tactics.
   Apply [tac gs fs vs info] to [g]
   where [info] is of type [Logic.info]
   and [gs], [fs], [vs] are the goal, formulas and terms to be
   passed to tac.

   Returns [((ngs, nfs, nvs), ng)] 
   where [ng] is the result of tactic [tac]
   and [ngs], [nfs], [nvs] are the goals, formulas and terms
   extracted from the information stored in [info] by [tac].

   Example:
   [itac (fun _ -> allE) ([], [], [])]
 *)
val itac: 
    itactic 
  -> (Tag.t list * Tag.t list * Basic.term list)
    -> Logic.branch
      -> ((Tag.t list * Tag.t list * Basic.term list) * Logic.branch)

(** [iseq initial itacs g]
   Make a tactic from a list of itactics.
   Apply itactics in the list [itac] to goal [g] in order.
   
   [initial], if present, is fed to the first itactic in the list.
   The remaning itactics get their information from the prevoius 
   itactic.
 *)
val iseq: 
    ?initial:(Tag.t list * Tag.t list * Basic.term list) 
  -> itactic list -> tactic

(** [ialt initial itacs g]
   Make a tactic from a list of itactics.
   Apply itactics in the list [itac] to goal [g] until one succeeds.
   
   [initial], if present, is fed to the first itactic in the list.
   The remaning itactics get their information from the prevoius 
   itactic.
 *)
val ialt: 
    ?initial:(Tag.t list * Tag.t list * Basic.term list) 
  -> itactic list -> tactic
