(* tactics and tacticals *)
(* user level rules *)

type tactic = Logic.rule

(* label functions *)

val fnum: int -> Logic.label
val ftag: Tag.t -> Logic.label


(* [!~]: 
   integer to label.
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

(* conversion from rule to tactic *)

val rotateA : tactic
val rotateC : tactic

val copy_asm : Logic.label -> tactic
val copy_concl : Logic.label -> tactic

(* delete a list of assumptions/conclusions *)
val deleten: Logic.label list -> Logic.rule

val trivial : ?c:Logic.label -> tactic
val skip : tactic
val basic : tactic
val postpone: tactic

val cut : Logic.thm -> tactic

(*
   [unify_tac a c g]
   unify assumption [a] with conclusion [c]
*)
val unify_tac : ?info: Logic.info ->  
  ?a:Logic.label -> ?c:Logic.label -> Logic.rule
(*
val unify_tac : Logic.label -> Logic.label -> tactic
*)
(*
   [lift id sqnt]
   Move assumption/conclusion with identifier [id] to 
   to top of the assumptions/conclusions of sequent sqnt.

   Raise Not_found if identified formula is not in 
   assumptions/conclusions.
*)
val lift : Logic.label -> tactic

(* 
   the following apply the basic rules to the first assumption/conclusion
   which will succeed 
*)

val conjI : ?c: Logic.label -> tactic
val conjE : ?a: Logic.label -> tactic
val disjI : ?a: Logic.label -> tactic
val disjE : ?c: Logic.label -> tactic
val negA : ?a: Logic.label -> tactic
val negC : ?c: Logic.label -> tactic
val implI : ?c: Logic.label -> tactic
val implE : ?a: Logic.label -> tactic
val existI : ?a: Logic.label -> tactic
val existE : ?c: Logic.label -> Basic.term -> tactic 
val allI : ?c: Logic.label -> tactic
val allE : ?a: Logic.label -> Basic.term -> tactic

(*
   beta conversion 
   to given asumption/conclusion
   or search for suitable assumption/conclusion
*)
val beta_tac : ?f:Logic.label -> tactic


(* delete assumption or conclusion *)
val delete: Logic.label -> tactic 

(* tacticals *)

type tactical = tactic 

val repeat : tactic -> tactic
val (++) : tactic -> tactic -> tactic
val (||) : tactic -> tactic -> tactic
val orelseF : tactic -> tactic -> tactic
val firstF : tactic list -> tactic      
(* val thenl : tactic list -> tactic *)
val seq : tactic list -> tactic   (* seq was thenl *)
val apply_list : tactic list -> tactic
val orl: tactic list -> tactic



(** [gen_rewrite_tac info dir rules f]
   rewrite formula [f] with list of theorems and assumptions given
   in [rules].
   if f is not given, rewrite all formulas in sequent.
*)
val gen_rewrite_tac: 
    ?info: Logic.info 
  -> Rewrite.control
      -> Logic.rr_type list 
	-> ?f:Logic.label -> Logic.rule

(** [rewrite_tac info dir thms f]
   rewrite formula [f] with list of theorems [thms].
   if f is not given, rewrite all formulas in sequent.
*)
val rewrite_tac: 
    ?dir:Rewrite.direction
      -> Logic.thm list -> ?f:Logic.label -> Logic.rule


(** [replace_tac info dir asms f]
   rewrite formula [f] with assumptions in list [asms].
   if [f] is not given, rewrite all formulas in sequent.
   if [asms] is not given, use all assumptions 
   of the form [l=r] or [!x1 .. xn: l = r].
   Doesn't rewrite the used assumptions.
*)
val replace_tac: 
    ?dir:Rewrite.direction
      -> ?asms:Logic.label list 
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

(* Tacticals for dealing with information returned by tactics *)

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
    -> Logic.goal
      -> ((Tag.t list * Tag.t list * Basic.term list) * Logic.goal)

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
val iseq: 
   ?initial:(Tag.t list * Tag.t list * Basic.term list) 
    -> itactic list -> tactic
