(*-----
 Name: simputils.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)


(** [dest_rrthm t]
   If [r] is [RRThm x] then return [x]
   otherwise raise Failure
*)
val dest_rrthm : Logic.rr_type -> Logic.thm

val dest_option: 'a option -> 'a

(** [dest_implies t] 
   destruct implication.
*)
val dest_implies : Basic.term -> Basic.term * Basic.term

(* val dest_option : 'a option -> 'a *)

val has_cond : 'a option -> bool

(** [sqnt_solved st b]: 
   [true] if sqequent tagged [st] is no longer in branch [b ]
 *)
val sqnt_solved : Tag.t -> Logic.branch -> bool

(** [apply_on_cond c t f x]
   if [c] is [None] then return [(t x)] else return [(f x)]
*)
val apply_on_cond : 
    'a option -> ('b -> 'c) -> ('b -> 'c) -> 'b -> 'c

(** [apply_tag tac g]
   Apply tactic [tac] to node [g], return new goal and tag record of tactic
 *)
val apply_tag : 
    (Logic.info -> Tactics.tactic) 
  -> Logic.node -> (Logic.tag_record * Logic.branch)

(** [apply_get_formula_tag n tac g]
   apply tactic [tac] to goal [g]
   return tags of formulas
   fail if more than [g] new formulas (tags) are generated
*)
val apply_get_formula_tag :
    int -> (Logic.info -> Tactics.tactic) 
      -> Logic.node -> (Tag.t list * Logic.branch)

(** [apply_get_single_formula_tag tac g]
   apply tactic [tac] to goal [g]
   return tag of single formula.
   fail if more than [1] new formula is reported by [tac]
 *)
val apply_get_single_formula_tag :
    (Logic.info -> Tactics.tactic) 
  -> Logic.node -> (Tag.t * Logic.branch)


val allA_list : Logic.label -> Basic.term list -> Logic.tactic

(** [is_variable qnts x]:
   test for variables (universal quantifiers) in an entry 
*)
val is_variable: Basic.binders list -> Basic.term -> bool


(**
 [equal_upto_vars varp x y]: Terms [x] and [y] are equal upto the
   position of the terms for which [varp] is true (which are
   considered to be variables.)

   This is used to determine whether a rewrite- or simp-rule could
   lead to an infinite loop (e.g. |- (x and y) = (y and x) ).
*)
val equal_upto_vars: 
    (Basic.term -> bool) -> Basic.term -> Basic.term -> bool
