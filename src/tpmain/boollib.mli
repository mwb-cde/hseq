(*-----
 Name: boollib.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

module BaseTheory:
    sig
      (*
	 A minimal base theory which can be used if no other 
	 theory can be found
       *)

      val builder:unit -> unit

  (** 
     [init()]: call [Tpenv.set_base_thy_builder builder].
   *)
      val init: unit -> unit

    end

(** Parser-Printer for If-Then-else *)
module BoolPP :
  sig
    open Parser.Pkit
    open Parser.Grammars
    open Lexer

    val ifthenelse_parser: infotyp -> Basic.term phrase
    val init_ifthenelse_parser: unit -> unit

(**
   [ifthenelse_id]
   Identifier of the conditional function.

   [ifthenelse_prec]
   precedence/fixity/associativity
*)
    val ifthenelse_id: Basic.ident
    val ifthenelse_pprec : Printer.record

    val ifthenelse_printer: 
	Printer.ppinfo-> int -> Basic.ident * Basic.term list -> unit
end


(*
   Tactics, some of which depend on theorems already having been
   proved.
*)

(* prove goals of the form A|- x=x, C *)
    val eq_tac : ?c:Logic.label -> Tactics.tactic

(* unfold a definition *)
    val unfold : ?f:Logic.label -> string -> Tactics.tactic

(* cut a named theorem *)
val cut_thm: string -> Tactics.tactic

(* test and rules for Iff *)
val is_iff: Formula.form -> bool

(* val iffC_rule: Logic.label -> Logic.rule *)
val iffC: ?c:Logic.label -> Tactics.tactic
val iffA: ?a:Logic.label -> Tactics.tactic

val asm_elims : 
    unit -> ((Formula.form->bool) * (Logic.label -> Logic.rule)) list
val conc_elims : 
    unit -> ((Formula.form->bool) * (Logic.label -> Logic.rule)) list

(*
val false_rule:  ?a:Logic.label -> Logic.rule
*)
val falseR:  ?a:Logic.label -> Tactics.tactic

val trivial : ?f:Logic.label -> Tactics.tactic


(* [flatten_tac]

   flattens formulas in a subgoal without creating new subgoals.

   apply negC, disjE, implI, allI
   to conclusions then apply negA, conjE, existI 
   to assumptions.
*)
    val flatten_tac : Tactics.tactic

(* [split_tac]

   split conjunctions and iff in the conclusions
   and disjunctions and implications in the assumptions
   creating new subgoals.
*)
    val split_tac : Tactics.tactic

(* [inst_tac f consts] 
   instantiate formula [f] with terms [consts]
*)
val inst_tac: ?f:Logic.label -> Basic.term list -> Tactics.tactic 
val inst_asm : ?a:Logic.label -> Basic.term list -> Tactics.tactic
val inst_concl : ?c:Logic.label -> Basic.term list -> Tactics.tactic

val inst_asm_rule : Logic.label -> Basic.term list -> Tactics.tactic

(**
   [cases_full_tac info x g]
   [cases_tac ?info x g]

   Adds formula [x] to assumptions of [g],
   creates new subgoal in which to prove [x].

   [cases_full_tac] does the work.
   [cases_tac] is a wrapper for [cases_full_tac], making [info] an
   optional argument.

   g|asm |- cncl      
   --> 
   g|asm |- t:x, cncl, g'| t:x, asm |- cncl 

   info: [g, g'] [t]
*)
val cases_full_tac : Logic.info option -> Basic.term -> Tactics.tactic
val cases_tac: ?info:Logic.info -> Basic.term -> Tactics.tactic

val get_cases_thm: unit -> Logic.thm


(* convert boolean equality to iff *)
val equals_tac: ?f:Logic.label -> Tactics.tactic

(** [false_tac]
   solve the subgoal if it has [false] in the assumptions
*)
val false_tac: Tactics.tactic

(** [bool_tac]
   solve the subgoal if it has [false] in the assumptions or [true]
   in the conclusions.
*)
val bool_tac:  Tactics.tactic

(**
   [mp_tac ~a ~a1]

   Modus ponens.
   if [a] is [l=>r] and [a1] is [l],
   then reduce [a] to [r].

   if [a] is [! x1 .. xn: l = r] and [a1] is [l],
   instantiate all of the [x1 .. xn] from [a1] before 
   reducing.

   [cut_mp_tac ?info thm ?a]

   Apply modus ponens to theorem [thm] and assumption [a].
   [thm] must be a (possibly quantified) implication [!x1 .. xn: l=>r]
   and [a] must be [l].

   If [a] is not given, finds a suitable assumption to unify with [l].

   info [] [thm_tag] []
   where tag [thm_tag] identifies the theorem in the sequent.
*)
val mp_tac: ?a:Logic.label -> ?a1:Logic.label -> Tactics.tactic
val cut_mp_tac:?info:Logic.info -> Logic.thm 
    -> ?a:Logic.label -> Tactics.tactic

(**
   [back_tac ~a ~c]
   
   Match, backward tactic.

   If [a] is [l=>r] and [c] is [r],
   then reduce [c] to [l].

   thm= |- l => r
   asms |- l, concls
   -->
   asms |- r, concls

   if [a] is [! x1 .. xn: l = r] instantiate all of the [x1 .. xn]
   from [c] before reducing.

   info: [g_tag] [c_tag] []
   where 
   [g_tag] is the new goal
   [c_tag] identifies the new conclusion.

   [cut_back_tac ?info thm ?c]
   Cut theorem [thm] into the sequent and apply [asm_back_tac] 
   to the theorem and conclusion [c].

   [thm] must be a (possibly quantified) implication [!x1 .. xn: l=>r]
   and [c] must be [r].

   info: as for [asm_back_tac].
*)
val back_tac: 
    ?info:Logic.info ->  ?a:Logic.label -> ?c:Logic.label -> Tactics.tactic

val cut_back_tac:
    ?info:Logic.info -> Logic.thm 
      -> ?c:Logic.label -> Tactics.tactic



module Props : 
sig

(**
   [make_n_ax()]: prove theorem n
   [get_n_ax()]: get theorem n, proving it if necessary
*)

(**
   [iff_equals_ax]:  |- !x y: (x iff y) = (x = y)
*)
val make_iff_equals_ax : unit -> Logic.thm
val iff_equals_ax : Logic.thm option ref
val get_iff_equals_ax : unit -> Logic.thm

(**
   [equals_iff_ax]:  |- !x y: (x = y) = (x iff y)
*)
val make_equals_iff_ax : unit -> Logic.thm
val equals_iff_ax : Logic.thm option ref
val get_equals_iff_ax : unit -> Logic.thm

(**
   [bool_eq_ax]: |- !x y: x iff y = ((x => y) and (y=>x))
 *)
val make_bool_eq_ax : unit -> Logic.thm
val bool_eq_ax : Logic.thm option ref
val get_bool_eq_ax : unit -> Logic.thm

(**
   [double_not_ax]: |- ! x: x = (not (not x))
 *)
val make_double_not_ax : unit -> Logic.thm
val double_not_ax : Logic.thm option ref
val get_double_not_ax : unit -> Logic.thm

(**
   [rule_true_ax]:  |- !x: x = (x=true) 
 *)
val make_rule_true_ax : unit -> Logic.thm
val rule_true_ax : Logic.thm option ref
val get_rule_true_ax : unit -> Logic.thm

(**
   rule_false_ax: !x: (not x) = (x=false)
 *)
val make_rule_false_ax : unit -> Logic.thm
val rule_false_ax : Logic.thm option ref
val get_rule_false_ax : unit -> Logic.thm
end

(** [conv_rule scp conv thm]
   apply conversion [conv] to theorem [thm]
 *)

val conv_rule :
    Scope.t ->
      (Scope.t -> Basic.term -> Logic.thm) -> Logic.thm -> Logic.thm

module Rules:
sig
(** 
   Rules: Functions to construct theorems from other theorems.
   These may depend on the theorems in Props.
*)

(** [once_rewrite_rule scp rules thm]: 
   rewrite [thm] with [rules] once.
*)
val once_rewrite_rule :
    Scope.t -> Logic.thm list -> Logic.thm -> Logic.thm


(*
   [conjunctL scp thm]
   Get the left hand side of conjunct [thm].
   [conjunctL scp << l and r >> = l]

   [conjunctR scp thm]
   Get the right hand side of conjunct [thm].
   [conjunctL scp << l and r >> = r]

   [conjuncts scp thm]
   break theorem [thm] into the list of conjuncts.
   [conjuncts scp << f1 and f2 and .. and fn>> = [f1; f2; ..; fn]]
 *)
  val conjunctL : Scope.t -> Logic.thm -> Logic.thm 
  val conjunctR : Scope.t -> Logic.thm -> Logic.thm 
  val conjuncts : Scope.t -> Logic.thm -> Logic.thm list 

end

module Convs:
sig
(** 
   Convs: Conversions on boolean operators.
   These may depend on the theorems in Props.
*)

(** [neg_all_conv]: |- (not (!x..y: a)) = ?x..y: not a *)
val neg_all_conv: Scope.t -> Basic.term -> Logic.thm

(** [neg_exists_conv]: |- (not (?x..y: a)) = !x..y: not a *)
val neg_exists_conv: Scope.t -> Basic.term -> Logic.thm


end

