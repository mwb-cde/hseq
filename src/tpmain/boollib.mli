(*-----
   Name: boollib.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(**
   Support for boolean proofs.
 *)

(** A minimal base theory. *)
module BaseTheory:
    sig
      (**
	 A minimal base theory which can be used if no other 
	 theory can be found.
       *)

      val builder: unit -> unit
	  (** Build the minimal theory. *)

(** {7 Initialising function} *)
      val init: unit -> unit
	  (** 
	     Set {!Global.Init.set_base_thy_builder} to
	     {!Boollib.BaseTheory.builder}.
	   *)
    end

(** Printer-Parser for Boolean functions. *)
module PP :
    sig

      open Parser.Pkit
      open Parser.Grammars
      open Lexer

      val negation_pprec : Printer.record
(**
   Printer for negation (base.not). Prints [ << base.not x >> ] 
   as [~x] rather than [~ x].
 *)

      val ifthenelse_id: Basic.ident
(**
   [ifthenelse_id]: Identifier for the conditional.
 *)

      val ifthenelse_pprec : Printer.record
(**
   [ifthenelse_prec]: Precedence/fixity/associativity of the conditional.
 *)

      val ifthenelse_parser: infotyp -> Basic.term phrase
(**
   Parser for the conditional. The conditional has syntax [<< if b then
   t else f >>].
 *)

      val ifthenelse_printer: 
	  Printer.ppinfo
	-> (Printer.fixity * int) 
	  -> (Basic.term * Basic.term list) Printer.printer
(**
   Printer for the conditional
 *)

      val choice_ident: Basic.ident
(** Identifier for choice (the Hilbert epsilon) *)

      val choice_sym : string
(**
   The symbol denoting the choice quantifier ([choice_sym = "@"])
 *)

      val choice_pp: Printer.fixity * int
(**
   Precedence and fixity of the choice operator.
 *)

      val choice_parser: infotyp -> Basic.term phrase
(**
   Parser for the choice operator. Syntax [<< @ x: P >>]
 *)

      val choice_printer: 
	  Printer.ppinfo
	-> (Printer.fixity * int) 
	  -> (Basic.term * Basic.term list) Printer.printer
(** Printer for the choice operator. *)


(** {7 Initialising functions } *)

      val init_printers : unit -> unit
(** Initialise printers. *)   

      val init_parsers : unit -> unit
(**
   Initialise parsers.
 *)

      val init: unit -> unit
(** Initialise printers and parsers. *)
    end

(** {5 Support functions} *)

val find_unifier: 
    Scope.t ->  Gtypes.substitution 
      -> (Basic.term -> bool)
	-> Basic.term -> ?exclude:(Logic.tagged_form -> bool)
	  -> Logic.tagged_form list 
	    -> (Tag.t * Term.substitution)
(**
   [find_unifier scp typenv varp trm ?exclude forms]: Find the first
   formula in [forms] which unifies with [trm]. Return the tag of the
   formula and the substitution cosntructed by unification. Ignore
   those formulas for which [?exclude] is true (if it is given).

   [varp] determines what is a bindable variable for unification.
   [typenv] is the type environment, to pass to the unifier.
   [scp] is the scope, to pass to the unifier.
   Raise Not_found if no unifiable formula is found.
 *)

(**
   [find_qnt_opt kind ?f pred forms] 

   Find the first formula in [forms] to satisfy [pred].  The formula
   may by quantified by binders of kind [kind].  Returns the binders,
   the tag and the formula.

   if [f] is given, the formula must be tagged with [f]. 

   Raises [Not_found] if no formula can be found which satisfies all the
   conditions.
*)
val find_qnt_opt: 
    Basic.quant_ty 
  -> (Basic.term -> bool)
    -> Logic.tagged_form list 
      -> (Tag.t * Basic.binders list * Basic.term)



(** {5 Basic Tactics} *)

val false_def: unit -> Logic.thm
(** Get the definition of [false]. *)

val falseA: ?info:Logic.info -> ?a:Logic.label -> Tactics.tactic
(** 
   Solve a goal of the form \[ false{_ a}, A |- C \].
   [info] is unchanged.
*)

val trivial : ?info:Logic.info -> ?f:Logic.label -> Tactics.tactic
(** 
   Solve a goal of the form \[ false{_ f}, A |- C \] 
   or \[ A |- true{_ f}, C \].
   [info] is unchanged.
*)

val eq_tac :  ?info:Logic.info -> ?c:Logic.label -> Tactics.tactic
(**
   Prove goals of the form \[A|- x=x{_ c}, C\].
   [info] is unchanged.
 *)

val unfold : 
    ?info:Logic.info -> ?f:Logic.label -> string -> Tactics.tactic
(** 
   [unfold ?f n]: Unfold the definition of [n] at formula [?f]. 

   info: [aforms=[f'], cforms=[]] or [aforms=[], cforms=[f']]
   depending on whether [f] is in the assumptions or conclusions.
   [f'] is the tag of the formula resulting from rewriting.
*)

val cut_thm: 
    ?info:Logic.info -> ?inst:Basic.term list 
      -> string -> Tactics.tactic
(** Cut a named theorem, with optional instantiation. *)

(** {7 Boolean equivalence} *)

val is_iff: Formula.form -> bool
(** Test whether a formula is a boolean equivalence. *)

val iff_def: unit -> Logic.thm
(** Get the definition of [iff]. *)

val iffA: ?info:Logic.info -> ?a:Logic.label -> Tactics.tactic
(** 
   [iffA l sq]: Elminate the equivalance at assumption [l].

   {L
   g:\[(A iff B){_ l}, asms |- concl\]

   ---->

   g1:\[A{_ l1}, asms |- B{_ l2}, concl\]; 
   g2:\[B{_ l3}, asms |- A{_ l4}, concl\]; 
   }

   info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
 *)

val iffC: ?info:Logic.info -> ?c:Logic.label -> Tactics.tactic
(** 
   [iffC l sq]: Elminate the equivalence at conclusion [l]

   {L
   g:\[asms |- (A iff B){_ l}, concl\]

   ---->

   g1:\[asms |- (A=>B){_ l}, concl\]; 
   g2:\[asms |- (B=>A){_ l}, concl\]; 
   }

   info: [goals = [g1; g2], aforms=[], cforms=[l], terms = []]
 *)

val iffE: ?info:Logic.info -> ?c:Logic.label -> Tactics.tactic
(** 
   [iffE l sq]: Fully elminate the equivalence at conclusion [l]

   {L
   g:\[asms |- (A iff B){_ l}, concl\]

   ---->

   g1:\[A{_ l1}, asms |- B{_ l2}, concl\]; 
   g2:\[B{_ l3}, asms |- A{_ l4}, concl\]; 
   }

   info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
 *)

(** 
   {5 Eliminating boolean operators} 
*)

val direct_alt: 
    (Logic.info -> Logic.label -> Tactics.tactic) list 
    ->  Logic.info -> Logic.label -> Tactics.tactic
(**
   [direct_alt tacs info l]: Directed alt. Like {!Tactics.alt} but
   pass [info] and [l] to each tactic in [tacs].
**)

val direct_map_some: 
  (Logic.label -> Tactics.tactic)
    -> Logic.label list ref -> Logic.label list -> Tactics.tactic
(**
   [direct_map_some tac lst l]: Directed map_some. Like
   {!Tactics.map_some} but pass [info] and [l] to [tac]. If [tac]
   fails for [l], then [lst:=l::!lst].
**)

val asm_elim_rules_tac :
    ?info:Logic.info 
  -> ((Logic.info -> Logic.label -> Tactics.tactic) list
	* (Logic.info -> Logic.label -> Tactics.tactic) list)
    -> Logic.label
      -> Tactics.tactic
(** 
   [asm_elim_rules ?info (arules, crules) f goal]: Apply elimination
   rules to assumption [f] and to all resulting assumptions and
   conclusions. Assumptions are eliminated with [arules], conclusions
   with [crules]. Any new tag which can't be eliminated are stored in
   [?info] (in arbitrary order and may contain duplicates).
*)


val concl_elim_rules_tac :
    ?info:Logic.info 
  -> ((Logic.info -> Logic.label -> Tactics.tactic) list
	* (Logic.info -> Logic.label -> Tactics.tactic) list)
    -> Logic.label
      -> Tactics.tactic
(** 
   [concl_elim_rules ?info (arules, crules) f goal]: Apply elimination
   rules to conclusion [f] and to all resulting assumptions and
   conclusions. Assumptions are eliminated with [arules], conclusions
   with [crules]. The tag of any new formula for which the elimination
   rules fails is stored in [?info] (in arbitrary order and may
   contain duplicates).
*)


val elim_rules_tac :
    ?info:Logic.info 
  -> ((Logic.info -> Logic.label -> Tactics.tactic) list
     * (Logic.info -> Logic.label -> Tactics.tactic) list)
      -> Logic.label list -> Logic.label list
	-> Tactics.tactic
(**
   [elim_rules_tac ?info (arules, crules) albls clbls]: Apply
   elimination rules to all assumptions with a label in [albls] and
   all conclusions with a label in [clbls] and with to all resulting
   assumptions and conclusions. The tag of any new formula for which
   the elimination rules fails is stored in [?info] (in arbitrary
   order and may contain duplicates).
*)

val apply_elim_tac :
    (?info:Logic.info 
     -> Logic.label list -> Logic.label list
       -> Tactics.tactic)
    -> ?info:Logic.info 
      -> ?f:Logic.label
	-> Tactics.tactic
(**
   [apply_elim_tac tac ?info ?f]: Apply elimination tactic [tac] to
   formula [?f]. If [?f] is not given, use all formulas in the
   sequent. The tag of any new formula for which the elimination rules
   fails is stored in [?info] (in arbitrary order and may contain
   duplicates).

   [apply_elim_tac] is intended to be used to wrap
   {!Boollib.elim_rules_tac}.
*)

(** 
   {7 Splitting subgoals}

   A subgoal formula constructed from an operator [op] {e split} is
   split if eliminating the operator results in more than one subgoal
   (or proves the subgoal).  For example, {!Tactics.conjC} [~c:f] will
   split formula [f].
*)

val split_asm_rules:
    (Logic.info -> Logic.label -> Tactics.tactic) list
(** The rules used by {!Boollib.split_tac} to split assumptions *)
val split_concl_rules:
    (Logic.info -> Logic.label -> Tactics.tactic) list
(** The rules used by {!Boollib.split_tac} to conclusions assumptions *)

val split_asms_tac: 
    ?info:Logic.info -> Logic.label -> Tactics.tactic
(**
   Eliminate operators in the assumptions which introduce new
   subgoals. Uses the same rules as {!Boollib.split_tac}.
*)
val split_concls_tac: 
    ?info:Logic.info -> Logic.label -> Tactics.tactic
(**
   Eliminate operators in the conclusions which introduce new
   subgoals. Uses the same rules as {!Boollib.split_tac}.
*)

val split_tac : 
    ?info:Logic.info -> ?f:Logic.label -> Tactics.tactic
(**
   Eliminate operators in the assumptions and conclusions which
   introduce new subgoals. Resulting tag information, in [?info], may
   contain duplicates.

   In the assumptions, eliminates [false], disjunction ([|]),
   implication ([=>]).

   In the conclusions, eliminates [true], conjunction ([&]).
*)

(** 
   {7 Flattening subgoals}

   A subgoal formula constructed from an operator [op] is {e
   flattened} if eliminating the operator results in at most one
   subgoal (or proves the subgoal). For example, {!Tactics.conjA}
   [~a:f] flattens formal [f].
*)

val flatter_asm_rules:
    (Logic.info -> Logic.label -> Tactics.tactic) list
(** 
   The rules used by {!Boollib.flatten_tac} to flatten assumptions. 
*)
val flatter_concl_rules:
    (Logic.info -> Logic.label -> Tactics.tactic) list
(** 
   The rules used by {!Boollib.flatten_tac} to flatten conclusions.
*)

val flatter_asms_tac: 
    ?info:Logic.info -> Logic.label -> Tactics.tactic
(**
   Eliminate operators in the assumptions which don't introduce new
   subgoals. Uses the same rules as {!Boollib.flatten_tac}.
*)
val flatter_concls_tac: 
    ?info:Logic.info -> Logic.label -> Tactics.tactic
(**
   Eliminate operators in the conclusions which don't introduce new
   subgoals. Uses the same rules as {!Boollib.flatten_tac}.
*)

val flatten_tac : 
    ?info:Logic.info -> ?f:Logic.label -> Tactics.tactic
(**
   Eliminate operators in the assumptions and conclusions which don't
   introduce new subgoals. Resulting tag information, in [?info], may
   contain duplicates.

   In the assumptions, eliminates [false], negation ([not]),
   conjunction ([&]) and existential quantification ([?]).

   In the conclusions, eliminates [true], negation ([not]),
   disjunction ([|]), implication ([=>]), universal quantification
   ([!]).
*)

(** 
   {7 Scatter subgoals}

   Split and flatten subgoals.
*)

val scatter_asm_rules:
    (Logic.info -> Logic.label -> Tactics.tactic) list
(** 
   Rules used by {!Boollib.scatter_tac} to scatter assumptions.
*)
val scatter_concl_rules:
    (Logic.info -> Logic.label -> Tactics.tactic) list
(** 
   Rules used by {!Boollib.scatter_tac} to scatter conclusions.
*)

val scatter_tac : 
    ?info:Logic.info -> ?f:Logic.label -> Tactics.tactic
(**
   Eliminate boolean operators in the assumptions and conclusions. 

   In the assumptions, eliminates [false], negation ([not]),
   conjunction ([&]) and existential quantification ([?]), disjunction
   ([|]), implication ([=>]).

   In the conclusions, eliminates [true], negation ([not]),
   disjunction ([|]), implication ([=>]), universal quantification
   ([!]), conjunction ([&]) and boolean equivalence ([iff]).

   Resulting tag information, in [?info], may contain duplicates.
*)

val blast_asm_rules:
    (Logic.info -> Logic.label -> Tactics.tactic) list
(** 
   Rules used by {!Boollib.blast_tac}.
*)
val blast_concl_rules:
    (Logic.info -> Logic.label -> Tactics.tactic) list
(** 
   Rules used by {!Boollib.blast_tac}.
*)

val blast_tac : 
    ?info:Logic.info -> ?f:Logic.label -> Tactics.tactic
(**
   Eliminate boolean operators in the assumptions and conclusions then
   try to solve subgoals. 

   In the assumptions, eliminates [false], negation ([not]),
   conjunction ([&]) and existential quantification ([?]), disjunction
   ([|]), implication ([=>]) then calls {!Tactics.basic}.

   In the conclusions, eliminates [true], negation ([not]),
   disjunction ([|]), implication ([=>]), universal quantification
   ([!]), conjunction ([&]) and boolean equivalence ([iff]) then calls
   {!Tactics.basic}.

   This is like {!Boollib.scatter_tac}, followed by {!Tactics.basic}.
*)

(** {5 Cases} *)

val cases_thm: unit -> Logic.thm
(** [cases_thm]: |- !P: (~P) | P *)

val cases_tac: ?info:Logic.info -> Basic.term -> Tactics.tactic
(**
   [cases_tac ?info x g]: Cases tactic.

   Add formula [x] to assumptions of [g] and
   create new subgoal in which to prove [x].

   {L
   g:\[asms |- concls\]

   ---> 

   g1:\[asms |- x{_ t}, concls\]; g2:\[x{_ t}, asms |- concls\]
   }

   info: [goals = [g1; g2], aforms=[t], cforms=[t], terms = []]
 *)

val show_tac: 
    ?info:Logic.info
  -> Basic.term -> Tactics.tactic -> Tactics.tactic
(**
   [show_tac trm tac]: Use [tac] to show that [trm] is true,
   introducing [trm] as a new assumption. If [tac] fails to prove
   [trm], introduces [trm] as the conclusion of a new subgoal.
*)

val show: 
    ?info:Logic.info
  -> Basic.term -> Tactics.tactic -> Tactics.tactic
(**
   [show trm tac]: Use [tac] to show that [trm] is true,
   introducing [trm] as a new assumption. If [tac] fails to prove
   [trm], introduces [trm] as the conclusion of a new subgoal.

   {!Boollib.show} is a synonym for {!Boollib.show_tac}.
*)


val cases_of: 
    ?info:Logic.info 
  -> ?thm:Logic.thm -> Basic.term 
    -> Tactics.tactic
(**
   [cases_of ?info ?thm trm]: Try to introduce a case split based on
   the type of term [trm]. If [thm] is given, it is used as the cases
   theorem. If [thm] is not given, the theorem named ["T_cases"] is
   used, where [T] is the name of the type of [trm].
*)


(** {5 Modus Ponens} *)

val mp_tac: 
    ?info:Logic.info
    -> ?a:Logic.label -> ?a1:Logic.label -> Tactics.tactic
(**
   [mp_tac ?a ?a1]: Modus ponens.

   {L
   g:\[(A=>B){_ a}, A{_ a1}, asms |- concls\]

   ---> 

   g:\[B{_ t}, A{_ a1}, asms |- concls\]
   }

   info: [goals = [], aforms=[t], cforms=[], terms = []]

   If [a] is [! x1 .. xn: A => B] and [a1] is [l], try to instantiate
   all of the [x1 .. xn] with values from [a1] (found by
   unification). 

   If [?a] is not given, the first (possibly quantified) implication
   in the assumptions is used. If [?a1] is not given, the assumptions
   are searched for a suitable formula.
*)

val cut_mp_tac:
    ?info:Logic.info 
  -> ?inst:Basic.term list
    -> Logic.thm 
      -> ?a:Logic.label -> Tactics.tactic

(**
   [cut_mp_tac ?info ?inst ?a ?a1]: Cut theorem for Modus ponens.

   {L
   g:\[A{_ a}, asms |- concls\]; thm: |- A => B

   ---> 

   g:\[B{_ t}, A{_ a}, asms |- concls\]
   }

   info: [goals = [], aforms=[t], cforms=[], terms = []]

   If [inst] is given, instantiate [thm] with the given terms.

   If [thm] is [! x1 .. xn: A => B] and [a1] is [l], try to instantiate
   all of the [x1 .. xn] with values from [a] (found by
   unification). 

   If [?a] is not given, the first (possibly quantified) implication
   in the assumptions is used. If [?a1] is not given, the assumptions
   are searched for a suitable formula.
*)

val back_tac: 
    ?info:Logic.info 
  -> ?a:Logic.label -> ?c:Logic.label -> Tactics.tactic
(**
   [back_tac ~a ~c]: Match, backward tactic.

   {L
   g:\[(A=>B){_ a}, asms |- B{_ c}, concls\]

   ---> 

   g1:\[asms |- A{_ t}, concls\]
   }

   info: [goals = [g1], aforms=[], cforms=[t], terms = []]

   If [a] is [! x1 .. xn: A => B] and [a1] is [l], try to instantiate
   all of the [x1 .. xn] with values from [c] (found by
   unification). 

   If [a] is not given, the first (possibly quantified) implication
   in the assumptions is used. If [c] is not given, the assumptions
   are searched for a suitable formula.
*)

val cut_back_tac:
    ?info:Logic.info -> ?inst:Basic.term list 
      -> Logic.thm -> ?c:Logic.label -> Tactics.tactic
(**
   [cut_back_tac ?inst thm ~c]: Match, backward tactic.

   {L
   g:\[asms |- B{_ c}, concls\]; thm: |- A => B

   ---> 

   g1:\[asms |- A{_ t}, concls\]
   }

   info: [goals = [g1], aforms=[], cforms=[t], terms = []]

   If [inst] is given, instantiate [thm] with the given terms.

   If [thm] is [! x1 .. xn: A => B] and [a1] is [l], try to instantiate
   all of the [x1 .. xn] with values from [c] (found by
   unification). 

   If [c] is not given, the assumptions are searched for a suitable
   formula.
 *)

(** {5 Theorems, Rules and Conversions} *)

module Thms:
    sig
(** 
   Theorems used by tactics. A theorem [n] is accessed by calling
   function [n_thm()]. This returns the theorem, proving it if
   necessary. If [n_thm()] has to prove the theorem, it stores the
   result so that subsequent calls to [n_thm()] do not have to carry
   out the proof again. Some theorems have a function [make_n_thm()]
   which actually carries out the proof. 
 *)

(**
   [iff_equals_thm]:  |- !x y: (x iff y) = (x = y)
 *)

      val make_iff_equals_thm : unit -> Logic.thm
      val iff_equals_thm : unit -> Logic.thm

(**
   [equals_iff_thm]:  |- !x y: (x = y) = (x iff y)
 *)

      val equals_iff_thm : unit -> Logic.thm


(**
   [bool_eq_thm]: |- !x y: x iff y = ((x => y) and (y=>x))
 *)

      val make_bool_eq_thm : unit -> Logic.thm
      val bool_eq_thm : unit -> Logic.thm

(**
   [double_not_thm]: |- ! x: x = (not (not x))
 *)

      val make_double_not_thm : unit -> Logic.thm
      val double_not_thm : unit -> Logic.thm

(**
   [rule_true_thm]:  |- !x: x = (x=true) 
 *)

      val make_rule_true_thm : unit -> Logic.thm
      val rule_true_thm : unit -> Logic.thm

(**
   rule_false_thm: !x: (not x) = (x=false)
 *)

      val make_rule_false_thm : unit -> Logic.thm
      val rule_false_thm : unit -> Logic.thm

(**
   eq_sym_thm: !x y: (x = y) = (y = x)
 *)

      val make_eq_sym_thm : unit -> Logic.thm
      val eq_sym_thm : unit -> Logic.thm

    end

module Rules:
    sig
(** 
   Functions to construct theorems from other theorems. 
*)

      val once_rewrite_rule :
	  Scope.t -> Logic.thm list -> Logic.thm -> Logic.thm
(** 
   [once_rewrite_rule scp rules thm]: Rewrite [thm] with [rules] once.
*)

      val conjunctL : Scope.t -> Logic.thm -> Logic.thm 
(**
   [conjunctL scp thm]: Get the left hand side of conjunct [thm].
   [conjunctL scp << l and r >> = l]
*)

      val conjunctR : Scope.t -> Logic.thm -> Logic.thm 
(**
   [conjunctR scp thm]: Get the right hand side of conjunct [thm].
   [conjunctL scp << l and r >> = r]
*)

      val conjuncts : Scope.t -> Logic.thm -> Logic.thm list 
(**
   [conjuncts scp thm]: Break theorem [thm] into the list of conjuncts.
   [conjuncts scp << f1 and f2 and .. and fn>> = [f1; f2; ..; fn]]
 *)

      val conv_rule :
	  Scope.t ->
	    (Scope.t -> Basic.term -> Logic.thm) -> Logic.thm -> Logic.thm
(** 
   [conv_rule scp conv thm]: Apply conversion [conv] to theorem [thm]
 *)
    end

module Convs:
    sig
(** 
   Conversions on boolean operators.
*)

(** [neg_all_conv]: |- (not (!x..y: a)) = ?x..y: not a *)
      val neg_all_conv: Scope.t -> Basic.term -> Logic.thm

(** [neg_exists_conv]: |- (not (?x..y: a)) = !x..y: not a *)
      val neg_exists_conv: Scope.t -> Basic.term -> Logic.thm

    end

(** {5 More tactics} *)

val equals_tac: ?info:Logic.info -> ?f:Logic.label -> Tactics.tactic
(** Convert boolean equality to iff *)


(****** Retired

(** {7 Miscellaneous unification functions} *)

val unify_formula_for_consts:
    Gtypes.substitution
  -> Scope.t
    -> (Basic.binders list * Basic.term) 
      -> Basic.term -> Basic.term list
(**
   [unify_formula_for_consts scp trm f]: Unify [trm] with formula [f]
   returning the list of terms needed to make [trm] alpha-equal to [f]
   by instantiating the topmost quantifiers of [trm].

   raise Not_found, if no unifiable formula found.
 *)

val unify_concl_for_consts:
    Basic.quant_ty
  -> ?c:Logic.label
    -> Basic.term -> Logic.node -> Basic.term list
(**
   [unify_concl_for_consts ?c trm g]: If [c] is given, unify [trm]
   with the conclusion labelled [c], returning the list of terms
   needed to make [trm] alpha-equals to the conclusion by
   instantiating the topmost quantifiers of trm.

   [trm] must be universally quantified.
 *)

val unify_asm_for_consts:
    Basic.quant_ty
  -> ?a:Logic.label
    -> Basic.term -> Logic.node -> Basic.term list
(**
   [unify_asm_for_consts ?a qnt trm g]: If [a] is given, unify [trm]
   with the assumption labelled [a], returning the list of terms
   needed to make [trm] alpha-equals to the conclusion by
   instantiating the topmost quantifiers of trm.

   [trm] must be quantified by [qnt].
 *)


******)

(** {5 Debugging} *)

val get_type_name: Basic.gtype -> Basic.ident

(*

val make_false_def: unit -> Logic.thm
val make_iff_def: unit -> Logic.thm
*)
