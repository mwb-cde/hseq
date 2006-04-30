(*-----
  Name: boolbase.mli
  Author: M Wahab <mwahab@users.sourceforge.net>
  Copyright M Wahab 2006
  ----*)

(** Basic boolean tactics *)

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

val cut_thm: 
    ?info:Logic.info -> ?inst:Basic.term list 
      -> string -> Tactics.tactic
(** Cut a named theorem, with optional instantiation. *)

(** {7 Basic equality reasoning} *)

val make_bool_cases_thm : unit -> Logic.thm
val bool_cases_thm_var : Logic.thm Lib.deferred
val bool_cases_thm : unit -> Logic.thm
(** [bool_cases_thm]: [! (x:bool): (x=true) | (x=false)] *)

val make_eq_refl_thm : unit -> Logic.thm
val eq_refl_thm_var : Logic.thm Lib.deferred
val eq_refl_thm : unit -> Logic.thm
(** [eql_refl]: [!x : (x = x)] *)

val make_eq_sym_thm : unit -> Logic.thm
val eq_sym_thm_var : Logic.thm Lib.deferred
val eq_sym_thm : unit -> Logic.thm
(** [eql_sym]: [!x y: (x = y) = (y = x) ] *)

val eq_sym_rule : Scope.t -> Logic.thm -> Logic.thm
(** 
   [eq_sym_rule scp thm]: If the body of [thm] is [ |- x = y], return 
   [ |- y=x ].
 *)

val eq_symA: ?info:Logic.info -> Logic.label -> Tactics.tactic
(** 
   [eq_symA a]: Rewrite assumption [a] with [eq_sym_thm] once.
 *)

val eq_symC: ?info:Logic.info -> Logic.label -> Tactics.tactic
(**
   [eq_symA a]: Rewrite conclusion [c] with [eq_sym_thm] once.
 *)

val eq_sym_tac: ?info:Logic.info -> Logic.label -> Tactics.tactic
(** 
   [eq_sym_tac f]: Try to apply [eq_symA f], if that fails, try [eq_symC f].
 *)

val eq_tac :  ?info:Logic.info -> ?c:Logic.label -> Tactics.tactic
(**
   Prove goals of the form \[A|- x=x{_ c}, C\].
   [info] is unchanged.
 *)

(**  {5 Eliminating boolean operators}  *)

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
   {!Boolbase.elim_rules_tac}.
 *)
