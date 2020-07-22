(*----
  Copyright (c) 20062021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** Basic boolean tactics *)

val make_false_def: Context.t -> Logic.thm
val false_def: Context.t -> Logic.thm
(** Get the definition of [false]. *)

val falseA_at: Logic.label -> Tactics.tactic
(** Solve a goal of the form \[ false{_ a}, A |- C \].
*)

val falseA: Tactics.tactic
(** Solve a goal of the form \[ false{_ a}, A |- C \]. Search for [false] in the
    assumptions and then applies [falseA_at] *)

val trivial_at: Logic.label -> Tactics.tactic
(** Solve a goal of the form \[ false{_ f}, A |- C \] or \[ A |-
    true{_ f}, C \].
*)

val trivial: Tactics.tactic
(** Search for assumptions and conclusions which satisfy [trivial_at] *)

val cut_thm: Term.term list -> string -> Tactics.tactic
(** Cut a named theorem, with optional instantiation. *)

(** {7 Basic equality reasoning} *)

val bool_cases_id: Ident.t
val make_bool_cases_thm: Context.t -> Logic.thm
val bool_cases_thm: Context.t -> Logic.thm
(** [bool_cases_thm]: [! (x:bool): (x = true) | (x = false)]. *)

val eq_refl_thm_id: Ident.t
val make_eq_refl_thm: Context.t -> Logic.thm
val eq_refl_thm: Context.t -> Logic.thm
(** [eql_refl]: [!x: (x = x)]. *)

val eq_sym_thm_id: Ident.t
val make_eq_sym_thm: Context.t -> Logic.thm
(* val eq_sym_thm_var: Logic.thm Lib.deferred *)
val eq_sym_thm: Context.t -> Logic.thm
(** [eql_sym]: [!x y: (x = y) = (y = x) ]. *)

val eq_sym_rule: Context.t -> Logic.thm -> Logic.thm
(** [eq_sym_rule scp thm]: If the body of [thm] is [ |- x = y], return
    [ |- y=x ].
*)

val eq_symA: Logic.label -> Tactics.tactic
(** [eq_symA a]: Rewrite assumption [a] with [eq_sym_thm] once.
*)

val eq_symC: Logic.label -> Tactics.tactic
(** [eq_symA a]: Rewrite conclusion [c] with [eq_sym_thm] once.
*)

val eq_sym_tac: Logic.label -> Tactics.tactic
(** [eq_sym_tac f]: Try to apply [eq_symA f], if that fails, try
    [eq_symC f].
*)

val eq_tac: Tactics.tactic
(** Prove goals of the form \[A|- x = x{_ c}, C\]. *)

val eq_at: Logic.label -> Tactics.tactic
(** Apply [eq_tac} at a specific conclusion *)

(**  {5 Eliminating boolean operators}  *)

val direct_alt:
  'a -> ('a -> Tactics.tactic) list
  -> Tactics.tactic
(** [direct_alt tacs info l]: Directed alt. Like {!Tactics.alt} but
    pass [info] and [l] to each tactic in [tacs].  **)

val direct_map_some:
  ('a -> Tactics.tactic)
  -> 'a list
  -> ('a list)Tactics.data_tactic
(** [direct_map_some tac lst]: Directed map_some. Like
    {!Tactics.map_some} but pass and [l] to [tac]. **)


val asm_elim_rules_tac:
  ((Logic.label -> Tactics.tactic) list
   * (Logic.label -> Tactics.tactic) list)
  -> Logic.label
  -> Tactics.tactic
(** [asm_elim_rules (arules, crules) f goal]: Apply elimination
    rules to assumption [f] and to all resulting assumptions and
    conclusions. Assumptions are eliminated with [arules], conclusions
    with [crules].
*)

val concl_elim_rules_tac:
  ((Logic.label -> Tactics.tactic) list
   * (Logic.label -> Tactics.tactic) list)
  -> Logic.label
  -> Tactics.tactic
(** [concl_elim_rules  (arules, crules) f goal]: Apply
    elimination rules to conclusion [f] and to all resulting
    assumptions and conclusions. Assumptions are eliminated with
    [arules], conclusions with [crules].
*)

val plain_concl_elim_rules_tac:
  (Logic.label -> Tactics.tactic) list
  -> Logic.label list
  -> Tactics.tactic
val base_concl_elim_rules_tac:
  ((Logic.label -> Tactics.tactic) list
   * (Logic.label -> Tactics.tactic) list)
  -> Logic.label list
  -> Tactics.tactic

val plain_asm_elim_rules_tac:
  (Logic.label -> Tactics.tactic) list
  -> Logic.label list
  -> Tactics.tactic
val base_asm_elim_rules_tac:
  ((Logic.label -> Tactics.tactic) list
   * (Logic.label -> Tactics.tactic) list)
  -> Logic.label list
  -> Tactics.tactic


val elim_rules_tac:
  ((Logic.label -> Tactics.tactic) list
      * (Logic.label -> Tactics.tactic) list)
  -> Logic.label list -> Logic.label list
  -> Tactics.tactic
(** [elim_rules_tac (arules, crules) albls clbls]: Apply elimination
    rules to all assumptions with a label in [albls] and all
    conclusions with a label in [clbls] and with to all resulting
    assumptions and conclusions. The tag of any new formula for which
    the elimination rules fails is stored in arbitrary order and may
    contain duplicates.
*)

val apply_elim_tac:
  (Logic.label list -> Logic.label list -> Tactics.tactic)
  -> (Logic.label)option -> Tactics.tactic
(** [apply_elim_tac tac]: Apply elimination tactic
    [tac] to all formulas in the sequent. The tag
    of any new formula for which the elimination
    rules fails is stored in arbitrary order and
    may contain duplicates).

[apply_elim_tac] is intended to be used to wrap {!Boolbase.elim_rules_tac}.
 *)
