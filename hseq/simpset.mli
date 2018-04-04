(*----
  Name: simpset.mli
  Copyright Matthew Wahab 2005-2016
  Author: Matthew Wahab <mwb.cde@gmail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under
  the terms of the Lesser GNU General Public License as published by
  the Free Software Foundation; either version 3, or (at your option)
  any later version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
  License for more details.

  You should have received a copy of the Lesser GNU General Public
  License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
  ----*)

(** Simplification sets

    Simpsets store rewrite rules and conversions for simplifying
    formulas. Each simpset also holds an optional link to a simpset,
    allowing a chain of simpsets to be built up. Rules are always
    added to the first simpset in a chain. Rules are looked up in the
    order the simpsets appear in the chain: rules in the earlier
    simpsets are appear earlier in the list of returned rules.
*)

(** {7 Utility functions} *)

val lt_var:
  (Basic.term -> bool) -> (Basic.term -> bool)
  -> Basic.term -> Basic.term -> bool
(** [lt_var x y xvarp yvarp]: Less-than ordering of terms for use with
    Net.insert. Makes variables (for which [xvarp] or [yvarp] is true)
    larger than any other term.
*)

val get_rr_order: Logic.rr_type -> Rewrite.order
(** [get_rr_order rl]: Get the ordering of rule [rl]. Fail if [rl] is
    not ordered.
*)

val set_rr_order: Logic.rr_type -> Rewrite.order -> Logic.rr_type
(** [set_rr_order rl order]: Set the ordering of rule [rl] to order.
*)

(** {5 Rules} *)

(** [rule]: A simpset rule.  Made up of variables, optional condition,
    lhs , rhs, source of the rule (theorem or assumption).
*)
type rule =
    Basic.binders list
     * Basic.term option * Basic.term * Basic.term
     * Rewrite.order option
     * Logic.rr_type

val dest_rule:
  rule ->
  (Basic.binders list
   * Basic.term option * Basic.term * Basic.term
   * Rewrite.order option
   * Logic.rr_type)
(** Destructor for rules. *)

val rule_binders: rule -> Basic.binders list
(** Get the rules variables. *)
val rule_cond: rule -> Basic.term option
(** Get the condition. *)
val rule_lhs: rule -> Basic.term
(** Get the left-hand-side. *)
val rule_rhs: rule -> Basic.term
(** Get the right-hand-side.*)
val rule_order: rule -> Rewrite.order option
(** Get the ordering.*)
val rule_src: rule -> Logic.rr_type
(** Get the source of the rule. *)

val termnet_lt: rule -> rule -> bool
(** [termnet_lt x y]: Less-than ordering of terms for use with
    Net.insert. Makes variables larger than any other term, making
    terms with variables larger than terms without.
*)

val dest_rr_rule:
  Basic.term ->
  (Basic.binders list
   * Basic.term option * Basic.term * Basic.term
   * Rewrite.order option)
(** [dest_rr_rule trm]: Split term [trm] into binders, condition, lhs,
    rhs. Rules must be of the form: [c=>(l=r)] or [l=r].
*)

val make_rule: Logic.rr_type -> Basic.term -> rule
(** [make_rule src trm]: Make a rule from [trm], store as key for
    [src].  Term [trm] is assumed to be extracted from [src].
*)

val make_asm_rules:
  (Logic.tagged_form  -> bool)
  -> Logic.tagged_form list
  -> rule list
(** [make_asm_rules except forms]: Make a list of simp rules from the
    tagged formulas [forms], excluding those for which [except] is
    true. The list [forms] is assumed to be a list of assumptions.
*)

(** {5 Sets} *)

(** Simpsets *)
type simpset =
    {
      convs: (Context.t -> Logic.conv) Net.net;
      (** Conversions **)
      basic: rule Net.net;
      (** Rewrite-rules. *)
      next: simpset option;
    (** The next simpset in the list. *)
    }

val empty_set: unit -> simpset
(** [empty_set()]: Make an empty simpset. *)

val join: simpset -> simpset -> simpset
(** [join s t]: Join sets s and t together.  In set [join s t], set
    [s] will be searched before set [t]
*)

val split: simpset -> simpset * simpset
(** [split s]: Split simpset [s] into two parts.  Fails if [s] is not
    joined to another set.
*)

val add_rule: rule -> simpset -> simpset
(** [add_rule rl set]: Add rule [rl= c=>(l=r)] to [set]. If rule could
    lead to looping ({!Simputils.equal_upto_vars} [l r] is true) then
    make rule an ordered rewrite (using {!Term.term_lt}).
*)

val add_conv:
  (Basic.binders list * Basic.term)
  -> (Context.t -> Logic.conv) -> simpset -> simpset
(** [add_conv (vars, key) conv s]: Add conversion [conv] to set [s],
    indexed by terms of [key] in which [vars] are the list of unifiable
    variables.
*)

(** {7 Look-up functions} *)

val lookup_conv:
  Context.t -> simpset -> Basic.term -> rule list -> rule list
(** [lookup_conv scp set trm lst]: Look up [trm] in the conversions of
    [set]. Add new rules to [lst], in the order they are found. Raise
    [Not_found] on failure.
*)

val lookup_all:
  Context.t -> simpset -> Basic.term -> rule list -> rule list
(** [lookup_all scp set trm lst]: Lookup trm in [set], adding list of
    matches to list. First the conversions of [set] are searched then
    the rewrite rules then the next set in the chain (if any). The
    rules are added, in the order they are found, to [lst].
*)

val lookup:
  Context.t -> simpset -> Basic.term -> rule list
(** [lookup trm set]: find list of possible matches for term [trm] in
    [set].  First the conversions of [set] are searched then the
    rewrite rules then the next set in the chain (if any). The rules
    are returned in the order they are found.
*)

(** {5 Adding rules to a simpset}

    Use {!Simpset.simpset_add_thm} or {!Simpset.simpset_add_thms} to
    add a theorem to a simpset.

    Use {!Simpset.simpset_add_asm_rule} to add an assumption to a
    simpset as a simplification rule.

    Use {!Simpset.add_conv} to add a conversion to a simpset.
*)

val simpset_add_rules:
  simpset -> rule list -> simpset
(** [simpset_add_rules set rules]: Add [rules] to simpset [set]. *)

val simpset_add_thm:
  Context.t -> simpset -> Logic.thm -> simpset
(** [simpset_add_thm scp set thms]: Add theorem [thm] to simpset
    [set].
*)

val simpset_add_thms:
  Context.t -> simpset -> Logic.thm list -> simpset
(** [simpset_add_thms scp set thms]: Add theorems [thms] to simpset
    [set].
*)

val simpset_add_asm_rule:
  simpset -> Logic.ftag_ty -> Logic.node -> simpset
(** [simpset_add_asm_rule scp asm node thms]: Add assumption [asm] of
    [node] to simpset [set] as a simplification rule.
*)

val add_context: simpset -> Basic.term -> simpset
(** [add_context set trm]: Add [trm] as a new assumption in which
    decision procedures operate. This currently does nothing, in the
    future it will be used to notify conversions of new context.  *)

(** {5 Printers} *)

val print_rule: Printers.ppinfo -> rule -> unit
(** Printer for simp rules. *)

val print_rule_net: Printers.ppinfo -> rule Net.net -> unit
(** Printer for rule nets. *)

val print: Printers.ppinfo -> simpset -> unit
(** Printer for simp sets. *)

(** {5 Debugging information} *)

val make_thm_rule: Logic.thm -> rule
val thm_to_entries: Context.t -> Logic.thm -> rule list
val make_asm_rule: Logic.tagged_form -> rule
