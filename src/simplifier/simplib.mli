(*-----
 Name: simplib.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(**
   [std_ss()]: The standard simpset
   [set_std_ss set]: Set the standard simpset to [set]
   [empty_simp()]: Clear the standard simpset.
   [add_simps thms]: Add [thms] to the standard simpset.
   [add_simp thm]: Add [thm] to the standard simpset.
 *)
val std_ss : unit -> Simpset.simpset 
val set_std_ss: Simpset.simpset -> unit
val empty_simp : unit -> unit
val add_simps : Logic.thm list -> unit
val add_simp : Logic.thm -> unit
val add_conv : Basic.term -> Logic.conv -> unit

(** [simp_tac ?f ?cntrl ?asms ?set ?with ?rules ?ignore goal]
   
   Simplifier tactic.

   If [f] is not given, repeat for each conclusion:
   - eliminate toplevel universal quantifiers of [f]
   - if (asms=true), put conclusions other than [f] into assumptions
   and make simp rules
   - if (asms=true), make simp rules from assumptions, other than [f]
   - simplify [f]: find (possibly conditional) rules for
   rewriting [f], rewrite [f], repeat until no change.

   When done, delete temporary assumptions

   Don't use formulas in [ignore] or for which [except] is true.

   Arguments:

   @param f The formula to simplify. Default: all conclusions.

   @param cntrl The rewrite control to use (used to select top-down or
   bottom up simplifying). Default: top-down.  

   @param asms Whether to use the assumptions and conclusions as
   rewrite rules. Default: [true].

   @params set The simpset to use. Default: [std_ss].

   @params use Add this simpset to the set specified with [set]. This
   allows extra simpsets to be used with the standard simpset.

   @param rules Additional rewrite rules to use. Default: [[]].

   @param ignore List of assumptions/conclusions to ignore. Default: [[]].

   @raise Simplifier.No_change If no change is made.
 *)
val simp_tac :
    ?f:Logic.label ->
      ?cntrl:Simplifier.control ->
	?asms:bool ->
	  ?set:Simpset.simpset ->
	    ?use:Simpset.simpset ->
	      ?rules:Logic.thm list ->
		?ignore:Logic.label list -> Tactics.tactic

(** 
   [once_simp_tac]: like [simp_tac] but only apply simplification
   once to each formula. 
 *)
val once_simp_tac :
    ?f:Logic.label ->
      ?cntrl:Simplifier.control ->
	?asms:bool ->
	  ?set:Simpset.simpset ->
	    ?use:Simpset.simpset ->
	      ?rules:Logic.thm list ->
		?ignore:Logic.label list -> Tactics.tactic

(* Printer *)

val print_set : Simpset.simpset -> unit

(** Initialising functions *)

(** [on_load thy] function to call when a theory is loaded *)

val on_load: Theory.contents -> unit

val init: unit -> unit

(* Debugging *)

val has_property: 'a -> 'a list -> bool
val thm_is_simp : ('a * Theory.thm_record) -> unit
val def_is_simp : ('a * Theory.id_record) -> unit
