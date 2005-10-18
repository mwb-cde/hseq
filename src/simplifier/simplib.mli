(*-----
 Name: simplib.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(**
   The simplifier libray.
*)

(** {5 The standard simplification set} *)

val std_ss : unit -> Simpset.simpset 
(**
   [std_ss()]: The standard simpset
*)

val set_std_ss: Simpset.simpset -> unit
(**
   [set_std_ss set]: Set the standard simpset to [set]
*)

val empty_simp : unit -> unit
(**
   [empty_simp()]: Clear the standard simpset.
*)

val add_simps : Logic.thm list -> unit
(**
   [add_simps thms]: Add [thms] to the standard simpset.
*)
val add_simp : Logic.thm -> unit
(**
   [add_simp thm]: Add [thm] to the standard simpset.
 *)

val add_conv : Basic.term list -> Logic.conv -> unit
(**
   [add_conv trms conv]: Add conversion [conv] to the standard simpset,
   with [trms] as the representative keys.  Example: [add_conv [<< !x A:
   (%y: A) x >>] Logic.Conv.beta_conv] applies [beta_conv] on all terms
   matching [(%y: A) x].
 *)

(** {5 User level simplification tactics} *)

(** [simp_tac ?f ?cntrl ?ignore ?asms ?set ?add ~rules goal]
   
   Simplifier tactic.

   If [f] is not given, repeat for each conclusion:
   {ul
   {- Eliminate toplevel universal quantifiers of [f].}
   {- If [asms=true], put conclusions other than [f] into assumptions
   and make simp rules.}
   {- if [asms=true], make simp rules from assumptions, other than [f].}
   {- Simplify [f]: find (possibly conditional) rules for
   rewriting [f], rewrite [f], repeat until no change.}}
   Don't use formulas identified by a label in [ignore].

   @param f The formula to simplify. Default: all conclusions.

   @param cntrl The rewrite control to use (used to select top-down or
   bottom up simplifying). Default: top-down.  

   @param ignore List of assumptions/conclusions to ignore. Default: [[]].

   @param asms Whether to use the assumptions and conclusions as
   rewrite rules. Default: [true].

   @param set The simpset to use. Default: [std_ss].

   @param add Add this simpset to the set specified with [set]. This
   allows extra simpsets to be used with the standard simpset.

   @param rules Additional rewrite rules to use. Default: [[]].

   @raise No_change If no change is made.
 *)
val simp_tac :
  ?f:Logic.label ->
  ?cntrl:Simplifier.control ->
  ?ignore:Logic.label list -> 
  ?asms:bool ->
  ?set:Simpset.simpset ->
  ?add:Simpset.simpset ->
  ?rules:Logic.thm list ->
  Tactics.tactic


(** {5 Initialising functions} *)

val on_load: Theory.contents -> unit
(** Function to call when a theory is loaded *)

val init: unit -> unit
(** 
   Initialise the simplification library. Reset the standard simp set
   and add {!Simplib.on_load} to the functions called when a theory is
   loaded.
*)

(** {5 Printer} *)

val print_set : Simpset.simpset -> unit
(** Print a simp set. **)


(** {5 Debugging} *)

val has_property: 'a -> 'a list -> bool
val thm_is_simp : ('a * Theory.thm_record) -> unit
val def_is_simp : ('a * Theory.id_record) -> unit
