(*----
  Name: simplib.mli
  Copyright M Wahab 2005-2010
  Author: M Wahab  <mwb.cde@googlemail.com>

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

(** The simplifier libray. *)

(** {5 Simplification sets} *)

val empty_simp: unit -> Simpset.simpset
(** [empty_simp()]: Clear the standard simpset.
*)

val add_simps: 
  Context.t -> Simpset.simpset -> Logic.thm list
  -> Simpset.simpset
(** [add_simps thms]: Add [thms] to the standard simpset. *)

val add_simp: 
  Context.t -> Simpset.simpset -> Logic.thm
  -> Simpset.simpset
(** [add_simp thm]: Add [thm] to the standard simpset.
*)

val add_conv: 
  Simpset.simpset -> Basic.term list 
  -> (Context.t -> Logic.conv) -> Simpset.simpset
(** [add_conv trms conv]: Add conversion [conv] to the standard
    simpset, with [trms] as the representative keys.  Example:
    [add_conv [<< !x A: (%y: A) x >>] Logic.Conv.beta_conv] applies
    [beta_conv] on all terms matching [(%y: A) x].
*)

val init_std_ss: unit -> Simpset.simpset
(** The initial standard simpset *)
  

(****
(** Global state *)
module User :  
sig

val std_ss: unit -> Simpset.simpset 
(** [std_ss()]: The standard simpset
*)

val set_std_ss: Simpset.simpset -> unit
(** [set_std_ss set]: Set the standard simpset to [set]
*)
end
****)

(** {5 User level simplification tactics} *)

val simpA_tac:
  ?cntrl:Simplifier.control
  -> ?ignore:Logic.label list
  -> Simpset.simpset
  -> ?add:Simpset.simpset
  -> ?a:Logic.label
  -> Logic.thm list
  -> Tactics.tactic
(** [simpA_tac ?cntrl ?ignore ?asms ?set ?add ?a rules goal]
    
    Simplify assumptions.

    If [a] is not given then all assumptions are to be simplified.

    {ul
    {- Add all conclusions as simp rules.}
    {- Add all assumptions other than the targets as simp
    rules.}
    {- Simplify the assumption and then add it as a simp rule.
    Repeat for all assumptions to be simplified.}}

    Doesn't use formulas identified by a label in [ignore].

    @param a The assumption to simplify. Default: all assumptions.

    @param cntrl The rewrite control to use (used to select top-down or
    bottom up simplifying). Default: top-down.  

    @param ignore List of assumptions/conclusions to ignore. Default: [[]].

    @param set The simpset to use. Default: [std_ss].

    @param add Add this simpset to the set specified with [set]. This
    allows extra simpsets to be used with the standard simpset.

    @param rules Additional rewrite rules to use. 

    @raise No_change If no change is made.
*)

val simpA:
  Simpset.simpset 
  -> ?a:Logic.label
  -> Tactics.tactic
(** [simp ?a]: Shorthand for {!Simplib.simpA_tac}.
    
    @raise No_change If no change is made.
*)

val simpC_tac:
  ?cntrl:Simplifier.control
  -> ?ignore:Logic.label list
  -> Simpset.simpset
  -> ?add:Simpset.simpset
  -> ?c:Logic.label
  -> Logic.thm list
  -> Tactics.tactic
(** [simpC_tac ?cntrl ?ignore ?asms ?set ?add ?c rules goal]
    
    Simplify assumptions.

    If [c] is not given then all conclusions are to be simplified.

    {ul
    {- Add all assumptions as simp rules.}
    {- Add all conclusions other than the target conclusions as simp
    rules.}
    {- Simplify the conclusions and then add it as a simp rule.
    Repeat for all assumptions to be simplified.}}

    Doesn't use formulas identified by a label in [ignore].

    @param c The conclusion to simplify. Default: all conclusions

    @param cntrl The rewrite control to use (used to select top-down or
    bottom up simplifying). Default: top-down.  

    @param ignore List of assumptions/conclusions to ignore. Default: [[]].

    @param set The simpset to use. Default: [std_ss].

    @param add Add this simpset to the set specified with [set]. This
    allows extra simpsets to be used with the standard simpset.

    [rules] are the additional rewrite rules to use. 

    @raise No_change If no change is made.
*)

val simpC: 
  Simpset.simpset
  -> ?c:Logic.label ->  Tactics.tactic
(** [simp ?c]: Shorthand for {!Simplib.simpC_tac}.
    
    @raise No_change If no change is made.
*)

val simp_all_tac:
  ?cntrl:Simplifier.control
  -> ?ignore:Logic.label list
  -> Simpset.simpset
  -> ?add:Simpset.simpset
  -> Logic.thm list
  -> Tactics.tactic
(** [simp_all_tac ?cntrl ?ignore ?asms ?set ?add rules goal]
    
    Simplify each formula in the subgoal.

    {ul 
    {- Simplify each assumption, starting with the first (most recent),
    adding it to the simpset}
    {- Simplify each conclusion, starting with the last (least recent),
    adding it to the simpset.}}

    Don't use formulas identified by a label in [ignore].

    @param cntrl The rewrite control to use (used to select top-down or
    bottom up simplifying). Default: top-down.  

    @param ignore List of assumptions/conclusions to ignore. Default: [[]].

    @param set The simpset to use. Default: [std_ss].

    @param add Add this simpset to the set specified with [set]. This
    allows extra simpsets to be used with the standard simpset.

    @param rules Additional rewrite rules to use.

    @raise No_change If no change is made.
*)

val simp_all: Simpset.simpset -> Tactics.tactic
(** [simp_all]: Shorthand for {!Simplib.simp_all_tac}.
    
    @raise No_change If no change is made.
*)


val simp_tac:
  ?cntrl:Simplifier.control
  -> ?ignore:Logic.label list
  -> Simpset.simpset
  -> ?add:Simpset.simpset
  -> ?f:Logic.label
  -> Logic.thm list
  -> Tactics.tactic
(** [simp_tac ?cntrl ?ignore ?asms ?set ?add ?f rules goal]
    
    Simplifier tactic.

    If [f] is not given, simplify the the conclusions using
    {!Simplib.simpC_tac}.

    If [f] is given and is a conclusion then simplify using
    {!Simplib.simpC_tac} otherwise simplify using {!Simplib.simpA_tac}.

    Doesn't use formulas identified by a label in [ignore].

    @param f The formula to simplify. Default: all conclusions.

    @param cntrl The rewrite control to use (used to select top-down or
    bottom up simplifying). Default: top-down.  

    @param ignore List of assumptions/conclusions to ignore. Default: [[]].

    @param asms Whether to use the assumptions and conclusions as
    rewrite rules. Default: [true].

    @param set The simpset to use. Default: [std_ss].

    @param add Add this simpset to the set specified with [set]. This
    allows extra simpsets to be used with the standard simpset.

    @param rules Additional rewrite rules to use. 

    @raise No_change If no change is made.
*)


val simp: 
  Simpset.simpset -> ?f:Logic.label ->  Tactics.tactic
(** [simp ?f]: Shorthand for {!Simplib.simp_tac}.
    
    @raise No_change If no change is made.
*)


(** {5 Initialising functions} *)

val on_load: 
  Context.t -> Simpset.simpset -> Theory.contents -> Simpset.simpset
(** Function to call when a theory is loaded. *)

(**


val init: unit -> unit
(** Initialise the simplification library. Reset the standard simp set
    and add {!Simplib.on_load} to the functions called when a theory
    is loaded.
*)

(** {5 Printer} *)

val print_set: Simpset.simpset -> unit
(** Print a simp set. **)

(** {5 Debugging} *)
*)

val has_property: 'a -> 'a list -> bool
val thm_is_simp: 
  Context.t -> Simpset.simpset -> ('a * Theory.thm_record) -> Simpset.simpset
val def_is_simp: 
  Context.t -> Simpset.simpset -> ('a * Theory.id_record) -> Simpset.simpset
