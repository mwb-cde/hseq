(*----
 Name: simptacs.mli
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

(**
   Tactical interface to the simplifier engine. 
 *)

open Simplifier

(** {5 Simplification Data} *)

val default_data: Data.t
(** The default data set. *)

val add_rule_data:
    Data.t -> Simpconvs.rule_data list -> Data.t
(** 
   [add_rule_data data rules]: Update [data] with assumption
   [rules]. [rules] should be as provided by {!Simpconvs.prepare_asm}.
*)

(** {7 Adding assumptions and conclusions} *)

val add_asms_tac:
    Data.t option ref
    -> Tag.t list
      -> Tactics.tactic
(**
   [add_asms_tac data tags g]: Prepare the assumptions in [tags] for
   use as simp-rules. Add them to [data].
*)

val add_concls_tac:
    Data.t option ref
    -> Tag.t list
      -> Tactics.tactic
(**
   [add_concls_tac data tags g]: Prepare the conclusions in [tags] for
   use as simp-rules. Add them to [data].
*)


(** {5 Simplification engines} *)

val simp_engine_tac :
    Data.t 
  -> (Data.t option) ref
    -> Tag.t 
      -> Tactics.tactic
(** The engine for [simp_tac]. 

   [simp_engine_tac ret cntrl l goal]:

   {ul 
   {- Eliminate toplevel universal quantifiers of [l].}
   {- Simplify [l], using {!Simplifier.basic_simp_tac}}
   {- Solve trivial goals}
   {- Repeat until nothing works}}

   Returns the updated simp data in [ret].
*)

val simpA_engine_tac :
    Data.t 
    -> Data.t option ref 
      -> bool ref
	-> Logic.label -> Tactics.tactic
(**
   [simpA_engine_tac cntrl ret chng l goal]: Simplify assumption [l],
   returning the updated data in [ret]. Sets [chng] to true on
   success. Doesn't clean-up.
*) 

val simpC_engine_tac :
    Data.t 
    -> Data.t option ref 
      -> bool ref
      -> Logic.label -> Tactics.tactic
(**
   [simpC_engine_tac cntrl ret chng l goal]: Simplify conclusion [l],
   returning the updated data in [ret]. Sets [chng] to true on
   success. Doesn't clean-up.
*) 


(** {5 Simplifying assumptions} *)

val simpA0_tac :
    Data.t 
    -> Data.t option ref 
      -> ?a:Logic.label
      -> Tactics.tactic
(** 
   [simpA1_tac cntrl ret ?a goal]: Simplify assumptions

   If [a] is given, add other assumptions to the simpset and then
   simplify [a]. If [a] is not given, simplify each assumption,
   starting with the last, adding it to the simpset rules it is
   simplified. The conclusions are always added to the simpset.

   Doesn't clean-up.
*)

val simpA_tac :
    Data.t -> ?a:Logic.label -> Tactics.tactic
(** 
   [simpA_tac cntrl ?a goal]: Simplify assumptions

   If [a] is given, add other assumptions to the simpset and then
   simplify [a]. If [a] is not given, simplify each assumption,
   starting with the last, adding it to the simpset rules it is
   simplified. The conclusions are always added to the simpset.

   This is the top-level tactic (for this module) for simplifying
   assumptions.
*)


(** {5 Simplifying conclusions} *)

val simpC0_tac :
    Data.t 
    -> Data.t option ref 
      -> ?c:Logic.label
	-> Tactics.tactic
(** 
   [simpC1_tac cntrl ret ?c goal]: Simplify conclusions.

   If [c] is given, add other conclusions to the simpset and simplify
   [c]. Otherwise, simplify each conclusion, starting with the last,
   adding it to the assumptions after it is simplified. The
   assumptions are always added to the simpset.

   Doesn't clean-up.
*)

val simpC_tac :
    Data.t -> ?c:Logic.label -> Tactics.tactic
(** 
   [simpC_tac cntrl ?c goal]: Simplify conclusions.

   If [c] is given, add other conclusions to the simpset and simplify
   [c]. Otherwise, simplify each conclusion, starting with the last,
   adding it to the assumptions after it is simplified. The
   assumptions are always added to the simpset.

   This is the top-level tactic (for this module) for simplifying
   conclusions.
*)

(** {5 Simplifying subgoals} *)

val full_simp0_tac:
    Data.t 
    -> Data.t option ref 
      -> Tactics.tactic
(** 
   [full_simp0_tac cntrl ret goal]: Simplify subgoal

   {ul
   {- Simplify each assumption, starting with the last, adding it to the
   simpset rules after it is simplified.}
   {- Simplify each conclusion, starting with the last, adding it to the
   simpset rules after it is simplified.}}

   Doesn't clean-up.
*)
 
val full_simp_tac:
    Data.t -> Tactics.tactic
(** 
   [full_simp_tac cntrl ret goal]: Simplify subgoal

   {ul
   {- Simplify each assumption, starting with the last, adding it to the
   simpset rules after it is simplified.}
   {- Simplify each conclusion, starting with the last, adding it to the
   simpset rules after it is simplified.}}

   This is the top-level tactic (for this module) for simplifying
   subgoals.
*)
 

(** Debugging information **)

val log : string -> Data.t -> unit
