(*----
  Name: simplib.ml
  Copyright M Wahab 2005-2014
  Author: M Wahab  <mwb.cde@gmail.com>

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

open Tactics

(** Simpset functions *)
let empty_simp = Simpset.empty_set
let add_simps = Simpset.simpset_add_thms 
let add_simp ctxt set thm = add_simps ctxt set [thm]

let add_conv set terms conv =
  let add_aux set trm = 
    let (vs, body) = Term.strip_qnt Basic.All trm
    in 
    Simpset.add_conv (vs, body) conv set
  in 
  List.fold_left add_aux set terms

(** Initial simpset *)
let init_std_ss () =
  add_conv (empty_simp()) [ << !x A: (%y: A) x >> ] 
    (fun ctxt -> Logic.Conv.beta_conv (Context.scope_of ctxt))

(** Global state *)

(*** Toplevel simplification tactics ***)

let simpC_tac 
    ?(cntrl = Formula.default_rr_control) ?(ignore = [])
    set ?add ?c rules ctxt goal =
  (** uset: The simpset to use. **)
  let sctxt = goal_context ctxt goal in 
  let uset = 
    let uset1 = 
      match add with
	| None -> set
        | Some s -> Simpset.join s set
    in 
    (** If there are rules, make a simpset from them. **)
    match rules with 
      | [] -> uset1
      | _ -> Simpset.simpset_add_thms sctxt uset1 rules
  in 
  (** ignore_tags: The tags of sequent formulas to be left alone. **)
  let ignore_tags = 
    let sqnt = Tactics.sequent goal in 
    List.map (fun l -> Logic.label_to_tag l sqnt) ignore
  in 
  (** simp_data: The simpset data. *)
  let simp_data = 
    let data1 = Simplifier.Data.set_exclude Simptacs.default_data ignore_tags
    in 
    Simplifier.Data.set_simpset
      (Simplifier.Data.set_control data1 cntrl)
      uset
  in 
  Simptacs.simpC_tac simp_data ?c ctxt goal

let simpC set ?c ctxt goal = simpC_tac set ?c [] ctxt goal

let simpA_tac 
    ?(cntrl = Formula.default_rr_control) ?(ignore = [])
    set ?add ?a rules ctxt goal =
  let sctxt = goal_context ctxt goal in 
  (** uset: The simpset to use. **)
  let uset = 
    let uset1 = 
      match add with
	| None -> set
        | Some s -> Simpset.join s set
    in 
    (** If there are rules, make a simpset from them. **)
    match rules with 
      | [] -> uset1
      | _ -> Simpset.simpset_add_thms sctxt uset1 rules
  in 
  (** ignore_tags: The tags of sequent formulas to be left alone. **)
  let ignore_tags = 
    let sqnt = Tactics.sequent goal 
    in 
    List.map (fun l -> Logic.label_to_tag l sqnt) ignore
  in 
  (** simp_data: The simpset data. *)
  let simp_data = 
    let data1 = Simplifier.Data.set_exclude Simptacs.default_data ignore_tags
    in 
    Simplifier.Data.set_simpset
      (Simplifier.Data.set_control data1 cntrl)
      uset
  in 
  Simptacs.simpA_tac simp_data ?a ctxt goal

let simpA set ?a ctxt goal = simpA_tac ?a set [] ctxt goal

let simp_all_tac 
    ?(cntrl = Formula.default_rr_control) ?(ignore = [])
    set ?add rules ctxt goal =
  let sctxt = goal_context ctxt goal in 
  (** uset: The simpset to use. **)
  let uset = 
    let uset1 = 
      match add with
	| None -> set
        | Some s -> Simpset.join s set
    in 
    (** If there are rules, make a simpset from them. **)
    match rules with 
      | [] -> uset1
      | _ -> Simpset.simpset_add_thms sctxt uset1 rules
  in 
  (** ignore_tags: The tags of sequent formulas to be left alone. **)
  let ignore_tags = 
    let sqnt = Tactics.sequent goal 
    in 
    List.map (fun l -> Logic.label_to_tag l sqnt) ignore
  in 
  (** simp_data: The simpset data. *)
  let simp_data = 
    let data1 = 
      Simplifier.Data.set_exclude Simptacs.default_data ignore_tags
    in 
    Simplifier.Data.set_simpset
      (Simplifier.Data.set_control data1 cntrl)
      uset
  in 
  Simptacs.full_simp_tac simp_data ctxt goal

let simp_all set ctxt goal = simp_all_tac set [] ctxt goal

let simp_tac 
    ?(cntrl = Formula.default_rr_control) ?(ignore = [])
    set ?add ?f rules (ctxt: Context.t) goal =
  let sqnt = Tactics.sequent goal in 
  let tac =
    match f with 
      | None ->
        simpC_tac ~cntrl:cntrl ~ignore:ignore set ?add rules
      | Some(x) -> 
	let tg = Logic.label_to_tag x sqnt in 
        begin
	  match Lib.try_find (get_tagged_concl (ftag tg)) goal with
	    | None -> 
	      simpA_tac ~cntrl:cntrl ~ignore:ignore 
                set ?add ~a:(ftag tg)rules
	    | _ -> 
	      simpC_tac ~cntrl:cntrl ~ignore:ignore 
                set ?add ~c:(ftag tg)rules
        end
  in 
  tac ctxt goal
    
let simp set ?f ctxt goal = simp_tac ?f set [] ctxt goal

(*** Initialising functions ***)

let has_property p ps = List.mem p ps

let thm_is_simp ctxt set (_, tr) =
  if has_property Theory.simp_property tr.Theory.props
  then add_simp ctxt set tr.Theory.thm
(*
    try add_simp ctxt set tr.Theory.thm
    with _ -> set
*)
  else set

let def_is_simp ctxt set (_, dr) =
  match dr.Theory.def with
    | None -> set
    | Some(thm) -> 
      if has_property Theory.simp_property dr.Theory.dprops
      then add_simp ctxt set thm 
(*
        try add_simp ctxt set thm 
        with _ -> set
*)
      else set
        
(** Function to call when a theory is loaded **)

let on_load (ctxt:Context.t) (set:Simpset.simpset) thy = 
  let set1 = List.fold_left (thm_is_simp ctxt) set thy.Theory.caxioms in 
  let set2 = List.fold_left (thm_is_simp ctxt) set1 thy.Theory.ctheorems in
  let set3 = List.fold_left (def_is_simp ctxt) set2 thy.Theory.cdefns in
  set3

