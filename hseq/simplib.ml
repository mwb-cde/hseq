(*----
  Copyright (c) 2005-2024 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

open Tactics

let term = BoolPP.read

(** Simpset functions *)
let empty_simp = Simpset.empty_set
let add_simps = Simpset.simpset_add_thms
let add_simp ctxt set thm = add_simps ctxt set [thm]

let add_conv set terms conv =
  let add_aux set trm =
    let (vs, body) = Term.strip_qnt Term.All trm
    in
    Simpset.add_conv (vs, body) conv set
  in
  List.fold_left add_aux set terms

(** Initial simpset *)
let init_std_ss () =
  let trm =
    let type_a = Gtype.mk_vartype (Gtype.mk_gtype_id "a") in
    let type_b = Gtype.mk_vartype (Gtype.mk_gtype_id "b") in
    let type_c = Gtype.mk_vartype (Gtype.mk_gtype_id "c") in
    let binder_x = Term.Binder.make Term.All "x" type_a in
    let binder_A = Term.Binder.make Term.All "A" type_b in
    let binder_y = Term.Binder.make Term.Lambda "y" type_c in
    let open Term in
    (* term "!x A: (%y: A) x" *)
    mk_qnt binder_x
      (mk_qnt binder_A
         (mk_app
            (mk_qnt binder_y (mk_bound binder_A))
            (mk_bound binder_x)))
  in
  add_conv (empty_simp()) [ trm ]
    (fun ctxt -> Logic.Conv.beta_conv (Context.scope_of ctxt))

(** Global state *)

(*** Toplevel simplification tactics ***)

let gen_simpC_tac
      arg_cntrl arg_ignore arg_cncl
      set rules ctxt goal =
  (** uset: The simpset to use. **)
  let cntrl = Lib.from_option arg_cntrl Rewrite.default
  and ignore_cncls = Lib.from_option arg_ignore []
  in
  let sctxt = goal_context ctxt goal in
  let uset =
    if rules = []
    then set
    else Simpset.simpset_add_thms sctxt set rules
  in
  (** ignore_tags: The tags of sequent formulas to be left alone. **)
  let ignore_tags =
    let sqnt = Tactics.sequent goal in
    List.map (fun l -> Logic.label_to_tag l sqnt) ignore_cncls
  in
  (** simp_data: The simpset data. *)
  let simp_data =
    let data1 = Simplifier.Data.set_exclude Simptacs.default_data ignore_tags
    in
    Simplifier.Data.set_simpset
      (Simplifier.Data.set_control data1 cntrl)
      uset
  in
  Simptacs.simpC_tac simp_data arg_cncl ctxt goal

let simpC_tac set rules = gen_simpC_tac None None None set rules
let simpC_at_tac set rules c = gen_simpC_tac None None (Some(c)) set rules

let simpC set ctxt goal = simpC_tac set [] ctxt goal
let simpC_at set c ctxt goal = simpC_at_tac set [] c ctxt goal

let gen_simpA_tac arg_cntrl arg_ignore arg_a set rules ctxt goal =
  let cntrl = Lib.from_option arg_cntrl Rewrite.default
  and ignore_asms = Lib.from_option arg_ignore []
  in
  let sctxt = goal_context ctxt goal in
  (** uset: The simpset to use. **)
  let uset =
    (** If there are rules, make a simpset from them. **)
    if rules = []
    then set
    else Simpset.simpset_add_thms sctxt set rules
  in
  (** ignore_tags: The tags of sequent formulas to be left alone. **)
  let ignore_tags =
    let sqnt = Tactics.sequent goal
    in
    List.map (fun l -> Logic.label_to_tag l sqnt) ignore_asms
  in
  (** simp_data: The simpset data. *)
  let simp_data =
    let data1 = Simplifier.Data.set_exclude Simptacs.default_data ignore_tags
    in
    Simplifier.Data.set_simpset
      (Simplifier.Data.set_control data1 cntrl)
      uset
  in
  Simptacs.simpA_tac simp_data arg_a ctxt goal

let simpA_tac set rules =
  gen_simpA_tac None None None set rules
let simpA_at_tac set rules a =
  gen_simpA_tac None None (Some(a)) set rules

let simpA set = simpA_tac set []
let simpA_at set a = simpA_at_tac set [] a

let gen_simp_all_tac arg_cntrl arg_ignore set rules ctxt goal =
  let cntrl = Lib.from_option arg_cntrl Rewrite.default
  and ignore_asms = Lib.from_option arg_ignore []
  in
  let sctxt = goal_context ctxt goal in
  (** uset: The simpset to use. **)
  let uset =
    if rules = []
    then set
    else Simpset.simpset_add_thms sctxt set rules
  in
  (** ignore_tags: The tags of sequent formulas to be left alone. **)
  let ignore_tags =
    let sqnt = Tactics.sequent goal
    in
    List.map (fun l -> Logic.label_to_tag l sqnt) ignore_asms
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

let simp_all_tac set rules ctxt goal =
  gen_simp_all_tac None None set rules ctxt goal

let simp_all set ctxt goal = gen_simp_all_tac None None set [] ctxt goal

let gen_simp_tac arg_cntrl arg_ignore arg_f set rules ctxt goal =
  let cntrl = Lib.from_option arg_cntrl Rewrite.default
  and ignore_asms = Lib.from_option arg_ignore []
  in
  let sqnt = Tactics.sequent goal in
  let tac =
    match arg_f with
      | Some(x) ->
        let tg = Logic.label_to_tag x sqnt in
        begin
          match Lib.try_find (get_tagged_concl (ftag tg)) goal with
            | None ->
              gen_simpA_tac (Some(cntrl)) (Some(ignore_asms)) (Some(ftag tg))
                set rules
            | _ ->
              gen_simpC_tac (Some(cntrl)) (Some(ignore_asms)) (Some(ftag tg))
                set rules
        end
      | _ ->
        gen_simpC_tac (Some(cntrl)) (Some(ignore_asms)) None set rules
  in
  tac ctxt goal

let simp_tac set rules ctxt goal =
  gen_simp_tac None None None set rules ctxt goal
let simp_at_tac set rules f ctxt goal =
  gen_simp_tac None None (Some(f)) set rules ctxt goal

let simp set ctxt goal = simp_tac set [] ctxt goal
let simp_at set f ctxt goal = simp_at_tac set [] f ctxt goal

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
