(*----
  Name: rewritelib.ml
  Copyright Matthew Wahab 2006-2019
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

open Boolutil
open Boolbase
open Commands
open Tactics
open Lib.Ops

(*** Generalised Rewriting ***)

module Rewriter =
struct

  (** [gen_rewrite_conv scp ctrl rules trm]: rewrite term [trm] with rules
      [rrl] in scope [scp].

      Returns |- trm = X where [X] is the result of rewriting [trm]
  *)
  let gen_rewrite_conv ctxt ctrl (rls: Logic.rr_type list) term =
    let is_rl = ctrl.Rewrite.rr_dir = rightleft in
    let mapper f x =
      match x with
        | Logic.RRThm(t) -> Logic.RRThm(f t)
        | Logic.ORRThm(t, o) -> Logic.ORRThm(f t, o)
        | _ ->
          raise (error "rewrite_conv: Invalid assumption rewrite rule")
    in
    let rules =
      if is_rl
      then List.map (mapper (eq_sym_rule ctxt)) rls
      else rls
    in
    let plan = Tactics.mk_thm_plan ctxt ctrl rules term
    in
    Tactics.pure_rewrite_conv plan ctxt term

  (** [gen_rewrite_rule scp ctrl rules thm: rewrite theorem [thm] with rules
      [rrl] in scope [scp].

      Returns |- X where [X] is the result of rewriting [thm]
  *)
  let gen_rewrite_rule ctxt ctrl rls thm =
    let is_rl = ctrl.Rewrite.rr_dir=rightleft in
    let mapper f x =
      match x with
        | Logic.RRThm t -> Logic.RRThm(f t)
        | Logic.ORRThm (t, o) -> Logic.ORRThm(f t, o)
        | _ ->
          raise (error "rewrite_rule: Invalid assumption rewrite rule")
    in
    let rules =
      if is_rl
      then List.map (mapper (eq_sym_rule ctxt)) rls
      else rls
    in
    let plan = Tactics.mk_thm_plan ctxt ctrl rules (Logic.term_of thm)
    in
    Tactics.pure_rewrite_rule plan ctxt thm

  (** [map_sym_tac ret rules goal]: Apply [eq_sym] to each rule in
      [rules], returning the resulting list in [ret]. The list in
      [ret] will be in reverse order of [rules].  *)
  let map_sym_tac (rules: Tactics.rule list) ctxt goal =
    let asm_fn l ctxt0 g =
      try
        let g2 = eq_symA l ctxt0 g in
        let nl = Lib.get_one (Info.aformulas (Info.branch_changes g2))
          (error "Rewriter.map_sym_tac: Invalid assumption")
        in
        (ftag nl, g2)
      with err -> raise (add_error "Rewriter.map_sym_tac" err)
    in
    let fn_tac r ctxt0 g =
      let sctxt = set_scope ctxt0 (scope_of_goal g) in
      match r with
        | Logic.RRThm(th) ->
          (Logic.RRThm(eq_sym_rule sctxt th), skip sctxt g)
        | Logic.ORRThm(th, o) ->
          (Logic.ORRThm(eq_sym_rule sctxt th, o), skip sctxt g)
        | Logic.Asm(l) ->
          let (nl, ng) = asm_fn l sctxt g in
          (Logic.Asm(nl), ng)
        | Logic.OAsm(l, o) ->
          let (nl, ng) = asm_fn l sctxt g in
          (Logic.OAsm(nl, o), ng)
    in
    let mapping lst rl ctxt0 g =
      let (nr, g2) = fn_tac rl ctxt0 g in
      (nr::lst, g2)
    in
    fold_data mapping [] rules ctxt goal

  let gen_rewriteA_tac ctrl rules albl ctxt goal =
    let (atag, aform) = get_tagged_asm albl goal in
    let aterm = Formula.term_of aform in
    let is_lr = not (ctrl.Rewrite.rr_dir = rightleft) in
    let tac1 ctxt0 g =
      if is_lr
      then (rules, pass ctxt0 g)
      else
        fold_seq rules
          [
            map_sym_tac;
            (fun x ctxt1 g1 -> (List.rev x, pass ctxt1 g1))
          ] ctxt0 g
    in
    let tac2 rls ctxt0 g =
      let plan = Tactics.mk_plan ctrl g rls aterm
      in
      Tactics.pure_rewriteA plan (ftag atag) ctxt0 g
    in
    try apply_tac tac1 tac2 ctxt goal
    with err -> raise (add_error "Rewriter.rewriteA_tac" err)

  let gen_rewriteC_tac ctrl rules clbl ctxt goal =
    let (ctag, cform) = get_tagged_concl clbl goal in
    let cterm = Formula.term_of cform in
    let is_lr = (ctrl.Rewrite.rr_dir = leftright) in
    let tac1 ctxt0 g =
      if is_lr
      then (rules, pass ctxt0 g)
      else
        fold_seq rules
          [
            map_sym_tac;
            (fun x ctxt1 g1 -> (List.rev x, pass ctxt1 g1))
          ] ctxt0 g
    in
    let tac2 rls ctxt0 g =
      let plan = Tactics.mk_plan ctrl g rls cterm
      in
      Tactics.pure_rewriteC plan (ftag ctag) ctxt0 g
    in
    try apply_tac tac1 tac2 ctxt goal
    with err -> raise (add_error "Rewriter.rewriteC_tac" err)

  (** [gen_rewrite_tac ctrl rules l sq]: Rewrite formula [l] with
      [rules].

      If [l] is in the conclusions then call [rewriteC_tac] otherwise
      call [rewriteA_tac].  *)
  let gen_rewrite_tac ctrl rls f ctxt g =
    try
      begin
        try gen_rewriteA_tac ctrl rls f ctxt g
        with Not_found -> gen_rewriteC_tac ctrl rls f ctxt g
      end
    with err -> raise (add_error "Rewriter.rewrite_tac" err)

end

let gen_rewrite_conv ctxt ctrl rls trm =
  Rewriter.gen_rewrite_conv ctxt ctrl
    (List.map (fun x -> Logic.RRThm(x)) rls) trm

let rewrite_conv ctxt rls trm =
  gen_rewrite_conv ctxt Rewrite.default rls trm

let gen_rewrite_rule ctxt ctrl rls thm =
  Rewriter.gen_rewrite_rule ctxt ctrl
    (List.map (fun x -> Logic.RRThm(x)) rls) thm

let rewrite_rule ctxt rls thm =
  gen_rewrite_rule ctxt Rewrite.default rls thm

let gen_rewrite_asm_tac ctrl f rules ctxt goal =
  (* Rewrite the assumptions and record the changes *)
  let rewrite_asm l ct g =
    (?> (fun inf1 ->
         (Rewriter.gen_rewriteA_tac ctrl rules l
          ++ append_changes_tac inf1))) ct g
  in
  if f = None
  then foreach_asm rewrite_asm ctxt goal
  else
    Rewriter.gen_rewrite_tac ctrl rules (Lib.from_some f) ctxt goal

let gen_rewrite_concl_tac ctrl f rules ctxt goal =
  (* Rewrite the conclusions and record the changes *)
  let rewrite_concl l ct g =
    (?> (fun inf1 ->
         (Rewriter.gen_rewriteC_tac ctrl rules l
          ++ append_changes_tac inf1))) ct g
  in
  if f = None
  then foreach_concl rewrite_concl ctxt goal
  else
    Rewriter.gen_rewrite_tac ctrl rules (Lib.from_some f) ctxt goal

let gen_rewrite_tac ctrl f rules ctxt goal =
  (* Rewrite the assumptions and record the changes *)
  let rewrite_asm l ct g =
    (?> (fun inf1 ->
         (Rewriter.gen_rewriteA_tac ctrl rules l
          ++ append_changes_tac inf1))) ct g
  (* Rewrite the concluions and record the changes *)
  and rewrite_concl l ct g =
    (?> (fun inf1 ->
         (Rewriter.gen_rewriteC_tac ctrl rules l
          ++ append_changes_tac inf1))) ct g
  in
  match f with
    | None ->
       seq_some
         [
           foreach_asm rewrite_asm;
           foreach_concl rewrite_concl;
         ] ctxt goal
    | Some (x) ->
      Rewriter.gen_rewrite_tac ctrl rules x ctxt goal

let rewrite_tac ths ctxt goal =
  let ctrl = Rewrite.default in
  let rules = List.map (fun x -> Logic.RRThm x) ths
  in
  gen_rewrite_tac ctrl None rules ctxt goal

let rewrite_at ths f ctxt goal =
  let ctrl = Rewrite.default in
  let rules = List.map (fun x -> Logic.RRThm x) ths
  in
  gen_rewrite_tac ctrl (Some(f)) rules ctxt goal

let once_rewrite_tac ths ctxt goal =
  let ctrl =
    Rewrite.control leftright Rewrite.TopDown (Some 1)
  in
  let rules = List.map (fun x -> Logic.RRThm x) ths
  in
  gen_rewrite_tac ctrl None rules ctxt goal

let once_rewrite_at ths f ctxt goal =
  let ctrl =
    Rewrite.control leftright Rewrite.TopDown (Some 1)
  in
  let rules = List.map (fun x -> Logic.RRThm x) ths
  in
  gen_rewrite_tac ctrl (Some(f)) rules ctxt goal

let rewriteA_tac ths ctxt goal =
  let ctrl = Rewrite.default in
  let rules = List.map (fun x -> Logic.RRThm x) ths
  in
  gen_rewrite_asm_tac ctrl None rules ctxt goal

let rewriteA_at ths a ctxt goal =
  let ctrl = Rewrite.default in
  let rules = List.map (fun x -> Logic.RRThm x) ths
  in
  gen_rewrite_asm_tac ctrl (Some(a)) rules ctxt goal

let once_rewriteA_tac ths ctxt goal =
  let ctrl =
    Rewrite.control leftright Rewrite.TopDown (Some 1)
  in
  let rules = List.map (fun x -> Logic.RRThm x) ths
  in
  gen_rewrite_asm_tac ctrl None rules ctxt goal

let once_rewriteA_at ths a ctxt goal =
  let ctrl =
    Rewrite.control leftright Rewrite.TopDown (Some 1)
  in
  let rules = List.map (fun x -> Logic.RRThm x) ths
  in
  gen_rewrite_asm_tac ctrl (Some(a)) rules ctxt goal

let rewriteC_tac ths ctxt goal =
  let ctrl = Rewrite.default in
  let rules = List.map (fun x -> Logic.RRThm x) ths
  in
  gen_rewrite_concl_tac ctrl None rules ctxt goal

let rewriteC_at ths c ctxt goal =
  let ctrl = Rewrite.default in
  let rules = List.map (fun x -> Logic.RRThm x) ths
  in
  gen_rewrite_concl_tac ctrl (Some(c)) rules ctxt goal

let once_rewriteC_tac ths ctxt goal =
  let ctrl =
    Rewrite.control leftright Rewrite.TopDown (Some 1)
  in
  let rules = List.map (fun x -> Logic.RRThm x) ths
  in
  gen_rewrite_concl_tac ctrl None rules  ctxt goal

let once_rewriteC_at ths c ctxt goal =
  let ctrl =
    Rewrite.control leftright Rewrite.TopDown (Some 1)
  in
  let rules = List.map (fun x -> Logic.RRThm x) ths
  in
  gen_rewrite_concl_tac ctrl (Some(c)) rules  ctxt goal

let gen_replace_tac ctrl asms f ctxt goal =
  let sqnt = sequent goal in
  (*** ttag: The tag of tag of the target (if given) ***)
  let ttag =
    match f with
      | None -> None
      | Some(x) -> Some(Logic.label_to_tag x sqnt)
  in
  (*** exclude: a predicate to filter the rewriting target ***)
  let exclude =
    match ttag with
      | None -> (fun _ -> false)
      | Some(x) -> (Tag.equal x)
  in
  (*** find_equality_asms: Find the assumptions which are equalities ***)
  let rec find_equality_asms sqasms rst =
    match sqasms with
      | [] -> List.rev rst
      | form::xs ->
        let tg = drop_formula form
        in
        if (not (exclude tg))  &&
             (qnt_opt_of Term.All (Lterm.is_equality)
                (Formula.term_of (drop_tag form)))
        then find_equality_asms xs (tg::rst)
        else find_equality_asms xs rst
  in
  (*** asm_tags: The assumptions to use for rewriting. ***)
  let asm_tags =
    match asms with
    | [] -> find_equality_asms (Logic.Sequent.asms sqnt) []
    | _ -> List.map (fun x -> Logic.label_to_tag x sqnt) asms
  in
  (*** rules: Assumption labels in rewriting form ***)
  let rules = List.map (fun x -> Logic.Asm (ftag x)) asm_tags
  in
  (*** filter_replace: The replacment tactics, filtering the target to
       avoid trying to rewrite a formula with itself.  ***)
  let filter_replace x =
    if List.exists (Tag.equal (Logic.label_to_tag x sqnt)) asm_tags
    then fail (error "gen_replace")
    else gen_rewrite_tac ctrl (Some(x)) rules
  in
  (*** tac: apply filter_replace to an identified formula or to all
       formulas in the sequent.  ***)
  let tac =
    match ttag with
      | None -> foreach_form filter_replace
      | Some(x) -> filter_replace (ftag x)
  in
  alt
    [
      tac;
      fail (error "gen_replace")
    ] ctxt goal

let replace_tac asms ctxt goal =
  gen_replace_tac Rewrite.default asms None ctxt goal

let replace_at asms f ctxt goal =
  gen_replace_tac Rewrite.default asms (Some(f)) ctxt goal

let replace_rl_tac asms ctxt goal =
  gen_replace_tac {Rewrite.default with rr_dir = rightleft}
    asms None ctxt goal

let replace_rl_at asms f ctxt goal =
  gen_replace_tac {Rewrite.default with rr_dir = rightleft}
    asms (Some(f)) ctxt goal

let once_replace_tac asms ctxt goal =
  let ctrl =
    Rewrite.control leftright Rewrite.TopDown (Some 1)
  in
  gen_replace_tac ctrl asms None ctxt goal

let once_replace_at asms f ctxt goal =
  let ctrl =
    Rewrite.control leftright Rewrite.TopDown (Some 1)
  in
  gen_replace_tac ctrl asms (Some(f)) ctxt goal

let unfold_at str f ctxt g =
  match Lib.try_find (defn ctxt) str with
    | None -> raise (error ("unfold_at: Can't find definition of "^str))
    | Some th -> rewrite_at [th] f ctxt g

let unfold str ctxt g =
  match Lib.try_find (defn ctxt) str with
    | None -> raise (error ("unfold: Can't find definition of "^str))
    | Some th -> rewrite_tac [th] ctxt g
