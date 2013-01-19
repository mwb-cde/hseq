(*----
  Name: boolbase.ml
  Copyright M Wahab 2006-2010
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

open Boolutil
open Commands
open Tactics
open Lib.Ops

(*** Basic Tactics ***)

let false_id = Ident.mk_long Lterm.base_thy "false_def"
let make_false_def sctxt = 
  thm (context_of sctxt) (Lterm.base_thy ^ "false_def")
let false_def sctxt = 
  Context.find_thm sctxt false_id make_false_def

let falseA ctxt ?a goal =
  let sctxt = scoped ctxt (scope_of goal) in
  let af = first_asm_label a Formula.is_false goal in 
  let th =
    try false_def sctxt
    with Not_found -> 
      raise (Report.error 
	       ("Tactics.Rewriter.falseA: "
	        ^"Can't find needed theorem false_def: |- false = not true"))
  in 
  let plan = Rewrite.mk_node [Rewrite.mk_rules [Logic.RRThm(th)]] in 
  seq
    [ 
      pure_rewriteA plan af;
      (?> fun inf g ->
	let atag = Lib.get_one (Info.aformulas inf) (Tactics.error "falseA")
	in 
	seq
	  [ 
	    negA ~a:(ftag atag);
	    (?> fun inf g1  -> 
	      let ctag = Lib.get_one (Info.cformulas inf) (error "falseA")
	      in 
	      trueC ~c:(ftag ctag) g1)
	  ] g)
    ] goal

let trivial ctxt ?f g =  
  try (trueC ?c:f // falseA ctxt ?a:f) g
  with _ -> raise (error "trivial")

let cut_thm ctxt ?inst str = cut ?inst (thm ctxt str)

(*** Basic equality reasoning ***)

let eq_refl_thm_id = Ident.mk_long Lterm.base_thy "eq_refl"
let make_eq_refl_thm sctxt = 
  try thm (context_of sctxt) (Ident.string_of eq_refl_thm_id)
  with Not_found ->
    raise (error 
	     ("Tactics.Rewriter.make_eq_refl_thm:"
	      ^"Can't find needed axiom eq_refl: |- !x: (x = x)"))
let eq_refl_thm  = make_eq_refl_thm

let bool_cases_id = Ident.mk_long Lterm.base_thy "bool_cases"
let make_bool_cases_thm sctxt = 
  try thm (context_of sctxt) (Ident.string_of bool_cases_id)
  with Not_found ->
    raise (error 
	     ("Tactics.Rewriter.make_bool_cases_thm:"
	      ^"Can't find needed axiom bool_cases: "
	      ^"|- !x: (x = true) | (x=false)"))

let bool_cases_thm = make_bool_cases_thm

let eq_sym_thm_id = Ident.mk_long "Bool" "eq_sym"
let make_eq_sym_thm sctxt = 
  let ctxt = context_of sctxt in
  match Lib.try_app 
    (thm ctxt) (Ident.string_of eq_sym_thm_id) 
  with
    | Some(th) -> th
    | None -> 
      begin
        let eq_l1 =
	  prove sctxt << !x y : (x = y) => (y = x) >>
	  ((repeat allC) ++ implC
	   ++ substC [!~1] (!! 1) 
	   ++ cut ~inst:[ << _y >> ] (eq_refl_thm sctxt) ++ basic)
        in 
        let eq_l2 =
	  prove sctxt << !x y : ((x => y) & (y => x)) => (x = y)>>
	  ((repeat allC)
	   ++ cut ~inst:[ << _x >>] (bool_cases_thm sctxt) ++ disjA
	   ++ cut ~inst:[ << _y >>] (bool_cases_thm sctxt) ++ disjA
	   ++ substC [ !~ 1; !~ 2] (!! 1) ++ implC
	   --
	     [
	       cut ~inst:[ << true >> ] (eq_refl_thm sctxt) ++ basic ;
	       conjA ++ implA ++ (trivial ctxt);
	       conjA ++ implA ++ implA ++ (trivial ctxt);
	       cut ~inst:[ << false >> ] (eq_refl_thm sctxt) ++ basic
	     ])
        in 
        prove sctxt << !x y : (x = y) = (y = x)>>
            ((repeat allC)
	     ++ cut ~inst:[ << _x = _y >> ; << _y = _x >>] eq_l2
	     ++ implA 
	     --
	       [ 
	         conjC
	         -- 
	           [
	             cut ~inst:[ << _x >> ; << _y >> ] eq_l1 ++ basic ;
	             cut ~inst:[ << _y >> ; << _x >> ] eq_l1 ++ basic 
	           ] ;
	         basic
	       ])
      end

let eq_sym_thm sctxt = 
  Context.find_thm sctxt eq_sym_thm_id make_eq_sym_thm

let eq_sym_rule sctxt thm = 
  let scp = Context.scope_of sctxt in
  let ctrl = {Formula.default_rr_control with Rewrite.depth = Some 1} in 
  let term = Logic.term_of thm in 
  let plan = 
    Tactics.mk_thm_plan scp ~ctrl:ctrl 
      [ Logic.RRThm (eq_sym_thm sctxt) ] term
  in 
  Tactics.pure_rewrite_rule plan scp thm

let eq_symA ctxt a goal =
  let ctrl = {Formula.default_rr_control with Rewrite.depth = Some 1} in 
  let (atag, form) = get_tagged_asm a goal in 
  let term = Formula.term_of form in 
  let sctxt = scoped ctxt (scope_of goal) in
  let plan = 
    Tactics.mk_plan ~ctrl:ctrl goal [ Logic.RRThm (eq_sym_thm sctxt) ] term
  in 
  Tactics.pure_rewriteA plan (ftag atag) goal

let eq_symC ctxt c goal =
  let ctrl = {Formula.default_rr_control with Rewrite.depth = Some 1} in 
  let (ctag, form) = (get_tagged_concl c goal) in 
  let term = Formula.term_of form in 
  let sctxt = scoped ctxt (scope_of goal) in
  let plan = 
    Tactics.mk_plan ~ctrl:ctrl goal [ Logic.RRThm (eq_sym_thm sctxt) ] term
  in 
  Tactics.pure_rewriteC plan (ftag ctag) goal
    
let eq_sym_tac f goal = 
  try eq_symA f goal
  with Not_found -> eq_symC f goal

let eq_tac ctxt ?c goal = 
  let th = 
    try thm ctxt (Lterm.base_thy ^ ".eq_refl")
    with Not_found -> 
      (raise (error ("eq_tac: Can't find required lemma "
		     ^Lterm.base_thy^".eq_refl")))
  in 
  let cforms = concls_of (sequent goal) in 
  let tac albl (t, f) g = 
    if Formula.is_equality f
    then unify_tac ~a:albl ~c:(ftag t) g
    else fail g
  in 
  seq 
    [
      Tactics.cut th; 
      (?> fun info1 g ->
	let af = get_one ~msg:"eq_tac" (Info.aformulas info1)
	in 
	map_first (tac (ftag af)) cforms g)
    ] goal


(*** Eliminating boolean operators ***)

(** [direct_alt lbl tacs]: Directed alt. Like {!Tactics.alt} but
    pass [lbl] to each tactic in [tacs].  **)
let direct_alt lbl tacl goal =
  let rec alt_aux ts g = 
    match ts with
      | [] -> raise (Failure "direct_alt: no successful tactic")
      | tac::rest ->
	(try tac lbl g with _ -> alt_aux rest g)
  in alt_aux tacl goal

let direct_map_some tac lst goal =
  let app (flag, fail_list) lbl node =
    try 
      let branch1 = tac lbl node
      in
      ((true, fail_list), branch1)
    with _ -> ((flag, lbl::fail_list), skip node)
  in
  match lst with
    | [] -> ([], fail ~err:(error "direct_map_some: no data.") goal)
    | _ ->
      let ((flag, fail_list), branch) = 
        fold_data app (false, []) lst goal
      in
      if not flag
      then (fail_list, 
            fail ~err:(error "direct_map_some: no tactic suceeded") goal)
      else (fail_list, branch)

(** [asm_elim_rules (arules, crules) f goal]: Apply elimination rules
    to assumption [f] and to all resulting assumptions and
    conclusions. Assumptions are eliminated with [arules], conclusions
    with [crules]. Any new tag which can't be eliminated is recorded
    (in arbitrary order).
*)
let rec asm_elim_rules_tac rules lbl goal = 
  base_asm_elim_rules_tac rules [lbl] goal
(** [concl_elim_rules (arules, crules) f goal]: Apply elimination
    rules to conclusion [f] and to all resulting assumptions and
    conclusions. Assumptions are eliminated with [arules], conclusions
    with [crules]. The tag of any new formula for which the elimination
    rules fails is stored in arbitrary order.  *)
and concl_elim_rules_tac rules lbl goal = 
  base_concl_elim_rules_tac rules [lbl] goal
and formulas inf = (List.map ftag (Info.aformulas inf), 
                    List.map ftag (Info.cformulas inf))
and plain_asm_elim_rules_tac arules lbl_list goal = 
  (* Try to apply one of the rules, making an empty change record on
     failure. *)
  let try_arule_tac flist lbl g = 
    try ((true, flist), direct_alt lbl arules g)
    with _ -> 
      let sqnt = sequent g in
      ((false, (Logic.label_to_tag lbl sqnt)::flist),
       set_changes_tac (Changes.empty()) g)
  in
  (* Iterate through the labels, try to apply one of the rules. New
     assumptions are added to list of labels to eliminate. *)
  let rec asm_tac (flag, flist, chngs) lbls g =
    match lbls with
      | [] -> 
        if not flag
        then fail ~err:(error "asm_elim_rules_tac: No tactic suceeded.") g
        else 
          let chngs1 = (Changes.make (Info.subgoals chngs)
                          flist (Info.cformulas chngs) (Info.constants chngs))
          in
          set_changes_tac chngs1 g
      | lbl::rest ->
        apply_tac (try_arule_tac flist lbl)
          (fun (flag1, flist1) -> (?> fun inf2 g2 -> 
            let albls = List.map ftag (Info.aformulas inf2) 
            and chngs1 = 
              Changes.rev_append
                (Changes.make 
                   (Info.subgoals inf2)
                   [] (Info.cformulas inf2) 
                   (Info.constants inf2))
                chngs
            in
            let albls1 = List.rev_append albls rest 
            in
            asm_tac ((flag1 or flag), flist1, chngs1) albls1 g2)) g
  in
  asm_tac (false, [], Changes.empty()) lbl_list goal
(* Iterate through the labels trying to apply one of the rules then
   eliminate any resulting conclusions. *)
and base_asm_elim_rules_tac rules lbl_list goal = 
  let (arules, _) = rules in
  seq [
    plain_asm_elim_rules_tac arules lbl_list ;
    (?> fun info g ->
      (* Extract failing assumptions and eliminate new
         conclusions. *)
      let concls = List.map ftag (Info.cformulas info) 
      and asm_fails = Info.aformulas info 
      in
      seq [
        (base_concl_elim_rules_tac rules concls // skip);
        (* Form final change record. *)
        (?> fun info1 g1 ->
          let chngs = Changes.make (Info.subgoals info1)
            (List.rev_append asm_fails (Info.aformulas info1))
            (Info.cformulas info1) (Info.constants info1)
          in
          set_changes_tac chngs g1)
      ] g)
  ] goal

(* Iterate through the labels trying to apply one of the rules then
   eliminate any resulting assumptions. *)
and plain_concl_elim_rules_tac crules lbl_list goal = 
  (* Try to apply one of the rules, making an empty change record on
     failure. *)
  let try_crule_tac flist lbl g = 
    try ((true, flist), direct_alt lbl crules g)
    with _ -> 
      let sqnt = sequent g in
      ((false, (Logic.label_to_tag lbl sqnt)::flist), 
       set_changes_tac (Changes.empty()) g)
  in
  (* Iterate through the labels, try to apply one of the rules. New
     conclusions are added to list of labels to eliminate. *)
  let rec concl_tac (flag, flist, chngs) lbls g =
    match lbls with
      | [] -> 
        if not flag
        then fail ~err:(error "concl_elim_rules_tac: No tactic suceeded.") g
        else
          let chngs1 = Changes.make (Info.subgoals chngs)
            (Info.aformulas chngs) flist (Info.constants chngs) 
          in
          set_changes_tac chngs1 g
      | lbl::rest ->
        apply_tac (try_crule_tac flist lbl)
          (fun (flag1, flist1) -> (?> fun inf2 g2 -> 
            let clbls = List.map ftag (Info.cformulas inf2) 
            and chngs1 = 
              Changes.rev_append 
                (Changes.make (Info.subgoals inf2)
                   (Info.aformulas inf2) [] (Info.constants inf2))
                chngs
            in
            let clbls1 = List.rev_append clbls rest 
            in
            concl_tac (flag1 or flag, flist1, chngs1) clbls1 g2)) g
  in
  concl_tac (false, [], Changes.empty()) lbl_list goal
and base_concl_elim_rules_tac rules lbl_list goal = 
  let (_, crules) = rules in
  seq [
    plain_concl_elim_rules_tac crules lbl_list ;
    (?> fun info g ->
      (* Extract failing conclusions and eliminate new assumptions. *)
      let asms = List.map ftag (Info.aformulas info) 
      and concl_fails = Info.cformulas info 
      in
      seq [
        (base_asm_elim_rules_tac rules asms // skip);
        (* Form final change record. *)
        (?> fun info1 g1 ->
          let chngs = Changes.make (Info.subgoals info1)
            (Info.aformulas info1)
            (List.rev_append concl_fails (Info.cformulas info1))
            (Info.constants info1)
          in
          set_changes_tac chngs g1)
      ] g)
  ] goal

(** [elim_rules_tac (arules, crules) albls clbls]: Apply elimination
    rules to all assumptions with a label in [albls] and all
    conclusions with a label in [clbls] and to all resulting
    assumptions and conclusions. The tag of any new formula for which
    the elimination rules fails is stored in arbitrary order.
*)
let elim_rules_tac rules albls clbls g =
  if (albls != [])
  then
    apply_tac
      (try_tac (map_some (asm_elim_rules_tac rules) albls))
      (fun good g1->
        try (map_some (concl_elim_rules_tac rules) clbls) g1
        with _ -> 
          if good 
          then skip g1 
          else (fail ~err:(error "elim_rules_tac") g1)) g
  else
    try map_some (concl_elim_rules_tac rules) clbls g
    with _ -> fail ~err:(error "elim_rules_tac") g

(** [apply_elim_tac tac ?f]: Apply elimination tactic [tac] to
    formula [?f]. If [?f] is not given, use all formulas in the
    sequent. The tag of any new formula for which the elimination rules
    fails is recorded in arbitrary order.

    [apply_elim_tac] is a wrapper for [elim_rules_tac].
*)
let apply_elim_tac tac ?f goal =
  let sqnt = sequent goal in 
  let alst, clst = 
    match f with 
      | None -> 
        let get_formula_ftag x = ftag (drop_formula x) 
        in
	(List.map get_formula_ftag (asms_of sqnt), 
	 List.map get_formula_ftag (concls_of sqnt))
      | Some(x) ->
        begin
	  match Lib.try_find (get_asm x) goal with
	    | None -> ([], [x])
	    | _ -> ([x], [])
        end
  in 
  tac alst clst goal
