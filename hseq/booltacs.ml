(*----
  Name: booltacs.ml
  Copyright Matthew Wahab 2006-2016
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
open Rewritelib
open Commands
open Tactics
open Lib.Ops

(** General support for boolean reasoning *)

let term = BoolPP.read

(*** Boolean equivalence ***)

let iff_def_id = Lterm.iffid
let make_iff_def sctxt =
  defn sctxt (Ident.string_of Lterm.iffid)
let iff_def sctxt =
  Context.find_thm sctxt iff_def_id make_iff_def

(** [iffA l sq]: Elminate the equivalance at assumptin [l]

    {L
    g:\[(A iff B){_ l}, asms |- concl]
    ---->
    g:[(A => B){_ l1}, (B => A){_ l2}, asms |- concl];
    }

    info: [goals = [], aforms=[l1; l2], cforms=[], terms = []]
*)
let iffA ?a ctxt goal =
  let af = first_asm_label a is_iff goal in
  let sqnt = Tactics.sequent goal in
  let (t, f) =
    Logic.Sequent.get_tagged_asm (Logic.label_to_tag af sqnt) sqnt
  in
  if not (is_iff f)
  then raise (error "iffA")
  else
    let sctxt = set_scope ctxt (scope_of_goal goal) in
    seq
      [
        rewrite_tac [iff_def sctxt] ~f:(ftag t);
        Tactics.conjA ~a:(ftag t);
      ] sctxt goal

(** [iffC l sq]: Elminate the equivalence at conclusion [l]

    {L
    g:\[asms |- (A iff B){_ l}, concl]
    ---->
    g1:\[asms |- (A => B){_ l}, concl]
    g2:\[asms |- (B => A){_ l}, concl]
    }

    info: [goals = [g1; g2], aforms=[], cforms=[l], terms = []]
**)

let iffC ?c ctxt goal =
  let cf = first_concl_label c is_iff goal in
  let sqnt=sequent goal in
  let (t, f) =
    Logic.Sequent.get_tagged_cncl (Logic.label_to_tag cf sqnt) sqnt
  in
  if not (is_iff f)
  then raise (error "iffC")
  else
    let sctxt = set_scope ctxt (scope_of_goal goal) in
    seq
      [
        rewrite_tac [iff_def sctxt] ~f:(ftag t);
        Tactics.conjC ~c:(ftag t);
      ] sctxt goal

(** [iffE l sq]: Fully elminate the equivalence at conclusion [l]

    {L
    g:\[asms |- (A iff B){_ l}, concl]
    ---->
    g1:[A{_ l1}, asms |- B{_ l2}, concl];
    g2:[B{_ l3}, asms |- A{_ l4}, concl];
    }

    info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
**)
let iffE ?c ctxt goal =
  let sqnt = sequent goal
  and cf = first_concl_label c is_iff goal
  in
  let (t, f) =
    Logic.Sequent.get_tagged_cncl (Logic.label_to_tag cf sqnt) sqnt
  in
  if not (is_iff f)
  then raise (error "iffE")
  else
    let tac ctxt0 g =
      let sctxt = set_scope ctxt0 (scope_of_goal g) in
      (rewrite_tac [iff_def sctxt] ~f:(ftag t) ++
        (?> (fun inf1 ->
          Tactics.conjC ~c:(ftag t) ++
            Tactics.implC ~c:(ftag t) ++
              (?> (fun inf2 ->
                set_changes_tac
                  (Changes.make (Info.subgoals inf1)
                     (Info.aformulas inf2)
                     (Info.cformulas inf2) [])))))) sctxt g
    in
    alt [ tac; fail ~err:(error "iffE") ] ctxt goal

(*** Splitting formulas ***)

let split_asm_rules =
  [
    (fun l -> falseA ~a:l);
    (fun l -> Tactics.disjA ~a:l);
    (fun  l -> Tactics.implA ~a:l)
  ]

let split_concl_rules =
  [
    (fun l -> Tactics.trueC ~c:l);
    (fun l -> Tactics.conjC ~c:l)
  ]

let split_asms_tac lst =
  asm_elim_rules_tac (split_asm_rules, []) lst

let split_concls_tac lst =
  concl_elim_rules_tac ([], split_concl_rules) lst

let splitter_tac ?f ctxt goal =
  let basic_splitter ctxt0 g =
    elim_rules_tac (split_asm_rules, split_concl_rules) ctxt0 g
  in
  apply_elim_tac basic_splitter ?f ctxt goal

let split_tac = splitter_tac

(*** Flattening formulas. ***)

let flatter_asm_rules =
  [
    (fun l -> falseA ~a:l);
    (fun l -> Tactics.negA ~a:l);
    (fun l -> Tactics.conjA ~a:l);
    (fun l -> Tactics.existA ~a:l)
  ]

let flatter_concl_rules =
  [
    (fun l -> Tactics.trueC ~c:l);
    (fun l -> Tactics.negC ~c:l);
    (fun l -> Tactics.disjC ~c:l);
    (fun l -> Tactics.implC ~c:l);
    (fun l -> Tactics.allC ~c:l)
  ]

let flatter_asms_tac lst ctxt g =
  asm_elim_rules_tac (flatter_asm_rules, []) lst ctxt g

let flatter_concls_tac lst ctxt g =
  concl_elim_rules_tac ([], flatter_concl_rules) lst ctxt g

let flatter_tac ?f ctxt goal =
  let basic_flatter g  =
    elim_rules_tac (flatter_asm_rules, flatter_concl_rules) g
  in
  apply_elim_tac basic_flatter ?f ctxt goal

let flatten_tac ?f ctxt g = flatter_tac ?f:f ctxt g

(*** Scattering formulas ***)

let scatter_asm_rules =
  [
    (fun l -> falseA ~a:l);

    (fun l -> Tactics.negA ~a:l);
    (fun l -> Tactics.existA ~a:l);
    (fun l -> Tactics.conjA ~a:l);

    (fun l -> Tactics.disjA ~a:l);
    (fun l -> Tactics.implA ~a:l)
  ]

let scatter_concl_rules =
  [
    (fun l -> Tactics.trueC ~c:l);

    (fun l -> Tactics.negC ~c:l);
    (fun l -> Tactics.allC ~c:l);

    (fun l -> Tactics.disjC ~c:l);
    (fun l -> Tactics.conjC ~c:l);
    (fun l -> Tactics.implC ~c:l);
    (fun l -> iffE ~c:l)
  ]

let scatter_tac ?f ctxt goal =
  let tac ctxt0 g =
    elim_rules_tac (scatter_asm_rules, scatter_concl_rules) ctxt0 g
  in
  apply_elim_tac tac ?f ctxt goal

(*** Scattering, solving formulas ***)

let blast_asm_rules =
  [
    (fun l -> falseA ~a:l);

    (fun l -> Tactics.negA ~a:l);
    (fun l -> Tactics.conjA ~a:l);
    (fun l -> Tactics.existA ~a:l);

    (fun l -> Tactics.disjA ~a:l);
    (fun l -> Tactics.implA ~a:l);

    (fun l -> basic ~a:l ?c:None)
  ]

let blast_concl_rules =
  [
    (fun l -> Tactics.trueC ~c:l);

    (fun l -> Tactics.negC ~c:l);
    (fun l -> Tactics.disjC ~c:l);
    (fun l -> Tactics.implC ~c:l);
    (fun l -> Tactics.allC ~c:l);

    (fun l -> Tactics.conjC ~c:l);

    (fun l -> iffE ~c:l);

    (fun l -> basic ?a:None ~c:l)
  ]

let blast_tac ?f ctxt goal =
  let tac ctxt0 g =
    elim_rules_tac (blast_asm_rules, blast_concl_rules) ctxt0 g
  in
  apply_elim_tac tac ?f ctxt goal

(*** Cases ***)

(** [cases_tac x sq]

    Adds formula x to assumptions of sq, creates new subgoal in which
    to prove x.

    {L
    g:\[asms |- concls\]

    --->

    g1:\[asms |- x{_ l}, concls\]; g2:\[x{_ l}, asms |- concls\]
    }

    info: [goals = [g1; g2], aforms=[l], cforms=[l], terms = []]
*)
let cases_thm_id = Ident.mk_long "Bool" "cases_thm"
let make_cases_tac_thm ctxt =
  Commands.get_or_prove ctxt (Ident.string_of cases_thm_id)
    (term "!P: (not P) or P")
    (allC ++ disjC ++ negC ++ basic)

let cases_thm sctxt =
  Context.find_thm sctxt cases_thm_id make_cases_tac_thm

let cases_tac (t: Basic.term) ctxt goal =
  let thm = cases_thm (set_scope ctxt (scope_of_goal goal)) in
  seq
    [
      cut thm;
      (?> (fun inf ->
        let thm_tag = get_one ~msg:"cases_tac 1" (Info.aformulas inf) in
        allA t ~a:(ftag thm_tag)));
      (?> (fun inf ->
        let thm_tag = get_one ~msg:"cases_tac 2" (Info.aformulas inf) in
        disjA ~a:(ftag thm_tag)))
      --
        [
          (?> (fun inf1 ->
            let asm_tag = get_one ~msg:"cases_tac 3" (Info.aformulas inf1)
            and lgoal, rgoal = get_two ~msg:"cases_tac 4" (Info.subgoals inf1)
            in
            (negA ~a:(ftag asm_tag) ++
               (?> (fun inf2 g2 ->
                 let nasm_tag = get_one ~msg:"cases_tac 5" (Info.cformulas inf2)
                 in
                 set_changes_tac
                   (Changes.make [lgoal; rgoal] [asm_tag] [nasm_tag] []) g2)))));
          skip
        ]
    ] ctxt goal

let show_tac (trm: Basic.term) tac ctxt goal =
  let thm = cases_thm (set_scope ctxt (scope_of_goal goal)) in
  seq
    [
      cut thm;
      (?> (fun inf1 ->
        let thm_tag = get_one ~msg:"show_tac 1" (Info.aformulas inf1) in
        allA trm ~a:(ftag thm_tag)));
      (?> (fun inf1 ->
        let thm_tag = get_one ~msg:"show_tac 2" (Info.aformulas inf1) in
        disjA ~a:(ftag thm_tag)))
      --
        [
          (?> (fun inf1 ->
            let asm_tag = get_one ~msg:"show_tac 3" (Info.aformulas inf1)
            in
            (negA ~a:(ftag asm_tag) ++ tac)));
          (?> (fun inf1 ->
            let (_, gl_tag) = get_two (Info.subgoals inf1)
            and asm_tag = get_one (Info.aformulas inf1)
            in
            set_changes_tac (Changes.make [gl_tag] [asm_tag] [] [])))
        ]
    ] ctxt goal

let show = show_tac

(** [cases_of ?thm trm]: Try to introduce a case split based on
    the type of term [trm]. If [thm] is given, it is used as the cases
    theorem. If [thm] is not given, the theorem named ["T_cases"] is
    used, where [T] is the name of the type of [trm].
*)

(** [disj_splitter_tac ?f]: Split an assumption using disjA
*)
let disj_splitter_tac ?f ctxt goal =
  let tac =
    elim_rules_tac ([ (fun l -> Tactics.disjA ~a:l) ], [])
  in
  apply_elim_tac tac ?f ctxt goal

let cases_of ?thm t ctxt goal =
  let scp = Tactics.scope_of_goal goal
  and tyenv = Tactics.typenv_of goal in
  let trm = Lterm.set_names scp t in
  let case_thm =
    match thm with
      | Some x -> x
      | _ ->
        begin
          let sb = Typing.settype scp ~env:tyenv trm in
          let ty = Gtype.mgu (Typing.typeof scp ~env:tyenv trm) sb
          in
          let (th, id) = Ident.dest (Gtype.get_type_name ty) in
          let thm_name = id^"_cases" in
          try Commands.thm ctxt (Ident.string_of (Ident.mk_long th thm_name))
          with _ ->
            try Commands.thm ctxt thm_name
            with _ -> failwith ("Can't find cases theorem "^thm_name)
        end
  in
  try
    seq [
        cut ~inst:[trm] case_thm;
        (?> (fun inf1 ->
          let a_tg = get_one (Info.aformulas inf1)
          in
          seq [
            (disj_splitter_tac ~f:(ftag a_tg) // skip);
            (?> (fun inf2 ->
              ((specA ~a:(ftag a_tg) // skip)
               ++ (?> (fun inf3 ->
                 set_changes_tac
                   (Changes.add_aforms inf2 (Info.aformulas inf3)))))))
          ]));
        (?> (fun inf ->
          set_changes_tac
            (Changes.make [] [] (Info.subgoals inf) (Info.constants inf))))
    ] ctxt goal
  with err -> raise (add_error "cases_of" err)


(*** Modus Ponens ***)

let mp0_tac a a1lbls ctxt g =
  let typenv = Tactics.typenv_of g
  and sqnt = Tactics.sequent g in
  let scp = Logic.Sequent.scope_of sqnt in
  let (a_label, mp_vars, mp_form) =
    try find_qnt_opt Basic.All Lterm.is_implies [get_tagged_asm a g]
    with Not_found ->
      raise (error "mp_tac: No implications in assumptions")
  and a1_forms =
    try Lib.map_find (fun x -> get_tagged_asm x g) a1lbls
    with Not_found -> raise (error "mp_tac: No such assumption")
  in
  let (_, mp_lhs, mp_rhs) = Term.dest_binop mp_form in
  let varp = Rewrite.is_free_binder mp_vars in
  let (a1_label, a1_env) =
    let exclude (t, _) = (Tag.equal t a_label) in
    try find_unifier scp typenv varp mp_lhs
          ~exclude:exclude a1_forms
    with Not_found ->
      raise
        (Term.term_error ("mp_tac: no matching formula in assumptions")
           [Term.mk_fun Lterm.impliesid [mp_lhs; mp_rhs]])
  in
  let tac1 =
    match mp_vars with
      | [] -> skip (* No quantifier *)
      | _ -> (* Implication has quantifier *)
        instA ~a:(ftag a_label)
          (Tactics.extract_consts mp_vars a1_env)
  in
  seq [
    tac1;
    Tactics.implA ~a:(ftag a_label);
    (?> (fun inf1 ->
      ((fun n ->
        Lib.apply_nth 0 (Tag.equal (Tactics.node_tag n))
          (Info.subgoals inf1) false)
       -->
       (Tactics.basic ~a:(ftag a1_label)
          ~c:(ftag (Lib.get_one (Info.cformulas inf1)
                    (Failure "mp_tac2.2")))))));
    (?> (fun inf4 ->
      set_changes_tac (Changes.make [] (Info.aformulas inf4) [] [])))
  ] ctxt g

let mp_tac ?a ?h ctxt goal =
  let sqnt = sequent goal
  in
  let albls =
    match a with
      | None -> List.map (ftag <+ drop_formula) (asms_of sqnt)
      | Some(x) -> [x]
  and hlbls =
    match h with
      | None -> List.map (ftag <+ drop_formula) (asms_of sqnt)
      | Some(x) -> [x]
  in
  try map_first (fun x -> mp0_tac x hlbls) albls ctxt goal
  with err -> raise (error "mp_tac: Failed")

(** [cut_mp_tac thm ?a]

    Apply modus ponens to theorem [thm] and assumption [a].  [thm]
    must be a (possibly quantified) implication [!x1 .. xn: l=>r] and
    [a] must be [l].

    If [a] is not given, finds a suitable assumption to unify with
    [l].

    info: [] [thm_tag] [] [] where tag [thm_tag] identifies the theorem
    in the sequent.
*)
let cut_mp_tac ?inst thm ?a ctxt goal =
  let f_label =
    Lib.apply_option
      (fun x -> Some (ftag (Logic.label_to_tag x (Tactics.sequent goal))))
      a None
  in
  (Tactics.cut ?inst:inst thm ++
     (?> (fun inf1 ->
       let a_tag =
         Lib.get_one (Info.aformulas inf1)
           (Logic.logic_error "cut_mp_tac: Failed to cut theorem"
              [Logic.formula_of thm])
       in
       ((mp_tac ~a:(ftag a_tag) ?h:f_label) ++
           (?> (fun inf2 ->
             set_changes_tac
               (Changes.add_aforms inf1 (Info.aformulas inf2))))))))
    ctxt goal

(** [back_tac]: Backward match tactic. [back0_tac] is the main engine.

    info [g_tag] [] [c_tag] []
    where
    [g_tag] is the new goal
    [c_tag] identifies the new conclusion.
*)
let back0_tac a cs ctxt goal =
  let typenv = Tactics.typenv_of goal
  and sqnt = Tactics.sequent goal in
  let scp = Logic.Sequent.scope_of sqnt in
  let (a_label, back_vars, back_form) =
    try find_qnt_opt Basic.All Lterm.is_implies [get_tagged_asm a goal]
    with Not_found -> raise (error "back_tac: No assumption")
  and c_forms =
    try Lib.map_find (fun x -> get_tagged_concl x goal) cs
    with Not_found -> raise (error "back_tac: No such conclusion")
  in
  let (_, back_lhs, back_rhs) = Term.dest_binop back_form in
  let varp = Rewrite.is_free_binder back_vars in
  (* find, get the conclusion and substitution *)
  let (c_label, c_env) =
    let exclude (t, _) = (Tag.equal t a_label)
    in
    try find_unifier scp typenv varp back_rhs
          ~exclude:exclude c_forms
    with Not_found ->
      raise (Term.term_error
               ("back_tac: no matching formula in conclusion")
               [Term.mk_fun Lterm.impliesid [back_lhs; back_rhs]])
  in
  let tac1 =
    if back_vars = []
    then (* No quantifier *)
      skip
    else (* Implication has quantifier *)
      instA ~a:(ftag a_label)
        (Tactics.extract_consts back_vars c_env)
  and tac2 = Tactics.implA ~a:(ftag a_label)
  and tac3 =
    (?> (fun inf3 g3 ->
      let atag3 = get_one (Info.aformulas inf3) in
      ((fun n ->
        (Lib.apply_nth 1
           (Tag.equal (Tactics.node_tag n))
           (Info.subgoals inf3) false))
       -->
       (Tactics.basic
          ~a:(ftag atag3)
          ~c:(ftag c_label))) g3))
  in
  let tac4 =
    (?> (fun inf4 g4 ->
     (delete (ftag c_label) ++
        set_changes_tac
        (Changes.make
           [get_one (Info.subgoals inf4)] []
           [get_one (Info.cformulas inf4)] [])) g4))
  in
  (tac1 ++ seq [tac2; tac3; tac4]) ctxt goal

let back_tac ?a ?c ctxt goal =
  let sqnt = sequent goal in
  let alabels =
    match a with
      | None -> List.map (ftag <+ drop_formula) (asms_of sqnt)
      | Some x -> [x]
  and clabels =
    match c with
      | None -> List.map (ftag <+ drop_formula) (concls_of sqnt)
      | Some(x) -> [x]
  in
  try map_first (fun x -> back0_tac x clabels) alabels ctxt goal
  with err -> raise (error "back_tac: Failed")

let cut_back_tac ?inst thm ?c ctxt g =
  let c_label =
    Lib.apply_option
      (fun x -> Some (ftag (Logic.label_to_tag x (Tactics.sequent g))))
      c None
  in
  let tac1 = Tactics.cut ?inst:inst thm in
  let tac2 =
    (?> (fun inf2 ->
      let a_tag =
        Lib.get_one
          (Info.aformulas inf2)
          (Logic.logic_error "cut_back_tac: Failed to cut theorem"
             [Logic.formula_of thm])
      in
      back_tac ~a:(ftag a_tag) ?c:c_label))
  in
  (tac1 ++ tac2) ctxt g
