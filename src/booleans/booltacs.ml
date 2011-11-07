(*----
  Name: booltacs.ml
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
open Boolbase
open Rewritelib
open Commands
open Tactics
open Lib.Ops

(** General support for boolean reasoning *)

(*** Boolean equivalence ***)

let make_iff_def () = defn (Ident.string_of Lterm.iffid)
let iff_def_var = Lib.freeze make_iff_def
let iff_def () = Lib.thaw ~fresh:fresh_thm iff_def_var

(** [iffA l sq]: Elminate the equivalance at assumptin [l]

    {L
    g:\[(A iff B){_ l}, asms |- concl]
    ---->
    g:[(A => B){_ l1}, (B => A){_ l2}, asms |- concl]; 
    }

    info: [goals = [], aforms=[l1; l2], cforms=[], terms = []]
*)
let iffA ?info ?a goal = 
  let af = first_asm_label a is_iff goal in 
  let sqnt = Tactics.sequent goal in 
  let (t, f) = 
    Logic.Sequent.get_tagged_asm (Logic.label_to_tag af sqnt) sqnt
  in 
  if not (is_iff f) 
  then raise (error "iffA")
  else 
    seq 
      [
        rewrite_tac [iff_def()] ~f:(ftag t);
        lift_info ?info (Tactics.conjA ~a:(ftag t));
      ] goal

(** [iffC l sq]: Elminate the equivalence at conclusion [l]

    {L
    g:\[asms |- (A iff B){_ l}, concl]
    ---->
    g1:\[asms |- (A => B){_ l}, concl]
    g2:\[asms |- (B => A){_ l}, concl]
    }

    info: [goals = [g1; g2], aforms=[], cforms=[l], terms = []]
**)

let iffC ?info ?c goal = 
  let cf = first_concl_label c is_iff goal in 
  let sqnt=sequent goal in 
  let (t, f) =
    Logic.Sequent.get_tagged_cncl (Logic.label_to_tag cf sqnt) sqnt
  in
  if not (is_iff f) 
  then raise (error "iffC")
  else 
    seq 
      [
        rewrite_tac [iff_def()] ~f:(ftag t);
        lift_info ?info (Tactics.conjC ~c:(ftag t));
      ] goal

(** [iffE l sq]: Fully elminate the equivalence at conclusion [l]

    {L
    g:\[asms |- (A iff B){_ l}, concl]
    ---->
    g1:[A{_ l1}, asms |- B{_ l2}, concl]; 
    g2:[B{_ l3}, asms |- A{_ l4}, concl]; 
    }

    info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
**)
let iffE ?info ?c goal = 
  let cf = first_concl_label c is_iff goal in 
  let sqnt = sequent goal in 
  let (t, f) = 
    Logic.Sequent.get_tagged_cncl (Logic.label_to_tag cf sqnt) sqnt
  in
  let add_goals info gls = info_add info gls [] [] []
  in 
  let add_forms info atgs ctgs = info_add info [] atgs ctgs []
  in
  if not (is_iff f) 
  then raise (error "iffE")
  else 
    let tac g =
      seq 
	[
	  rewrite_tac [iff_def()] ~f:(ftag t);
          (?> fun inf g1 ->
            add_goals info (New.subgoals inf);
	    Tactics.conjC ~c:(ftag t) g1);
	  Tactics.implC ~c:(ftag t)
	] g
    in 
    alt [ 
      (tac ++ 
         (?> fun inf g1 ->
           add_forms info (New.aformulas inf) (New.cformulas inf);
           skip g1));
      fail ~err:(error "iffE") 
    ] goal

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

let split_asms_tac ?info lst = 
  lift_info ?info (asm_elim_rules_tac (split_asm_rules, []) lst)

let split_concls_tac ?info lst = 
  lift_info ?info (concl_elim_rules_tac ([], split_concl_rules) lst)

let splitter_tac ?info ?f goal =
  let basic_splitter ?info = 
    elim_rules_tac (split_asm_rules, split_concl_rules)
  in 
  apply_elim_tac basic_splitter ?f goal

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

let flatter_asms_tac ?info lst = 
  lift_info ?info (asm_elim_rules_tac (flatter_asm_rules, []) lst)

let flatter_concls_tac ?info lst = 
  lift_info ?info (concl_elim_rules_tac ([], flatter_concl_rules) lst)

let flatter_tac ?info ?f goal =
  let basic_flatter ?info =
    elim_rules_tac (flatter_asm_rules, flatter_concl_rules)
  in 
  apply_elim_tac basic_flatter ?f goal

let flatten_tac ?info ?f g = flatter_tac ?info:info ?f:f g

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
    (fun l -> iffE ?info:None ~c:l)
  ]

let scatter_tac ?info ?f goal =
  let tac ?info =
    elim_rules_tac (scatter_asm_rules, scatter_concl_rules)
  in 
  apply_elim_tac tac ?f goal


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

    (fun l -> iffE ?info:None ~c:l);

    (fun l -> basic ?a:None ~c:l)
  ]

let blast_tac ?info ?f goal =
  let tac ?info =
    elim_rules_tac (blast_asm_rules, blast_concl_rules)
  in 
  apply_elim_tac tac ?f goal

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
let make_cases_tac_thm () = 
  Commands.get_or_prove "Bool.cases_thm"
  << !P: (not P) or P >> (allC ++ disjC ++ negC ++ basic)

let cases_thm_var = Lib.freeze make_cases_tac_thm
let cases_thm () =  Lib.thaw ~fresh:fresh_thm cases_thm_var

let set_info dst (sgs, afs, cfs, cnsts) = 
  Tactics.info_add dst sgs afs cfs cnsts

let cases_tac ?info (t:Basic.term) = 
  let thm = cases_thm() in
  seq 
    [
      cut thm;
      (?> fun inf1 g -> 
        let thm_tag = get_one ~msg:"cases_tac 1" (New.aformulas inf1)
        in 
        allA t ~a:(ftag thm_tag) g);
      (?> fun inf1 g -> 
        let thm_tag = get_one ~msg:"cases_tac 2" (New.aformulas inf1)
        in 
        disjA ~a:(ftag thm_tag) g)
      --
        [
	  (?> fun inf1 g ->
	    let asm_tag = get_one ~msg:"cases_tac 3" (New.aformulas inf1)
	    and lgoal, rgoal = get_two ~msg:"cases_tac 4" (New.subgoals inf1)
	    in 
	    seq
	      [
	        negA ~a:(ftag asm_tag);
	        (?> fun inf1 g1 -> 
	          let nasm_tag = get_one ~msg:"cases_tac 5" (New.cformulas inf1)
	          in 
                  set_info_tac ?info
		    ([lgoal; rgoal], [nasm_tag], [nasm_tag], []) g1)
	      ] g);
	  skip
        ]
    ]

let show_tac ?info (trm:Basic.term) tac = 
  let thm = cases_thm()
  in 
  seq 
    [
      cut thm;
      (?> fun inf1 g -> 
        let thm_tag = get_one ~msg:"show_tac 1" (New.aformulas inf1)
        in 
        allA trm ~a:(ftag thm_tag) g);
      (?> fun inf1 g -> 
        let thm_tag = get_one ~msg:"show_tac 2" (New.aformulas inf1)
        in 
        lift_info (disjA ~a:(ftag thm_tag)) g)
      --
        [
	  (?> fun inf1 g ->
	    let asm_tag = get_one ~msg:"show_tac 3" (New.aformulas inf1)
	    in 
	    seq [ negA ~a:(ftag asm_tag); tac ] g);
	  (?> fun inf1 g -> 
	    let (_, gl_tag) = get_two (New.subgoals inf1)
	    and asm_tag = get_one (New.aformulas inf1)
	    in 
            set_info_tac ?info ([gl_tag], [asm_tag], [], []) g)
        ]
    ]

let show = show_tac

(** [cases_of ?info ?thm trm]: Try to introduce a case split based on
    the type of term [trm]. If [thm] is given, it is used as the cases
    theorem. If [thm] is not given, the theorem named ["T_cases"] is
    used, where [T] is the name of the type of [trm].
*)

(** [disj_splitter_tac ?info ?f]: Split an assumption using disjA
*)
let disj_splitter_tac ?info ?f goal = 
  let tac ?info =
    elim_rules_tac ([ (fun l -> Tactics.disjA ~a:l) ], []) 
  in 
  apply_elim_tac tac ?f goal
    

let cases_of ?info ?thm t g =
  let scp = Tactics.scope_of g
  and tyenv = Tactics.typenv_of g in 
  let trm = Lterm.set_names scp t in 
  let case_thm = 
    match thm with
      | Some x -> x
      | _ -> 
        begin
          
	  let ty = 
	    let sb = Typing.settype scp ~env:tyenv trm
	    in Gtypes.mgu (Typing.typeof scp ~env:tyenv trm) sb
	  in
	  let (th, id) = Ident.dest (get_type_name ty) in 
	  let thm_name = id^"_cases" 
          in 
	  try Commands.thm (Ident.string_of (Ident.mk_long th thm_name))
	  with _ ->
	    try Commands.thm thm_name
	    with _ -> failwith ("Can't find cases theorem "^thm_name)
        end
  in 
  let inf1 = 
    match info with 
      | None -> None 
      | _ -> Some(Tactics.info_make())
  in 
  let tac1 g1 = 
    seq 
      [
        cut ~inst:[trm] case_thm;
        (?> fun inf g -> 
	  let a_tg = get_one (New.aformulas inf)
	  in 
          seq [
            set_info_tac ?info ([a_tg], [], [], []);
	    (disj_splitter_tac ?info:info ~f:(ftag a_tg) // skip);
	    lift_info ?info:info (specA ~a:(ftag a_tg) // skip)
          ] g)
      ] g1
  in 
  let tac2 g2 = 
    match inf1 with
      | None -> skip g2
      | Some inf2 -> 
        update_tac 
	  (set_info info)
          ([], [], (subgoals inf2), (constants inf2)) 
          g2
  in 
  try (tac1 ++ tac2) g
  with err -> raise (add_error "cases_of" err)
    

(*** Modus Ponens ***)

let mp0_tac ?info a a1lbls g =
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
    (?> fun inf1 ->
      ((fun n -> Lib.apply_nth 0 (Tag.equal (Tactics.node_tag n))
	(New.subgoals inf1) false)
       --> 
       (Tactics.basic ~a:(ftag a1_label)
          ~c:(ftag (Lib.get_one (New.cformulas inf1)
	            (Failure "mp_tac2.2"))))));
    (?> fun inf4 -> set_info_tac ?info ([], New.aformulas inf4, [], []))
  ] g

    
let mp_tac ?info ?a ?h goal =
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
  let tac g = map_first (fun x -> mp0_tac ?info x hlbls) albls g
  in 
  try tac goal
  with err -> raise (error "mp_tac: Failed")

(** [cut_mp_tac ?info thm ?a]

    Apply modus ponens to theorem [thm] and assumption [a].  [thm]
    must be a (possibly quantified) implication [!x1 .. xn: l=>r] and
    [a] must be [l].

    If [a] is not given, finds a suitable assumption to unify with
    [l].

    info [] [thm_tag] [] [] where tag [thm_tag] identifies the theorem
    in the sequent.
*)
let cut_mp_tac ?info ?inst thm ?a g =
  let f_label = 
    Lib.apply_option 
      (fun x -> Some (ftag (Logic.label_to_tag x (Tactics.sequent g))))
      a None
  in 
  let tac1 = Tactics.cut ?inst:inst thm in 
  let tac2 info1 g2 = 
    begin
      let a_tag = 
        Lib.get_one (New.aformulas info1) 
	  (Logic.logic_error "cut_mp_tac: Failed to cut theorem" 
	     [Logic.formula_of thm])
      in 
      mp_tac ?info:info ~a:(ftag a_tag) ?h:f_label g2
    end
  in 
  (tac1 ++ (?> tac2)) g

(** [back_tac]: Backward match tactic. [back0_tac] is the main engine.

    info [g_tag] [] [c_tag] []
    where 
    [g_tag] is the new goal
    [c_tag] identifies the new conclusion.
*)
let back0_tac ?info a cs goal =
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
  let info1 = Tactics.info_make() in 
  let tac1=
    match back_vars with
      | [] -> (* No quantifier *)
	skip
      | _ -> (* Implication has quantifier *)
	instA ~a:(ftag a_label)
	  (Tactics.extract_consts back_vars c_env)
  and tac2 g2 = lift_info ~info:info1 (Tactics.implA ~a:(ftag a_label)) g2
  and tac3 g3 =
    ((fun n -> 
      (Lib.apply_nth 1 (Tag.equal (Tactics.node_tag n)) 
	 (Tactics.subgoals info1) false))
     --> 
     lift_info ~info:info1 
       (Tactics.basic ~a:(ftag (Lib.get_nth (Tactics.aformulas info1) 1))
          ~c:(ftag c_label))) g3
  in 
  let tac4 g4 = delete (ftag c_label) g4 in 
  let tac5 g5 = 
    update_tac 
      (set_info info)
      ([Lib.get_nth (Tactics.subgoals info1) 0],
       [], 
       [Lib.get_nth (Tactics.cformulas info1) 0],
       [])
      g5
  in 
  (tac1++ (tac2 ++ tac3 ++ tac4 ++ tac5)) goal

let back_tac ?info ?a ?c goal =
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
  let tac g = map_first (fun x -> back0_tac ?info x clabels) alabels g
  in 
  try tac goal
  with err -> raise (error "back_tac: Failed")

let cut_back_tac ?info ?inst thm ?c g =
  let c_label = 
    Lib.apply_option 
      (fun x -> Some (ftag (Logic.label_to_tag x (Tactics.sequent g))))
      c None
  in 
  let tac1 = Tactics.cut ?inst:inst thm in 
  let tac2 info1 g2 = 
    begin
      let a_tag = 
        Lib.get_one (New.aformulas info1) 
	  (Logic.logic_error "cut_back_tac: Failed to cut theorem" 
	     [Logic.formula_of thm])
      in 
      back_tac ?info:info ~a:(ftag a_tag) ?c:c_label g2
    end
  in 
  (tac1 ++ (?> tac2)) g
