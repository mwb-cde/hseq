let hyp_conc_thm f = 
  let (qnts, t)=
    Term.strip_qnt Basic.All (Formula.dest_form f)
  in
  if (Logicterm.is_implies t)
  then
    match Term.dest_fun t with
      (_, (a::b::[])) -> (List.rev qnts, a, b)
    | _ -> (Result.raiseError "hyp_conc_thm: unusually shaped implication")
  else (List.rev qnts, Term.mkbool true, t)

let match_mp_rule0 thm i sq=
  let (qnts, a, b) = hyp_conc_thm (Logic.dest_thm thm)
  and c = Formula.dest_form (Logic.get_cncl i sq)
  and scp = Logic.scope_of sq
  in 
  let qenv = Unify.unify scp (Rewrite.is_free_binder qnts) b c
  in 
  let ncnsts = List.map (fun x -> Term.find (Term.Bound x) qenv) qnts
  in 
  (Logic.thenl 
    [Logic.cut thm; Bool_tacs.inst_term_rule (-1) ncnsts; Logic.implE (-1)] sq)

let match_mp_tac thm i g = 
  (Tactics.thenl[Tactics.rule_tac (match_mp_rule0 thm i); 
		 Tactics.unify_tac (-1) i]) g

let match_mp_sqnt_rule0 j i sq=
  let (qnts, a, b) = hyp_conc_thm (Logic.get_asm j sq)
  and c = Formula.dest_form (Logic.get_cncl i sq)
  and scp = Logic.scope_of sq
  in 
  let qenv = Unify.unify scp (Rewrite.is_free_binder qnts) b c
  in 
  let ncnsts = List.map (fun x -> Term.find (Term.Bound x) qenv) qnts
  in 
  (Logic.thenl 
    [Bool_tacs.inst_term_rule j ncnsts; Logic.implE j] sq)

let back_mp_tac j i g = 
  (Tactics.thenl[Tactics.rule_tac (match_mp_sqnt_rule0 j i); 
		 Tactics.unify_tac j i]) g
