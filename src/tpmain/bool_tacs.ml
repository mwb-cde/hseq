
open Drule
    open Commands
    open Tactics


let eq_tac0 sqnt = 
  let a = first_concl Formula.is_equals (Logic.get_sqnt sqnt)
  in 
  let th = 
    try lemma "base.eq_sym"
    with Not_found -> 
      (Result.raiseError "Can't find required lemma base.eq_sym")
  in rule_tac (Logic.Rules.thenl [cut th; unify_tac (-1) a]) sqnt

let eq_tac sqnt = rule_tac eq_tac0 sqnt

    let cut_thm str = (cut (lemma str))

    let unfold str i sqnt= 
      let j= if i<0 then i-1 else i
      in
      rule_tac(Logic.Rules.thenl[cut (defn str); replace (-1) j; delete (-1)]) sqnt

    let rewrite_thm str i= 
     Drule.rewrite_thm [lemma str] true i

    let rewrite_rl str i= 
      Tactics.rule_tac (Drule.rewrite_thm [lemma str] rightleft i)

    let rewrite_lr str i= 
      Tactics.rule_tac (Drule.rewrite_thm [lemma str] leftright i)

    let rewrite th i= 
      let j= if i<0 then i-1 else i
      in
      rule_tac(Logic.Rules.thenl[(cut th);(replace (-1) j); delete (-1)])

    let rewrite_dir dir thms i= 
      Tactics.rule_tac (Drule.rewrite_thm thms dir i)

(* iffI_rule i sq:
   asm |- a iff b, cncl 
   -->
   a, asm |- b, cncl       and     b, asm |- a, cncl
*)

let is_iff f = 
  try 
    (fst (Term.dest_fun (Formula.term_of_form f)) 
       = (Basic.mklong "base" "iff"))
  with _ -> false

let iffI_rule i sq = 
  let _, f = Logic.get_cncl i (Logic.get_sqnt sq)
  in
  if not (is_iff f) then (Result.raiseError "iffI_rule")
  else 
      (Logic.Rules.thenl 
	 [Drule.rewrite_thm [lemma "boolean.iff_def"] true i;
	   Logic.Rules.conjI i;
	   Logic.Rules.implI i]) sq

let iffI g = 
    (fun sq-> 
	 iffI_rule (first_concl is_iff (Logic.get_sqnt sq)) sq) g

let false_rule0 a sq =
  let  thm = lemma "base.false_def"
  in 
  rule_tac(Logic.Rules.thenl
	     [(Drule.rewrite_thm [thm] true a); 
	       Logic.Rules.negA a; Logic.Rules.trueR 1]) sq

let false_rule sqnt =
  rule_tac (fun sq ->
  let a= Drule.first_asm Formula.is_false (Logic.get_sqnt sq)
  in false_rule0 a sq) sqnt;;

    let asm_elims () = 
      [	(Formula.is_false, false_rule0);
	(Formula.is_neg, Logic.Rules.negA);  
	(Formula.is_conj, Logic.Rules.conjE); 
(*	(Formula.is_implies, Drule.mp_basic_rule); *)
	(Formula.is_exists, Logic.Rules.existI)]


(*
	 (fun x -> Logic.Rules.orl[Drule.mp_basic_rule x; Logic.Rules.implE x]));
*)

    let conc_elims () =
      [
      (Formula.is_true, Logic.Rules.trueR);
      (Formula.is_neg, Logic.Rules.negC); 
      (Formula.is_disj, Logic.Rules.disjE);
      (Formula.is_implies, Logic.Rules.implI);
      (Formula.is_all, Logic.Rules.allI)]

(*
let rec flatten_tac g =
  rule_tac(repeat
	     (rule_tac
	 	(Logic.Rules.apply_list 
		   [ Drule.foreach_conc (conc_elims()); 
		     Drule.foreach_asm (asm_elims())]))) g
*)

let rec flatten_tac g =
  rule_tac(repeat
	     (rule_tac
	 	(Logic.Rules.orl 
		   [ Drule.foreach_conc (conc_elims()); 
		     Drule.foreach_asm (asm_elims())]))) g


let split_asm () = 
  [(Formula.is_disj, Logic.Rules.disjI);  
    (Formula.is_implies, Logic.Rules.implE)]

let split_conc () =
  [(Formula.is_conj, Logic.Rules.conjI); 
    (Formula.is_disj, Logic.Rules.disjE);
    (is_iff, iffI_rule)]

let split_tac g=
    Logic.Rules.repeat
       (Logic.Rules.apply_list 
	     [ Drule.foreach_conc (split_conc()); 
	       Drule.foreach_asm (split_asm())]) g

let inst_rule l i sqnt=
  let rec rule ys sqs = 
      match ys with 
      	[] -> sqs
      | (x::xs) -> 
	  let nsqnt=
	    if (i<0) 
	    then 
	      (Logic.Rules.allE (Tpenv.read_unchecked x ) i) sqs
	    else 
	      (Logic.Rules.existE (Tpenv.read_unchecked x ) i) sqs
	  in rule xs nsqnt
  in rule l sqnt

let inst_term_rule l i sqnt=
  let rec rule ys sqs = 
      match ys with 
      	[] -> sqs
      | (x::xs) -> 
	  let nsqnt=
	    if (i<0) 
	    then 
	      (Logic.Rules.allE x i) sqs
	    else 
	      (Logic.Rules.existE x i) sqs
	  in rule xs nsqnt
  in rule l sqnt


let inst_tac l i g= 
  rule_tac (inst_rule l i) g

let inst_asm l g=
  rule_tac 
    (fun sq -> 
      inst_rule l
	(Drule.first_asm 
	 (Formula.is_all) (Logic.get_sqnt sq)) sq) g

let inst_concl l g=
  rule_tac 
    (fun sq -> 
      inst_rule l
	(Drule.first_concl (Formula.is_all) (Logic.get_sqnt sq)) sq) g


let cases_tac0 (x:Term.term) g= 
  let thm = 
    try
      lemma "boolean.cases_thm"
    with Not_found -> 
      (Result.raiseError "Can't find required lemma boolean.cases_thm")
  in 
  rule_tac(thenl
	     [cut thm; rule_tac (Drule.allE x); disjI; negA; postpone]) g

let cases_tac x = cases_tac0 (Tpenv.read_unchecked x)

let equals_tac i g =
  let thm = 
    try
      lemma "boolean.equals_bool"
    with Not_found -> 
      (Result.raiseError "Can't find required lemma boolean.equals_bool")
  in 
  (rewrite thm i g)

let false_tac g = (rule_tac false_rule) g

let bool_tac g=
  (false_tac || trivial) g

(* match_mp_rule thm i sqnt: 

where thm is of the form A=>B and concl i of sqnt is C

remove outermost universal quantifiers of thm into list qnts
splits A=>B into (A, B)
unifies B with C
cuts thm into sqnt
instantiates quantifiers of thm with terms obtained by unification.
applies implE to sqnt, getting a list of sqnts
applies basic to second sqnt in the sqnt list

fails if any except first two steps fails 
*)

let hyp_conc_thm f = 
  let (qnts, t)=
    Term.strip_qnt Basic.All (Formula.dest_form f)
  in
  if (Logicterm.is_implies t)
  then
    match Term.dest_fun t with
      (_, (a::b::[])) -> (qnts, a, b)
    | _ -> (Result.raiseError "hyp_conc_thm: unusually shaped implication")
  else (qnts, Term.mkbool true, t)

let match_mp_rule0 thm i sq=
  let (qnts, a, b) = hyp_conc_thm (Logic.dest_thm thm)
  and c = 
    Formula.dest_form (snd (Logic.get_cncl i (Logic.get_sqnt sq)))
  and scp = Logic.scope_of (Logic.get_sqnt sq)
  and lookup y env = (try Term.find y env with Not_found -> y)
  in 
  let qenv = Unify.unify scp (Rewrite.is_free_binder qnts) b c
  in 
  let ncnsts = List.map (fun x -> lookup (Term.Bound x) qenv) qnts
  in 
  (Logic.Rules.thenl 
     [Logic.Rules.cut thm; inst_term_rule ncnsts (-1); Logic.Rules.implE (-1);
      Logic.Rules.postpone; Logic.Rules.unify (-1) i]) sq

let match_mp_tac thm i g = match_mp_rule0 thm i g

(*
  rule_tac(Logic.Rules.thenl[match_mp_rule0 thm i; 
			Logic.Rules.postpone; Logic.Rules.unify (-1) i]) g
*)

let match_mp_sqnt_rule0 j i sq=
  let (qnts, a, b) = hyp_conc_thm 
    (snd (Logic.get_asm j (Logic.get_sqnt sq)))
  and c = Formula.dest_form 
    (snd (Logic.get_cncl i (Logic.get_sqnt sq)))
  and scp = Logic.scope_of (Logic.get_sqnt sq)
  and lookup y env = (try Term.find y env with Not_found -> y)
  in 
  let qenv = Unify.unify scp (Rewrite.is_free_binder qnts) b c
  in 
  let ncnsts =List.rev(List.map (fun x -> lookup (Term.Bound x) qenv) qnts)
  in 
  Logic.Rules.thenl 
    [inst_term_rule ncnsts j; Logic.Rules.implE j;
      Logic.Rules.postpone; Logic.Rules.unify j i] sq


let back_mp_tac j i g =match_mp_sqnt_rule0 j i g
