module Simpconvs =
  struct

    open Simputils

    let dest_option x=
      match x with 
	None -> raise (Failure "dest_option")
      | Some(y) -> y


(** [case_tac x sq]

   adds formula x to assumptions of sq, 
   creates new subgoal in which to prove x

   g|asm |- cncl      --> g|asm |- t:x, cncl, g'| t:x, asm |- cncl 

   info: [g, g'] [t]
 *)
    let cases_tac_thm = ref None

    let get_cases_thm ()=
      match !cases_tac_thm with
	None ->
	  let nthm =
	    try 
	      Commands.lemma "boolean.cases_thm"
	    with Not_found -> 
	      (Goals.prove_goal <<!P: (not P) or P>>
	       (seq [flatten_tac; basic]))
	  in 
	  cases_tac_thm := Some(nthm);
	  nthm
      | Some(t) -> t


    let cases_full_tac inf (x:Basic.term) g= 
      let thm =  get_cases_thm()
      and tinf=Drule.mk_info()
      in 
      let g1=Logic.Rules.cut (Some tinf) thm g
      in 
      let nt=Lib.get_one ((!tinf).Logic.forms) (Failure "case_info")
      in 
      let g2=
	seq[
	Logic.Rules.allA None x (Logic.FTag nt);
	Logic.Rules.disjA (Some tinf) (Logic.FTag nt);
	Logic.Rules.negA None (Logic.FTag nt) ] g1
      in 
      let ng1, ng2=Lib.get_two (!tinf).Logic.goals (Failure "case_info")
      in 
      Logic.add_info inf [ng1;ng2] [nt] [];
      g2

    let cases_tac (x:Basic.term) g= cases_full_tac None x g

(**
   [simple_rewrite_conv scp rule thm]
   rewrite theorem [thm] with [rule]
   by descending through topmost universal quantifiers and applying
   rewrite once only to the body of [thm].
   i.e. 
   [rewrite_simple << ! a: a = true >> << !x y z: (f x y z) >>]
   ->
   [ << ! x y z: (f x y z) = true >> ]
*)
let simple_rewrite_conv scp rule thm=
  let make_goal (rvars, rlhs, rrhs) (tvars, tbody) = 
    let env=Unify.unify scp (Rewrite.is_free_binder rvars) rlhs tbody
    in 
    Formula.mk_form scp
      (rebuild_qnt Basic.All tvars 
	 (Logicterm.mk_equality tbody (Term.subst env rrhs)))
  in 
  let rvars, rbody = 
    Term.strip_qnt Basic.All (Formula.dest_form (Logic.dest_thm rule))
  in 
  let rlhs, rrhs = Logicterm.dest_equality rbody
  in 
  let tvars, tbody = 
    Term.strip_qnt Basic.All (Formula.dest_form (Logic.dest_thm thm))
  in 
  let info = Drule.mk_info()
  in 
  let goal = 
    Logic.mk_goal (scope()) (make_goal (rvars, rlhs, rrhs) (tvars, tbody))
  in 
  let sqnt=Logic.get_sqnt goal
  in 
  let g1=repeat (Logic.Rules.allC None (Logic.FNum 1)) goal
  in let g2= Logic.Rules.cut (Some info) rule g1
  in let g3=
    let atag = 
      Logic.FTag(Lib.get_one (!info.Logic.forms) (Failure "get_one"))
    and ctag = Logic.FNum 1
    in 
    Logic.Rules.basic None atag ctag g2
  in 
  Logic.mk_thm g3


(**
   [simple_asm_rewrite_tac scp info rule asm]
   rewrite assumption [asm] with [rule]
   by descending through topmost universal quantifiers and applying
   rewrite once only to the body of [asm].
   i.e. 
  
   rule=|- asm = rhs
   asm, A |- C 
   -->
   t':rhs, asm, A |- C

   info = [] [t'] []
 *)
let simple_asm_rewrite_tac inf rule asm goal=
  let sqnt=Logic.get_sqnt goal
  in 
  let scp = Logic.Sequent.scope_of sqnt
  in
  let make_goal (rvars, rlhs, rrhs) (tvars, tbody) = 
    let env=Unify.unify scp (Rewrite.is_free_binder rvars) rlhs tbody
    in 
      (rebuild_qnt Basic.All tvars 
	 (Logicterm.mk_equality tbody (Term.subst env rrhs)))
  in 
  let atag=Logic.label_to_tag asm sqnt
  in 
  let athm=
    try
      Logic.drop_tag (Logic.Sequent.get_tagged_asm atag sqnt)
    with Not_found -> 
      raise (Result.error "simple_asm_rewrite_tac: No such assumption")
  in 
  let rvars, rbody = 
    Term.strip_qnt Basic.All (Formula.dest_form (Logic.dest_thm rule))
  in 
  let rlhs, rrhs = Logicterm.dest_equality rbody
  in 
  let tvars, tbody = 
    Term.strip_qnt Basic.All (Formula.dest_form athm)
  in 
  let info = Drule.mk_info()
  in 
  let g0 = 
    cases_full_tac (Some info)
      (make_goal (rvars, rlhs, rrhs) (tvars, tbody)) goal
  in 
  let (disch_goal, ret_goal) = 
    Lib.get_two (!info.Logic.goals) (Failure "Getting goal tags")
  and ret_form = 
    Lib.get_one (!info.Logic.forms) (Failure "Getting formula tag")
  in 
  let g1=repeat (Logic.Rules.allC None (Logic.FNum 1)) g0
  in 
  let g2= Logic.Rules.cut (Some info) rule g1
  in 
  let g3=
    let atag = 
      Logic.FTag(Lib.get_one (!info.Logic.forms) (Failure "get_one"))
    and ctag = Logic.FNum 1
    in 
    Logic.Rules.basic None atag ctag g2
  in 
  if(sqnt_solved disch_goal g3)
  then 
    (Logic.add_info inf [ret_goal] [ret_form] []; g3)
  else 
    raise (Result.error "simple_asm_rewrite_tac: Failed");;


(* 
   iff_equals_ax:  !x y: (x iff y) = (x = y)

   make_n_ax(): prove theorem n
   get_n_ax(): get theorem n, proving it if necessary
 *)

    let make_iff_equals_ax ()=
      let iff_l1 = prove_goal <<!x y: (x = y ) => (x => y)>>
	(seq[flatten_tac; replace_tac ~asms:[!~ 2] ~f:(!~1); basic])
      in 
      let iff_l2 = prove_goal
	  <<!x y: ((x => y) and (y => x)) => (x=y)>>
	(seq [
	 flatten_tac; 
	 cut_thm "bool_cases"; allA <<x_1>>;
	 cut_thm "bool_cases"; allA <<y_1>>;
	 split_tac;
	 replace_tac ~asms:[!~1] ~f:(!! 1); flatten_tac;
	 replace_tac ~asms:[!~2] ~f:(!! 1); flatten_tac;
	 replace_tac ~asms:[!~ 1] ~f:(!! 1); 
	 replace_tac ~asms:[!~2] ~f:(!! 1); eq_tac;
	 split_tac;
	 replace_tac ~asms:[!~ 1] ~f:(!! 1); flatten_tac;
	 basic;
	 split_tac; 
	 basic;
	 replace_tac ~asms:[!~2] ~f:(!~4); flatten_tac;
	 split_tac;
	 replace_tac ~asms:[!~2] ~f:(!! 2); flatten_tac;
	 basic;
	 replace_tac  ~asms:[!~1] ~f:(!~3); flatten_tac;
	 replace_tac ~asms:[!~1; !~2] ~f:(!! 1); eq_tac])
      in 
      let iff_l3 = 
	prove_goal << !x y: (x iff y) iff (x = y) >>
	  (seq[
	   flatten_tac; unfold "iff" ~f:(!!1); 
	   conjC ; flatten_tac; 
	   cut iff_l2; allA <<x_1>>; allA <<y_1>>; 
	   implA; conjC; flatten_tac;  
	   implA; basic; basic; basic; basic;
	   flatten_tac;
	   replace_tac ~asms:[!~1] ~f:(!! 1);
	   conjC;
	   repeat (flatten_tac++basic)])
      in 
      prove_goal <<!x y: (x iff y) = (x = y)>>
      (seq [flatten_tac; cut iff_l2; 
	      allA <<x_1 iff y_1>>; 
	      allA <<x_1 = y_1>>;
	      split_tac; flatten_tac;
	      cut iff_l2; allA <<x_1>>; allA <<y_1>>;
	      unfold "iff" ~f:(!~2);
	      implA; basic; basic;
	      flatten_tac; 
	      replace_tac ~asms:[!~1] ~f:(!! 1); unfold "iff" ~f:(!! 1);
	      split_tac;
	      repeat (implC++basic);
	      basic])

    let iff_equals_ax = ref None
    let get_iff_equals_ax ()=
      match !iff_equals_ax with
	None -> 
	  let nthm = make_iff_equals_ax()
	  in 
	  iff_equals_ax := Some(nthm);
	  nthm
      | Some(x) -> x

(* 
   rule_true_ax:   !x: x = (x=true) 
 *)
    let make_rule_true_ax ()= 
      let rule_true_l1 =  prove_goal <<!x: (x=true) => x>> 
	(seq [flatten_tac; replace_tac ;trivial])
      in
      let rule_true_l2 = prove_goal <<!x: x => (x=true)>>
	(seq 
	   [flatten_tac; cut (lemma "bool_cases"); 
	    allA << x_1 >>; disjA; basic;
	    rewrite_tac [lemma "false_def"];
	    replace_tac ;
	    flatten_tac])
      in
      let rule_true_l3 = prove_goal <<! x: x iff (x=true)>>
	(seq 
	   [flatten_tac; unfold "iff" ~f:(!! 1);
	    conjC; cut rule_true_l2; unify_tac ~a:(!~1) ~c:(!! 1); 
	    cut rule_true_l1; unify_tac ~a:(!~1) ~c:(!! 1)])
      in 
      Logic.ThmRules.rewrite_conv (scope()) 
	[get_iff_equals_ax()] rule_true_l3

    let rule_true_ax = ref None

    let get_rule_true_ax ()= 
      match !rule_true_ax with
	None -> 
	  let nthm =make_rule_true_ax()
	  in 
	  rule_true_ax:=Some(nthm);
	  nthm
      | Some(t) -> t

(*
   rule_false_ax: !x: (not x) = (x=false)
 *)
    let make_rule_false_ax ()= prove_goal<<! x : (not x)=(x=false)>>
      (seq 
	 [ 
	   flatten_tac;
	   Logic.Rules.rewrite None
	     [Logic.RRThm(get_iff_equals_ax())] (!! 1);
	   cut_thm "bool_cases"; allA <<x_1>>;
	   unfold "iff" ~f:(!! 1); disjA;
	   replace_tac ~asms:[!~1] ~f:(!! 1);
	   conjC; 
	   flatten_tac;
	   replace_tac ~asms:[!~2] ~f:(!~1); flatten_tac;
	   replace_tac ~asms:[!~1] ~f:(!! 1);
	   conjC; 
	   flatten_tac;
	   eq_tac; 
	   flatten_tac])

    let rule_false_ax = ref None
    let get_rule_false_ax ()= 
      match !rule_false_ax with
	None -> 
	  let nthm =make_rule_false_ax()
	  in 
	  rule_false_ax:=Some(nthm);
	  nthm
      | Some(t) -> t

(*
   cond_rule_true_ax: !x y: (x=>y) = (x => (y=true))
 *)
    let make_cond_rule_true_ax()=
      prove_goal << !x y: (x=>y) = (x => (y=true)) >>
      (seq [flatten_tac;
	      cut (get_rule_true_ax());
	      allA <<y_1>>;
	      replace_tac ~asms:[!~1] ~f:(!! 1);
	      eq_tac])


    let cond_rule_true_ax = ref None
    let get_cond_rule_true_ax ()= 
      match !cond_rule_true_ax with
	None -> 
	  let nthm =make_cond_rule_true_ax()
	  in 
	  cond_rule_true_ax:=Some(nthm);
	  nthm
      | Some(t) -> t

(*
   cond_rule_false_ax: !x y: (x=>~y) = (x => (y=false))
 *)
    let make_cond_rule_false_ax()=
      prove_goal << !x y: (x=>(not y)) = (x => (y=false)) >>
      (seq [flatten_tac;
	      cut (get_rule_false_ax());
	      allA <<y_1>>;
	      replace_tac ~asms:[!~1] ~f:(!! 1);
	      eq_tac])


    let cond_rule_false_ax = ref None
    let get_cond_rule_false_ax ()= 
      match !cond_rule_false_ax with
	None -> 
	  let nthm =make_cond_rule_false_ax()
	  in 
	  cond_rule_false_ax:=Some(nthm);
	  nthm
      | Some(t) -> t




(** [asm_rewrite info thm tg g]:

   Rewrite assumption [tg] with rule [thm] = |- a=b

   tg:a, asms |- concl
   -->
   tg:b, asms |- concl

   info: [] [tg] []
 *)
    let asm_rewrite info thm tg g=
	Logic.Rules.rewrite
	  info 
	  [Logic.RRThm(thm)] (Drule.ftag tg) g

(** [thm_rewrite scp rl thm]:
   Rewrite theorem [thm] with rule [rl]=|- a=b in scope [scp]
   |- a -->  |- b
 *)
    let thm_rewrite scp rl thm=
      Logic.ThmRules.rewrite_conv scp [rl] thm

(** [many_conj_conv thm]:
   Break conjunctions in theorem [thm] to a list of theorems

   |- a and b and c and 
   -->
   |- a ; |- b ; |- c ; ..
 *)

    let many_conj_conv thm=
      let rec many_aux t ths=
	if(Formula.is_conj (Logic.dest_thm t))
	then 
	  match (Logic.ThmRules.conjE_conv t) with 
	    [a; b] ->
	      many_aux a (many_aux b ths)
	  | _ -> raise 
		(Logic.logicError "many_conj_conv: unknown error" 
		   [Logic.dest_thm t])
	else t::ths
      in many_aux thm []


(** [negate_concl info t g]:
   Copy conclusion with tag [t], 
   negate it, making it assumption tagged [t'].

   asms|- t:c, cncl
   -->
   t':~c, asms |- t:c, cncl
   info [] [t'] []
 *)
    let negate_concl info t g=
      let ctrm = 
	Formula.dest_form 
	  (Logic.drop_tag (Logic.Sequent.get_tagged_cncl t (Logic.get_sqnt g)))
      in 
      let newtrm = Logicterm.mk_not ctrm
      in 
      let inf1, g1=
	apply_tag (fun x -> cases_full_tac (Some(x)) newtrm) g
      in 
      let ogt, ngt=
	Lib.get_two inf1.Logic.goals  
	  (Logic.logicError "negate_concl failed creating subgoal" [])
      and ntag=Lib.get_one inf1.Logic.forms
	  (Logic.logicError "negate_concl failed creating formula" [])
      in 
      let g2=
	Tactics.seq
	  [Logic.Rules.negC None (Logic.FTag ntag);
	   Logic.Rules.basic None (Logic.FTag ntag) (Logic.FTag t)]
	  g1
      in 
      if (sqnt_solved ngt g2)
      then 
	(Logic.add_info info [] [t] [];
	 g2)
      else raise (Logic.logicError "negate_concl failed" [])


(** [test_apply_tac info ttacs tg goal]

   [ttacs] is a list of test-tactic pairs.
   Each test is applied to asm tagged [tg] in first subgoal of [goal].
   The tactic [tac] of the first test to suceed is applied to [goal].
   [info] is passed to [tac].
   raises Not_found if no tactic matches.
*)

    let test_apply_tac info ttac tg goal=
      let rec app_aux ts=
	match ts with 
	  [] -> raise Not_found
	| (tst, tac)::tts ->
	    if(tst 
		 (Formula.dest_form
		    (Logic.drop_tag 
		       (Logic.Sequent.get_tagged_asm tg 
			  (Logic.get_sqnt goal)))))
	    then 
	      tac info tg goal
	    else 
	      app_aux tts 
      in 
      app_aux ttac 


(** Tests on theorems *)

(** [is_many_conj thm]:
   test [thm] is of the form |- a and b and ... and z 
 *)
    let is_many_conj thm=
      let thmtrm=Formula.dest_form (Logic.dest_thm thm)
      in 
      Logicterm.is_conj thmtrm

(** [is_iffterm (vars, cnd, main)]: 
   true if [main] is of the for [a iff b] 
 *)
    let is_iffterm (vars, cnd, main) =
      (try(fst(Term.dest_fun main) = Logicterm.iffid) with _ -> false)

(** [is_negation (var, cnd, main):  
   true if [main] is of the form [not a]
 *)
    let is_negation (vars, cnd, main)=
      Logicterm.is_neg main

(** [is_equality (var, cnd, main): 
   true if [main] is of the form a=b 
 *)
    let is_equality (vars, cnd, main)=
      Logicterm.is_equality main


(** [prep_asm_rule info tg goal]:

   Prepare assumption [tg] to be used as a rewrite rule.
*)

(** [is_cond_fact t]
   [t] is of the form [a=>b] where [b] isn't an equality
*)
    let is_cond_true_fact trm=
      let (_, t)=Term.strip_qnt Basic.All trm 
      in 
      if(Logicterm.is_implies t)
      then 
	(let (_, args)=Term.dest_fun t
	in 
	let (asm, cncl)=Lib.get_two args (Not_found)
	in 
	not((Logicterm.is_equality cncl) or (Logicterm.is_neg cncl)))
      else false

    let is_cond_false_fact trm=
      let (_, t)=Term.strip_qnt Basic.All trm 
      in 
      if(Logicterm.is_implies t)
      then 
	(let (_, args)=Term.dest_fun t
	in 
	try 
	  let (asm, cncl)=Lib.get_two args (Not_found)
	  in 
	  (Logicterm.is_neg cncl)
	with _ -> false)
      else false

    let is_false_fact trm= 
      let (_, t)=Term.strip_qnt Basic.All trm 
      in 
      Logicterm.is_neg t

    let is_true_fact t= true

    let cond_true_asm_to_rule info tg g=
      asm_rewrite info (get_cond_rule_true_ax()) tg g

    let cond_false_asm_to_rule info tg g=
      asm_rewrite info (get_cond_rule_false_ax()) tg g

    let false_asm_to_rule info tg g=
      asm_rewrite info (get_rule_false_ax()) tg g

    let true_asm_to_rule info tg g=
      asm_rewrite info (get_rule_true_ax()) tg g


    let asm_rule_makers=
      [
       (is_cond_false_fact, cond_false_asm_to_rule);
       (is_cond_true_fact, cond_true_asm_to_rule);
       (is_false_fact, false_asm_to_rule);
       (is_true_fact, true_asm_to_rule)
     ]

    let prep_asm_rule info tg g=
      test_apply_tac info asm_rule_makers tg g

  end

