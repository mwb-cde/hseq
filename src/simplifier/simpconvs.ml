(* 
   Function to prepare theorems and assumptions being added to a simpset.
*)

module Simpconvs =
  struct

    open Simputils

(**
   [simple_rewrite_conv scp rule trm]

   Form an equality from term [trm=!x .. y: body] and [rule=(l=r)] by
   descending through topmost universal quantifiers of [trm] and
   applying rewrite once only to the body of [trm]. Return the theorem
   stating the equality [trm=(!x.. y: (body=r))].

   e.g 
   [rewrite_simple << ! a: a = true >> << !x y z: (f x y z) >>]
   ->
   [ << (!x y z: f x y z) = (! x y z: (f x y z = true)) >> ]
 *)
let simple_rewrite_conv scp rule trm=
  let rule_trm =(Formula.dest_form (Logic.dest_thm rule))
  in 
  let rvars, rbody = 
    Term.strip_qnt Basic.All rule_trm
  in 
  let rlhs, rrhs = Logicterm.dest_equality rbody
  in 
  let trmvars, trmbody = Term.strip_qnt Basic.All trm
  in 
  let info = Drule.mk_info()
  in 
  let env=Unify.unify scp (Rewrite.is_free_binder rvars) rlhs trmbody
  in 
  let new_goal =
    Drule.rebuild_qnt Basic.All trmvars 
       (Logicterm.mk_equality trmbody (Term.subst env rrhs))
  in 
  let proof g= 
    (let g1=repeat (Logic.Rules.allC None (fnum 1)) g
    in let g2= foreach (Logic.Rules.cut (Some info) rule) g1
    in let g3=
      let atag = 
	Logic.FTag(Lib.get_one (!info.Logic.forms) (Failure "get_one"))
      and ctag = Logic.FNum 1
      in 
      foreach 
	((fun n -> 
	  (Drule.inst_list 
	     (Logic.Rules.allA None) 
	     (Drule.unify_concl_for_consts Basic.All ~c:ctag rule_trm n)
	     atag) n)
	   ++ (Logic.Rules.basic None atag ctag)) g2
    in g3)
  in 
  Goals.prove_goal scp new_goal proof;;

(**
   [simple_rewrite_rule scp rule thm]

   Apply [simple_rewrite_conv] to theore [thm].
*)
let simple_rewrite_rule scp rule thm=
  simple_rewrite_conv scp rule (Formula.dest_form (Logic.dest_thm thm))


(**
   [simple_asm_rewrite_tac scp info rule asm]

   Rewrite assumption [asm] with [rule] by descending through topmost
   universal quantifiers and applying rewrite once only to the body of
   [asm].  i.e.
   
   rule=|- asm = rhs
   t:asm, A |- C 
   -->
   t:rhs, A |- C
 *)
    let simple_asm_rewrite_tac rule asm node=
      let sqnt=Drule.sequent node
      in 
      let (_, f)=Logic.get_label_asm asm sqnt
      and scp = Drule.scope_of node
      in 
      let thm=simple_rewrite_conv scp rule (Formula.dest_form f)
      in 
      once_rewrite_tac [thm] ~f:asm node


(* 
   iff_equals_ax:  !x y: (x iff y) = (x = y)

   make_n_ax(): prove theorem n
   get_n_ax(): get theorem n, proving it if necessary
 *)

let make_iff_equals_ax ()=
  let iff_l1 = prove_goal << !x y: (x = y ) => (x => y) >>
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

(* Previously in  simpset.ml *)

(* needed properties *)

(*
   let iff_equals_ax = saxiom "! a b: (a iff b)=(a=b)"
   let rule_false_ax = saxiom "! a : (not a)=(a=false)"
   let rule_true_ax = saxiom "! a : a=(a=true)"
 *)

(* prepare simplifier rules *)

(*
   Two types of rule: theorems and assumptions 
   for both: a formula f is transformed to T(f) as follows:

   T(x and y) = T(x), T(y)
   T(x iff y) = T(x=y), if this results in a rewrite rule
   T(not x) = x=false
   T(x) = x=true

   conditional formulas: 
   T(c=>x) transformed as above 
   except 
   T(c=>(x and y)) = c=>(x and y)

   rewrite rules:
   T(x=y) = x=y, if all variables in y also occur in x 
   = (x=y)=true

   T(c=>x=y) 
   = c=>(x=y), if all variables in y and c occur in x
   = c=>((x=y)=true), if variables in y don't occur in x 
   and all variables in c occur in x
   = (c=>x=y)=true, otherwise
 *)


(* utility functions *)


(** [find_variables is_var vars trm]
   find all subterms [t] of [trm] s.t. [(is_var t)] is true.
   add [t] to [vars]
   return [vars]
*)
    let find_variables is_var vars trm=
      let rec find_aux env t=
	match t with
	  Basic.Qnt(_, _, b) ->
	    find_aux env b
	| Basic.Bound(q) ->
	    if(is_var q)
	    then 
	      (try ignore(Term.find t env); env
	      with 
		Not_found ->
		  (Term.bind t t env))
	    else env
	| Basic.Typed(tr, _) -> find_aux env tr
	| Basic.App(f, a) -> 
	    let nv=find_aux env f
	    in find_aux nv a
	| _ -> env
      in find_aux vars trm

(** [check_variables is_var vars trm]
   check that all subterms [t] of [trm] s.t. [is_var t] 
   is in [vars]
*)   
    let check_variables is_var vars trm=
      let rec check_aux t=
	match t with
	  Basic.Qnt(_, _, b) ->
	    check_aux b
	| Basic.Bound(q) ->
	    if(is_var q)
	    then 
	      ignore(Term.find t vars)
	    else ()
	| Basic.Typed(tr, _) -> check_aux tr
	| Basic.App(f, a) -> check_aux f; check_aux a
	| _ -> ()
      in check_aux trm

(**  [is_rr_rule qs c l r]: 

   Check that [c=>(l=r)] is a rewrite rule.

   All variables (in [qs]) occuring in [c] or [r] must also occur in [l]
   return: [(cnd, rhs)]
   where 
   [cnd]= [Some(true)] iff all variables in [c] occur in [l]
      = [None] if no condition
   [rhs]= [Some(true)] iff all variables in [r] occur in [l]
      = [None] if no [rhs]
 *)
    let is_rr_rule qs c l r=
      let is_var b=List.mem b qs
      and rret=ref (Some true)
      and cret=ref (Some true)
      in
      let vars=find_variables is_var (Term.empty_subst()) l
      in 
      (* check rhs *)
      (try
	match r with 
	  None -> rret:=None | 
	  Some (rhs) -> check_variables is_var vars rhs;
      with Not_found -> rret:=(Some false));
      (* check cond (if any) *)
      (try
	(match c with 
	  None -> rret:=None 
	| Some(cnd) -> check_variables is_var vars cnd)
      with Not_found -> cret:=(Some false));
      (!cret, !rret)

(* [dest_rr_rule trm]: 
   Split rule [trm] into binders, condition, lhs, rhs 
   rules are of the form:
   [c=>(l=r)] or [l=r]
 *)
    let dest_rr_rule trm =
      (* Get leading quantifiers *)
      let (qs, t1)=strip_qnt (Basic.All) trm 
      in 
      (* test for conditional equalities *)
      let (cnd, rl)=
	if (is_implies t1)         
	then (* is conditional *)
	  (let (asm, cncl)=dest_implies t1
	  in 
	  (Some(asm), cncl))
	else  (* is not conditional *)
	  (None, t1)
      in 
      (* break the equality *)
      if (Logicterm.is_equality rl)
      then 
	let (lhs, rhs)=Logicterm.dest_equality rl
	in (qs, cnd, lhs, rhs)
      else 
	  raise (Failure 
		   ("Not an equality or a conditional equality\n"))

(** [strip_qnt_cond trm]
   split rule [trm] into variable binders, condition, equality
   rules are of the form:
   a=>c
   c
 *)
    let strip_qnt_cond t =
      let (qs, t1)=strip_qnt (Basic.All) t  (* get leading quantifiers *)
      in 
      if (is_implies t1)         (* deal with conditional equalities *)
      then 
	(let (_, args)=dest_fun t1
	in 
	match args with
	  [a; c] -> (qs, Some a, c)
	| _  -> 
	    raise 
	      (termError "strip_qnt_cond: not a valid implication" [t]))
      else
	(qs, None, t1)


(* conversions for manipulating rules *)


(** [term_cond_rewrite scp rl fm]:
   for rewrite rule [rl]=|-l=r,
   rewrite conditional term [fm]= c=>a=l to c=>a=r
   in scope [scp]
 *)
    let term_cond_rewrite scp rl fm =
      let qs, cnd, qb=strip_qnt_cond fm
      in 
      let rrtrm = Formula.dest_form (Logic.dest_thm rl)
      in 
      rebuild_qnt Basic.All qs 
	(Logicterm.mk_implies (dest_option cnd)
	   (Rewrite.rewrite scp
	      (Rewrite.control 
		 ~dir:Rewrite.rightleft
		 ~strat:Rewrite.BottomUp
		 ~max:None) [rrtrm] qb))
	
(** [form_cond_rewrite scp rl fm]:
   for rewrite rule [rl]=|-l=r,
   rewrite conditional formula [fm]= c=>a=l to c=>a=r
   in scope [scp]
 *)
    let form_cond_rewrite scp rl fm =
      Formula.mk_form scp 
	(term_cond_rewrite scp rl (Formula.dest_form fm))

(* functions to make simp rules from assumptions *)


    let monitor = ref None

(* make_entry t:
   y -> y=true
   x=y -> x=y        (if a rr rule)
   -> (x=y)=true (otherwise)
   c=>x=y -> c=>x=y        (if a rr rule)
   -> (c=>x=y)=true (otherwise)
 *)   


(* addition of rules *)

(** [is_valid_rule rl]:
   true if [th] is a valid rule
*)

(** [make_rule src]:
   make rule from theorem or assumption [src] in scope [scp]
*)
    let make_rule rl=
      match rl with
	Logic.RRThm(th) -> 
	  let qs, c, l, r=
	    dest_rr_rule (Formula.dest_form (Logic.dest_thm th))
	  in 
	  (qs, c, l, r, rl)
      | _ -> failwith "make_rule: can only handle theorems"


(** [add_simp_rule scp set rls]
   add (properly formed rules) [rls] to set [set]
   in scope [scp]
*)
    let add_simp_rule scp sset src =
      let nset=ref sset
      in 
      let rec add_aux thms =
	match thms with
	  [] -> !nset
	| rl::ts -> 
	    nset:=add_rule (make_rule rl) (!nset);
	    add_aux ts
      in 
      add_aux src


(*
    let add_sqnt_asms sset asms sqnt =
      let nset=ref sset
      in 
      let rec get_asms nasms bl= 
	match nasms with 
	  [] -> List.rev bl
	| ((_, (_, _, _, _, Logic.Tagged(t))) ::na) 
	  -> get_asms nasms (t::bl)
	| _ -> failwith "add_sqnt_asms.get_asms"
      in 
      let rec add_aux asml =
	match asml with
	  [] -> ()
	| rl::ts -> 
	    nset:=add_rule rl (!nset);
	    add_aux ts
      in 
      let (ents, ng)=make_asm_entry asms [] sqnt
      in 
      add_aux ents;
      (!nset, get_asms ents, ng)
*)


end

(* Previously in simpset.ml *)

   open Simpset;;


   let scope () = Tpenv.scope();;

   let saxiom str = 
   Logic.mk_axiom 
   (Formula.form_of_term (scope()) (Tpenv.read str));;

   let get_tags asms g=
   let sqnt=Logic.get_sqnt g
   in 
   List.map (fun i -> Logic.Sequent.index_to_tag i sqnt) asms;;

(*
   let mk_entry asm g=
   make_asm_entry (get_tags asm g) [] g;;
*)   
(*
   let mk_goal str= 
   Logic.mk_goal (scope()) 
   (Formula.mk_form (scope()) (read str));;

   goal <<(!a b: a=>b) => true>>;;
   by  implI;;
   let gl=curr_goal(top());;
   let sqnt=curr_sqnt gl;;
   let thm=saxiom "!a: a= (a=true)";;
   let scp=Logic.scope_of sqnt;;
   let info=ref (Logic.Rules.make_tag_record [][][]);;
   let ftg=Logic.index_to_tag (-1) sqnt;;

   let g= implI (mk_goal "(!a b: a iff b) => true");;
*)

   let scp=scope();;

   let rl=saxiom "!a: a= (a=true)";;
   let thm=saxiom"(!a b: a=>b)";;


