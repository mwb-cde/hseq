module Simplifier =
  struct

    open Basic
    open Term
    open Logicterm

    open Simputils
    open Simpconvs
    open Simpset

    class simpError s ts =
      object (self)
	inherit Result.error s
	val trms = (ts :term list)
	method get() = trms
	method print st = 
	  Format.open_box 0; 
	  print_string "Simplifier Error: ";
	  print_string ((self#msg())^" "); 
	  Format.print_newline();
	  Format.open_box 0; 
	  Printer.print_sep_list ((Term.print st), ",")
	    (self#get());
	  Format.close_box();
	  Format.close_box();
      end

    let mk_error s t = Result.mk_error((new simpError s t):>Result.error)
    let add_error s t e =
      Result.add_error e (mk_error s t) 

    exception No_change

(* [type control]
   Information used by and built up during simplification.
 *)
    type control =
	{
(** conds: max. no. of conditions to try and prove at once *)
	 conds: int;  
(** rr_depth: max. no. of rr rules to apply at one level *)
	 rr_depth: int;
(** asms: assumptions generated during the course of simplification *)
	 asms: Tag.t list;
(** rules: 
   rewrite rules to pass to the rewriter (the result of the simplifier)
*)
	 rules: Logic.rr_type list
       }

    let mk_cntrl cd rd a rs= 
      {
       conds=cd;
       rr_depth=rd;
       asms=a;
       rules=rs
     }

    let cntrl_set_conds cntrl d=
      {cntrl with conds=d}

    let cntrl_set_rr_depth cntrl d=
      {cntrl with rr_depth=d}

    let cntrl_set_asms cntrl ds=
      {cntrl with asms=ds}

    let cntrl_set_rules cntrl ds=
      {cntrl with rules=ds}

    let cntrl_add_asm cntrl a=
      cntrl_set_asms cntrl (a::(cntrl.asms))


    let cntrl_dec_cond_depth cntrl=
      cntrl_set_conds cntrl ((cntrl.conds)-1)

    let cntrl_add_rule cntrl r=
      cntrl_set_rules cntrl (r::(cntrl.rules))

(** [mk_control()]
   The default control information 
*)
    let mk_control ()= mk_cntrl 50 50 [] [] 

(* simplifier actions:

   simple actions (no conditional rewrites):
   recurse through the structure of a term, 
   getting a list of the rewrite rules to be applied

   always:
   recurse through the structure of a term, 
   getting a list of the rewrite rules to be applied
   for each subterm:
   get a (un)conditional rule which could be applied
   if conditional, try to show the condition=true 
   discarding the rule on failure
   if successfull apply the rule.
 *)

(* utility functions *)
(* strip_rrs: prepare for direct rewriting of term *)
(* for tests only  *)

    let strip_rrs rrs=
      let strip_rr x = 
	match x with
	  Logic.RRThm x -> Formula.dest_form (Logic.dest_thm x)
	|	 _ -> failwith "simp_tac"
      in 
      (List.map strip_rr rrs)

(* utility tactics *)


(* repeat_for_sqnt: apply a tactic to a single sequent *)

    let repeat_for_sqnt rl g=
      if not(Logic.has_subgoals g)
      then raise No_change
      else 
	(let chng=ref false
	and st=Logic.sqnt_tag (Logic.get_sqnt g)
	in 
	let rec rule_aux fg =
	  let chng_aux=ref false
	  in 
	  if(sqnt_solved st fg)
	  then fg
	  else 
	    (let ng=
	      try 
		let tmp=rl fg
		in 
		chng_aux:=true; tmp
	      with _ -> fg
	    in 
	    if !chng_aux 
	    then (chng:=true; rule_aux ng)
	    else ng)
	in 
	let ret=rule_aux g
	in 
	if (!chng) then ret
	else raise No_change)
	  

(** [prep_cond_tac qs env thm g]: 
   Prepare conditional rule [thm] for use in rewriting.

   Push theorem (copy an assumption) into sequent.
   Instantiate outermost quantifiers [qs] with values from [env].
   Apply implE to create new sequent with condition as conclusion 
   to prove.
 *)

(** [allE_list i vs g]
   Instantiate formula [i] of goal [g] with value [vs]
*)
    let allE_list i vs g =
      let rec inst_aux xs sq=
	match xs with 
	  [] -> sq
	| (c::cs) -> 
	    let nsq=Logic.Rules.allE c i sq
	    in 
	    inst_aux cs nsq
      in 
      inst_aux vs g

    let make_consts qs env = 
      List.map (fun q-> Term.find (Bound q) env) qs

(** [cut_rr_rule info t g]
   Put rule [t] into first subgoal of [g].
   If [t] is a theorem, it is cut into the goal.
   If [t] is an assumption, it is copied.

   thm: c=>a=b

    asms|-cncl
   ->
   tl| asms |- fl:c, cncl
   tr| tr: fr:a=b, asms |- cncl

   return 
   ([tl; tr], [fl; fr], g')
   where g' is the new goal.
*)
    let cut_rr_rule info t g =
      match t with
	Logic.RRThm(th) ->
	  Logic.Rules.cut_info info th g
      | Logic.Asm(x) ->
	  Logic.Rules.copy_asm_info info 
	    (Logic.label_to_index x (Logic.get_sqnt g)) g

(*
      | Logic.Tagged(tg) ->
	  Logic.Rules.copy_asm_info info 
	    (Logic.tag_to_index tg (Logic.get_sqnt g)) g
*)

    let prep_cond_tac cntrl values thm g =
      let info = ref (Logic.Rules.make_tag_record [] [] [])
      in
      try 
	let g1=cut_rr_rule info thm g
	in 
	let rrftg= get_one (!info).Logic.Rules.forms (No_change)
	in 
	let g2=allE_list (Logic.tag_to_index rrftg (Logic.get_sqnt g1)) 
	    values g1
	in 
	let g3=Logic.Rules.implE_info info 
	    (Logic.tag_to_index rrftg (Logic.get_sqnt g2)) g2
	in 
	let (cgltg, rgltg)= 
	  get_two (!info).Logic.Rules.goals (Failure "prep_cond_tac: goals")
	in 
	let cftg=
	  get_one (!info).Logic.Rules.forms (Failure
					       "prep_cond_tac: forms")
	in 
	let ncntrl= cntrl_add_asm cntrl rrftg
	in 
	(ncntrl, [cgltg;rgltg], [cftg; rrftg], g3)
      with _ -> raise No_change

(** 
   [prove_cond_rule tac values entry g]

   Prepare a simpset entry [entry] for use in rewriting.
   
   If [entry] is unconditional, return immediatly.
   Otherwise, use [prep_cond_tac] to  create a subgoal from the
   condition; apply tactic [tac] to prove the condition; fail
   if [tac] fails. 

   Return RRInfo of the theorem/assumption to use as i rewriting
   and new goal.
*)
    let rec prove_cond_rule cntrl tac values entry g = 
      let (qs, cnd, _, _, thm)=entry
      in 
      match cnd with
	None -> (cntrl, thm, g)
      | Some(cd) -> 
	  let (ncntrl, glinfo, forminfo, ng)=
	    prep_cond_tac cntrl values thm g
	  in 
	  let (cgltg, rgltg)=
	    get_two glinfo (Failure "prove_cond_rule: 1")
	  and (cftg, rftg)=
	    get_two forminfo (Failure "prove_cond_rule: 1")
	  in 
	  let ng1=Logic.goal_focus cgltg ng
	  in 
	  let ng2= 
	    try 
	      (tac ncntrl cftg ng1) 
	    with _ -> ng1
	  in 
	  if(sqnt_solved cgltg ng2)
	  then 
	    (ncntrl, Logic.Asm(Drule.ftag rftg), ng2)
	  else raise No_change

(*
   match_rewrite:
   try to match lhs with trm, return rhs if sucessful
*)
    let match_rewrite scp tyenv tenv varp lhs rhs trm = 
      try
	(let ntyenv, nenv=
	  Unify.unify_fullenv_rewrite scp tyenv tenv varp lhs trm
	 in 
	 (ntyenv, nenv, Term.subst tenv rhs))
      with x -> (failwith "match_rewrite")


(** [find_basic scp tyenv tac rl trm g]
   try to match rule [rl] with term [trm] in goal [g].
   if [rl] matches but is conditional, try to prove the condition
   using tactic [tac].

   returns rewritten term, matched rules and new goal.
*)
    let find_basic cntrl tyenv tac rl trm g=
	let (qs, c, lhs, rhs, thm)=rl
	in 
	let tenv=Term.empty_subst()
	in 
	let scp=Logic.scope_of (Logic.get_sqnt g)
	in 
	let (ntyenv, ntenv, nt)=
	  match_rewrite scp tyenv tenv
	    (Rewrite.is_free_binder qs) lhs rhs trm
	in 
	let values=make_consts qs ntenv
	in 
	let (ncntrl, rr, ng)=prove_cond_rule cntrl tac values rl g
	in 
	(ncntrl, nt, rr, ng)

(** [find_match scp tyenv rslt set tac trm g]
   Find rule in simpset [set] which match term [trm] in goal [g].

   If a possible match is conditional, try to prove the condition 
   using tactic [tac].

   returns rewritten term, matching rule and new goal.
   raise No_change if no matches.
*)
      let find_match cntrl tyenv set tac trm goal=
	let rec find_aux rls t g= 
	  match rls with
	    [] -> raise No_change
	  | (rl::nxt) ->
	      try 
		find_basic cntrl tyenv tac rl t g
	      with _ -> find_aux nxt t g
	in 
	find_aux (lookup set trm) trm goal


(** [find_all_matches scp tyenv rslt set tac trm g]
   Find all rules in simpset [set] which can be used to rewrite
   term [trm] in goal [g].

   puts matched rules into rslt
   returns rewritten term, matched rules and new goal.
*)
      let rec find_all_matches cntrl tyenv set tac trm goal=
	let chng = ref false
	in 
	let rec find_aux c t g= 
	  try 
	    (let (ncntrl, nt, nr, ng)=
	      find_match c tyenv set tac t g
	    in 
	    chng:=true;
	    find_aux (cntrl_add_rule cntrl nr) nt ng)
	  with _ -> (c, t, g)
	in 
	let (nc, nt, ng)=find_aux cntrl trm goal
	in 
	if(!chng)
	then (nc, nt, ng)
	else raise No_change

(**
   [simp_prep_tac [control] i g]: 
   Prepare goal [g] for the simplifier.
   Formula to be simplified is tagged [i].
   returns
   [(ncontrol, ng)]
   where 
   [ng] is the prepared goal
   [ncontrol]
     is the new control recording formulas added/modified by simp_prep_tac

   N.B.
   Currently this does nothing except strip the quantifiers off
   formula [i].
*)
    let simp_prep_tac control tag g= 
      let sqnt= Logic.get_sqnt g
      in 
      let is_asm =
	try
	  (ignore(Logic.get_tagged_asm tag sqnt); true)
	with _ -> false
      in 
      let fid = Logic.FTag tag
      in 
      let (newcontrol, newgoal)=
	if(is_asm)
	then 
	  try 
	    (control, Tactics.repeat (Logic.Rules.existI_full None fid) g)
	  with _ -> (control, g)
	else 
	  try 
	    (control, Tactics.repeat (Logic.Rules.allI_full None fid) g)
	  with _ -> (control, g)
      in 
      (newcontrol, newgoal)

(* simp_tac: toplevel for simplifier

   basic_simp_tac: workhorse of simplifier
   basic_simp_tac cntrl:control set:rule_set ft:Tag.t st:Tag.t g:goal
   where ft is tag of formula to work on
   and st is tag of sequent to work on
 *)
    let is_true t = Term.is_true t

    let rec basic_simp_tac cntrl set ft goal=
      let prove_cond_tac ctrl tg g=
	let init_simp_tac ctrl0 tg0 g0=
	  let (ctrl1, g1) = simp_prep_tac ctrl0 tg0 g0
	  in 
	  basic_simp_tac ctrl1 set tg0 g1
	in 
	Tactics.orl
	  [
	   Tactics.thenl
	     [
	      init_simp_tac ctrl tg;
	      Logic.Rules.trueR_full None (Logic.FTag tg)
	    ];
	   Logic.Rules.trueR_full None (Logic.FTag tg)] g
      in 
      let sqnt=Logic.get_sqnt goal
      in 
      let chng=ref false
      in 
      let tyenv=Gtypes.empty_subst()
(* 
   find_rrs: scope -> Term.substitution -> rule_set -> term
   make list of rewrite rules using unification
   Term.substitution is used to keep track of changing term
   as rewrites are found
*)
      in 
      let rec find_rrs ctrl t g=
	match t with
	  Basic.Qnt(k, q, b) -> 
	    (let (bcntrl, nb, bg) = find_rrs ctrl b g
	    in 
	    try 
	      find_all_matches bcntrl tyenv set 
		prove_cond_tac (Qnt(k, q, nb)) bg
	    with No_change -> (bcntrl, Qnt(k, q, nb), bg))
	| Basic.Typed(tt, ty) -> find_rrs ctrl tt g
	| Basic.App(f, a)->
	    (let (fcntrl, nf, nfg) = (find_rrs ctrl f g)
	    in 
	    let (acntrl, na, nag)= (find_rrs fcntrl a nfg)
	    in 
	    try 
	      find_all_matches acntrl tyenv set 
		prove_cond_tac (App(nf, na)) nag
	    with No_change -> (acntrl, App(nf, na), nag))
	| _ -> 
	    (try 
	      find_all_matches ctrl tyenv set prove_cond_tac t g
	    with No_change -> (ctrl, t, g))
      in
      let trm=
	Formula.dest_form(Logic.drop_tag(Logic.get_tagged_form ft sqnt))
      in 
      let (ncntrl, ntrm, ngoal)= find_rrs cntrl trm goal
      in 
      let rrs=List.rev (ncntrl.rules)
      in 
      if rrs=[]
      then raise No_change
      else 
	(try
	  Logic.Rules.rewrite rrs (Logic.tag_to_index ft sqnt) ngoal
	with _ -> raise No_change)

(*
   initial flatten
   flatten a goal, try to prove trivial (true/false) facts
 *)


(*
   let initial_flatten ft g=
   let conc_elims =
   [Logic.Rules.trueR ft;
   Logic.Rules.allI ft]
   and asm_elims = 
   [	Bool_tacs.false_rule0 ft;
   Logic.Rules.conjE ft; 
   Logic.Rules.existI ft]
   in 
   repeat_for_sqnt 
   (Tactics.orl 
   [Tactics.orl conc_elims;
   Tactics.orl asm_elims]) g
 *)

(* simp_flatten_tac fts g:
   flatten all except formulas with tag in fts
   try to prove trivial facts, 
   put conclusions into assumptions (by negation)
 *)

(*
    let simp_flatten_tac fts g=
      let asm_elims = 
	[	(Formula.is_false, false_rule0);
	  (Formula.is_conj, Logic.Rules.conjE); 
	  (Formula.is_exists, Logic.Rules.existI)]
      and conc_elims =
	[
	 (Formula.is_true, Logic.Rules.trueR);
	 (Formula.is_neg, Logic.Rules.negC); 
	 (Formula.is_disj, Logic.Rules.disjE);
	 (Formula.is_implies, Logic.Rules.implI);
	 (Formula.is_all, Logic.Rules.allI)]
      in 
      repeat_for_sqnt
	(Tactics.orl 
	   [ Drule.foreach_conc_except fts conc_elims; 
	     Drule.foreach_asm_except fts asm_elims]) g

*)



(* final_flatten_tac fts g:
   flatten sequent after simplification
   flatten all except formulas with tag in fts
   try to prove trivial facts, 
 *)


(* simp_tac i st:
   - flatten sequent
   - put conclusions into assumption by negation (** not done yet **)
   - make assumption entries
   - simplify
   - delete temporary assumptions
   - flatten sequent
 *)

(*
   let simp_tac set i g =
   let sqnt=(Logic.get_sqnt g)
   in 
   let st=Logic.sqnt_tag sqnt
   and ft=Logic.index_to_tag i sqnt
   in 
   basic_simp_tac set ft st g
 *)

(* inital_flatten_tac fts g:
   prepare sequent for simplification
   flatten all except formulas with tag in fts
   try to prove trivial facts, 
   put conclusions into assumptions (by negation)
 *)


      let simp_asm_elims = 
	[(Formula.is_false, false_rule0);
	  (Formula.is_conj, Logic.Rules.conjE); 
	  (Formula.is_neg, Logic.Rules.negA); 
	  (Formula.is_exists, Logic.Rules.existI)]

      let simp_conc_elims =
	[
	 (Formula.is_true, Logic.Rules.trueR);
	 (Formula.is_disj, Logic.Rules.disjE);
	 (Formula.is_all, Logic.Rules.allI)]

     let initial_flatten_tac fts goal=
      (repeat_for_sqnt 
	  (Tactics.orl
 	     [ Drule.foreach_conc_except fts simp_conc_elims;
	       Drule.foreach_asm_except fts simp_asm_elims]) goal)


(* 
   full_simp_tac cntrl sset tg gl
   cntrl: control
   sset: simpset to use
   tg: tag formula to simplifier
   gl: goal

   simplifies formula tg in the first subgoal of goal

   raises
   Not_found if no formula tagged tg in subgoal
   No_change if not change is made
 *)
    let full_simp_tac cntrl simpset tg gl=
      let chng=ref false
      in
      (* get the first sequent *)
      let sqnt = 
	try (Logic.get_sqnt gl)
	with _ -> raise (Result.error "full_simp_tac: No such formula in goal")
      in 
      (* prepare the subgoal for simplification *)
      let (prepared_cntrl, prepared_goal) = 
	(try 
	  let tmp=simp_prep_tac cntrl tg gl
	  in (chng:=true; tmp)
	with 
	  No_change -> (cntrl, gl)
	| err -> 
	    raise 
	       (Result.error "simp_tac: stage 1"))
      in 
      (* invoke the simplifier *)
      let simped_goal = 
	(try 
	  (basic_simp_tac prepared_cntrl simpset tg prepared_goal)
	with No_change -> (chng:=false; gl))
      in 
      (* clean up afterwards *)
      let ret_goal=simped_goal
      in 
      if(!chng) 
      then ret_goal
      else raise No_change

(* tests *)

    let simp_set = ref(empty_set())

(* add_simp f: add theorem thm to simp_set *)

    let add_simp thms = 
      simp_set:=
	add_simp_rule (scope()) (!simp_set) 
	  (List.map (fun x -> Logic.RRThm x) thms)

(* empty_simp: empty simp set *)
    let empty_simp () = simp_set:=empty_set()

(* user-level version of full_simp_tac *)
    let simp_tac ?(i=1) gl=
      let cntrl = mk_control()
      in 
      let tg=Logic.index_to_tag i (Logic.get_sqnt gl)
      in 
(*
      basic_simp_tac cntrl (!simp_set) tg gl
*)
      full_simp_tac cntrl (!simp_set) tg gl

 end

open Simplifier;;

let axioms = 
  [
   "!x: (true and x) = x";
   "!x: (x and true) = x";

   "!x: (false and x) = false";
   "!x: (x and false) = false";

   "!x: (true or x) = true";
   "!x: (x or true) = true";
   "!x: (false or x) = x";
   "!x: (x or false) = x";

   "(not false) = true";
   "(not true) = false";

   "!x: (not (not x))=x";
   "!x y: (x => y) = ((not x) or y)";

   "(!x: true) = true";
   "(!x: false) = false";

   "! x: (x=x)=true";

(*

   "(finite empty)=true";
   "!x y : (finite x) => ((finite (add y x))=true)"
*)
 ] 

let setup()= add_simp (List.map saxiom axioms);;

let t= << !x y : (not (false and x)) => (x or (y or true)) >>;;
let t1= <<!x:(false and x)>>;;
let t2= <<not (not false)>>;;
let t3= <<!f g: 1 = 2>>;;
let t4= <<!f x y: (x=y) => ((f x)=(f y))>>;;

add_simp [(saxiom "!x: true => ((x and true)=x)")];;

(*
   let rrs() = List.map read axioms;;
*)
