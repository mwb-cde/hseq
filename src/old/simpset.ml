module Simpset =
struct

open Term
open Logicterm

(* simpsets *)

(* simp_rule :
   variable , optional condition, lhs , rhs , source of rule
*)

type rule = 
    (binders list 
       * term option * term * term 
       * Logic.rr_type)

(* simp_set:
   set of simplifier rules 
*)

type simp_set = 
{ 
  assumptions: rule Net.net;   (* rules from a sequent assumption *)
  basic: rule Net.net          (* global rules *)
} 


let empty_set() = 
  { cond_depth=5; assumptions=Net.empty(); basic=Net.empty()}


let lookup set trm =
  try
    Net.lookup trm set.basic
  with 
    Not_found -> Net.lookup trm set.assumptions


(* needed properties *)

let cond_rule_true_ax = saxiom "! a b: (a=>b)=(a=>(b=true))"
let iff_equals_ax = saxiom "! a b: (a iff b)=(a=b)"
let rule_false_ax = saxiom "! a : (not a)=(a=false)"
let rule_true_ax = saxiom "! a : a=(a=true)"

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

(* sqnt_solved: true if sqnt st is no longer in goal g *)


let sqnt_solved st g =
  try
    ignore(List.find 
	     (fun x->Logic.Tag.equal st x) 
	     (Logic.get_all_goal_tags g));
    false
  with Not_found ->true

let apply_on_cond c uncondf condf x =
  match c with
    None -> uncondf x
  | Some(_) -> condf x

let apply_tag tac g =
  let inf=ref (Logic.Rules.make_tag_record [] [])
  in 
  let ng = tac inf g
  in 
  (!inf, ng)

let apply_get_tag tac g =
  let inf=ref (Logic.Rules.make_tag_record [] [])
  in 
  let ng = tac inf g
  in 
  let ntg = 
    match !(inf).Logic.Rules.forms with
      [t] -> t
    | _ -> raise (Failure "too many tags")
  in 
  (ntg, ng)


let rec rebuild_qnt qs b=
    match qs with
      [] -> b
    | (x::xs) -> Term.Qnt(x, rebuild_qnt xs b)

let allE_list i vs g =
  let rec inst_aux xs sq=
    match xs with 
      [] -> sq
    | (c::cs) -> 
	let nsq=Logic.Rules.allE_full None c i sq
	in 
	inst_aux cs nsq
  in 
  inst_aux vs g

(* make_consts: 
   uses base.some constant defined in theory base
*)

let make_consts qs env = 
  let make_aux q=
    try Term.find (Bound q) env
    with 
      Not_found -> Logicterm.mksome
  in 
  List.map make_aux qs


(* is_rr_rule qs c l r: check that c=>l=r is a rewrite rule
   all variables (in qs) occuring in c or r must also occur in l
   ret: (cnd, rhs)
   cnd= Some(true) iff all variables in c occur in l
      = None if no condition
   rhs= Some(true) iff all variables in r occur in l
      = None if no rhs
*)

let is_rr_rule qs c l r=
  let vars=Term.empty_subst()
  and is_var b=List.mem b qs
  in 
  let rec find_variables t=
    match t with
      Term.Qnt(_, b) ->
	find_variables b
    | Term.Bound(q) ->
	if(is_var q)
	then 
	  (try ignore(Term.find t vars)
	  with Not_found ->
	    ignore(Term.add t t vars))
	else ()
    | Term.Typed(tr, _) -> find_variables tr
    | Term.App(f, a) -> find_variables f; find_variables a
    | _ -> ()
  in 
  let rec check_variables t=
    match t with
      Term.Qnt(_, b) ->
	check_variables b
    | Term.Bound(q) ->
	if(is_var q)
	then 
	  ignore(Term.find t vars)
	else ()
    | Term.Typed(tr, _) -> check_variables tr
    | Term.App(f, a) -> check_variables f; check_variables a
    | _ -> ()
  in 
  let rret=ref (Some true)
  and cret=ref (Some true)
  in
  find_variables l;
  (try
    match r with None -> rret:=None | Some (rhs) -> check_variables rhs;
  with Not_found -> rret:=(Some false));
  (try
    (match c with None -> rret:=None | Some(cnd) -> check_variables cnd)
  with Not_found -> cret:=(Some false));
  (!cret, !rret)


(* dest_rule: 
   split a rule into variable binders, condition, lhs, rhs 
   rules are of the form:
   c=>l=r
   l=r
*)

let dest_rule t =
  let (qs, t1)=strip_qnt (Basic.All) t  (* get leading quantifiers *)
  in 
  if (is_equal t1)                (* deal with simple equalities *)
  then 
    (let (l, r) = dest_equal t1 
    in 
    (qs, None, l, r))
  else 
    if (is_implies t1)         (* deal with conditional equalities *)
    then 
      (let (_, args)=dest_fun t1
      in 
      match args with
	[a; c] -> 
	  (if (is_equal c)
	  then 
	    let (l, r)=dest_equal c
	    in (qs, Some(a), l, r)
	  else 
	    raise (Failure ("Not an equality or a conditional equality\n")))
      |	_  -> raise (Failure ("Can't add rule: "^
			      "not an equality or a conditional equality\n")))
    else
      raise (Failure ("Not an equality or a conditional equality\n"))


(* strip_qnt_cond: 
   split a rule into variable binders, condition, equality
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

let dest_rrthm t = 
  match t with 
    Logic.RRThm (x) -> x
  | _ -> failwith("dest_rrth: failure")

let dest_option x=
  match x with
    None -> failwith "dest_option"
  | Some c -> c

let has_cond c =
  match c with
    None -> false
  | Some(_) -> true


let get_one ls err=
  match ls with
    [x] -> x
  | _ -> raise err

let get_two ls err=
  match ls with
    [x; y] -> (x, y)
  | _ -> raise err


(* conversions for manipulating rules *)


(* simp_case_tac x sq: adds formula x to assumptions of sq, 
   creates new subgoal in which to prove x

   g|asm |- cncl      --> g'|asm |- t:x, cncl, g| t:x, asm |- cncl 

info: [g', g] [t]
*)

let simp_case_tac_thm = ref None

let get_simp_case_thm ()=
  match !simp_case_tac_thm with
    None ->
      let nthm =
	try 
	  lemma "boolean.cases_thm"
	with Not_found -> 
	  (prove_goal "!P: (not P) or P"
	    (thenl [flatten_tac; basic]))
      in 
      simp_case_tac_thm := Some(nthm);
      nthm
  | Some(t) -> t


let simp_case_tac inf (x:Term.term) g= 
  let thm =  get_simp_case_thm()
  and tinf=ref(Logic.Rules.make_tag_record [] [])
  in 
  let g1=Logic.Rules.cut_full (Some tinf) thm g
  in 
  let nt=get_one ((!tinf).Logic.Rules.forms) (Failure "case_info")
  in 
  let g2=
    thenl[
    Logic.Rules.allE_full None x (Logic.FTag nt);
    Logic.Rules.disjI_full (Some tinf) (Logic.FTag nt);
    Logic.Rules.negA_full None (Logic.FTag nt) ] g1
  in 
  let ng1, ng2=get_two (!tinf).Logic.Rules.goals (Failure "case_info")
  in 
  Logic.Rules.do_tag_info (Some inf) [ng2;ng1] [nt];
  g2


(* functions to make simp rules from assumptions *)

(* term_cond_rewrite:
   for rewrite rule |-l=r,
   rewrite term
   c=>a=l -> c=>a=r
*)

let term_cond_rewrite scp rl fm =
  let qs, cnd, qb=strip_qnt_cond fm
  in 
  let rrtrm = Formula.dest_form (Logic.dest_thm rl)
  in 
  rebuild_qnt qs 
    (Logicterm.mkimplies (dest_option cnd)
       (Rewrite.rewrite_univs scp ~dir:true ~simple:true 
	  [rrtrm] qb))
  
let form_cond_rewrite scp rl fm =
   Formula.mk_form scp 
    (term_cond_rewrite scp rl (Formula.dest_form fm))



let monitor = ref None

(* asm_cond_copy_rewrite:
   with rewrite rule |- x=y
   transform conditional assumption:
   t:a=>x |- c
-> t':a=>y, t:a=>b |- c

 info: [] [t']
*)

let asm_cond_copy_rewrite info thm ftg g=
  let sqnt=Logic.get_sqnt g
  in 
  let initial_sqnt_tag = Logic.sqnt_tag sqnt
  in 
  let scp=Logic.scope_of sqnt
  in 
  (* break down assumption *)
  let asm_form = Logic.drop_tag (Logic.get_tagged_asm ftg sqnt)
  in
  let asm_trm=Formula.dest_form  asm_form
  in
  let (qs, cnd, qb)=strip_qnt_cond asm_trm
  in 
  let new_subgoals = ref []  (* list of subgoals produced *)
  in 
  let add_subgoal sg = new_subgoals:=sg::!new_subgoals
  in 
  (* introduce new formula by cases, 
     focus on subgoal establishing correctness of formula
     newform a=>y;  asm |- cncs
     -->
     gt1|  ft:a=>y, ftg:a=>x, asm | -> cncs

     gt2| ftg:a=>x, asm |- ft: a=> y, cncs
   *)
  let introduce_formula thm ftg g=
    let rrtrm=Formula.dest_form (Logic.dest_thm thm)
    in
  (* make new formula *)
    let newform=form_cond_rewrite scp thm asm_form 
    in 
(*
    let linfo, g1=apply_tag 
	(fun inf-> Logic.Rules.case_info inf newform) g
*)
  (* introduce new formula by cases
     using simp_case_tac
   *)
    let linfo, g1=apply_tag 
      (fun inf-> simp_case_tac inf (Formula.dest_form newform)) g
    in 
    let gt1, gt2=
      get_two linfo.Logic.Rules.goals
	(Logic.logicError 
	   "asm_cond_rule_rewrite: Too many subgoals after trying cases"
	   [])
    in 
    add_subgoal gt1; add_subgoal gt2;
  (* get tag of new formula *)
    let ft=
      get_one linfo.Logic.Rules.forms
	(Logic.logicError 
	   "asm_cond_rule_rewrite: Too many formulas after trying cases"
	   [])
    in 
(*
  (* focus on subgoal proving new formula is true *)
    (ft, gt1, gt2, Logic.goal_focus gt2 g1)
*)
    monitor := Some g1;
    (ft, gt2, gt1, g1)

  in 

  let prove_formula ft nsg gt2 g2=
    let tinfo=ref (Logic.Rules.make_tag_record [] [])
    in 
  (* eliminate universal quantifiers
     and implication:
     ftg:a=>x, asm4:a |- ft:y

     rewrite conclusion,:
     ftg:a=>x, asm4:a |- ft:y
     -> 
     ftg:a=>x, asm4:a |- ft:x
   *)

    let g5 = 
      Logic.Rules.thenl
	[
	 Logic.Rules.orl 
	   [Logic.Rules.repeat 
	      (Logic.Rules.allI_full None (Logic.FTag ft)); 
	    Logic.Rules.skip];
	 Logic.Rules.implI_full (Some tinfo) (Logic.FTag ft);
	 Logic.Rules.rewrite_any_full None ~dir:false ~simple:true 
	   [Logic.RRThm thm] (Logic.FTag ft)]
	g2
    in
    let asm4, _= 
      get_two 
	(!tinfo).Logic.Rules.forms 
	(Logic.logicError 
	   "asm_cond_rule_rewrite: Too many formulas after trying implI"
	   [])
    in 
  (* record subgoals *)
    List.iter add_subgoal (!tinfo).Logic.Rules.goals; 
    let sqnt5=Logic.get_sqnt g5
    in 
    let nasm=Formula.dest_form 
	(Logic.drop_tag (Logic.get_tagged_asm asm4 sqnt5))
    and ncncl=
      Formula.dest_form 
	(Logic.drop_tag (Logic.get_tagged_cncl ft sqnt5))
    in 
  (* sqnt5=  ftg:a=>x, asm4:a |- ft:x
     nasm = a
     ncncl = x
     ftg= a=>x
     prepare to unify ftg with nasm and ncncl
   *)
  (* make new implication from nasm and ncncl *)
    let new_trm = Logicterm.mkimplies nasm ncncl
    and (_, nasm_trm)= Term.strip_qnt Basic.All asm_trm 
    in 
    let env = Term.empty_subst()
    in 
  (* unify new implication with assumption ftg
     (to find constants for allE, if needed)
   *)
    let binders = List.map (fun x->Bound x) qs
    in 
    ignore(Unify.unify_env scp env (fun q->List.mem q binders) 
	     nasm_trm new_trm);
(*
   instantiate ftg with constants found by unification 
   apply implication and unification to remove subgoals 
*)
    let g7=
      Logic.Rules.thenl
	[allE_list (Logic.FTag ftg) (make_consts qs env);
	 Logic.Rules.implE_full (Some tinfo) (Logic.FTag ftg)] g5
    in 
    let cncl7=
      get_one (!tinfo).Logic.Rules.forms
	(Logic.logicError 
	   "asm_cond_rule_rewrite: Too many formulas after trying implE"
	   [])
    in   
    let g9=
      Logic.Rules.thenl
	[Logic.Rules.unify_full None (Logic.FTag asm4) (Logic.FTag cncl7);
	 Logic.Rules.unify_full None (Logic.FTag ftg) (Logic.FTag ft)] 
	g7
    in 
  (* check all new subgoals have been proved *)
    monitor := Some(g9);
    List.iter 
      (fun sg -> 
	if(not (Logic.Tag.equal sg gt2)) then 
	  if(sqnt_solved sg g9) then ()
	  else  raise 
	      (Logic.logicError 
		 "asm_cond_rule_rewrite: Unsolved subgoals remain" [])
	else ())
      !new_subgoals;
  (* set info *)
    Logic.Rules.do_tag_info info [gt2] [ft]; g9
  in 
  let (ft, nsg, gt2, g2)=
    introduce_formula thm ftg g
  in 
  prove_formula ft nsg gt2 g2
      

(* asm_cond_rewrite: rewrite assumption, delete original, make new tag

   |- a=b
   g| t:c=>a, asms |- concl
-->
   g'| t':c=>b, asms |- concl

info: [g'] [t']
*)

  let asm_cond_rewrite info thm tg g= 
    let g1=asm_cond_copy_rewrite (Some info) thm tg g
    in 
    let g2=Logic.Rules.delete
	(Logic.tag_to_index tg (Logic.get_sqnt g1)) g1
    in 
    g2


(* asm_rewrite:
   |- a=b
   t:a, asms |- concl
-->
   t:b, asms |- concl

info: [] [t]
*)

  let asm_rewrite info thm tg g=
    let tg_idx=Logic.tag_to_index tg (Logic.get_sqnt g)
    in 
    let g1=
      Logic.Rules.rewrite_any_info info ~dir:true ~simple:true 
	[Logic.RRThm(thm)] tg_idx g
    in g1


(* thm_cond_rewrite:
   for rewrite rule:
   |- b=x 
   rewrite conditional theorem:
   |- a=>b -->  |- a=>(b=x)
*)

let thm_cond_rewrite scp rl thm=
  (* prepare new term *)
  let thmform=Logic.dest_thm thm
  in 
  let newform=form_cond_rewrite scp rl thmform
  in 
  let g1=Logic.mk_goal scp newform
  in 
  let cncltag=Logic.index_to_tag 1 (Logic.get_sqnt g1)
  in 
  let info, g2 = 
    apply_tag 
      (fun inf -> Logic.Rules.cut_info inf thm) g1
  in 
  let rltag= 
    match info.Logic.Rules.forms with
      [a] -> a
    | _ -> raise
	  (Logic.logicError 
	     "thm_cond_rewrite: too many formulas after cut"
	     [])
  in 
  (* g2=  a=>b |- a=>(b=x) *)
  let info3, g3=
    apply_tag
      (fun inf-> asm_cond_copy_rewrite (Some inf) rl rltag) g2
  in 
  let rltag3= 
    match info3.Logic.Rules.forms with
      [a] -> a
    | _ -> raise
	  (Logic.logicError 
	     "thm_cond_rewrite: too many formulas after asm_cond_copy_rewrite"
	     [])
  in 
  let sqnt3=Logic.get_sqnt g3
  in 
  let g4=
    Logic.Rules.orl
      [Logic.Rules.unify 
	 (Logic.tag_to_index rltag3 sqnt3) 
	 (Logic.tag_to_index cncltag sqnt3);
       Logic.Rules.thenl
	 [Logic.Rules.repeat 
	    (Logic.Rules.allI (Logic.tag_to_index cncltag sqnt3));
	  Logic.Rules.unify 
	    (Logic.tag_to_index rltag3 sqnt3) 
	    (Logic.tag_to_index cncltag sqnt3)]] g3
  in Logic.mk_thm g4

(* thm_rewrite:
   |- a=b 
   |- a -->  |- b
*)

  let thm_rewrite scp rl thm=
    Logic.ThmRules.rewrite_conv scp ~dir:true ~simple:true [rl] thm

(* many_conj_conv:
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


(* make_entry t:
   y -> y=true
   x=y -> x=y        (if a rr rule)
       -> (x=y)=true (otherwise)
   c=>x=y -> c=>x=y        (if a rr rule)
          -> (c=>x=y)=true (otherwise)
*)   


(* tests on theorems *)

(* is_many_conj: test for: a and b and ... and z *)

  let is_many_conj thm=
    let thmtrm=Formula.dest_form (Logic.dest_thm thm)
    in 
    Logicterm.is_conj thmtrm

(* is_iffterm:  a iff b *)

  let is_iffterm (vars, cnd, main) =
    (try(fst(Term.dest_fun main) = Logicterm.iffid) with _ -> false)

(* is_negation:  not a  *)
  let is_negation (vars, cnd, main)=
    Logicterm.is_neg main

(* is_equality: a=b *)
  let is_equality (vars, cnd, main)=
    Logicterm.is_equal main

(* make_thm_entry: make simpset entries from a list of theorems *)

  let rec make_thm_entry scp thmlist buildlist=

(* do_equality: deal with equality theorems 
   testing for possible rewrite rules 
*)
    let do_equality (vars, cnd, lhs, rhs) thm=
      match is_rr_rule vars cnd lhs (Some rhs) with
	Some(false), _ ->   (* c=>a=b --> (c=>a=b)=true) *)
	  apply_on_cond cnd
	    (thm_rewrite scp rule_true_ax)
	    (thm_cond_rewrite scp rule_true_ax)
	    thm
      | _, Some(false) ->    (* c=>a=b --> c=>(a=b=true) *)
	  apply_on_cond cnd
	    (thm_rewrite scp rule_true_ax)
	    (thm_cond_rewrite scp rule_true_ax)
	    thm
      | _, _ -> thm
    in 
(* make_aux: main loop, test for different cases *)
    let rec make_aux thm bl=
      let thmtrm=Formula.dest_form (Logic.dest_thm thm)
      in 
      let vars, cnd, main = strip_qnt_cond thmtrm 
      in 
      if is_many_conj thm
      then 
	make_thm_entry scp (many_conj_conv thm) bl
      else 
	if is_iffterm (vars, cnd, main)
	then 
	  make_aux
	    (apply_on_cond cnd
	       (thm_rewrite scp iff_equals_ax)
	       (thm_cond_rewrite scp iff_equals_ax)
	       thm)
	    bl
	else 
	  if is_negation (vars, cnd, main)
	  then 
	    make_aux
	      (apply_on_cond cnd
		 (thm_rewrite scp rule_false_ax)
		 (thm_cond_rewrite scp rule_false_ax) 
		 thm) bl
	  else 
	    if is_equality (vars, cnd, main)
	    then 
	      let l, r=Logicterm.dest_equal main 
	      in 
	      let nthm=do_equality (vars, cnd, l, r) thm
	      in 
	      let nvars, ncnd, nmain=
		strip_qnt_cond (Formula.dest_form (Logic.dest_thm nthm))
	      in 
	      let llhs, lrhs=Logicterm.dest_equal nmain
	      in 
	      (l, (nvars, ncnd, llhs, lrhs, Logic.RRThm(nthm)))::bl
   (* default *)
	    else 
	      match is_rr_rule vars cnd main None with    
		Some(false), _ -> 
		  make_aux (thm_rewrite scp rule_true_ax thm) bl
	      |	_, _ ->
		  make_aux 
		    (apply_on_cond cnd
		       (thm_rewrite scp rule_true_ax)
		       (thm_cond_rewrite scp rule_true_ax)
		       thm) bl
    in 
    match thmlist with
      [] -> List.rev buildlist
    | firstthm::otherthms -> 
	make_thm_entry scp otherthms (make_aux firstthm buildlist)
	  
	  
(* make_asm_entry: 
   make simpset entries from a list of assumptions in 
   the first subgoal of a goal
   
   rules similar to make_thm_entry except:
   conjunctions are already broken down 
   (no assumptions of the form  'a and b and ...)

** UNTESTED **

 *)
	  
  let rec make_asm_entry asmlist buildlist gl=

    let rewrite_cnd_tag cnd rl tg g=
      apply_on_cond cnd
	(apply_get_tag (fun inf -> asm_rewrite inf rl tg))
	(apply_get_tag (fun inf -> asm_cond_rewrite inf rl tg)) g
    in 
    let do_equality (vars, cnd, lhs, rhs) asm g=
      match is_rr_rule vars cnd lhs (Some rhs) with
	Some(false), _ -> 
	  let nasm, ng =
	    rewrite_cnd_tag cnd rule_true_ax asm g
	  in (nasm, ng)
      |	_, Some(false) ->
	  let nasm, ng =
	    rewrite_cnd_tag cnd rule_true_ax asm g
	  in (nasm, ng)
      |	_, _ -> (asm, g)
    in
    let rec make_aux asm g=
      let sqnt=Logic.get_sqnt gl
      in 
      let asmindx=Logic.tag_to_index asm sqnt
      in 
      let asmtrm=
	Formula.dest_form 
	  (Logic.drop_tag (Logic.get_tagged_asm asm sqnt))
      in 
      let vars, cnd, main = strip_qnt_cond asmtrm
      in 
      if is_iffterm (vars, cnd, main)
      then 
	let nasm, ng=
	  rewrite_cnd_tag cnd iff_equals_ax asm g
	in make_aux nasm ng
      else 
	if is_negation (vars, cnd, main)
	then
	  let nasm, ng=
	    rewrite_cnd_tag cnd rule_false_ax asm g
	  in make_aux nasm ng
	else 
	  if is_equality (vars, cnd, main)
	  then 
	    let l, r=Logicterm.dest_equal main
	    in 
	    let nasm, ng=do_equality (vars, cnd, l, r) asm g
	    in 
	    let nsqnt=Logic.get_sqnt ng
	    in 
	    let nasmtrm = 
	      Formula.dest_form
		(Logic.drop_tag (Logic.get_tagged_asm nasm nsqnt))
	    in 
	    let nvars, ncnd, nmain = strip_qnt_cond nasmtrm
	    in 
	    let llhs, lrhs =Logicterm.dest_equal nmain
	    in 
	    ((l, (nvars, ncnd, llhs, lrhs, Logic.Tagged(nasm))), ng)
	  else
(* default *)
	    match is_rr_rule vars cnd main None with
	      Some(false), _ -> 
		let nasm, ng=
		  apply_get_tag 
		    (fun inf -> asm_rewrite inf rule_true_ax asm) g
		in make_aux nasm ng
	    | _, _ ->
		let nasm, ng=
		  rewrite_cnd_tag cnd rule_true_ax asm g
		in 
		make_aux nasm ng
    in 
    match asmlist with
      [] -> List.rev buildlist
    | firstasm::otherasms ->
	let newentrys, ng =make_aux firstasm gl
	in 
	make_asm_entry otherasms (newentrys::buildlist) ng

end

open Simpset;;


goal "(!a b: a=>b) => true";;
by  implI;;
let gl=curr_goal(top());;
let sqnt=curr_sqnt gl;;
let thm=saxiom "!a: a= (a=true)";;
let scp=Logic.scope_of sqnt;;
let info=ref (Logic.Rules.make_tag_record [][]);;
let ftg=Logic.index_to_tag (-1) sqnt;;

(*
let scp=scope();;

let rl=saxiom "!a: a= (a=true)";;
let thm=saxiom"(!a b: a=>b)";;
*)
