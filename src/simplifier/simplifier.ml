
open Basic
open Term
open Logicterm
open Tactics

open Simputils
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

type control = Rewrite.control

module Data =
  struct

(* [type Data.t]
   Information used by and built up during simplification.
 *)
    type t =
	{
(**
   [simpset]: the simpset being used. Assumptions may be added to this
   during the course of simplification
*)
	 simpset:Simpset.simpset;

(** [cond_tac]: the tactic used to prove conditions of rewrite rules *)
	 cond_tac: t -> Tag.t -> Tactics.tactic;

(** [control]: rewrite control ([direction] is ignored *)
	   control: Rewrite.control;

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

    let make sset tac cntrl cd rd a rs= 
      {
       simpset=sset;
       cond_tac=tac;
       conds=cd;
       control=cntrl;
       rr_depth=rd;
       asms=a;
       rules=rs
     }

    let set_simpset cntrl set=
      {cntrl with simpset = set}

    let set_tactic cntrl tac=
      {cntrl with cond_tac = tac}

    let set_conds cntrl d=
      {cntrl with conds=d}

    let set_control cntrl c=
      {cntrl with control=c}

    let set_rr_depth cntrl d=
      {cntrl with rr_depth=d}

    let set_asms cntrl ds=
      {cntrl with asms=ds}

    let set_rules cntrl ds=
      {cntrl with rules=ds}

    let get_simpset cntrl=cntrl.simpset
    let get_tactic cntrl=cntrl.cond_tac
    let get_control cntrl=cntrl.control

    let add_asm cntrl a=
      set_asms cntrl (a::(cntrl.asms))

    let get_asms cntrl =cntrl.asms

    let dec_cond_depth cntrl=
      set_conds cntrl ((cntrl.conds)-1)

    let add_rule cntrl r=
      set_rules cntrl (r::(cntrl.rules))

    let add_simp_rule cntrl rule=
      set_simpset cntrl
	(Simpset.add_rule rule (get_simpset cntrl))

(** [default]: The default control information  *)
    let default = 
      make (Simpset.empty_set()) (fun _ _ -> skip) 
	Formula.default_rr_control 50 50 [] [] 

  end

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


(*
   [clean_up_tac ctrl g]
   
   Clean up after simplification.
   Delete all assumptions listed in [ctrl.asms].
*)

let rec clean_aux_tac tags g=
  match tags with
    [] -> g
  | x::xs ->
      let ng=
	try
	  foreach (Logic.Rules.delete None (Logic.FTag x)) g
	with _ -> g
      in clean_aux_tac xs ng

let clean_up_tac ctrl g=
  clean_aux_tac (Data.get_asms ctrl) g

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
      Logic.Rules.cut info th g
  | Logic.Asm(x) ->
      Logic.Rules.copy_asm info x g

(** [prep_cond_tac cntrl values thm g]

   Cut [thm] into the sequent, instantiate with [values].  Apply
   [implA] to get two subgoals, tagged [(cgltg, rgltg)] with condition
   in [cgltg] tagged [cftg] and rewrite-rule in [rgltg] tagged [rrftg].
   Add [rrftg] to [cntrl], getting [ncntrl].

   return [(ncntrl, [cgltg; rgltg], [cftg; rrftg], g3)].
 *)

let prep_cond_tac cntrl values thm goal =
  let info =Drule.mk_info()
  in
   try 
    let (rrftg, goal1)=
      (let ng = cut_rr_rule (Some(info)) thm goal
      in 
      let rrftg=Lib.get_one (Drule.formulas info) No_change
      in (rrftg, ng))
    in 
    let (cgltg, rgltg, cftg, goal2)=
      let ng= 
	foreach
	  ((allA_list (ftag rrftg) values)
	     ++ (Logic.Rules.implA (Some(info)) (ftag rrftg))) 
	  goal1
      in 
      let (cgltg, rgltg)= 
	Lib.get_two (Drule.subgoals info)
	  (Failure "prep_cond_tac: goals")
      in 
      let cftg=
	Lib.get_one (Drule.formulas info)
	  (Failure "prep_cond_tac: forms")
      in 
      (cgltg, rgltg, cftg, ng)
    in
    let ncntrl= Data.add_asm cntrl rrftg
    in 
    (ncntrl, (cgltg, rgltg), (cftg, rrftg), goal2)
  with _ -> raise No_change


(** 
   [prove_cond_rule tac values entry g]

   Prepare a simpset entry [entry] for use in rewriting.
   
   If [entry] is unconditional, return immediatly.
   Otherwise, use [prep_cond_tac] to  create a subgoal from the
   condition; apply tactic [tac] to prove the condition; fail
   if [tac] fails to solve the condition.

   Return RRInfo of the theorem/assumption to use as i rewriting
   and new goal.
 *)

let rec prove_cond cntrl values entry goal = 
  let (qs, cnd, _, _, thm)=entry
  in 
  match cnd with
    None -> (cntrl, thm, (skip goal))
  | Some(_) -> 
      let (ncntrl, rftg, ng1) = 
	let (ncntrl1, (cgltg1, rgltg1), (cftg1, rftg1), ng)=
	  prep_cond_tac cntrl values thm goal
	in 
	(ncntrl1, rftg1,
	 foreach 
	   ((fun n -> 
	     (Tag.equal cgltg1 (Drule.node_tag n)))
	      --> 
		(fun g -> 
		  let tac=Data.get_tactic ncntrl1
		  in 
		  (tac ncntrl1 cftg1) g)) ng)
      in 
      (* check for subgoals *)
      match (Drule.branch_subgoals ng1) with
      | [] -> (ncntrl, Logic.Asm(Drule.ftag rftg), ng1)
      | [sqnt] ->  
	  (* add assumption as a rule, 
	     to avoid having to prove it again *)
	  let form=Logic.drop_tag(Logic.Sequent.get_tagged_asm rftg sqnt)
	  in 
	  let rule = 
	    Simpset.make_rule 
	      (Logic.Asm (Drule.ftag rftg)) 
	      (Formula.dest_form form)
	  in 
	  (Data.add_simp_rule ncntrl rule,
	   Logic.Asm(Drule.ftag rftg), ng1)
      | _ -> (* >1 subgoals = failure *)
	  raise No_change


(*
   match_rewrite:
   try to match lhs with trm, return rhs if sucessful
 *)
let match_rewrite scp tyenv tenv varp lhs rhs trm = 
  try
    (let ntyenv, nenv=
      Unify.unify_fullenv_rewrite 
	scp tyenv (Term.empty_subst()) varp lhs trm
    in 
    (ntyenv, nenv, Term.subst tenv rhs))
  with x -> (failwith "match_rewrite")

(** [find_basic scp tyenv tac rl trm g]

   Try to match rule [rl] with term [trm] in node [g].
   If [rl] matches but is conditional, try to prove the condition
   using tactic [tac].

   returns rewritten term, matched rules and new goal.
 *)
let find_basic cntrl tyenv rl trm g=
  let (qs, c, lhs, rhs, thm)=rl
  in 
  let tenv=Term.empty_subst()
  in 
  let scp=Drule.scope_of g
  in 
  let (ntyenv, ntenv, nt)=
    match_rewrite scp tyenv tenv
      (Rewrite.is_free_binder qs) lhs rhs trm
  in 
  let values=Drule.make_consts qs ntenv
  in 
  let (ncntrl, rr, ng)=prove_cond cntrl values rl g
  in 
  (ncntrl, ntyenv, nt, rr, ng)

(** [find_match scp tyenv rslt set tac trm g]
   Find rule in simpset [set] which matches term [trm] in goal [g].

   If a possible match is conditional, try to prove the condition
   using tactic [tac].

   raise No_change if no matches.

   [find_match_tac ret c tyenv set tac t g] Apply [find_match c tyenv
   set tac g].  If sucessful, set [ret] to the rewritten term,
   matching rule and new goal.

   raise No_change and set [ret:=None] if no matches.
 *)
let find_match_tac cntrl tyenv ret trm (goal: Logic.node)=
  let rec find_aux rls t g= 
    match rls with
      [] -> (ret:=None; raise No_change)
    | (rl::nxt) ->
	try 
	  find_basic cntrl tyenv rl t g
	with _ -> find_aux nxt t g
  in 
  let (ncntrl, ntyenv, nt, rr, ng) = 
    find_aux (lookup (Data.get_simpset cntrl) trm) trm goal
  in 
  ret:=(Some(ncntrl, ntyenv, nt, rr)); ng

(** [find_all_matches scp tyenv rslt set tac trm g]

   Find all rules in simpset [set] which can be used to rewrite
   term [trm] in goal [g].

   puts matched rules into rslt
   returns rewritten term, matched rules and new goal.

   Messy implementation:

   Apply find_match_tac to a term, store result,
   repeat until find_match fails. 
   Return result and last sucessfull goal.
 *)
let rec find_all_matches cntrl tyenv trm branch=
  let chng = ref false
  in 
  let rec find_aux c ty t g= 
    (let ret1=ref None
    in 
    try
      let ng=foreach (find_match_tac c ty ret1 t) g
      in 
      match (!ret1) with
	None -> (c, ty, t, g)
      | Some (cntrl1, tyenv1, t1, r1) -> 
	  (chng:=true;
	   find_aux (Data.add_rule cntrl1 r1) tyenv1 t1 ng)
    with _ -> (c, ty, t, g))
  in 
  let (nc, ntyenv, nt, ng)=find_aux cntrl tyenv trm branch
  in 
  if(!chng)
  then (nc, ntyenv, nt, ng)
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
  let sqnt= Drule.sequent g
  in 
  let is_asm =
    try
      (ignore(Drule.get_tagged_asm (ftag tag) g); true)
    with _ -> false
  in 
  let fid = Logic.FTag tag
  in 
  let (newcontrol, newgoal)=
    if(is_asm)
    then 
      try 
	(control, 
	 Tactics.repeat (Logic.Rules.existA None fid) g)
      with _ -> (control, skip g)
    else 
      try 
	(control, Tactics.repeat (Logic.Rules.allC None fid) g)
      with _ -> (control, skip g)
  in 
  (newcontrol, newgoal)

(* basic_simp_tac: toplevel for simplifier

   basic_simp_tac: workhorse of simplifier
   basic_simp_tac cntrl:control set:rule_set ft:Tag.t st:Tag.t g:goal
   where ft is tag of formula to work on
   and st is tag of sequent to work on
 *)
let is_true t = Term.is_true t

(**
   [get_form t n]:
   Get formula tagged [t] from node [n].
   First try conclusions, then try assumptions.

   raise [Not_found] if not found.
 *)
let get_form t sqnt = 
  try 
    Logic.Sequent.get_tagged_cncl t sqnt
  with Not_found -> Logic.Sequent.get_tagged_asm t sqnt


(**
   [find_rrs_bottom_up ctrl t g]

   Make list of rewrite rules using unification
   Term.substitution is used to keep track of changing term
   as rewrites are found
 *)
let rec find_rrs_bottom_up ctrl tyenv t g=
  match t with
    Basic.Qnt(k, q, b) -> 
      (let (bcntrl, btyenv, nb, bg) = 
	find_rrs_bottom_up ctrl tyenv b g
      in 
      try 
	find_all_matches bcntrl btyenv (Qnt(k, q, nb)) bg
      with No_change -> (bcntrl, btyenv, Qnt(k, q, nb), bg))
  | Basic.Typed(tt, ty) -> find_rrs_bottom_up ctrl tyenv tt g
  | Basic.App(f, a)->
      (let (fcntrl, ftyenv, nf, nfg) = 
	find_rrs_bottom_up ctrl tyenv f g
      in 
      let (acntrl, atyenv, na, nag) = 
	find_rrs_bottom_up fcntrl ftyenv a nfg
      in 
      try 
	find_all_matches acntrl atyenv (App(nf, na)) nag
      with No_change -> (acntrl, atyenv, App(nf, na), nag))
  | _ -> 
      (try 
	find_all_matches ctrl tyenv t g
      with No_change -> (ctrl, tyenv, t, g))

(* 
   [find_rrs_top_down cntr t g]

   Top-down search through term [t] for rewrite rules to apply.
 *)
let rec find_rrs_top_down ctrl tyenv trm g=
  let find_td_aux tyenv1 cntrl1 trm1 g=
    match trm1 with
      Basic.Qnt(k, q, b) -> 
	(let (bcntrl, btyenv, nb, bg) = 
	  find_rrs_top_down  cntrl1 tyenv1 b g
	in 
	(bcntrl, btyenv, Qnt(k, q, nb), bg))
    | Basic.Typed(tt, ty) -> find_rrs_top_down  cntrl1 tyenv1 tt g
    | Basic.App(f, a)->
	(let (fcntrl, ftyenv, nf, nfg) = 
	  find_rrs_top_down  cntrl1 tyenv1 f g
	in 
	let (acntrl, atyenv, na, nag) = 
	  find_rrs_top_down  fcntrl ftyenv a nfg
	in 
	(acntrl, atyenv, App(nf, na), nag))
    | _ -> (cntrl1, tyenv1, trm1, g)
  in
  let (nctrl, ntyenv, ntrm, ng) = 
    (try 
      find_all_matches ctrl tyenv trm g
    with No_change -> (ctrl, tyenv, trm, g))
  in 
  find_td_aux ntyenv nctrl ntrm ng


let rec basic_simp_link ft cntrl goal=
  let chng=ref false
  in 
  let tyenv=Drule.branch_tyenv goal
  in 
  let sqnt = 
    match (Drule.branch_subgoals goal) with
      [x] -> x
    | _ -> raise (Failure "basic_simp_link: too many subgoals")
  in 
  let trm=
    Formula.dest_form (Logic.drop_tag (get_form ft sqnt))
  in 
  let (ncntrl, ntyenv, ntrm, ngoal)= 
    let rr_cntrl = Data.get_control cntrl
    in 
    if (rr_cntrl.Rewrite.rr_strat = Rewrite.bottomup)
    then 
      find_rrs_bottom_up cntrl tyenv trm goal
    else
      find_rrs_top_down cntrl tyenv trm goal
  in 
  let rrs=List.rev (ncntrl.Data.rules)
  in 
  if rrs=[]
  then raise No_change
  else 
    let goal1=
      let rr_cntrl0 = Data.get_control cntrl
      in 
      let rr_cntrl = 
	Rewrite.control 
	  ~dir:Rewrite.leftright
	  ~max:None
	  ~strat:rr_cntrl0.Rewrite.rr_strat
      in 
      (try
	Tactics.foreach 
	  (Logic.Rules.rewrite None ~ctrl:rr_cntrl rrs (ftag ft)) ngoal
      with _ -> raise No_change)
    in 
    (ncntrl, goal1)

    (* Clean up afterwards *)
(*     clean_up_tac ncntrl goal1 *)

let basic_simp_tac cntrl ft goal=
  let (_, g) = basic_simp_link ft cntrl (skip goal)
  in g

(**
   [prove_cond_tac ctrl tg g]: The tactic used to prove the conditions of
   rewrite rules.

   Apply [simp_prep_tac] then [basic_simp_tac].
   Then apply [Logic.Rules.trueR] to solve goal.
*) 
let prove_cond_tac ctrl tg goal=
  Tactics.orl
    [
     Logic.Rules.trueR None (Logic.FTag tg);
     (fun g -> 
       let init_simp_tac ctrl0 tg0 g0=
	 let (ctrl1, g1) = simp_prep_tac ctrl0 tg0 g0
	 in 
	 Tactics.foreach(basic_simp_tac ctrl1 tg0) g1
       in 
       Tactics.seq
	 [
	  init_simp_tac ctrl tg;
	  Logic.Rules.trueR None (Logic.FTag tg)
	] g)
   ] goal


(* inital_flatten_tac fts g:
   prepare sequent for simplification
   flatten all except formulas with tag in fts
   try to prove trivial facts, 
   put conclusions into assumptions (by negation)
 *)

let simp_asm_elims = 
  [(Formula.is_false, (fun x -> Boollib.falseR ~a:x));
   (Formula.is_conj, Logic.Rules.conjA None); 
   (Formula.is_neg, Logic.Rules.negA None); 
   (Formula.is_exists, Logic.Rules.existA None)]

let simp_conc_elims =
  [
   (Formula.is_true, Logic.Rules.trueR None);
   (Formula.is_disj, Logic.Rules.disjC None);
   (Formula.is_all, Logic.Rules.allC None)]

let initial_flatten_tac fts goal=
  (repeat
     (Tactics.orl
 	[ Drule.foreach_conc_except fts simp_conc_elims;
	  Drule.foreach_asm_except fts simp_asm_elims]) goal)




(**
   [once_simp_tac cntrl set l g]

   Simplify formula [label] with [set], once.
*)
let once_simp_tac cntrl set l goal =
  let tag=Logic.label_to_tag l (Drule.sequent goal)
  in 
  let data0 = Data.set_control Data.default cntrl
  in 
  let data1 = Data.set_tactic data0 prove_cond_tac 
  in
  let data2 = Data.set_simpset data1 set
  in 
  let (ncntrl, goal1) =
    chain (fun cdata -> simp_prep_tac cdata tag)
      [ basic_simp_link tag ] data2 goal
  in 
  let goal2 = goal1
  in goal2


 (* Clean up afterwards *)
(* 
   (foreach (clean_up_tac ncntrl) goal1)
*)


(* simp_tac i st:
   not implemented 

   - flatten sequent
   - put conclusions into assumption by negation (** not done yet **)
   - make assumption entries
   - simplify
   - delete temporary assumptions
   - flatten sequent
 *)

let simp_tac cntrl set l goal=
  let tag = Logic.label_to_tag l (Drule.sequent goal)
  in 
  let data0 = Data.set_control Data.default cntrl
  in 
  let data1 = Data.set_tactic data0 prove_cond_tac 
  in
  let data2 = Data.set_simpset data1 set
  in 
  let (ncntrl, goal1) =
    iter_chain (fun cdata -> simp_prep_tac cdata tag)
      (basic_simp_link tag) data2 goal
  in 
  let goal2 = goal1
  in goal2


 (* Clean up afterwards *)
(* 
   (foreach (clean_up_tac ncntrl) goal1)
*)


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
    try (Drule.sequent gl)
    with _ -> raise (Result.error "full_simp_tac: No such formula in goal")
  in 
  let cntrl1=Data.set_simpset cntrl simpset
  in 
  (* prepare the subgoal for simplification *)
  let (prepared_cntrl, prepared_goal) = 
    (try 
      let tmp= simp_prep_tac cntrl1 tg gl
      in (chng:=true; tmp)
    with 
      No_change -> (cntrl1, (skip gl))
    | err -> 
	raise (Result.error "simp_tac: stage 1"))
  in 
  (* invoke the simplifier *)
  let simped_goal = 
    (try 
      Tactics.foreach
	(basic_simp_tac prepared_cntrl tg) prepared_goal
    with No_change -> (chng:=false; skip gl))
  in 
  (* clean up afterwards *)
  let ret_goal=simped_goal
  in 
  if(!chng) 
  then ret_goal
  else raise No_change

