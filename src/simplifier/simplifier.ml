
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

module Control =
  struct

(* [type Control.t]
   Information used by and built up during simplification.
 *)
    type t =
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

    let make cd rd a rs= 
      {
       conds=cd;
       rr_depth=rd;
       asms=a;
       rules=rs
     }

    let set_conds cntrl d=
      {cntrl with conds=d}

    let set_rr_depth cntrl d=
      {cntrl with rr_depth=d}

    let set_asms cntrl ds=
      {cntrl with asms=ds}

    let set_rules cntrl ds=
      {cntrl with rules=ds}

    let add_asm cntrl a=
      set_asms cntrl (a::(cntrl.asms))

    let dec_cond_depth cntrl=
      set_conds cntrl ((cntrl.conds)-1)

    let add_rule cntrl r=
      set_rules cntrl (r::(cntrl.rules))

(** [mk_control()]
   The default control information 
 *)
    let default ()= make 50 50 [] [] 

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
let prep_cond_tac cntrl values thm g =
  let info =Drule.mk_info()
  in
  try 
    let g1=cut_rr_rule (Some(info)) thm g
    in 
    let rrftg=Lib.get_one (Drule.formulas info) No_change
    in 
    let g2=foreach (allA_list (ftag rrftg) values) g1
    in 
    let g3=foreach (Logic.Rules.implA (Some(info)) (ftag rrftg)) g2
    in 
    let (cgltg, rgltg)= 
      Lib.get_two (Drule.subgoals info)
	(Failure "prep_cond_tac: goals")
    in 
    let cftg=
      Lib.get_one (Drule.formulas info)
	(Failure "prep_cond_tac: forms")
    in 
    let ncntrl= Control.add_asm cntrl rrftg
    in 
    (ncntrl, (cgltg, rgltg), (cftg, rrftg), g3)
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
let rec prove_cond_rule cntrl tac values entry g = 
  let (qs, cnd, _, _, thm)=entry
  in 
  match cnd with
    None -> (cntrl, thm, (skip g))
  | Some(cd) -> 
      let (ncntrl, (cgltg, rgltg), (cftg, rftg), ng)=
	prep_cond_tac cntrl values thm g
      in 
      let ng1= 
	try 
	  foreach (tac ncntrl cftg) ng
	with _ -> ng
      in 
      if(sqnt_solved cgltg ng1)
      then 
	(ncntrl, Logic.Asm(Drule.ftag rftg), ng1)
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

   Try to match rule [rl] with term [trm] in node [g].
   If [rl] matches but is conditional, try to prove the condition
   using tactic [tac].

   returns rewritten term, matched rules and new goal.
 *)
let find_basic cntrl tyenv tac rl trm g=
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
  let (ncntrl, rr, ng)=prove_cond_rule cntrl tac values rl g
  in 
  (ncntrl, nt, rr, ng)

(** [find_match scp tyenv rslt set tac trm g]
   Find rule in simpset [set] which matches term [trm] in goal [g].

   If a possible match is conditional, try to prove the condition
   using tactic [tac].

   raise No_change if no matches.

   [find_match_tac ret c tyenv set tac t g] Apply [find_match c tyenv
   set tac g].  If sucessful, set [ret] to the rewritten term,
   matching rule and new goal.

   raise No_change if no matches.
 *)
let find_match cntrl tyenv set tac ret trm (goal: Logic.node)=
  let rec find_aux rls t g= 
    match rls with
      [] -> raise No_change
    | (rl::nxt) ->
	try 
	  find_basic cntrl tyenv tac rl t g
	with _ -> find_aux nxt t g
  in 
  let (ncntrl, nt, rr, ng) = find_aux (lookup set trm) trm goal
  in 
  ret:=(Some(ncntrl, nt, rr)); ng


(*
   let find_match_tac ret c tyenv set tac t g=
   let (ncntrl, nt, rr, ng) = find_match c tyenv set tac t g
   in 
   ret:=(Some(ncntrl, nt, rr)); ng
 *)

(** [find_all_matches scp tyenv rslt set tac trm g]

   Find all rules in simpset [set] which can be used to rewrite
   term [trm] in goal [g].

   puts matched rules into rslt
   returns rewritten term, matched rules and new goal.

   Messy implementation:

   Apply find_match to a term, store result,
   repeat until find_match fails. 
   Return result and last sucessfull goal.
 *)
let rec find_all_matches cntrl tyenv set tac trm branch=
  let chng = ref false
  and ret=ref (cntrl, trm)
  in 
  let rec find_aux g= 
    (let (c, t) = (!ret)
    in 
    let ret1=ref None
    in 
    (try
      let ng=foreach (find_match c tyenv set tac ret1 t) g
      in 
      let (ncntrl, nt, nr) = 
	match (!ret1) with
	  None -> raise (Failure "find_all_matches: No matches")
	| (Some x) -> x
      in 
      chng:=true;
      ret:=(Control.add_rule cntrl nr, nt);
      find_aux ng
    with _ -> g))
  in 
  let ng=find_aux branch
  in 
  let (nc, nt)=(!ret)
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


let rec basic_simp_tac cntrl set ft goal=
  let sqnt=Drule.sequent goal
  in 
  let chng=ref false
  in 
  let tyenv=Gtypes.empty_subst()
  in 
  (* [prove_cond_tac]
     The tactic used to prove conditions.
   *)
  let prove_cond_tac ctrl tg g=
    let init_simp_tac ctrl0 tg0 g0=
      let (ctrl1, g1) = simp_prep_tac ctrl0 tg0 g0
      in 
      Tactics.foreach(basic_simp_tac ctrl1 set tg0) g1
    in 
    Tactics.orl
      [
       Tactics.seq
	 [
	  init_simp_tac ctrl tg;
	  Logic.Rules.trueR None (Logic.FTag tg)
	];
       Logic.Rules.trueR None (Logic.FTag tg)] g
  in 
(* 
   find_rrs_bottom_up: scope -> Term.substitution -> rule_set -> term
   make list of rewrite rules using unification
   Term.substitution is used to keep track of changing term
   as rewrites are found
 *)
  let rec find_rrs_bottom_up ctrl t g=
    match t with
      Basic.Qnt(k, q, b) -> 
	(let (bcntrl, nb, bg) = find_rrs_bottom_up ctrl b g
	in 
	try 
	  find_all_matches bcntrl tyenv set 
	    prove_cond_tac (Qnt(k, q, nb)) bg
	with No_change -> (bcntrl, Qnt(k, q, nb), bg))
    | Basic.Typed(tt, ty) -> find_rrs_bottom_up ctrl tt g
    | Basic.App(f, a)->
	(let (fcntrl, nf, nfg) = (find_rrs_bottom_up ctrl f g)
	in 
	let (acntrl, na, nag)= (find_rrs_bottom_up fcntrl a nfg)
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
(* 
   find_rrs_top_down: scope -> Term.substitution -> rule_set -> term
   make list of rewrite rules using unification
   Term.substitution is used to keep track of changing term
   as rewrites are found
 *)
  let rec find_rrs_top_down ctrl t g=
    let find_td_aux c t g=
      match t with
	Basic.Qnt(k, q, b) -> 
	  (let (bcntrl, nb, bg) = find_rrs_top_down ctrl b g
	  in 
	  (bcntrl, Qnt(k, q, nb), bg))
      | Basic.Typed(tt, ty) -> find_rrs_top_down ctrl tt g
      | Basic.App(f, a)->
	  (let (fcntrl, nf, nfg) = (find_rrs_top_down ctrl f g)
	  in 
	  let (acntrl, na, nag)= (find_rrs_top_down fcntrl a nfg)
	  in 
	  (acntrl, App(nf, na), nag))
      | _ -> (ctrl, t, g)
    in
    (try 
      let (nctrl, ntrm, ng) = 
	find_all_matches ctrl tyenv set prove_cond_tac t g
      in 
      find_td_aux nctrl ntrm ng
    with No_change -> (ctrl, t, g))
      
  in
  let trm=
    Formula.dest_form
      (Logic.drop_tag
	 (Logic.Sequent.get_tagged_form ft sqnt))
  in 
  let (ncntrl, ntrm, ngoal)= find_rrs_top_down cntrl trm (skip goal)
  in 
  let rrs=List.rev (ncntrl.Control.rules)
  in 
  if rrs=[]
  then raise No_change
  else 
    (try
      Tactics.foreach (Logic.Rules.rewrite None rrs (ftag ft)) ngoal
    with _ -> raise No_change)




(* inital_flatten_tac fts g:
   prepare sequent for simplification
   flatten all except formulas with tag in fts
   try to prove trivial facts, 
   put conclusions into assumptions (by negation)
 *)

let simp_asm_elims = 
  [(Formula.is_false, (fun x -> Boollib.false_rule ~a:x));
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


(* simp_tac i st:
   - flatten sequent
   - put conclusions into assumption by negation (** not done yet **)
   - make assumption entries
   - simplify
   - delete temporary assumptions
   - flatten sequent
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
  (* prepare the subgoal for simplification *)
  let (prepared_cntrl, prepared_goal) = 
    (try 
      let tmp= simp_prep_tac cntrl tg gl
      in (chng:=true; tmp)
    with 
      No_change -> (cntrl, (skip gl))
    | err -> 
	raise 
	  (Result.error "simp_tac: stage 1"))
  in 
  (* invoke the simplifier *)
  let simped_goal = 
    (try 
      Tactics.foreach
	(basic_simp_tac prepared_cntrl simpset tg) prepared_goal
    with No_change -> (chng:=false; skip gl))
  in 
  (* clean up afterwards *)
  let ret_goal=simped_goal
  in 
  if(!chng) 
  then ret_goal
  else raise No_change

