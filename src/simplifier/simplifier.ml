(*-----
 Name: simplifier.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)


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

(**
   [type Data.t]
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

(** visited: formulas visited during the course of simplification *)
	   visited: Tag.t list;
(** rules: 
   rewrite rules to pass to the rewriter (the result of the simplifier)
 *)
	   rules: Logic.rr_type list
       }

    let make sset tac cntrl cd rd a vs rs= 
      {
       simpset=sset;
       cond_tac=tac;
       conds=cd;
       control=cntrl;
       rr_depth=rd;
       asms=a;
       visited = vs;
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

    let set_visited cntrl ds=
      {cntrl with visited=ds}

    let set_rules cntrl ds=
      {cntrl with rules=ds}

    let get_simpset cntrl=cntrl.simpset
    let get_tactic cntrl=cntrl.cond_tac
    let get_control cntrl=cntrl.control

    let add_asm cntrl a=
      set_asms cntrl (a::(cntrl.asms))

    let get_asms cntrl =cntrl.asms

    let get_visited cntrl =cntrl.visited

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
	Formula.default_rr_control 50 50 [] [] [] 

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
      Logic.RRThm x -> (Logic.term_of x)
    |	 _ -> failwith "simp_tac"
  in 
  (List.map strip_rr rrs)

(* utility tactics *)


let cleanup = ref true

(*
   [clean_up_tac ctrl g]
   
   Clean up after simplification.
   Delete all assumptions listed in [ctrl.asms].
 *)


let clean_aux_tac tags g =
  each tags (fun x -> Logic.Rules.delete None (Logic.FTag x)) g

let clean_up_tac ctrl g=
  if (!cleanup) 
  then clean_aux_tac (Data.get_asms ctrl) g
  else (skip g)


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
  | Logic.ORRThm(th, _) ->
      Logic.Rules.cut info th g
  | Logic.Asm(x) ->
      Logic.Rules.copy_asm info x g
  | Logic.OAsm(x, _) ->
      Logic.Rules.copy_asm info x g

(** [prep_cond_tac cntrl values thm g]

   Cut [thm] into the sequent, instantiate with [values].  Apply
   [implA] to get two subgoals, tagged [(cgltg, rgltg)] with condition
   in [cgltg] tagged [cftg] and rewrite-rule in [rgltg] tagged [rrftg].
   Add [rrftg] to [cntrl], getting [ncntrl].

   return g3
   ret=(ncntrl, [cgltg; rgltg], [cftg; rrftg])
 *)
let prep_cond_tac cntrl ret values thm goal =
  let info =Drule.mk_info()
  in
  try 
    let tac1 g1 = cut_rr_rule (Some(info)) thm g1
    in 
    let add_data rrftg x=
      let (cgltg, rgltg)= 
	Lib.get_two (Drule.subgoals info)
	  (Failure "prep_cond_tac: goals")
      in 
      let cftg=
	Lib.get_one (Drule.formulas info)
	  (Failure "prep_cond_tac: forms")
      in 
      let ncntrl= Data.add_asm cntrl rrftg
      in 
      Lib.set_option ret  (ncntrl, (cgltg, rgltg), (cftg, rrftg))
    in 
    let tac2 g2 =
      let rrftg=Lib.get_one (Drule.formulas info) No_change
      in 
      seq[allA_list (ftag rrftg) values;
	  Logic.Rules.implA (Some(info)) (ftag rrftg);
	  data_tac (add_data rrftg) info
	] g2
    in 
    (tac1 ++ tac2) goal
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

let restrict_tac p tac g=
  let ng = tac g
  in 
  if(p ng) then ng else raise (Failure "restrict_tac")

let rec prove_cond_tac cntrl ret values entry goal = 
  let (qs, cnd, _, _, thm)=entry
  in 
  match cnd with
    None -> Lib.set_option ret (cntrl, thm); (skip goal)
  | Some(_) -> 
      let ret1=ref None
      in 
      let tac1 g1= prep_cond_tac cntrl ret1 values thm g1
      in 
      let tac2 g2=
	let (ncntrl1, (cgltg1, rgltg1), (cftg1, rftg1))
	    = Lib.dest_option ~err:(Failure "prove_cond_tac: 1") (!ret1)
	in 
	((fun n -> 
	  (Tag.equal cgltg1 (Drule.node_tag n)))
	   --> 
	     (fun g3 -> 
	       let tac=Data.get_tactic ncntrl1
	       in 
	       seq 
		 [tac ncntrl1 cftg1;
		  data_tac 
		    (fun x -> ret:=(Some x))
		    (ncntrl1, Logic.Asm(Drule.ftag rftg1))]
		 g3)) g2
      in 
      let tac2_pred br=
	match (Drule.branch_subgoals br) with
	  [] -> true
	| [ _ ] -> true
	| _ -> false
      in 
      let tac3 g3 = restrict_tac tac2_pred tac2 g3
      in 
      let tac4 g4=
	let (ncntrl1, (cgltg1, rgltg1), (cftg1, rftg1))
	    = Lib.dest_option ~err:(Failure "prove_cond_tac: 2") (!ret1)
	in 
	let form=
	  Logic.drop_tag(Logic.Sequent.get_tagged_asm 
			   rftg1 (Drule.sequent g4))
	in 
	let rule = 
	  Simpset.make_rule 
	    (Logic.Asm (Drule.ftag rftg1)) 
	    (Formula.term_of form)
	in 
	data_tac (fun x -> ret := Some x)
	  (Data.add_simp_rule ncntrl1 rule,
	   Logic.Asm(Drule.ftag rftg1)) g4
      in
      seq [tac1; (tac3++tac4)] goal


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
let find_basic cntrl ret tyenv rl trm g=
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
  let ret1=ref None
  in 
  let ng=prove_cond_tac cntrl ret1 values rl g
  in 
  let (ncntrl, rr) = Lib.dest_option ~err:(Failure "find_basic: 1") (!ret1)
  in 
  Lib.set_option ret (ncntrl, ntyenv, nt, rr);
  ng

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
	  find_basic cntrl ret tyenv rl t g
	with _ -> find_aux nxt t g
  in 
  find_aux (lookup (Data.get_simpset cntrl) trm) trm goal


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

let rec find_all_matches_tac cntrl ret tyenv trm node =
  let chng = ref false
  and ret1 = ref None
  in 
  let rec find_aux c ty t g= 
    let tac2 g2=
      match (!ret1) with
	None -> 
	  Lib.set_option ret (c, ty, t); 
	  skip g
      | Some (cntrl1, tyenv1, t1, r1) -> 
	  (chng:=true; 
	   Lib.set_option ret ((Data.add_rule cntrl1 r1), tyenv, t1);
	   ((find_aux (Data.add_rule cntrl1 r1) tyenv1 t1 || skip) g2))
    in 
    try
      seq
	[(find_match_tac c ty ret1 t 
	|| (fun g3 -> ret1:=None ; skip g3)); 
	 tac2] g
    with _ -> Lib.set_option ret (c, ty, t); (skip g)
  in 
  let ng=find_aux cntrl tyenv trm node
  in
  if(!chng)
  then 
    match !ret with
      None -> ng
    | Some(nc, ntyenv, nt) ->
	(Lib.set_option ret (nc, ntyenv, nt); ng)
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
let simp_prep_tac control ret tag g= 
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
  Lib.set_option ret newcontrol;
  newgoal

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
let rec find_rrs_bottom_up_tac ctrl ret tyenv t g=
  match t with
    Basic.Qnt(k, q, b) -> 
      (let ret1 = ref None
      in 
      seq[(find_rrs_bottom_up_tac ctrl ret1 tyenv b);
	  (fun g1 -> 
	    let (bcntrl, btyenv, nb) = 
	      Lib.get_option (!ret1) (ctrl, tyenv, b)
	    in 
	    ret1:=None;
	    let g2= 
	      (find_all_matches_tac bcntrl ret1 btyenv (Qnt(k, q, nb)) 
	     || skip) g1
	    in 
	    Lib.set_option ret 
	      (Lib.get_option (!ret1) (bcntrl, btyenv, Qnt(k, q, nb)));
	    g2)] g)
  | Basic.Typed(tt, ty) -> find_rrs_bottom_up_tac ctrl ret tyenv tt g
  | Basic.App(f, a)->
      (let ret1 = ref None
      in 
      seq [find_rrs_bottom_up_tac ctrl ret1 tyenv f;
	   (fun g1 ->
	     let (fcntrl, ftyenv, nf)=
	       Lib.get_option (!ret1) (ctrl, tyenv, f)
	     in 
	     ret1:=None;
	     seq[find_rrs_bottom_up_tac fcntrl ret1 ftyenv a;
		 (fun g2 -> 
		   let (acntrl, atyenv, na) = 
		     Lib.get_option (!ret1)
		       (fcntrl, ftyenv, a)
		   in 
		   let g3=
		     (find_all_matches_tac acntrl ret1 atyenv (App(nf, na)) 
		    || skip) g2
		   in 
		   Lib.set_option ret
		     (Lib.get_option (!ret1) (acntrl, atyenv, App(nf, na)));
		   g3)] g1)] g)
  | _ ->  
      (try 
	find_all_matches_tac ctrl ret tyenv t g
      with No_change -> 
	(Lib.set_option ret (ctrl, tyenv, t); skip g))

(* 
   [find_rrs_top_down cntr t g]

   Top-down search through term [t] for rewrite rules to apply.
 *)
let rec find_rrs_top_down_tac ctrl ret tyenv trm goal=
  let find_td_aux ret1 tyenv1 cntrl1 trm1 g=
    match trm1 with
      Basic.Qnt(k, q, b) -> 
	(let ret2=ref None 
	in 
	let bg=find_rrs_top_down_tac cntrl1 ret2 tyenv1 b g
	in 
	let (bcntrl, btyenv, nb) = 
	  Lib.get_option (!ret2) (cntrl1, tyenv1, b)
	in 
	Lib.set_option ret1 (bcntrl, btyenv, Basic.Qnt(k, q, nb)); bg)
    | Basic.Typed(tt, ty) -> 
	find_rrs_top_down_tac cntrl1 ret1 tyenv1 tt g
    | Basic.App(f, a)->
	(let ret2=ref None
	in 
	seq[find_rrs_top_down_tac cntrl1 ret2 tyenv1 f;
	    (fun g1->
	      let (fcntrl, ftyenv, nf) = 
		Lib.get_option (!ret2) (cntrl1, tyenv1, f)
	      in 
	      ret2:=None;
	      let nag=find_rrs_top_down_tac fcntrl ret2 ftyenv a g1
	      in 
	      let (acntrl, atyenv, na) = 
		Lib.get_option (!ret2) (fcntrl, ftyenv, a)
	      in 
	      Lib.set_option ret1 (acntrl, atyenv, App(nf, na)); nag)] g)
    | _ -> (Lib.set_option ret1 (cntrl1, tyenv1, trm1); skip g)
  in
  let ret1=ref None 
  in 
  let ng = 
    seq
      [(find_all_matches_tac ctrl ret1 tyenv trm || skip);
       (fun g -> 
	 let (nctrl, ntyenv, ntrm)=
	   Lib.get_option (!ret1) (ctrl, tyenv, trm)
	 in 
	 find_td_aux ret1 ntyenv nctrl ntrm g)] goal
  in 
  Lib.set_option ret (Lib.get_option (!ret1) (ctrl, tyenv, trm)); ng


let rec basic_simp_tac cntrl ret ft goal=
  let chng=ref false
  in 
  let tyenv=Drule.typenv_of goal
  in 
  let sqnt = Drule.sequent goal
  in 
  let trm=
    Formula.term_of (Logic.drop_tag (get_form ft sqnt))
  in 
  let ret1=ref None
  in
  let tac1 = 
    let rr_cntrl = Data.get_control cntrl
    in 
    if (rr_cntrl.Rewrite.rr_strat = Rewrite.bottomup)
    then 
      find_rrs_bottom_up_tac cntrl ret1 tyenv trm
    else
      find_rrs_top_down_tac cntrl ret1 tyenv trm
  in 
  let tac2 g2 = 
    let (ncntrl, ntyenv, ntrm) =  
      Lib.dest_option ~err:(Failure "basic_simp_tac: 1") (!ret1)
    in let rrs=List.rev (ncntrl.Data.rules)
    in 
    if rrs=[] then raise No_change
    else 
      let rr_cntrl0 = Data.get_control cntrl
      in 
      let rr_cntrl = 
	Rewrite.control 
	  ~dir:Rewrite.leftright
	  ~max:None
	  ~strat:rr_cntrl0.Rewrite.rr_strat
      in 
      Lib.set_option ret ncntrl;
      (try
	Logic.Rules.rewrite None ~ctrl:rr_cntrl rrs (ftag ft) g2
      with _ -> raise No_change)
  in 
  seq[tac1 ; tac2] goal

(**
   [prove_cond_tac ctrl tg g]: The tactic used to prove the conditions of
   rewrite rules.

   Apply [simp_prep_tac] then [basic_simp_tac].
   Then apply [Logic.Rules.trueR] to solve goal.
 *) 
let prove_cond_tac ctrl tg goal=
  Tactics.alt
    [
     Logic.Rules.trueR None (Logic.FTag tg);
     (fun g -> 
       let init_simp_tac ctrl0 tg0 g0=
	 let ret=ref None
	 in 
	 seq [simp_prep_tac ctrl0 ret tg0;
	      fun g2 -> 
		let ctrl1 = 
		  Lib.dest_option ~err:(Failure "prove_cond_tac: 1") (!ret)
		in 
		basic_simp_tac ctrl1 ret tg0 g2] g0
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
     (Tactics.alt
 	[ Drule.foreach_conc_except fts simp_conc_elims;
	  Drule.foreach_asm_except fts simp_asm_elims]) goal)


let dfst (x, _) = x
let dsnd (_, x) = x

(*
let asm_info_tags = ref []
let concl_info_tags = ref []
*)

(** {6c} Simplification tactics. 

   [make_asm_entries_tac]/[make_concl_entries_tac]: Tactics to prepare
   assumptions/conclusions for use as simp rules.

   [simp_tac]: The standard simplification tactic. Repeatedly
   simplifies formulas until nothing else can be done.

   [once_simp_tac]: Standard simplication applied at most once to each
   formula.
*)

(** [make_asm_entries_tac ret tags except goal]

   Copy, prepare the assumptions in [tags], set ret to [(asm_tags,
   forms)] where [asm_tags] is the list of pairs [(a, e)] where [a] is
   the assumption used to make the rule tagged [e] and [forms] is the
   list of tagged formulas to be used as the simp rules.
*)
let make_asm_entries_tac ret tags except goal=
  let data = ref []
  in 
  (* prepare assumptions *)
  let tac1 g = Simpconvs.prepare_asms data tags except g
  in 
  (* make list of tagged formulas *)
  let tac2 g =
    let sqnt = Drule.sequent g
    in 
    let asm_forms = Drule.asms_of sqnt
    and asm_tags = !data
    in 
    let forms = 
      let use (t, f) = 
	List.exists (fun (_, x) -> Tag.equal t x) asm_tags
      in 
      List.filter use asm_forms
    in 
    data_tac (fun x -> ret:=x) (asm_tags, forms) g
  in 
  seq [tac1; tac2] goal

(** [make_concl_entries_tac ret tags except goal]

   Copy, lift, prepare the conclusions in [tags], set ret to
   [(asm_tags, rules)] where [asm_tags] is a list of pairs [(c, a)]
   where [c] is the conclusion used to form assumption [a] (from which
   a simp rule is formed) and [rules] is the list of tagged formulas
   to be used as simp rules.
*)
let make_concl_entries_tac ret tags except goal=
  let data = ref []
  in 
  (* copy, prepare conclusions *)
  let tac1 g=
    Simpconvs.prepare_concls data tags except g
  in 
  (* make list of tagged formulas *)
  let tac2 g =
    let sqnt = Drule.sequent g
    in 
    let asm_forms = Drule.asms_of sqnt
    and asm_tags = !data
    in 
    let forms=
      let use (t, f) = 
	List.exists (fun (_, x) -> Tag.equal t x) asm_tags
      in 
      List.filter use asm_forms
    in 
    data_tac (fun x -> ret:=x) (asm_tags, forms) g
  in 
  seq [tac1; tac2] goal


(**
   [simp_engine_tac cntrl asms l goal]:
   The engine for [simp_tac]

   - eliminate toplevel universal quantifiers of [l]
   - if (asms=true),
     put conclusions other than [l] into assumptions and make simp rules
   - if (asms=true), make simp rules from assumptions
   - simplify
   - delete temporary assumptions
*)
let simp_engine_tac (cntrl, ret, except, concl_forms) tag goal=
  let concl_rules = 
    Simpset.make_simp_asm_rules 
      (fun x -> not (Tag.equal tag (dfst x))) 
      concl_forms
  in 
  let set=Simpset.add_simp_rule (Data.get_simpset cntrl) concl_rules
  in 
  let cntrl1=Data.set_simpset cntrl set
  in 
  let tac1 g = simp_prep_tac cntrl1 ret tag g
  in 
  let tac2 g =
    let ncntrl = Lib.get_option (!ret) cntrl1
    in 
    ret:=None;
    try 
      basic_simp_tac ncntrl ret tag g
    with e -> 
      (Lib.set_option ret ncntrl; raise e)
  in 
  let trivial g = 
    try Boollib.trivial ~f:(ftag tag) g
    with _ -> skip g
  in 
  ret:=None; 
  seq [tac1; repeat tac2; trivial] goal

(**
   [simp_tac cntrl asms except ?l goal]:
   - eliminate toplevel universal quantifiers of [l]
   - if (asms=true),
     put conclusions other than [l] into assumptions and make simp rules
   - if (asms=true), make simp rules from assumptions
   - simplify
   - delete temporary assumptions

   If [l] is not given, repeat for each conclusion.
   Ignore formulas for which [except] is true.
 *)
let simp_tac cntrl asms except l goal=
  let sqnt = (Drule.sequent goal)
  in 
  let asm_forms = Drule.asms_of sqnt
  and concl_forms = Drule.concls_of sqnt
  in 
  let asm_tags = List.map (fun (x, _) -> x) asm_forms
  and concl_tags = List.map (fun (x, _) -> x) concl_forms
  in 
  let targets = 
    match l with
      None -> concl_tags
    | Some x -> [Logic.label_to_tag x sqnt]
  in
  let data1 =
    Data.set_tactic cntrl prove_cond_tac 
  in
  let set = Data.get_simpset data1
  in 
  let ret=ref (None: Data.t option)
  in 
  let asm_rules = ref ([], [])
  and concl_rules = ref ([], [])
  and asm_entry_tags = ref []
  and concl_entry_tags = ref []
  in 
  let tac1 g= 
   seq
      [
       (fun _ -> asms) 
	 --> 
       seq 
	 [make_asm_entries_tac asm_rules asm_tags except;
	  make_concl_entries_tac concl_rules concl_tags except];
       data_tac 
	 (fun () -> 
       (* get the information, put it into a useful form *)
	   asm_entry_tags := dfst (!asm_rules);
	   concl_entry_tags := dfst (!concl_rules);
       (*
	  update the simp set with the rules for the assumptions,
	  the tags of the visited formulas
	  and the tags of the new formulas.
	*)
	   let rules = 
	     Simpset.make_simp_asm_rules (fun _ -> false) (dsnd(!asm_rules))
	   in 
	   let set1 = Simpset.add_simp_rule set rules
	   in 
	   let data2=
	     Data.set_asms data1
	       (List.append 
		  (List.map dsnd (!asm_entry_tags))
		  (Data.get_asms data1))
	   in 
	   let data2a=
	     Data.set_asms data1
	       (List.append 
		  (List.map dsnd (!concl_entry_tags))
		  (Data.get_asms data2))
	   in 
	   let data3= 
	     Data.set_visited data2a
	       (List.append 
		  (List.map dfst (!asm_entry_tags))
		  (List.append  
		     (List.map dfst (!concl_entry_tags))
		     (Data.get_asms data2a)))
	   in 
	   let data4= 
	     Data.set_simpset data3 set1
	   in 
	    Lib.set_option ret data4) ()] g
  in 
  let chng = ref false 
  in 
  let tac2 g = 
    let ncntrl = Lib.dest_option ~err:(Failure "simp_tac: 1") (!ret) 
    in 
    Tactics.each targets 
      (fun tg -> 
	alt
	  [seq
	     [simp_engine_tac (ncntrl, ret, except, snd (!concl_rules)) tg;
	      data_tac (fun _ -> chng:=true) ()];
	   skip]) g
  in 
  let tac3 g =
    let ncntrl = Lib.dest_option ~err:(Failure "simp_tac: 2") (!ret)
    in 
    clean_up_tac ncntrl g
  in 
  let tac4 g =
    if(!chng) 
    then skip g
    else raise No_change
  in 
  seq [tac1; tac2; tac3; tac4] goal



(**
   [once_simp_tac cntrl set l g]

   Simplify formula [label] with [set], once.

   NOTE: The *only* difference between the code for once_simp_tac and
   the code fore simp_tac is that simp_engine_tac uses [repeat tac2]
   where once_simp_engine_tac has [tac2] (in the last line of the
   tactics).
*)

(**
   [once_simp_engine_tac cntrl asms l goal]:
   The engine for [once_simp_tac]

   - eliminate toplevel universal quantifiers of [l]
   - if (asms=true),
     put conclusions other than [l] into assumptions and make simp rules
   - if (asms=true), make simp rules from assumptions
   - simplify
   - delete temporary assumptions
*)
let once_simp_engine_tac (cntrl, ret, except, concl_forms) tag goal=
  let concl_rules = 
    Simpset.make_simp_asm_rules 
      (fun x -> not (Tag.equal tag (dfst x))) 
      concl_forms
  in 
  let set=Simpset.add_simp_rule (Data.get_simpset cntrl) concl_rules
  in 
  let cntrl1=Data.set_simpset cntrl set
  in 
  let tac1 g = simp_prep_tac cntrl1 ret tag g
  in 
  let tac2 g =
    let ncntrl = Lib.get_option (!ret) cntrl1
    in 
    ret:=None;
    try 
      basic_simp_tac ncntrl ret tag g
    with e -> 
      (Lib.set_option ret ncntrl; raise e)
  in 
  let trivial g = 
    try Boollib.trivial ~f:(ftag tag) g
    with _ -> skip g
  in 
  ret:=None; 
  seq [tac1; tac2; trivial] goal

(**
   [once_simp_tac cntrl asms except ?l goal]:
   - eliminate toplevel universal quantifiers of [l]
   - if (asms=true),
     put conclusions other than [l] into assumptions and make simp rules
   - if (asms=true), make simp rules from assumptions
   - simplify
   - delete temporary assumptions

   If [l] is not given, repeat for each conclusion.
   Ignore formulas for which [except] is true.
 *)
let once_simp_tac cntrl asms except l goal=
  let sqnt = (Drule.sequent goal)
  in 
  let asm_forms = Drule.asms_of sqnt
  and concl_forms = Drule.concls_of sqnt
  in 
  let asm_tags = List.map (fun (x, _) -> x) asm_forms
  and concl_tags = List.map (fun (x, _) -> x) concl_forms
  in 
  let targets = 
    match l with
      None -> concl_tags
    | Some x -> [Logic.label_to_tag x sqnt]
  in
  let data1 =
    Data.set_tactic cntrl prove_cond_tac 
  in
  let set = Data.get_simpset data1
  in 
  let ret=ref (None: Data.t option)
  in 
  let asm_rules = ref ([], [])
  and concl_rules = ref ([], [])
  and asm_entry_tags = ref []
  and concl_entry_tags = ref []
  in 
  let tac1 g= 
   seq
      [
       (fun _ -> asms) 
	 --> 
       seq 
	 [make_asm_entries_tac asm_rules asm_tags except;
	  make_concl_entries_tac concl_rules concl_tags except];
       data_tac 
	 (fun () -> 
       (* get the information, put it into a useful form *)
	   asm_entry_tags := dfst (!asm_rules);
	   concl_entry_tags := dfst (!concl_rules);
       (*
	  update the simp set with the rules for the assumptions,
	  the tags of the visited formulas
	  and the tags of the new formulas.
	*)
	   let rules = 
	     Simpset.make_simp_asm_rules (fun _ -> false) (dsnd(!asm_rules))
	   in 
	   let set1 = Simpset.add_simp_rule set rules
	   in 
	   let data2=
	     Data.set_asms data1
	       (List.append 
		  (List.map dsnd (!asm_entry_tags))
		  (Data.get_asms data1))
	   in 
	   let data2a=
	     Data.set_asms data1
	       (List.append 
		  (List.map dsnd (!concl_entry_tags))
		  (Data.get_asms data2))
	   in 
	   let data3= 
	     Data.set_visited data2a
	       (List.append 
		  (List.map dfst (!asm_entry_tags))
		  (List.append  
		     (List.map dfst (!concl_entry_tags))
		     (Data.get_asms data2a)))
	   in 
	   let data4= 
	     Data.set_simpset data3 set1
	   in 
	    Lib.set_option ret data4) ()] g
  in 
  let chng = ref false 
  in 
  let tac2 g = 
    let ncntrl = Lib.dest_option ~err:(Failure "once_simp_tac: 1") (!ret) 
    in 
    Tactics.each targets 
      (fun tg -> 
	alt
	  [seq
	     [once_simp_engine_tac 
		(ncntrl, ret, except, snd (!concl_rules)) tg;
	      data_tac (fun _ -> chng:=true) ()];
	   skip]) g
  in 
  let tac3 g =
    let ncntrl = Lib.dest_option ~err:(Failure "once_simp_tac: 2") (!ret)
    in 
    clean_up_tac ncntrl g
  in 
  let tac4 g =
    if(!chng) 
    then skip g
    else raise No_change
  in 
  seq [tac1; tac2; tac3; tac4] goal

(*
let once_simp_tac cntrl set l goal =
  let tag=Logic.label_to_tag l (Drule.sequent goal)
  in 
  let data0 = Data.set_control Data.default cntrl
  in 
  let data1 = Data.set_tactic data0 prove_cond_tac 
  in
  let data2 = Data.set_simpset data1 set
  in 
  let ret=ref None
  in 
  seq [simp_prep_tac data2 ret tag;
       (fun g -> 
	 let ncntrl = 
	   Lib.dest_option ~err:(Failure "once_simp_tac: 1") (!ret)
	 in 
	 ret:=None; basic_simp_tac ncntrl ret tag g);
       (fun g-> 
	 let ncntrl = 
	   Lib.dest_option ~err:(Failure "once_simp_tac: 2") (!ret)
	 in 
	 clean_up_tac ncntrl g)	   
     ] goal
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
(*
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
 *)
