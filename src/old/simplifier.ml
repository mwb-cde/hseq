module Simplifier =
struct

(* needed properties *)

let scope () = Tpenv.typenv()

let saxiom str = 
  Logic.mk_axiom 
	  (Formula.form_of_term (scope()) (Tpenv.read str))

let cond_rule_true_ax = saxiom "! a b: (a=>b)=(a=>(b=true))"
let rule_true_ax = saxiom "! a : (a=true)=a"
let iff_equals_ax = saxiom "! a b: (a iff b)=(a=b)"

open Term
open Logicterm

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
      Corepp.list_print (print_term st) 
	(fun _ -> Format.print_string ","; 
	  Format.print_break 1 2; 
	  Format.close_box(); Format.open_box 0)
	(self#get());
      Format.close_box();
      Format.close_box();
  end

  let error s t = Result.mkError((new simpError s t):>Result.error)
  let addError s t e =
    Result.addError e (error s t) 


  exception No_change

(* control: 
  cond_depth: (* maximum number of conditions at one go *)
*)

type control =
    {
     cond_depth: int
   }


let dec_cond_depth set=
  if(set.cond_depth=0) then set
  else
    {cond_depth=(set.cond_depth -1)}

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
let sqnt_exists st g =
  try
    ignore(Logic.goal_focus st g); true
  with _ -> false
*)
(* sqnt_solved: true if sqnt st is no longer in goal g *)

let sqnt_solved st g =
  if(Logic.has_subgoals g)
  then 
    not(Logic.Tag.equal st (Logic.sqnt_tag (Logic.get_sqnt g)))
  else true


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
	

(* simp_flatten_tac
   flatten all except ft (formula being dealt with) *)
  let simp_flatten_tac i g=
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
    let ft=Logic.index_to_tag i (Logic.get_sqnt g)
    in 
    repeat_for_sqnt
      (Logic.Rules.orl 
	 [ Drule.foreach_conc_except ft conc_elims; 
	   Drule.foreach_asm_except ft asm_elims]) g


(* 
   initial flatten
   flatten a goal without creating conclusions or assumptions
*)

let initial_flatten ft g=
    let conc_elims =
      [Logic.Rules.trueR ft;
(*      Logic.Rules.implI;*)
      Logic.Rules.allI ft]
    and asm_elims = 
      [	Bool_tacs.false_rule0 ft;
	Logic.Rules.conjE ft; 
	Logic.Rules.existI ft]
    in 
    repeat_for_sqnt 
      (Logic.Rules.orl 
	 [Logic.Rules.orl conc_elims;
	  Logic.Rules.orl asm_elims]) g


(* simp_prep_tac ft st:
   skolemise formula ft,
   flatten other formulas in sequent
   reduce to true where possible (removing the subgoal)
*)

let simp_prep_tac ft st g=
    let chng=ref false
    in 
    let retg=
      if(sqnt_solved st g)
      then g
      else 
	let ng = 
	  try 
	    let tmp=initial_flatten ft g
	    in (chng:=true; tmp)
	  with _ -> g
	in 
	if(sqnt_solved st ng)
	then ng
	else 
	  try 
	    let tmp=simp_flatten_tac ft ng
	    in chng:=true; tmp
	  with _ -> ng
    in 
    if(!chng) then retg
    else raise No_change
	

(*
   prep_cond_tac: 
   push a theorem into a sequent,
   instantiate outermost quantifiers,
   apply implE to create new sequent 
   with condition as conclusion to prove
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

(*
let monitor=ref []
*)

(*
let args = ref []
*)

let prep_cond_tac qs env thm g =
  let info = ref (Logic.Rules.make_tag_record [] [])
  and cut_thm t =
    match t with
      Logic.RRThm(th) ->
	Logic.Rules.cut_info info th g
    | Logic.Asm(i) ->
	Logic.Rules.copy_asm_info info i g
    | Logic.Tagged(tg) ->
	Logic.Rules.copy_asm_info info 
	  (Logic.tag_to_index tg (Logic.get_sqnt g)) g
  in 
(*  args:=(qs, env, thm, g)::!args; *)
  try 
    let g1=cut_thm thm
    in 
    let ft=
      (match (!info).Logic.Rules.forms with
	[tg] -> tg
      |	_ -> raise No_change)
    in 
    let g2=allE_list (Logic.tag_to_index ft (Logic.get_sqnt g1)) 
	(make_consts qs env) g1
    in 
    let g3=Logic.Rules.implE_info info 
	(Logic.tag_to_index ft (Logic.get_sqnt g2)) g2
    in 
(*    monitor:=g3::(!monitor); *)
    match (!info).Logic.Rules.goals with
      [tl;tr] -> ([tl;tr], ft, g3)
    | _ -> raise No_change
  with _ -> raise No_change

(* match_rewrite:
   try to match lhs with trm, return rhs if sucessful
*)

  let match_rewrite scp tyenv tenv varp lhs rhs trm = 
    let find_match_rewrite()=
      try 
	Unify.unify_fullenv_rewrite scp tyenv tenv varp lhs trm
      with x -> 
	failwith "Can't match terms"
    in 
    try
      (ignore(find_match_rewrite ());
       Term.subst_env tenv rhs)
    with x -> (failwith "match_rewrite")


(* simp_tac: toplevel for simplifier

   basic_simp_tac: workhorse of simplifier
   basic_simp_tac cntrl:control set:rule_set ft:Tag.t st:Tag.t g:goal
   where ft is tag of formula to work on
   and st is tag of sequent to work on
*)

let is_true t = Term.is_true t

let rec basic_simp_tac cntrl set ft st g=
  let sqnt=Logic.get_sqnt g
  in 
  let scp = Logic.scope_of sqnt
  in 
(* 
   find_rrs: scope -> Term.substitution -> rule_set -> term
   make list of rewrite rules using unification
   Term.substitution is used to keep track of changing term
   as rewrites are found
*)

(* prove_condition: try to prove the condition is true
   by applying simplification rules
*)

  let rec prove_condition scp set tenv entry g = 
    if(cntrl.cond_depth=0) then raise No_change
    else
      let (qs, cnd, _, _, thm)=entry
      in 
      (match cnd with
	None -> ()
      | Some(cd) -> 
	  let (_, _, ng)=prep_cond_tac qs tenv thm g
	  in 
	  raise No_change)
  in 

(* find_match: find rules which match a term *)

  let rec find_match scp tyenv rslt set trm =
    let chng=ref false
    in 
    let rec find_basic_aux rls t= 
      match rls with
	[] -> t
      | ((qs, c, lhs, rhs, thm)::nxt) ->
	  (try 
	    let tenv = Term.empty_subst ()
	    in 
	    let nt=
	      match_rewrite scp tyenv tenv
		(Rewrite.is_free_binder qs) lhs rhs t
	    in 
	    prove_condition scp set tenv (qs, c, lhs, rhs, thm) g;
	    rslt:=thm::(!rslt); 
	    chng:=true; nt
	  with _ -> find_basic_aux nxt t)
    in 
(*
    let nb=find_basic_aux (Net.lookup trm set.basic) trm
*)
    let nb=find_basic_aux (lookup set trm) trm
    in 
    if (!chng) 
    then find_match scp tyenv rslt set nb
    else nb

(* find_basic_match scp tyenv set trm rslt*) 

  in
  let rec find_rrs scp tyenv set trm =
    let rslt=ref[]
    in 
    let rec find_rrs_aux t=
      match t with
	Term.Qnt(q, b) -> 
	  (let nb = find_rrs_aux b
	  in 
	  find_match scp tyenv rslt set (Qnt(q, nb)))
      | Term.Typed(tt, ty) -> 
	  find_rrs_aux tt
      | Term.App(f, a)->
	  let nf = 
	    (find_rrs_aux f)
	  in 
	  let na = (find_rrs_aux a)
	  in 
	  (find_match scp tyenv rslt set (Term.App(nf, na)))
      | _ -> (find_match scp tyenv rslt set t)
    in
    let nt=find_rrs_aux trm
    in List.rev(!rslt)

(*
   make_rr_list: Gtypes.scope -> rule_set -> term -> rr_type list
   make a list of rewrites with which to simplify the term
*)
  in 
  let trm=Formula.dest_form(Logic.drop_tag(Logic.get_tagged_form ft sqnt))
  in 
  let rrs=
    find_rrs scp (Gtypes.empty_subst()) set trm
  in 
  if rrs=[]
  then raise No_change
  else 
    (try
      Logic.Rules.rewrite_any rrs (Logic.tag_to_index ft sqnt) g
    with _ -> raise No_change)


let simp_tac set i g =
  let sqnt=(Logic.get_sqnt g)
  in 
  let st=Logic.sqnt_tag sqnt
  and ft=Logic.index_to_tag i sqnt
  in 
  basic_simp_tac set ft st g



let add_simp_rule set scp src =
  let (l, (vs, c, a, b, r))= make_entry scp src
  in 
  let varp x= Rewrite.is_free_binder vs x
  and lt (_, _, _, x, _) (_, _, _, y, _)=term_gt y x
  in
  {cond_depth=set.cond_depth;
   assumptions=set.assumptions; 
   basic=Net.insert lt varp (l, (vs, c, a, b, r)) set.basic}


(* tests *)


let simp_set = ref(empty_set())

(* add_simp f: add theorem thm to simp_set *)

let add_simp thm = 
  simp_set:=add_simp_rule (!simp_set)  (scope()) thm

(* empty_simp: empty simp set *)
let empty_simp () = simp_set:=empty_set()

end

