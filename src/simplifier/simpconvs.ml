module Simpconvs =
struct

  let dest_option x=
    match x with 
      None -> raise (Failure "dest_option")
    | Some(y) -> y

let make_iff_equals_ax ()=
  let iff_l1 = prove_goal "!x y: (x = y ) => (x => y)"
      (thenl[flatten_tac; replace (-2) (-1); basic])
  in 
  let iff_l2 = prove_goal
      "!x y: ((x => y) and (y => x)) => (x=y)"
      (thenl [
       flatten_tac; 
       cut_thm "bool_cases"; allE "x_1";
       cut_thm "bool_cases"; allE "y_1";
       split_tac;
       replace (-1) 1; flatten_tac;
       replace (-2) 1; flatten_tac;
       replace (-1) 1; replace(-2) 1; eq_tac;
       split_tac;
       replace (-1) 1; flatten_tac;
       basic;
       split_tac; 
       basic;
       replace (-2) (-4); flatten_tac;
       split_tac;
       replace (-2) 2; flatten_tac;
       basic;
       replace (-1) (-3); flatten_tac;
       replace (-1) 1; replace (-2) 1; eq_tac])
  in 
  let iff_l3 = 
    prove_goal "!x y: (x iff y) iff (x = y)"
      (thenl[
       flatten_tac; unfold "iff" 1; 
       conjI ; flatten_tac; cut iff_l2; allE "x_1"; allE "y_1"; 
       implE; conjI; flatten_tac;  
       implE; basic; basic; basic; basic;
       flatten_tac;
       replace (-1) 1;
       conjI;
       repeat (flatten_tac++basic)])
  in 
  prove_goal "!x y: (x iff y) = (x = y)"
    (thenl [flatten_tac; cut iff_l2; allE "x_1 iff y_1"; allE "x_1 = y_1";
	    split_tac; flatten_tac;
	    cut iff_l2; allE "x_1"; allE "y_1";
	    unfold "iff" (-2);
	    implE; basic; basic;
	    flatten_tac; 
	    replace (-1) 1; unfold "iff" 1;
	    split_tac;
	    repeat (implI++basic);
	    basic]);;

let iff_equals_ax = ref None
let get_iff_equals_ax ()=
  match !iff_equals_ax with
    None -> 
      let nthm = make_iff_equals_ax()
      in 
      iff_equals_ax := Some(nthm);
      nthm
  | Some(x) -> x

let make_rule_true_ax ()= 
  let rule_true_l1 =  prove_goal "!x: (x=true) => x" 
      (thenl [flatten_tac;(replace (-1) 1);trivial])
  in
  let rule_true_l2 = prove_goal "!x: x => (x=true)"
      (thenl [flatten_tac; cut_thm "bool_cases";
	      allE "x_1"; disjI; replace (-1) 1; eq_tac;
	      replace (-1) (-2); rewrite_thm "false_def" (-2);
	      flatten_tac])
  in
  let rule_true_l3 = prove_goal "!x: x iff (x=true)"
      (thenl [flatten_tac; rewrite_thm "iff" 1;
	      conjI; cut rule_true_l1; unify_tac (-1) 1; 
	      cut rule_true_l2; unify_tac (-1) 1])
      
  in 
  Logic.ThmRules.rewrite_conv (scope()) 
    [get_iff_equals_ax()] rule_true_l3;;


let rule_true_ax = ref None
let get_rule_true_ax ()= 
  match !rule_true_ax with
    None -> 
      let nthm =make_rule_true_ax()
      in 
      rule_true_ax:=Some(nthm);
      nthm
  | Some(t) -> t

let make_rule_false_ax ()= prove_goal"! x : (not x)=(x=false)"
    (thenl 
       [ 
	 flatten_tac;
	 Logic.Rules.rewrite_any ~dir:false ~simple:true 
	   [Logic.RRThm(get_iff_equals_ax())] 1;
	 cut_thm "bool_cases"; allE "x_1";
	 unfold "iff" 1; disjI;
	 replace (-1) 1;
	 conjI; 
	 flatten_tac;
	 replace (-2) (-1); flatten_tac;
	 replace (-1) 1;
	 conjI; 
	 flatten_tac;
	 eq_tac; flatten_tac]);;

let rule_false_ax = ref None
let get_rule_false_ax ()= 
  match !rule_false_ax with
    None -> 
      let nthm =make_rule_false_ax()
      in 
      rule_true_ax:=Some(nthm);
      nthm
  | Some(t) -> t

(* case_tac x sq: adds formula x to assumptions of sq, 
   creates new subgoal in which to prove x

   g|asm |- cncl      --> g'|asm |- t:x, cncl, g| t:x, asm |- cncl 

info: [g', g] [t]
*)

let case_tac_thm = ref None

let get_case_thm ()=
  match !case_tac_thm with
    None ->
      let nthm =
	prove_goal "!P: (not P) or P"
	  (thenl [flatten_tac; basic])
      in 
      case_tac_thm := Some(nthm);
      nthm
  | Some(t) -> t

let get_one ls err=
  match ls with
    [x] -> x
  | _ -> raise err

let get_two ls err=
  match ls with
    [x; y] -> (x, y)
  | _ -> raise err

let case_info inf (x:Term.term) g= 
  let thm =  get_case_thm()
  and tinf=ref(Logic.Rules.make_tag_record [] [])
  in 
  let g1=Logic.Rules.cut_full (Some inf) thm g
  in 
  let nt=get_one ((!tinf).Logic.Rules.forms) (Failure "case_info")
  in 
  let g2=
    thenl[
    Logic.Rules.allE_full None x (Logic.FTag nt);
    Logic.Rules.disjE_full (Some tinf) (Logic.FTag nt);
    Logic.Rules.negA_full None (Logic.FTag nt) ] g1
  in 
  let ng1, ng2=get_two (!tinf).Logic.Rules.goals (Failure "case_info")
  in 
  Logic.Rules.do_tag_info (Some inf) [ng1;ng2] [nt];
  g2

end
