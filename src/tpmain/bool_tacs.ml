
open Drule
open Commands
open Tactics


let eq_tac0 sqnt = 
  let a = first_concl Formula.is_equals (Logic.get_sqnt sqnt)
  in 
  let th = 
    try lemma "base.eq_sym"
    with Not_found -> 
      (raise (Result.error "Can't find required lemma base.eq_sym"))
  in thenl [cut th; unify_tac ~a:(fnum (-1)) ~c:a] sqnt

let eq_tac sqnt =  eq_tac0 sqnt

let cut_thm str = (cut (lemma str))

let unfold str i sqnt= 
  let j= if i<0 then i-1 else i
  in
  (thenl[cut (defn str); replace (fnum(-1)) (fnum j); 
	 delete (fnum (-1))]) sqnt

let rewrite_thm str i= 
  Tactics.rewrite_thm [lemma str] ~dir:leftright i

let rewrite_rl str i= 
  Tactics.rewrite_thm[lemma str] ~dir:rightleft i

let rewrite_lr str i= 
  Tactics.rewrite_thm[lemma str] ~dir:leftright i

let rewrite th i= 
  Tactics.rewrite_thm[th] i
(*
  let j= if i<0 then i-1 else i
  in
  thenl[(cut th);(replace (-1) j); delete (fnum (-1))]
*)

let rewrite_dir dir thms i= 
  Tactics.rewrite_thm thms ~dir:dir i

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

let iffI_rule i goal = 
  let sqnt=Logic.get_sqnt goal
  in 
  let t, f = Logic.get_tagged_cncl (Logic.fident_to_tag i sqnt) sqnt
  in
  if not (is_iff f) then (raise (Result.error "iffI_rule"))
  else 
    (thenl 
       [Tactics.rewrite_thm [lemma "boolean.iff_def"] ~dir:leftright (ftag t);
	Logic.Rules.conjI None (ftag t);
	Logic.Rules.implI None (ftag t)]) goal

let iffI ?c g = 
  let cf = 
    match c with
      Some x -> x
    | _ -> (first_concl is_iff (Logic.get_sqnt g))
  in 
  iffI_rule cf g

let false_rule0 a sq =
  let  thm = lemma "base.false_def"
  in 
  thenl [(Tactics.rewrite_thm [thm]  a); 
	 Logic.Rules.negA None a; 
	 trivial] sq

let false_rule ?a goal =
  let af =
    match a with
      Some x -> x
    | _ -> Drule.first_asm Formula.is_false (Logic.get_sqnt goal)
    in 
    false_rule0 af goal 

let asm_elims () = 
  [	(Formula.is_false, (fun x -> false_rule ~a:x));
    (Formula.is_neg, Logic.Rules.negA None);  
    (Formula.is_conj, Logic.Rules.conjE None); 
(*	(Formula.is_implies, Drule.mp_basic_rule); *)
    (Formula.is_exists, Logic.Rules.existI None)]


(*
   (fun x -> Logic.Rules.orl[Drule.mp_basic_rule x; Logic.Rules.implE x]));
 *)

let conc_elims () =
  [
   (Formula.is_true, Logic.Rules.trueR None);
   (Formula.is_neg, Logic.Rules.negC None); 
   (Formula.is_disj, Logic.Rules.disjE None);
   (Formula.is_implies, Logic.Rules.implI None);
   (Formula.is_all, Logic.Rules.allI None)]

(*
   let rec flatten_tac g =
   rule_tac(repeat
   (rule_tac
   (Logic.Rules.apply_list 
   [ Drule.foreach_conc (conc_elims()); 
   Drule.foreach_asm (asm_elims())]))) g
 *)

let rec flatten_tac g =
  repeat
    (Tactics.orl 
       [ Drule.foreach_conc (conc_elims()); 
	 Drule.foreach_asm (asm_elims())]) g


let split_asm () = 
  [(Formula.is_disj, Logic.Rules.disjI None);  
   (Formula.is_implies, Logic.Rules.implE None)]

let split_conc () =
  [(Formula.is_conj, Logic.Rules.conjI None); 
   (Formula.is_disj, Logic.Rules.disjE None);
   (is_iff, iffI_rule)]

let split_tac g=
  repeat
    (apply_list 
       [ Drule.foreach_conc (split_conc()); 
	 Drule.foreach_asm (split_asm())]) g

let inst_asm_rule i l sqnt=
  let rec rule ys sqs = 
    match ys with 
      [] -> sqs
    | (x::xs) -> 
	let nsqnt=
	    (Logic.Rules.allE None x i) sqs
	in rule xs nsqnt
  in rule l sqnt

let inst_concl_rule i l sqnt=
  let rec rule ys sqs = 
    match ys with 
      [] -> sqs
    | (x::xs) -> 
	let nsqnt=
	    (Logic.Rules.existE None x i) sqs
	in rule xs nsqnt
  in rule l sqnt

(*
let inst_term_rule i l sqnt=
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
*)

let inst_asm ?a l g=
  let af = 
    match a with 
      Some x -> x
    | _ -> (Drule.first_asm (Formula.is_all) (Logic.get_sqnt g))
  in 
  inst_asm_rule af l g

let inst_concl ?c l g=
  let cf = 
    match c with 
      Some x -> x
    | _ -> (Drule.first_concl (Formula.is_all) (Logic.get_sqnt g))
  in 
  inst_concl_rule cf l g

let inst_tac f l g= 
  let sqnt = Logic.get_sqnt g
  in 
  try 
    ignore(Logic.get_fident_asm f sqnt);
    inst_asm ~a:f l g
  with Not_found -> inst_concl ~c:f l g

let cases_tac0 (x:Basic.term) g= 
  let thm = 
    try
      lemma "boolean.cases_thm"
    with Not_found -> 
      (raise (Result.error "Can't find required lemma boolean.cases_thm"))
  in 
  thenl
	     [cut thm; allE x; disjI; negA; postpone] g

let cases_tac x = cases_tac0 (Tpenv.read_unchecked x)

let equals_tac ?f g =
  let ff =
    match f with
      Some x -> x
    | _ -> 
	try
	  (Drule.first_asm Formula.is_equals (Logic.get_sqnt g))
	with 
	  Not_found ->
	    try
	      (Drule.first_concl Formula.is_equals (Logic.get_sqnt g))
	    with Not_found ->
	      raise 
		(Result.error "equals_tac: No equality term")
  in 
  let thm = 
    try
      lemma "boolean.equals_bool"
    with Not_found -> 
      (raise (Result.error "Can't find required lemma boolean.equals_bool"))
  in 
  (rewrite thm ff g)

let false_tac g = false_rule g

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
    | _ -> (raise (Result.error "hyp_conc_thm: unusually shaped implication"))
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
  let ncnsts = List.map (fun x -> lookup (Basic.Bound x) qenv) qnts
  in 
  (thenl 
     [Tactics.cut thm; inst_tac (fnum(-1)) ncnsts; 
      Logic.Rules.implE None (fnum (-1));
      Logic.Rules.postpone; 
      Tactics.unify_tac ~a:(fnum(-1)) ~c:(fnum i)]) sq

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
  let ncnsts =List.rev(List.map (fun x -> lookup (Basic.Bound x) qenv) qnts)
  in 
  thenl 
    [inst_tac (fnum j) ncnsts; Logic.Rules.implE None (fnum j);
     Logic.Rules.postpone; 
     Tactics.unify_tac ~a:(fnum j) ~c:(fnum i)] sq

let back_mp_tac j i g =match_mp_sqnt_rule0 j i g
