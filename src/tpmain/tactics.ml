
open Logic

type tactic = Logic.rule

let rule_tac r g =  r g

let rotateA g 
    = Logic.Rules.rotate_asms g
let rotateC g 
    = Logic.Rules.rotate_cncls  g

let copy_asm i g 
    = rule_tac (Logic.Rules.copy_asm i) g
let copy_concl i g 
    = rule_tac (Logic.Rules.copy_cncl i) g

let lift id g = 
  if (id<0) then Logic.Rules.lift_asm (Logic.FNum id) g
  else Logic.Rules.lift_concl (Logic.FNum id) g

let skip g= g

let trivial = rule_tac Drule.trueR

let find_basic sq = 
  let ams = Drule.asm_forms sq
  in 
  let rec find_basic_aux xs i =
    match xs with
      [] -> raise Not_found
    | c::cs -> 
	try 
	  (-(Drule.first 
	       (fun x-> Formula.alpha_convp 
		   (Logic.scope_of sq) x c) ams), i)
	with Not_found -> find_basic_aux cs (i+1)
  in find_basic_aux (Drule.concl_forms sq) 1

let basic0 sqnt = 
  let sq=Logic.get_sqnt sqnt
  in 
  try
    let a,c = find_basic sq
    in Logic.Rules.unify a c sqnt
  with Not_found -> Result.raiseError "Not basic"

let basic sqnt = rule_tac basic0 sqnt

let postpone = Logic.Rules.postpone

let unify_tac i j=  (Logic.Rules.unify i j)
let cut th =  (Logic.Rules.cut th)


let implI0 sq =
  let c=Drule.first_concl Formula.is_implies (Logic.get_sqnt sq)
  in Logic.Rules.implI c sq

let implE0 sq =
  let c=Drule.first_asm Formula.is_implies (Logic.get_sqnt sq)
  in Logic.Rules.implE c sq

let conjI0 sq =
  let c=Drule.first_concl Formula.is_conj (Logic.get_sqnt sq)
  in Logic.Rules.conjI c sq

let conjE0 sq =
  let c=Drule.first_asm Formula.is_conj (Logic.get_sqnt sq)
  in Logic.Rules.conjE c sq

let disjI0 sq =
  let c=Drule.first_asm Formula.is_disj (Logic.get_sqnt sq)
  in Logic.Rules.disjI c sq

let disjE0 sq =
  let c=Drule.first_concl Formula.is_disj (Logic.get_sqnt sq)
  in Logic.Rules.disjE c sq

let negC0 sq =
  let c=Drule.first_concl Formula.is_neg (Logic.get_sqnt sq)
  in Logic.Rules.negC c sq

let negA0 sq =
  let c=Drule.first_asm Formula.is_neg (Logic.get_sqnt sq)
  in Logic.Rules.negA c sq

let allI0 sq =
  let c=Drule.first_concl Formula.is_all (Logic.get_sqnt sq)
  in Logic.Rules.allI c sq

let existI0 sq =
  let c=Drule.first_asm Formula.is_exists (Logic.get_sqnt sq)
  in Logic.Rules.existI c sq


let mp_tac0 sq = 
  Drule.mp_basic_rule 
    (Drule.first_asm Formula.is_implies (Logic.get_sqnt sq)) sq

let mp_tac sq = rule_tac mp_tac0 sq


let trueR0 sq =
  let c=Drule.first_concl Formula.is_true (Logic.get_sqnt sq)
  in Logic.Rules.trueR c sq


let conjI = rule_tac  conjI0
let conjE = rule_tac conjE0

let disjI = rule_tac disjI0
let disjE = rule_tac disjE0

let negA = rule_tac  negA0
let negC = rule_tac  negC0

let implI = rule_tac  implI0
let implE = rule_tac  implE0

(*     let mp_tac = rule_tac Drule.mp_rule *)

let allI = rule_tac  allI0

(*
let allE str = 
  let t=Tpenv.read_unchecked str
  in rule_tac (Drule.allE t)
*)
let allE trm =  rule_tac (Drule.allE trm)

let existI = rule_tac existI0  

(*
let existE str= 
  let t= Tpenv.read_unchecked str
  in rule_tac (Drule.existE t)
*)
let existE trm= rule_tac (Drule.existE trm)


let beta i =  (Logic.Rules.beta i)

let delete i =  (Logic.Rules.delete i)

let beta_tac = rule_tac 
    (Drule.foreach_once Logic.Rules.beta)

let replace i j =
  (Logic.Rules.rewrite ~dir:true [i] j)

let replace_rl i j =
  (Logic.Rules.rewrite ~dir:false [i] j)


(* tacticals *)

type tactical =  tactic 

(* fail sq:
   do nothing and fail
 *)
let fail sq = raise (logicError "failed" [])

(*
 let repeat tac g = Logic.Rules.repeat tac g 
*)

let rec repeat tac g = 
  let ng = tac g
  in 
  try 
    if Logic.has_subgoals ng 
    then repeat tac ng
    else ng
  with _ -> ng


(* orl fs sq:
   apply first of fs to to sq, 
   if unsucesseful try next f
 *)

let rec orl fs g =
  match fs with
    [] -> fail g
  | r::rs -> 
      if Logic.has_subgoals g
      then
	(try r g 
	with  _ -> orl rs g)
      else g
  

(*
   let (++) tac1 tac2 =
   (fun g1 ->
   let g2 = sqnt_apply tac1 g1
   in sqnt_apply tac2 g2)
 *)

let (||) tac1 tac2 g=
  if Logic.has_subgoals g 
  then 
    (try tac1 g
    with  _ -> tac2 g)
  else g

let orelseF tac1 tac2 g =  (tac1 || tac2) g

let firstF ts g = 
  List.fold_right (fun t -> (orelseF t skip)) ts g

(* thenl rules sq:
   applies rules, in order, to each subgoal resulting from
   application of first rule to sq 
   fails if any rule fails ( sequential composition )
 *)

(*
   let thenl ts g = 
   Logic.Rules.thenl ts g
*)

let thenl rls sq =
  let rec thenl_aux fs sqs =
    match fs with 
      [] -> sqs 
    |	r::rs ->
        let nsq=(r sqs)
	in 
	if Logic.has_subgoals nsq
	then (thenl_aux rs nsq)
	else nsq
  in 
  thenl_aux rls sq 


let (++) tac1 tac2 g =
  thenl [tac1; tac2] g

(*
*)

(*
let apply_list ts g = Logic.Rules.apply_list ts g
*)

(* apply_list rules sq:
   applies rules, in order, to each subgoal resulting from
   application of first rule to sq 
   fails if no rule succeeds 
 *)


let apply_list fs g =
  let chng = ref false
  in 
  let rec appl_aux fs gs =
    match fs with 
      [] -> gs 
    | r::rs-> 
        (try 
          (let ng=(r gs) 
          in chng:=true; appl_aux rs ng)
        with _ -> appl_aux rs gs)
  in let ngs = appl_aux fs g
  in if !chng then ngs else (fail g)


