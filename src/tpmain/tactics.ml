
open Logic

type tactic = Logic.rule

let fnum = Drule.fnum
let ftag = Drule.ftag
let (!!) = fnum

let leftright=Rewrite.leftright
let rightleft = Rewrite.rightleft

let rule_tac r g =  r g

let rotateA ?info g = Logic.Rules.rotate_asms info g
let rotateC ?info g = Logic.Rules.rotate_cncls info g

let copy_asm ?info i g 
    = Logic.Rules.copy_asm info i g
let copy_concl ?info i g 
    = Logic.Rules.copy_cncl info i g

let lift ?info id g = Logic.Rules.lift info id g

let skip g= g

let find_basic sq = 
  let ams = Logic.asms sq
  and cncs = Logic.concls sq
  in 
  let rec find_basic_aux xs =
    match xs with
      [] -> raise Not_found
    | (t, c)::cs -> 
	try 
	  ((Drule.first 
	     (fun x-> Formula.alpha_convp 
		 (Logic.scope_of sq) x c) ams), 
	   ftag t)
	with Not_found -> find_basic_aux cs 
  in 
  find_basic_aux cncs

let basic ?info sqnt = 
  let sq=Logic.get_sqnt sqnt
  in 
  try
    let a,c = find_basic sq
    in Logic.Rules.assume info a c sqnt
  with Not_found -> raise (Result.error "Not basic")

let postpone = Logic.Rules.postpone

let cut ?info th = Logic.Rules.cut info th

let implI ?info ?c sq =
  let cf=
    match c with
      Some x -> x
    | _ -> (Drule.first_concl Formula.is_implies (Logic.get_sqnt sq))
  in 
  Logic.Rules.implI info cf sq

let implE ?info ?a sq =
  let af=
    match a with 
      (Some x) -> x
    | _ -> (Drule.first_asm Formula.is_implies (Logic.get_sqnt sq))
  in 
  Logic.Rules.implE info af sq

let conjI ?info ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_conj (Logic.get_sqnt sq))
  in Logic.Rules.conjI info cf sq

let conjE ?info ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_conj (Logic.get_sqnt sq))
  in Logic.Rules.conjE info af sq

let disjI ?info ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_disj (Logic.get_sqnt sq))
  in Logic.Rules.disjI info af sq

let disjE ?info ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_disj (Logic.get_sqnt sq))
  in Logic.Rules.disjE info cf sq

let negC ?info ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_neg (Logic.get_sqnt sq))
  in Logic.Rules.negC info cf sq

let negA ?info ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_neg (Logic.get_sqnt sq))
  in Logic.Rules.negA info af sq

let allI ?info ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_all (Logic.get_sqnt sq))
  in Logic.Rules.allI info cf sq

let existI ?info ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_exists (Logic.get_sqnt sq))
  in Logic.Rules.existI info af sq

let trueR ?info ?c sq =
  let cf=
    match c with 
      Some x -> x
    | _ -> (Drule.first_concl Formula.is_true (Logic.get_sqnt sq))
  in Logic.Rules.trueR info cf sq

let trivial = trueR

let allE ?info ?a trm sq =
  let af=
    match a with
      Some x -> x
    | _ ->  (Drule.first_asm Formula.is_all (Logic.get_sqnt sq))
  in Logic.Rules.allE info trm af sq

let existE ?info ?c trm sq =
  let cf=
    match c with
      (Some x) -> x
    | _ -> (Drule.first_concl Formula.is_exists (Logic.get_sqnt sq))
  in Logic.Rules.existE info trm cf sq

let deleten ?info ns sq = 
  let rec del_aux l s=
    match l with
      [] -> s
    | (x::xs) -> del_aux xs (Logic.Rules.delete info x s)
  in del_aux ns sq

let delete ?info i =  (Logic.Rules.delete info i)

let beta_tac ?info ?f= 
    match f with
    (Some x) -> Logic.Rules.beta info x
    | _ -> (Drule.foreach_once (fun x -> Logic.Rules.beta info x))



(*
   [unify_tac a c g]
   unify assumption [a] with conclusion [c]
*)
let unify_tac ?info ?(a=(fnum (-1))) ?(c=(fnum 1)) g=
  let sqnt=Logic.get_sqnt g
  in 
  let asm = 
    try 
      Formula.dest_form 
	(Logic.drop_tag (Logic.get_asm (Logic.label_to_index a sqnt) sqnt))
    with Not_found ->
      raise(Result.error "unify_tac: assumption not found")
  and concl = 
    try 
      Formula.dest_form
	(Logic.drop_tag (Logic.get_cncl (Logic.label_to_index c sqnt) sqnt))
    with Not_found ->
      raise(Result.error "unify_tac: conclusion not found")
  in 
  let asm_vars, asm_body = 
    Term.strip_qnt (Basic.All) asm
  and concl_vars, concl_body = 
    Term.strip_qnt (Basic.Ex) concl
  in 
  let asm_varp x = (Rewrite.is_free_binder asm_vars x) 
  and concl_varp x = (Rewrite.is_free_binder concl_vars x) 
  in 
  let varp x = (asm_varp x) or (concl_varp x)
  and scope = Logic.scope_of sqnt
  in 
  let env1 = 
    try  (* unify asm and concl *)
      Unify.unify scope varp asm concl
    with _ -> 
      try (* unify asm and concl_body with concl_vars *)
	Unify.unify scope concl_varp asm concl_body
      with _ -> 
	try (* unify asm_body and concl with asm_vars *)
	  Unify.unify scope asm_varp asm_body concl
	with _ -> 
	  try (* unify asm_body and concl_body with all vars *)
	    Unify.unify scope asm_varp asm_body concl_body
	  with _ -> 
	    raise (Result.error "unify_tac: can't unify formulas")
  in 
  let asm_consts = Drule.make_consts asm_vars env1
  and concl_consts = Drule.make_consts concl_vars env1
  in 
  let g1 = Drule.inst_list (Logic.Rules.allE info) asm_consts a g
  in 
  let g2 = Drule.inst_list (Logic.Rules.existE info) concl_consts c g
  in 
  try 
    Logic.Rules.assume info a c g2
  with _ -> g2


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



let rewrite_control 
    ?(max=None) ?(strat=Rewrite.topdown) dir=
  Rewrite.control ~max:max ~dir:dir ~strat:strat 

let gen_rewrite_tac ?info ctrl rules ?f goal=
  match f with
    None -> 
      Drule.foreach_once 
	(fun x -> 
	  Logic.Rules.rewrite_any 
	    info ~dir:(ctrl.Rewrite.rr_dir) rules x) goal
  | Some (x) ->
      Logic.Rules.rewrite_any 
	info ~dir:(ctrl.Rewrite.rr_dir) rules x goal
      

let rewrite_tac ?info ?(dir=leftright) ths ?f goal=
  let rules = (List.map (fun x -> Logic.RRThm x) ths) 
  in 
  match f with
    None -> 
      Drule.foreach_formula
	[(fun x -> true),
	 (fun x -> 
	   orl[Logic.Rules.rewrite_any info ~dir:dir rules x;
	       skip]) 
       ]
	goal
  | Some (x) ->
      Logic.Rules.rewrite_any info ~dir:dir rules x goal

let replace_tac ?info ?(dir=leftright) asms ?f goal =
  let rules = (List.map (fun x -> Logic.Asm x) asms) 
  in 
  match f with
    None -> 
      let sqnt = Drule.sequent goal
      in 
      let tags = 
	(List.map (fun i -> Logic.label_to_tag i sqnt) asms)
      in 
      Drule.foreach_once
	(fun x -> 
	   if (List.exists 
		 (fun y -> Tag.equal (Logic.label_to_tag x sqnt) y)
		 tags)
	   then fun g -> g
	   else 
	     orl [Logic.Rules.rewrite_any info ~dir:dir rules x; skip])
	goal
  | Some (x) ->
      Logic.Rules.rewrite_any info ~dir:dir rules x goal
