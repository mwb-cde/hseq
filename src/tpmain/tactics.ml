
open Logic

type tactic = Logic.rule

let fnum = Drule.fnum
let ftag = Drule.ftag

let (!~) x= fnum (-x)
let (!+) = fnum
let (!!) = fnum

let leftright=Rewrite.leftright
let rightleft = Rewrite.rightleft

let rule_tac r g =  r g

let rotateA g = Logic.Rules.rotate_asms None g
let rotateC g = Logic.Rules.rotate_cncls None g

let copy_asm i g 
    = Logic.Rules.copy_asm None i g
let copy_concl i g 
    = Logic.Rules.copy_cncl None i g

let lift id g = Logic.Rules.lift None id g

let skip g= g

let find_basic sq = 
  let ams = Logic.Sequent.asms sq
  and cncs = Logic.Sequent.concls sq
  in 
  let rec find_basic_aux xs =
    match xs with
      [] -> raise Not_found
    | (t, c)::cs -> 
	try 
	  ((Drule.first 
	     (fun x-> Formula.alpha_equals
		 (Logic.Sequent.scope_of sq) x c) ams), 
	   ftag t)
	with Not_found -> find_basic_aux cs 
  in 
  find_basic_aux cncs

let basic sqnt = 
  let sq=Logic.get_sqnt sqnt
  in 
  try
    let a,c = find_basic sq
    in Logic.Rules.basic None a c sqnt
  with Not_found -> raise (Result.error "Not basic")

let postpone = Logic.Rules.postpone

let cut th = Logic.Rules.cut None th

let implI ?c sq =
  let cf=
    match c with
      Some x -> x
    | _ -> (Drule.first_concl Formula.is_implies (Logic.get_sqnt sq))
  in 
  Logic.Rules.implI None cf sq

let implE ?a sq =
  let af=
    match a with 
      (Some x) -> x
    | _ -> (Drule.first_asm Formula.is_implies (Logic.get_sqnt sq))
  in 
  Logic.Rules.implE None af sq

let conjI ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_conj (Logic.get_sqnt sq))
  in Logic.Rules.conjI None cf sq

let conjE ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_conj (Logic.get_sqnt sq))
  in Logic.Rules.conjE None af sq

let disjI ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_disj (Logic.get_sqnt sq))
  in Logic.Rules.disjI None af sq

let disjE ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_disj (Logic.get_sqnt sq))
  in Logic.Rules.disjE None cf sq

let negC ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_neg (Logic.get_sqnt sq))
  in Logic.Rules.negC None cf sq

let negA ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_neg (Logic.get_sqnt sq))
  in Logic.Rules.negA None af sq

let allI ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_all (Logic.get_sqnt sq))
  in Logic.Rules.allI None cf sq

let existI ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_exists (Logic.get_sqnt sq))
  in Logic.Rules.existI None af sq

let trueR ?c sq =
  let cf=
    match c with 
      Some x -> x
    | _ -> (Drule.first_concl Formula.is_true (Logic.get_sqnt sq))
  in Logic.Rules.trueR None cf sq

let trivial = trueR

let allE ?a trm sq =
  let af=
    match a with
      Some x -> x
    | _ ->  (Drule.first_asm Formula.is_all (Logic.get_sqnt sq))
  in Logic.Rules.allE None trm af sq

let existE ?c trm sq =
  let cf=
    match c with
      (Some x) -> x
    | _ -> (Drule.first_concl Formula.is_exists (Logic.get_sqnt sq))
  in Logic.Rules.existE None trm cf sq

let deleten ns sq = 
  let rec del_aux l s=
    match l with
      [] -> s
    | (x::xs) -> del_aux xs (Logic.Rules.delete None x s)
  in del_aux ns sq

let delete i =  (Logic.Rules.delete None i)

let beta_tac ?f= 
    match f with
    (Some x) -> Logic.Rules.beta None x
    | _ -> (Drule.foreach_once (fun x -> Logic.Rules.beta None x))



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
	(Logic.drop_tag (Logic.Sequent.get_asm 
			   (Logic.label_to_index a sqnt) sqnt))
    with Not_found ->
      raise(Result.error "unify_tac: assumption not found")
  and concl = 
    try 
      Formula.dest_form
	(Logic.drop_tag (Logic.Sequent.get_cncl 
			   (Logic.label_to_index c sqnt) sqnt))
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
  and scope = Logic.Sequent.scope_of sqnt
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
  let g1 = 
    match asm_vars with
      [] -> g
    | _ -> Drule.inst_list (Logic.Rules.allE info) asm_consts a g
  in 
  let g2 = 
    match concl_vars with
      [] -> g1
    | _ -> Drule.inst_list (Logic.Rules.existE info) concl_consts c g1
  in 
  try 
    Logic.Rules.basic info a c g2
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
*)

(* seq rules sq:
   applies rules, in order, to each subgoal resulting from
   application of first rule to sq 
   fails if any rule fails ( sequential composition )
 *)
let seq rls sq =
  let rec seq_aux fs sqs =
    match fs with 
      [] -> sqs 
    |	r::rs ->
        let nsq=(r sqs)
	in 
	if Logic.has_subgoals nsq
	then (seq_aux rs nsq)
	else nsq
  in 
  seq_aux rls sq 


let (++) tac1 tac2 g =
  seq [tac1; tac2] g


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
	  Logic.Rules.rewrite 
	    info ~dir:(ctrl.Rewrite.rr_dir) rules x) goal
  | Some (x) ->
      Logic.Rules.rewrite
	info ~dir:(ctrl.Rewrite.rr_dir) rules x goal
      

let rewrite_tac ?(dir=leftright) ths ?f goal=
  let rules = (List.map (fun x -> Logic.RRThm x) ths) 
  in 
  match f with
    None -> 
      Drule.foreach_once
	(fun l -> Logic.Rules.rewrite None ~dir:dir rules l)
	goal
  | Some (x) ->
      Logic.Rules.rewrite None ~dir:dir rules x goal


let is_rewrite_formula t=
  let (_, t1) = Term.strip_qnt Basic.All t
  in 
  (Logicterm.is_equality t1)
  

let replace_tac ?(dir=leftright) ?asms ?f goal =
  let sqnt = Drule.sequent goal
  in 
  let rec find_equality_asms sqasms rst=
    match sqasms with 
      [] -> List.rev rst
    | (l, af)::xs -> 
	(if (Drule.qnt_opt_of Basic.All 
	       (Logicterm.is_equality) (Formula.dest_form af))
	then find_equality_asms xs (l::rst)
	else find_equality_asms xs rst)
  in 
  let asm_tags=
    match asms with
      None -> find_equality_asms (Logic.Sequent.asms sqnt) []
    | Some xs -> (List.map (fun i -> Logic.label_to_tag i sqnt) xs)
  in 
  let rules = (List.map (fun x -> Logic.Asm (ftag x)) asm_tags) 
  in 
  match f with
    None -> 
      Drule.foreach_once
	(fun x -> 
	   if (List.exists 
		 (fun y -> Tag.equal (Logic.label_to_tag x sqnt) y)
		 asm_tags)
	   then fun g -> g
	   else 
	     orl [Logic.Rules.rewrite None ~dir:dir rules x; skip])
	goal
  | Some (x) ->
      Logic.Rules.rewrite None ~dir:dir rules x goal



(* pattern matching tacticals *)

let match_asm trm tac g =
  try 
    tac (Drule.match_asm (Logic.goal_tyenv g) trm (Drule.sequent g)) g
  with Not_found -> raise (Term.termError "No matching assumption" [trm])

let match_concl trm tac g =
  try 
    tac (Drule.match_concl (Logic.goal_tyenv g) trm (Drule.sequent g)) g
  with Not_found -> raise (Term.termError "No matching conclusion" [trm])

let match_formula trm tac g=
  let sqnt=Drule.sequent g
  and tyenv=Logic.goal_tyenv g
  in 
  try
    tac (Drule.match_asm tyenv trm sqnt) g
  with Not_found -> 
    try 
      tac (Drule.match_concl tyenv trm sqnt) g
    with Not_found ->
      raise (Term.termError "No matching formula in sequent" [trm])

(* Tacticals for dealing with information returned by tactics *)

(** [itactic]
   Information passing tactics.
*)
type itactic = 
    Logic.info
      -> (Tag.t list * Tag.t list * Basic.term list)
	-> tactic

let itac (tac:itactic) (goals, forms, vars) g= 
  let info=Drule.mk_info()
  in 
  let g1=tac info (goals, forms, vars) g
  in 
  let ngs, nfs, nvars = 
    ((!info).Logic.goals, (!info).Logic.forms, (!info).Logic.terms)
  in 
  ((ngs, nfs, nvars), g1)

let iseq ?(initial=([], [], [])) (tacs: itactic list) goal=
  let rec seq_aux rls (gs, fs, vs) g = 
    match rls with 
      [] -> g
    | (t::ts) ->
	if(Logic.has_subgoals g)
	then 
	  let (nr, g1) = itac t (gs, fs, vs) g
	  in 
	  seq_aux ts nr g1
	else g
  in 
  seq_aux tacs ([], [], []) goal

let ialt ?(initial=([], [], [])) (tacs: itactic list) goal=
  let rec alt_aux rls (gs, fs, vs) g = 
    match rls with 
      [] -> fail g
    | (t::ts) ->
	if (Logic.has_subgoals g)
	then 
	  try 
	    (let (nr, g1) = itac t (gs, fs, vs) g
	    in g1)
	  with _ -> alt_aux ts (gs, fs, vs) g
	else g
  in 
  alt_aux tacs ([], [], []) goal
