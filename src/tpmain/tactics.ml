1
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

let skip = Drule.skip
let foreach = Drule.foreach


(** [add_info_tac f info g]

   Tactic to add information to [info].

   Apply [f info] then [skip].
*)
let add_info_tac f info g= f info; skip g


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
  let sq=Drule.sequent sqnt
  in 
  try
    let a,c = find_basic sq
    in Logic.Rules.basic None a c sqnt
  with Not_found -> raise (Result.error "Not basic")

let postpone = Logic.postpone

let cut th = Logic.Rules.cut None th

let implC ?c sq =
  let cf=
    match c with
      Some x -> x
    | _ -> (Drule.first_concl Formula.is_implies (Drule.sequent sq))
  in 
  Logic.Rules.implC None cf sq

let implA ?a sq =
  let af=
    match a with 
      (Some x) -> x
    | _ -> (Drule.first_asm Formula.is_implies (Drule.sequent sq))
  in 
  Logic.Rules.implA None af sq

let conjC ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_conj (Drule.sequent sq))
  in Logic.Rules.conjC None cf sq

let conjA ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_conj (Drule.sequent sq))
  in Logic.Rules.conjA None af sq

let disjA ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_disj (Drule.sequent sq))
  in Logic.Rules.disjA None af sq

let disjC ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_disj (Drule.sequent sq))
  in Logic.Rules.disjC None cf sq

let negC ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_neg (Drule.sequent sq))
  in Logic.Rules.negC None cf sq

let negA ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_neg (Drule.sequent sq))
  in Logic.Rules.negA None af sq

let allC ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_all (Drule.sequent sq))
  in Logic.Rules.allC None cf sq

let existA ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_exists (Drule.sequent sq))
  in Logic.Rules.existA None af sq

let trueR ?c sq =
  let cf=
    match c with 
      Some x -> x
    | _ -> (Drule.first_concl Formula.is_true (Drule.sequent sq))
  in Logic.Rules.trueR None cf sq

let trivial = trueR

let allA ?a trm sq =
  let af=
    match a with
      Some x -> x
    | _ ->  (Drule.first_asm Formula.is_all (Drule.sequent sq))
  in Logic.Rules.allA None trm af sq

let existC ?c trm sq =
  let cf=
    match c with
      (Some x) -> x
    | _ -> (Drule.first_concl Formula.is_exists (Drule.sequent sq))
  in Logic.Rules.existC None trm cf sq

let deleten ns sq = 
  let rec del_aux l b=
    match l with
      [] -> b
    | (x::xs) -> 
	del_aux xs (foreach (Logic.Rules.delete None x) b)
  in del_aux ns (skip sq)

let delete i = (Logic.Rules.delete None i)

let beta_tac ?f= 
  match f with
    (Some x) -> Logic.Rules.beta None x
  | _ -> (Drule.foreach_once (fun x -> Logic.Rules.beta None x))


(*
   [unify_tac a c g]
   unify assumption [a] with conclusion [c]
 *)
let unify_tac ?info ?(a=(fnum (-1))) ?(c=(fnum 1)) g=
  let sqnt=Drule.sequent g
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
  let g1=
    (Drule.seq 
       (Drule.inst_list (Logic.Rules.allA info) asm_consts a)
       (Drule.inst_list (Logic.Rules.existC info) concl_consts c)) 
      g
  in 
  try 
    Logic.foreach (Logic.Rules.basic info a c) g1
  with _ -> g1


(* tacticals *)

type tactical =  tactic 

(* fail sq:
   do nothing and fail
*)
let fail ?err sq = 
  match err with 
    None -> raise (Result.error "failed")
  | Some e -> raise e


(*
let rec repeat tac g =
  let rec app_aux ng=
    if(Drule.has_subgoals ng)
    then 
      try app_aux (Logic.foreach tac ng)
      with _ -> ng
    else ng
  in 
  app_aux (tac g)
*)
(*
   [orl tacl g]
   apply first tactic of [tacl] which succeeds.
   raise error if none succeeds.
 *)
let orl tacl g = 
  let rec orl_aux ts =
    match ts with
      [] -> raise (Result.error "orl: no successful tactic")
    | (x::xs) ->
	try x g
	with _ -> orl_aux xs
  in orl_aux tacl 

let (||) tac1 tac2 g=
  (try tac1 g
  with  _ -> tac2 g)


(* thenl tac rules sq:
   applies rules, in order, to each subgoal resulting from
   application of tac to sq 
   fails if any rule fails ( sequential composition )
 *)
let thenl tac rls sq = Logic.Subgoals.zip rls (tac sq)
let (--) = thenl

(* seq rules sq:
   applies rules, in order, to each subgoal resulting from
   application of first rule to sq 
   fails if any rule fails ( sequential composition )
 *)
let seq rls sq =
  let rec seq_aux fs sqs =
    match fs with 
      [] -> sqs 
    | r::rs ->
	if Drule.has_subgoals sqs
	then (seq_aux rs (foreach r sqs))
	else sqs
  in 
  match rls with
    [] -> raise (Result.error "seq: empty tactic list")
  | x::xs -> seq_aux xs (x sq)

let (++) tac1 tac2 g =
  seq [tac1; tac2] g

let rec repeat tac g =
  (tac ++ ((repeat tac) || skip)) g

(* apply_list rules sq:
   applies rules, in order, to each subgoal resulting from
   application of first rule to sq 
   fails if no rule succeeds 
 *)
(*
let apply_list fs g =
  let chng = ref false
  in 
  let rec appl_aux fs gs =
    match fs with 
      [] -> gs 
    | r::rs-> 
        (try 
          (let ng=foreach r gs
          in chng:=true; appl_aux rs ng)
        with _ -> appl_aux rs gs)
  in 
  let ngs = appl_aux fs (skip g)
  in if !chng then ngs else (fail g)
*)

(** 
   [apply_if pred tac]
   Apply tactic [tac] if predicate [pred] is true for node.
   Do nothing if [pred] is false (using [skip]).
 *)
let apply_if pred tac n =
  if (pred n) then tac n
  else skip n

(** 
   [pred --> tac]
   Apply tactic [tac] if predicate [pred] is true for node.
   Do nothing if [pred] is false (using [skip]).
   (synonym for apply_if)
 *)
let (-->) = apply_if


let rewrite_control 
    ?(max=None) ?(strat=Rewrite.topdown) dir=
  Rewrite.control ~max:max ~dir:dir ~strat:strat 

let gen_rewrite_tac ?info ctrl rules ?f goal=
  match f with
    None -> 
      Drule.foreach_once 
	(fun x -> 
	  Logic.Rules.rewrite 
	    info ~ctrl:ctrl rules x) goal
  | Some (x) ->
      Logic.Rules.rewrite
	info ~ctrl:ctrl rules x goal
	

let rewrite_tac ?(ctrl=Formula.default_rr_control) ths ?f goal=
  let rules = (List.map (fun x -> Logic.RRThm x) ths) 
  in 
  match f with
    None -> 
      Drule.foreach_once
	(fun l -> Logic.Rules.rewrite None ~ctrl:ctrl rules l)
	goal
  | Some (x) ->
      Logic.Rules.rewrite None ~ctrl:ctrl rules x goal

let once_rewrite_tac ths ?f goal=
  let ctrl=
    {Formula.default_rr_control with Rewrite.depth=Some 1}
  in 
  rewrite_tac ~ctrl ths ?f:f goal

let is_rewrite_formula t=
  let (_, t1) = Term.strip_qnt Basic.All t
  in 
  (Logicterm.is_equality t1)
    
let replace_tac ?(ctrl=Formula.default_rr_control) ?asms ?f goal =
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
	  then fun g -> (skip g)
	  else 
	    orl [Logic.Rules.rewrite None ~ctrl:ctrl rules x; skip])
	goal
  | Some (x) ->
      Logic.Rules.rewrite None ~ctrl:ctrl rules x goal


let once_replace_tac ?asms ?f goal=
  let ctrl=
    {Formula.default_rr_control with Rewrite.depth=Some 1}
  in 
  replace_tac ~ctrl ?asms:asms ?f:f goal

(* pattern matching tacticals *)

let match_asm trm tac g =
  try 
    tac (Drule.match_asm (Drule.typenv_of g) trm (Drule.sequent g)) g
  with Not_found -> raise (Term.termError "No matching assumption" [trm])

let match_concl trm tac g =
  try 
    tac (Drule.match_concl (Drule.typenv_of g) trm (Drule.sequent g)) g
  with Not_found -> raise (Term.termError "No matching conclusion" [trm])

let match_formula trm tac g=
  let sqnt=Drule.sequent g
  and tyenv=Drule.typenv_of g
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
  let g1=Logic.first_only (tac info (goals, forms, vars)) g
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
	if(Drule.has_subgoals g)
	then 
	  let (nr, g1) = itac t (gs, fs, vs) g
	  in 
	  seq_aux ts nr g1
	else g
  in 
  seq_aux tacs ([], [], []) (skip goal)

let ialt ?(initial=([], [], [])) (tacs: itactic list) goal=
  let rec alt_aux rls (gs, fs, vs) g = 
    match rls with 
      [] -> fail g
    | (t::ts) ->
	if (Drule.has_subgoals g)
	then 
	  try 
	    (let (nr, g1) = itac t (gs, fs, vs) g
	    in g1)
	  with _ -> alt_aux ts (gs, fs, vs) g
	else g
  in 
  alt_aux tacs ([], [], []) (skip goal)
