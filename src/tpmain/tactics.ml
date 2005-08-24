(*-----
 Name: tactics.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Logic

type tactic = Logic.tactic

(*** 
* Support functions
***)

let fnum = Drule.fnum
let ftag = Drule.ftag
let fname = Drule.fname

let (!!) = fnum
let (!~) x= fnum (-x)
let (!$) = fname

(*** 
* Basic tacticals and tactics
***)

let foreach = Logic.Subgoals.apply_to_each 

let skip = Logic.Tactics.skip None

let fail ?err sq = 
  match err with 
    None -> raise (Result.error "failed")
  | Some e -> raise e

let data_tac f info g= f info; skip g

(***
* Tacticals
***)

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

let (++) tac1 tac2 g = seq [tac1; tac2] g

let alt tacl g = 
  let rec alt_aux ts =
    match ts with
      [] -> raise (Result.error "alt: no successful tactic")
    | (x::xs) ->
	try x g
	with _ -> alt_aux xs
  in alt_aux tacl 

let (||) tac1 tac2 g=
  try tac1 g with  _ -> tac2 g

let thenl tac rls sq = Logic.Subgoals.zip rls (tac sq)

let (--) = thenl

let rec repeat tac g =
  (tac ++ ((repeat tac) || skip)) g

let cond pred ttac ftac g =
  if (pred g) then (ttac g) else (ftac g)

let (-->) pred tac = cond pred tac skip

let rec map_every tac l goal = 
  let rec every_aux ls g=
    match ls with 
      [] -> skip g
    | (x::xs) -> (tac x ++ every_aux xs) g
  in 
  every_aux l goal

let map_first tac l goal = 
  let rec every_aux ls g=
    match ls with 
      [] -> skip g
    | (x::xs) -> 
	try (tac x) g
	with _ -> every_aux xs g
  in 
  every_aux l goal
  

(***
* Tactics
***)

(*** Formula manipulation ***)

let rotateA ?info g = Logic.Tactics.rotate_asms info g
let rotateC ?info g = Logic.Tactics.rotate_cncls info g

let copyA ?info i g 
    = Logic.Tactics.copy_asm info i g
let copyC ?info i g 
    = Logic.Tactics.copy_cncl info i g

let lift ?info id g = Logic.Tactics.lift info id g

let delete ?info i = (Logic.Tactics.delete info i)

let deleten ns sq = 
  let rec del_aux l b=
    match l with
      [] -> b
    | (x::xs) -> 
	del_aux xs (foreach (Logic.Tactics.delete None x) b)
  in del_aux ns (skip sq)

(*** Logic Rules **)

let trueR ?info ?c sq =
  let cf=
    match c with 
      Some x -> x
    | _ -> (Drule.first_concl Formula.is_true (Drule.sequent sq))
  in Logic.Tactics.trueR info cf sq

let conjC ?info ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_conj (Drule.sequent sq))
  in Logic.Tactics.conjC info cf sq

let conjA ?info ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_conj (Drule.sequent sq))
  in Logic.Tactics.conjA info af sq

let disjC ?info ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_disj (Drule.sequent sq))
  in Logic.Tactics.disjC info cf sq

let disjA ?info ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_disj (Drule.sequent sq))
  in Logic.Tactics.disjA info af sq

let negC ?info ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_neg (Drule.sequent sq))
  in Logic.Tactics.negC info cf sq

let negA ?info ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_neg (Drule.sequent sq))
  in Logic.Tactics.negA info af sq

let implC ?info ?c sq =
  let cf=
    match c with
      Some x -> x
    | _ -> (Drule.first_concl Formula.is_implies (Drule.sequent sq))
  in 
  Logic.Tactics.implC info cf sq

let implA ?info ?a sq =
  let af=
    match a with 
      (Some x) -> x
    | _ -> (Drule.first_asm Formula.is_implies (Drule.sequent sq))
  in 
  Logic.Tactics.implA info af sq

let existC ?info ?c trm sq =
  let cf=
    match c with
      (Some x) -> x
    | _ -> (Drule.first_concl Formula.is_exists (Drule.sequent sq))
  in Logic.Tactics.existC info trm cf sq

let existA ?info ?a sq =
  let af=
    match a with 
      Some(x) -> x
    | _ -> (Drule.first_asm Formula.is_exists (Drule.sequent sq))
  in Logic.Tactics.existA info af sq

let allC ?info ?c sq =
  let cf=
    match c with 
      Some(x) -> x
    | _ -> (Drule.first_concl Formula.is_all (Drule.sequent sq))
  in Logic.Tactics.allC info cf sq

let allA ?info ?a trm sq =
  let af=
    match a with
      Some x -> x
    | _ ->  (Drule.first_asm Formula.is_all (Drule.sequent sq))
  in Logic.Tactics.allA info trm af sq

let instA0 ?info l trms goal =
  let info1 = Drule.mk_info ()
  and tag1 = Logic.label_to_tag l (Drule.sequent goal)
  in 
  let instf infof trm g = 
    let alabel = Lib.get_one (Drule.aformulas infof) (Failure "instA")
    in
    ignore(Drule.empty_info infof);
    allA ~info:infof ~a:(ftag alabel) trm g
  in 
  Logic.do_info (Some info1) [] [tag1]  [] [];
  let g1 = map_every (instf info1) trms goal
  in 
  Logic.add_info info [] (Drule.aformulas info1) [] [];
  g1

let instA ?info ?a trms goal = 
  let af =
    match a with 
      Some x -> x
    | _ ->  (Drule.first_asm Formula.is_all (Drule.sequent goal))
  in 
  instA0 ?info af trms goal

let instC0 ?info l trms goal =
  let info1 = Drule.mk_info ()
  and tag1 = Logic.label_to_tag l (Drule.sequent goal)
  in 
  let instf infof trm g = 
    let clabel = Lib.get_one (Drule.cformulas infof) (Failure "instA")
    in
    ignore(Drule.empty_info infof);
    existC ~info:infof ~c:(ftag clabel) trm g
  in 
  Logic.do_info (Some info1) [] [] [tag1] [];
  let g1 = map_every (instf info1) trms goal
  in 
  Logic.add_info info [] [] (Drule.cformulas info1) [];
  g1
  
let instC ?info ?c trms goal=
  let cf=
    match c with
      (Some x) -> x
    | _ -> (Drule.first_concl Formula.is_exists (Drule.sequent goal))
  in 
  instC0 ?info cf trms goal

let inst_tac ?info ?f trms goal = 
  try
    instA ?info:info ?a:f trms goal
  with _ -> instC ?info:info ?c:f trms goal

let cut ?info ?inst th goal = 
  let cut0 trms g = 
      let info1 = Drule.mk_info()
      in 
      let g1 = Logic.Tactics.cut (Some info1) th g
      in 
      let atag = Lib.get_one (Drule.aformulas info1) (Failure "cut")
      in 
      ignore(Drule.empty_info info1);
      foreach (instA ?info:info ~a:(ftag atag) trms) g1
  in 
  match inst with
    None -> Logic.Tactics.cut info th goal
  | Some(trms) -> 
      try cut0 trms goal
      with err -> 
	raise (Result.add_error (Result.error "cut") err)

let beta_tac ?info ?f= 
  match f with
    (Some x) -> Logic.Tactics.beta info x
  | _ -> (Drule.foreach_once (fun x -> Logic.Tactics.beta info x))

let name_tac ?info n lbl goal = 
  let sqnt = Drule.sequent goal
  in 
  match Lib.try_app (Logic.get_label_asm lbl) sqnt with
    Some _ -> Logic.Tactics.nameA info n lbl goal
  | None -> Logic.Tactics.nameC info n lbl goal


(*** Unification tactics ***)

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

let basic ?info sqnt = 
  let sq=Drule.sequent sqnt
  in 
  try
    let a,c = find_basic sq
    in Logic.Tactics.basic info a c sqnt
  with Not_found -> raise (Result.error "Not basic")

let unify_tac ?info ?(a=(fnum (-1))) ?(c=(fnum 1)) g=
  let sqnt=Drule.sequent g
  in 
  let asm = 
    try 
      Formula.term_of 
	(Logic.drop_tag (Logic.Sequent.get_asm 
			   (Logic.label_to_index a sqnt) sqnt))
    with Not_found ->
      raise(Result.error "unify_tac: assumption not found")
  and concl = 
    try 
      Formula.term_of
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
       (Drule.inst_list (Logic.Tactics.allA info) asm_consts a)
       (Drule.inst_list (Logic.Tactics.existC info) concl_consts c)) 
      g
  in 
  try 
    Logic.foreach (Logic.Tactics.basic info a c) g1
  with _ -> g1

(***
* Rewriting tactics
***)

let leftright=Rewrite.leftright
let rightleft = Rewrite.rightleft

let rewrite_control 
    ?max ?(strat=Rewrite.topdown) dir=
  Rewrite.control ~max:max ~dir:dir ~strat:strat 

let is_rewrite_formula t=
  let (_, t1) = Term.strip_qnt Basic.All t
  in 
  Logicterm.is_equality t1

let gen_rewrite_tac ?info ctrl rules ?f goal=
  match f with
    None -> 
      Drule.foreach_once 
	(fun x -> Logic.Tactics.rewrite info ~ctrl:ctrl rules x) goal
  | Some (x) ->
      Logic.Tactics.rewrite info ~ctrl:ctrl rules x goal
	
let rewrite_tac ?info ?(dir=leftright) ths ?f goal=
  let ctrl = rewrite_control dir
  in 
  let rules = (List.map (fun x -> Logic.RRThm x) ths) 
  in 
  gen_rewrite_tac ?info:info ctrl rules ?f:f goal 

let once_rewrite_tac ?info ?(dir=leftright) ths ?f goal=
  let ctrl=rewrite_control ~max:1 dir
  in 
  let rules = (List.map (fun x -> Logic.RRThm x) ths) 
  in 
  gen_rewrite_tac ?info:info ctrl rules ?f:f goal

let gen_replace_tac ?info ?(ctrl=Formula.default_rr_control) ?asms ?f goal =
  let sqnt = Drule.sequent goal
  in 
  let rec find_equality_asms sqasms rst=
    match sqasms with 
      [] -> List.rev rst
    | (l, af)::xs -> 
	(if (Drule.qnt_opt_of Basic.All 
	       (Logicterm.is_equality) (Formula.term_of af))
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
	    alt [Logic.Tactics.rewrite info ~ctrl:ctrl rules x; skip])
	goal
  | Some (x) ->
      Logic.Tactics.rewrite info ~ctrl:ctrl rules x goal

let replace_tac ?info ?(dir=leftright) ?asms ?f goal=
  let ctrl=rewrite_control dir
  in 
  gen_replace_tac ?info:info ~ctrl:ctrl ?asms:asms ?f:f goal

let once_replace_tac ?info ?(dir=leftright) ?asms ?f goal=
  let ctrl=rewrite_control ~max:1 dir
  in 
  gen_replace_tac ?info:info ~ctrl:ctrl ?asms:asms ?f:f goal


(***
* Derived tactics and tacticals
***)

(** 
   [named_tac tac anames cnames]: apply [tac ~info:inf goal], rename
   each of [Drule.aformulas inf] with a name from [anames], rename
   each of [Drule.cformulas inf] with a name from [cnames], in
   order. Set [info=inf'] where [inf'] is [inf], with the formula tag
   produced by renaming.
*) 
let named_tac ?info tac anames cnames (goal: Logic.node) =
    let inf1 = Drule.mk_info()
    and inf2 = Drule.mk_info()
    in 
    let rec name_list ns ls g = 
      match (ns, ls) with 
	([], _) -> g
      | (_, []) -> g
      | (x::xs, y::ys) -> 
	 (name_list xs ys) (foreach (name_tac ~info:inf2 x y) g)
    in 
    let g1 = tac ~info:inf1 goal
    in 
    let albls = List.map ftag (Drule.aformulas inf1)
    and clbls = List.map ftag (Drule.cformulas inf1)
    in 
    let g2 = name_list anames albls g1
    in 
    let g3 = name_list cnames clbls g2
    in 
    add_info info 
      (Drule.subgoals inf1) 
      (List.rev (Drule.aformulas inf2)) 
      (List.rev (Drule.cformulas inf2))
      (Drule.constants inf1);
    g3


(*** Pattern matching tacticals ***)

let match_asm trm tac g =
  try 
    tac (Drule.match_asm (Drule.typenv_of g) trm (Drule.sequent g)) g
  with Not_found -> raise (Term.term_error "No matching assumption" [trm])

let match_concl trm tac g =
  try 
    tac (Drule.match_concl (Drule.typenv_of g) trm (Drule.sequent g)) g
  with Not_found -> raise (Term.term_error "No matching conclusion" [trm])

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
      raise (Term.term_error "No matching formula in sequent" [trm])

(* Tacticals for dealing with information returned by tactics *)

(** [itactic]
   Information passing tactics.
 *)
(*
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
*)
