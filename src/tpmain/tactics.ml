(*-----
   Name: tactics.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

open Lib.Ops
open Logic

type tactic = Logic.tactic

(*** 
 * Support functions
 ***)

(*** Error reporting ***)

let error s = Result.error s
let add_error s err = Result.add_error (error s) err

(*** Accessing elements of a list ***)

let get_one ?(msg="Tactics.get_one failed") l =
  Lib.get_one l (Failure msg)

let get_two ?(msg="Tactics.get_two failed") l =
  Lib.get_two l (Failure msg)

(*** Formulas ***)

let drop_tag = Logic.drop_tag
let drop_formula = Logic.form_tag

(*** Labels ***)

let fnum n = Logic.FNum n
let ftag t = Logic.FTag t
let fname s = Logic.FName s

let (!!) = fnum
let (!~) x= fnum (-x)
let (!$) = fname

(*** Sequents ***)

let asms_of = Logic.Sequent.asms
let concls_of = Logic.Sequent.concls
let sqnt_tag  = Logic.Sequent.sqnt_tag

(*** Nodes ***)

let sequent g = Logic.Subgoals.node_sqnt g
let scope_of g = Logic.Sequent.scope_of (sequent g)
let typenv_of n = Logic.Subgoals.node_tyenv n
let node_tag n = Logic.Sequent.sqnt_tag (sequent n)

let get_tagged_asm i g= Logic.get_label_asm i (sequent g)
let get_tagged_concl i g= Logic.get_label_cncl i (sequent g)

let get_asm i g= Logic.drop_tag (get_tagged_asm i g)
let get_concl i g= Logic.drop_tag (get_tagged_concl i g)
let get_form i g= 
  try get_concl i g
  with Not_found -> get_asm i g

(*** Branches ***)

let branch_tyenv b = Logic.Subgoals.branch_tyenv b
let branch_subgoals b = Logic.Subgoals.branch_sqnts b
let has_subgoals b=
  match (branch_subgoals b) with
    [] -> false
  | _ -> true
let num_subgoals b = List.length (branch_subgoals b) 

(*** Information records ***)

let mk_info () = ref (Logic.make_tag_record [] [] [] [])
let empty_info info = info:=(Logic.make_tag_record[] [] [] [])

let subgoals inf= (!inf).Logic.goals
let aformulas inf = (!inf).Logic.aforms
let cformulas inf = (!inf).Logic.cforms
let constants inf = (!inf).Logic.terms
let set_info dst (sgs, afs, cfs, cnsts) = 
  Logic.add_info dst sgs afs cfs cnsts
    
(*** Utility functions ***)

let extract_consts vars env=
  let rec extract_aux qs cnsts=
    match qs with 
      [] -> cnsts
    | (x::xs) -> 
	try 
	  let nv = Term.find (Basic.Bound x) env
	  in 
	  if(Term.is_closed [] nv)
	  then extract_aux xs (nv::cnsts)
	  else cnsts
	with 
	  Not_found -> cnsts
  in 
  List.rev (extract_aux vars [])

let qnt_opt_of kind pred trm=
  let (_, body) = Term.strip_qnt kind trm
  in pred body

let first_asm p sq =
  Lib.first p (asms_of sq)

let first_concl p sq =
  Lib.first p (concls_of sq)

let first_form p sq = 
  try first_asm p sq 
  with Not_found -> first_concl p sq

let first_asm_label a pred sq =
  match a with
    Some x -> x
  | _ -> 
      let tag = 
	drop_formula(first_asm (fun f -> pred (drop_tag f)) (sequent sq))
      in 
      ftag tag

let first_concl_label c pred sq =
  match c with
    Some x -> x
  | _ -> 
      let tag = 
	drop_formula (first_concl (fun f -> pred (drop_tag f)) (sequent sq))
      in 
      ftag tag


(*** 
 * Basic tacticals and tactics
 ***)

let foreach = Logic.Subgoals.apply_to_each 

let skip = Logic.Tactics.skip ?info:None

let fail ?err sq = 
  match err with 
    None -> raise (error "failed")
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
	if has_subgoals sqs
	then (seq_aux rs (foreach r sqs))
	else sqs
  in 
  match rls with
    [] -> raise (error "seq: empty tactic list")
  | x::xs -> seq_aux xs (x sq)

let (++) tac1 tac2 g = seq [tac1; tac2] g

let alt tacl g = 
  let rec alt_aux ts =
    match ts with
      [] -> raise (error "alt: empty tactic list")
    | x::[] -> x g
    | (x::xs) ->
	try x g
	with _ -> alt_aux xs
  in alt_aux tacl 

let (//) tac1 tac2 g=
  try tac1 g with  _ -> tac2 g

let thenl tac rls sq = Logic.Subgoals.zip rls (tac sq)

let (--) = thenl

let rec repeat tac g =
  (tac ++ ((repeat tac) // skip)) g

let cond pred ttac ftac g =
  if (pred g) then (ttac g) else (ftac g)

let (-->) pred tac = cond pred tac skip

let restrict p tac goal =
  let ng = tac goal
  in 
  if (p ng) 
  then ng 
  else raise (Failure "restrict_tac")

let notify_tac f d tac goal =
  let ng = tac goal
  in 
  f d; ng

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
      [] -> fail ~err:(error "map_first: no tactic succeeded.") g
    | (x::xs) -> 
	try (tac x) g
	with _ -> every_aux xs g
  in 
  every_aux l goal
    
let map_some tac l goal =
  let nofail_tac l = (tac l // skip)
  in 
  let rec some_aux ls g =
    match ls with 
      [] -> fail ~err:(error "map_some: no tactic succeeded.") g
    | (x::xs) ->
	try (tac x ++ map_every nofail_tac xs) g
	with _ -> some_aux xs g
  in 
  some_aux l goal

let seq_some tacs goal =
  let nofail_tac tac g = (tac // skip) g
  in 
  let rec some_aux ls g =
    match ls with 
      [] -> 
	fail ~err:(Result.error "seq_some: no tactic succeeded.") g
    | (x::xs) ->
	try (x ++ map_every nofail_tac xs) g
	with _ -> some_aux xs g
  in 
  some_aux tacs goal

let foreach_asm tac goal =
  let label_tac tf = tac (ftag (drop_formula tf))
  in 
  try 
    map_some label_tac (asms_of (sequent goal)) goal
  with 
    err -> 
      raise (add_error "foreach_asm: no change." err)

let foreach_concl tac goal =
  let label_tac tf = tac (ftag (drop_formula tf))
  in 
  try 
    map_some label_tac (concls_of (sequent goal)) goal
  with 
    err -> 
      raise (add_error "foreach_concl: no change." err)

let foreach_form tac goal = 
  let chng = ref false
  in 
  let notify () = chng:=true
  in 
  let asms_tac g = 
    (((foreach_asm tac) ++ data_tac notify ()) // skip) g 
  and concls_tac g = 
    (((foreach_concl tac) ++ data_tac notify ()) // skip) g 
  in 
  try 
    restrict (fun _ -> !chng) (asms_tac ++ concls_tac) goal
  with 
    Failure _ -> raise (Failure "foreach_form")
  | err -> raise err

(***
 * Tactics
 ***)

(*** Formula manipulation ***)

let rotateA ?info g = Logic.Tactics.rotate_asms ?info g
let rotateC ?info g = Logic.Tactics.rotate_cncls ?info g

let copyA ?info i g 
    = Logic.Tactics.copy_asm ?info i g
let copyC ?info i g 
    = Logic.Tactics.copy_cncl ?info i g

let lift ?info id g = Logic.Tactics.lift ?info id g

let deleteA ?info i = Logic.Tactics.deleteA ?info i
let deleteC ?info i = Logic.Tactics.deleteC ?info i
let delete ?info i g = 
  try deleteA ?info i g
  with Not_found -> deleteC ?info i g

let deleten ns sq = 
  let rec del_aux l b=
    match l with
      [] -> b
    | (x::xs) -> 
	del_aux xs (foreach (delete x) b)
  in del_aux ns (skip sq)

(*** Logic Rules **)

let trueC ?info ?c sq =
  let cf = first_concl_label c Formula.is_true sq
  in Logic.Tactics.trueC ?info cf sq

let conjC ?info ?c sq =
  let cf = first_concl_label c Formula.is_conj sq
  in Logic.Tactics.conjC ?info cf sq

let conjA ?info ?a sq =
  let af = first_asm_label a Formula.is_conj sq
  in Logic.Tactics.conjA ?info af sq

let disjC ?info ?c sq =
  let cf= first_concl_label c Formula.is_disj sq
  in Logic.Tactics.disjC ?info cf sq

let disjA ?info ?a sq =
  let af= first_asm_label a Formula.is_disj sq
  in Logic.Tactics.disjA ?info af sq

let negC ?info ?c sq =
  let cf= first_concl_label c Formula.is_neg sq
  in Logic.Tactics.negC ?info cf sq

let negA ?info ?a sq =
  let af= first_asm_label a Formula.is_neg sq
  in Logic.Tactics.negA ?info af sq

let implC ?info ?c sq =
  let cf= first_concl_label c Formula.is_implies sq
  in
  Logic.Tactics.implC ?info cf sq

let implA ?info ?a sq =
  let af= first_asm_label a Formula.is_implies sq
  in 
  Logic.Tactics.implA ?info af sq

let existC ?info ?c trm sq =
  let cf= first_concl_label c Formula.is_exists sq
  in Logic.Tactics.existC ?info trm cf sq

let existA ?info ?a sq =
  let af= first_asm_label a Formula.is_exists sq
  in Logic.Tactics.existA ?info af sq

let allC ?info ?c sq =
  let cf= first_concl_label c Formula.is_all sq
  in Logic.Tactics.allC ?info cf sq

let allA ?info ?a trm sq =
  let af= first_asm_label a Formula.is_all sq
  in Logic.Tactics.allA ?info trm af sq

let instA0 ?info l trms goal =
  let info1 = mk_info ()
  and tag1 = Logic.label_to_tag l (sequent goal)
  in 
  let instf infof trm g = 
    let alabel = get_one ~msg:"instA" (aformulas infof)
    in
    empty_info infof;
    allA ~info:infof ~a:(ftag alabel) trm g
  in 
  Logic.do_info (Some info1) [] [tag1]  [] [];
  let g1 = map_every (instf info1) trms goal
  in 
  Logic.add_info info [] (aformulas info1) [] [];
  g1

let instA ?info ?a trms goal = 
  let af = first_asm_label a Formula.is_all goal
  in 
  instA0 ?info af trms goal

let instC0 ?info l trms goal =
  let info1 = mk_info ()
  and tag1 = Logic.label_to_tag l (sequent goal)
  in 
  let instf infof trm g = 
    let clabel = get_one ~msg:"instC" (cformulas infof)
    in
    empty_info infof;
    existC ~info:infof ~c:(ftag clabel) trm g
  in 
  Logic.do_info (Some info1) [] [] [tag1] [];
  let g1 = map_every (instf info1) trms goal
  in 
  Logic.add_info info [] [] (cformulas info1) [];
  g1
    
let instC ?info ?c trms goal=
  let cf= first_concl_label c Formula.is_exists goal
  in 
  instC0 ?info cf trms goal

let inst_tac ?info ?f trms goal = 
  try
    instA ?info:info ?a:f trms goal
  with _ -> instC ?info:info ?c:f trms goal

let cut ?info ?inst th goal = 
  let cut0 trms g = 
    let info1 = mk_info()
    in 
    let g1 = Logic.Tactics.cut ~info:info1 th g
    in 
    let atag = get_one ~msg:"cut" (aformulas info1)
    in 
    empty_info info1;
    foreach (instA ?info:info ~a:(ftag atag) trms) g1
  in 
  match inst with
    None -> Logic.Tactics.cut ?info th goal
  | Some(trms) -> 
      try cut0 trms goal
      with err -> 
	raise (add_error "cut" err)


let betaA ?info lbl goal =
  let scp = scope_of goal
  and (ft, form) = get_tagged_asm lbl goal
  in 
  let thm = Logic.Conv.beta_conv scp (Formula.term_of form)
  in 
  let info1 = mk_info()
  in 
  seq 
    [
     Logic.Tactics.cut ~info:info1 thm;
     (fun g1 ->
       let atag = 
	 Lib.get_one (aformulas info1) (error "Tactics.betaA")
       in 
       let albl = ftag atag
       in
       seq
	 [
	  Logic.Tactics.substA ?info:info [albl] lbl;
	  Logic.Tactics.deleteA albl
	] g1)
   ] goal

let betaA_tac ?info ?a goal =
  match a with
    Some(x) -> betaA ?info x goal
  | None -> 
      foreach_asm (betaA ?info) goal

let betaC ?info lbl goal =
  let scp = scope_of goal
  and (ft, form) = get_tagged_concl lbl goal
  in 
  let thm = Logic.Conv.beta_conv scp (Formula.term_of form)
  in 
  let info1 = mk_info()
  in 
  seq 
    [
     Logic.Tactics.cut ~info:info1 thm;
     (fun g1 ->
       let atag = 
	 Lib.get_one (aformulas info1) (error "Tactics.betaC")
       in 
       let albl = ftag atag
       in
       seq
	 [
	  Logic.Tactics.substC ?info:info [albl] lbl;
	  Logic.Tactics.deleteA albl
	] g1)
   ] goal

let betaC_tac ?info ?c goal =
  match c with
    Some(x) -> betaC ?info x goal
  | None -> 
      foreach_concl (betaC ?info) goal

(*
let beta_tac ?info ?f g= 
  match f with
    (Some x) -> Logic.Tactics.beta ?info x g
  | _ -> 
      try foreach_form (Logic.Tactics.beta ?info) g
      with err -> 
	raise (add_error "beta_tac: failed." err)
*)
let beta_tac ?info ?f goal = 
  try 
    seq_some
      [ 
	betaC_tac ?info ?c:f;
	betaA_tac ?info ?a:f
      ] goal
  with err -> raise (add_error "beta_tac" err)

let name_tac ?info n lbl goal = 
  let sqnt = sequent goal
  in 
  match Lib.try_app (Logic.get_label_asm lbl) sqnt with
    Some _ -> Logic.Tactics.nameA ?info n lbl goal
  | None -> Logic.Tactics.nameC ?info n lbl goal


(*** Unification tactics ***)

let find_basic asm concl node = 
  let sqnt = sequent node
  in 
  let scp = scope_of node
  and node_asms = 
    match asm with 
      None -> asms_of sqnt
    | Some(x) -> [get_tagged_asm x node]
  and node_concls = 
    match concl with
      None ->  concls_of sqnt
    | Some(x) -> [get_tagged_concl x node]
  in 
(** 
   find_match c: 
   find a match for conclusion c in the list of assumptions 
 *)
  let find_match c =  
    Lib.try_find 
      (Lib.first 
	 (fun x -> Formula.alpha_equals scp (drop_tag x) c)) 
      node_asms
  in 
  let rec find_basic_aux xs =
    match xs with
      [] -> raise Not_found
    | cform::cs -> 
	match find_match (drop_tag cform) with
	  Some aform -> 
	    (ftag (drop_formula aform), ftag (drop_formula cform))
	| None -> find_basic_aux cs 
  in 
  find_basic_aux node_concls

let basic ?info ?a ?c goal =
  match (a, c) with 
    (Some albl, Some clbl) ->
      (try Logic.Tactics.basic ?info albl clbl goal
      with err -> raise (add_error "basic: failed" err))
  | _ -> 
      (match (Lib.try_find (find_basic a c) goal) with
	None -> raise (error "basic: failed")
      | Some(al, cl) -> Logic.Tactics.basic ?info al cl goal)

let unify_tac ?info ?(a=(fnum (-1))) ?(c=(fnum 1)) goal =
  let sqnt=sequent goal
  in 
  let asm = 
    try Formula.term_of (get_asm a goal)
    with Not_found ->
      raise(error "unify_tac: assumption not found")
  and concl = 
    try Formula.term_of (get_concl c goal)
    with Not_found ->
      raise(error "unify_tac: conclusion not found")
  in 
  let asm_vars, asm_body =  Term.strip_qnt Basic.All asm
  and concl_vars, concl_body = Term.strip_qnt Basic.Ex concl
  in 
  let asm_varp x = Rewrite.is_free_binder asm_vars x
  and concl_varp x = Rewrite.is_free_binder concl_vars x
  in 
  let varp x = (asm_varp x) || (concl_varp x)
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
	    raise (error "unify_tac: can't unify formulas")
  in 
  let asm_consts = extract_consts asm_vars env1
  and concl_consts = extract_consts concl_vars env1
  in 
  seq 
    [
     instA ?info:info ~a:a asm_consts;
     instC ?info:info ~c:c concl_consts;
     (basic ?info:info ~a:a ~c:c // skip) 
   ] goal

let substA ?info rs l g = Logic.Tactics.substA ?info rs l g

let substC ?info rs l g = Logic.Tactics.substC ?info rs l g


(***
 * Derived tactics and tacticals
 ***)

(** 
   [named_tac tac anames cnames]: apply [tac ~info:inf goal], rename
   each of [aformulas inf] with a name from [anames], rename
   each of [cformulas inf] with a name from [cnames], in
   order. Set [info=inf'] where [inf'] is [inf], with the formula tag
   produced by renaming.
 *) 
let named_tac ?info tac anames cnames (goal: Logic.node) =
  let inf1 = mk_info()
  and inf2 = mk_info()
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
  let albls = List.map ftag (aformulas inf1)
  and clbls = List.map ftag (cformulas inf1)
  in 
  let g2 = name_list anames albls g1
  in 
  let g3 = name_list cnames clbls g2
  in 
  add_info info 
    (subgoals inf1) 
    (List.rev (aformulas inf2)) 
    (List.rev (cformulas inf2))
    (constants inf1);
  g3


(*** Pattern matching tacticals ***)

(**
   [find_match_formulas typenv scp varp t fs]

   Match a list of tagged formulas 
   Return the tag of the first formula in [fs] to unify 
   with term [t] in scope [scp].

   [varp] determines which terms can be bound by unification.
   [typenv] is the goals type environment.

   raise Not_found if no match.
 *)
let find_match_formulas typenv scp varp t fs=
  let rec match_aux l = 
    match l with 
      [] -> raise Not_found
    | (tf::tfs) ->
	try 
	  let tg, f = tf
	  in
	  ignore(Unify.unify ~typenv:typenv scp 
		   varp t (Formula.term_of f));
	  tg
	with _ -> match_aux tfs
  in
  Logic.FTag (match_aux fs)

let find_match_asm typenv t sq=
  let scp = Logic.Sequent.scope_of sq
  and asms = Logic.Sequent.asms sq
  in 
  let t1 = Term.set_names scp t
  in let vars = Term.get_free_vars t1
  in let varp x = 
    try (ignore(List.find (Term.equals x) vars); true)
    with Not_found -> false
  in 
  find_match_formulas typenv scp varp t1 asms

let find_match_concl typenv t sq=
  let scp = Logic.Sequent.scope_of sq
  and concls = Logic.Sequent.concls sq
  in let t1=Term.set_names scp t
  in let vars = Term.get_free_vars t1
  in let varp x = 
    try (ignore(List.find (Term.equals x) vars); true)
    with Not_found -> false
  in 
  find_match_formulas typenv scp varp t1 concls

let match_asm trm tac g =
  try 
    tac (find_match_asm (typenv_of g) trm (sequent g)) g
  with Not_found -> raise (Term.term_error "No matching assumption" [trm])

let match_concl trm tac g =
  try 
    tac (find_match_concl (typenv_of g) trm (sequent g)) g
  with Not_found -> raise (Term.term_error "No matching conclusion" [trm])

let match_formula trm tac g=
  let sqnt=sequent g
  and tyenv=typenv_of g
  in 
  try
    tac (find_match_asm tyenv trm sqnt) g
  with Not_found -> 
    try 
      tac (find_match_concl tyenv trm sqnt) g
    with Not_found ->
      raise (Term.term_error "No matching formula in sequent" [trm])


let specA ?info ?a g =
  let inf1 = mk_info()
  in 
  let add_data (atgs, cs) =
    add_info info [] [get_one atgs] [] (List.rev cs) 
  in 
  alt
    [
     seq 
       [ 
	 repeat (existA ~info:inf1 ?a);
	 (fun g1 ->
	   data_tac add_data ((aformulas inf1), (constants inf1)) g1)
       ];
     fail ~err:(error "specA")
   ] g

let specC ?info ?c g =
  let inf1 = mk_info()
  in 
  let add_data (ctgs, cs) =
    add_info info [] [] [get_one ctgs] (List.rev cs) 
  in 
  alt
    [
     seq 
       [ 
	 repeat (allC ~info:inf1 ?c);
	 (fun g1 ->
	   data_tac add_data ((cformulas inf1), (constants inf1)) g1)
       ];
     fail ~err:(error "specC")
   ] g
    
let spec_tac ?info ?f g=
  alt
    [
     specC ?info ?c:f;
     specA ?info ?a:f;
     fail ~err:(error "specA")
   ] g




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

module Rewriter =
  struct

    open Rewrite.Planned
      
    type ('a)plan = ('a)Rewrite.Planned.plan
	  (** Rewrite plans *)

    type rule = Logic.rr_type
	  (** Rewrite rules *)

	  (** 
	     [pure_rewriteA info p l]: Rewrite assumption [l] with
	     plan [p].
	   *)
    let pure_rewriteA ?info ?term plan lbl goal =
      let inf = mk_info()
      in 
      let ltag = Logic.label_to_tag lbl (sequent goal)
      in 
      let trm = 
	match term with
	  None -> Formula.term_of (get_asm lbl goal)
	| Some(x) -> x
      in 
      let tac1 g = 
	Logic.Tactics.rewrite_intro ~info:inf plan trm g
      in 
      let tac2 g =
	let rule_tag = get_one (aformulas inf)
	in 
	seq
	  [
	   Logic.Tactics.substA ?info [ftag (rule_tag)] (ftag ltag);
	   deleteA (ftag rule_tag)
	 ] g
      in 
      try seq [ tac1; tac2 ] goal 
      with 
	Not_found -> raise Not_found
      | err -> raise (add_error "Tactics.Rewriter.pure_rewriteA" err)

	    (** 
	       [pure_rewriteC info p l]: Rewrite conclusion [l] with
	       plan [p].
	     *)
    let pure_rewriteC ?info ?term plan lbl goal =
      let inf = mk_info()
      in 
      let ltag = Logic.label_to_tag lbl (sequent goal)
      in 
      let trm = 
	match term with
	  None -> Formula.term_of (get_concl lbl goal)
	| Some(x) -> x
      in 
      let tac1 g = 
	Logic.Tactics.rewrite_intro ~info:inf plan trm g
      in 
      let tac2 g =
	let rule_tag = get_one (aformulas inf)
	in 
	seq
	  [
	   Logic.Tactics.substC ?info [ftag (rule_tag)] (ftag ltag);
	   deleteA (ftag rule_tag)
	 ] g
      in 
      try seq [ tac1; tac2 ] goal 
      with 
	Not_found -> raise Not_found
      | err -> raise (add_error "Tactics.Rewriter.pure_rewriteA" err)

(** 
   [pure_rewrite info p l]: Combination of [pure_rewriteC] and
   [pure_rewriteA]. First tries [pure_rewriteC] then tries
   [pure_rewriteA].
 *)
    let pure_rewrite_tac ?info ?term plan lbl goal =
      try
	(try (pure_rewriteC ?info ?term plan lbl goal)
	with Not_found -> (pure_rewriteA ?info ?term plan lbl goal))
      with 
	Not_found -> raise Not_found
      | err -> raise (add_error "Tactics.Rewrite.pure_rewrite_tac" err)


(** 
   [plan_rewrite_conv plan scp trm]: rewrite term [trm] according to
   [plan] in scope [scp]. This is an interface to
   {!Logic.Conv.plan_rewrite_conv}.

   Returns [|- trm = X] where [X] is the result of rewriting [trm]
 *)
    let pure_rewrite_conv = Logic.Conv.plan_rewrite_conv

(** 
   [plan_rewrite_rule plan scp thm]: rewrite theorem [thm] according to
   [plan] in scope [scp]. 

   Returns [|- X] where [X] is the result of rewriting [trm]
 *)
    let pure_rewrite_rule plan scp thm =
      let info = mk_info()
      in 
      let term = Logic.term_of thm
      in 
      let rule = pure_rewrite_conv plan scp term
      in 
      let plan1 = 
	mk_node appln_key [mk_rules [RRThm rule]]
      in 
      let (lhs, rhs) = 
	(Logicterm.dest_equality <+ Logic.term_of) rule
      in 
      let goal = mk_goal ~info:info scp (Formula.make scp rhs)
      in 
      let tac g =
	let ctag = 
	  Lib.get_one (cformulas info) (Failure "pure_rewrite_rule")
	in 
	seq
	  [ 
	    cut ~info:info thm;
	    fun g1 ->
	      let atag = 
		Lib.get_one (aformulas info) (Failure "pure_rewrite_rule")
	      in 
	      seq
		[
		 pure_rewriteA ~info:info ~term:term plan1 (ftag atag);
		 fun g2 -> 
		   let atag1 = 
		     Lib.get_one 
		       (aformulas info) (Failure "pure_rewrite_rule")
		   in 
		   basic ~a:(ftag atag1) ~c:(ftag ctag) g2
	       ] g1
	  ] g
      in
      mk_thm (Logic.Subgoals.apply_to_goal tac goal)


(***
 * Rewrite planning
 ***)

    let dest_term x p=
      let qs, b = Term.strip_qnt Basic.All x
      in 
      let lhs, rhs= Logicterm.dest_equality b
      in 
      (qs, lhs, rhs, p)

(**
   [extract_rules (scp, node) rl]: Extract the rewrite rule
   [rl], getting assumptions from [node].
   
   Extracts the assumptions to use as a rule from subgoal [sg]. Checks
   that other rules are in the scope of [sg]. Creates unordered or
   ordered rewrite rules as appropriate.

   Fails if any rule in [rls] is the label of an assumption 
   which does not exist in [sg].

   Fails if any rule in [rls] is not in scope.
 *)
    let extract_rule node src= 
      let (form, p) = 
	match src with
	  Asm(x) ->
	    let sq = 
	      Subgoals.node_sqnt 
		(Lib.dest_option ~err:(error "extract_rule") node)
	    in
	    let asm=
	      (try 
		drop_tag(Sequent.get_tagged_asm (label_to_tag x sq) sq)
	      with 
		Not_found -> 
		  raise 
		    (error "extract_rule: can't find tagged assumption"))
	    in 
 	    (asm, None)
	| OAsm(x, order) ->
	    let sq = 
	      Subgoals.node_sqnt 
		(Lib.dest_option ~err:(error "extract_rule") node)
	    in
	    let asm=
	      (try drop_tag (Sequent.get_tagged_asm (label_to_tag x sq) sq)
	      with 
		Not_found -> 
		  raise 
		    (error "extract_rule: can't find tagged assumption"))
	    in 
	    (asm, Some(order))
	| RRThm(x) -> 
 	    (formula_of x, None)
	| ORRThm(x, order) -> 
	    (formula_of x, Some(order))
      in 
      dest_term (Formula.term_of form) p

(** The main rewrite planner *)
    module PlannerData =
      struct
	type rule = Logic.rr_type
	type data = Logic.node option
	let dest = extract_rule
      end

    module Planner = Rewrite.Planner(PlannerData)

    let mk_plan ?(ctrl=Formula.default_rr_control) goal rules term =
      let scp = scope_of goal 
      in 
      let (_, p) = Planner.make (Some(goal)) scp ctrl rules term
      in p

(** The theorem rewrite planner (for conversions) *)
    let dest_rr_thm src = 
      match src with
	RRThm(x) -> x
      | ORRThm(x, _) -> x
      | _ -> failwith "Tactics.Rewriter.dest_rr_thm"

(*
   module ThmPlannerData =
   struct
   type rule = Logic.thm
   type data = unit
   let dest = extract_rule
   end

   module ThmPlanner = Rewrite.Planner(ThmPlannerData)
 *)

    let dest_rr_thm src = 
      match src with
	RRThm(x) -> x
      | ORRThm(x, _) -> x
      | _ -> failwith "Tactics.Rewriter.dest_rr_thm"

    let to_thm_plan plan = 
      mapping dest_rr_thm plan

    let mk_thm_plan scp ?(ctrl=Formula.default_rr_control) rules term =
      let (_, p) = Planner.make None scp ctrl rules term
      in 
      to_thm_plan p

    let get_thm id =
      let t, n = Global.read_identifier id
      in 
      let thys = Global.theories()
      in 
      Thydb.get_lemma t n thys

    let prove ?info ?scp trm tac = 
      let sp = Lib.get_option scp (Global.scope())
      in 
      let goal = mk_goal ?info sp (Formula.make sp trm)
      in 
      mk_thm (Logic.Subgoals.apply_to_goal tac goal)

    let make_false_def () = 
      get_thm (Logicterm.base_thy ^"."^"false_def")
    let false_def_var = Lib.freeze make_false_def
    let false_def () = Lib.thaw false_def_var

    let falseA ?info ?a goal =
      let af= first_asm_label a Formula.is_false goal
      in 
      let th=
	try false_def()
	with Not_found -> 
	  raise 
	    (Result.error 
	       ("Tactics.Rewriter.falseA: "
		^"Can't find needed theorem false_def: |- false = not true"))
      in 
      let plan = 
	Rewrite.Planned.mk_node 
	  Rewrite.Planned.anyterm
	  [Rewrite.Planned.mk_rules [RRThm(th)]]
      in 
      pure_rewriteA ?info plan af goal


    let trivial ?info ?f g =  
      try 
	(trueC ?info ?c:f 
	   // falseA ?info ?a:f) g
      with _ -> raise (error "trivial")

(*
   let unfold ?info f str g= 
   let defn id =
   let t, n = Global.read_identifier id
   in 
   Thydb.get_defn t n (Global.theories())
   in 
   let pl th= 
   let term = get_form f g
   and scp = scope_of g
   in 
   mk_plan scp g [RRThm th] (Formula.term_of term)
   in 
   match Lib.try_find defn str with
   None -> 
   raise (error ("unfold: Can't find definition of "^str))
   | (Some th) -> pure_rewrite_tac ?info ?f (pl th) g
 *)

    let make_eq_refl_thm () = 
      try 
	get_thm 
	  (Basic.string_fnid (Basic.mk_long Logicterm.base_thy "eq_refl"))
      with Not_found ->
	raise (error 
		 ("Tactics.Rewriter.make_eq_refl_thm:"
		  ^"Can't find needed axiom eq_refl: |- !x: (x = x)"))

    let eq_refl_thm_var = Lib.freeze make_eq_refl_thm
    let eq_refl_thm () =  Lib.thaw eq_refl_thm_var

    let make_bool_cases_thm () = 
      try 
	get_thm 
	  (Basic.string_fnid (Basic.mk_long Logicterm.base_thy "bool_cases"))
      with Not_found ->
	raise (error 
		 ("Tactics.Rewriter.make_bool_cases_thm:"
		  ^"Can't find needed axiom bool_cases: "
		  ^"|- !x: (x = true) | (x=false)"))

    let bool_cases_thm_var = Lib.freeze make_bool_cases_thm
    let bool_cases_thm () =  Lib.thaw bool_cases_thm_var

    let make_eq_sym_thm () = 
      match Lib.try_app get_thm "Bool.eq_sym" with
	Some(th) -> th
      | None -> 
	  let eq_l1 =
	    prove << !x y : (x = y) => (y = x) >>
	    ((repeat allC) ++ implC
	       ++ substC [!~1] (!! 1) 
	       ++ cut ~inst:[ << _y >> ] (eq_refl_thm ()) ++ basic)
	  in 
	  let eq_l2 =
	    prove << !x y : ((x => y) & (y => x)) => (x = y)>>
	    ((repeat allC)
	       ++ cut ~inst:[ << _x >>] (bool_cases_thm()) ++ disjA
	       ++ cut ~inst:[ << _y >>] (bool_cases_thm()) ++ disjA
	       ++ substC [ !~ 1; !~ 2] (!! 1) ++ implC
	       --
	       [
		cut ~inst:[ << true >> ] (eq_refl_thm()) ++ basic ;
		conjA ++ implA ++ trivial;
		conjA ++ implA ++ implA ++ trivial;
		cut ~inst:[ << false >> ] (eq_refl_thm()) ++ basic
	      ])
	  in 
	  prove << !x y : (x = y) = (y = x)>>
	  ((repeat allC)
	     ++ cut ~inst:[ << _x = _y >> ; << _y = _x >>] eq_l2
	     ++ implA 
	     --
	     [ 
	       conjC
		 -- 
		 [
		  cut ~inst:[ << _x >> ; << _y >> ] eq_l1 ++ basic ;
		  cut ~inst:[ << _y >> ; << _x >> ] eq_l1 ++ basic 
		] ;
	       basic
	     ])

    let eq_sym_thm_var = Lib.freeze make_eq_sym_thm;;
  let eq_sym_thm () =  Lib.thaw eq_sym_thm_var;;

let eq_sym_rule scp thm= 
  let ctrl = 
    {Formula.default_rr_control with Rewrite.depth = Some 1}
  in 
  let term = Logic.term_of thm
  in 
  let plan = mk_thm_plan scp ~ctrl:ctrl [ RRThm (eq_sym_thm()) ] term
  in 
  pure_rewrite_rule plan scp thm

let eq_symA ?info a goal =
  let ctrl = 
    {Formula.default_rr_control with Rewrite.depth = Some 1}
  in 
  let (atag, form) = get_tagged_asm a goal
  in 
  let term = Formula.term_of form
  in 
  let plan = mk_plan ~ctrl:ctrl goal [ RRThm (eq_sym_thm()) ] term
  in 
  pure_rewriteA ?info plan (ftag atag) goal

let eq_symC ?info c goal =
  let ctrl = 
    {Formula.default_rr_control with Rewrite.depth = Some 1}
  in 
  let (ctag, form) = (get_tagged_concl c goal)
  in 
  let term = Formula.term_of form
  in 
  let plan = mk_plan ~ctrl:ctrl goal [ RRThm (eq_sym_thm()) ] term
  in 
  pure_rewriteC ?info plan (ftag ctag) goal
    
let eq_sym_tac ?info f goal = 
  try 
    eq_symA ?info f goal
  with Not_found -> eq_symC ?info f goal

(** {7 Rewrite functions} *)

(**
   [rewrite_conv scp ctrl rules trm]:
   rewrite term [trm] with rules [rrl] in scope [scp].

   Returns |- trm = X where [X] is the result of rewriting [trm]
 *)

let rewrite_conv ?ctrl rls scp term = 
  let c = Lib.get_option ctrl Rewrite.default_control
  in 
  let is_rl = c.Rewrite.rr_dir=rightleft
  in 
  let mapper f x = 
    match x with
      RRThm t -> RRThm(f t)
    | ORRThm (t, o) -> ORRThm(f t, o)
    | _ -> 
	raise 
	  (error "rewrite_conv: Invalid assumption rewrite rule")
  in 
  let rules = 
    if is_rl 
    then List.map (mapper (eq_sym_rule scp)) rls
    else rls
  in
  let plan = mk_thm_plan scp ~ctrl:c rules term
  in 
  Logic.Conv.plan_rewrite_conv plan scp term
    

(**
   [map_sym_tac ret rules goal]: Apply [eq_sym] to each rule in
   [rules], returning the resulting list in [ret]. The list in [ret]
   will be in reverse order of [rules]. 
 *)
(*
let map_sym_tac ret rules goal = 
  let scp = scope_of goal
  in 
  let set v x = Lib.set_option v x
  in 
  let asm_fn f v l g = 
    let info = mk_info()
    in 
    let tac1 g1 = 
      let nl = 
	Lib.get_one (aformulas info)
	  (error "Rewriter.map_sym_tac: Invalid assumption")
      in 
      data_tac (set v) (f nl) g
    in       
    try (eq_symA ~info:info l ++ tac1) g
    with  err -> raise (add_error "Rewriter.map_sym_tac" err)
  in 
  let fn_tac v r g =
    match r with
      RRThm(th) -> 
	data_tac (set v) (RRThm(eq_sym_rule scp th)) g
    | ORRThm(th, o) -> 
	data_tac (set v) (ORRThm(eq_sym_rule scp th, o)) g
    | Asm(l) -> 
	asm_fn (fun nl -> Asm(ftag nl)) v l g
    | OAsm(l, o) -> 
	asm_fn (fun nl -> OAsm(ftag nl, o)) v l g
  in 
  let mapping lst rl g = 
    let nr = ref None
    in 
    seq
      [
       fn_tac nr rl;
       (fun g1 ->
	 data_tac 
	   (fun x -> 
	     let rl1 = 
	       Lib.dest_option (!x) ~err:(error "Rewriter.map_sym_tac")
	     in 
	     lst:=(rl1::(!lst))) nr g1)
     ] g
  in 
  map_every (mapping ret) rules goal
*)
let map_sym_tac ret rules goal = 
  let scp = scope_of goal
  in 
  let asm_fn l g = 
    let info = mk_info()
    in 
    try 
      let g2 = eq_symA ~info:info l g
      in 
      let nl = 
	Lib.get_one (aformulas info)
	  (error "Rewriter.map_sym_tac: Invalid assumption")
      in 
      (ftag nl, g2)
    with  err -> raise (add_error "Rewriter.map_sym_tac" err)
  in 
  let fn_tac r g =
    match r with
      RRThm(th) -> 
	(RRThm(eq_sym_rule scp th), skip g)
    | ORRThm(th, o) -> 
	(ORRThm(eq_sym_rule scp th, o), skip g)
    | Asm(l) -> 
	let (nl, ng) = asm_fn l g
	in 
	(Asm(nl), ng)
    | OAsm(l, o) -> 
	let (nl, ng) = asm_fn l g
	in 
	(OAsm(nl, o), ng)
  in 
  let mapping lst rl g = 
    let (nr, g2) = fn_tac rl g
    in 
    lst := nr:: (!lst); g2
  in 
  map_every (mapping ret) rules goal


let rewriteA_tac 
    ?info ?(ctrl=Formula.default_rr_control) 
    rules albl goal =
  let (atag, aform) = get_tagged_asm albl goal
  in 
  let aterm = Formula.term_of aform
  in 
  let is_lr = not (ctrl.Rewrite.rr_dir = rightleft)
  in 
  let urules = ref [] 
  in 
  let tac1 g = 
    if is_lr 
    then (data_tac (fun _ -> urules := rules) ()) g
    else 
      (seq 
	 [
	  map_sym_tac urules rules;
	  (fun g1 -> 
	    data_tac (fun x -> urules := List.rev !x) urules g1)
	]) g
  in 
  let tac2 g = 
    let rls = !urules 
    in 
    let plan = mk_plan ~ctrl:ctrl goal rls aterm
    in 
    pure_rewriteA ?info plan (ftag atag) g
  in 
  let tac3 g = 
    if is_lr
    then skip g
    else (map_sym_tac (ref []) rules) g
  in 
  try 
    seq [tac1; tac2; tac3] goal
  with 
    err -> 
      raise (add_error "Rewriter.rewriteA_tac" err)
	

let rewriteC_tac ?info ?(ctrl=Formula.default_rr_control) rules clbl goal =
  let (ctag, cform) = get_tagged_concl clbl goal
  in 
  let cterm = Formula.term_of cform
  in 
  let is_lr = not (ctrl.Rewrite.rr_dir = rightleft)
  in 
  let urules = ref [] 
  in 
  let tac1 g = 
    if is_lr
    then (data_tac (fun x -> urules := x) rules) g
    else 
      seq 
	 [
	  map_sym_tac urules rules;
	  (fun g1 -> 
	    data_tac (fun x -> urules := List.rev !x) urules g1)
	] g
  in 
  let tac2 g = 
    let rls = !urules 
    in 
    let plan = mk_plan ~ctrl:ctrl goal rls cterm
    in 
    pure_rewriteC ?info plan (ftag ctag) g
  in 
  let tac3 g = 
    if is_lr
    then skip g
    else (map_sym_tac (ref []) rules) g
  in 
  try 
    seq [tac1; tac2; tac3] goal
  with 
    err -> 
      raise (add_error "Rewriter.rewriteA_tac" err)
	

(**
   rewrite ?info ctrl rules l sq: Rewrite formula [l] with [rules].
   
   If [l] is in the conclusions then call [rewrite_concl]
   otherwise call [rewrite_asm].
 *)
let rewrite_tac ?info ?(ctrl=Formula.default_rr_control) rls f g=
  try
    (try 
      rewriteA_tac ?info ~ctrl:ctrl rls f g
    with Not_found -> 
      rewriteC_tac ?info ~ctrl:ctrl rls f g)
  with err -> 
    raise (add_error "Rewriter.rewrite_tac" err)

end


(*
let gen_rewrite_tac ?info ?asm ctrl ?f rules goal =
  match f with
    None -> 
      (match asm with
	None -> 
	  foreach_form
	    (Logic.Tactics.rewrite info ~ctrl:ctrl rules) goal
      | Some(x) ->
	  if x 
	  then 
	    foreach_asm
	      (Logic.Tactics.rewrite info ~ctrl:ctrl rules) goal
	  else 
	    foreach_concl
	      (Logic.Tactics.rewrite info ~ctrl:ctrl rules) goal)
  | Some (x) ->
      Logic.Tactics.rewrite info ~ctrl:ctrl rules x goal
*)
let gen_rewrite_tac ?info ?asm ctrl ?f rules goal =
  match f with
    None -> 
      (match asm with
	None -> 
	  seq_some
	    [
	     foreach_asm
	       (Rewriter.rewriteA_tac ?info ~ctrl:ctrl rules);
	     foreach_concl
	       (Rewriter.rewriteC_tac ?info ~ctrl:ctrl rules);
	   ] goal
      | Some(x) ->
	  if x 
	  then 
	    foreach_asm
	      (Rewriter.rewriteA_tac ?info ~ctrl:ctrl rules) goal
	  else 
	    foreach_concl
	      (Rewriter.rewriteC_tac ?info ~ctrl:ctrl rules) goal)
  | Some (x) ->
     Rewriter.rewrite_tac ?info ~ctrl:ctrl rules x goal

	
let rewrite_tac ?info ?(dir=leftright) ?f ths goal=
  let ctrl = rewrite_control dir
  in 
  let rules = (List.map (fun x -> Logic.RRThm x) ths) 
  in 
  gen_rewrite_tac ?info:info ctrl ?f:f rules goal 

let once_rewrite_tac ?info ?(dir=leftright) ?f ths goal=
  let ctrl=rewrite_control ~max:1 dir
  in 
  let rules = (List.map (fun x -> Logic.RRThm x) ths) 
  in 
  gen_rewrite_tac ?info:info ctrl rules ?f:f goal

let rewriteA_tac ?info ?(dir=leftright) ?f ths goal=
  let ctrl = rewrite_control dir
  in 
  let rules = (List.map (fun x -> Logic.RRThm x) ths) 
  in 
  gen_rewrite_tac ?info:info ~asm:true ctrl ?f:f rules goal 

let once_rewriteA_tac ?info ?(dir=leftright) ?f ths goal=
  let ctrl=rewrite_control ~max:1 dir
  in 
  let rules = (List.map (fun x -> Logic.RRThm x) ths) 
  in 
  gen_rewrite_tac ?info:info ~asm:true ctrl rules ?f:f goal

let rewriteC_tac ?info ?(dir=leftright) ?f ths goal=
  let ctrl = rewrite_control dir
  in 
  let rules = (List.map (fun x -> Logic.RRThm x) ths) 
  in 
  gen_rewrite_tac ?info:info ~asm:false ctrl ?f:f rules goal 

let once_rewriteC_tac ?info ?(dir=leftright) ?f ths goal=
  let ctrl=rewrite_control ~max:1 dir
  in 
  let rules = (List.map (fun x -> Logic.RRThm x) ths) 
  in 
  gen_rewrite_tac ?info:info ~asm:false ctrl rules ?f:f goal

let gen_replace_tac ?info ?(ctrl=Formula.default_rr_control) ?asms ?f goal =
  let sqnt = sequent goal
  in
  (*** ttag: The tag of tag of the target (if given) ***)
  let ttag = 
    match f with 
      None -> None 
    | Some(x) -> Some(Logic.label_to_tag x sqnt)
  in 
  (*** exclude: a predicate to filter the rewriting target ***)
  let exclude tg = 
    match ttag with 
      None -> false 
    | Some(x) -> Tag.equal tg x
  in 
  (*** find_equality_asms: Find the assumptions which are equalities ***)
  let rec find_equality_asms sqasms rst=
    match sqasms with 
      [] -> List.rev rst
    | form::xs -> 
	let tg = drop_formula form 
	in 
	(if not (exclude tg)
	    && (qnt_opt_of Basic.All 
		  (Logicterm.is_equality) (Formula.term_of (drop_tag form)))
	then find_equality_asms xs (tg::rst)
	else find_equality_asms xs rst)
  in 
  (*** asm_tags: The assumptions to use for rewriting. ***)
  let asm_tags =
    match asms with
      None -> find_equality_asms (Logic.Sequent.asms sqnt) []
    | Some xs -> List.map (fun x -> Logic.label_to_tag x sqnt) xs
  in 
  (*** rules: Assumption labels in rewriting form ***)
  let rules = List.map (fun x -> Logic.Asm (ftag x)) asm_tags
  in 
  (*** 
     filter_replace: The replacment tactics, filtering the target
     to avoid trying to rewrite a formula with itself. 
   ***)
  let filter_replace x =
    if (List.exists 
	  (Tag.equal (Logic.label_to_tag x sqnt)) asm_tags)
    then fail ~err:(error "gen_replace")
    else 
      (Logic.Tactics.rewrite ?info ~ctrl:ctrl rules x)
  in 
  (*** 
     tac: apply filter_replace to an identified formula or to 
     all formulas in the sequent.
   ***)
  let tac = 
    match ttag with
      None -> foreach_form filter_replace
    | Some(x) -> filter_replace (ftag x)
  in 
  alt 
    [
     tac;
     fail ~err:(error "gen_replace")
   ] goal


let replace_tac ?info ?(dir=leftright) ?asms ?f goal=
  let ctrl=rewrite_control dir
  in 
  gen_replace_tac ?info:info ~ctrl:ctrl ?asms:asms ?f:f goal

let once_replace_tac ?info ?(dir=leftright) ?asms ?f goal=
  let ctrl=rewrite_control ~max:1 dir
  in 
  gen_replace_tac ?info:info ~ctrl:ctrl ?asms:asms ?f:f goal


