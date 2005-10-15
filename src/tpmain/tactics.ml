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

let skip = Logic.Tactics.skip None

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
      [] -> raise (error "alt: no successful tactic")
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

let restrict p tac goal =
  let ng = tac goal
  in 
  if (p ng) then ng else raise (Failure "restrict_tac")

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
  let nofail_tac l = (tac l || skip)
  in 
  let rec some_aux ls g =
    match ls with 
      [] -> fail ~err:(error "map_some: no tactic succeeded.") g
    | (x::xs) ->
	try (tac x ++ map_every nofail_tac xs) g
	with _ -> some_aux xs g
  in 
  some_aux l goal

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
    (((foreach_asm tac) ++ data_tac notify ()) || skip) g 
  and concls_tac g = 
    (((foreach_concl tac) ++ data_tac notify ()) || skip) g 
  in 
  restrict 
    (fun _ -> !chng)
    (asms_tac ++ concls_tac) goal

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

let trueC ?info ?c sq =
  let cf = first_concl_label c Formula.is_true sq
  in Logic.Tactics.trueC info cf sq

let conjC ?info ?c sq =
  let cf = first_concl_label c Formula.is_conj sq
  in Logic.Tactics.conjC info cf sq

let conjA ?info ?a sq =
  let af = first_asm_label a Formula.is_conj sq
  in Logic.Tactics.conjA info af sq

let disjC ?info ?c sq =
  let cf= first_concl_label c Formula.is_disj sq
  in Logic.Tactics.disjC info cf sq

let disjA ?info ?a sq =
  let af= first_asm_label a Formula.is_disj sq
  in Logic.Tactics.disjA info af sq

let negC ?info ?c sq =
  let cf= first_concl_label c Formula.is_neg sq
  in Logic.Tactics.negC info cf sq

let negA ?info ?a sq =
  let af= first_asm_label a Formula.is_neg sq
  in Logic.Tactics.negA info af sq

let implC ?info ?c sq =
  let cf= first_concl_label c Formula.is_implies sq
  in
  Logic.Tactics.implC info cf sq

let implA ?info ?a sq =
  let af= first_asm_label a Formula.is_implies sq
  in 
  Logic.Tactics.implA info af sq

let existC ?info ?c trm sq =
  let cf= first_concl_label c Formula.is_exists sq
  in Logic.Tactics.existC info trm cf sq

let existA ?info ?a sq =
  let af= first_asm_label a Formula.is_exists sq
  in Logic.Tactics.existA info af sq

let allC ?info ?c sq =
  let cf= first_concl_label c Formula.is_all sq
  in Logic.Tactics.allC info cf sq

let allA ?info ?a trm sq =
  let af= first_asm_label a Formula.is_all sq
  in Logic.Tactics.allA info trm af sq

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
      let g1 = Logic.Tactics.cut (Some info1) th g
      in 
      let atag = get_one ~msg:"cut" (aformulas info1)
      in 
      empty_info info1;
      foreach (instA ?info:info ~a:(ftag atag) trms) g1
  in 
  match inst with
    None -> Logic.Tactics.cut info th goal
  | Some(trms) -> 
      try cut0 trms goal
      with err -> 
	raise (add_error "cut" err)


let beta_tac ?info ?f g= 
  match f with
    (Some x) -> Logic.Tactics.beta info x g
  | _ -> 
      try foreach_form (Logic.Tactics.beta info) g
      with err -> 
	raise (add_error "beta_tac: failed." err)

let name_tac ?info n lbl goal = 
  let sqnt = sequent goal
  in 
  match Lib.try_app (Logic.get_label_asm lbl) sqnt with
    Some _ -> Logic.Tactics.nameA info n lbl goal
  | None -> Logic.Tactics.nameC info n lbl goal


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
      (try Logic.Tactics.basic info albl clbl goal
      with err -> raise (add_error "basic: failed" err))
  | _ -> 
      (match (Lib.try_find (find_basic a c) goal) with
	None -> raise (error "basic: failed")
      | Some(al, cl) -> Logic.Tactics.basic info al cl goal)

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
  let varp x = Pervasives.(||) (asm_varp x) (concl_varp x)
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
     (basic ?info:info ~a:a ~c:c || skip) 
   ] goal

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
      foreach_form
	(Logic.Tactics.rewrite info ~ctrl:ctrl rules) goal
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
      (Logic.Tactics.rewrite info ~ctrl:ctrl rules x)
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


