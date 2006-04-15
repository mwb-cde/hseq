(*-----
  Name: boollib.ml
  Author: M Wahab <mwahab@users.sourceforge.net>
  Copyright M Wahab 2005
  ----*)


open Commands
open Tactics
open Lib.Ops


(****
* Support functions
*****)

(**
   [find_unifier scp typenv varp trm ?exclude ?f forms]: Find the first
   formula in forms which unifies with trm. Return the tag of the
   formula and the substitution cosntructed by unification. Ignore
   those formulas for which [?exclude] is true (if it is given).

   [varp] determines what is a bindable variable for unification.
   [typenv] is the type environment, to pass to the unifier.
   [scp] is the scope, to pass to the unifier.
   Raise Not_found if no unifiable formula is found.
*)
let find_unifier scp typenv varp trm ?exclude forms = 
  let not_this = Lib.get_option exclude (fun _ -> false)
  in 
  let find_fn form =
    if (not_this form) then raise Not_found
    else 
      (Tactics.drop_formula form,
       Unify.unify ~typenv:typenv scp varp trm 
	 (Formula.term_of (Tactics.drop_tag form)))
  in 
  Lib.find_first find_fn forms


(**
   [is_qnt_opt kind pred form]: Test whether [form] satifies [pred].
   The formula may by quantified by binders of kind [kind]. 
*)
let is_qnt_opt kind pred form = 
  Tactics.qnt_opt_of kind pred 
    (Formula.term_of (Tactics.drop_tag form))

(**
   [dest_qnt_opt forms]: Destruct a possibly quantified tagged formula.
   Returns the binders, the tag and the formula.
*)
let dest_qnt_opt kind tform = 
  let tag = Tactics.drop_formula tform
  and form = Tactics.drop_tag tform
  in
  let (vs, term) = Term.strip_qnt kind (Formula.term_of form)
  in 
  (tag, vs, term)
	
(**
   [find_qnt_opt kind ?f pred forms] 

   Find the first formula in [forms] to satisfy [pred].  The formula
   may by quantified by binders of kind [kind].  Returns the binders,
   the tag and the formula.

   if [f] is given, the formula must be tagged with [f]. 

   Raises [Not_found] if no formula can be found which satisfies all the
   conditions.
 *)
let find_qnt_opt kind pred forms = 
  let find_fn tagged_form =
    Tactics.qnt_opt_of kind pred 
      (Formula.term_of (Tactics.drop_tag tagged_form))
  in 
  let tform = Lib.first find_fn forms
  in 
  let tag = Tactics.drop_formula tform
  and form = Tactics.drop_tag tform
  in
  let (vs, term) = Term.strip_qnt kind (Formula.term_of form)
  in 
  (tag, vs, term)


(*****
 * Basic Tactics
 *****)

let fresh_thm th = Logic.is_fresh (Global.scope()) th

let make_false_def () = 
  thm (Lterm.base_thy ^"."^"false_def")
let false_def_var = Lib.freeze make_false_def
let false_def () = 
  Lib.thaw ~fresh:fresh_thm false_def_var
    

let falseA ?info ?a goal =
  let af= first_asm_label a Formula.is_false goal
  in 
  let th=
    try false_def()
    with Not_found -> 
      raise 
	(Report.error 
	   ("Tactics.Rewriter.falseA: "
	    ^"Can't find needed theorem false_def: |- false = not true"))
  in 
  let plan = 
    Rewrite.mk_node 
      Rewrite.anyterm [Rewrite.mk_rules [Logic.RRThm(th)]]
  in 
  let inf = mk_info()
  in 
  seq
    [ 
      pure_rewriteA ~info:inf plan af;
      (fun g ->
	let atag = Lib.get_one (aformulas inf) (Tactics.error "falseA")
	in 
	seq
	  [ 
	    negA ~info:inf ~a:(ftag atag);
	    (fun g1  -> 
	      let ctag = Lib.get_one (cformulas inf) (error "falseA")
	      in 
	      trueC ~c:(ftag ctag) g1)
	  ] g)
    ] goal
	
let trivial ?info ?f g =  
  try 
    (trueC ?info ?c:f 
       // falseA ?info ?a:f) g
  with _ -> raise (error "trivial")

let cut_thm ?info ?inst str = (cut ?info ?inst (thm str))

(*** Basic equality reasoning ***)


let make_eq_refl_thm () = 
  try 
    thm 
      (Ident.string_of (Ident.mk_long Lterm.base_thy "eq_refl"))
  with Not_found ->
    raise (error 
	     ("Tactics.Rewriter.make_eq_refl_thm:"
	      ^"Can't find needed axiom eq_refl: |- !x: (x = x)"))
      
let eq_refl_thm_var = Lib.freeze make_eq_refl_thm
let eq_refl_thm () =  Lib.thaw ~fresh:fresh_thm eq_refl_thm_var

let make_bool_cases_thm () = 
  try
    thm 
      (Ident.string_of (Ident.mk_long Lterm.base_thy "bool_cases"))
  with Not_found ->
    raise (error 
	     ("Tactics.Rewriter.make_bool_cases_thm:"
	      ^"Can't find needed axiom bool_cases: "
	      ^"|- !x: (x = true) | (x=false)"))

let bool_cases_thm_var = Lib.freeze make_bool_cases_thm
let bool_cases_thm () =  Lib.thaw ~fresh:fresh_thm bool_cases_thm_var

let make_eq_sym_thm () = 
  match Lib.try_app thm "Bool.eq_sym" with
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

let eq_sym_thm_var = Lib.freeze make_eq_sym_thm
let eq_sym_thm () =  Lib.thaw ~fresh:fresh_thm eq_sym_thm_var

let eq_sym_rule scp thm= 
  let ctrl = 
    {Formula.default_rr_control with Rewrite.depth = Some 1}
  in 
  let term = Logic.term_of thm
  in 
  let plan = 
    Tactics.mk_thm_plan scp ~ctrl:ctrl [ Logic.RRThm (eq_sym_thm()) ] term
  in 
  Tactics.pure_rewrite_rule plan scp thm

let eq_symA ?info a goal =
  let ctrl = 
    {Formula.default_rr_control with Rewrite.depth = Some 1}
  in 
  let (atag, form) = get_tagged_asm a goal
  in 
  let term = Formula.term_of form
  in 
  let plan = 
    Tactics.mk_plan ~ctrl:ctrl goal [ Logic.RRThm (eq_sym_thm()) ] term
  in 
  Tactics.pure_rewriteA ?info plan (ftag atag) goal

let eq_symC ?info c goal =
  let ctrl = 
    {Formula.default_rr_control with Rewrite.depth = Some 1}
  in 
  let (ctag, form) = (get_tagged_concl c goal)
  in 
  let term = Formula.term_of form
  in 
  let plan = 
    Tactics.mk_plan ~ctrl:ctrl goal [ Logic.RRThm (eq_sym_thm()) ] term
  in 
  Tactics.pure_rewriteC ?info plan (ftag ctag) goal
    
let eq_sym_tac ?info f goal = 
  try 
    eq_symA ?info f goal
  with Not_found -> eq_symC ?info f goal


let eq_tac ?info ?c goal = 
  let th = 
    try thm (Lterm.base_thy ^ ".eq_refl")
    with Not_found -> 
      (raise (error ("eq_tac: Can't find required lemma "
		     ^Lterm.base_thy^".eq_refl")))
  in 
  let info1 = Tactics.mk_info()
  in 
  let tac albl (t, f) g = 
    if (Formula.is_equality f)
    then 
      unify_tac ~a:albl ~c:(ftag t) g
    else 
      fail g
  in 
  let cforms = concls_of (sequent goal)
  in 
  seq 
    [
      Logic.Tactics.cut ~info:info1 th; 
      (fun g -> 
	 let af = get_one ~msg:"eq_tac" (Tactics.aformulas info1)
	 in 
	   map_first (tac (ftag af)) cforms g)
    ] goal


(*** 
 * Generalised Rewriting
 ***)

module Rewriter = 
  struct

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
      Logic.RRThm t -> Logic.RRThm(f t)
    | Logic.ORRThm (t, o) -> Logic.ORRThm(f t, o)
    | _ -> 
	raise 
	  (error "rewrite_conv: Invalid assumption rewrite rule")
  in 
  let rules = 
    if is_rl 
    then List.map (mapper (eq_sym_rule scp)) rls
    else rls
  in
  let plan = Tactics.mk_thm_plan scp ~ctrl:c rules term
  in 
  Tactics.pure_rewrite_conv plan scp term

(**
   [rewrite_rule scp ctrl rules thm:
   rewrite theorem [thm] with rules [rrl] in scope [scp].

   Returns |- X where [X] is the result of rewriting [thm]
 *)
let rewrite_rule scp ?ctrl rls thm = 
  let c = Lib.get_option ctrl Rewrite.default_control
  in 
  let is_rl = c.Rewrite.rr_dir=rightleft
  in 
  let mapper f x = 
    match x with
      Logic.RRThm t -> Logic.RRThm(f t)
    | Logic.ORRThm (t, o) -> Logic.ORRThm(f t, o)
    | _ -> 
	raise 
	  (error "rewrite_conv: Invalid assumption rewrite rule")
  in 
  let rules = 
    if is_rl 
    then List.map (mapper (eq_sym_rule scp)) rls
    else rls
  in
  let plan = Tactics.mk_thm_plan scp ~ctrl:c rules (Logic.term_of thm)
  in 
  Tactics.pure_rewrite_rule plan scp thm

(**
   [map_sym_tac ret rules goal]: Apply [eq_sym] to each rule in
   [rules], returning the resulting list in [ret]. The list in [ret]
   will be in reverse order of [rules]. 
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
	  Logic.RRThm(th) -> 
	    (Logic.RRThm(eq_sym_rule scp th), skip g)
	| Logic.ORRThm(th, o) -> 
	    (Logic.ORRThm(eq_sym_rule scp th, o), skip g)
	| Logic.Asm(l) -> 
	    let (nl, ng) = asm_fn l g
	    in 
	    (Logic.Asm(nl), ng)
	| Logic.OAsm(l, o) -> 
	    let (nl, ng) = asm_fn l g
	    in 
	    (Logic.OAsm(nl, o), ng)
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
	let plan = Tactics.mk_plan ~ctrl:ctrl g rls aterm
	in 
	Tactics.pure_rewriteA ?info plan (ftag atag) g
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
	    

    let rewriteC_tac ?info ?(ctrl=Formula.default_rr_control) 
	rules clbl goal =
      let (ctag, cform) = get_tagged_concl clbl goal
      in 
      let cterm = Formula.term_of cform
      in 
      let is_lr = (ctrl.Rewrite.rr_dir = leftright)
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
	let plan = Tactics.mk_plan ~ctrl:ctrl g rls cterm
	in 
	Tactics.pure_rewriteC ?info plan (ftag ctag) g
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
   [rewrite_tac ?info ctrl rules l sq]: Rewrite formula [l] with [rules].
   
   If [l] is in the conclusions then call [rewriteC_tac]
   otherwise call [rewriteA_tac].
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

let rewrite_conv ?ctrl rls scp trm = 
  Rewriter.rewrite_conv ?ctrl 
    (List.map (fun x -> Logic.RRThm(x)) rls) scp trm

let rewrite_rule scp ?ctrl rls thm = 
  Rewriter.rewrite_rule ?ctrl scp 
    (List.map (fun x -> Logic.RRThm(x)) rls) thm

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
		  (Lterm.is_equality) (Formula.term_of (drop_tag form)))
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
      gen_rewrite_tac ?info ?asm:None ctrl rules ~f:x
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

let unfold ?info ?f str g= 
  match Lib.try_find defn str with
    None -> 
      raise (error ("unfold: Can't find definition of "^str))
  | (Some th) -> rewrite_tac ?info ?f [th] g

(***
 * More boolean tactics
 ***)


(*** Boolean equivalence ***)

let is_iff f = 
  try 
    (fst (Term.dest_fun (Formula.term_of f)) = Lterm.iffid)
  with _ -> false

let make_iff_def () = defn (Ident.string_of Lterm.iffid)
let iff_def_var = Lib.freeze make_iff_def
let iff_def () = Lib.thaw ~fresh:fresh_thm iff_def_var


(** 
   [iffA l sq]: Elminate the equivalance at assumptin [l]

   {L
   g:\[(A iff B){_ l}, asms |- concl]
   ---->
   g:[(A => B){_ l1}, (B => A){_ l2}, asms |- concl]; 
   }

   info: [goals = [], aforms=[l1; l2], cforms=[], terms = []]
 *)
let iffA ?info ?a goal = 
  let af = first_asm_label a is_iff goal
  in 
  let sqnt=Tactics.sequent goal
  in 
  let t, f = Logic.Sequent.get_tagged_asm (Logic.label_to_tag af sqnt) sqnt
  in 
  if not (is_iff f) 
  then (raise (error "iffA"))
  else 
    seq 
      [
       rewrite_tac [iff_def()] ~f:(ftag t);
       Logic.Tactics.conjA ?info (ftag t);
     ] goal
      


(** 
   [iffC l sq]: Elminate the equivalence at conclusion [l]

   {L
   g:\[asms |- (A iff B){_ l}, concl]
   ---->
   g1:\[asms |- (A => B){_ l}, concl]
   g2:\[asms |- (B => A){_ l}, concl]
   }

   info: [goals = [g1; g2], aforms=[], cforms=[l], terms = []]
 **)

let iffC ?info ?c goal = 
  let cf = first_concl_label c is_iff goal
  in 
  let sqnt=sequent goal
  in 
  let t, f = Logic.Sequent.get_tagged_cncl (Logic.label_to_tag cf sqnt) sqnt
  in
  if not (is_iff f) 
  then raise (error "iffC")
  else 
    seq 
      [
       rewrite_tac [iff_def()] ~f:(ftag t);
       Logic.Tactics.conjC ?info (ftag t)
     ] goal




(** 
   [iffE l sq]: Fully elminate the equivalence at conclusion [l]

   {L
   g:\[asms |- (A iff B){_ l}, concl]
   ---->
   g1:[A{_ l1}, asms |- B{_ l2}, concl]; 
   g2:[B{_ l3}, asms |- A{_ l4}, concl]; 
   }

   info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
 **)
let iffE ?info ?c goal = 
  let cf = first_concl_label c is_iff goal
  in 
  let sqnt=sequent goal
  in 
  let t, f = Logic.Sequent.get_tagged_cncl (Logic.label_to_tag cf sqnt) sqnt
  in
  let add_goals info inf =
    let gls = subgoals inf
    in 
    Logic.add_info info gls [] [] [];
    empty_info inf
  in 
  let add_forms info inf=
    let atgs = List.rev (aformulas inf)
    and ctgs = List.rev (cformulas inf)
    in 
    Logic.add_info info [] atgs ctgs [];
    empty_info inf
  in
  if not (is_iff f) 
  then raise (error "iffE")
  else 
    let inf = mk_info()
    in 
    let tac g =
      (seq 
	 [
	  rewrite_tac [iff_def()] ~f:(ftag t);
	  notify_tac (add_goals info) inf
	    (Logic.Tactics.conjC ~info:inf (ftag t));
	  Logic.Tactics.implC ~info:inf (ftag t)
	]) g
    in 
    alt [ notify_tac (add_forms info) inf tac; 
	  fail ~err:(error "iffE") ] goal

(*** 
 * Eliminating boolean operators 
 ***)

(**
   [direct_alt tacs info l]: Directed alt. Like {!Tactics.alt} but
   pass [info] and [l] to each tactic in [tacs].
 **)
let direct_alt tacl info l g=
  let rec alt_aux ts =
    match ts with
      [] -> raise (error "direct_alt: no successful tactic")
    | (x::xs) ->
	try (x info l) g
	with _ -> alt_aux xs
  in alt_aux tacl 


(**
   [direct_map_some tac lst l]: Directed map_some. Like
   {!Tactics.map_som} but pass [info] and [l] to [tac]. If [tac] fails
   for [l], then [lst:=l::!lst].
 **)
let direct_map_some tac lst l goal =
  let add_lbl x = lst:=x::(!lst)
  in 
  let nofail_tac lbl = (tac lbl // data_tac add_lbl lbl)
  in 
  let rec some_aux ls g =
    match ls with 
      [] -> fail ~err:(error "direct_map_some: no tactic succeeded.") g
    | (x::xs) ->
	try (tac x ++ map_every nofail_tac xs) g
	with _ -> add_lbl x; some_aux xs g
  in 
  some_aux l goal

(** 
   [asm_elim_rules ?info (arules, crules) f goal]: Apply elimination
   rules to assumption [f] and to all resulting assumptions and
   conclusions. Assumptions are eliminated with [arules], conclusions
   with [crules]. Any new tag which can't be eliminated are stored in
   [?info] (in arbitrary order).
 *)
let rec asm_elim_rules_tac ?info rules lbl goal=
  let (arules, _) = rules
  and inf = mk_info()
  and alst = ref []
  and clst = ref []
  in 
  let set_info dst (sgs, afs, cfs, cnsts) = 
    Logic.add_info dst sgs afs cfs cnsts
  in 
  seq
    [ 
      (** 
	 Try to elminate the operator.
       **)
      direct_alt arules inf lbl;
      (** 
	 Eliminate new assumptions and conclusions.
       **)
      (fun g -> 
	let albls = List.map ftag (aformulas inf)
	and clbls = List.map ftag (cformulas inf)
	and sqnt = sequent g
	in 
	seq
	  [
	   (** Eliminate assumptions, saving failing labels **)
	   alt
	     [ 
	       direct_map_some (asm_elim_rules_tac ?info rules) alst albls;
	       skip
	     ];
	   (** Eliminate conclusions, saving failing labels **)
	   alt
	     [ 
	       direct_map_some (concl_elim_rules_tac ?info rules) clst clbls;
	       skip
	     ];
	   (** Save failing labels and any other information **)
	   data_tac (set_info info)
	     (subgoals inf, 
	      List.map 
		(fun x -> Logic.label_to_tag x sqnt)  (List.rev (!alst)), 
	      List.map 
		(fun x -> Logic.label_to_tag x sqnt)  (List.rev (!clst)), 
	      constants inf)
	 ] g)
    ] goal
and 

(** 
   [concl_elim_rules ?info (arules, crules) f goal]: Apply elimination
   rules to conclusion [f] and to all resulting assumptions and
   conclusions. Assumptions are eliminated with [arules], conclusions
   with [crules]. The tag of any new formula for which the elimination
   rules fails is stored in [?info] (in arbitrary order).
 *)
    concl_elim_rules_tac ?info rules lbl goal=
  let (_, crules) = rules
  and inf = mk_info()
  and alst = ref []
  and clst = ref []
  in 
  let set_info dst (sgs, afs, cfs, cnsts) = 
    Logic.add_info dst sgs afs cfs cnsts
  in 
  seq
    [ 
      (** 
	 Try to elminate the operator.
       **)
      direct_alt crules inf lbl;
      (** 
	 Eliminate new assumptions and conclusions.
       **)
      (fun g -> 
	let albls = List.map ftag (aformulas inf)
	and clbls = List.map ftag (cformulas inf)
	and sqnt = sequent g
	in 
	seq
	  [
	   (** Eliminate conclusions, saving failing labels **)
	   alt
	     [ 
	       direct_map_some (concl_elim_rules_tac ?info rules) clst clbls;
	       skip
	     ];
	   (** Eliminate assumptions, saving failing labels **)
	   alt
	     [ 
	       direct_map_some (asm_elim_rules_tac ?info rules) alst albls;
	       skip
	     ];
	   (** Save failing labels and any other information **)
	   data_tac (set_info info)
	     (subgoals inf, 
	      List.map 
		(fun x -> Logic.label_to_tag x sqnt)  (List.rev (!alst)), 
	      List.map 
		(fun x -> Logic.label_to_tag x sqnt)  (List.rev (!clst)), 
	      constants inf)
	 ] g)
    ] goal


(**
   [elim_rules_tac ?info (arules, crules) albls clbls]: Apply
   elimination rules to all assumptions with a label in [albls] and
   all conclusions with a label in [clbls] and to all resulting
   assumptions and conclusions. The tag of any new formula for which
   the elimination rules fails is stored in [?info] (in arbitrary
   order).
 *)
let elim_rules_tac ?info rules albls clbls =
  match albls with 
    [] -> map_some (concl_elim_rules_tac ?info rules) clbls
  | _ ->
      let chng = ref false
      in 
      let tac g =
	seq
	  [
	   alt 
	     [
	      notify_tac (fun _ -> chng:=true) ()
		(map_some (asm_elim_rules_tac ?info rules) albls);
	      skip
	    ];
	   alt 
	     [ 
	       notify_tac (fun _ -> chng:=true) ()
		 (map_some (concl_elim_rules_tac ?info rules) clbls); 
	       skip 
	     ]
	 ] g
      in 
      restrict (fun _ -> !chng) tac

(**
   [apply_elim_tac tac ?info ?f]: Apply elimination tactic [tac] to
   formula [?f]. If [?f] is not given, use all formulas in the
   sequent. The tag of any new formula for which the elimination rules
   fails is stored in [?info] (in arbitrary order).

   [apply_elim_tac] is a wrapper for [elim_rules_tac].
 *)
let apply_elim_tac tac ?info ?f goal =
  let sqnt = sequent goal 
  in 
  let alst, clst = 
    match f with 
      None -> 
	(List.map (fun x -> ftag (drop_formula x)) (asms_of sqnt), 
	 List.map (fun x -> ftag (drop_formula x)) (concls_of sqnt))
    | Some(x) ->
	match Lib.try_find (get_asm x) goal with
	  None -> ([], [x])
	| _ -> ([x], [])
  in 
  tac ?info alst clst goal

(***
   Splitting formulas
 ***)

let split_asm_rules = 
  [
   (fun inf l -> falseA ~info:inf ~a:l); 
   (fun inf -> Logic.Tactics.disjA ~info:inf); 
   (fun inf -> Logic.Tactics.implA ~info:inf)
 ]

let split_concl_rules =
  [
   (fun inf -> Logic.Tactics.trueC ~info:inf); 
   (fun inf -> Logic.Tactics.conjC ~info:inf); 
(*   (fun inf c -> iffE ~info:inf ~c:c) *)
 ]


let split_asms_tac ?info lst = 
  asm_elim_rules_tac ?info (split_asm_rules, []) lst

let split_concls_tac ?info lst = 
  concl_elim_rules_tac ?info ([], split_concl_rules) lst

let splitter_tac ?info ?f goal =
  let basic_splitter ?info = 
    elim_rules_tac ?info (split_asm_rules, split_concl_rules)
  in 
  apply_elim_tac basic_splitter ?info ?f goal

let split_tac = splitter_tac 

(***
   Flattening formulas.
 ***)

let flatter_asm_rules =
  [
   (fun inf l -> falseA ~info:inf ~a:l);
   (fun inf -> Logic.Tactics.negA ~info:inf);
   (fun inf -> Logic.Tactics.conjA ~info:inf);
   (fun inf -> Logic.Tactics.existA ~info:inf)
 ]

let flatter_concl_rules =
  [
   (fun inf -> Logic.Tactics.trueC ~info:inf);
   (fun inf -> Logic.Tactics.negC ~info:inf);
   (fun inf -> Logic.Tactics.disjC ~info:inf);
   (fun inf -> Logic.Tactics.implC ~info:inf);
   (fun inf -> Logic.Tactics.allC ~info:inf)
 ]

let flatter_asms_tac ?info lst = 
  asm_elim_rules_tac ?info (flatter_asm_rules, []) lst

let flatter_concls_tac ?info lst = 
  concl_elim_rules_tac ?info ([], flatter_concl_rules) lst

let flatter_tac ?info ?f goal =
  let basic_flatter ?info =
    elim_rules_tac ?info (flatter_asm_rules, flatter_concl_rules)
  in 
  apply_elim_tac basic_flatter ?info ?f goal

let flatten_tac ?info ?f g = flatter_tac ?info:info ?f:f g

(***
   Scattering formulas
 ***)

let scatter_asm_rules =
  [
   (fun inf l -> falseA ~info:inf ~a:l); 

   (fun inf -> Logic.Tactics.negA ~info:inf);
   (fun inf -> Logic.Tactics.conjA ~info:inf);
   (fun inf -> Logic.Tactics.existA ~info:inf);

   (fun inf -> Logic.Tactics.disjA ~info:inf); 
   (fun inf -> Logic.Tactics.implA ~info:inf)
 ]

let scatter_concl_rules =
  [
   (fun inf -> Logic.Tactics.trueC ~info:inf);

   (fun inf -> Logic.Tactics.negC ~info:inf);
   (fun inf -> Logic.Tactics.disjC ~info:inf);
   (fun inf -> Logic.Tactics.implC ~info:inf);
   (fun inf -> Logic.Tactics.allC ~info:inf);

   (fun inf -> Logic.Tactics.conjC ~info:inf); 
   (fun inf c -> iffE ~info:inf ~c:c)
 ]

let scatter_tac ?info ?f goal =
  let tac ?info =
    elim_rules_tac ?info (scatter_asm_rules, scatter_concl_rules)
  in 
  apply_elim_tac tac ?info ?f goal


(***
   Scattering, solving formulas
 ***)

let blast_asm_rules =
  [
   (fun inf l -> falseA ~info:inf ~a:l); 

   (fun inf -> Logic.Tactics.negA ~info:inf);
   (fun inf -> Logic.Tactics.conjA ~info:inf);
   (fun inf -> Logic.Tactics.existA ~info:inf);

   (fun inf -> Logic.Tactics.disjA ~info:inf); 
   (fun inf -> Logic.Tactics.implA ~info:inf);

   (fun inf l -> basic ~info:inf ~a:l ?c:None)
 ]

let blast_concl_rules =
  [
   (fun inf -> Logic.Tactics.trueC ~info:inf);

   (fun inf -> Logic.Tactics.negC ~info:inf);
   (fun inf -> Logic.Tactics.disjC ~info:inf);
   (fun inf -> Logic.Tactics.implC ~info:inf);
   (fun inf -> Logic.Tactics.allC ~info:inf);

   (fun inf -> Logic.Tactics.conjC ~info:inf); 
   (fun inf c -> iffE ~info:inf ~c:c);

   (fun inf l -> basic ~info:inf ?a:None ~c:l)
 ]

let blast_tac ?info ?f goal =
  let tac ?info =
    elim_rules_tac ?info (blast_asm_rules, blast_concl_rules)
  in 
  apply_elim_tac tac ?info ?f goal



(*
   let inst_asm_rule i l sqnt=
   let rec rule ys sqs = 
   match ys with 
   [] -> sqs
   | (x::xs) -> 
   let nsqnt=
   Tactics.foreach (Logic.Tactics.allA None x i) sqs
   in rule xs nsqnt
   in rule l (skip sqnt)

   let inst_asm ?a l g=
   let af = first_asm_label a Formula.is_all g
   in 
   inst_asm_rule af l g

   let inst_concl_rule i l sqnt=
   let rec rule ys sqs = 
   match ys with 
   [] -> sqs
   | (x::xs) -> 
   let nsqnt=
   Tactics.foreach (Logic.Tactics.existC None x i) sqs
   in rule xs nsqnt
   in rule l (skip sqnt)

   let inst_concl ?c l g=
   let cf = first_concl_label c Formula.is_exists g
   in 
   inst_concl_rule cf l g

   let inst_tac ?f l g= 
   let sqnt = Tactics.sequent g
   in 
   try inst_asm ?a:f l g
   with _ -> inst_concl ?c:f l g
 *)

(***
 * Cases
 ***)

(**
   [cases_tac x sq]

   Adds formula x to assumptions of sq, 
   creates new subgoal in which to prove x.

   {L
   g:\[asms |- concls\]

   ---> 

   g1:\[asms |- x{_ l}, concls\]; g2:\[x{_ l}, asms |- concls\]
   }

   info: [goals = [g1; g2], aforms=[l], cforms=[l], terms = []]
 *)
let make_cases_tac_thm ()= 
  (Commands.get_or_prove "Bool.cases_thm"
     <<!P: (not P) or P>>
   (allC ++ disjC ++ negC ++ basic))

let cases_thm_var = Lib.freeze make_cases_tac_thm
let cases_thm () =  Lib.thaw ~fresh:fresh_thm cases_thm_var

let set_info dst (sgs, afs, cfs, cnsts) = 
  Logic.add_info dst sgs afs cfs cnsts

let cases_tac ?info (t:Basic.term)= 
  let thm = cases_thm()
  and inf1=Tactics.mk_info()
  in 
  seq 
    [
     cut ~info:inf1 thm;
     (fun g -> 
       let thm_tag = get_one ~msg:"cases_tac 1" (aformulas inf1)
       in 
       empty_info inf1; 
       allA ~info:inf1 t ~a:(ftag thm_tag) g);
     (fun g -> 
       let thm_tag = get_one ~msg:"cases_tac 2" (aformulas inf1)
       in 
       empty_info inf1; 
       disjA ~info:inf1 ~a:(ftag thm_tag) g)
       --
       [
	(fun g ->
	  let asm_tag = get_one ~msg:"cases_tac 3" (aformulas inf1)
	  and lgoal, rgoal = get_two ~msg:"cases_tac 4" (subgoals inf1)
	  in 
	  empty_info inf1;
	  seq
	    [
	     negA ~info:inf1 ~a:(ftag asm_tag);
	     (fun g1 -> 
	       let nasm_tag = get_one ~msg:"cases_tac 5" (cformulas inf1)
	       in 
	       data_tac (set_info info)
		 ([lgoal; rgoal], [nasm_tag], [nasm_tag], []) g1);
	   ] g);
	skip
      ]
   ]

let show_tac ?info (trm:Basic.term) tac= 
  let thm = cases_thm()
  and inf1=Tactics.mk_info()
  in 
  seq 
    [
     cut ~info:inf1 thm;
     (fun g -> 
       let thm_tag = get_one ~msg:"show_tac 1" (aformulas inf1)
       in 
       empty_info inf1; 
       allA ~info:inf1 trm ~a:(ftag thm_tag) g);
     (fun g -> 
       let thm_tag = get_one ~msg:"show_tac 2" (aformulas inf1)
       in 
       empty_info inf1; 
       disjA ~info:inf1 ~a:(ftag thm_tag) g)
       --
       [
	(fun g ->
	  let asm_tag = get_one ~msg:"show_tac 3" (aformulas inf1)
	  in 
	  seq [ negA ~a:(ftag asm_tag); tac ] g);
	(fun g -> 
	  let (_, gl_tag) = get_two (subgoals inf1)
	  and asm_tag = get_one (aformulas inf1)
	  in 
	  data_tac (set_info info) ([gl_tag], [asm_tag], [], []) g)
      ]
   ]

let show = show_tac



(**
   [cases_of ?info ?thm trm]: Try to introduce a case split based on
   the type of term [trm]. If [thm] is given, it is used as the cases
   theorem. If [thm] is not given, the theorem named ["T_cases"] is
   used, where [T] is the name of the type of [trm].
 *)

(** 
   [disj_splitter_tac ?info ?f]: 
   Split an assumption using disjA
 *)

let disj_splitter_tac ?info ?f goal = 
  let tac ?info =
    elim_rules_tac ?info
      ([ (fun inf1 -> Logic.Tactics.disjA ~info:inf1) ], []) 
  in 
  apply_elim_tac tac ?info ?f goal
    
let get_type_name ty =
  match ty with
    Basic.Constr (id, _) -> id
  | _ -> failwith "get_type_name"

let cases_of ?info ?thm t g =
  let scp = Tactics.scope_of g
  and tyenv = Tactics.typenv_of g
  in 
(*
  let trm = Global.PP.expand_term scp t
  in 
*)
  let trm = Lterm.set_names scp t
  in 
  let case_thm = 
    match thm with
      Some x -> x
    | _ -> 
	let ty = 
	  let sb = Typing.settype scp ~env:tyenv trm
	  in Gtypes.mgu (Typing.typeof scp ~env:tyenv trm) sb
	in
	let (th, id) = Ident.dest (get_type_name ty)
	in 
	let thm_name = id^"_cases"
	in 
	try 
	  Commands.thm (Ident.string_of (Ident.mk_long th thm_name))
	with 
	  _ ->
	    try Commands.thm thm_name
	    with _ -> 
	      failwith ("Can't find cases theorem "^thm_name)
  in 
  let inf = Tactics.mk_info()
  in 
  let inf1 = 
    match info with 
      None -> None | _ -> Some(Tactics.mk_info())
  in 
  let tac1 g1 = 
    seq 
      [
       cut ~info:inf ~inst:[trm] case_thm;
       (fun g -> 
	 let a_tg = get_one (aformulas inf)
	 in 
	 (data_tac (set_info info) ([a_tg], [], [], [])
	    ++ (disj_splitter_tac ?info:info ~f:(ftag a_tg) // skip)
	    ++ (specA ?info:info ~a:(ftag a_tg) // skip)) g)
     ] g1
  in 
  let tac2 g2 = 
    match inf1 with
      None -> skip g2
    | Some inf2 -> 
	data_tac 
	  (set_info info) ([], [], (subgoals inf2), (constants inf2)) g2
  in 
  try
    (tac1 ++ tac2) g
  with err -> raise (add_error "cases_of" err)
      

(***
 * Modus Ponens
 ***)

let mp0_tac ?info a a1lbls g=
  let typenv = Tactics.typenv_of g
  and sqnt = Tactics.sequent g
  in 
  let scp = Logic.Sequent.scope_of sqnt
  in 
  let (a_label, mp_vars, mp_form) = 
    try
      find_qnt_opt Basic.All Lterm.is_implies [get_tagged_asm a g] 
    with Not_found -> 
      raise (error "mp_tac: No implications in assumptions")
  and a1_forms = 
    try Lib.map_find (fun x -> get_tagged_asm x g) a1lbls
    with Not_found -> raise (error "mp_tac: No such assumption") 
  in
  let (_, mp_lhs, mp_rhs) = Term.dest_binop mp_form
  in 
  let varp = Rewrite.is_free_binder mp_vars
  in 
  let (a1_label, a1_env)= 
    let exclude (t, _) = (Tag.equal t a_label)
    in
    try 
      (find_unifier scp typenv varp mp_lhs 
	 ~exclude:exclude a1_forms)
    with 
      Not_found -> 
	raise 
	  (Term.term_error ("mp_tac: no matching formula in assumptions") 
	     [Term.mk_fun Lterm.impliesid [mp_lhs; mp_rhs]])
  in 
  let inf1= Tactics.mk_info()
  in 
  let tac1=
    match mp_vars with
      [] -> (* No quantifier *)
	skip
    | _ -> (* Implication has quantifier *)
	instA ~a:(ftag a_label)
	  (Tactics.extract_consts mp_vars a1_env)
  and tac2 g2= Logic.Tactics.implA ~info:inf1 (ftag a_label) g2
  and tac3 g3 =
    ((fun n -> 
      (Lib.apply_nth 0 (Tag.equal (Tactics.node_tag n)) 
	 (Tactics.subgoals inf1) false))
       --> 
	 Logic.Tactics.basic ~info:inf1 (ftag a1_label)
	   (ftag (Lib.get_one (Tactics.cformulas inf1) 
		    (Failure "mp_tac2.2")))) g3
  and tac4 g4 = 
    data_tac (set_info info) ([], aformulas inf1, [], []) g4
  in 
  (tac1++ (tac2 ++ tac3 ++ tac4)) g 
   
let mp_tac ?info ?a ?h goal =
  let sqnt = sequent goal
  in 
  let albls = 
    match a with
	None -> List.map (ftag <+ drop_formula) (asms_of sqnt)
      | Some(x) -> [x]
  and hlbls =
    match h with
	None -> List.map (ftag <+ drop_formula) (asms_of sqnt)
      | Some(x) -> [x]
  in 
  let tac g = 
    map_first (fun x -> mp0_tac ?info x hlbls) albls g
  in 
    try tac goal
    with err -> raise (error "mp_tac: Failed")
   

(**
   [cut_mp_tac ?info thm ?a]

   Apply modus ponens to theorem [thm] and assumption [a].
   [thm] must be a (possibly quantified) implication [!x1 .. xn: l=>r]
   and [a] must be [l].

   If [a] is not given, finds a suitable assumption to unify with [l].

   info [] [thm_tag] [] []
   where tag [thm_tag] identifies the theorem in the sequent.
 *)
let cut_mp_tac ?info ?inst thm ?a g=
  let info1 = Tactics.mk_info()
  and f_label = 
    Lib.apply_option 
      (fun x -> Some (ftag (Logic.label_to_tag x (Tactics.sequent g))))
      a None
  in 
  let tac1 = Tactics.cut ~info:info1 ?inst:inst thm
  in 
  let tac2 g2 = 
    (let a_tag = 
      Lib.get_one (Tactics.aformulas info1) 
	(Logic.logic_error "cut_mp_tac: Failed to cut theorem" 
	   [Logic.formula_of thm])
    in 
    mp_tac ?info:info ~a:(ftag a_tag) ?h:f_label g2)
  in 
  (tac1++tac2) g
    

(**
   [back_tac]: Backward match tactic. [back0_tac] is the main engine.

   info [g_tag] [] [c_tag] []
   where 
   [g_tag] is the new goal
   [c_tag] identifies the new conclusion.
 *)
let back0_tac ?info a cs goal=
  let typenv = Tactics.typenv_of goal
  and sqnt = Tactics.sequent goal
  in 
  let scp = Logic.Sequent.scope_of sqnt
  in 
  let (a_label, back_vars, back_form) = 
    try
      find_qnt_opt Basic.All Lterm.is_implies [get_tagged_asm a goal] 
    with Not_found -> raise (error "back_tac: No assumption")
  and c_forms = 
    try Lib.map_find (fun x -> get_tagged_concl x goal) cs
    with Not_found -> raise (error "back_tac: No such conclusion") 
  in
  let (_, back_lhs, back_rhs) = Term.dest_binop back_form
  in 
  let varp = Rewrite.is_free_binder back_vars
  in 
  (* find, get the conclusion and substitution *)
  let (c_label, c_env)= 
    let exclude (t, _) = (Tag.equal t a_label)
    in 
    try 
      find_unifier scp typenv varp back_rhs 
	~exclude:exclude c_forms
    with 
      Not_found -> 
	raise (Term.term_error 
		 ("back_tac: no matching formula in conclusion") 
		 [Term.mk_fun Lterm.impliesid [back_lhs; back_rhs]])
  in 
  let info1= Tactics.mk_info()
  in 
  let tac1=
    match back_vars with
      [] -> (* No quantifier *)
	skip
    | _ -> (* Implication has quantifier *)
	instA ~a:(ftag a_label)
	  (Tactics.extract_consts back_vars c_env)
  and tac2 g2= Logic.Tactics.implA ~info:info1 (ftag a_label) g2
  and tac3 g3 =
    ((fun n -> 
      (Lib.apply_nth 1 (Tag.equal (Tactics.node_tag n)) 
	 (Tactics.subgoals info1) false))
       --> 
	 Logic.Tactics.basic ~info:info1 
	   (ftag (Lib.get_nth (Tactics.aformulas info1) 1))
	   (ftag c_label)) g3
  in 
  let tac4 g4 =
    delete (ftag c_label) g4
  in 
  let tac5 g5 = 
    data_tac (set_info info)
      ([Lib.get_nth (Tactics.subgoals info1) 0],
       [], 
       [Lib.get_nth (Tactics.cformulas info1) 0],
       []) g5
  in 
  (tac1++ (tac2 ++ tac3 ++ tac4 ++ tac5)) goal

let back_tac ?info ?a ?c goal =
  let sqnt = sequent goal
  in 
  let alabels = 
    match a with
      None -> List.map (ftag <+ drop_formula) (asms_of sqnt)
      | Some x -> [x]
  and clabels = 
    match c with
      None -> List.map (ftag <+ drop_formula) (concls_of sqnt)
    | Some(x) -> [x]
  in
  let tac g = 
    map_first (fun x -> back0_tac ?info x clabels) alabels g
  in 
    try tac goal
    with err -> raise (error "back_tac: Failed")


let cut_back_tac ?info ?inst thm ?c g=
  let info1 = Tactics.mk_info()
  and c_label = 
    Lib.apply_option 
      (fun x -> Some (ftag (Logic.label_to_tag x (Tactics.sequent g))))
      c None
  in 
  let tac1 = Tactics.cut ~info:info1 ?inst:inst thm
  in 
  let tac2 g2 = 
    (let a_tag = 
      Lib.get_one (Tactics.aformulas info1) 
	(Logic.logic_error "cut_back_tac: Failed to cut theorem" 
	   [Logic.formula_of thm])
    in 
    back_tac ?info:info ~a:(ftag a_tag) ?c:c_label) g2
  in 
  (tac1++tac2) g


module Thms =
  struct
(** 
   {5 Theorems}

   Theorems about boolean operators which may be needed by tactics.
 *)


(**
   [make_n_thm()]: prove theorem n
   [n_thm()]: get theorem n, proving it if necessary
 *)

(**
   [iff_equals_thm]:  |- !x y: (x iff y) = (x = y)
 *)
    let make_iff_equals_thm ()=
      let iff_l2 = 
	let info = Tactics.mk_info()
	in 
	Commands.prove
	  <<!x y: ((x => y) and (y => x)) => (x=y)>>
	(allC ~info:info 
	   ++ allC ~info:info 
	   ++ 
	   (fun g -> 
	     let y_term, x_term = 
	       Lib.get_two (Tactics.constants info) 
		 (Failure "make_iff_equals_thm")
	     in 
	     (flatten_tac
		++ (cut_thm "bool_cases" ++ allA x_term)
		++ (cut_thm "bool_cases" ++ allA y_term)
		++ split_tac 
		++ 
		alt 
		[(replace_tac ++ (basic // trivial));
		 (basic // trivial);
		 (replace_tac ++ eq_tac)]) g))
      in 
      let info = Tactics.mk_info()
      in 
      Commands.prove <<!x y: (x iff y) = (x = y)>>
      (allC ~info ++ allC ~info
	 ++ 
	 (fun g -> 
	   let y_term, x_term = 
	     Lib.get_two (Tactics.constants info) 
	       (Failure "make_iff_equals_thm")
	   in 
	   ((cut iff_l2)
	      ++ inst_tac [Lterm.mk_iff x_term y_term;
			   Lterm.mk_equality x_term y_term]
	      ++ split_tac
	      --
	      [flatten_tac
		 ++ cut iff_l2 ++ inst_tac [x_term; y_term]
		     ++ unfold "iff" ~f:(!~2)
		     ++ (implA --  [basic; basic]);
	       flatten_tac
		 ++ replace_tac
		 ++ unfold "iff" ~f:(!! 1)
		 ++ split_tac ++ flatten_tac ++ basic;
	       replace_tac ++ eq_tac]) g))

    let iff_equals_thm_var = Lib.freeze make_iff_equals_thm
    let iff_equals_thm() = Lib.thaw ~fresh:fresh_thm iff_equals_thm_var

(**
   [equals_iff_thm]:  |- !x y: (x = y) = (x iff y)
 *)
    let make_equals_iff_thm ()=
      get_or_prove "Bool.equals_bool"
	<< !x y: (x = y) = (x iff y) >>
      (flatten_tac 
	 ++ (rewrite_tac [iff_equals_thm()])
	 ++ eq_tac)

    let equals_iff_thm_var = Lib.freeze make_equals_iff_thm
    let equals_iff_thm() = Lib.thaw ~fresh:fresh_thm equals_iff_thm_var

(**
   [bool_eq_thm]: |- !x y: x = y = ((x => y) and (y=>x))
 *)
    let make_bool_eq_thm ()= 
      prove << !x y: (x=y) = ((x => y) and (y => x)) >>
      (flatten_tac 
	 ++ rewrite_tac [equals_iff_thm()]
	 ++ unfold "iff"
	 ++ (split_tac ++ flatten_tac ++ split_tac ++ flatten_tac ++ basic))

    let bool_eq_thm_var=  Lib.freeze make_bool_eq_thm
    let bool_eq_thm ()=  Lib.thaw ~fresh:fresh_thm bool_eq_thm_var

(**
   [double_not_thm]: |- ! x: x = (not (not x))
 *)
    let make_double_not_thm () = 
      Commands.prove << !x: x=(not (not x)) >> 
      (flatten_tac ++ rewrite_tac [bool_eq_thm()]
	 ++ split_tac ++ flatten_tac ++ basic)

    let double_not_thm_var = Lib.freeze make_double_not_thm
    let double_not_thm () = Lib.thaw ~fresh:fresh_thm double_not_thm_var

(**
   [rule_true_thm]:  |- !x: x = (x=true) 
 *)

    let make_rule_true_thm ()= 
      let rule_true_l1 =  
	Commands.prove <<!x: (x=true) => x>> 
	(flatten_tac ++ replace_tac ++ trivial)
      in
      let rule_true_l2 = 
	let info = Tactics.mk_info()
	in 
	Commands.prove <<!x: x => (x=true)>>
	(allC ~info:info
	   ++ 
	   (fun g -> 
	     let x_term = 
	       Lib.get_one (Tactics.constants info) 
		 (Failure "rule_true_l2")
	     in 
	     (flatten_tac 
		++ (cut_thm "bool_cases") 
		++ (allA x_term) 
		++ disjA
		-- 
		[basic;
		 rewrite_tac [Commands.thm "false_def"]
		   ++ replace_tac ++ flatten_tac]) g))
      in
      let rule_true_l3 = 
	Commands.prove <<! x: x iff (x=true)>>
	  ((flatten_tac ++ unfold "iff" ~f:(!! 1) ++ conjC)
	     --
	     [cut rule_true_l2 ++ unify_tac ~a:(!~1) ~c:(!! 1); 
	      cut rule_true_l1 ++ unify_tac ~a:(!~1) ~c:(!! 1)])
      in 
      rewrite_rule (Global.scope()) [iff_equals_thm()] rule_true_l3

    let rule_true_thm_var = Lib.freeze make_rule_true_thm
    let rule_true_thm () = Lib.thaw ~fresh:fresh_thm rule_true_thm_var

(**
   rule_false_thm: !x: (not x) = (x=false)
 *)
    let make_rule_false_thm ()= 
      let info = Tactics.mk_info()
      in 
      Commands.prove <<! x : (not x)=(x=false)>>
      (allC ~info:info
	 ++
	 (fun g -> 
	   let x_term = 
	     Lib.get_one (Tactics.constants info)
	       (Failure "make_rule_false_thm")
	   in 
	   ((once_rewrite_tac [equals_iff_thm()]
	       ++ unfold "iff"
	       ++ split_tac ++ flatten_tac)
	      -- 
	      [
	       cut_thm "bool_cases" ++ inst_tac [x_term]
		 ++
		 (split_tac 
		    ++ replace_tac 
		    ++ (trivial // eq_tac));
	       replace_tac ++ trivial]) g))


    let rule_false_thm_var = Lib.freeze make_rule_false_thm
    let rule_false_thm () = Lib.thaw ~fresh:fresh_thm rule_false_thm_var


(**
   eq_sym_thm: !x y: (x = y) = (y = x)
 *)
    let make_eq_sym_thm ()= 
      Commands.prove << !x y: (x = y) = (y = x) >>
      (allC ++ allC 
	 ++ (once_rewrite_tac [equals_iff_thm()])
	 ++ iffE
	     ++ replace_tac ++ eq_tac);;

let eq_sym_thm_var = Lib.freeze make_eq_sym_thm
let eq_sym_thm () = Lib.thaw ~fresh:fresh_thm eq_sym_thm_var

end


(** 
   Functions to construct theorems from other theorems.
 *)
module Rules=
  struct

(** 
   [once_rewrite_rule scp rules thm]: 
   rewrite [thm] with [rules] once.
 *)
    let once_rewrite_rule scp rules thm =
      let ctrl = {Formula.default_rr_control with Rewrite.depth=Some(1)}
      in 
      rewrite_rule scp ~ctrl:ctrl rules thm

(**
   [conjunctL scp thm]
   Get the left hand side of conjunct [thm].
   [conjunctL scp << l and r >> = l]
 *)
    let conjunctL scp thm = 
      let trm = Logic.term_of thm
      in 
      if not (Lterm.is_conj trm)
      then raise (error "conjunct1: not a conjunction")
      else 
	let (_, lhs, rhs) = Term.dest_binop trm
	in 
	let info = Tactics.mk_info()
	in 
	let proof l g =
	  seq [Logic.Tactics.cut ~info:info thm;
	       (fun g1 -> 
		 let ttag = 
		   Lib.get_one (Tactics.aformulas info) 
		     (error "conjunctL")
		 in 
		 Tactics.empty_info info;
		 Logic.Tactics.conjA ~info:info (ftag ttag) g1);
	       (fun g1 -> 
		 let (ltag, rtag)=
		   Lib.get_two (Tactics.aformulas info) 
		     (error "conjunctL")
		 in 
		 Logic.Tactics.basic (ftag ltag) l g1)] g
	in 
	Commands.prove ~scp:scp lhs (proof (fnum 1))

	  
(**
   [conjunctR scp thm]
   Get the right hand side of conjunct [thm].
   [conjunctL scp << l and r >> = r]
 *)
    let conjunctR scp thm = 
      let trm = Logic.term_of thm
      in 
      if not (Lterm.is_conj trm)
      then raise (error "conjunct1: not a conjunction")
      else 
	let (_, lhs, rhs) = Term.dest_binop trm
	in 
	let info = Tactics.mk_info()
	in 
	let proof l g =
	  seq [Logic.Tactics.cut ~info:info thm;
	       (fun g1 -> 
		 let ttag = 
		   Lib.get_one (Tactics.aformulas info) 
		     (error "conjunctL")
		 in 
		 Tactics.empty_info info;
		 Logic.Tactics.conjA ~info:info (ftag ttag) g1);
	       (fun g1 -> 
		 let (ltag, rtag)=
		   Lib.get_two (Tactics.aformulas info) 
		     (error "conjunctL")
		 in 
		 Logic.Tactics.basic (ftag rtag) l g1)] g
	in 
	Commands.prove ~scp:scp rhs (proof (fnum 1))

(**
   [conjuncts scp thm]
   break theorem [thm] into the list of conjuncts.
   [conjuncts scp << f1 and f2 and .. and fn>> = [f1; f2; ..; fn]]
 *)
    let conjuncts scp thm =
      let is_conj_thm thm = 
	Lterm.is_conj (Logic.term_of thm)
      in 
      let rec conjuncts_aux scp thm result = 
	if not(is_conj_thm thm)
	then thm::result
	else 
	  let lhs = conjunctL scp thm 
	  and rhs = conjunctR scp thm 
	  in 
	  let result1=conjuncts_aux scp rhs result
	  in 
	  conjuncts_aux scp lhs result1
      in 
      conjuncts_aux scp thm []

  end

(** 
   Conversions on boolean operators.
 *)
module Convs=
  struct

    open Thms

(** 
   [neg_all_conv]: |- (not (!x..y: a)) = ?x..y: not a 
 *)
    let neg_all_conv scp trm=
      if(not (Lterm.is_neg trm))
      then failwith "neg_all_conv: not a negation"
      else 
	let (_, trmbody) = Term.dest_unop trm
	in 
	let (aqvars, aqbody) = Term.strip_qnt Basic.All trmbody
	in 
	(match aqvars with 
	  [] -> 
	    failwith 
	      "neg_all_conv: body of negation is not universally quantified"
	| _ -> ());
	let eqvars = 
	  List.map 
	    (fun b ->
	      let (_, n, ty) = Basic.dest_binding b
	      in Basic.mk_binding Basic.Ex n ty) 
	    aqvars
	in 
	let eqbody =
	  let nsubst = 
	    List.fold_left2 
	      (fun s l r -> Term.bind l r s)
	      (Term.empty_subst())
	      (List.map Term.mk_bound aqvars)
	      (List.map Term.mk_bound eqvars)
	  in 
	  Term.subst nsubst aqbody
	in 
	let newterm= 
	  Term.rename 
	    (Term.rebuild_qnt eqvars (Lterm.mk_not eqbody))
	in 
	let goal_term = 
	  Lterm.mk_equality trm newterm
	in 
	let info = Tactics.mk_info()
	in
	let proof g= 
	  seq [once_rewrite_tac [bool_eq_thm()] ~f:(fnum 1);
	       Logic.Tactics.conjC (fnum 1)
		 --
		 [
		  seq 
		    [Logic.Tactics.implC ~info:info (fnum 1);
		     (fun g1 ->
		       let atag = Lib.get_one (Tactics.aformulas info)
			   (Failure "neg_all_conv: 1")
		       and ctag = 
			 Lib.get_one (Tactics.cformulas info) 
			   (Failure "neg_all_conv: 1")
		       in 
		       Tactics.empty_info info;
		       seq
			 [
			  Logic.Tactics.negA ~info:info (ftag atag);
			  (fun g2-> 
			    let ctag2 = 
			      Lib.get_one (Tactics.cformulas info)
				(Failure "neg_all_conv: 2")
			    in 
			    Tactics.empty_info info;
			    seq
			      [repeat (Logic.Tactics.allC 
					 ~info:info (ftag ctag2));
			       (fun g3 -> 
				 instC ~c:(ftag ctag)
				   (List.rev (Tactics.constants info)) g3);
			       data_tac 
				 (fun () -> Tactics.empty_info info) ();
			       Logic.Tactics.negC ~info:info (ftag ctag);
			       (fun g3 ->
				 let atag3 = 
				   Lib.get_one (Tactics.aformulas info)
				     (Failure "neg_all_conv: 3")
				 in 
				 Tactics.empty_info info;
				 Logic.Tactics.basic 
				   (ftag atag3) (ftag ctag2) g3)
			     ] g2)] g1)];
		  
		  seq 
		    [Logic.Tactics.implC ~info:info (fnum 1);
		     (fun g1 ->
		       let atag = Lib.get_one (Tactics.aformulas info)
			   (Failure "neg_all_conv: 4")
		       and ctag = 
			 Lib.get_one (Tactics.cformulas info) 
			   (Failure "neg_all_conv: 4")
		       in 
		       Tactics.empty_info info;
		       seq
			 [
			  Logic.Tactics.negC ~info:info (ftag ctag);
			  (fun g2-> 
			    let atag2 = 
			      Lib.get_one (Tactics.aformulas info)
				(Failure "neg_all_conv: 2")
			    in 
			    Tactics.empty_info info;
			    seq
			      [repeat (Logic.Tactics.existA 
					 ~info:info (ftag atag));
			       (fun g3 -> 
				 instA ~a:(ftag atag2)
				   (List.rev (Tactics.constants info)) g3);
			       data_tac 
				 (fun () -> Tactics.empty_info info) ();
			       Logic.Tactics.negA ~info:info (ftag atag);
			       (fun g3 ->
				 let ctag3 = 
				   Lib.get_one (Tactics.cformulas info)
				     (Failure "neg_all_conv: 3")
				 in 
				 Logic.Tactics.basic 
				   (ftag atag2) (ftag ctag3) g3)
			     ] g2)] g1)]]
	     ] g
	in 
	Commands.prove ~scp:scp goal_term proof

(** 
   [neg_exists_conv]: |- (not (?x..y: a)) = !x..y: not a 
 *)
    let neg_exists_conv scp trm=
      if(not (Lterm.is_neg trm))
      then failwith "neg_exists_conv: not a negation"
      else 
	let (_, trmbody) = Term.dest_unop trm
	in 
	let (eqvars, eqbody) = Term.strip_qnt Basic.Ex trmbody
	in 
	(match eqvars with 
	  [] -> 
	    failwith 
	      "neg_all_conv: body of negation is not universally quantified"
	| _ -> ());
	let aqvars = 
	  List.map 
	    (fun b ->
	      let (_, n, ty) = Basic.dest_binding b
	      in Basic.mk_binding Basic.All n ty) 
	    eqvars
	in 
	let aqbody =
	  let nsubst = 
	    List.fold_left2 
	      (fun s l r -> Term.bind l r s)
	      (Term.empty_subst())
	      (List.map Term.mk_bound eqvars)
	      (List.map Term.mk_bound aqvars)
	  in 
	  Term.subst nsubst eqbody
	in 
	let newterm= 
	  Term.rename 
	    (Term.rebuild_qnt aqvars (Lterm.mk_not aqbody))
	in 
	let goal_term = 
	  Lterm.mk_equality trm newterm
	in 
	let info = Tactics.mk_info()
	in
	let proof g= 
	  seq [once_rewrite_tac [bool_eq_thm()] ~f:(fnum 1);
	       Logic.Tactics.conjC (fnum 1)
		 --
		 [
		  seq 
		    [Logic.Tactics.implC ~info:info (fnum 1);
		     (fun g1 ->
		       let atag =
			 Lib.get_one (Tactics.aformulas info)
			   (Failure "neg_exists_conv: 1")
		       and ctag = 
			 Lib.get_one (Tactics.cformulas info) 
			   (Failure "neg_exists_conv: 1")
		       in 
		       Tactics.empty_info info;
		       seq
			 [
			  Logic.Tactics.negA ~info:info (ftag atag);
			  (fun g2-> 
			    let ctag2 = 
			      Lib.get_one (Tactics.cformulas info)
				(Failure "neg_all_conv: 2")
			    in 
			    Tactics.empty_info info;
			    seq
			      [repeat (Logic.Tactics.allC 
					 ~info:info (ftag ctag));
			       (fun g3 -> 
				 instC ~c:(ftag ctag2)
				   (List.rev (Tactics.constants info)) g3);
			       data_tac 
				 (fun () -> Tactics.empty_info info) ();
			       Logic.Tactics.negC ~info:info (ftag ctag);
			       (fun g3 ->
				 let atag3 = 
				   Lib.get_one (Tactics.aformulas info)
				     (Failure "neg_exists_conv: 3")
				 in 
				 Tactics.empty_info info;
				 Logic.Tactics.basic 
				   (ftag atag3) (ftag ctag2) g3)
			     ] g2)] g1)];
		  
		  seq 
		    [Logic.Tactics.implC ~info:info (fnum 1);
		     (fun g1 ->
		       let atag = 
			 Lib.get_one (Tactics.aformulas info) 
			   (Failure "neg_exists_conv: 4")
		       and ctag = 
			 Lib.get_one (Tactics.cformulas info) 
			   (Failure "neg_exists_conv: 4")
		       in 
		       Tactics.empty_info info;
		       seq
			 [
			  Logic.Tactics.negC ~info:info (ftag ctag);
			  (fun g2-> 
			    let atag2 = 
			      Lib.get_one (Tactics.aformulas info)
				(Failure "neg_exists_conv: 2")
			    in 
			    Tactics.empty_info info;
			    seq
			      [repeat 
				 (Logic.Tactics.existA 
				    ~info:info (ftag atag2));
			       (fun g3 -> 
				 instA ~a:(ftag atag)
				   (List.rev (Tactics.constants info)) g3);
			       data_tac 
				 (fun () -> Tactics.empty_info info) ();
			       Logic.Tactics.negA ~info:info (ftag atag);
			       (fun g3 ->
				 let ctag3 = 
				   Lib.get_one (Tactics.cformulas info)
				     (Failure "neg_exists_conv: 3")
				 in 
				 Logic.Tactics.basic 
				   (ftag atag2) (ftag ctag3) g3)
			     ] g2)] g1)]]
	     ] g
	in 
	Commands.prove ~scp:scp goal_term proof

  end

(***
 * More tactics 
 ***)

let equals_tac ?info ?f g =
  let ff = 
    match f with
      Some x -> x
    | _ -> 
	try first_asm_label None Formula.is_equality g
	with 
	  Not_found ->
	    try first_concl_label None Formula.is_equality g
	    with Not_found ->
	      raise 
		(error "equals_tac: No equality term")
  in 
  let thm = 
    try Thms.equals_iff_thm()
    with Not_found -> 
      (raise (error "Can't find required lemma Bool.equals_bool"))
  in 
  once_rewrite_tac ?info [thm] ~f:ff g


(***
 *
 * Initialisation
 *
 ***)

let init_boollib()= 
(*  BaseTheory.init(); *)
  BoolPP.init()

let _ = Global.Init.add_init init_boollib


