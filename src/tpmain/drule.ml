
let ftag t = (Logic.FTag t)
let fnum n = (Logic.FNum n)

let asm_forms sq = 
  List.map Logic.drop_tag (Logic.Sequent.asms sq)

let concl_forms sq = 
  List.map Logic.drop_tag (Logic.Sequent.concls sq)

let sequent g = Logic.Subgoals.node_sqnt g
let scope_of g = Logic.Sequent.scope_of (sequent g)
let typenv_of n = Logic.Subgoals.node_tyenv n

let get_asm i g= Logic.drop_tag(Logic.get_label_asm i (sequent g))
let get_cncl i g= Logic.drop_tag(Logic.get_label_cncl i (sequent g))

let has_subgoals b=
  match (Logic.Subgoals.branch_sqnts b) with
    [] -> false
  | _ -> true

let node_tag n = Logic.Sequent.sqnt_tag (sequent n)

let mk_info () = ref (Logic.make_tag_record[] [] [])
let empty_info info = (info:=(Logic.make_tag_record[] [] []); info)

let subgoals inf= (!inf).Logic.goals
let formulas inf = (!inf).Logic.forms
let constants inf = (!inf).Logic.terms
    
let skip = Logic.Rules.skip None
let foreach = Logic.Subgoals.apply_to_each 

(*
   [seq tac1 tac2 node]
   apply tactic [tac1] to [node] then [tac2] 
   to each of the resulting subgoals.

   if [tac1] solves the goal (no subgoals), then [tac2] is not used.
 *)
let seq tac1 tac2 node=
  let branch = tac1 node
  in 
  if (has_subgoals branch) 
  then foreach tac2 branch
  else branch

(** [make_consts qs env]: 
   Get values for each binder in [qs] from [env].
   stop when a quantifier has no (closed) value.
   all values must be closed
 *)

(*
let make_consts qs env = 
  let make_aux q=
    try 
      Term.find (Basic.Bound q) env
    with 
      Not_found -> Logicterm.mk_some
  in 
  List.map make_aux qs
*)
let make_consts vars env=
  let rec make_aux qs cnsts=
    match qs with 
      [] -> cnsts
    | (x::xs) -> 
	try 
	  let nv = Term.find (Basic.Bound x) env
	  in 
	  if(Formula.is_closed [] nv)
	  then make_aux xs (nv::cnsts)
	  else cnsts
	with 
	  Not_found -> cnsts
  in 
  List.rev (make_aux vars [])


(* 
   [inst_list rule cs id goal]: 
   instantiate formula [id] in [goal] with constants [cs]
   using tactic [rule].
   do nothing if [cs] is empty.
 *)
let inst_list rule cs id goal = 
  let rec inst_aux cnsts g=
    match cnsts with 
      []  -> g
    | (x::xs) -> (inst_aux xs) (Logic.Subgoals.apply_to_each (rule x id) g)
  in 
  inst_aux cs (skip goal)

(* utility functions for tactics *)

let first p xs =
  let rec first_aux ys=
    match ys with
      [] -> raise Not_found
    | (t, y)::yss -> if (p y) then t else first_aux yss 
  in 
  ftag(first_aux xs)

let first_asm p sq =
  (first p (Logic.Sequent.asms sq))

let first_concl p sq =
  (first p (Logic.Sequent.concls sq))

let rec find_rule t rs =
  match rs with 
    [] -> raise Not_found
  | ((p, r)::xs) -> 
      if p t then r else find_rule t xs

let foreach_asm rs sq = 
  let get_rule i n = 
    find_rule 
      (Logic.drop_tag (Logic.get_label_asm (fnum (-i)) (sequent n))) rs
  in 
  let chng = ref false
  in 
  let rec each_safe i node =
    if (List.length (asm_forms (sequent node)))>= i 
    then 
      (try
	(let rl = get_rule i node
	in 
        (let branch= (rl (fnum (-i))) node
  	in 
	(chng:=true; 
         if (has_subgoals branch)
         then Logic.foreach (each_safe i) branch
    	 else branch)))
      with Not_found -> (each_safe (i+1)) node)
    else (skip node)
  in 
  (let rslt = (each_safe 1) sq
  in 
  if !chng then rslt 
  else raise (Result.error "No change"))

let foreach_asm_except excpt rs sq = 
  let exclude t =
    try 
      ignore(List.find (fun x -> Tag.equal t x) excpt);
      true
    with Not_found -> false
  in 
  let chng = ref false
  in 
  let rec each_safe i nsq =
    if (List.length (asm_forms (sequent nsq)))>= i 
    then 
      (try
	let (ft, fa)=Logic.get_label_asm (fnum (-i)) (sequent nsq)
	in 
	if exclude ft
	then raise Not_found
	else 
	  (let rl = find_rule fa rs
	  in 
          (let branch= (rl (fnum (-i))) nsq
  	  in 
	  (chng:=true; 
           if (has_subgoals branch)
           then Logic.foreach (each_safe i) branch
    	   else branch)))
      with Not_found -> (each_safe (i+1)) nsq)
    else (skip nsq)
  in 
  (let rslt = (each_safe 1) sq
  in 
  if !chng then rslt 
  else raise (Result.error "No change"))


let foreach_conc rs sq = 
  let get_rule i n = 
    find_rule 
      (Logic.drop_tag (Logic.get_label_cncl (fnum i) (sequent n))) rs
  in 
  let chng = ref false
  in 
  let rec each_safe i node =
    if (List.length (concl_forms (sequent node)))>= i 
    then 
      (try
	(let rl = get_rule i node
	in 
        (let branch= (rl (fnum i)) node
  	in 
	(chng:=true; 
         if (has_subgoals branch)
         then Logic.foreach (each_safe i) branch
    	 else branch)))
      with Not_found -> (each_safe (i+1)) node)
    else (skip node)
  in 
  (let rslt = (each_safe 1) sq
  in 
  if !chng then rslt 
  else raise (Result.error "No change"))


let foreach_formula rs g=
  let chng = ref true
  in let g1=
    (try foreach_asm rs g
    with (Result.Error _) -> chng:=false; (skip g))
  in 
  (try
    Logic.foreach (foreach_conc rs) g1
  with 
    (Result.Error x) -> 
      if !chng then g1 else (raise (Result.Error x)))

let foreach_conc_except excpt rs sq = 
  let exclude t =
    try 
      ignore(List.find (fun x -> Tag.equal t x) excpt);
      true
    with Not_found -> false
  in 
  let chng=ref false
  in 
  let rec each_safe i nsq =
    if (List.length (Logic.Sequent.concls (sequent nsq)))>= i 
    then 
      try
	let (ft, fc)=Logic.get_label_cncl (fnum i) (sequent nsq)
	in 
	if exclude ft
	then raise Not_found
	else 
	  (let rl = find_rule fc rs
	  in 
	  (let branch= (rl (fnum i)) nsq
	  in 
	  chng:=true;
	  if (has_subgoals branch)
	  then Logic.foreach (each_safe i) branch
	  else branch))
      with 
	Not_found -> (each_safe (i+1) nsq)
    else (skip nsq)
  in
  (let rslt = (each_safe 1) sq
  in 
  if !chng then rslt else raise (Result.error "No change"))


let foreach_except excpt rs g=
  let chng = ref true
  in let b1=
    (try foreach_asm_except excpt rs g
    with (Result.Error _) -> chng:=false; (skip g))
  in 
  (try
    Logic.foreach (foreach_conc_except excpt rs) b1
  with 
    (Result.Error x) -> 
      if !chng then b1 else (raise (Result.Error x)))

let foreach_conc_once r sq =
  let chng = ref false
  in let rec each_once i sq =
    if (List.length (Logic.Sequent.concls (sequent sq)))>= i
    then 
      (try 
	(let rsl = r (fnum i) sq
	in 
	chng:=true; Logic.foreach (each_once (i+1)) rsl)
      with _ -> each_once (i+1) sq)
    else (skip sq)
  in let rslt = each_once 1 sq 
  in if !chng then rslt else raise (Result.error "No change")


let foreach_asm_once r sq =
  let chng = ref false
  in let rec each_once i sq =
    if (List.length (Logic.Sequent.asms (sequent sq)))>= i
    then 
      (try
	(let rsl = r (fnum (-i)) sq
	in 
	chng:=true; Logic.foreach (each_once (i+1)) rsl)
      with _ -> each_once (i+1) sq)
    else (skip sq)
  in let rslt = each_once 1 sq 
  in if !chng then rslt else raise (Result.error "No change")

let foreach_once r sq = 
  let chng = ref true
  in let sq1=
    (try foreach_asm_once r sq 
    with (Result.Error _) -> chng:=false; (skip sq))
  in 
  (try
    Logic.foreach (foreach_conc_once r) sq1
  with 
    (Result.Error x) -> 
      if !chng then sq1 else (raise (Result.Error x)))


(*
   Matching assumptions/conclusions against a given term.
   (Similar to HOL's PAT_ASSUM).
 *)

(**
   [match_formulas typenv scp varp t fs]

   Match a list of tagged formulas 
   Return the tag of the first formula in [fs] to unify 
   with term [t] in scope [scp].

   [varp] determines which terms can be bound by unification.
   [typenv] is the goals type environment.

   raise Not_found if no match.
*)
let match_formulas typenv scp varp t fs=
  let rec match_aux l = 
    match l with 
      [] -> raise Not_found
    | (tf::tfs) ->
	try 
	  let tg, f = tf
	  in
	  ignore(Unify.unify ~typenv:typenv scp 
		   varp t (Formula.dest_form f));
	  tg
	with _ -> match_aux tfs
  in
  Logic.FTag (match_aux fs)


(* [match_asm typenv t sq]
   Find a match for [t] in the assumptions of [sq].
   Return the tag of the first formula in the assumptions to unify 
   with term [t] in the scope of sequent [sq].
   [typenv] is the type environment of the goal (from [Logic.goal_tyenv])

   raise Not_found if no match.

   Only free variables are bound in the matching process.
   e.g. in [<< !x. y and x >>] only [y] is a bindable variable 
   for the match.
 *)
let match_asm typenv t sq=
  let scp = Logic.Sequent.scope_of sq
  and asms = Logic.Sequent.asms sq
  in 
  let t1 = Term.set_names scp t
  in let vars = Term.get_free_vars t1
  in let varp x = 
    try (ignore(List.find (Term.equals x) vars); true)
    with Not_found -> false
  in 
  match_formulas typenv scp varp t1 asms


(* [match_concl typenv t sq]
   Find a match for [t] in the assumptions of [sq].
   Return the tag of the first formula in the assumptions to unify 
   with term [t] in the scope of sequent [sq].
   [typenv] is the type environment of the goal (from [Logic.goal_tyenv])

   raise Not_found if no match.

   Only free variables are bound in the matching process.
   e.g. in [<< !x. y and x >>] only [y] is a bindable variable 
   for the match.
 *)
let match_concl typenv t sq=
  let scp = Logic.Sequent.scope_of sq
  and concls = Logic.Sequent.concls sq
  in let t1=Term.set_names scp t
  in let vars = Term.get_free_vars t1
  in let varp x = 
    try (ignore(List.find (Term.equals x) vars); true)
    with Not_found -> false
  in 
  match_formulas typenv scp varp t1 concls

let qnt_opt_of qnt p t=
  let (_, b) = Term.strip_qnt qnt t
  in 
  p b

let dest_qnt_opt qnt d t=
  let (vs, b) = Term.strip_qnt qnt t
  in 
  (vs, d b)
