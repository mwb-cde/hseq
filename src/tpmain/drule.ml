
let ftag t = (Logic.FTag t)
let fnum n = (Logic.FNum n)

let asm_forms sq = 
  List.map Logic.drop_tag (Logic.asms sq)

let concl_forms sq = 
  List.map Logic.drop_tag (Logic.concls sq)

let sequent g = Logic.get_sqnt g
let scope_of g = Logic.scope_of (sequent g)

let get_asm i g= Logic.drop_tag(Logic.get_label_asm i (sequent g))
let get_cncl i g= Logic.drop_tag(Logic.get_label_cncl i (sequent g))

let mk_info () = ref (Logic.make_tag_record[] [] [])
let empty_info info = (info:=(Logic.make_tag_record[] [] []); info)

let subgoals inf= (!inf).Logic.goals
let formulas inf = (!inf).Logic.forms
let constants inf = (!inf).Logic.terms
  

(** [make_consts qs env]: 
   Get values for each binder in [qs] from [env].
   stop when a quantifier has no (closed) value.
   all values must be closed
 *)

let make_consts qs env = 
  let make_aux q=
    try 
      Term.find (Basic.Bound q) env
    with 
      Not_found -> Logicterm.mksome
  in 
  List.map make_aux qs

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
*)
let inst_list rule cs id goal = 
  let rec inst_aux cnsts g=
    match cnsts with 
      []  -> g
    | (x::xs) -> 
	inst_aux xs (rule x id g)
  in 
  inst_aux cs goal


(* utility functions for tactics *)

let first p xs =
  let rec first_aux ys=
    match ys with
      [] -> raise Not_found
    | (t, y)::yss -> if (p y) then t else first_aux yss 
  in 
  ftag(first_aux xs)

let first_asm p sq =
  (first p (Logic.asms sq))

let first_concl p sq =
  (first p (Logic.concls sq))


let rec find_rule t rs =
  match rs with 
    [] -> raise Not_found
  |	((p, r)::xs) -> 
      if p t then r else find_rule t xs

let foreach_asm rs sq = 
  let chng = ref false
  in 
  let rec each_safe i nsq =
    if (List.length (asm_forms (Logic.get_sqnt nsq)))>= i 
    then 
      (try
	(let rl = 
	  find_rule 
	    (Logic.drop_tag
	       (Logic.get_label_asm (fnum (-i)) (Logic.get_sqnt nsq))) rs
	in 
        (let nsq0= (rl (fnum (-i))) nsq
  	in 
	(chng:=true; 
         if (Logic.has_subgoals nsq0)
         then (each_safe i) nsq0
    	 else nsq0)))
      with Not_found -> (each_safe (i+1)) nsq)
    else nsq
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
    if (List.length (asm_forms (Logic.get_sqnt nsq)))>= i 
    then 
      (try
	let (ft, fa)=Logic.get_label_asm (fnum (-i)) (Logic.get_sqnt nsq)
	in 
	if exclude ft
	then raise Not_found
	else 
	  (let rl = find_rule fa rs
	  in 
          (let nsq0= (rl (fnum (-i))) nsq
  	  in 
	  (chng:=true; 
           if (Logic.has_subgoals nsq0)
           then (each_safe i) nsq0
    	   else nsq0)))
      with Not_found -> (each_safe (i+1)) nsq)
    else nsq
  in 
  (let rslt = (each_safe 1) sq
  in 
  if !chng then rslt 
  else raise (Result.error "No change"))


let foreach_conc rs sq = 
  let chng=ref false
  in 
  let rec each_safe i nsq =
    if (List.length (Logic.concls (Logic.get_sqnt nsq)))>= i 
    then 
      try
	(let rl = 
	  find_rule 
	    (Logic.drop_tag
	       (Logic.get_label_cncl (fnum i) (Logic.get_sqnt nsq))) rs
	in 
	(let nsq0= (rl (fnum i)) nsq
	in 
	chng:=true;
        if (Logic.has_subgoals nsq0)
        then (each_safe i) nsq0
        else nsq0))
      with 
	Not_found -> (each_safe (i+1) nsq)
    else nsq
  in
  (let rslt = (each_safe 1) sq
  in 
  if !chng then rslt else raise (Result.error "No change"))

let foreach_formula rs g=
  let chng = ref true
  in let g1=
    (try foreach_asm rs g
    with (Result.Error _) -> chng:=false; g)
  in 
  (try
    (foreach_conc rs g1)
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
    if (List.length (Logic.concls (Logic.get_sqnt nsq)))>= i 
    then 
      try
	let (ft, fc)=Logic.get_label_cncl (fnum i) (Logic.get_sqnt nsq)
	in 
	if exclude ft
	then raise Not_found
	else 
	  (let rl = find_rule fc rs
	  in 
	  (let nsq0= (rl (fnum i)) nsq
	  in 
	  chng:=true;
	  if (Logic.has_subgoals nsq0)
	  then (each_safe i) nsq0
	  else nsq0))
      with 
	Not_found -> (each_safe (i+1) nsq)
    else nsq
  in
  (let rslt = (each_safe 1) sq
  in 
  if !chng then rslt else raise (Result.error "No change"))

let foreach_except excpt rs g=
  let chng = ref true
  in let g1=
    (try foreach_asm_except excpt rs g
    with (Result.Error _) -> chng:=false; g)
  in 
  (try
    (foreach_conc_except excpt rs g1)
  with 
    (Result.Error x) -> 
      if !chng then g1 else (raise (Result.Error x)))

let foreach_conc_once r sq =
  let chng = ref false
  in let rec each_once i sq =
    if (List.length (Logic.concls (Logic.get_sqnt sq)))>= i
    then 
      (try 
	(let rsl = r (fnum i) sq
	in 
	chng:=true; each_once (i+1) rsl)
      with _ -> sq)
    else sq
  in let rslt = each_once 1 sq 
  in if !chng then rslt else raise (Result.error "No change")


let foreach_asm_once r sq =
  let chng = ref false
  in let rec each_once i sq =
    if (List.length (Logic.asms (Logic.get_sqnt sq)))>= i
    then 
      (try
	(let rsl = r (fnum (-i)) sq
	in 
	chng:=true; each_once (i+1) rsl)
      with _ -> sq)
    else sq
  in let rslt = each_once 1 sq 
  in if !chng then rslt else raise (Result.error "No change")

let foreach_once r sq = 
  let chng = ref true
  in let sq1=
    (try foreach_asm_once r sq 
    with (Result.Error _) -> chng:=false;sq)
  in 
  (try
    (foreach_conc_once r sq1)
  with 
    (Result.Error x) -> 
      if !chng then sq1 else (raise (Result.Error x)))

let foreach_subgoal lst tac goal=
  let rec for_aux ls g= 
    match ls with
      [] -> g
    | (x::xs) -> 
	let fg= try (Some (Logic.goal_focus x g)) with Not_found -> None
	in 
	match fg with
	  None -> for_aux xs g
	| Some ng -> for_aux xs (tac ng)
  in for_aux lst goal
