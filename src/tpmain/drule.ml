let asm_forms sq = 
  List.map snd (Logic.asms sq)

let concl_forms sq = 
  List.map snd (Logic.concls sq)

let get_asm i sq= snd(Logic.get_asm i sq)
let get_cncl i sq= snd(Logic.get_cncl i sq)

let first p xs =
  let rec first_aux ys i=
    match ys with
      [] -> raise Not_found
    | y::yss -> if (p y) then i else first_aux yss (i+1)
  in (first_aux xs 1)

let first_asm p sq =
  -(first p (asm_forms sq))

let first_concl p sq =
  (first p (concl_forms sq))

let find_basic sq = 
  let ams = List.map snd (Logic.asms sq)
  and cncs = List.map snd (Logic.concls sq)
  in 
  let rec find_basic_aux xs i =
    match xs with
      [] -> raise Not_found
    | c::cs -> 
	try 
	  (-(first 
	       (fun x-> Formula.alpha_convp 
		   (Logic.scope_of sq) x c) ams), i)
	with Not_found -> find_basic_aux cs (i+1)
  in 
  find_basic_aux cncs 1

let basic sqnt = 
  let sq=Logic.get_sqnt sqnt
  in 
  try
    let a,c = find_basic sq
    in Logic.Rules.unify a c sqnt
  with Not_found -> Result.raiseError "Not basic"

let implI sq =
  let c=first_concl Formula.is_implies (Logic.get_sqnt sq)
  in Logic.Rules.implI c sq

let implE sq =
  let c=first_asm Formula.is_implies (Logic.get_sqnt sq)
  in Logic.Rules.implE c sq

let conjI sq =
  let c=first_concl Formula.is_conj (Logic.get_sqnt sq)
  in Logic.Rules.conjI c sq

let conjE sq =
  let c=first_asm Formula.is_conj (Logic.get_sqnt sq)
  in Logic.Rules.conjE c sq


let disjI sq =
  let c=first_asm Formula.is_disj (Logic.get_sqnt sq)
  in Logic.Rules.disjI c sq

let disjE sq =
  let c=first_concl Formula.is_disj (Logic.get_sqnt sq)
  in Logic.Rules.disjE c sq

let negC sq =
  let c=first_concl Formula.is_neg (Logic.get_sqnt sq)
  in Logic.Rules.negC c sq

let negA sq =
  let c=first_asm Formula.is_neg (Logic.get_sqnt sq)
  in Logic.Rules.negA c sq

let allI sq =
  let c=first_concl Formula.is_all (Logic.get_sqnt sq)
  in Logic.Rules.allI c sq

let existI sq =
  let c=first_asm Formula.is_exists (Logic.get_sqnt sq)
  in Logic.Rules.existI c sq


let mp_basic_rule i g= 
  let g0=Logic.Rules.implE i g
  in 
  (fun ng -> 
    (let sq=Logic.get_sqnt ng
    in 
    let c= get_cncl 1 sq
    in 
    (Logic.Rules.unify 
       (first_asm 
	  (Formula.alpha_convp (Logic.scope_of sq) c) sq) 1)) ng) g0

let mp_rule sq = 
  mp_basic_rule (first_asm Formula.is_implies (Logic.get_sqnt sq)) sq

let trueR sq =
  let c=first_concl Formula.is_true (Logic.get_sqnt sq)
  in Logic.Rules.trueR c sq

let existE trm sq =
  let c=first_concl Formula.is_exists (Logic.get_sqnt sq)
  in Logic.Rules.existE trm c sq

let allE trm sq =
  let c=first_asm Formula.is_all (Logic.get_sqnt sq)
  in Logic.Rules.allE trm c sq

let deleten ns sq = 
  let rec del_aux l s=
    match l with
      [] -> s
    | (x::xs) -> del_aux xs (Logic.Rules.delete x s)
  in del_aux ns sq

let leftright=false
let rightleft = true

let rewrite_thm ths dir i sq=
  let rec cut_thms ls sqs = 
    match ls with 
      [] -> sqs
    | x::xs -> cut_thms xs (Logic.Rules.cut x sqs)
  and mk_nums n l=
    match n with 
      0 -> l
    | _ -> mk_nums (n-1) ((-n)::l)
  in 
  let numths = List.length ths
  in 
  let j= if i<0 then (i-numths) else i
  in
  let sq0 = cut_thms ths sq
  and ns = mk_nums numths []
  in 
  let sq1= Logic.Rules.rewrite ~dir:dir ns j sq0
  in deleten ns sq1


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
	(let rl = find_rule 
	    (get_asm (-i) (Logic.get_sqnt nsq)) rs
	in 
        (let nsq0= (rl (-i)) nsq
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
  else Result.raiseError "No change")

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
	let (ft, fa)=Logic.get_asm (-i) (Logic.get_sqnt nsq)
	in 
	if exclude ft
	then raise Not_found
	else 
	  (let rl = find_rule fa rs
	  in 
          (let nsq0= (rl (-i)) nsq
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
  else Result.raiseError "No change")


let foreach_conc rs sq = 
  let chng=ref false
  in 
  let rec each_safe i nsq =
    if (List.length (Logic.concls (Logic.get_sqnt nsq)))>= i 
    then 
      try
	(let rl = find_rule (get_cncl i (Logic.get_sqnt nsq)) rs
	in 
	(let nsq0= (rl i) nsq
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
  if !chng then rslt else Result.raiseError "No change")


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
	let (ft, fc)=Logic.get_cncl i (Logic.get_sqnt nsq)
	in 
	if exclude ft
	then raise Not_found
	else 
	  (let rl = find_rule fc rs
	  in 
	  (let nsq0= (rl i) nsq
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
  if !chng then rslt else Result.raiseError "No change")


(*
let foreach_in_sq ars crs sq =
  thenl
    [Logic.Rules.orl [foreach_conc crs; Logic.Rules.skip]; 
     Logic.Rules.orl [foreach_asm ars; Logic.Rules.skip]] sq
*)

let foreach_conc_once r sq =
  let chng = ref false
  in let rec each_once i sq =
    if (List.length (Logic.concls (Logic.get_sqnt sq)))>= i
    then 
      (try 
	(let rsl = r i sq
	in 
	chng:=true; each_once (i+1) rsl)
      with _ -> sq)
    else sq
  in let rslt = each_once 1 sq 
  in if !chng then rslt else Result.raiseError "No change"


let foreach_asm_once r sq =
  let chng = ref false
  in let rec each_once i sq =
    if (List.length (Logic.asms (Logic.get_sqnt sq)))>= i
    then 
      (try
	(let rsl = r (-i) sq
	in 
	chng:=true; each_once (i+1) rsl)
      with _ -> sq)
    else sq
  in let rslt = each_once 1 sq 
  in if !chng then rslt else Result.raiseError "No change"

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


