
let ftag t = (Logic.FTag t)
let fnum n = (Logic.FNum n)

let asm_forms sq = 
  List.map snd (Logic.asms sq)

let concl_forms sq = 
  List.map snd (Logic.concls sq)

let get_asm i sq= snd(Logic.get_asm i sq)
let get_cncl i sq= snd(Logic.get_cncl i sq)

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


let find_basic sq = 
  let ams = Logic.asms sq
  and cncs = Logic.concls sq
  in 
  let rec find_basic_aux xs =
    match xs with
      [] -> raise Not_found
    | (t, c)::cs -> 
	try 
	  ((first 
	     (fun x-> Formula.alpha_convp 
		 (Logic.scope_of sq) x c) ams), 
	   ftag t)
	with Not_found -> find_basic_aux cs 
  in 
  find_basic_aux cncs

let basic goal=
  let sq=Logic.get_sqnt goal
  in 
  try
    let a,c = find_basic sq
    in Logic.Rules.assume None a c goal
  with Not_found -> raise (Result.error "Not basic")

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
	let (ft, fa)=Logic.get_asm (-i) (Logic.get_sqnt nsq)
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
	(let rl = find_rule (get_cncl i (Logic.get_sqnt nsq)) rs
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

let get_one ls err=
  match ls with
    x::_ -> x
  | _ -> raise err
	
let get_two ls err=
  match ls with
    x::y::_ -> (x, y)
  | _ -> raise err
