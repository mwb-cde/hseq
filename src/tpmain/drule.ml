
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

(*
let first_asm p sq =
  -(first p (asm_forms sq))

let first_concl p sq =
  (first p (concl_forms sq))
*)

let first_asm p sq =
  (first p (Logic.asms sq))

let first_concl p sq =
  (first p (Logic.concls sq))


(*
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
*)

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

let implI sq =
  let c=first_concl Formula.is_implies (Logic.get_sqnt sq)
  in Logic.Rules.implI None c sq

let implE sq =
  let c=first_asm Formula.is_implies (Logic.get_sqnt sq)
  in Logic.Rules.implE None c sq

let conjI sq =
  let c=first_concl Formula.is_conj (Logic.get_sqnt sq)
  in Logic.Rules.conjI None c sq

let conjE sq =
  let c=first_asm Formula.is_conj (Logic.get_sqnt sq)
  in Logic.Rules.conjE None c sq

let disjI sq =
  let c=first_asm Formula.is_disj (Logic.get_sqnt sq)
  in Logic.Rules.disjI None c sq

let disjE sq =
  let c=first_concl Formula.is_disj (Logic.get_sqnt sq)
  in Logic.Rules.disjE None c sq

let negC sq =
  let c=first_concl Formula.is_neg (Logic.get_sqnt sq)
  in Logic.Rules.negC None c sq

let negA sq =
  let c=first_asm Formula.is_neg (Logic.get_sqnt sq)
  in Logic.Rules.negA None c sq

let allI sq =
  let c=first_concl Formula.is_all (Logic.get_sqnt sq)
  in Logic.Rules.allI None c sq

let existI sq =
  let c=first_asm Formula.is_exists (Logic.get_sqnt sq)
  in Logic.Rules.existI None c sq

(*
let mp_basic_rule i g= 
  let g0=Logic.Rules.implE None i g
  in 
  (fun ng -> 
    (let sq=Logic.get_sqnt ng
    in 
    let c= get_cncl 1 sq
    in 
    (Logic.Rules.assume None
       (first_asm 
	  (Formula.alpha_convp (Logic.scope_of sq) c) sq)
       (fnum 1))) ng) g0

let mp_rule sq = 
  mp_basic_rule (first_asm Formula.is_implies (Logic.get_sqnt sq)) sq
*)

(*
let trueR ?c sq =
  let cf=
    match c with 
      Some x -> x
    | _ -> (first_concl Formula.is_true (Logic.get_sqnt sq))
  in Logic.Rules.trueR None cf sq
*)

(*
let existE ?c trm sq =
  let cf=
    match c with
      (Some x) -> x
    | _ -> (first_concl Formula.is_exists (Logic.get_sqnt sq))
  in Logic.Rules.existE None trm cf sq
*)

(*
let allE ?a trm sq =
  let af=
    match a with
      Some x -> x
    | _ -> (first_asm Formula.is_all (Logic.get_sqnt sq))
  in Logic.Rules.allE None trm af sq
*)


(*
let rewrite_thm ths ?(dir=leftright) i sq=
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
  let sq1= Logic.Rules.rewrite ~dir:dir (List.map fnum ns) j sq0
  in deleten ns sq1
*)



let rec find_rule t rs =
  match rs with 
    [] -> raise Not_found
  |	((p, r)::xs) -> 
      if p t then r else find_rule t xs


(*
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
  else raise (Result.error "No change"))
*)

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

(*
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
  else raise (Result.error "No change"))
*)

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


(*
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
  if !chng then rslt else raise (Result.error "No change"))
*)

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


(*
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
  if !chng then rslt else raise (Result.error "No change"))
*)

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
