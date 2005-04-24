(*-----
 Name: thydb.ml
 Author: M Wahab <Mwahab@Users.Sourceforge.Net>
 Copyright M Wahab 2005
----*)

open Result

exception Importing

type thydb = {db: (string, Theory.thy)Hashtbl.t;
	      mutable curr:  Theory.thy;
	      mutable importing : string list}

let emptydb thy = {db= Hashtbl.create 1; 
		   curr=thy;
		   importing=[]}
let getcur thdb = thdb.curr
let getdb thdb = thdb.db
let imported th thdb = List.mem th thdb.importing

let is_loaded name thdb = 
  Lib.member name thdb.db

let add_thy thdb thy = 
  let name = Theory.get_name thy
  in 
  if is_loaded name thdb
  then raise (Result.error ("Theory "^name^" exists"))
  else (Lib.add name thy thdb.db)

let remove_thy thdb n= 
  if n=Theory.get_name thdb.curr 
  then raise (Result.error ("Theory "^n^" is current theory"))
  else Hashtbl.remove thdb.db n

let get_thy thdb name = Lib.find name thdb.db
let get_parents thdb s = Theory.get_parents (get_thy thdb s)


let rec filter p xs =
  match xs with
    [] -> []
  |	(y::ys) -> if p y then filter p ys else y::(filter p ys)

let add_importing ls thdb = 
  thdb.importing<-
    Lib.remove_dups (thdb.importing@ ls)

let setcur thdb name = 
  try
    thdb.curr<- (get_thy thdb name); 
    thdb.importing<-[name];
    thdb
  with Not_found -> raise (Result.error ("Can' find theory "^name))

let setcur_thy thdb thy = 
  thdb.curr<- thy;
  thdb.importing<-[Theory.get_name thy];
  if is_loaded (Theory.get_name thy)  thdb
  then thdb 
  else (ignore(add_thy thdb thy); thdb)


let print_date d =
  let (y, mo, day, h, mi) = Lib.nice_date d
  in 
  Format.printf "@[Date: %i/%i/%i %i:%i@]" day (mo+1) y h mi

let test_date tim thy = 
  if (Theory.get_date thy) <= tim 
  then () 
  else 
    (warning ("Imported theory "^(Theory.get_name thy)
	      ^" is more recent than"
	      ^" its importing theory");
     raise (Result.error 
      	      ("Imported theory "^(Theory.get_name thy)
	       ^" is more recent than"
	       ^" its importing theory")))
and test_protection prot thy =
  if prot 
  then 
    (if (Theory.get_protection thy) 
    then ()
    else 
      (warning 
	 ("Imported theory "^(Theory.get_name thy)
	  ^" is not complete");
       raise 
	 (Result.error 
	    ("Imported theory "^(Theory.get_name thy)
	     ^" is not complete"))))
  else ()

let load_thy p tim (filefn, thfn) x thdb=
  let fname = filefn x
  in 
  let thy = Theory.load_theory fname
  in 
  test_protection p thy;
  test_date tim thy;
  ignore(add_thy thdb thy); thy

let build_thy tim buildfn x thdb= 
  buildfn x; 
  get_thy thdb x

let apply_fn db thyfn thy =
  (try (thyfn (Theory.contents thy)) with _ -> ())

let rec load_parents bundle tyme ps imports = 
  let (db, thyfn, filefn, buildfn, name) = bundle
  in 
    match ps with 
      [] -> imports
    | (x::xs) ->
	(if (x=name) 
	then 
	  raise (Result.error ("Circular importing in Theory "^x))
	else 
	  (if is_loaded x db
	  then 
	    (let thy = get_thy db x 
	    in 
	    test_protection true thy;
	    test_date tyme thy;
	    let imports0 = load_parents bundle tyme xs 
	      (if List.mem x imports then imports else (x::imports))
	    in 
	    add_importing [Theory.get_name thy] db;
	    imports0)
	  else 
	    let thy = 
	      (try 
		load_thy true tyme (filefn, thyfn) x db
	      with _ -> 
		 build_thy tyme buildfn x db);
	    in 
	    add_importing [Theory.get_name thy] db;
	    let imports0=
	      load_parents bundle (Theory.get_date thy)
		(Theory.get_parents thy) (x::imports)
	    in 
	    let imports1 = 
	      load_parents bundle tyme xs imports0
	    in 
	    apply_fn db thyfn thy;
	    imports1))

let load_theory thdb name prot thfn filefn buildfn =
  let current_time = Lib.date()
  in 
  if is_loaded name thdb
  then 
    let thy=get_thy thdb name
    in 
    test_protection prot thy;
    let imprts = 
      load_parents 
	(thdb, thfn, filefn, buildfn, name)
	(Theory.get_date thy) (Theory.get_parents thy) [name]
    in 
    List.rev imprts
  else 
    try 
      (let thy = load_thy prot current_time (filefn, thfn) name thdb
      in 
      test_protection prot thy; 
      let imprts = 
	load_parents 
	  (thdb, thfn, filefn, buildfn, name)
	  (Theory.get_date thy) (Theory.get_parents thy) [name]
      in 
      add_importing [Theory.get_name thy] thdb;
      apply_fn thdb thfn thy;
      List.rev imprts)
    with 
      _ -> 
	(let thy = build_thy current_time buildfn name thdb
	in 
	test_protection prot thy; 
	let imprts = 
	  load_parents 
	    (thdb, thfn, filefn, buildfn, name)
	    (Theory.get_date thy) (Theory.get_parents thy) [name]
	in 
	List.rev imprts)
      
let mk_importing thdb=
  let rec mk_aux thdb ls rs =
    match ls with 
      [] -> rs
    | (x::xs) -> 
	if List.mem x rs 
	then mk_aux thdb xs rs
	else 
	  (try
	    let nls = get_parents thdb x
	    in let nrs= mk_aux thdb nls (x::rs)
	    in mk_aux thdb xs (rs@(filter (fun x->List.mem x rs) nrs ))
	  with _ -> raise (Result.error("mk_importing: theory "^x)))
  in 
  (mk_aux thdb (Theory.get_parents thdb.curr) [])

let set_importing thdb  = 
  let name =(Theory.get_name (thdb.curr))
  in 
  thdb.importing<- 
    (name :: (mk_importing thdb));
  thdb

let add_axiom s th ps thdb= Theory.add_axiom s th ps thdb.curr
let add_thm s th ps thdb = Theory.add_thm s th ps thdb.curr

let add_pp_rec idsel n ppr thdb = 
  Theory.add_pp_rec idsel n ppr thdb.curr

let add_type_rec tr thdb = Theory.add_type_rec tr thdb.curr

let add_defn_rec s ty def inf pr ps thdb =
  Theory.add_defn_rec s ty def inf pr ps thdb.curr

let add_defn s ty def ps thdb =
  Theory.add_defn s ty def ps thdb.curr

let add_decln_rec dcl pr ps thdb =
  let s, ty = Logic.Defns.dest_termdecln dcl
  in 
  Theory.add_decln_rec (Basic.name s) ty pr ps thdb.curr

let add_decln dcl ps thdb =
  let s, ty = Logic.Defns.dest_termdecln dcl
  in 
  Theory.add_decln_rec (Basic.name s) ty 0 ps thdb.curr

let find f tdb =
  let rec find_aux ls =
    match ls with 
      [] -> raise Importing
    | x::xs ->
	try f (get_thy tdb x)
	with Not_found -> find_aux xs
  in find_aux tdb.importing

let quick_find f th tdb =
  if imported th tdb
  then f (get_thy tdb th)
  else raise Not_found

let find_apply f tdb=
  try
    find f tdb
  with Importing -> raise Not_found

let get_pplist idsel th tdb =
  let get_aux cur =
    Theory.get_pplist idsel cur
  in quick_find get_aux th tdb

let get_pp_rec idsel th n tdb =
  let get_aux cur= 
    try 
      Theory.get_pp_rec idsel n cur
    with _ -> raise Not_found
  in 
  if th="" 
  then (find_apply get_aux tdb)
  else quick_find get_aux th tdb
      
let remove_pp_rec idsel th n tdb = 
  let get_aux cur= 
    try 
      Theory.remove_pp_rec idsel n cur
    with _ -> raise Not_found
  in 
  if th="" 
  then (find_apply get_aux tdb)
  else quick_find get_aux th tdb


let get_axiom th n tdb =
  let get_aux cur= 
    try 
      Theory.get_axiom n cur
    with _ -> raise Not_found
  in 
  if th="" 
  then (find_apply get_aux tdb)
  else quick_find get_aux th tdb

let get_theorem th n tdb =
  let get_aux cur=
    try Theory.get_theorem n cur
    with _ -> raise Not_found
  in 
  if th="" 
  then (find_apply get_aux tdb)
  else quick_find get_aux th tdb

let get_defn_rec th n tdb =
  let get_aux cur= 
    try Theory.get_defn_rec n cur
    with _ -> raise Not_found 
  in 
  if th="" 
  then (find_apply get_aux tdb)
  else quick_find get_aux th tdb

let get_type_rec th n tdb=
  let get_aux cur= 
    try Theory.get_type_rec n cur
    with _ -> raise Not_found 
  in 
  if th="" 
  then (find_apply get_aux tdb)
  else quick_find get_aux th tdb

let get_term_pp_rec th n tdb =
  let get_aux cur= 
    try Theory.get_pp_rec Basic.fn_id n cur
    with _ -> raise Not_found 
  in 
  if th="" 
  then (find_apply get_aux tdb)
  else quick_find get_aux th tdb

let get_type_pp_rec th n tdb =
  let get_aux cur= 
    try Theory.get_pp_rec Basic.type_id n cur
    with _ -> raise Not_found 
  in 
  if th="" 
  then (find_apply get_aux tdb)
  else quick_find get_aux th tdb


let get_lemma th n tdb =
  let get_aux cur =
    try 
      Theory.get_theorem n cur
    with _ -> 
      try Theory.get_axiom n cur
      with _ ->
	try Theory.get_defn n cur
	with _ -> raise Not_found
  in
  if th="" 
  then (find_apply get_aux tdb)
  else quick_find get_aux th tdb

let get_defn th n tdb = 
  (let r = get_defn_rec th n tdb
  in match r.Theory.def with
    None -> raise (Result.error ("No definition for "^n))
  | Some(d) -> d)

let get_id_type th n tdb = 
  let r = (get_defn_rec th n tdb)
  in r.Theory.typ

let id_is_infix th n tdb = 
  let r = get_term_pp_rec th n tdb
  in 
  Printer.is_infix r.Printer.fixity

let get_id_prec th n tdb = 
  let r =  get_term_pp_rec th n tdb
  in 
  r.Printer.prec

let id_exists th n tdb = 
  (try 
    (ignore(get_defn_rec th n tdb); true)
  with Not_found -> false)


type memos=(string, bool) Lib.substype
let empty_memo()=Lib.empty_env()

let find_to_apply memo f thy_name thdb =
  let rec find_aux names =
    match names with
      [] -> raise Not_found
    | (n::ns) -> 
	if Lib.member n memo 
	then find_aux ns
	else 
	  (let th = get_thy thdb n
	  in 
	  (ignore(Lib.add n true memo);
	   try f th
	   with _ -> find_aux (get_parents thdb n)))
  in find_aux [thy_name]

let thy_in_scope th1 th2 thdb =   (* true if th2 is in scope of th1 *)
  let memo = empty_memo()
  in 
  let is_in_scope thy =
    if (Theory.get_name thy)=th2 then ()
    else raise Not_found
  in try (find_to_apply memo is_in_scope th1 thdb; true)
  with Not_found -> false

let thy_of name th thdb =
  let is_thy_of thy =
    if (Theory.id_exists name thy) then (Theory.get_name thy)
    else raise Not_found
  in find_to_apply (empty_memo()) is_thy_of th thdb 

let expunge thdb th= 
  Hashtbl.clear thdb.db;
  thdb.importing<-[];
  thdb.curr<-th
      
      
