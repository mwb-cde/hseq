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

    let addthy thdb thy = 
      let name = Theory.get_name thy
      in 
      if is_loaded name thdb
      then raiseError ("Theory "^name^" exists")
      else (Lib.add name thy thdb.db)

    let remove_thy thdb n= 
      if n=Theory.get_name thdb.curr 
      then raiseError ("Theory "^n^" is current theory")
      else Hashtbl.remove thdb.db n

    let getthy thdb name = Lib.find name thdb.db
    let get_parents thdb s = Theory.get_parents (getthy thdb s)


    let rec filter p xs =
      match xs with
	[] -> []
      |	(y::ys) -> if p y then filter p ys else y::(filter p ys)

    let add_importing ls thdb = 
      thdb.importing<-
	 Lib.remove_dups (thdb.importing@ ls)

    let setcur thdb name = 
      try
	thdb.curr<- (getthy thdb name); 
	thdb.importing<-[name];
 	thdb
      with Not_found -> raiseError ("Can' find theory "^name)

    let setcur_thy thdb thy = 
      thdb.curr<- thy;
      thdb.importing<-[Theory.get_name thy];
      if is_loaded (Theory.get_name thy)  thdb
      then thdb 
      else (ignore(addthy thdb thy); thdb)

let load_theory thdb name prot thfn filefn =
  let test_date tim thy = 
    if (Theory.get_date thy)<tim then () 
    else raiseError 
      	("Imported theory "^(Theory.get_name thy)^" is more recent than "
	 ^" its importing theory")
  and test_protection thy =
    if prot 
    then 
      if (Theory.get_protection thy) then ()
      else raiseError 
	  ("Imported theory "^(Theory.get_name thy)^" is not complete")
    else ()
  in 
  let rec load_aux tim ls imps=
    match ls with 
      [] -> imps
    | (x::xs) ->
	(if (x=name) 
	then raiseError ("Circular importing in Theory "^x)
	else 
	  (if is_loaded x thdb
	  then 
	    (let thy = getthy thdb x 
	    in 
	    test_protection thy;
	    test_date tim thy;
	    load_aux tim xs 
	      (if List.mem x imps then imps else (x::imps)))
	  else 
	    (let fname = filefn x
	    in let thy = Theory.load_theory fname
	    in 
	    test_protection thy;
	    test_date tim thy; 
	    ignore(addthy thdb thy); 
	    thfn thy;
	    load_aux tim xs 
	      (load_aux (Theory.get_date thy)
		 (Theory.get_parents thy) (x::imps)))))
  in 
  (if is_loaded name thdb
  then 
    (let thy=getthy thdb name
    in 
    test_protection thy;
    let imprts = 
      load_aux (Theory.get_date thy)
	(Theory.get_parents thy) [Theory.get_name thy]
    in List.rev imprts)
  else 
    (let thy = Theory.load_theory (filefn name)
    in 
    (test_protection thy; 
     ignore(addthy thdb thy);
     thfn thy;
     (let imprts = 
       (load_aux (Theory.get_date thy)
	  (Theory.get_parents thy) [Theory.get_name thy])
     in (List.rev imprts)))))
    
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
	      with _ -> raiseError("mk_importing: theory "^x))
      in 
      (mk_aux thdb (Theory.get_parents thdb.curr) [])

    let set_importing thdb  = 
      let name =(Theory.get_name (thdb.curr))
      in 
	thdb.importing<- 
	   (name :: (mk_importing thdb));
 	thdb


    let add_axiom s th thdb= Theory.add_axiom s th thdb.curr
    let add_thm s th thdb = Theory.add_thm s th thdb.curr

let add_pp_rec idsel n ppr thdb = 
  Theory.add_pp_rec idsel n ppr thdb.curr


(*
    let add_type_rec s tr thdb = Theory.add_type_rec s tr thdb.curr
*)
    let add_type_rec tr thdb = Theory.add_type_rec tr thdb.curr

    let add_defn_rec s ty def inf pr thdb =
      Theory.add_defn_rec s ty def inf pr thdb.curr

    let add_defn s ty def thdb =
      Theory.add_defn s ty def thdb.curr

    let add_decln_rec dcl pr thdb =
      let s, ty = Defn.dest_decln dcl
      in 
      Theory.add_decln_rec (Basic.name s) ty pr thdb.curr

    let add_decln dcl thdb =
      let s, ty = Defn.dest_decln dcl
      in 
      Theory.add_decln_rec (Basic.name s) ty 0 thdb.curr


(*
    let add_defn_rec s ar ty def inf pr thdb =
      Theory.add_defn_rec s ar ty def inf pr thdb.curr

    let add_defn s ty def thdb =
      Theory.add_defn s ty def thdb.curr

    let add_decln_rec s ar ty thdb =
      Theory.add_decln_rec s ar ty thdb.curr
*)

    let find f tdb =
      let rec find_aux ls =
	match ls with 
	  [] -> raise Importing
	| x::xs ->
	    try f (getthy tdb x)
	    with Not_found -> find_aux xs
      in find_aux tdb.importing

    let quick_find f th tdb =
      if imported th tdb
      then f (getthy tdb th)
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
	  None -> raiseError ("No definition for "^n)
	| Some(d) -> d)

    let get_id_type th n tdb = 
      (let r = (get_defn_rec th n tdb)
     	in r.Theory.typ)

(*
    let get_id_arity th n tdb = 
     	(let r =  get_defn_rec th n tdb
     	in r.Theory.arity)
*)
    let id_is_infix th n tdb = 
     	(let r =  get_defn_rec th n tdb
     	in r.Theory.infix)

    let get_id_prec th n tdb = 
     	(let r =  get_defn_rec th n tdb
     	in r.Theory.prec)

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
	  (let th = getthy thdb n
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
  

  
