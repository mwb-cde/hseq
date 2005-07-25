(*-----
   Name: thydb.ml
   Author: M Wahab <Mwahab@Users.Sourceforge.Net>
   Copyright M Wahab 2005
   ----*)

open Result

exception Importing

(***
* Databases
***)

type thydb = {db: (string, Theory.thy)Hashtbl.t;
	      mutable curr:  Theory.thy;
	      mutable importing : string list}

let empty thy = {db= Hashtbl.create 253; 
		   curr=thy;
		   importing=[]}

let table thdb = thdb.db
let current thdb = thdb.curr
let imported thdb = thdb.importing

let add_importing ls thdb = 
  thdb.importing<- Lib.remove_dups ((imported thdb)@ ls)

(***
* Operations on Theories
***)

let current_name db = 
  let thy = current db
  in 
  Theory.get_name thy

let is_imported th thdb = List.mem th thdb.importing

let thy_in_scope th thydb = is_imported th thydb

let is_loaded name thdb = Lib.member name thdb.db

let add_thy thdb thy = 
  let name = Theory.get_name thy
  in 
  if is_loaded name thdb
  then raise (Result.error ("Theory "^name^" exists"))
  else (Lib.add name thy thdb.db)

let remove_thy thdb n= 
  if n=current_name thdb
  then raise (Result.error ("Theory "^n^" is current theory"))
  else Hashtbl.remove thdb.db n

let get_thy thdb name = Lib.find name thdb.db

let get_parents thdb s = Theory.get_parents (get_thy thdb s)

let set_current thdb thy = 
  let db = 
    {thdb with
     curr = thy;
     importing = [Theory.get_name thy]
   }
  in 
  if is_loaded (Theory.get_name thy)  db
  then db 
  else (ignore(add_thy db thy); db)

(***
* Operations on the current theory 
***)

(*** House keeping functions ***)

(*** Constructing an importing list from the current theory. ***)

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
	    in 
	    mk_aux thdb xs 
	      (rs@(List.filter (fun x->not(List.mem x rs)) nrs ))
	  with _ -> raise (Result.error("mk_importing: theory "^x)))
  in 
  (mk_aux thdb (Theory.get_parents thdb.curr) [])

let set_importing thdb = 
  let name = current_name thdb
  in 
  {thdb with importing = (name :: (mk_importing thdb))}

(*** Find functions ***)

(** 
   [find f tdb]: apply [f] to each theory in the importing list,
   returning the first that succeeds. Raise [Not_found] if none succeed.
*)
let find f tdb =
  let rec find_aux ls =
    match ls with 
      [] -> raise Not_found
    | x::xs ->
	try f (get_thy tdb x)
	with Not_found -> find_aux xs
  in find_aux tdb.importing

(*
let find_apply f tdb=
  try find f tdb
  with Importing -> raise Not_found
*)

(** 
   [quick_find f th tdb]: apply [f] to theory [th] if it is in the
   importing list. Raise [Not_found] if not found.
*)
let quick_find f th tdb =
  if is_imported th tdb
  then f (get_thy tdb th)
  else raise Not_found

(**
   [find_to_apply mem f thy_name thdb]: Starting with the theory named
   [thy_name], apply [f] to each theory in the imported by [thy_name]
   returning the first to succeed. Return [Not_found] if none succeed.
*)
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

(*** Types ***)

let add_type_rec tr thdb = Theory.add_type_rec tr thdb.curr

let get_type_rec th n tdb=
  let get_aux cur= 
    try Theory.get_type_rec n cur
    with _ -> raise Not_found 
  in 
  if th="" 
  then (find get_aux tdb)
  else quick_find get_aux th tdb

let thy_of_type th name thdb =
  let is_thy_of thy =
    if (Theory.type_exists name thy) then (Theory.get_name thy)
    else raise Not_found
  in 
  find_to_apply (empty_memo()) is_thy_of th thdb 


(*** Definitions and Declarations ***)

let add_decln_rec dcl ps thdb =
  let s, ty = Logic.Defns.dest_termdecln dcl
  in 
  Theory.add_decln_rec (Basic.name s) ty ps thdb.curr

let add_decln dcl ps thdb =
  let s, ty = Logic.Defns.dest_termdecln dcl
  in 
  Theory.add_decln_rec (Basic.name s) ty ps thdb.curr

let add_defn_rec s ty def ps thdb =
  Theory.add_defn_rec s ty def ps thdb.curr

let add_defn s ty def ps thdb =
  Theory.add_defn_rec s ty (Some def) ps thdb.curr

let get_defn_rec th n tdb =
  let get_aux cur= 
    try Theory.get_defn_rec n cur
    with _ -> raise Not_found 
  in 
  if th="" 
  then (find get_aux tdb)
  else quick_find get_aux th tdb


let get_defn th n tdb = 
  let r = get_defn_rec th n tdb
  in 
  match r.Theory.def with
    None -> raise (Result.error ("No definition for "^n))
  | Some(d) -> d

let get_id_type th n tdb = 
  let r = (get_defn_rec th n tdb)
  in r.Theory.typ

let get_id_options n db = 
  let get_id x =
    try Some(get_defn_rec x n db) with Not_found -> None
  in 
  let rec get_aux ls r= 
    match ls with
      [] -> List.rev r
    | (x::xs) -> 
	(match(get_id x) with
	  None -> get_aux xs r
	| (Some defn) ->
	    get_aux xs (((Basic.mk_long x n), (defn.Theory.typ))::r))
  in 
  get_aux db.importing []

let id_exists th n tdb = 
  (try 
    (ignore(get_defn_rec th n tdb); true)
  with Not_found -> false)

let thy_of th name thdb =
  let is_thy_of thy =
    if (Theory.id_exists name thy) then (Theory.get_name thy)
    else raise Not_found
  in 
  find_to_apply (empty_memo()) is_thy_of th thdb 

(*** Theorems ***)

let add_axiom s th ps thdb= Theory.add_axiom s th ps thdb.curr

let add_thm s th ps thdb = Theory.add_thm s th ps thdb.curr

let get_axiom th n tdb =
  let get_aux cur= 
    try 
      Theory.get_axiom n cur
    with _ -> raise Not_found
  in 
  if th="" 
  then (find get_aux tdb)
  else quick_find get_aux th tdb

let get_theorem th n tdb =
  let get_aux cur=
    try Theory.get_theorem n cur
    with _ -> raise Not_found
  in 
  if th="" 
  then (find get_aux tdb)
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
  then (find get_aux tdb)
  else quick_find get_aux th tdb

(*** Type Printer-Parser records ***)


let add_type_pp_rec n ppr thdb = 
  Theory.add_type_pp_rec n ppr thdb.curr

let get_type_pp_rec th n tdb =
  let get_aux cur= 
    try 
      Theory.get_type_pp_rec n cur
    with _ -> raise Not_found
  in 
  if th="" 
  then (find get_aux tdb)
  else quick_find get_aux th tdb
      
let remove_type_pp_rec th n tdb = 
  let get_aux cur= 
    try 
      Theory.remove_type_pp_rec n cur
    with _ -> raise Not_found
  in 
  if th="" 
  then (find get_aux tdb)
  else quick_find get_aux th tdb

let get_type_pplist th tdb =
  let get_aux cur =
    Theory.get_type_pplist cur
  in quick_find get_aux th tdb

(*** Term Printer-Parser records ***)

let add_term_pp_rec n ppr thdb = 
  Theory.add_term_pp_rec n ppr thdb.curr

let get_term_pp_rec th n tdb =
  let get_aux cur= 
    try 
      Theory.get_term_pp_rec n cur
    with _ -> raise Not_found
  in 
  if th="" 
  then (find get_aux tdb)
  else quick_find get_aux th tdb
      
let remove_term_pp_rec th n tdb = 
  let get_aux cur= 
    try 
      Theory.remove_term_pp_rec n cur
    with _ -> raise Not_found
  in 
  if th="" 
  then (find get_aux tdb)
  else quick_find get_aux th tdb

let get_term_pplist th tdb =
  let get_aux cur =
    Theory.get_term_pplist cur
  in quick_find get_aux th tdb

(*** 
 * Scopes from database 
 ***)

let scope_term_type db f= 
  let thstr, idstr = Basic.dest_fnid f
  in 
  get_id_type thstr idstr db

let scope_term_thy thy_name db x = 
  thy_of thy_name x db

let scope_type_defn db f = 
  let thstr, idstr = Basic.dest_fnid f
  in 
  get_type_rec thstr idstr db

let scope_type_thy thy_name db x = 
  thy_of_type thy_name x db

let scope_thy_in_scope db th1 = 
  if(th1=Basic.null_thy)  (* ignore the empty scope *)
  then true
  else thy_in_scope th1 db

let mk_scope db =
  let thy_name = (current_name db)
  in 
  {
   Scope.curr_thy = thy_name;
   Scope.term_type = scope_term_type db; 
   Scope.term_thy = scope_term_thy thy_name db;
   Scope.type_defn = scope_type_defn db;
   Scope.type_thy = scope_type_thy thy_name db;
   Scope.thy_in_scope  = scope_thy_in_scope db
 } 

(***
* Theory loader 
***)

module Loader = 
  struct

    (*** Data needed for loading a theory. ***)
    type data = 
	{
	 thy_fn : (Theory.contents -> unit);
	 (** 
	    Function to apply to a successfully loaded theory.
	  *)
	 file_fn : (string -> string);
	 (** Function to construct the filename of theory file to load. *)
	 build_fn: string -> unit;
	   (** Function to build the theory if it can't be loaded. *)
	   prot: bool
       }

    let mk_data tfn ffn bfn p = 
      { thy_fn = tfn; file_fn = ffn; build_fn = bfn; prot = p }

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

    let rec load_parents db bundle name tyme ps imports = 
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
	      let imports0 = load_parents db bundle name tyme xs 
		  (if List.mem x imports then imports else (x::imports))
	      in 
	      add_importing [Theory.get_name thy] db;
	      imports0)
	    else 
	      let thy = 
		(try 
		  load_thy true tyme (bundle.file_fn, bundle.thy_fn) x db
		with _ -> 
		  build_thy tyme bundle.build_fn x db);
	      in 
	      add_importing [Theory.get_name thy] db;
	      let imports0=
		load_parents db bundle name (Theory.get_date thy)
		  (Theory.get_parents thy) (x::imports)
	      in 
	      let imports1 = 
		load_parents db bundle name tyme xs imports0
	      in 
	      apply_fn db bundle.thy_fn thy;
	      imports1))

    let load_theory thdb name data =
      let current_time = Lib.date()
      in 
      if is_loaded name thdb
      then 
	let thy=get_thy thdb name
	in 
	test_protection data.prot thy;
	let imprts = 
	  load_parents 
	    thdb data name
	    (Theory.get_date thy) (Theory.get_parents thy) [name]
	in 
	List.rev imprts
      else 
	match 
	  (Lib.try_app
	    (load_thy data.prot current_time 
	      (data.file_fn, data.thy_fn) name) thdb)
	with 
	  Some(thy) -> 
	    (let imprts = 
	      load_parents 
		thdb data name
		(Theory.get_date thy) (Theory.get_parents thy) [name]
	    in 
	    add_importing [Theory.get_name thy] thdb;
	    apply_fn thdb data.thy_fn thy;
	    List.rev imprts)
	| None -> 
	    (let thy = build_thy current_time data.build_fn name thdb
	    in 
	    let imprts = 
	      load_parents 
		thdb data name
		(Theory.get_date thy) (Theory.get_parents thy) [name]
	    in 
	    List.rev imprts)

(*
    let load_theory thdb name data =
      let current_time = Lib.date()
      in 
      if is_loaded name thdb
      then 
	let thy=get_thy thdb name
	in 
	test_protection data.prot thy;
	let imprts = 
	  load_parents 
	    thdb data name
	    (Theory.get_date thy) (Theory.get_parents thy) [name]
	in 
	List.rev imprts
      else 
	try 
	  (let thy = 
	    load_thy data.prot current_time 
	      (data.file_fn, data.thy_fn) name thdb
	  in 
	  test_protection data.prot thy; 
	  let imprts = 
	    load_parents 
	      thdb data name
	      (Theory.get_date thy) (Theory.get_parents thy) [name]
	  in 
	  add_importing [Theory.get_name thy] thdb;
	  apply_fn thdb data.thy_fn thy;
	  List.rev imprts)
	with 
	  _ -> 
	    (let thy = build_thy current_time data.build_fn name thdb
	    in 
	    test_protection data.prot thy; 
	    let imprts = 
	      load_parents 
		thdb data name
		(Theory.get_date thy) (Theory.get_parents thy) [name]
	    in 
	    List.rev imprts)
*)
  end      

