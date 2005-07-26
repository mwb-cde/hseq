(*-----
   Name: thydb.ml
   Author: M Wahab <Mwahab@Users.Sourceforge.Net>
   Copyright M Wahab 2005
   ----*)

open Result

exception Importing

(***
* Error handling 
***)

class dbError s ns =
  object (self)
    inherit Result.error s
    val names = (ns :string list)
    method get() = names
    method print st = 
      Format.printf "@[%s@ @[" (self#msg()); 
      Printer.print_sep_list 
	(Format.print_string , ",") (self#get());
      Format.printf "@]@]"
  end

let error s t = mk_error((new dbError s t):>Result.error)
let add_error s t es = raise (Result.add_error (error s t) es)

(***
* Databases
***)

(**
   NameSet: A data structure for storing the names of theories 
   in the order they are added. 

   This behaves like a list of strings but also supports fast lookup.
*)
module NameSet = 
struct
  type t = { list : string list ; set : Lib.StringSet.t }

  let empty = { list = []; set = Lib.StringSet.empty }
  let add s x = { list = x::s.list; set = Lib.StringSet.add x s.set}

  let filter p s  = 
    { list = List.filter p s.list; set = Lib.StringSet.filter p s.set }

  let to_list s = s.list
  let to_set s = s.set
  let from_list ls = 
    List.fold_left add empty ls

  let mem s x = Lib.StringSet.mem x s.set

end

type thydb = 
    {
     db: (string, Theory.thy)Hashtbl.t;
     mutable curr: Theory.thy option;
     mutable importing : NameSet.t
   }

let empty ()= 
    {
     db= Hashtbl.create 253; 
     curr=None;
     importing = NameSet.empty
   }
(*
let empty thy= 
  if Theory.get_parents thy = [] 
  then 
    {
     db= Hashtbl.create 253; 
     curr=None
     importing = NameSet.empty
   }
  else 
    raise (Result.error ("Initial theory can't have parents."))
*)

let table thdb = thdb.db
let current thdb = 
  Lib.dest_option ~err:(Failure "No current theory") thdb.curr
let imported thdb = NameSet.to_list thdb.importing
let thys thdb = NameSet.to_set thdb.importing

let add_importing thdb ls = 
  let ls1 = Lib.remove_dups ((imported thdb)@ ls)
  in 
  let thys1 = List.fold_left NameSet.add thdb.importing ls1
  in 
  {thdb with importing = thys1}

(***
* Operations on Theories
***)

let current_name db = 
  let thy = current db
  in 
  Theory.get_name thy

let is_imported th thdb = NameSet.mem thdb.importing th
let thy_in_scope th thydb = is_imported th thydb
let is_loaded name thdb = Lib.member name thdb.db

let add_thy thdb thy = 
  let name = Theory.get_name thy
  in 
  if is_loaded name thdb
  then raise (Result.error ("Theory "^name^" exists"))
  else 
    {thdb with db =(Hashtbl.add thdb.db name thy; thdb.db)}

let remove_thy thdb n= 
  if n=current_name thdb
  then raise (Result.error ("Theory "^n^" is current theory"))
  else {thdb with db = (Hashtbl.remove thdb.db n; thdb.db)}

let get_thy thdb name = Lib.find name thdb.db

let get_parents thdb s = Theory.get_parents (get_thy thdb s)

let set_current thdb thy = 
  let db = 
    {
     thdb with
     curr = Some(thy);
     importing = NameSet.add NameSet.empty (Theory.get_name thy)
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
  let parents = try Theory.get_parents (current thdb) with _ -> []
  in 
  (mk_aux thdb parents [])

let set_importing thdb = 
  let name = current_name thdb
  in 
  {thdb with importing = NameSet.from_list (name :: (mk_importing thdb))}
(*
let set_importing thdb = 
  let name = current_name thdb
  in 
  {thdb with importing = (name :: (mk_importing thdb))}
*)

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
  in find_aux (imported tdb)

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

let add_type_rec tr thdb = 
  Theory.add_type_rec tr (current thdb); thdb

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
  Theory.add_decln_rec (Basic.name s) ty ps (current thdb);
  thdb

let add_decln dcl ps thdb =
  let s, ty = Logic.Defns.dest_termdecln dcl
  in 
  Theory.add_decln_rec (Basic.name s) ty ps (current thdb);
  thdb

let add_defn_rec s ty def ps thdb =
  Theory.add_defn_rec s ty def ps (current thdb);
  thdb

let add_defn s ty def ps thdb =
  Theory.add_defn_rec s ty (Some def) ps (current thdb);
  thdb

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
  get_aux (imported db) []

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

let add_axiom s th ps thdb= Theory.add_axiom s th ps (current thdb); thdb

let add_thm s th ps thdb = Theory.add_thm s th ps (current thdb); thdb

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
  Theory.add_type_pp_rec n ppr (current thdb); thdb

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
  Theory.add_term_pp_rec n ppr (current thdb); thdb

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
  let thy_name = 
    try (current_name db) with _ -> Basic.null_thy
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

    (** Information about a theory passed to file-handling functions. *)
    type info =
	{ 
	  name: string;
	  date : float;
	  protected : bool
	}

    let mk_info n d p = { name = n; date = d; protected = p }

    (*** Data needed for loading a theory. ***)
    type data = 
	{
	 thy_fn : (Theory.contents -> unit);
	 (** 
	    Function to apply to a successfully loaded theory.
	  *)
	 load_fn : (info -> Theory.thy);
	 (** Function to find and load a theory file. *)
(*
	 file_fn : (string -> string);
	 (** Function to construct the filename of theory file to load. *)
*)
	 build_fn: thydb -> string -> thydb;
	   (** Function to build the theory if it can't be loaded. *)
	   prot: bool
       }

    let mk_data tfn lfn bfn p = 
      { thy_fn = tfn; load_fn = lfn; build_fn = bfn; prot = p }

(*** Support functions for loading a theory ***)

(** 
   [test_data tim thy]: Ensure that the date of theory [thy] is not
   greater then [tim].
*)
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

(** 
   [test_protection prot thy]: Ensure that the protection of theory
   [thy] is [prot].
*)
    let test_protection prot thy =
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

(**
   [load_thy prot tim (filefn, thfn) n thdb]: Load theory named [n]
   into database [thdb]. The protection and date of the theory must be
   as specified by [prot] and [tim]. [filefn] and [thfn] are [file_fn]
   and [app_fn] from type [data].

   Adds the theory to [thdb]. Returns the theory.
*)
    let load_thy n tim prot data thdb=
      let thy = data.load_fn (mk_info n tim prot)
      in 
      test_protection prot thy;
      test_date tim thy;
      ignore(add_thy thdb thy); thy
	  
(**
   [build_thy buildfn n thdb]: Build theory named [n] using
   function [buildfn]. Function [buildfn] is assumed to add the theory
   to [thdb]. Returns the build theory.
*)
    let build_thy buildfn x thdb= 
      try
	let db = buildfn thdb x
	in 
	get_thy thdb x
      with err -> add_error "Failed to rebuild theory" [x] err

(**
   [apply_fn db thy_fn thy]: Apply [thy_fn] to the contents of theory
   [thy]. Ignores all errors.
*)
    let apply_fn db thy_fn thy =
      (try (thy_fn (Theory.contents thy)) with _ -> ())

(**
   [load_parents db data name tyme ps imports]: Load the theories with
   names in [ps] as parents of theory named [name] into database
   [db]. Each parent must be no younger then the date given by [tyme].
*)
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
	      let db1 = add_importing db [Theory.get_name thy]
	      in 
	      let imports0 = load_parents db1 bundle name tyme xs 
		  (if List.mem x imports then imports else (x::imports))
	      in 
	      imports0)
	    else 
	      let thy = 
		(try 
		  load_thy x tyme true bundle db
		with _ -> 
		  build_thy bundle.build_fn x db);
	      in 
	      let db1= add_importing db [Theory.get_name thy]
	      in 
	      let imports0=
		load_parents db1 bundle name (Theory.get_date thy)
		  (Theory.get_parents thy) (x::imports)
	      in 
	      let imports1 = 
		load_parents db bundle name tyme xs imports0
	      in 
	      apply_fn db bundle.thy_fn thy;
	      imports1))

(**
   [load_theory thdb name data]: Load the theory named [name] into
   database [thdb]. Also load the parents of the theory and applies
   the functions [data.thy_fn] to each loaded theory.
*)
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
	    (load_thy name current_time data.prot data) thdb)
	with 
	  Some(thy) -> 
	    (let imprts = 
	      load_parents 
		thdb data name
		(Theory.get_date thy) (Theory.get_parents thy) [name]
	    in 
	    let thdb1 = add_importing thdb [Theory.get_name thy]
	    in 
	    apply_fn thdb1 data.thy_fn thy;
	    List.rev imprts)
	| None -> 
	    (let thy = build_thy data.build_fn name thdb
	    in 
	    let imprts = 
	      load_parents 
		thdb data name
		(Theory.get_date thy) (Theory.get_parents thy) [name]
	    in 
	    List.rev imprts)

  end      

