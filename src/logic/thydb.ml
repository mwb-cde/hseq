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
  let mem s x = Lib.StringSet.mem x s.set
  let rev s  = { s with list = List.rev s.list }

  let filter p s  = 
    { list = List.filter p s.list; set = Lib.StringSet.filter p s.set }

  let to_list s = s.list
  let to_set s = s.set
  let from_list ls = 
    List.fold_left add empty ls
end

(*
type thydb = 
    {
     db: (string, Theory.thy)Hashtbl.t;
     mutable curr: Theory.thy option;
     mutable importing : NameSet.t
   }
*)
module Tree = Treekit.StringTree

type table_t = (Theory.thy)Tree.t

type thydb = 
    {
     db: table_t;
     curr: Theory.thy option;
     importing : NameSet.t
   }

let empty ()= 
    {
     db= Tree.nil; 
     curr=None;
     importing = NameSet.empty
   }

(*
let empty ()= 
    {
     db= Hashtbl.create 253; 
     curr=None;
     importing = NameSet.empty
   }
*)

let table thdb = thdb.db
let current thdb = 
  Lib.dest_option ~err:(Failure "No current theory") thdb.curr
let imported thdb = NameSet.to_list thdb.importing
let thys thdb = NameSet.to_set thdb.importing

(***
* Operations on Theories
***)

let current_name db = 
  let thy = current db
  in 
  Theory.get_name thy

let is_imported th thdb = NameSet.mem thdb.importing th
let thy_in_scope th thydb = is_imported th thydb
let is_loaded name thdb = Tree.mem thdb.db name

let add_thy thdb thy = 
  let name = Theory.get_name thy
  in 
  if is_loaded name thdb
  then raise (error ("Theory "^name^" already present in database.") [])
  else 
    {thdb with db =Tree.add thdb.db name thy}
(*
    {thdb with db =(Hashtbl.add thdb.db name thy; thdb.db)}
*)

let remove_thy thdb n= 
  if (n=current_name thdb) || (thy_in_scope n thdb)
  then raise (error ("Theory "^n^" is being used in the database.") [])
  else 
    {thdb with db = Tree.delete thdb.db n}
(*
    {thdb with db = (Hashtbl.remove thdb.db n; thdb.db)}
*)

let get_thy thdb name = Tree.find thdb.db name

let get_parents thdb s = Theory.get_parents (get_thy thdb s)

(***
* Operations on the current theory 
***)

(*** House keeping functions ***)

(*** Constructing an importing list from the current theory. ***)

(**
   [not_loaded db ns]: Get the list of names in [ns] which are not
   loaded in the database of [db].
*)
let not_loaded db ns = 
  List.filter (fun x -> not (is_loaded x db)) ns

(**
   [all_loaded db ns]: Test that each theory named in [ns] is loaded
   in [db]. Raise error if any is not loaded.
*)
let all_loaded db ns = 
  match not_loaded db ns with
    [] -> true
  | (x::_) -> raise (error "Theory not in database" [x])

let add_importing thdb ls = 
  let ls1 = Lib.remove_dups ((imported thdb)@ ls)
  in 
  if all_loaded thdb ls1
  then 
    let thys1 = List.fold_left NameSet.add thdb.importing ls1
    in 
    {thdb with importing = thys1}
  else thdb

(** 
   [mk_importing db]: Build the importing list of the current theory 
   Fail if any theory in not loaded.
*)
let mk_importing thdb=
  let rec mk_aux thdb ls rs =
    match ls with 
      [] -> rs
    | (x::xs) -> 
	if NameSet.mem rs x
	then mk_aux thdb xs rs
	else 
	  (let nls = 
	    try (get_parents thdb x)
	    with err -> 
	      raise (add_error "mk_importing, theory" [x] err)
	    in 
	    let rs1 = mk_aux thdb nls (NameSet.add rs x)
	    in 
	    mk_aux thdb xs rs1)
  in 
  let parents = try Theory.get_parents (current thdb) with _ -> []
  in 
  let thy_list = 
    try (current_name thdb)::parents with _ -> parents
  in 
  NameSet.rev (mk_aux thdb thy_list NameSet.empty)

(*
  NameSet.to_list (mk_aux thdb parents NameSet.empty)
*)

(*
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
*)

(*
let set_importing thdb = 
  let name = current_name thdb
  in 
  {thdb with importing = NameSet.from_list (name :: (mk_importing thdb))}
*)

let set_current thdb thy = 
  let name = Theory.get_name thy 
  in
  let err = 
    try (ignore(all_loaded thdb (imported thdb)); None)
    with e -> Some(e)
  in
  match err with
  | Some(e) -> 
      raise (add_error 
	       ("Can't set theory "^name^" as the current theory.") [] e)
  | None ->
      let db1 = 
	try (add_thy thdb thy) with _ -> thdb
      in 
      let db2 = 
	{db1 with curr = Some(thy)}
      in 
      {
       db2 with
       importing = mk_importing db2
     }

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
	  date : float option;
	  prot : bool option
	}

    let mk_info n d p = { name = n; date = d; prot = p }

    (*** Data needed for loading a theory. ***)
    type data = 
	{
	 thy_fn : (Theory.contents -> unit);
	 (** 
	    Function to apply to a successfully loaded theory.
	  *)
	 load_fn : (info -> Theory.thy);
	 (** Function to find and load a theory file. *)
	 build_fn: thydb -> string -> thydb
	   (** Function to build the theory if it can't be loaded. *)
       }

    let mk_data tfn lfn bfn = 
      { thy_fn = tfn; load_fn = lfn; build_fn = bfn }

(*** Support functions for loading a theory ***)

(**
   A version of [set_current] for internal use. Sets the current
   theory and adds the name to the importing list. 

   Doesn't re-calculate the importing list. 
   Doesn't test whether any of the theories are loaded.
*)
    let set_curr db thy = 
      let name = Theory.get_name thy
      and imps = db.importing
      in 
      let thy_list = 
	if NameSet.mem imps name 
	then imps 
	else NameSet.add db.importing name
      in 
      {
       db with
       curr = Some(thy);
       importing = thy_list
     }

(** 
   [test_data tim thy]: Ensure that the date of theory [thy] is not
   greater then [tim].
*)
    let test_date tym thy = 
      match tym with
	None -> ()
      | (Some tim) ->
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
      match prot with 
	None -> ()
      | (Some pval) ->
	  if pval
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

   Adds the theory to [thdb]. Returns the updated database.
   Doesn't change the current theory. Doesn't change the importing data.
*)
    let load_thy info data thdb=
      try
	let thy = data.load_fn info
	in 
	test_protection info.prot thy;
	test_date info.date thy;
	add_thy thdb thy
      with err -> add_error "Failed to load theory" [info.name] err
	  
(**
   [build_thy info buildfn thdb]: Build theory named [info.name] using
   function [buildfn]. Function [buildfn] is assumed to add the theory
   to [thdb]. 

   Fails if the newly built theory does not have the attributes
   specified in [info].

   Returns the database with the newly built theory as the current theory.
*)

(** 
   [check_build db thy]: verify that [thy] is in database [db], that
   the parents of [thy] are in the importing list of [db] and that
   [thy] is the first in the importing list.

   raise Failure if checks fail.
*)
    let check_build db thy =
      let thy_list = imported db 
      and name = Theory.get_name thy
      in 
      try 
	((match thy_list with 
	  (x::_) -> 
	    if ((x == name) || (x = name)) 
	    then ()
	    else 
	      raise 
		(error "Built theory not first in importing list." [name; x])
	| [] -> 
	    raise 
	      (error "Empty importing list, trying to build " [name]));
	 ignore(all_loaded db thy_list);
	 try ignore (get_thy db name)
	 with _ -> raise (error "Built theory not in database." [name]))
      with err -> 
	raise (add_error "Failed to build theory" [name] err)

    let build_thy info data thdb= 
	let db = 
	  try data.build_fn thdb info.name
	  with err -> add_error "Failed to rebuild theory" [info.name] err
	in 
	let thy = 
	  try get_thy db info.name 
	  with err -> 
	    add_error 
	      "Failed to rebuild theory. Theory not in database." 
	      [info.name] err
	in 
	(try test_protection info.prot thy
	with err -> 
	  add_error 
	    "Failed to rebuild theory. Theory not protected." 
	    [info.name] err);
	(try check_build db thy
	with err -> 
	  add_error "Failed to rebuild theory" [info.name] err);
	(try set_curr db thy
	with err -> 
	  add_error 
	    "Failed to rebuild theory. Can't make theory current." 
	    [info.name] err)


(**
   [apply_fn db thy_fn thy]: Apply [thy_fn] to the contents of theory
   [thy]. Ignores all errors.
*)
    let apply_fn db thy_fn thy =
      (try (thy_fn (Theory.contents thy)) with _ -> ())


(**
   [load_theory thdb name data]: Load the theory named [name] into
   database [thdb]. Also load the parents of the theory and applies
   the functions [data.thy_fn] to each loaded theory.

   Makes the newly loaded theory the current theory.
*)
    let rec load_theory thdb data info =
      let name = info.name
      and tyme = info.date
      in 
      if is_loaded name thdb
      then 
	let thy=get_thy thdb name
	in 
	test_date tyme thy;
	test_protection info.prot thy;
	let db1 =
	  load_parents thdb data 
	    (mk_info name (Some (Theory.get_date thy)) (Some true))
	    (Theory.get_parents thy)
	in 
	set_curr db1 thy
      else 
	let load_attempt = 
	  Lib.try_app (load_thy info data) thdb
	in 
	match load_attempt with 
	  Some(db1) ->  (** Loading from file succeeded. **)
	    let thy = 
	      try get_thy db1 name
	      with err -> 
		raise 
		  (add_error 
		     "Load theory: something went wrong with theory " 
		     [name] err)
	    in 
	    let db2 = 
	      load_parents db1 data
		(mk_info name (Some (Theory.get_date thy)) (Some true))
		(Theory.get_parents thy) 
	    in 
	    let db3 = set_curr db2 thy
	    in 
	    apply_fn db3 data.thy_fn thy;
	    db3
	| None -> (** Loading from file failed, try to rebuild. **)
	      build_thy info data thdb

(**
   [load_parents db data name tyme ps imports]: Load the theories with
   names in [ps] as parents of theory named [name] into database
   [db]. Each parent must be no younger then the date given by [tyme].
*)
    and load_parents db bundle info ps =
      let name = info.name
      and tyme = info.date
      in 
      match ps with 
	[] -> db
      | (x::xs) ->
	  (if (x = name)
	  then 
	    raise (Result.error ("Circular importing in Theory "^x))
	  else 
	    let db1 = 
	      if(is_imported x db)
	      then db
	      else 
	      load_theory db bundle (mk_info x tyme info.prot)
	    in 
	    load_parents db1 bundle info xs)


let make_current db data thy = 
  let ps = Theory.get_parents thy
  and name = Theory.get_name thy
  and tyme = Theory.get_date thy
  and prot = true
  in 
  let info = mk_info name (Some tyme) (Some prot)
  in 
  let db1 = load_parents db data info ps
  in 
(*  set_curr db1 thy  *)
  set_current db1 thy  

  end      

let table_as_list db = 
  Treekit.StringTree.to_list (table db)

let print db = 
  let print_tbl ths = 
    Printer.print_sep_list 
    ((fun (n, _) -> Format.print_string n), ",") ths
  in 
  let name = try current_name db with _ -> "(none)"
  in 
  Format.printf "@[<v 2> {@,Current theory: %s@," name;
  Format.printf "Importing: @[<2>";
  Printer.print_sep_list (Format.print_string, ",") (imported db);
  Format.printf "@]@,";
  Format.printf "Table: @[<2>";
  Printer.print_sep_list 
    (print_tbl, ",") (table_as_list db);
  Format.printf "@]@,}@]"
    
