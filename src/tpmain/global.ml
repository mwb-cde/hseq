(*-----
   Name: global.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

open Basic
open Parser

module Thys =
  struct

(***
 * Theories
 ***)

    let empty_thy_name = Basic.null_thy
    let anon_thy ()= Theory.mk_thy empty_thy_name []

(** base_thy: The theory on which all user theories are based *)
    let base_name = ref (Some("Main"))
    let get_base_name()= Lib.dest_option ~err:Not_found (!base_name)
    let set_base_name x = base_name:=(Some(x))
    let clear_base_name () = base_name:=None

(** The theory database *)
    let theoryDB = ref (Thydb.empty())
    let get_theories () = !theoryDB
    let set_theories thdb = theoryDB:=thdb
    let init_theories () = set_theories (Thydb.empty ())

(** The current theory *)
    let current () = Thydb.current (get_theories())
    let current_name () = Theory.get_name (current())
    let set_current thy = 
      theoryDB:=Thydb.set_current (get_theories()) thy

  end

(*** Toplevel theory functions *)
let theories () = Thys.get_theories()
let current () = Thydb.current (theories())
let current_name () = Theory.get_name (current())
let scope() = Thydb.mk_scope (theories())

(***
 * Pretty printing 
 ***)
module PP=
  struct

(*** Printer tables ***)
    let tp_pp_info=ref (Printer.empty_ppinfo())
    let info() = !tp_pp_info 
    let set info = tp_pp_info:=info
    let pp_reset () = set (Printer.empty_ppinfo())

(*** Parser tables ***)
    let sym_init() = Parser.init()
    let sym_info() = Parser.symtable()
    let sym_reset () = Parser.init()

    let init() = pp_reset(); sym_init()

(*** Terms ***)

    let get_term_pp id=
      Printer.get_term_info (info()) id

    let add_term_pp id prec fixity repr=
      Printer.add_term_info (info()) id prec fixity repr;
      Parser.add_token id (Lib.get_option repr (name id)) fixity prec

    let add_term_pp_record id rcrd=
      Printer.add_term_record (info()) id rcrd;
      Parser.add_token 
	id 
	(Lib.get_option rcrd.Printer.repr (name id)) 
	(rcrd.Printer.fixity)
	(rcrd.Printer.prec)

    let remove_term_pp id =
      let (_, _, sym) = get_term_pp id
      in 
      Printer.remove_term_info (info()) id;
      Parser.remove_token (Lib.get_option sym (name id))

(*** Types ***)

    let get_type_pp id=
      Printer.get_type_info (info()) id

    let add_type_pp id prec fixity repr=
      Printer.add_type_info (info()) id prec fixity repr;
      Parser.add_type_token id (Lib.get_option repr (name id)) fixity prec

    let add_type_pp_record id rcrd=
      Printer.add_type_record (info()) id rcrd;
      Parser.add_type_token 
	id 
	(Lib.get_option rcrd.Printer.repr (name id)) 
	(rcrd.Printer.fixity)
	(rcrd.Printer.prec)

    let remove_type_pp id =
      let (_, _, sym) = get_type_pp id
      in 
      Printer.remove_type_info (info()) id;
      Parser.remove_type_token (Lib.get_option sym (name id))

(*** User-defined printers ***)

    let get_term_printer id=
      Printer.get_term_printer (info()) id

    let add_term_printer id printer=
      Printer.add_term_printer (info()) id (printer (info()))

    let remove_term_printer id=
      Printer.remove_term_printer (info()) id

    let get_type_printer id=
      Printer.get_type_printer (info()) id

    let add_type_printer id printer=
      Printer.add_type_printer (info()) id (printer (info()))

    let remove_type_printer id=
      Printer.remove_type_printer (info()) id

(** Functions to add PP information when a theory is loaded *)

    let add_id_record id rcrd =
      let pr, fx, repr = 
	rcrd.Printer.prec, rcrd.Printer.fixity, rcrd.Printer.repr
      in 
      add_term_pp id pr fx repr

    let add_type_record id rcrd =
      let pr, fx, repr = 
	rcrd.Printer.prec, rcrd.Printer.fixity, rcrd.Printer.repr
      in 
      add_type_pp id pr fx repr

    let add_loaded_term_pp th =
      let thy_name = th.Theory.cname
      and pp_list = List.rev th.Theory.cid_pps
      in 
      let add_pp (id, (rcrd, pos)) = 
	add_id_record (Basic.mk_long thy_name id) rcrd;
	let repr = rcrd.Printer.repr
	in 
	match repr with
	  None -> ()
	| Some(sym) -> 
	    (try
	      let id_record = List.assoc id th.Theory.cdefns
	      in 
	      let id_type = id_record.Theory.typ
	      in 
	      Parser.add_overload sym pos
		(Basic.mk_long thy_name id, id_type)
	    with _ -> ())
      in 
      List.iter add_pp pp_list

    let add_loaded_type_pp th =
      let thy_name = th.Theory.cname
      and pp_list = List.rev th.Theory.ctype_pps
      in 
      let add_pp (id, rcrd) = 
	add_type_record (Basic.mk_long thy_name id) rcrd
      in 
      List.iter add_pp pp_list


(***
 * Parsing
 ***)

    let catch_parse_error e a = 
      (try (e a)
      with 
	Pkit.ParsingError x ->
	  raise (Result.error ("Parsing error: "^x))
      | Lexer.Lexing _ -> raise (Result.error ("Lexing error: "^a)))

    let expand_term scp t = 
      let db s = Thydb.get_id_options s (Thys.get_theories())
      in 
      let lookup = 
	Parser.Resolver.make_lookup scp db
      in 
      let (t1, env) = Parser.Resolver.resolve_term scp lookup t
      in 
      let t2 = Term.retype_pretty env t1
      in t2

    let expand_type_names scp t=
      Gtypes.set_name ~strict:false scp t

    let expand_typedef_names scp t=
      match t with
	Parser.NewType (n, args) -> t
      | Parser.TypeAlias (n, args, def) ->
	  Parser.TypeAlias(n, args, expand_type_names scp def)
      | Parser.Subtype (n, args, def, set) ->
	  Parser.Subtype(n, args, 
			 expand_type_names scp def, 
			 expand_term scp set)

    let mk_term scp pt = expand_term scp pt

    let read str= 
      mk_term (scope()) 
	(catch_parse_error Parser.read_term str)

    let read_unchecked  x=
      (catch_parse_error Parser.read_term x)

    let read_defn x =
      let (lhs, rhs)= 
	catch_parse_error (Parser.read defn_parser) x
      in 
      let rhs1=expand_term (scope()) rhs
      in 
      (lhs, rhs1)

    let read_type_defn x =
      expand_typedef_names (scope())
	(catch_parse_error (Parser.read Parser.typedef_parser) x)

    let read_type x = 
      expand_type_names (scope()) (catch_parse_error Parser.read_type x)

    let read_identifier x = 
      catch_parse_error (Parser.read Parser.identifier_parser) x
  end

let read = PP.read
let read_type = PP.read_type
let read_identifier = PP.read_identifier
let read_defn = PP.read_defn
let read_type_defn = PP.read_type_defn

let mk_term t = PP.mk_term (scope()) t

(***
 * File-Handling
 ***)
module Files =
  struct

    let get_cdir () = Sys.getcwd ()

(*** Paths ***)

(** Functions for dealing with paths. *)
    let get_path pth = !pth
    let set_path x pth = pth:=x
    let add_path x pth = set_path (x::(get_path pth)) pth
    let remove_path x pth = 
      set_path (Lib.filter (fun y -> x = y) (get_path pth)) pth

    let thy_path = ref []
    let init_thy_path() = thy_path := ["."; Settings.thys_dir()]

    let get_thy_path ()= get_path thy_path
    let add_thy_path x = add_path x thy_path
    let set_thy_path x = set_path x thy_path
    let remove_from_path x = remove_path x thy_path

    let init_paths() = init_thy_path()

    let find_file f path =
      let rec find_aux ths =
	match ths with
	  [] -> raise Not_found
	| (t::ts) ->
	    let nf = Filename.concat t f
	    in 
	    if Sys.file_exists nf 
	    then nf 
	    else find_aux ts
      in 
      find_aux path

(*** Theory files ***)

    let file_of_thy th = th^Settings.thy_suffix
    let script_of_thy th = th^Settings.script_suffix

    let find_thy_file f =
      let tf = file_of_thy f
      in 
      try find_file tf (get_thy_path())
      with Not_found -> 
	raise (Result.error ("Can't find theory "^f))

(**
   [forbidden]: Theories that can't be used, otherwise a ciruclar
   importing results.
*)
    let forbidden = ref Lib.StringSet.empty
    let init_forbidden () = forbidden:=Lib.StringSet.empty

    let is_forbidden s = Lib.StringSet.mem s (!forbidden)
    let add_forbidden s = forbidden := Lib.StringSet.add s (!forbidden)
    let drop_forbidden s = forbidden := Lib.StringSet.remove s (!forbidden)
	
(**
   [build_thy_file f]: build a theory by running script file f.
*)
    let build_thy_file thydb f=
      let db0 = Thys.get_theories()
      in 
      (if (is_forbidden f)
      then raise (Result.error ("Circular importing, theory "^f))
      else());
      try 
	let script = find_file (script_of_thy f) (get_thy_path())
	in 
	Thys.set_theories(thydb);
	Result.warning ("Trying to build theory "^f);
	add_forbidden f;
	Unsafe.use_file ~silent:false script;
	drop_forbidden f;
	Result.warning ("Built theory "^f);
	let db1 = Thys.get_theories()
	in 
	Thys.set_theories (db0);
	db1
      with Not_found ->
	(Result.warning ("Failed to build theory "^f);
	 raise (Result.error ("Can't find script to build theory "^f)))


(**
   [load_thy_file info]: Load the file storing the theory
   named [info.name] with protection [info.prot] and date no later
   than [info.date]. Finds the file from the path [get_thy_path()].
 *)
    let load_thy_file info = 
      let test_protection prot b =
	match prot with 
	  None -> true
	| (Some p) -> p && b
      in 
      let test_date tym d = 
	match tym with 
	  None -> true
	| (Some tim) -> d <= tim
      in 
      let name = info.Thydb.Loader.name
      and date = info.Thydb.Loader.date
      and prot = info.Thydb.Loader.prot
      in 
      let thyfile =file_of_thy name
      in 
      let rec load_aux ths =
	match ths with
	  [] -> raise Not_found
	| (t::ts) ->
	    let filename = Filename.concat t thyfile
	    in 
	    if Sys.file_exists filename
	    then 
	      let sthy = Theory.load_theory filename
	      in 
	      if (test_protection prot (Theory.saved_prot sthy))
		  && (test_date date (Theory.saved_date sthy))
	      then sthy
	      else load_aux ts
	    else load_aux ts
      in 
      load_aux (get_thy_path())

(** 
   [load_use_theory thy]: Load or use each of the files named in
   theory [thy].
 *)
    let load_use_theory_files thy = 
      List.iter (Unsafe.load_use_file) thy.Theory.cfiles

(***
* Theory inspection functions
***)

(** 
   [default_load_function]: The default list of functions to call on a
   newly-loaded theory.
 *)
    let default_load_functions = 
      [
       load_use_theory_files; (* load files *)
       PP.add_loaded_type_pp;    (* add type PP information *)
       PP.add_loaded_term_pp;    (* add term PP information *)
     ]

    let load_functions = ref default_load_functions
    let add_load_fn f = load_functions:=(f::!load_functions)
    let init_load_functions () = 
      init_forbidden();
      load_functions:=default_load_functions

(** 
   [on_load_thy]: The toplevel function that is passed a newly loaded
   theory. This just passes the theory to the functions in [load_functions].
 *)
    let on_load_thy db th =
      let odb = Thys.get_theories()
      in
	Thys.set_theories db;
	List.iter (fun f -> f th) (List.rev !load_functions);
	Thys.set_theories odb
	

(*** Miscellaneous ***)

(** [loader_data]: Data to use when loading into a theory database. *)
    let loader_data = 
      Thydb.Loader.mk_data on_load_thy load_thy_file build_thy_file

  end

(*** Toplevel file functions *)
let get_thy_path = Files.get_thy_path
let add_thy_path = Files.add_thy_path

(** Initialising functions *)
module Init=
  struct

    (** Setting up the base theory *)

(** [base_thy_builder]: The function to call to build the base theory. 
   This is only called if the base theory can't be loaded from disk.
*)
    let base_thy_builder = ref None
    let set_base_thy_builder f = base_thy_builder:=Some(f)
    let get_base_thy_builder () = !base_thy_builder

(**
   [load_base_thy()]: Try to load the base theory and make it the
   current theory. If unsuccessful: use an empty theory as the
   current theory then if [!base_thy_builder=Some(f)], call [f]
   otherwise clear the base theory name ([clear_base_name()])
 *)
    let load_base_thy ()=
      try
	let thy_name = Thys.get_base_name()
	in 
	let db1 = Thys.get_theories()
	in 
	let db2=
	  Thydb.Loader.load db1 Files.loader_data
	    (Thydb.Loader.mk_info thy_name None None)
	in 
	Thys.set_theories(db2)
      with _ ->
	(* Can't find the base theory or no base theory set *)
	(Thys.clear_base_name();
	 match get_base_thy_builder() with
	   None -> Thys.init_theories()
	 | Some f -> 
	     (Result.warning 
		"Building minimal theory from internal data.");
	     f())

(***
 * Main initialising function 
 ***)

(** [init_list]: The list of functions to call to set-up the system. *)

    let init_theoryDB () = Thys.init_theories(); load_base_thy()

    let init_list = 
      ref 
	[
	 init_theoryDB;
	 PP.init;
	 Files.init_load_functions;
	 Files.init_paths
       ]

    let add_init x = init_list:=(x::!init_list)

(** The toplevel initialising function. *)
    let init ()=
      List.iter (fun x -> x()) (List.rev (!init_list))

(*** Reset functions ***)
(** [reset_list]: The functions to call when the system is to be reset. *)
    let reset_list = ref [Thys.init_theories]
    let add_reset x= reset_list:= x::(!reset_list)

(** The toplevel reset function *)
    let reset() = 
      List.iter (fun x -> x()) (!reset_list); init()
  end

let init () = Init.init()
let reset () = Init.reset()
