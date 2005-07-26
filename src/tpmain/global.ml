(*-----
   Name: global.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

open Basic

open Parser

let dest_name f = Lib.chop_at '.' f

(* let empty_thy_name = "(empty)" *)
let empty_thy_name = Basic.null_thy

(** [anont_thy()]: An anonymous theory *)
let anon_thy ()= Theory.mk_thy empty_thy_name

(**
   [base_thy]:
   The theory on which all user theories are based
 *)
let base_name = ref (Some("Main"))
let get_base_name()= 
  match !base_name with
    None -> raise Not_found
  | Some x -> x
let set_base_name x = base_name:=(Some(x))
let clear_base_name () = base_name:=None

(* theories: the theory database *)
(*
let thdb() = Thydb.empty (anon_thy ())
*)
let thdb() = Thydb.empty ()

let theories = ref (thdb())
let get_theories () = !theories
let set_theories thdb = theories:=thdb
let reset_thydb () = 
  let db = Thydb.empty ()
  in 
  set_theories db

let get_cur_thy () = Thydb.current (get_theories())
let get_cur_name () = Theory.get_name (get_cur_thy ())
let set_cur_thy thy = 
  theories:=Thydb.set_current (get_theories()) thy

(* [scope] the standard scope *)

let scope_term_type f= 
  let thstr, idstr = Basic.dest_fnid f
  in 
  Thydb.get_id_type thstr idstr (get_theories())

let scope_term_thy x = 
  let thy_name = (get_cur_name())
  in 
  Thydb.thy_of x (thy_name) (get_theories())

let scope_type_defn f = 
  let thstr, idstr = Basic.dest_fnid f
  in 
  Thydb.get_type_rec thstr idstr (get_theories())

let scope_type_thy x = 
  let thy_name = (get_cur_name())
  in 
  Thydb.thy_of_type x (thy_name) (get_theories())

let scope_thy_in_scope th1 = 
  if(th1=Basic.null_thy)  (* ignore the empty scope *)
  then true
  else Thydb.thy_in_scope th1 (get_theories())


let scope() = Thydb.mk_scope (get_theories())
(*
  let thy_name = (get_cur_name())
  in 
  {Scope.curr_thy = thy_name;
   Scope.term_type = scope_term_type; 
   Scope.term_thy = scope_term_thy;
   Scope.type_defn = scope_type_defn;
   Scope.type_thy = scope_type_thy;
   Scope.thy_in_scope  = scope_thy_in_scope 
 } 
*)

(* file handling *)

let thy_suffix = "."^Settings.thy_suffix

(* search directories *)
let thy_path = ref []

let init_thy_path() = thy_path := ["."; Settings.thys_dir()]
let get_thy_path ()= !thy_path
let add_thy_path x = thy_path:=(x::!thy_path)
let set_thy_path x = thy_path:=x
let remove_from_path x = 
  thy_path:= (Lib.filter (fun y -> x=y) !thy_path)

let thy_dir = ref "."
let set_thy_dir n = thy_dir := !n
let get_thy_dir () = !thy_dir
let get_cdir () = Sys.getcwd ()

let init_paths() = init_thy_path()

(** [find_file x]: Find file [x] in the theory path. 
   
   raise [Not_found] if not found
 *)
let find_file f =
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
  find_aux (get_thy_path())


(*
   [build_thy_file f]: 
   build a theory by using file f.
 *)
let build_thy_file thydb f=  
  let db0 = get_theories()
  in 
  let tf = f^Settings.script_suffix
  in 
  try 
    set_theories(thydb);
    Result.warning ("Trying to build theory "^f);
    Unsafe.use_file ~silent:false  (find_file tf);
    Result.warning ("Built theory "^f);
    let db1 = get_theories()
    in 
    set_theories (db0);
    db1
  with Not_found ->
    (Result.warning ("Failed to build theory "^f);
     raise (Result.error ("Can't find script to build theory "^f)))

let find_thy_file f =
  let tf = f^thy_suffix
  in 
  try find_file tf
  with Not_found -> 
    raise (Result.error ("Can't find theory "^f))

(**
   [load_thy_file info]: Load the file storing the theory
   named [info.name] with protection [info.prot] and date no later
   than [info.date]. Finds the file from the path [get_thy_path()].
*)
let load_thy_file info = 
  let test_protection prot thy =
    if prot
    then (Theory.get_protection thy)
    else not (Theory.get_protection thy)
  in 
  let test_date tym thy = 
    (tym > (0.0: float)) && ((Theory.get_date thy) <= tym)
  in 
  let name = info.Thydb.Loader.name
  and date = info.Thydb.Loader.date
  and prot = info.Thydb.Loader.protected
  in 
  let thyfile = name^thy_suffix
  in 
  let rec load_aux ths =
    match ths with
      [] -> raise Not_found
    | (t::ts) ->
	let filename = Filename.concat t thyfile
	in 
	if Sys.file_exists filename
	then 
	  let thy = Theory.load_theory filename
	  in 
	  if (test_protection prot thy) && (test_date date thy)
	  then thy
	  else load_aux ts
	else load_aux ts
  in 
  load_aux (get_thy_path())


(* Pretty printing and Parsing*)

module PP=
  struct

(* tp_pp_info: Printer Table *)
    let tp_pp_info=ref (Printer.empty_ppinfo())
    let info() = !tp_pp_info 
    let pp_set info = tp_pp_info:=info
    let pp_reset () = pp_set (Printer.empty_ppinfo())
    let pp_init() = pp_reset()

(* tp_sym_info: Parser symbol table *)
    let sym_init() = Parser.init()
    let sym_info() = Parser.symtable()
    let sym_reset () = Parser.init()

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

(* Functions to add PP information when a theory is loaded *)

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

  end

let load_use_theory_files th = 
  List.iter (Unsafe.load_use_file) th.Theory.cfiles

let add_loaded_term_pp th =
  let thy_name = th.Theory.cname
  and pp_list = List.rev th.Theory.cid_pps
  in 
  let add_pp (id, rcrd) = 
    PP.add_id_record (Basic.mk_long thy_name id) rcrd;
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
	  Parser.add_overload sym (Basic.mk_long thy_name id, id_type)
	with _ -> ())
  in 
  List.iter add_pp pp_list

let add_loaded_type_pp th =
  let thy_name = th.Theory.cname
  and pp_list = List.rev th.Theory.ctype_pps
  in 
  let add_pp (id, rcrd) = 
    PP.add_type_record (Basic.mk_long thy_name id) rcrd
  in 
  List.iter add_pp pp_list

let default_load_functions = 
  [
(* load files *)
   load_use_theory_files; 
(* add type PP information *)
   add_loaded_type_pp;
(* add term PP information *)
   add_loaded_term_pp;
 ]

let load_functions = ref default_load_functions
let init_load_functions () = load_functions:=default_load_functions
let add_load_fn f = load_functions:=(f::!load_functions)

(* on_load_thy: run load_functions, in reverse order *)
let on_load_thy th =
(*   Format.printf "@[Processing theory@ %s@]@." (th.Theory.cname); *)
  List.iter 
    (fun f -> f th) 
    (List.rev !load_functions)


(* parser functions and error handling *)

let catch_parse_error e a = 
  (try (e a)
  with 
    Pkit.ParsingError x ->
      raise (Result.error ("Parsing error: "^x))
  | Lexer.Error -> raise (Result.error ("Lexing error: "^a)))

let expand_term scp t = 
  let db s = Thydb.get_id_options s (get_theories())
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


(*
let mk_term scp pt = 
  let tenv = 
    Typing.typecheck_env scp (Gtypes.empty_subst()) pt (Gtypes.mk_null ())
  in 
  let nt = Term.retype_pretty tenv pt
  in 
(*  Typing.check_types scp nt;   *)
  nt
*)

let mk_term scp pt = 
  expand_term scp pt

let mk_term_raw tyenv trm = mk_term (scope()) trm
(*
let mk_term_raw tyenv trm = 
  let trm1=Term.set_names (scope()) trm
  in 
  let tyenv = Typing.settype (scope()) trm1
  in 
  Term.retype tyenv trm1
*)

let mk_term_unchecked tyenv pt =  pt

let expand_typedef_names scp t=
  match t with
    Parser.NewType (n, args) -> t
  | Parser.TypeAlias (n, args, def) ->
      Parser.TypeAlias(n, args, expand_type_names scp def)
  | Parser.Subtype (n, args, def, set) ->
      Parser.Subtype(n, args, 
		     expand_type_names scp def, 
		     expand_term scp set)


let read str= 
  mk_term_raw (scope()) 
    (catch_parse_error Parser.read_term str)

let read_unchecked  x=
  mk_term_unchecked (scope()) 
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

let read_fulltype x = 
  expand_type_names (scope()) (catch_parse_error Parser.read_type x)

let read_identifier x = 
  catch_parse_error (Parser.read Parser.identifier_parser) x

(* initialising functions *)

(*
   [load_base_thy()]
   try to load the base theory 
   if successful:
   make it the current theory.
   set the base theory name ([set_base_name(...)])

   if unsuccessful:
   use an empty theory as the current theory.
   if [!base_thy_builder=Some(f)] 
   then call [f]
   otherwise clear the base theory name ([clear_base_name()])
 *)

let base_thy_builder = ref None
let set_base_thy_builder f = base_thy_builder:=Some(f)
let get_base_thy_builder () = !base_thy_builder

let load_base_thy ()=
  try
    let thy_name = get_base_name()
    in 
(*
    let data = 
      Thydb.Loader.mk_data on_load_thy find_thy_file build_thy_file false
    in 
*)
    let data = 
      Thydb.Loader.mk_data on_load_thy load_thy_file build_thy_file false
    in 
    let imprts=
      Thydb.Loader.load_theory (get_theories()) thy_name data 
    in 
    set_cur_thy(Thydb.get_thy (get_theories()) thy_name);
    set_theories(Thydb.add_importing (get_theories()) imprts)
  with _ ->
    (* Can't find the base theory or no base theory set *)
    (clear_base_name();
     match get_base_thy_builder() with
       None -> theories:=(thdb())
     | Some f -> 
	 (Result.warning "Building minimal theory from internal function.");
	 f())

(*
   let load_base_thy ()=
   try 
   let imprts=
   Thydb.Loader.load_theory(get_theories()) 
   base_thy_name false on_load_thy find_thy_file
   in 
   set_cur_thy(Thydb.get_thy (get_theories()) base_thy_name);
   Thydb.add_importing imprts (get_theories())
   with _ -> theories:=(thdb())
 *)

let reset_theoryDB () = reset_thydb()
let init_theoryDB () = reset_theoryDB(); load_base_thy()

(* list of initialising functions *)

let init_list = 
  ref 
    [
     init_theoryDB;
     PP.sym_init;
     PP.pp_init;
     init_load_functions;
     init_paths
   ]

let add_init x = init_list:=(x::!init_list)

let init ()=
  List.iter (fun x -> x()) (List.rev (!init_list))

(* reseting functions *)

let reset_list = ref [reset_theoryDB]

let add_reset x= reset_list:= x::(!reset_list)

let reset() = 
  List.iter(fun x -> x()) (!reset_list);
  init()



