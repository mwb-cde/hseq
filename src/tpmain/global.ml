(*-----
 Name: global.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Basic

open Parser

let dest_name f = Lib.chop_at '.' f

let empty_thy_name = "(empty)"
(* An anonymous theory *)
let anon_thy ()= Theory.mk_thy empty_thy_name

(**
   base_thy:
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
let thdb() = Thydb.emptydb (anon_thy ())

let theories = ref (thdb())
let get_theories () = !theories
let set_theories thdb = theories:=thdb
let reset_thydb () = 
  Thydb.expunge (thdb()) (anon_thy()); 
  set_theories (thdb())

let get_cur_thy () = Thydb.getcur (!theories)
let get_cur_name () = Theory.get_name (get_cur_thy ())
let set_cur_thy thy = 
  theories:=Thydb.setcur_thy (!theories) thy

(* [scope] the standard scope *)
let scope() =
  let thy_name = (get_cur_name())
  in 
  {Gtypes.curr_thy = thy_name;
   Gtypes.typeof_fn = 
   (fun f -> 
     let thstr, idstr = Basic.dest_fnid f
     in 
     Thydb.get_id_type thstr idstr (get_theories()));
   Gtypes.typ_defn = 
   (fun f -> 
     let thstr, idstr = Basic.dest_fnid f
     in 
     Thydb.get_type_rec thstr idstr (get_theories()));

   Gtypes.prec_of = (fun idsel s -> 
     (try 
       let thstr, idstr = Basic.dest_fnid s
       in 
       if idsel = Basic.fn_id
       then (Thydb.get_id_prec thstr idstr (get_theories())) 
       else (-1)
     with _ -> (-1)));
   Gtypes.thy_of = 
   (fun idsel x -> Thydb.thy_of x (thy_name) (get_theories()));
   Gtypes.thy_in_scope  = 
   (fun th1 th2 -> Thydb.thy_in_scope th1 th2 (get_theories()))
 } 

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
let build_thy_file f=  
  let tf = f^Settings.script_suffix
  in 
  try 
    Result.warning ("Trying to build theory "^f);
    Unsafe.use_file ~silent:false  (find_file tf);
    Result.warning ("Built theory "^f)
  with Not_found ->
    raise (Result.error ("Can't find script to build theory "^f))

let find_thy_file f =
  let tf = f^thy_suffix
  in 
  try 
    find_file tf
  with Not_found -> 
    raise (Result.error ("Can't find theory "^f))


(* Pretty printing and Parsing*)

(* tp_pp_info: Printer Table *)
let tp_pp_info=ref (Printer.empty_ppinfo())
let pp_info() = !tp_pp_info 
let pp_set info = tp_pp_info:=info
let pp_reset () = pp_set (Printer.empty_ppinfo())
let pp_init() = pp_reset()

(* tp_sym_info: Parser symbol table *)
let sym_init() = Parser.init()
let sym_info() = Parser.symtable()
let sym_reset () = Parser.init()

(*
let sym_reset () = Parser.reset()
*)
let get_term_pp id=
  Printer.get_term_info (pp_info()) id

let add_term_pp id prec fixity repr=
  Printer.add_term_info (pp_info()) id prec fixity repr;
  Parser.add_token id (Lib.get_option repr (name id)) fixity prec

let add_term_pp_record id rcrd=
  Printer.add_term_record (pp_info()) id rcrd;
  Parser.add_token 
    id 
    (Lib.get_option rcrd.Printer.repr (name id)) 
    (rcrd.Printer.fixity)
    (rcrd.Printer.prec)

let remove_term_pp id =
  let (_, _, sym) = get_term_pp id
  in 
  Printer.remove_term_info (pp_info()) id;
  Parser.remove_token (Lib.get_option sym (name id))


let get_type_pp id=
  Printer.get_type_info (pp_info()) id

let add_type_pp id prec fixity repr=
  Printer.add_type_info (pp_info()) id prec fixity repr;
  Parser.add_type_token id (Lib.get_option repr (name id)) fixity prec

let add_type_pp_record id rcrd=
  Printer.add_type_record (pp_info()) id rcrd;
  Parser.add_type_token 
    id 
    (Lib.get_option rcrd.Printer.repr (name id)) 
    (rcrd.Printer.fixity)
    (rcrd.Printer.prec)

let remove_type_pp id =
  let (_, _, sym) = get_type_pp id
  in 
  Printer.remove_type_info (pp_info()) id;
  Parser.remove_type_token (Lib.get_option sym (name id))

(*
let get_type_pp id=
  Printer.get_type_info (pp_info()) id
let add_type_pp id prec fixity repr=
  Printer.add_type_info (pp_info()) id prec fixity repr
let add_type_pp_record id rcrd=
  Printer.add_type_record (pp_info()) id rcrd
*)

(*
let remove_type_pp id=
  Printer.remove_type_info (pp_info()) id
*)


let get_term_printer id=
  Printer.get_term_printer (pp_info()) id
let add_term_printer id printer=
  Printer.add_term_printer (pp_info()) id (printer (pp_info()))
let remove_term_printer id=
  Printer.remove_term_printer (pp_info()) id

let get_type_printer id=
  Printer.get_type_printer (pp_info()) id
let add_type_printer id printer=
  Printer.add_type_printer (pp_info()) id (printer (pp_info()))
let remove_type_printer id=
  Printer.remove_type_printer (pp_info()) id


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

let load_use_theory_files th = 
     List.iter (Unsafe.load_use_file) th.Theory.cfiles

let default_load_functions = 
  [
(* load files *)
   load_use_theory_files; 
(* add type PP information *)
   (fun th -> 
     List.iter 
       (fun (id, rcrd) -> 
	 add_type_record (Basic.mk_long th.Theory.cname id) rcrd)
       th.Theory.ctype_pps) ;
(* add term PP information *)
   (fun th -> 
     List.iter 
       (fun (id, rcrd) -> 
	 add_id_record (Basic.mk_long th.Theory.cname id) rcrd) 
       th.Theory.cid_pps)
 ]

let load_functions = ref default_load_functions
let init_load_functions () = load_functions:=default_load_functions
let add_load_fn f = load_functions:=(f::!load_functions)

(* on_load_thy: run load_functions, in reverse order *)
let on_load_thy th =
  List.iter (fun f -> f th) (List.rev !load_functions)

let mk_term scp pt = 
  let tenv = 
    Typing.typecheck_env scp (Gtypes.empty_subst()) pt (Gtypes.mk_null ())
  in 
  let nt = Term.retype_pretty tenv pt
  in 
(*  Typing.check_types scp nt;   *)
  nt

(* parser functions and error handling *)

let catch_parse_error e a = 
  (try (e a)
  with 
    Pkit.ParsingError x ->
      raise (Result.error ("Parsing error: "^x))
  | Lexer.Error -> raise (Result.error ("Lexing error: "^a)))

let mk_term_raw tyenv trm = 
  let trm1=Term.set_names (scope()) trm
  in 
  let tyenv = Typing.settype (scope()) trm1
  in 
  Term.retype tyenv trm1

let mk_term_unchecked tyenv pt =  pt

let read str= 
  mk_term (scope()) 
    (catch_parse_error Parser.read_term str)

let read_unchecked  x=
  mk_term_raw (scope()) 
    (catch_parse_error Parser.read_term x)

let read_defn x =
  let (l, r)= 
    catch_parse_error (Parser.read defn_parser) x
  in (l, r)

let read_type_defn x =
  let (l, args, r)= 
    catch_parse_error 
      (Parser.read Parser.typedef_parser) x
  in (match args with None -> (l, [], r) | Some(a) -> (l, a, r))

let read_type x = 
  catch_parse_error
    Parser.read_type x

let read_fulltype x = 
  catch_parse_error Parser.read_type x

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
    let imprts=
      Thydb.load_theory(get_theories()) 
	thy_name false on_load_thy find_thy_file build_thy_file
    in 
    set_cur_thy(Thydb.get_thy (get_theories()) thy_name);
    Thydb.add_importing imprts (get_theories())
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
      Thydb.load_theory(get_theories()) 
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
     sym_init;
     pp_init;
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



