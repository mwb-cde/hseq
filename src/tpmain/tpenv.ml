open Basic

open Parser

  let dest_name f = Lib.chop_at '.' f

let empty_thy_name = ""
let base_thy_name = "base"

  let base_thy ()= Theory.mk_thy empty_thy_name

  let thdb() = Thydb.emptydb (base_thy ())
  let theories = ref (thdb())


  let get_theories () = !theories
  let set_theories thdb = theories:=thdb

  let get_cur_thy () = Thydb.getcur (!theories)
  let get_cur_name () = Theory.get_name (get_cur_thy ())

  let stdtypenv() =
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

  let set_cur_thy thy = 
    theories:=Thydb.setcur_thy (!theories) thy

(* file handling *)

let thy_path = ref ["."]
let get_thy_path ()= !thy_path
let add_thy_path x = thy_path:=(x::!thy_path)
let set_thy_path x = thy_path:=x
let remove_from_path x = 
  thy_path:= (Lib.filter (fun y -> x=y) !thy_path)

let thy_dir = ref "."
let set_thy_dir n = thy_dir := !n
let get_thy_dir () = !thy_dir
let get_cdir () = Sys.getcwd ()

let thy_suffix = Settings.thy_suffix

let find_thy_file f =
  let tf = f^"."^thy_suffix
  in 
  let rec find_aux ths =
    match ths with
      [] -> raise Not_found
    | (t::ts) ->
	let nf = Filename.concat t tf
	in 
	if Sys.file_exists nf 
	then nf else find_aux ts
  in find_aux (get_thy_path())

let typenv ()=  stdtypenv()

(* parsing and pretty printing *)

type pp_info = (Basic.fnident , Corepp.pp_rec) Hashtbl.t
let empty_pp_info ()= Hashtbl.create 5

let get_pp name pp_ls= Hashtbl.find pp_ls name 
(*
let add_pp name pp_rec pp_ls = 
  Hashtbl.add pp_ls name pp_rec;
  match (pp_rec.Corepp.repr) with
    None -> ()
  | Some(x) -> 
      Termlexer.Lex.add_keyword x (Termlex.ID name)
*)

(* add_pp: needs to be fixed *)
let add_pp name pp_rec pp_ls = 
  Hashtbl.add pp_ls name pp_rec
(*  match (pp_rec.Corepp.repr) with
    None -> ()
  | Some(x) -> 
      Parser.add_symbol name x 
	(if pp_rec.Corepp.infix then Lexer.infix else Lexer.nonfix)
	Lexer.left_assoc
	pp_rec.Corepp.prec
*)

let remove_pp name pp_ls = 
  (match (get_pp name pp_ls).Corepp.repr with
    None -> ()
  | Some(x) ->  Parser.remove_symbol x
   );
  Hashtbl.remove pp_ls name

(*
   parser/printer information on reserved identifiers  
*)

(* reserved words *)

let base_id_list = 
  [Logicterm.notid, 5, false, "";
    Logicterm.andid, 4, true, "and";
    Logicterm.orid, 3, true, "or";
    Logicterm.iffid, 1, true, "iff";
    Logicterm.impliesid, 1, true, "=>";
    Logicterm.equalsid, 6, true, "="]

let base_type_list = []

let id_pp_info = empty_pp_info()
let type_pp_info = empty_pp_info()

let build_id_info () =
  Hashtbl.clear id_pp_info;
  List.iter 
    (fun (sym, id, fx, assoc, pr) -> 
      add_pp id 
	(Corepp.mk_pp_rec pr 
	   (Parserkit.Info.is_infix fx)
	   (if sym="" then None else Some(sym)))
	id_pp_info)
    Parser.reserved_words
(*
let build_id_info () =
  Hashtbl.clear id_pp_info;
  List.iter 
    (fun (x, pr, inf, trn) -> 
      add_pp x 
	(Corepp.mk_pp_rec pr inf (if trn="" then None else Some(trn)))
	id_pp_info)
    base_id_list
*)

let build_type_info () =
  Hashtbl.clear type_pp_info;
  List.iter 
    (fun (x, pr, inf, trn) -> 
      add_pp x 
      (Corepp.mk_pp_rec pr inf trn) type_pp_info)
    base_type_list

let get_id_info x =  get_pp x id_pp_info
let add_id_info x pr = 
  add_pp x pr id_pp_info

let remove_id_info x = remove_pp x id_pp_info

let get_type_info x = get_pp x type_pp_info
let add_type_info x pr = add_pp x pr type_pp_info
let remove_type_info x = remove_pp x type_pp_info

let empty_pp_rec= Corepp.empty_pp_rec()

let base_pp_state () = 
  { Corepp.id_info = 
    (fun x -> try get_id_info x  with Not_found -> empty_pp_rec) ;
    Corepp.type_info = 
    (fun x -> try get_type_info x with Not_found -> empty_pp_rec)
  } 

let is_infix idsel name =
  let precrd = 
    (try
      if idsel = Basic.fn_id 
      then get_id_info name
      else get_type_info name
    with Not_found -> empty_pp_rec)
  in precrd.Corepp.infix

let prec_of idsel name =
  let precrd = 
    (try
      if idsel = Basic.fn_id 
      then get_id_info name
      else get_type_info name
    with Not_found -> empty_pp_rec)
  in precrd.Corepp.prec

let on_load_thy th =
  List.iter (fun (x, y) -> add_id_info x y) 
    (Theory.get_pplist Basic.fn_id th);
  List.iter (fun (x, y) -> add_type_info x y) 
    (Theory.get_pplist Basic.type_id th); ()

let mkterm tyenv pt = 
  let tenv = Gtypes.empty_subst()
  in (ignore(Typing.typecheck_env tyenv tenv pt (Gtypes.mk_null ()));
      let nt = Typing.retype tenv pt
      in Typing.check_types tyenv nt; nt)

(* parser functions and error handling *)

let catch_parse_error e a = 
  (try (e a)
  with 
    Pkit.ParsingError x ->
      Result.raiseError("Parsing error: "^x)
  | Lexer.Error -> Result.raiseError("Lexing error: "^a)) 

  let mkterm_unchecked tyenv pt = pt

  let read str= 
    mkterm (typenv()) 
      (catch_parse_error (Parser.read_term (typenv())) str)

  let read_unchecked  x=
    mkterm_unchecked (typenv()) 
      (catch_parse_error (Parser.read_term (typenv())) x)

  let read_defn x =
    let (l, r)= 
      catch_parse_error (Parser.read defn_parser (typenv())) x
    in (l, mkterm_unchecked (typenv()) r)

let read_type_defn x =
  let (l, args, r)= 
    catch_parse_error 
      (Parser.read Parser.typedef_parser (typenv())) x
    in (match args with None -> (l, [], r) | Some(a) -> (l, a, r))

let read_type x = 
  catch_parse_error
    (Parser.read_type (typenv())) x

  let read_fulltype x = 
    catch_parse_error (Parser.read_type (typenv())) x

let read_identifier x = 
  catch_parse_error (Parser.read Parser.identifier_parser (typenv())) x

(*
let printer_info = 
  [("not", (-1, false)); 
      ("equals", (5, false));
      ("and", (4, true));
      ("or", (4, true));
      ("implies", (2, true));
      ("iff", (1, true))]
*)

(* initialising functions *)

(* try to load the base theory *)
(* use an empty theory if unsuccessful *)

let load_base_thy ()=
  try 
    let imprts=
      Thydb.load_theory(get_theories()) 
	base_thy_name false on_load_thy find_thy_file
    in 
    set_cur_thy(Thydb.getthy (get_theories()) base_thy_name);
    Thydb.add_importing imprts (get_theories())
  with _ -> theories:=(thdb())

let init_theoryDB () = load_base_thy()

(* theories:=(thdb()) *)

(* list of initialising functions *)

let init_list = 
  ref 
    [
     init_theoryDB; 
     Parser.init;
     build_id_info; build_type_info
   ]

let add_init x = init_list:=(x::!init_list)

let init ()=
  List.iter (fun x -> x()) (!init_list)

