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

let thy_suffix = "."^Settings.thy_suffix

let find_thy_file f =
  let tf = f^thy_suffix
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

(*
let typenv()=  stdtypenv()
*)

(* Pretty printing and Parsing*)

(* tp_pp_info: Printer Table *)
let tp_pp_info=ref (Basic.PP.empty_info())
let pp_info() = !tp_pp_info 
let pp_set info = tp_pp_info:=info
let pp_reset () = pp_set (Basic.PP.empty_info())
let pp_init() = pp_reset()

(* tp_sym_info: Parser symbol table *)
let sym_init() = Parser.init()
let sym_info() = Parser.symtable()
let sym_reset () = Parser.reset()

let get_term_pp id=
  Basic.PP.get_term_info (pp_info()) id

let add_term_pp id prec fixity repr=
  Basic.PP.add_term_info (pp_info()) id prec fixity repr;
  Parser.add_token id (Lib.get_option repr (name id)) fixity prec

let add_term_pp_record id rcrd=
  Basic.PP.add_term_record (pp_info()) id rcrd;
  Parser.add_token 
    id 
    (Lib.get_option rcrd.Basic.PP.repr (name id)) 
    (rcrd.Basic.PP.fixity)
    (rcrd.Basic.PP.prec)

let remove_term_pp id =
  let (_, _, sym) = get_term_pp id
  in 
  Basic.PP.remove_term_info (pp_info()) id;
  Parser.remove_token (Lib.get_option sym (name id))

let get_type_pp id=
  Basic.PP.get_type_info (pp_info()) id

let add_type_pp id prec fixity repr=
  Basic.PP.add_type_info (pp_info()) id prec fixity repr

let add_type_pp_record id rcrd=
  Basic.PP.add_type_record (pp_info()) id rcrd

let remove_type_pp id=
  Basic.PP.remove_type_info (pp_info()) id

let remove_type_pp id =
  let (_, _, sym) = get_type_pp id
  in 
  Basic.PP.remove_type_info (pp_info()) id;
  Parser.remove_type_token (Lib.get_option sym (name id))

(*
   parser/printer information on reserved identifiers  
 *)

(* reserved words *)

(*
let base_id_list = 
  [Logicterm.notid, 5, false, "";
   Logicterm.andid, 4, true, "and";
   Logicterm.orid, 3, true, "or";
   Logicterm.iffid, 1, true, "iff";
   Logicterm.impliesid, 1, true, "=>";
   Logicterm.equalsid, 6, true, "="]

let base_type_list = []

let build_id_info () =
  List.iter 
    (fun (sym, id, fx, pr) -> 
      add_term_pp id pr fx (if sym="" then None else Some(sym)))
    Parser.reserved_words

let build_type_info () =
  List.iter 
    (fun (id, pr, fx, trn) -> 
      add_type_pp id pr fx (if trn="" then None else Some(trn)))
    base_type_list
*)

(* Functions to add PP information when a theory is loaded *)

let add_id_record id rcrd =
  let pr, fx, repr = 
    rcrd.Basic.PP.prec, rcrd.Basic.PP.fixity, rcrd.Basic.PP.repr
  in 
  add_term_pp id pr fx repr

let add_type_record id rcrd =
  let pr, fx, repr = 
    rcrd.Basic.PP.prec, rcrd.Basic.PP.fixity, rcrd.Basic.PP.repr
  in 
  add_type_pp id pr fx repr

let on_load_thy th =
  List.iter (fun (id, rcrd) -> add_id_record id rcrd) 
    (Theory.get_pplist Basic.fn_id th);
  List.iter (fun (id, rcrd) -> add_type_record id rcrd) 
    (Theory.get_pplist Basic.type_id th); ()

let mkterm scp pt = 
  let tenv = 
    Typing.typecheck_env scp (Gtypes.empty_subst()) pt (Gtypes.mk_null ())
  in 
  let nt = Typing.retype_pretty tenv pt
  in 
  Typing.check_types scp nt; nt

(* parser functions and error handling *)

let catch_parse_error e a = 
  (try (e a)
  with 
    Pkit.ParsingError x ->
      Result.raiseError("Parsing error: "^x)
  | Lexer.Error -> Result.raiseError("Lexing error: "^a)) 

let mkterm_unchecked tyenv pt = pt

let read str= 
  mkterm (scope()) 
    (catch_parse_error Parser.read_term str)

let read_unchecked  x=
  mkterm_unchecked (scope()) 
    (catch_parse_error Parser.read_term x)

let read_defn x =
  let (l, r)= 
    catch_parse_error (Parser.read defn_parser) x
  in (l, mkterm_unchecked (scope()) r)

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

(* list of initialising functions *)

let init_list = 
  ref 
    [
     pp_init;
     sym_init;
     init_theoryDB 
(*     build_id_info; build_type_info *)
   ]

let add_init x = init_list:=(x::!init_list)

let init ()=
  List.iter (fun x -> x()) (!init_list)

