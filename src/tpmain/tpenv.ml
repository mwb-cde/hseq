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

let reset_thydb () = set_theories (thdb())

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
let sym_reset () = Parser.reset()

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
  Printer.add_type_info (pp_info()) id prec fixity repr
let add_type_pp_record id rcrd=
  Printer.add_type_record (pp_info()) id rcrd

(*
let remove_type_pp id=
  Printer.remove_type_info (pp_info()) id
*)

let remove_type_pp id =
  let (_, _, sym) = get_type_pp id
  in 
  Printer.remove_type_info (pp_info()) id;
  Parser.remove_type_token (Lib.get_option sym (name id))

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

let on_load_thy th =
  List.iter (fun (id, rcrd) -> add_id_record id rcrd) 
    (Theory.get_pplist Basic.fn_id th);
  List.iter (fun (id, rcrd) -> add_type_record id rcrd) 
    (Theory.get_pplist Basic.type_id th); ()

let mkterm scp pt = 
  let tenv = 
    Typing.typecheck_env scp (Gtypes.empty_subst()) pt (Gtypes.mk_null ())
  in 
  let nt = Term.retype_pretty tenv pt
  in 
  Typing.check_types scp nt; nt

(* parser functions and error handling *)

let catch_parse_error e a = 
  (try (e a)
  with 
    Pkit.ParsingError x ->
      raise (Result.error ("Parsing error: "^x))
  | Lexer.Error -> raise (Result.error ("Lexing error: "^a)))

let mkterm_raw tyenv pt = Term.set_names (scope()) pt
let mkterm_unchecked tyenv pt =  pt


let read str= 
  mkterm (scope()) 
    (catch_parse_error Parser.read_term str)

let read_unchecked  x=
  mkterm_raw (scope()) 
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
let reset_theoryDB () = reset_thydb()

(* list of initialising functions *)

let init_list = 
  ref 
    [
     pp_init;
     sym_init;
     init_theoryDB 
   ]

let add_init x = init_list:=(x::!init_list)

let init ()=
  List.iter (fun x -> x()) (!init_list)

(* reseting functions *)

let reset_list = ref []

let add_reset x= reset_list:= x::(!reset_list)

let reset() = 
  List.iter(fun x -> x()) (!reset_list);
  init()



