
module Pkit=Parserkit.Grammars
    (struct 
      type tokens=Lexer.tok 
      let matches = Lexer.match_tokens
      let string_of_token = Lexer.message_of_token
    end)

(* default precedence, fixity and associativity  *)
let default_term_prec = 0
let default_term_assoc = Parserkit.Info.non_assoc
let default_term_fixity=Parserkit.Info.nonfix

let default_type_prec = 0
let default_type_assoc = Parserkit.Info.non_assoc
let default_type_fixity= Parserkit.Info.nonfix


module Grammars  =
struct

  open Lexer
  open Pkit
  open Term

  type input = Pkit.input
  type 'a phrase = 'a Pkit.phrase

(* 
   token tables
   information about symbols
*)

type token_info =
    (Basic.ident
      * Parserkit.Info.fixity 
      * int) option

(* token table 
   stores the identifier associated with a symbol
   the fixity and precedence of the symbol
   the table is memoised
*)
let default_table_size=253;

type token_table = 
    { 
      mutable memo: (token * token_info) option;
      table: (token, token_info) Hashtbl.t
    }

let token_table_new()=
  {memo = None; table=Hashtbl.create default_table_size}

let token_table_reset tbl=
  tbl.memo<-None;
  Hashtbl.clear tbl.table

(* token_table_add: 
   should fail if token already exists
   (but doesn't *fixme* )
*)

let token_table_add tbl s tok=
  Hashtbl.add (tbl.table) s tok
(*
   let token_table_add tbl s tok=
   try 
    ignore(Hashtbl.find tbl.table s);
    Result.raiseError 
      ("Parsing information for "^(message_of_token s)^" already exists")
  with 
    Not_found -> Hashtbl.add (tbl.table) s tok
*)

let token_table_find tbl s=
  let mfind =
    match tbl.memo with
      Some(x, y) -> 
	if Lexer.match_tokens s x
	then Some(y) 
	else None
    | _ -> None
  in 
  match mfind with
    Some(r) -> r
  | _ -> 
      let ret=Hashtbl.find (tbl.table) s
      in 
      tbl.memo<-Some(s, ret); ret

let token_table_remove tbl s=
  Hashtbl.remove tbl.table s;
  match tbl.memo with
    Some(x, y) ->
      if Lexer.match_tokens s x 
      then tbl.memo<-None 
      else ()
  | _ -> ()


(* token information *)

type fixity=Parserkit.Info.fixity
let nonfix=Parserkit.Info.nonfix
let infix=Parserkit.Info.infix
let prefix=Parserkit.Info.prefix
let suffix=Parserkit.Info.suffix
    
type associativity=Parserkit.Info.associativity
let non_assoc=Parserkit.Info.non_assoc
let left_assoc=Parserkit.Info.left_assoc
let right_assoc=Parserkit.Info.right_assoc

let token_info tbl t= 
  match t with
   Sym s ->  
     (try
       token_table_find tbl t
     with Not_found -> None)
  | _ -> None

  type infotyp = 
      { 
	(* term information *)
	bound_names: (string* Basic.term) list ref;
	token_info: token -> token_info;
	  
        (* type information *)
        typ_indx : int ref;
        typ_names: (string* Basic.gtype) list ref;
	  
        type_token_info: token ->token_info;
      }

(* utility functions *)

  let string_of_tok tok = Lexer.message_of_token tok

  let string_tokens toks =
    Lib.list_string string_of_tok " " toks

  let get_type_indx inf = 
    inf.typ_indx:= (!(inf.typ_indx))+1; !(inf.typ_indx)

  let mk_vartyp inf = 
    Gtypes.mk_var
      ("typ"^(string_of_int (get_type_indx inf)))

  let lookup_name n inf = 
   List.assoc n !(inf.bound_names)
  let add_name n trm inf = 
   inf.bound_names:=(n, trm)::!(inf.bound_names)
  let drop_name n inf = 
    let rec d_aux ls =
      match ls with
	[] -> []
      | ((x, t)::xs) -> 
	  if(x=n) 
	  then xs
	  else (x, t)::(d_aux xs)
    in 
    inf.bound_names:=d_aux !(inf.bound_names)

  let get_term n inf = 
    (try lookup_name n inf 
    with Not_found -> Term.mkshort_var n)
  let clear_names inf = inf.bound_names:=[]

  let lookup_type_name n inf = 
    List.assoc n !(inf.typ_names)

  let add_type_name n ty inf = 
    inf.typ_names := (n, ty)::!(inf.typ_names); ty

  let get_type n inf = 
    (try lookup_type_name n inf 
    with Not_found -> 
      add_type_name n (Gtypes.mk_var n) inf)

  let clear_type_names inf = 
    inf.typ_names:=[]

  let mk_token_info x =
    match x with
      Some(_, f, p) ->  {fixity=f; prec=p}
    | _ -> {fixity = default_term_fixity; 
	    prec=default_term_prec}

  let mk_type_token_info tbl t=
    match (token_info tbl t) with
      Some(_, f, p)-> 
	{fixity=f; prec=p}
    | _ -> {fixity=default_type_fixity; 
	    prec=default_type_prec}

  let mk_empty_inf tbl type_tbl= 
    { 
      bound_names = ref [];
      token_info = token_info tbl;

      typ_indx = ref 0;
      typ_names = ref [];
      type_token_info = token_info type_tbl
    }

  let mk_inf tbl type_tbl = 
    { 
      bound_names = ref [];
      token_info = token_info tbl;

      typ_indx = ref 0;
      typ_names = ref [];
      type_token_info = token_info type_tbl
    }

  let rec alternates phl inf toks= 
    match phl with
      [] -> raise (ParsingError "No alternative parsers")
    | (ph::phs) -> 
	(try (ph inf) toks 
	with 
	  ParsingError _ -> (alternates phs inf toks))

  let error ?msg inp =  
    let str=
      match msg with None -> ""
      | Some(m) -> (": "^m)
    in 
    try 
      let tok, _ = next_token inp
      in 
      raise 
	(ParsingError
	   ("error at "^(string_of_token tok)^str))
    with _ -> raise (ParsingError str)

  let mk_type_binary_constr inf t=
    let lookup x =
      try inf.token_info x
      with Not_found -> None
    in 
    match t with
      Sym RIGHTARROW -> Gtypes.mk_fun
    | ID(s) -> (fun x y -> Gtypes.mk_def s [x;y])
    | _ ->
	match (lookup t) with
	  Some(name, _, _) -> 
	    (fun x y-> Gtypes.mk_def name [x; y])
	| _ -> 
	  raise (ParsingError 
		    ((string_of_tok t)^" is not a type constructor"))


  let mk_type_unary_constr inf t=
    let lookup x =
      try inf.token_info x
      with Not_found -> None
    in 
    match t with
      ID(s) -> (fun x -> Gtypes.mk_def s [x])
    | _ ->
	match (lookup t) with
	  Some(name, _, _)->
	    (fun x -> Gtypes.mk_def name [x])
	| _ -> 
	    raise (ParsingError 
		     ((string_of_tok t)^" is not a unary type constructor"))

  let mk_conn idsel inf t= 
    let lookup x =
      try inf.token_info x
      with Not_found -> None
    in 
    match t with 
     ID(i) -> 
       (fun x y -> Term.mkfun i [x; y])       
    | _ -> 
	match (lookup t) with
	  Some (name, _, _) ->
	    (fun x y -> Term.mkfun name [x; y])
	| _ ->
	  raise (ParsingError ((string_of_tok t)^" is not a connective"))


  let mk_prefix idsel inf t= 
    let lookup x =
      try inf.token_info x
      with Not_found -> None
    in 
    match t with 
     ID(i) -> 
       (fun x -> Term.mkfun i [x])
    | _ -> 
	match (lookup t) with
	  Some(name, _, _) -> 
	    (fun x -> Term.mkfun name [x])
	| _ -> 
	  raise (ParsingError ((string_of_tok t)^" is not a prefix"))

(* id/named_id/type_id
   parsers to read an identifier.
   compilicated by the need to look up symbols
   to test if the symbol maps to an identifier
   id: read an identifier
   type_id: read an identifier for the type parser

   named_id: read a specific (given) identifier
*)

  let id_parser info inp =
    let get_info x = info x
    in 
    let comp x= 
      match x with 
	ID _ -> true 
      | _ -> 
	  match (get_info x) with
	    Some (name, _, _) -> true
	  | _ -> false
    and mk x = 
      match x with 
	ID(s) -> s
      | _ -> 
	  (match (get_info x) with
	    Some(name, _, _) -> name
	  | _ ->  failwith "parser: id.")
    in 
    try Pkit.get comp mk inp
    with No_match -> raise (ParsingError "(id) Not an identifier")


  let id info inp = 
    let lookup x =
      info.token_info x
    in 
    (id_parser lookup inp)


(* 
   type_id: take into account that symbol -> should be 
   treated as an identifer 
*)
  let type_id info inp =
    let get_info x = 
      try info.type_token_info x
      with Not_found -> None
    in 
    let comp x= 
      match x with 
	ID _ -> true
      | Sym(RIGHTARROW) -> true
      | _ -> 
	  match (get_info x) with
	    Some (name, _, _) -> true
	  | _ -> false
    and mk x = 
      match x with 
	ID(s) -> s
      | Sym(RIGHTARROW) -> Basic.mkname "->"
      | _ -> 
	  (match (get_info x) with
	    Some(name, _, _) -> name
	  | _ ->  failwith "parser: id.")
    in 
    try Pkit.get comp mk inp
    with No_match -> raise (ParsingError "(id) Not an identifier")

  let named_id info idparser name inp =
    ((id info) >> 
     (fun x -> 
       if(x=name) 
       then x 
       else 
	 raise (ParsingError ("Expected identifier "
			      ^(Basic.string_fnid name)^" but got "
			      ^(Basic.string_fnid x)))))
      inp

  let number inp = 
    let comp x = match x with NUM _ -> true | _ -> false 
    and mk x = 
      match x with 
	NUM s -> Num.num_of_string s 
      | _ -> failwith "parser: number"
    in 
    try get comp mk inp
    with No_match -> raise (ParsingError "Not a number")

  let boolean inp = 
    let comp x = match x with BOOL _ -> true | _ -> false 
    and mk x = 
      match x with 
	BOOL b -> b
      | _ -> failwith "parser: boolean"
    in 
    try get comp mk inp
    with No_match -> raise (ParsingError "Not a boolean")

  let none inp = empty inp

  let bool_type info toks =
    try 
      ((named_id info type_id (Basic.mkname "bool"))
	 >> (fun _ -> Gtypes.mk_bool)) toks
    with No_match -> raise (ParsingError "Not a boolean type")

  let num_type info toks =
    try 
      ((named_id info type_id (Basic.mkname "num"))
	 >> (fun _ -> Gtypes.mk_num)) toks
    with No_match -> raise (ParsingError "Not a number type")

  let comma_list ph toks= 
    ( ((ph -- (repeat (!$(Sym COMMA) $-- ph))) 
	 >> (fun (x, y) -> x::y))
    ||  empty) toks

  let listof ph toks=  repeat ph toks
      
  let rec repeat_term ph term toks =
      (((ph -- (repeat_term ph term )) >> (fun (x, y) -> x ::y))
     || (term >> (fun _ -> []))) toks

  let tlistof ph tm toks= 
    ( ((ph -- (repeat_term ph tm))
	 >> (fun (x, y) -> (x::y)))
    || (tm >> (fun x -> []))) toks

  let short_id id inf toks =
    ((id inf >> 
      (fun x -> 
	match (Basic.dest_fnid x) with 
	  ("", s) -> s | _ -> raise (ParsingError "Not a short identifier"))) 
       toks)

  let long_id id inf toks = 
    (id inf >> 
     (fun x -> 
       match (Basic.dest_fnid x) with 
	 (_, "") -> raise (ParsingError "Badly formed identifier")
       | _ -> x))
      toks
      
  let mk_short_id id inf toks =
    (long_id id inf >> (fun x -> Basic.name x)) toks

(*
   core_type_parsers
     | num_type inf
     | bool_type inf 
*)

  let core_type_parsers =  []

  let other_type_parsers_list  =  ref core_type_parsers

  let add_type_parser pos n ph = 
    other_type_parsers_list:=
      Lib.named_add (!other_type_parsers_list) pos n ph

  let remove_type_parser n =
    other_type_parsers_list:=List.remove_assoc n (!other_type_parsers_list)

  let other_type_parsers inf toks = 
    named_alt (!other_type_parsers_list) inf toks

  let rec inner_types inf toks =
    (operators (atomic_types inf, 
		(fun x -> mk_token_info (inf.type_token_info x)), 
		mk_type_binary_constr inf, mk_type_unary_constr inf) toks) 
  and atomic_types inf toks =
      ((
       ((!$(Sym ORB) -- ((inner_types inf) -- !$(Sym CRB)))
	  >> (fun x -> fst (snd x)))
     || ((!$(Sym PRIME) -- short_id id inf )
	   >> (fun (_, x) -> get_type x inf))
     || num_type inf
     || bool_type inf
     || (((long_id id inf) -- 
	    (optional 
	       ((!$(Sym ORB) -- ((comma_list (inner_types inf))
				   -- (!$(Sym CRB))))
		  >> (fun (_, (args, _)) -> args))))
	   >> (fun (i, a) -> 
	     match a with
	       None -> Gtypes.mk_def i []
	     | Some(ts) -> Gtypes.mk_def i ts))
     || (other_type_parsers inf) 
     || error ~msg:"unknown construct in type.")
	 toks)

  let rec types inf toks = 
    (clear_type_names inf; inner_types inf toks)
      

  let typedef inf toks = 
    let type_name inf toks =
      ((!$(Sym PRIME) -- (short_id type_id inf))>> (fun (_, x) -> x)) toks
    in 
    (((short_id type_id inf) 
       -- 
       ((optional 
	  ((!$(Sym ORB)-- ((comma_list (type_name inf)) -- (!$(Sym CRB))))
	     >> (fun (_, (x, _)) -> x)))
       -- 
       (optional 
	  (((!$(mk_symbol Logicterm.equalssym))
	      -- (types inf)) >> (fun (_, x) -> x)))))
      >> (fun (x, (y, z)) -> (x, y, z))) toks

  let rec mkcomb x y = 
    match y with 
      [] -> x
    | t::ts -> mkcomb (mkapp x t) ts

  let id_type_op idnt inf toks=
    (( (((!$(Sym ORB)) -- (idnt inf) 
	   -- (!$(Sym COLON))-- (types inf) -- (!$(Sym CRB)))
	  >> (fun ((((_, i), _), t), _) -> (i, t)))
  || ( (idnt inf) >> (fun x -> (x, mk_vartyp inf))))
    toks)

  let optional_type inf =     
    ( (( !$(Sym COLON) -- (types inf)) >> (fun (_, ty) -> Some(ty)))
    || (empty >> (fun x -> None))) 

(* 
   Term parser, 
   keeps a record of bound records 
*)

(* 
   utility functions to create bound variable and quantified terms 
   (for use in the term parser)
*)
(*
   qnt_setup_bound_names inf qnt xs
   make bound variables from the name-type pairs in xs,
   add them to inf.bound_names
   qnt is the quantifier type (All, Ex or Lambda)
*)

  let qnt_setup_bound_names inf 
      (qnt: Basic.quant_ty) (xs : (string* Basic.gtype) list) =
    List.map 
      (fun (n, ty) -> 
	let b_id=Term.mkbound(Basic.mk_binding qnt n ty)
	in 
	add_name n b_id inf;
	(n, b_id)) xs

(*
   qnt_term_remove inf xs body
   use bound names in xs to form a quantified term, with body
   as the initial term.
   simplified example:  [!x, ?y, !z] t -> (!x: (?y: (!z: t)))

   remove each name in xs from inf.bound_names as it is used.
*)

  let qnt_term_remove_names inf (xs : (string* Basic.term) list) body=
    List.fold_right
      (fun (x, y) b ->
	let binder=dest_bound y
	in 
	let nt=Basic.Qnt(Basic.binder_kind binder, binder, b)
	in 
	drop_name x inf; nt) xs body

(* term_identifer inf:
   parse a possibly typed identifer satifying
      id | "(" id ":" type ")" 
   lookup identifier in inf, to check if it is a bound variable
   if not, it is a free variable
*)

   let term_identifier inf toks =
   ((id_type_op (long_id id) inf) 
   >>
   (fun ((n, i), t) -> 
   let nid=Basic.mklong n i
   in 
   if(Basic.is_short_id nid)
   then 
   try lookup_name i inf
   with Not_found -> mk_typed_var nid t
   else 
   mk_typed_var nid t)) toks

       
(* 
   Support for Ocaml anti-quotation.
   An anti-quotation expression must evaluate to a term 
   ANTI-QUOTATION NOT SUPPORTED
*)
(*
  let antiquote_parser inp = 
    let comp x= match x with ANTIQUOTE _ -> true | _ -> false
    and mk x =
      match x with 
	ANTIQUOTE s -> s
      | _ -> raise (ParsingError "parser: Not an antiquote")
    in 
    try
      get comp mk inp
    with No_match -> raise (ParsingError "parser: Not an antiquote")
*)    

(* term parsers *)
(* primary term parsers by named list *)
  let core_term_parser_list = 
     [ 
(* id "(" id ":" type ")" *)
      "identifier", term_identifier;
(*   | number *)
      "number", (fun _ -> (number >> (fun x -> mknum x)));
(*   | boolean *)
    "boolean", (fun _ -> (boolean >> (fun x -> mkbool x)));
     ]

  let other_parsers_list  = ref core_term_parser_list
  let other_parsers inf toks = 
    named_alt (!other_parsers_list) inf toks
  let add_parser pos n ph = 
    other_parsers_list:=Lib.named_add (!other_parsers_list) pos n ph

  let remove_parser n = 
    other_parsers_list:=List.remove_assoc n (!other_parsers_list)


(*
  let other_parsers inf toks = 
    alternates (!other_parsers_list) inf toks
*)


(* topmost term parser *)

  let rec form inf toks =
    (
     ((formula inf)-- (listof (formula inf)))
       >> (fun (x, y) -> mkcomb x y)
    ) toks

  and formula inf toks= 
    (
     operators(typed_primary inf, 
	       (fun x-> mk_token_info (inf.token_info x)), 
	     mk_conn Basic.fn_id inf, mk_prefix Basic.fn_id inf)
    ) toks
  and typed_primary inf toks =
      (
       ((primary inf) --  (optional_type inf))
	 >> 
       (fun (t, pty) -> 
	 match pty with None -> t | Some(ty) -> mktyped t ty)
      ) toks
  and
      primary inf toks = 
    (
(* "(" form ")" *)
     ( (( !$ (Sym ORB) -- ((form inf) -- !$(Sym CRB))))
	 >> (fun (_, (x, _)) ->  x))

(* "ALL" { id_type_op }+ ":" form *)
   || (((( !$ (Key ALL) 
           -- ((id_type_op (short_id id) inf)
		 -- ((listof (id_type_op (short_id id) inf))
		       -- (!$(Sym COLON)))))
	 >> 
	(fun (_, (v, (vs, _)))
	   ->
	     qnt_setup_bound_names inf Basic.All (v::vs)))
	 -- (form inf))
	 >> 
       (fun ((xs:(string*Basic.term)list), body)
	 ->
	   qnt_term_remove_names inf xs body))

(* "EX" { id_type_op }+ ":" form *)
   || (((( !$ (Key EX) 
           -- ((id_type_op (short_id id) inf)
		 -- ((listof (id_type_op (short_id id) inf))
		       -- (!$(Sym COLON)))))
	 >> 
	(fun (_, (v, (vs, _)))
	   ->
	     qnt_setup_bound_names inf Basic.Ex (v::vs)))
	 -- (form inf))
	 >> 
       (fun ((xs:(string*Basic.term)list), body)
	 ->
	   qnt_term_remove_names inf xs body))
(* "LAM" { id_type_op }+ ":" form *)
   || (((( !$ (Key LAM) 
           -- ((id_type_op (short_id id) inf)
		 -- ((listof (id_type_op (short_id id) inf))
		       -- (!$(Sym COLON)))))
	 >> 
	(fun (_, (v, (vs, _)))
	   ->
	     qnt_setup_bound_names inf Basic.Lambda (v::vs)))
	 -- (form inf))
	 >> 
       (fun ((xs:(string*Basic.term)list), body)
	 ->
	   qnt_term_remove_names inf xs body))
(* | id 
   | "(" id ":" type ")" 
   |  number
   | boolean
   | alternative_parsers
   | error 
*)
   || (other_parsers inf)
   || (error ~msg:"unknown construct in term.")
    ) toks

  let message m _ =  raise (ParsingError m)

  let rec lhs inf toks=
    ((((id_type_op (mk_short_id id) inf) 
	 -- (args_opt inf))
	>> (fun ((n, t) , args) -> (n, args))) 
    || error ~msg:"badly formed identifier for definition.")
       toks
  and args_opt inf toks= 
   ( repeat_term
      (id_type_op (short_id id) inf) 
      (!$(mk_symbol Logicterm.equalssym))
    || error ~msg:"badly formed argument list for definition.")
      toks
  and defn inf toks =
     (
      (((lhs inf) -- (form inf))
	   >> (fun (l, r) -> (l, r)))
    || (error ~msg:"Badly formed defintion"))
      toks


end

(*
   Tpparser: toplevel for parsing functions
*)

open Lexer
open Logicterm

type ('a)parse = Pkit.input -> 'a
type ('a)phrase = 'a Pkit.phrase

(* token fixity and associativity (exactly the same as in Lexer) *)

type fixity=Parserkit.Info.fixity
let nonfix=Parserkit.Info.nonfix
let infix=Parserkit.Info.infix
let prefix=Parserkit.Info.prefix
let suffix=Parserkit.Info.suffix
    
type associativity=Parserkit.Info.associativity
let non_assoc=Parserkit.Info.non_assoc
let left_assoc=Parserkit.Info.left_assoc
let right_assoc=Parserkit.Info.right_assoc

(* reserved words *)


(* reserved words *)


let syms_list = 
  [(".", Sym DOT); 
   ("(", Sym ORB);
   (")", Sym CRB); 
   (",", Sym COMMA);
   ("->", Sym RIGHTARROW); 
   ("'", Sym PRIME);
   (":", Sym COLON);
   ("true", BOOL true); ("false", BOOL false);
   ("!", Key ALL); ("all", Key ALL); 
   ("forall", Key ALL);
   ("?", Key EX); ("exists", Key EX);
   ("%", Key LAM); "lambda", Key LAM]

let reserved_words = 
  [ ("not", notid, prefix, 10);
    ("and", andid, infix left_assoc, 9);
    ("or", orid, infix left_assoc, 9);
    ("=>", impliesid, infix left_assoc, 5); 
    ("iff", iffid, infix left_assoc, 4); 
    ("=", equalsid, infix left_assoc, 3)]

let type_reserved_words =  []


(*
   token_info_list/type_token_info_list:
   information about symbols which do not map to identifiers 
*)
let token_info_list = [ ]

let type_token_info_list =
  [ (Sym RIGHTARROW, Some(Basic.null_id, infix right_assoc, 6)) ]

(* Symbol tables *)

let symtable_size=51

let symbols= ref (mk_symtable symtable_size)
let symtable()= !symbols

let token_table=Grammars.token_table_new()
let type_token_table=Grammars.token_table_new()

let find_symbol sym= Lexer.find_sym (!symbols) sym

let remove_symbol sym =
  symbols:=Lexer.remove_sym (!symbols) sym

(* add_symbol sym tok:
   add sym as symbol representing token tok.
   fail silently if sym already exists
*)
let add_symbol sym tok=
  try
    symbols:=Lexer.add_sym (!symbols) sym tok
  with _ -> ()

(* add_token_info tok info:
   add parsing information for the term token tok
   fail if token information exists
*)
let add_token_info tok tok_info=
  Grammars.token_table_add token_table tok tok_info

let remove_token_info tok =
  Grammars.token_table_remove token_table tok 

(* add_type_token_info tok info:
   add parsing information for type token tok, 
   fail if token information exists
*)
let add_type_token_info tok tok_info=
  Grammars.token_table_add type_token_table tok tok_info

let remove_type_token_info tok =
  Grammars.token_table_remove type_token_table tok

(* toplevel functions to add/remove tokens *)

let add_token id sym fx pr=
  add_symbol sym (Sym (OTHER sym));
  add_token_info (Sym(OTHER sym)) (Some(id, fx, pr))

let add_type_token id sym fx pr=
  add_symbol sym (Sym (OTHER sym));
  remove_type_token_info (Sym(OTHER sym))

let remove_token sym=
  remove_symbol sym;
  remove_token_info (Sym(OTHER sym))

let remove_type_token sym=
  remove_symbol sym;
  remove_type_token_info (Sym(OTHER sym))

(* set up a symbol table with the built in tokens *)

let init_symbols()=
  List.iter (fun (s, t) ->  add_symbol s t) syms_list
     
let init_token_table()=
  List.iter (fun (sym, id, fx, pr) -> add_token id sym fx pr) reserved_words;
  List.iter (fun (tok, inf) -> add_token_info tok inf) token_info_list

let init_type_token_table()=
  List.iter 
    (fun (sym, id, fx, pr) -> add_type_token id sym fx pr) 
    type_reserved_words;
  List.iter (fun (tok, inf) -> add_type_token_info tok inf) 
    type_token_info_list
   
let init_symtab ()=
  init_symbols();
  init_type_token_table()
(*  init_token_table(); *)


let init ()= init_symtab ()

let reset_symbols() = symbols:=(mk_symtable symtable_size)
let reset_token_table() = Grammars.token_table_reset token_table
let reset_type_token_table() = Grammars.token_table_reset type_token_table

let reset()=
  reset_symbols();
  reset_token_table();
  reset_type_token_table()

(* 
   Parsers
   read a given phrase followed by an end of file/string
*)

  let mk_info ()= Grammars.mk_inf token_table type_token_table

  let parse ph inp = Pkit.parse ph EOF inp

  let identifier_parser inp =
    parse (Grammars.long_id Grammars.id (mk_info ())) inp

  let typedef_parser inp =
    parse (Grammars.typedef (mk_info ())) inp
  let type_parser inp =
    parse (Grammars.types (mk_info ())) inp

  let defn_parser inp = 
    parse (Grammars.defn (mk_info ())) inp
  let term_parser inp=
    parse (Grammars.form (mk_info ())) inp

(* User defined parsers *)

let term_parser_list ()= !(Grammars.other_parsers_list)
let add_term_parser = Grammars.add_parser
let remove_term_parser = Grammars.remove_parser

let type_parser_list ()= !(Grammars.other_type_parsers_list)
let add_type_parser = Grammars.add_type_parser
let remove_type_parser = Grammars.remove_type_parser

(* readers: read and parse a string *)

let read ph str =
  Lexer.reader (scan (symtable())) ph str

let read_term str = 
  read term_parser str

let read_type str = 
  read type_parser str

let test_lex str = scan (symtable()) (Stream.of_string str);;

let test str =  
  reader (scan (symtable())) term_parser str

