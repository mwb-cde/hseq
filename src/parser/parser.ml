
module Pkit=Parserkit.Grammars
    (struct 
      type tokens=Lexer.tok 
      let matches = Lexer.match_tokens
    end)

module Grammars  =
struct

  open Lexer
  open Pkit
  open Term

  type input = Pkit.input
  type 'a phrase = 'a Pkit.phrase

  type infotyp = 
      { 
	scope: Gtypes.scope;
	(* term information *)
	bound_names: (string* Term.term) list ref;
	token_info: token -> Pkit.token_info;
        (* type information *)
      	typ_indx : int ref;
	typ_names: (string, Gtypes.gtype)Lib.substype;
        type_token_info: token -> Pkit.token_info
      }

(* utility functions *)

  let string_of_tok tok = Lexer.string_of_token tok

  let string_tokens toks =
    Lib.list_string string_of_tok " " toks

  let get_type_indx inf = 
    inf.typ_indx:= (!(inf.typ_indx))+1; !(inf.typ_indx)

  let mk_vartyp inf = 
    Gtypes.mk_var
      ("_typ"^(string_of_int (get_type_indx inf)))

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

(*   let lookup n inf = Lib.find n inf.typ_names *)
  let lookup_type_name n inf = Lib.find n inf.typ_names 
  let add_type_name n ty inf = Lib.add n ty inf.typ_names
  let get_type n inf = 
    (try lookup_type_name n inf 
    with Not_found -> 
      add_type_name n (Gtypes.mk_var n) inf)
  let clear_type_names inf = Hashtbl.clear inf.typ_names

  let mk_token_info t=
    let f, p= Lexer.token_info t
    in 
    {fixity=f; prec=p}

  let mk_type_token_info t=
    let f, p= Lexer.type_token_info t
    in 
    {fixity=f; prec=p}

  let mk_empty_inf scp = 
    { 
      scope=scp;

      bound_names = ref [];
      token_info = mk_token_info;

      typ_indx = ref 0;
      typ_names = Lib.empty_env ();
      type_token_info = mk_type_token_info
    }

  let mk_inf scp = 
    { 
      scope=scp;
      bound_names = ref [];
      token_info = mk_token_info;
      typ_indx = ref 0;
      typ_names = Lib.empty_env ();
      type_token_info = mk_type_token_info
    }


  let scope_of_inf inf = inf.scope

  let rec alternates phl inf toks= 
    match phl with
      [] -> raise (ParsingError "No alternative parsers")
    | (ph::phs) -> 
	(try (ph inf) toks 
	with 
	  ParsingError _ -> (alternates phs inf toks))

  let error toks =  raise (ParsingError "Error")

  let other_type_parsers_list  =  ref []

  let add_type_parser ph = 
    other_type_parsers_list:=ph::!other_type_parsers_list

  let other_type_parsers inf toks = 
    alternates (!other_type_parsers_list) inf toks

  let other_parsers_list  = ref [(fun x-> error)]

  let add_parser ph = 
    other_parsers_list:=(ph::!other_parsers_list)
  let other_parsers inf toks = 
    alternates (!other_parsers_list) inf toks


  let mk_type_binary_constr t=
    match t with
      Sym RIGHTARROW -> Gtypes.mk_fun
    | ID(s, _) -> (fun x y -> Gtypes.mk_def s [x;y])
    | _ -> raise (ParsingError 
		    ((string_of_tok t)^" is not a type constructor"))

  let mk_type_unary_constr t=
    match t with
      ID(s, _) -> (fun x -> Gtypes.mk_def s [x])
    | _ -> raise (ParsingError 
		    ((string_of_tok t)^" is not a unary type constructor"))

  let mk_conn idsel inf t= 
    match t with 
     ID(i, _) -> 
       (if (Lexer.is_infix t)
       then (fun x y -> Term.mkfun i [x; y])
       else raise (ParsingError ((string_of_tok t)^" is not a connective")))
    | _ -> raise (ParsingError ((string_of_tok t)^" is not a connective"))

  let mk_prefix idsel inf t= 
    match t with 
     ID(i, _) -> 
       (if (Lexer.is_prefix t)
       then (fun x -> Term.mkfun i [x])
       else raise (ParsingError ((string_of_tok t)^" is not a prefix")))
    | _ -> raise (ParsingError ((string_of_tok t)^" is not a prefix"))

  let id inp = 
    let comp x = match x with ID _ -> true | _ -> false 
    and mk x = match x with ID (s, _) -> s | _ -> failwith "parser: id"
    in 
    try get comp mk inp
    with No_match -> raise (ParsingError "Not an identifier")

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

  let named_id fnid inp =
    let comp x = 
      match x with 
	ID(id, _) ->  id=fnid
      | _ -> false 
    and mk x =  x
    in 
    get comp mk inp

  let bool_type toks =
    try 
      ((named_id (Basic.mkname "bool"))
	 >> (fun _ -> Gtypes.mk_bool)) toks
    with No_match -> raise (ParsingError "Not a boolean type")

  let num_type toks =
    try 
      ((named_id (Basic.mkname "num"))
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

  let short_id inf toks =
    ((id >> 
      (fun x -> 
	match (Basic.dest_fnid x) with 
	  ("", s) -> s | _ -> raise (ParsingError "Not a short identifier"))) 
       toks)

  let long_id inf toks = 
    (id >> 
     (fun x -> 
       match (Basic.dest_fnid x) with 
	 (_, "") -> raise (ParsingError "Badly formed identifier")
       | _ -> x))
      toks
      
  let mk_short_id inf toks =
    (long_id inf >> (fun x -> Basic.name x)) toks

  let rec types inf toks = 
    (clear_type_names inf; inner_types inf toks)
  and inner_types inf toks =
    (operators (atomic inf, inf.type_token_info , 
	      mk_type_binary_constr, mk_type_unary_constr) toks) 
  and atomic inf toks =
    ((
      ((!$(Sym ORB) -- ((inner_types inf) -- !$(Sym CRB)))
	>> (fun x -> fst (snd x)))
    || ((num_type) >> (fun x -> x))
    || ((bool_type >> (fun x -> x)))
    || ((!$(Sym PRIME) -- short_id inf )
	      >> (fun (_, x) -> get_type x inf))
    || (((long_id inf) -- 
	(optional 
	   ((!$(Sym ORB) -- ((comma_list (inner_types inf))
			       -- (!$(Sym CRB))))
	      >> (fun (_, (args, _)) -> args))))
	>> (fun (i, a) -> 
	     match a with
	       None -> Gtypes.mk_def i []
	     | Some(ts) -> Gtypes.mk_def i ts))
(*    || (other_type_parsers inf) *)
    || error)
      toks)

  let typedef inf toks = 
    let type_id inf toks =
      ((!$(Sym PRIME) -- (short_id inf))>> (fun (_, x) -> x)) toks
    in 
    (((short_id inf) 
       -- 
       ((optional 
	  ((!$(Sym ORB)-- ((comma_list (type_id inf)) -- (!$(Sym CRB))))
	     >> (fun (_, (x, _)) -> x)))
       -- 
       (optional 
	  (((!$(mk_ident Logicterm.equalsid))
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
(* term parser, using substitution to deal with bound variables *)
  let rec form inf toks =
    (
     ((formula inf)-- (listof (formula inf)))
       >> (fun (x, y) -> mkcomb x y)
    ) toks
  and formula inf toks= 
    (
     operators(typed_primary inf, inf.token_info, 
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
     ( (( !$ (Sym ORB) -- ((form inf) -- !$(Sym CRB))))
	 >> (fun (_, (x, _)) ->  x))
   || (( !$ (Key ALL) 
           -- ((id_type_op short_id inf)
		 -- ((listof (id_type_op short_id inf))
		       -- (!$(Sym COLON) -- (form inf)))))
	 >> (fun (_, (v, (vs, (_, b)))) -> 
	   (Logicterm.mkall_ty (scope_of_inf inf) (fst v) (snd v) 
	      (List.fold_right (fun (x, y) -> 
		Logicterm.mkall_ty (scope_of_inf inf) x y) vs b))))
   || (( !$ (Key EX) 
          -- ((id_type_op short_id inf)
		-- ((listof (id_type_op short_id inf))
		      -- (!$(Sym COLON) -- (form inf)))))
	>> (fun (_, ((v, vt), (vs, (_, b)))) -> 
	  (Logicterm.mkex_ty (scope_of_inf inf) v vt 
	     (List.fold_right 
		(fun (x, y) -> Logicterm.mkex_ty (scope_of_inf inf) x y) 
		vs b))))
   || (( !$ (Key LAM) 
           -- ((id_type_op short_id inf)
		 -- ((listof (id_type_op short_id inf))
		       -- (!$(Sym COLON) -- (form inf)))))
	 >> (fun (_, ((v, vt), (vs, (_, b)))) -> 
	   (Logicterm.mklam_ty (scope_of_inf inf) v  vt 
	      (List.fold_right 
		 (fun (x, y) -> Logicterm.mklam_ty (scope_of_inf inf)x y) 
		 vs b))))
   || ((id_type_op long_id inf)  >> 
       (fun ((n, i), t) -> mk_typed_var (Basic.mklong n i) t))
   || (number >> (fun x -> mknum x))
   || (boolean >> (fun x -> mkbool x))
   || (other_parsers inf)
   || (error)
    ) toks
*)

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
      (qnt: Basic.quant_ty) (xs : (string* Gtypes.gtype) list) =
    List.map 
      (fun (n, ty) -> 
	let b_id=Term.mkbound(Term.mk_binding qnt n ty)
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

  let qnt_term_remove_names inf (xs : (string* Term.term) list) body=
    List.fold_right
      (fun (x, y) b ->
	let nt=Term.Qnt(dest_bound y, b)
	in 
	drop_name x inf; nt) xs body

  let rec form inf toks =
    (
     ((formula inf)-- (listof (formula inf)))
       >> (fun (x, y) -> mkcomb x y)
    ) toks
  and formula inf toks= 
    (
     operators(typed_primary inf, inf.token_info, 
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
           -- ((id_type_op short_id inf)
		 -- ((listof (id_type_op short_id inf))
		       -- (!$(Sym COLON)))))
	 >> 
	(fun (_, (v, (vs, _)))
	   ->
	     qnt_setup_bound_names inf Basic.All (v::vs)))
	 -- (form inf))
	 >> 
       (fun ((xs:(string*Term.term)list), body)
	 ->
	   qnt_term_remove_names inf xs body))

(* "EX" { id_type_op }+ ":" form *)
   || (((( !$ (Key EX) 
           -- ((id_type_op short_id inf)
		 -- ((listof (id_type_op short_id inf))
		       -- (!$(Sym COLON)))))
	 >> 
	(fun (_, (v, (vs, _)))
	   ->
	     qnt_setup_bound_names inf Basic.Ex (v::vs)))
	 -- (form inf))
	 >> 
       (fun ((xs:(string*Term.term)list), body)
	 ->
	   qnt_term_remove_names inf xs body))
(* "LAM" { id_type_op }+ ":" form *)
   || (((( !$ (Key LAM) 
           -- ((id_type_op short_id inf)
		 -- ((listof (id_type_op short_id inf))
		       -- (!$(Sym COLON)))))
	 >> 
	(fun (_, (v, (vs, _)))
	   ->
	     qnt_setup_bound_names inf Basic.Lambda (v::vs)))
	 -- (form inf))
	 >> 
       (fun ((xs:(string*Term.term)list), body)
	 ->
	   qnt_term_remove_names inf xs body))
(* id | "(" id ":" type ")" *)
   || ((id_type_op long_id inf)  >> 
       (fun ((n, i), t) -> 
	 let nid=Basic.mklong n i
	 in 
	 if(Basic.is_short_id nid)
	 then 
	   try lookup_name i inf
	   with Not_found -> mk_typed_var nid t
	 else 
	   mk_typed_var nid t))
(*
     number
   | boolean
   | alternative_parsers
   | error 
*)
   || (number >> (fun x -> mknum x))
   || (boolean >> (fun x -> mkbool x))
   || (other_parsers inf)
   || (error)
    ) toks

      
  let rec lhs inf toks=
    ((( (id_type_op mk_short_id inf) -- (args_opt inf))
	>> (fun ((n, t) , args) -> (n, args))) toks)
  and args_opt inf toks= 
    (tlistof (id_type_op short_id inf) (!$(mk_ident Logicterm.equalsid))) toks
  and defn inf toks =
    ( (( ((lhs inf) -- (form inf))
	   >> (fun (l,  r) -> (l, r)))
     || error) toks)


end

(*
   Tpparser: toplevel for parsing functions
*)

open Lexer
open Logicterm

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

  let all_qnts = [("!", Key ALL); 
		  ("all", Key ALL); 
		  ("forall", Key ALL)]
  let ex_qnts = [("?", Key EX); 
		 ("exists", Key EX)]
  let lam_qnts = [("%", Key LAM); 
		  "lambda", Key LAM]

(*
  let not_ops = [("~", mk_full_ident notid prefix non_assoc 10); 
		 ("not",mk_full_ident notid prefix non_assoc 10)]
  let and_ops = [("&", mk_full_ident andid infix left_assoc 9); 
		 ("and", mk_full_ident andid infix left_assoc 9)]

  let or_ops =  [("|", mk_full_ident orid infix left_assoc 9); 
		 ("or", mk_full_ident orid infix left_assoc 9)]

  let implies_ops = [("=>", 
		      mk_full_ident impliesid infix left_assoc 5); 
		     ("implies", 
		      mk_full_ident impliesid infix right_assoc 5)]
  let iff_ops = [("<=>", mk_full_ident iffid infix left_assoc 4); 
		 ("iff", mk_full_ident iffid infix left_assoc 4)]
  let equals_ops = [("=", 
		     mk_full_ident equalsid infix left_assoc 3)]
*)

(* reserved words *)

let reserved_words = 
  [ ("not", notid, prefix, 10);
    ("and", andid, infix left_assoc, 9);
    ("or", orid, infix left_assoc, 9);
    ("=>", impliesid, infix left_assoc, 5); 
    ("iff", iffid, infix left_assoc, 4); 
    ("=", equalsid, infix left_assoc, 3)]

  let syms_list = 
    [(".", Sym DOT); ("(", Sym ORB);
     (")", Sym CRB); (",", Sym COMMA);
     ("->", Sym RIGHTARROW); ("'", Sym PRIME);
     (":", Sym COLON)]

  let keywords_list = 
    let rwords=
       List.map
	(fun (sym, id, fx, pr) ->
	  (sym, mk_full_ident id fx pr))
	reserved_words
    in 
    List.concat 
      [[("true", BOOL true)]; [("false", BOOL false)];
       all_qnts; ex_qnts; lam_qnts; 
       rwords
     ]

(* Symbol table *)

let symtable_size=51
  let symbols= ref (mk_symtable symtable_size)
  let symtable()= !symbols

let find_symbol sym=
  Lexer.find_sym (!symbols) sym

let remove_symbol sym =
  symbols:=Lexer.remove_sym (!symbols) sym

let add_symbol id sym fx pr=
  List.iter
    (fun (s, _) -> 
      if s=sym 
      then failwith ("Symbol "^sym^" is a reserved keyword")
      else ())
    syms_list;
  symbols:=Lexer.add_sym (!symbols) 
      sym
      (Lexer.mk_full_ident id fx pr)

let add_token sym tok=
  symbols:=Lexer.add_sym (!symbols) sym tok

(* set up a symbol table with the built in tokens *)

let init_symtab () = 
  List.iter 
    (fun (s, tk) -> add_token s tk) keywords_list;
  List.iter 
    (fun (s, tk) -> add_token s tk) syms_list
    
let init ()=
  init_symtab ()

(* 
   Parsers
   read a given phrase followed by an end of file/string
*)

let mk_info = Grammars.mk_inf

  let parse ph inp = Pkit.parse ph EOF inp

  let identifier_parser scp inp =
    parse (Grammars.long_id (mk_info scp)) inp

  let typedef_parser scp inp =
    parse (Grammars.typedef (mk_info scp)) inp
  let type_parser scp inp =
    parse (Grammars.types (mk_info scp)) inp

  let defn_parser scp inp = 
    parse (Grammars.defn (mk_info scp)) inp
  let term_parser scp inp=
    parse (Grammars.form (mk_info scp)) inp


(* readers: read and parse a string *)

let read ph scp str =
  Lexer.reader (scan (symtable())) (ph scp) str

let read_term scp str = 
  read term_parser scp str

let read_type scp str = 
  read type_parser scp str

let test_lex str = scan (symtable()) (Stream.of_string str);;
let test str =  
  reader (scan (symtable()))
    (term_parser (Gtypes.empty_scope()))
    str




