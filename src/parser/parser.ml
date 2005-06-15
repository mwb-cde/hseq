(*-----
   Name: parser.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

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


module Utility=
  struct 
    open Lexer
    open Pkit

    let (?$) tok =
      ((!$ tok) >> (fun _ -> Term.mk_short_var (Lexer.string_of_token tok)))
    let (?%) tok =
      ((!$ tok) >> (fun _ -> Gtypes.mk_var (Lexer.string_of_token tok)))
  end

(**
   [typdef_data]:
   Information returned by the typedef parsers 
 *)
type typedef_data =
    NewType of (string * (string list))
  | TypeAlias of (string * (string list) * Basic.gtype)
  | Subtype of (string * (string list) 
		  * Basic.gtype * Basic.term)


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

(* 
   [infotyp]
   Information used when parsing terms and gtypes.

   For terms:
   [bound_names]:
   string representation of bound variables and their associated terms.
   [token_info]:
   precedence and fixity of tokens.

   For gtypes:
   [typ_index]:
   Counter used to generate unique names for type variables.
   [type_token_info]:
   precedence and fixity of gtype tokens.
 *)   
    type infotyp = 
	{ 
	  (* term information *)
	  bound_names: (string* Basic.term) list ref;
	  token_info: (token -> token_info);

          (* type information *)
          typ_indx : int ref;
          typ_names: (string* Basic.gtype) list ref;
          type_token_info: (token ->token_info);
	}

(* utility functions *)

(**
   [string_of_tok], [string_tokens]
   get string representation of tokens.
 *)
    let string_of_tok tok = Lexer.message_of_token tok
    let string_tokens toks =
      Lib.list_string string_of_tok " " toks

(**
   [mk_vartyp inf]
   Make a new, uniquely named, type variable.
   Increments [inf.typ_indx].
 *)

    let get_type_indx inf = 
      inf.typ_indx:= (!(inf.typ_indx))+1; !(inf.typ_indx)

    let mk_vartyp inf = 
      Gtypes.mk_var
	("typ"^(string_of_int (get_type_indx inf)))

(**
   [lookup_name n inf]
   Look up [n] in [inf.bound_names].
   raise Not_found if not found.
 *)
    let lookup_name n inf = 
      List.assoc n !(inf.bound_names)

(**
   [add_name n trm inf]
   Associate [trm] with name [n] in [inf.bound_names].
   Used to associate variable name with a bound variable,

   E.g. at the top of a binding term [!x. t],
   [add_name x (Bound b) inf]
   is called to associate [x] with [Bound b] when parsing [t].
 *)
    let add_name n trm inf = 
      inf.bound_names:=(n, trm)::!(inf.bound_names)
(**
   [drop_name n inf]
   Remove [n] from the list of bound names
   e.g. after parsing the body [t] of a binding term [! x. t]
 *)
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

(**
   [get_term n inf]
   Get the term associated with bound name [n].
   if there is no term associated with [n] 
   then return an unqualified identifier.
 *)
    let get_term n inf = 
      (try lookup_name n inf 
      with Not_found -> Term.mk_free n (Gtypes.mk_null()))

(**
   [clear_names inf]
   Clear the bound names of [inf]
 *)
    let clear_names inf = inf.bound_names:=[]

(** 
   [lookup_type_name n inf]
   Lookup type variable name [n].
   if not found, raise [Not_found].
 *)
    let lookup_type_name n inf = 
      List.assoc n !(inf.typ_names)

(**
   [add_type_name n ty inf]
   Add [n] as the string representation of gtype [ty].
 *)
    let add_type_name n ty inf = 
      inf.typ_names := (n, ty)::!(inf.typ_names); ty

(**
   [get_type n inf]
   Get the type variable represented by name [n].
   If not found, create a type variable, with a unique name,
   add associate [n] with this type variable in [inf]
   and return this type variable.
 *)
    let get_type n inf = 
      (try lookup_type_name n inf 
      with Not_found -> 
	add_type_name n (Gtypes.mk_var n) inf)

(** 
   [clear_type_names inf]
   Clear the record of type variable names.
 *)
    let clear_type_names inf = 
      inf.typ_names:=[]

(**
   [mk_token_info x]
   Extract the precedence and fixity information from [x], if any.
   If not, return the default fixity and precedence.
   Used with the Parserkit.operator parser.
 *)
    let mk_token_info x =
      match x with
	Some(_, f, p) ->  {fixity=f; prec=p}
      | _ -> {fixity = default_term_fixity; 
	      prec=default_term_prec}

(**
   [mk_type_token_info x]
   Extract the precedence and fixity information from [x], if any.
   If not, return the default fixity and precedence.
   Used with the Parserkit.operator parser.
 *)
    let mk_type_token_info tbl t=
      match (token_info tbl t) with
	Some(_, f, p)-> 
	  {fixity=f; prec=p}
      | _ -> {fixity=default_type_fixity; 
	      prec=default_type_prec}

(**
   [mk_empty_inf tbl type_tbl]
   Make parsing information from 
   table [tbl] and [type_tbl] of term and type token information.
 *)
    let mk_empty_inf tbl type_tbl= 
      { 
	bound_names = ref [];
	token_info = token_info tbl;

	typ_indx = ref 0;
	typ_names = ref [];
	type_token_info = token_info type_tbl
      }

(**
   [mk_inf tbl type_tbl]
   Make parsing information from 
   table [tbl] and [type_tbl] of term and type token information.
 *)
    let mk_inf tbl type_tbl = 
      { 
	bound_names = ref [];
	token_info = token_info tbl;

	typ_indx = ref 0;
	typ_names = ref [];
	type_token_info = token_info type_tbl
      }

(** Specialised parsers *)

(** [message m _]
   Fail, raising [ParsingError m]
 *)
    let message m _ =  raise (ParsingError m)

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

    let comma_list ph toks=
      list0 ph (!$(Sym comma_sym)) toks

    let listof ph toks=  repeat ph toks
	
    let rec repeat_term ph term toks =
      (((ph -- (repeat_term ph term )) >> (fun (x, y) -> x ::y))
     || (term >> (fun _ -> []))) toks

(** 
   Identifier parsers.
   Compilicated by the need to look up symbols
   to test if the symbol maps to an identifier
   [id]: read an identifier

   [named_id]: read a specific (given) identifier
 *)

(**
   [id_parser info inp]

   General identifier parser.
   matches identifiers and symbols which translate to identifiers.
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
	    | _ ->  raise (ParsingError "(id) Not an identifier"))
      in 
      try Pkit.get comp mk inp
      with No_match -> raise (ParsingError "(id) Not an identifier")

(**
   [id_strict info inp]

   String identifier parser.
   matches possibly qualified identifiers only
 *)   
    let id_strict info inp =
      let comp x= 
	match x with 
	  ID _ -> true 
	| _ -> false
      and mk x = 
	match x with 
	  ID(s) -> s
	| _ -> raise (ParsingError "Not an identifier")
      in 
      try Pkit.get comp mk inp
      with No_match -> raise (ParsingError "Not an identifier")


(** [named_id info idparser name inp]
   Parse an identifier [name], using parser [idparser info].
   fail if token doesn't match.
 *)   
    let named_id info idparser name inp =
      ((idparser info) >> 
       (fun x -> 
	 if(x=name) then x 
	 else 
	   raise (ParsingError ("Expected identifier "
				^(Basic.string_fnid name)^" but got "
				^(Basic.string_fnid x)))))
	inp

(**
   [short_id idparser inf toks]
   Parse a short (unqualified) identifier, using parser [idparser inf].
 *)
    let short_id idparser inf toks =
      ((idparser inf >> 
	(fun x -> 
	  match (Basic.dest_fnid x) with 
	    ("", s) -> s 
	  | _ -> raise (ParsingError "Not a short identifier"))) 
	 toks)

(**
   [long_id idparser inf toks]
   Parse a possibly qualified identifier, using parser [idparser inf].
 *)
    let long_id idparser inf toks = 
      (idparser inf >> 
       (fun x -> 
	 match (Basic.dest_fnid x) with 
	   (_, "") -> raise (ParsingError "Badly formed identifier")
	 | _ -> x))
	toks


(**********
 *
 * Gtype parsers
 *
 **********)

(** 
   [mk_type_binary_constr inf t]
   Construct a gtype from binary operators.
 *)
    let mk_type_binary_constr inf t=
      let lookup x =
	try inf.type_token_info x
	with Not_found -> None
      in 
      match t with
      | ID(s) -> (fun x y -> Gtypes.mk_def s [x;y])
      | _ ->
	  match (lookup t) with
	    Some(name, _, _) -> 
	      (fun x y-> Gtypes.mk_def name [x; y])
	  | _ -> 
	      raise (ParsingError 
		       ((string_of_tok t)^" is not a type constructor"))

(** 
   [mk_type_unary_constr inf t]
   Construct a gtype from unary operators.
 *)
    let mk_type_unary_constr inf t=
      let lookup x =
	try inf.type_token_info x
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


(** 
   [type_id info inp] 
   Read an identifier for the type parser.
   Take into account that symbol -> should be 
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

(** 
   [primed_id inf]
   Read a type variable name.
 *)
    let primed_id inf toks =
      let comp x =
	match x with 
	  PrimedID _ -> true
	| _ -> false
      and mk x =
	match x with 
	  PrimedID s -> get_type s inf
	| _ -> failwith "parser: expected type variable"
      in 
      try Pkit.get comp mk toks
      with No_match -> raise (ParsingError "expected type variable")

(**
   [bool_type info]
   Parse type "bool"
 *)
    let bool_type info toks =
      try 
	((named_id info type_id (Basic.mk_name "bool"))
	   >> (fun _ -> Logicterm.mk_bool_ty)) toks
      with No_match -> raise (ParsingError "Not a boolean type")

(**
   [num_type info]
   Parse type "num"
 *)
    let num_type info toks =
      try 
	((named_id info type_id (Basic.mk_name "num"))
	   >> (fun _ -> Gtypes.mk_num)) toks
      with No_match -> raise (ParsingError "Not a number type")


(** [mk_short_id id inf]
   Parse a possibly qualified identifer with [id inf], 
   make it a short identifier.
 *)	  
    let mk_short_id id inf toks =
      (long_id id inf >> (fun x -> Basic.name x)) toks


(** 
   The builtin type parser.

   [inner_types]: parse types built with infix/prefix/suffix operators.
   [atomic_types]: 
   Parse types satisfying the grammar
   primed_id
   | num_type
   | bool_type
   | '(' list0_sep inner_type ',' ')' long_id
   | '(' inner_type ')'
   | type_parsers
   | error
 *)
    let rec inner_types inf toks =
      (operators (atomic_types inf, 
		  (fun x -> mk_token_info (inf.type_token_info x)), 
		  mk_type_binary_constr inf, mk_type_unary_constr inf) toks) 
    and atomic_types inf toks =
      (((type_parsers inf) 
      || error ~msg:"unknown construct in type")
	 toks)
(*
   Core Type Parsers:
 *)
    and core_type_parsers = 
      [
       "primed_id", primed_id;
       "num_type", num_type;
       "bool_type", bool_type;
       "type_constructor", 
       (fun inf ->
	 (((optional 
	      (((!$(Sym ORB) 
		   -- ((comma_list (inner_types inf))
			 -- (!$(Sym CRB)))))
		 >> (fun (_, (args, _)) -> args)))
	     -- (long_id id_strict inf))
	    >> (fun (a, i) -> 
	      match a with
		None -> Gtypes.mk_def i []
	      | Some(ts) -> Gtypes.mk_def i ts)));
       "bracketed_type", 
       (fun inf -> 
	 ((!$(Sym ORB) -- ((inner_types inf) -- !$(Sym CRB)))
	    >> (fun x -> fst (snd x))))
     ]
(**
   Support for adding type parsers.
 *)
    and 
	type_parsers_list  =  ref core_type_parsers
(**
   [type_parsers inf]
   Try each of the parsers in the list 
   [type_parsers_list].
 *)
    and 
	type_parsers inf toks = 
      named_alt (!type_parsers_list) inf toks

(**
   [types inf]
   Toplevel for the type parser.
 *)
    let rec types inf toks = 
      (clear_type_names inf; inner_types inf toks)
	
(**
   Support for adding type parsers.
 *)

(** 
   [add_type_parser pos n ph]
   Add type parser [ph] at position [pos] with name [n].
 *)
    let add_type_parser pos n ph = 
      type_parsers_list:=
	Lib.named_add (!type_parsers_list) pos n ph

(** 
   [remove_type_parser n]
   Remove the type parser named [n].
 *)
    let remove_type_parser n =
      type_parsers_list:=List.remove_assoc n (!type_parsers_list)


(**********
 *
 *  Term parsers 
 *
 **********)


(* Utility functions for use with the term parser. *)

(** 
   [mk_conn idsel inf t]
   Construct a function application term from a binary operator.
 *)
	  
    let mk_conn idsel inf t= 
      let lookup x =
	try inf.token_info x
	with Not_found -> None
      in 
      match t with 
	ID(i) -> 
	  (fun x y -> Term.mk_fun i [x; y])       
      | _ -> 
	  match (lookup t) with
	    Some (name, _, _) ->
	      (fun x y -> Term.mk_fun name [x; y])
	  | _ ->
	      raise (ParsingError ((string_of_tok t)^" is not a connective"))

(**
   [mk_prefix idsel inf t]
   Construct a function application term from a unary operator.
   
 *)
    let mk_prefix idsel inf t= 
      let lookup x =
	try inf.token_info x
	with Not_found -> None
      in 
      match t with 
	ID(i) -> 
	  (fun x -> Term.mk_fun i [x])
      | _ -> 
	  match (lookup t) with
	    Some(name, _, _) -> 
	      (fun x -> Term.mk_fun name [x])
	  | _ -> 
	      raise (ParsingError ((string_of_tok t)^" is not a prefix"))

(**
   [qnt_setup_bound_names inf qnt xs]
   Make bound variables from the name-type pairs in [xs],
   add them to [inf.bound_names]
   [qnt] is the quantifier type (All, Ex or Lambda)
 *)
    let qnt_setup_bound_names inf 
	(qnt: Basic.quant_ty) (xs : (string* Basic.gtype) list) =
      List.map 
	(fun (n, ty) -> 
	  let b_id=Term.mk_bound(Basic.mk_binding qnt n ty)
	  in 
	  add_name n b_id inf;
	  (n, b_id)) xs

(**
   [qnt_term_remove inf xs body]
   use bound names in [xs] to form a quantified term, with body
   as the initial term.
   simplified example:  [!x, ?y, !z] t -> (!x: (?y: (!z: t)))

   remove each name in [xs] from [inf.bound_names] as it is used.
 *)
    let qnt_term_remove_names inf (xs : (string* Basic.term) list) body=
      List.fold_right
	(fun (x, y) b ->
	  let binder=dest_bound y
	  in 
	  let nt=Basic.Qnt(binder, b)
	  in 
	  drop_name x inf; nt) xs body

(**
   [make_term_remove_names info wrapper vs body]:

   Remove the variables in [vs] from [info]. 
   Return the term constructed by quantifying 
   [body] with the variables [vs], applying [wrapper] to 
   each constructed term.
*)
    let make_term_remove_names inf wrapper xs body=
      List.fold_right
	(fun (x, y) b ->
	  let binder=Term.dest_bound y
	  in 
	  let nt=wrapper (Basic.Qnt(binder, b))
	  in 
	  drop_name x inf; nt) xs body


(**
   [mkcomb f args]
   Make the term ((((f a1) a2) .. ) an)
   (where args = [a1; a2; ..; an])
 *)
    let rec mk_comb x y = 
      match y with 
	[] -> x
      | t::ts -> mk_comb (mk_app x t) ts

(** [number]
   Read a number.
 *)
    let number inp = 
      let comp x = match x with NUM _ -> true | _ -> false 
      and mk x = 
	match x with 
	  NUM s -> Num.num_of_string s 
	| _ -> failwith "parser: number"
      in 
      try get comp mk inp
      with No_match -> raise (ParsingError "Not a number")

(** [boolean]
   Read a boolean.
 *)
    let boolean inp = 
      let comp x = match x with BOOL _ -> true | _ -> false 
      and mk x = 
	match x with 
	  BOOL b -> b
	| _ -> failwith "parser: boolean"
      in 
      try get comp mk inp
      with No_match -> raise (ParsingError "Not a boolean")

(** 
   [optional_type inf]
   parse optional type.
   { ':' types }?
 *)
    let optional_type inf =     
      ( (( !$(Sym COLON) -- (types inf)) >> (fun (_, ty) -> Some(ty)))
      || (empty >> (fun x -> None))) 


(** [id]
   parse an identifier occuring as a term
 *)
    let id info inp = 
      let lookup x = info.token_info x
      in 
      (id_parser lookup inp)

(** 
   [id_type_opt idnt inf]
   parse identifier [idnt inf] with optional type.
 *)
    let id_type_opt idnt inf toks=
      (( (((!$(Sym ORB)) -- (idnt inf) 
	     -- (!$(Sym COLON))-- (types inf) -- (!$(Sym CRB)))
	    >> (fun ((((_, i), _), t), _) -> (i, t)))
       || ( (idnt inf) >> (fun x -> (x, mk_vartyp inf))))
	 toks)


(**
   [term_identifer inf]
   Parse a possibly typed identifer satisfying [id_type_opt inf].
   Lookup identifier in [inf], to check if it is a bound variable.
   If not, it is a free variable.
 *)
    let term_identifier inf toks =
      ((id_type_opt (long_id id) inf) 
	 >>
       (fun ((n, i), t) -> 
	 let nid=Basic.mk_long n i
	 in 
	 if(Basic.is_short_id nid)
	 then 
	   (try (lookup_name i inf)
	   with Not_found -> mk_free i t)
	 else 
	   mk_typed_var nid t)) toks

(**
   [form]/[formula]/[type_primary]/[primary]
   Main term parser.

   form: formula {formula}*
   formula: prefix/infix/suffix operators built around typed_primary.
   typed_primary: primary optional_type
   primary:
   '(' form ')'
   | 'ALL' { id_type_opt }+ ':' form
   | 'EX' { id_type_opt }+ ':' form
   | 'LAM' { id_type_opt }+ ':' form
   | id_type_opt 
   | number
   | boolean
   | alternative_parsers
   | error 
 *)
    let rec form inf toks =
      (
       ((formula inf)-- (listof (formula inf)))
	 >> (fun (x, y) -> mk_comb x y)
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
	 match pty with None -> t | Some(ty) -> mk_typed t ty)
      ) toks
    and
	primary inf toks = 
      ((term_parsers inf)
     || (error ~msg:"unknown construct in term")) toks


(** [term_parsers_list] 
   list of term parsers.
 *)
    and 
	term_parsers_list  = ref core_term_parser_list
(**
   [core_term_parser_list]

   The primary term parsers are stored in a named list.
 *)
    and core_term_parser_list = 
      [ 
(* id '(' id ':' type ')' *)
	"identifier", term_identifier;
(*   | number *)
	"number", (fun _ -> (number >> (fun x -> mk_num x)));
(*   | boolean *)
	"boolean", 
	(fun _ -> 
 	  (boolean >> (fun x -> Logicterm.mk_bool x))); 
(* '(' form ')' *)
	"bracketed_term",
	(fun inf -> 
	  ( (( !$ (Sym ORB) -- ((form inf) -- !$(Sym CRB))))
	      >> (fun (_, (x, _)) ->  x)));

(* 'ALL' { id_type_opt }+ ':' form *)
	"forall",
	(fun inf -> 
	  (((( !$ (Key ALL) 
		 -- ((id_type_opt (short_id id) inf)
		       -- ((listof (id_type_opt (short_id id) inf))
			     -- (!$(Sym COLON)))))
	       >> 
	     (fun (_, (v, (vs, _))) ->
	       qnt_setup_bound_names inf Basic.All (v::vs)))
	      -- (form inf))
	     >> 
	   (fun ((xs:(string*Basic.term)list), body) -> 
	     qnt_term_remove_names inf xs body)));

(* 'EX' { id_type_opt }+ ':' form *)
	"exists",
	(fun inf -> 
	  (((( !$ (Key EX) 
		 -- ((id_type_opt (short_id id) inf)
		       -- ((listof (id_type_opt (short_id id) inf))
			     -- (!$(Sym COLON)))))
	       >> 
	     (fun (_, (v, (vs, _))) ->
	       qnt_setup_bound_names inf Basic.Ex (v::vs)))
	      -- (form inf))
	     >> 
	   (fun ((xs:(string*Basic.term)list), body) ->
	     qnt_term_remove_names inf xs body)));
(* 'LAM' { id_type_opt }+ ':' form *)
	"lambda", 
	(fun inf -> 
	  (((( !$ (Key LAM) 
		 -- ((id_type_opt (short_id id) inf)
		       -- ((listof (id_type_opt (short_id id) inf))
			     -- (!$(Sym COLON)))))
	       >> 
	     (fun (_, (v, (vs, _))) ->
	       qnt_setup_bound_names inf Basic.Lambda (v::vs)))
	      -- (form inf))
	     >> 
	   (fun ((xs:(string*Basic.term)list), body) ->
	     qnt_term_remove_names inf xs body)))
      ]

(**
   [term_parsers inf tok]
   parse using parsers in [term_parsers_list].
 *)
    and term_parsers inf toks = 
      named_alt (!term_parsers_list) inf toks

(**
   Support addition of parsers.
 *)

(**
   [add_parser pos n ph]
   Add term parser [ph] with name [n] in position [pos].
 *)
    let add_parser pos n ph = 
      term_parsers_list:=Lib.named_add (!term_parsers_list) pos n ph

(**
   [remove_parser n]
   Remove term parser named [n].
 *)
    let remove_parser n = 
      term_parsers_list:=List.remove_assoc n (!term_parsers_list)


(** [defn inf toks]
   Parse a definition.
   Grammar:
   (id_type_opt short_id) (id_type_opt short_id)* '=' form
 *)
    let rec lhs inf toks=
      ((((id_type_opt (short_id id) inf) 
	   -- (args_opt inf))
	  >> (fun ((n, t), args) -> (n, args))) 
     || error ~msg:"badly formed identifier for definition")
	toks
    and args_opt inf= 
      (((optional (repeat (id_type_opt (short_id id) inf)))
	  -- (!$(mk_symbol Logicterm.equalssym)))
	 >> (fun (x, _) -> match x with None -> [] | Some l -> l))
    || error ~msg:"badly formed argument list for definition"
    and defn inf toks =
      (
       (((lhs inf) -- (form inf))
	  >> (fun (l, r) -> (l, r)))
     || (error ~msg:"Badly formed definition"))
	toks


(** 
   [typedef inf]
   Parse a type definition.
   Grammar:
   typedef ::= simple_typedef
   | subtypedef

   simple_typedef::= ('(' {primed_id}* ')')? short_id ( '=' type )?
   subtypedef ::= type ':' term
 *)

    let simple_typedef inf toks = 
      (((optional 
	   ((!$(Sym ORB)-- ((comma_list (primed_id inf)) -- (!$(Sym CRB))))
	      >> (fun (_, (x, _)) -> (List.map Gtypes.get_var x))))
	  -- 
	  ((short_id type_id inf)
	     -- 
	     (optional 
		(((!$(mk_symbol Logicterm.equalssym))
		    -- (types inf)) >> (fun (_, x) -> x)))))
	 >> (fun (args, (name, defn)) 
	   -> (name, args, defn))) toks


    let subtypedef inf toks = 
      let lhs inp= 
	((optional 
	    ((!$(Sym ORB)-- ((comma_list (primed_id inf)) -- (!$(Sym CRB))))
	       >> (fun (_, (x, _)) -> (List.map Gtypes.get_var x))))
	   -- 
	   (short_id type_id inf)) inp
      and rhs inp = 
	(((types inf)
	    -- (!$(Sym COLON))
	    -- (form inf))
	   >> (fun ((ty, _), f) -> (ty, f))) inp
      in 
      ((lhs -- (!$(mk_symbol Logicterm.equalssym)) -- rhs) 
	 >> (fun (((args, name), _), (ty, trm)) 
	   -> (name, args, ty, trm))) toks


    let typedef inf toks = 
      (((subtypedef inf) >>
	(fun (n, args, dtyp, set) -> 
	  Subtype(n, Lib.get_option args [], dtyp, set)))
     ||
       ((simple_typedef inf) >> 
	(fun (n, args, dtyp) -> 
	  match dtyp with
	    None -> NewType (n, Lib.get_option args [])
	  | (Some dt) -> TypeAlias(n, Lib.get_option args [], dt)))) toks



(** 
   [parse_as_binder f sym]:
   Construct a grammar to parse function applications
   of the form [f (%x: P)] as [sym x: P].

   Symbol [sym] should be added to the lexer seperately.
   (e.g. using [Parser.add_symbol sym (Lexer.Sym(Lexer.OTHER sym))]).
 *)   
    let parse_as_binder ident sym= 
      let sym_tok = Sym(OTHER sym)
      and colon = Sym(COLON)
      in 
      let id_term = Term.mk_var ident
      in 
      let wrapper b = Term.mk_app id_term b
      in 
      let grammar inf inp = 
	(((((!$ sym_tok)
	      -- (id_type_opt (short_id id) inf)
	      -- (listof (id_type_opt (short_id id) inf))
	      -- (!$ colon))
	     >> 
	   (fun (((_, v), vs), _) -> 
	     qnt_setup_bound_names inf Basic.Lambda (v::vs)))
	    --
	    (form inf))
	   >>
	 (fun ((xs:(string*Basic.term)list), body) ->
	   make_term_remove_names inf wrapper xs body)) inp
      in 
      grammar 


  end

(**
   Toplevel for parsing functions
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

let syms_list = 
  [(".", Sym DOT); 
   ("(", Sym ORB);
   (")", Sym CRB); 
   (",", Sym comma_sym); 
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
let token_info_list = []

let type_token_info_list = []

(* Symbol and token tables *)

let symtable_size=51

let symbols= ref (mk_symtable symtable_size)
let symtable()= !symbols

let token_table=Grammars.token_table_new()
let type_token_table=Grammars.token_table_new()

(*
   add_symbol sym tok:
   add sym as symbol representing token tok.
   fail silently if sym already exists
 *)
let add_symbol sym tok=
  try
    symbols:=Lexer.add_sym (!symbols) sym tok
  with _ -> ()

let find_symbol sym= Lexer.find_sym (!symbols) sym

let remove_symbol sym =
  symbols:=Lexer.remove_sym (!symbols) sym

(*
   Overloading 

   Overload table 
 *)
let overload_table = Hashtbl.create 127

let init_overload () = Hashtbl.clear overload_table

let get_overload_list sym =
  Hashtbl.find overload_table sym

let add_overload sym (id, ty) =
  let list0 = 
    try 
      get_overload_list sym
    with Not_found -> []
  in 
  let list1=(id, ty)::list0
  in 
  Hashtbl.replace overload_table sym list1
    
let remove_overload sym id =
  let list0 = get_overload_list sym
  in 
  let list1 = 
    List.remove_assoc id list0
  in 
  match list1 with
    [] -> Hashtbl.remove overload_table sym
  | _ -> Hashtbl.replace overload_table sym list1

let print_overloads info = 
  let print_fn sym list= 
    Format.printf "@[<2>%s@ " sym;
    List.iter
      (fun (id, ty) -> 
	Printer.print_ident id;
	Format.printf ":@ ";
	Gtypes.print info ty;
	Format.printf ";@ ")
      list;
    Format.printf "@]@,"
  in 
  Format.printf "@[<v>";
  Hashtbl.iter print_fn overload_table;
  Format.printf "@]"

(*
   [add_token_info tok info]:
   add parsing information for the term token [tok]
   fail if token information exists
 *)
let add_token_info tok tok_info=
  Grammars.token_table_add token_table tok tok_info

let remove_token_info tok =
  Grammars.token_table_remove token_table tok 

let get_token_info tok=
  Grammars.token_table_find token_table tok

(**
   [add_type_token_info tok info]:
   add parsing information for type token [tok], 
   fail if token information exists
 *)
let add_type_token_info tok tok_info=
  Grammars.token_table_add type_token_table tok tok_info

let remove_type_token_info tok =
  Grammars.token_table_remove type_token_table tok

let get_type_token_info tok =
  Grammars.token_table_find type_token_table tok


(* toplevel functions to add/remove tokens *)

let add_token id sym fx pr=
(* lexer information *)
  add_symbol sym (Sym (OTHER sym)); 
(* parser information *)
  add_token_info (Sym(OTHER sym)) (Some(id, fx, pr))

let add_type_token id sym fx pr=
(* lexer information *)
  add_symbol sym (Sym (OTHER sym));
(* parser information *)
  add_type_token_info (Sym(OTHER sym)) (Some(id, fx, pr))

let remove_token sym=
  remove_symbol sym;
  remove_token_info (Sym(OTHER sym))

let remove_type_token sym=
  remove_symbol sym;
  remove_type_token_info (Sym(OTHER sym))

(* set up a symbol table with the built in tokens *)

let init_symbols()=
  symbols:=(mk_symtable symtable_size);
  List.iter (fun (s, t) ->  add_symbol s t) syms_list
    
let init_token_table()=
  Grammars.token_table_reset token_table

let init_type_token_table()=
  Grammars.token_table_reset type_token_table;
  List.iter 
    (fun (sym, id, fx, pr) -> add_type_token id sym fx pr) 
    type_reserved_words;
  List.iter (fun (tok, inf) -> add_type_token_info tok inf) 
    type_token_info_list
    
let init_symtab ()=
  init_symbols();
  init_type_token_table();
  init_token_table();
  init_overload()

let init ()= init_symtab ()

(**
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

let term_parser_list ()= !(Grammars.term_parsers_list)
let add_term_parser = Grammars.add_parser
let remove_term_parser = Grammars.remove_parser

let type_parser_list ()= !(Grammars.type_parsers_list)
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


module Resolver =
struct

(**
   [resolve_term env t]: Resolve the symbols in term [t].
   For each free variable [Free(s, ty)] in [t], 
   Lookup [s] in [env] to get long identifier [id]. 
   If not found, use [Free(s, ty)].
   If found, replace [Free(s, ty)] with the identifier [Id(id, ty)].
 *)
open Basic
  
let memo_find cache find table n =
  try
    Lib.find n cache
  with Not_found -> 
    let ret = find n table 
    in 
    (ignore(Lib.bind n ret cache); ret)

type resolve_memo =
    { 
      types : (Basic.ident, Basic.gtype)Hashtbl.t;
      idents: (string, Basic.ident)Hashtbl.t;
      symbols : (string, Basic.ident)Hashtbl.t;
      type_names: (string, Basic.thy_id) Hashtbl.t
    }

type resolve_arg =
    {
     scp: Scope.t;
     inf : int ref;
     memo: resolve_memo;
     qnts: Term.substitution;
     lookup: (string -> gtype -> (ident * gtype))
   }
      
(* 
   [resolve_aux data env expty term]: Resolve names in [term].

   Each type name is expanded to a long name. 

   Each free variable [n] (which may be an overloaded symbol or name)
   is resolved to an exact identifier.

   [expty] is the type the term is expected to have (can be a variable
   type).

   returns the renamed term, the actual type and a type substitution
   from which the exact type can be obtained (using Gtypes.mgu).

   Never fails.
*)

let rec resolve_aux data env expty term =
  let bind_qnt t1 t2 =
    { data with qnts=(Term.bind t1 t2 data.qnts) }
  in 
  let binding_set_names binding =
    let (qnt, qname, qtype) = Basic.dest_binding binding
    in 
    Basic.mk_binding qnt qname 
      (Gtypes.set_name ~memo:(data.memo.type_names) (data.scp) qtype)
  in 
  let set_type_name t =
    Gtypes.set_name ~memo:(data.memo.type_names) (data.scp) t
  in 
  let ident_find n s = 
    let thy = Scope.thy_of_term s n
    in 
    Basic.mk_long thy n
  in 
  let find_ident n = 
    (try 
      Some(memo_find data.memo.idents ident_find data.scp n)
    with Not_found -> None)
  in 
  let type_find n s = Scope.type_of s n
  in 
  let find_type n = 
    (try 
      Some 
	(Gtypes.copy_type 
	   (memo_find data.memo.types type_find data.scp n))
    with Not_found -> None)
  in 
  let find_sym n ty= 
    try Some(data.lookup n ty)
    with Not_found -> None
  in 
  match term with
    Id(n, ty) -> 
      let id_ty = find_type n
      in 
      let nty = set_type_name ty
      in 
      let (ty0, env0)=
	try
	  (nty, Gtypes.unify_env data.scp expty nty env)
	with _ -> (nty, env)
      in 
      let (ty1, env1)=
	(match id_ty with
	  None -> (ty0, env0)
	| Some(d_ty) -> 
	    (try 
	      (d_ty, Gtypes.unify_env data.scp ty0 d_ty env0)
	    with _ -> (d_ty, env0)))
      in 
      (Id(n, (Gtypes.mgu ty1 env1)), ty1, env1)
  | Free(n, ty) -> 
      let nty = set_type_name ty
      in 
      let (ty0, env0)=
	try (nty, Gtypes.unify_env data.scp expty nty env)
	with _ -> (nty, env)
      in 
      let ty1=
	try Gtypes.mgu ty0 env0
	with _ -> ty0
      in 
      (match (find_sym n ty1) with
	None -> 
	  (match (find_ident n) with
	    None -> (Free(n, ty1), ty1, env0) 
	  | Some(id3) -> 
	      (resolve_aux data env0 ty1 (Id(id3, ty1))))
      | Some(id2, ty2) -> 
	  resolve_aux data env0 ty1 (Id(id2, ty2)))
  | Bound(q) -> 
      let term1=
	try
	  Term.find term data.qnts
	with Not_found -> term
      in 
      let ty = Term.get_binder_type term1
      in
      let (ty0, env0)=
	try (ty, Gtypes.unify_env data.scp expty ty env)
	with _ -> (ty, env)
      in 
      (term1, ty0, env0)
  | Const(c) ->
      let ty = Logicterm.typeof_cnst c
      in
      let (ty0, env0)=
	try (ty, Gtypes.unify_env data.scp expty ty env)
	with _ -> (ty, env)
      in 
      (term, ty0, env0)
  | Typed(trm, ty) -> 
      let nty = set_type_name ty
      in 
      let (ty0, env0)=
	try (nty, Gtypes.unify_env data.scp expty nty env)
	with _ -> (nty, env)
      in 
      let trm1, nty1, env1 = 
	resolve_aux data env0 nty trm
      in 
      let (nty2, env2)=
	try (nty1, Gtypes.unify_env data.scp nty nty1 env1)
	with _ -> (nty1, env1)
      in 
      (Typed(trm1, nty2), nty2, env2)
  | App(lf, a) -> 
      let argty = Gtypes.mk_typevar data.inf
      in 
      let rty0 = Gtypes.mk_typevar data.inf
      in 
      let (rty1, env1)=
	try (rty0, Gtypes.unify_env data.scp expty rty0 env)
	with _ -> (rty0, env)
      in 
      let fty0 = Logicterm.mk_fun_ty argty rty1
      in 
      let (atrm, aty, aenv) = 
	resolve_aux data env1 (Gtypes.mgu argty env1) a
      in  
      let (ftrm, fty, fenv) = 
	resolve_aux data aenv (Gtypes.mgu fty0 aenv) lf
      in 
      (App(ftrm, atrm), (Gtypes.mgu rty1 fenv), fenv)
  | Qnt(qnt, body) ->
      (match Basic.binder_kind qnt with
	Lambda -> 
	  let qnt1=binding_set_names qnt
	  in 
	  let data1=bind_qnt (Bound(qnt)) (Bound(qnt1))
	  in 
	  let aty = Term.get_qnt_type (Bound qnt1)
	  and rty = Gtypes.mk_typevar data1.inf
	  in 
	  let nty0 = Logicterm.mk_fun_ty aty rty
	  in 
	  let (nty1, env1)=
	    try (nty0, Gtypes.unify_env data1.scp expty nty0 env)
	    with _ -> (nty0, env)
	  in 
	  let (body1, bty, benv) = 
	    resolve_aux data1 env1 rty body
	  in
	  (Qnt(qnt1, body1), nty1, benv)
      | _ -> 
	  let qnt1=binding_set_names qnt
	  in 
	  let data1=bind_qnt (Bound(qnt)) (Bound(qnt1))
	  in 
	  let (nty1, env1)=
	    try (Logicterm.mk_bool_ty, 
		 Gtypes.unify_env data1.scp expty Logicterm.mk_bool_ty env)
	    with _ -> (Logicterm.mk_bool_ty, env)
	  in 
	  let (body1, bty, benv)=
	    resolve_aux data1 env1 nty1 body
	  in 
	  (Qnt(qnt1, body1), nty1, benv))

let resolve_term scp lookup term=
  let rmemo=
    { 
      types = Lib.empty_env(); idents=Lib.empty_env();
      symbols=Lib.empty_env(); type_names = Lib.empty_env()
    }
  in 
  let data = 
    { 
      scp = scp;
      inf= ref 0;
      memo = rmemo;
      qnts = Term.empty_subst();
      lookup = lookup
    }
  in 
  let expty = Gtypes.mk_null()
  in 
  let (term1, ty1, subst) = 
    resolve_aux data (Gtypes.empty_subst()) expty term
  in 
  (term1, subst)

(* 
   find_type ty list: 
   return first identifier-type pair in list 
   where ty matches the type.
   
   Matching is by equality.
 *)
let rec find_type scp ty list =
  let matching_types t1 t2 = 
    try
      ignore(Gtypes.unify scp t1 t2); true
    with _ -> false
  in 
  let default = 
    match list with 
      [] -> None
    | (x::_) -> Some x
  in 
  match list with
    [] -> 
      raise Not_found
  | ((id, id_type)::xs) -> 
      if matching_types ty id_type
      then (id, id_type)
      else find_type scp ty xs

let make_lookup scp db s ty= 
  let type_list = db s
  in 
  let (id, id_type) = find_type scp ty type_list
  in 
  (id, id_type)


let ovl scp= 
  make_lookup scp get_overload_list 

end
