(* term/type parsing with camlp4 *)

module Oclex = 
struct

open Logicterm

  type symbols =
      DOT | COMMA | ORB | CRB | RIGHTARROW | PRIME | COLON 
    | OTHER of string

(*
  type keys = ALL | EX | LAM (* | AND | OR | NOT | IMPLIES | EQUALS | IFF*)

  type tok = 
      Key of keys | Sym of symbols | ID of Basic.fnident
    | NUM of string | BOOL of bool
*)

  type tok = (string* string)

  exception Error
  exception Lexer
  exception Found_match of (bool * int * tok)

let stream_empty s = 
  try Stream.empty s; true
  with Stream.Failure -> false

let stream_peek s = 
  match (Stream.peek s) with
    None -> raise Not_found
  | Some(c) -> c

let string_implode cs =
  String.concat "" (List.map (fun x -> String.make 1 x) cs)

let rec junkn n str= 
  if n=0 then ()
  else (Stream.junk str; 
	junkn (n-1) str)


  let white_space =  [' '; '\n'; '\t'; '\r']
  let special  =  ['_'; '\''; '.'] 
  let special_alpha  =  ['_'; '?'; '.'; '\''] 

  let all_qnts = [("!", ("ALL","")); 
		  ("all", ("ALL","")); ("forall", ("ALL",""))]
  let ex_qnts = [("?", ("EXISTS","")); ("exists", ("EXISTS",""))]
  let lam_qnts = [("%", ("LAM","")); "lambda", ("LAM","")]
  let not_ops = ["~", ("NOT", ""); 
		 "not", ("NOT", "")]
  let and_ops = ["&", ("AND", "");
		 "and", ("AND", "")]
  let or_ops =  ["|", ("OR", "");
		 "or", ("OR", "")]
  let implies_ops = ["=>", ("IMPLIES", "");
		     "implies",  ("IMPLIES", "")]
  let iff_ops = ["<=>", ("IFF", "");
		 "iff", ("IFF", "")]
  let equals_ops = ["=", ("EQUALS", "")]

  let keywords_list = 
    ref  (List.concat 
	    [["true", ("TRUE", "")]; ["false", ("FALSE", "")];
	      all_qnts; ex_qnts; lam_qnts; not_ops;
	      and_ops; or_ops; implies_ops; 
	      iff_ops; equals_ops])

  let syms_list = ref [".", ("DOT", ""); "(", ("ORB", "");
	       ")", ("CRB", ""); ",", ("COMMA", "");
	       "->", ("RIGHTARROW", ""); "'", ("PRIME", "");
	       ":", ("COLON", "")]

  let keywords ()= !keywords_list

  let rec remove s ls =
    Lib.filter (fun (x, y) -> x=s) ls

  let rec token_of s ls =
    match ls with
      [] -> raise Not_found
    | (key, t)::keys -> 
	if s=key then t else token_of s keys

  let rec add_to_list lessp x ls=
    match ls with 
      [] -> [x]
    | (y::ys) -> 
	if (lessp x y) 
	then (x::y::ys)
	else y::(add_to_list lessp x ys)

  let add_keyword k t =
      keywords_list:=add_to_list 
	   (fun x y -> 
	     (fst y) < (fst x)) (k, t) (!keywords_list)

  let remove_keyword k = keywords_list:=remove k (!keywords_list)

  let syms () = !syms_list

  let add_symbol k t =
    try (ignore(token_of k (syms())); failwith ("Symbol "^k^" exists"))
    with Not_found ->
      syms_list:=add_to_list 
	   (fun x y -> (fst y) < (fst x)) (k, t) (!syms_list)
  let remove_symbol k = syms_list:=remove k (!syms_list)


  let is_negate_char c = (c= '-')
  let is_space c = List.mem c white_space
  let is_special c = List.mem c special
  let is_special_alpha c=  List.mem c special_alpha

  let is_digit c =
    ((c>= '0') & (c <='9'))

  let is_alpha c =
     ((c>= 'a') & (c <='z'))
    or ((c>= 'A') & (c<='Z'))

  let is_num str i =
    if is_digit (stream_peek str)
    then true
    else 
      if (is_negate_char (stream_peek str))
      then 
	(let cs=(Stream.npeek 2 str)
	in 
	match cs with
	  [n; c] -> ((is_negate_char n) & (is_digit c))
	|	 _ -> false)
      else false

  let is_idstr str =
    let c=stream_peek str
    in 
    (is_alpha c) 
      or (is_digit c) 
      or (is_special c)


  let get_num str =
    let tok=ref []
    in 
    let rec get_aux () =
      if(stream_empty str)
      then ()
      else 
	let c=stream_peek str
	in 
	if(is_digit c)
	then (ignore(Stream.next str); 
	      tok:=c::(!tok); get_aux())
	else ()
    in 
    (if(is_negate_char (stream_peek str))
    then (ignore(Stream.next str); tok:=['-']; get_aux())
    else get_aux());
    ("NUM", string_implode (List.rev (!tok)))
      
  let other_lex = ref (
    fun str i -> 
      if is_num str i 
      then get_num str
      else raise Error)

  let set_other l = other_lex:=l
  let other () str i= 
    if is_num str i 
    then get_num str 
    else raise Error

(*
  let mk_id str = ("ID", (Lib.chop_at '.' str))
*)    
  let mk_id str = ("ID", str)

(* lexers.ml *)

  let match_str str i s =
    let szs= String.length s
    in 
    let inp=string_implode (Stream.npeek szs str)
    in 
    if inp=s
    then 
      (junkn szs str;
       raise (Found_match 
		(true, i+szs, mk_id  "")))
    else ()

  let match_with_list str i kls =
    try 
      ((List.iter 
	  (fun (x, y) -> 
	    (try 
	      (match_str str i x)
	    with Found_match (_, nxt, _) ->
	      raise (Found_match (true, nxt, y))))
	  kls); (false, i, mk_id ""))
    with Found_match x -> x
	
  let match_keywords str i = 
    match_with_list str 0 (keywords())
      
  let match_syms str i = 
    match_with_list str 0(syms())
      
  let get_alpha str i =
    let tok=ref []
    in 
    let rec get_aux () =
      if(stream_empty str)
      then ()
      else 
	let c=stream_peek str
	in 
	if(is_alpha c) or (is_special_alpha c) or (is_digit c)
	then (ignore(Stream.next str); 
	      tok:=c::(!tok); get_aux())
	else ()
    in get_aux(); string_implode (List.rev (!tok))


  let rec skip_space str =
    if is_space (stream_peek str) 
    then (ignore(Stream.next str); skip_space str)
    else ()
  
  let rec scan str i=
    if stream_empty str then ("EOF", "")
    else 
      (skip_space str;
       let key, nxt, tok= match_keywords str i 
       in 
       if key then tok
       else 
	 if (is_alpha (stream_peek str))
	 then 
	   (let stra = get_alpha str i
      	   in mk_id stra)
	 else 
	   (let sym, nxt, symtok = (match_syms str i)
	   in 
	   if sym then symtok
	   else 
	     try 
	       (let othtok = (other()) str i
	       in othtok)
	     with Error -> raise Lexer))
	
  let lex str = scan str 0

end;;

module Oclexer=
struct

(* 
lexer s: submit characters from stream s to the OcLex.lex
until something matches
*)

  let lex s=
    let tok = Oclex.lex s
    in 
    let last = Stream.count s
    in 
    (tok, (0, last))


  let ocp4lex =
    {
     Token.func=Token.lexer_func_of_parser lex;
     Token.using = (fun x -> ());
     Token.removing = (fun x -> ());
     Token.tparse = (fun x -> None);
     Token.text = Token.lexer_text
   } 

end


