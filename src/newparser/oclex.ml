(* term/type parsing with camlp4 *)

module Oclex = 
struct

open Logicterm

  type symbols =
      DOT | COMMA | ORB | CRB | RIGHTARROW | PRIME | COLON 
    | OTHER of string

  type tok = (string* string)

  exception Error
  exception Lexer
  exception Found_match of (bool * int * tok)

  let eof_tok = ("EOF", "")
  let null_tok = ("", "")

(* 
   symtable:  
      ( size  * number of symbols with this size)
   and (symbol, token) table 
*)

  type token_table=(string, tok)Hashtbl.t

  type symtable=((char * (int)Counter.t) list * token_table)

(*
  type symtable=((char * ((int* int) list)) list
		   * token_table)
*)
  let add_sym_size sz lst=Counter.add sz lst

(*
  let add_sym_size sz lst=
    let rec add_aux ls=
      match ls with 
	[] -> [(sz, 1)]
      |	(s, nm)::xs -> 
	  if s=sz 
	  then (s, nm+1)::xs
	  else 
	    if s<sz 
	    then (sz, 1)::(s, nm)::xs
	    else (s, nm)::(add_aux xs)
    in 
    add_aux lst
*)

  let add_char_info c sz lst=
    let rec add_aux ls =
      match ls with 
	[] -> [(c, add_sym_size sz (Counter.empty()))]
      | (ch, sizes) :: xs ->
	  if(ch=c)
	  then 
	    (ch, add_sym_size sz sizes)::xs
	  else
	    if(ch>c)
	    then 
	      (c, add_sym_size sz (Counter.empty()))::(ch, sizes)::xs
	    else 
	      (ch, sizes)::(add_aux xs)
    in 
    add_aux lst

  let remove_sym_size sz lst=Counter.remove sz lst
(*
  let remove_sym_size sz lst=
    let rec remove_aux ls=
      match ls with 
	[] -> []
      |	(s, nm)::xs -> 
	  if s=sz 
	  then 
	    (if nm=1 then xs
	    else (s, nm-1)::xs)
	  else 
	    if s<sz 
	    then (s, nm)::xs
	    else (s, nm)::(remove_aux xs)
    in 
    remove_aux lst
*)

  let remove_char_info c sz lst=
    let rec remove_aux ls =
      match ls with 
	[] -> []
      | (ch, sizes) :: xs ->
	  if(ch=c)
	  then 
	    (let nsizes=remove_sym_size sz sizes 
	    in 
	    if(Counter.is_empty nsizes)
	    then xs
	    else (ch, nsizes)::xs)
	  else
	    if(ch>c)
	    then 
	      (ch, sizes)::xs
	    else 
	      (ch, sizes)::(remove_aux xs)
    in 
    remove_aux lst

  let add_sym (ls, tbl) s tk =
    let sz=String.length s
    in 
    if(sz>0)
    then 
      Hashtbl.add tbl s tk;
      (add_char_info (String.get s 0) sz ls, tbl)

  let find_sym (_, tbl) s=
    Hashtbl.find tbl s

  let remove_sym (ls, tbl) s=
    Hashtbl.remove tbl s;
    (remove_sym_size (String.length s) ls, tbl) 


(*
  let find_sym_size lst =
    match lst with 
      [] -> 0
    | (sz, _)::_ -> sz
*)

  let find_char_info c lst=Counter.find c lst
(*
  let find_char_info c lst=
    let rec find_aux ls =
      match ls with 
	[] -> raise Not_found
      | (ch, sizes) :: xs ->
	  if(ch=c)
	  then 
	    sizes
	  else
	    if(ch>c)
	    then 
	      raise Not_found
	    else 
	      find_aux xs
    in 
    find_aux lst
*)

  let largest_sym c (ls, _) = 
    match (find_char_info c ls) with
      [] -> raise Not_found
    | (sz, _)::_ -> sz
	

  let lookup_sym (cinfo, tbl) strng=
    let str_sz=String.length strng
    in 
    let rec lookup_aux ls =
      match ls with
	[] -> raise Not_found
      |	(sz, nm)::xs ->
	  if(sz>str_sz)
	  then lookup_aux xs
	  else
	  try
	    (let tok=
	      find_sym (cinfo, tbl) 
		(String.sub strng 0 sz)
	    in 
	    (sz, tok))
	  with Not_found -> lookup_aux xs
    in 
    if(str_sz>0) 
    then
      lookup_aux (find_char_info (String.get strng 0) cinfo)
    else 
      raise Not_found

  let mk_symtable size = ([], Hashtbl.create size)

(* stream and string utility functions *)

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

(* lexing functions *)

  let white_space =  [' '; '\n'; '\t'; '\r']
  let special  =  ['_'; '\''; '.'] 
  let special_alpha  =  ['_'; '?'; '.'; '\''] 

  let is_negate_char c = (c= '-')
  let is_space c = List.mem c white_space
  let is_special c = List.mem c special
  let is_special_alpha c=  List.mem c special_alpha

  let is_digit c =
    ((c>= '0') & (c <='9'))

  let is_alpha c =
     ((c>= 'a') & (c <='z'))
    or ((c>= 'A') & (c<='Z'))

  let is_num str =
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
      

  let other str = 
    if is_num str  
    then get_num str 
    else raise Error

  let mk_id str = ("ID", str)

  let match_keywords symtable strm =
    if stream_empty strm 
    then (true, eof_tok)
    else 
      try 
	let first_char = List.hd(Stream.npeek 1 strm)
	in 
	let strng=
	  string_implode (Stream.npeek (largest_sym first_char symtable) strm)
	in 
	let (sz, tok)=lookup_sym symtable strng
	in 
	junkn sz strm;
	(true, tok)
      with Not_found -> (false, null_tok)

      
  let get_alpha str =
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
    if stream_empty str then ()
    else 
      (if is_space (stream_peek str) 
      then (ignore(Stream.next str); skip_space str)
      else ())

  let is_empty str =
    if(stream_empty str)
    then (true, eof_tok)
    else (false, null_tok)
  
  let lex symtable str=
    skip_space str;
    let empty, empty_tk = is_empty str
    in 
    if(empty) then empty_tk
    else 
      let key, tok= match_keywords (!symtable) str 
      in 
      if key then tok
      else 
	if (is_alpha (stream_peek str))
	then 
	  (let stra = get_alpha str 
      	  in mk_id stra)
	else 
	  other str 
  
end;;

module Oclexer=
struct

(* 
lexer s: submit characters from stream s to the OcLex.lex
until something matches
*)

  exception Lexing of (int * int)

(* symbols and their tokens *)

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
    (List.concat 
	    [["true", ("TRUE", "")]; ["false", ("FALSE", "")];
	      all_qnts; ex_qnts; lam_qnts; not_ops;
	      and_ops; or_ops; implies_ops; 
	      iff_ops; equals_ops])

  let syms_list = [".", ("DOT", ""); "(", ("ORB", "");
	       ")", ("CRB", ""); ",", ("COMMA", "");
	       "->", ("RIGHTARROW", ""); "'", ("PRIME", "");
	       ":", ("COLON", "")]


  let symtable_size=51
  let symbols= ref (Oclex.mk_symtable symtable_size)

(* set up a symbol table with the built in tokens *)
  let init_symtab () = 
    List.iter 
      (fun (s, tk) -> symbols:=Oclex.add_sym (!symbols) s tk) keywords_list;
    List.iter 
      (fun (s, tk) -> symbols:=Oclex.add_sym (!symbols) s tk) syms_list

(* toplevel lexing function *)
  let lex symtab s=
    let first=Stream.count s
    in 
    let tok = 
      try Oclex.lex symtab s
      with _ -> raise (Lexing (first, Stream.count s))
    in 
    let last = Stream.count s
    in 
    (tok, (first, last))

(* camlp4 lexing function *)
  let ocp4lex=
    {
     Token.func=Token.lexer_func_of_parser (lex symbols);
     Token.using = (fun x -> ());
     Token.removing = (fun x -> ());
     Token.tparse = (fun x -> None);
     Token.text = Token.lexer_text
   } 

end


