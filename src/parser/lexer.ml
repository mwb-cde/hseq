module Counter=
struct
(* counter: keep track of how many instances of a thing there are *)

(* counter: list of items and the number of times they've been added *)

  type ('a)t=('a * int) list


(* construct, recogniser *)

   let empty () = []

   let is_empty l = match l with [] -> true | _ -> false

(* add x to list lst:
   if x is in lst, increment the count,
   other wise add x to lst, set count to 1
*)

  let add x lst=
    let rec add_aux ls=
      match ls with 
	[] -> [(x, 1)]
      |	(s, nm)::xs -> 
	  if s=x
	  then (s, nm+1)::xs
	  else 
	    if s<x
	    then (x, 1)::(s, nm)::xs
	    else (s, nm)::(add_aux xs)
    in 
    add_aux lst

(* remove x from list lst:
   if x is not in lst, do nothing

   if x is in lst, decrement the count,
   if new count is 0, remove x from the list
*)

  let remove x lst=
    let rec remove_aux ls=
      match ls with 
	[] -> []
      |	(s, nm)::xs -> 
	  if s=x 
	  then 
	    (if nm=1 then xs
	    else (s, nm-1)::xs)
	  else 
	    if s<x 
	    then (s, nm)::xs
	    else (s, nm)::(remove_aux xs)
    in 
    remove_aux lst

(* find x in list lst:
   if x is not in lst, raise Not_found

   if x is in lst, return the size
*)

  let find x lst=
    let rec find_aux ls =
      match ls with 
	[] -> raise Not_found
      | (s, nm) :: xs ->
	  if(s=x)
	  then 
	    nm
	  else
	    if(s>x)
	    then 
	      raise Not_found
	    else 
	      find_aux xs
    in 
    find_aux lst

end


  open Logicterm

  exception Lexing of (int * int)

  type symbols =
      DOT | COMMA | ORB | CRB | RIGHTARROW | PRIME | COLON 
    | OTHER of string

  type keys = ALL | EX | LAM 
	

  type token_info = 
      (Parserkit.Info.fixity      
	 * Parserkit.Info.associativity  
	 * int)     (* precedence *)

  type tok = 
      Key of keys 
    | Sym of symbols 
    | ID of Basic.fnident * (token_info)option
    | NUM of string 
    | BOOL of bool 
    | EOF 
    | NULL

  exception Error
  exception Lexer

  let eof_tok = EOF
  let null_tok = NULL

  let string_of_token tok = 
    match tok with 
    | Sym DOT -> "."
    | Sym COMMA -> ","
    | Sym ORB -> "("
    | Sym CRB -> ")"
    | Sym RIGHTARROW -> "->"
    | Sym PRIME -> "'"
    | Sym COLON -> ":"
    | Sym (OTHER s) -> s
    | Key ALL -> "ALL"
    | Key EX -> "EXISTS"
    | Key LAM -> "LAMBDA"
    | ID(s, _) -> (Basic.string_fnid s)
    | NUM(n) -> n
    | BOOL(b) -> string_of_bool b
    | EOF -> "eof"
    | NULL -> "null"

(* function to match tokens *)

  let match_tokens x y=
    match (x, y) with
      (ID(sx, _), ID(sy, _)) -> sx=sy
    | _ -> x=y

(* token information *)

type fixity=Parserkit.Info.fixity
let nonfix=Parserkit.Info.nonfix
let infix=Parserkit.Info.infix
let prefix=Parserkit.Info.prefix
    
type associativity=Parserkit.Info.associativity
let non_assoc=Parserkit.Info.non_assoc
let left_assoc=Parserkit.Info.left_assoc
let right_assoc=Parserkit.Info.right_assoc

let prec_of t = 
  match t with
    ID(_, Some(_, _, p)) -> p
  | Key _ -> 0
  | _ -> (-1)

let prec_of_type t = 
  match t with
    ID(_, Some(_, _, p)) -> p
  | Sym RIGHTARROW -> 6
  | _ -> (-1)

let token_info t= 
  match t with
    ID(_, Some(f, a, p)) -> f, a, (prec_of t)
  | _ -> (nonfix, non_assoc, -1)

let type_token_info t= 
  match t with
    ID(_, Some(f, a, p)) -> f, a, (prec_of t)
  | Sym RIGHTARROW -> infix, left_assoc, 6
  | _ -> (nonfix, non_assoc, -1)


let is_infix t =
  let f, _, p = token_info t 
  in 
  Parserkit.Info.is_infix f

let is_prefix t =
  let f, _, _ = token_info t 
  in 
  Parserkit.Info.is_prefix f
    


  let mk_ident s = ID(s, None)
  let mk_full_ident s i a p = ID(s, Some(i, a, p))
  let mk_ident_left s i p = ID(s, Some(i, left_assoc, p))
  let mk_ident_right s i p = ID(s, Some(i, right_assoc,p))
  let mk_ident_none s i p = ID(s, Some(i, non_assoc, p))

(* 
   symtable:  
      ( size  * number of symbols with this size)
   and (symbol, token) table 
*)

  type token_table=(string, tok)Hashtbl.t

  type symtable=
      ((char * (int)Counter.t) list  (* (this could be made a tree *)
	 * token_table)

  let add_sym_size sz lst=Counter.add sz lst

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
    (remove_char_info (String.get s 0) (String.length s) ls, tbl)

  let find_char_info c lst=Counter.find c lst

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
    NUM (string_implode (List.rev (!tok)))
      

  let other str = 
    if is_num str  
    then get_num str 
    else raise Error

  let mk_id str = Lib.chop_at '.' str

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
    in 
    get_aux(); 
    string_implode (List.rev (!tok))

  let match_alpha symtable inp =
    if (is_alpha (stream_peek inp))
    then 
      let stra = get_alpha inp
      in 
      let th, n=mk_id stra
      in 
      let tok = 
	try
	  if th=""    (* no theory identifier *)
	  then        (* so check whether n is a symbol *)
	    let sym_tok=find_sym symtable n
	    in 
	    sym_tok
	  else 
	    mk_ident (Basic.mklong th n)
	with Not_found ->  mk_ident (Basic.mklong th n)
      in (true, tok)
    else (false, null_tok)


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
    if empty then empty_tk
    else 
      let is_alpha_tok, alpha_tok = match_alpha symtable str
      in 
      if is_alpha_tok then alpha_tok
      else 
	let key, tok= match_keywords symtable str 
	in 
	if key then tok
	else 
	  other str 
 

(* toplevel lexing functions *)

(*
 lex symtab strm:
   read token from stream strm, with symbol table symtab
*)
   let lexfn symtab strm = 
     let first=Stream.count strm
     in 
     try 
       lex symtab strm
     with _ -> raise (Lexing (first, Stream.count strm))

(*
   scan symtab strm:
   make token input stream from a char stream
   with symbol table symtab
*)

  let scan symtab strm=
    Parserkit.Input.make (fun () -> lex symtab strm)

(* reader lex ph str: 
   parse string 
   using lexer lex
   and parser ph
*)

  let reader lex ph str=
    ph (lex (Stream.of_string str))

