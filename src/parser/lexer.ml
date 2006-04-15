(*-----
 Name: lexer.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

  open Lterm

  exception Lexing of (int * int)

  type symbols =
      DOT | ORB | CRB 
    | PRIME | COLON 
    | OTHER of string
    | NULL_SYMBOL

  let comma_sym = OTHER ","

  type keys = ALL | EX | LAM 
	
  type token_info = 
      (Ident.t   (* identifiers represented by symbols *)
	 * Parserkit.Info.fixity      
	 * int)     (* precedence *)

  type tok = 
      Key of keys 
    | Sym of symbols 
    | ID of Ident.t 
    | PrimedID of string
    | NUM of string 
    | BOOL of bool 
    | EOF 
    | NULL

  let eof_tok = EOF
  let null_tok = NULL

  let mk_ident s = ID(s)
  let mk_symbol s = Sym(OTHER s)

  let string_of_token tok = 
    match tok with 
    | Sym DOT -> "."
    | Sym ORB -> "("
    | Sym CRB -> ")"
    | Sym PRIME -> "'"
    | Sym COLON -> ":"
    | Sym (OTHER s) -> s
    | Sym NULL_SYMBOL -> "(null_symbol)"
    | Key ALL -> "ALL"
    | Key EX -> "EXISTS"
    | Key LAM -> "LAMBDA"
    | ID(s) -> (Ident.string_of s)
    | PrimedID s -> ("'"^s)
    | NUM(n) -> n
    | BOOL(b) -> string_of_bool b
    | EOF -> "eof"
    | NULL -> "null"

(**
   [message_of_token tok]
   generate a string description of [tok] suitable for use
   in an error message
*)
  let message_of_token tok = 
    match tok with 
    | Sym DOT -> "."
    | Sym ORB -> "("
    | Sym CRB -> ")"
    | Sym PRIME -> "'"
    | Sym COLON -> ":"
    | Sym (OTHER s) -> s
    | Sym NULL_SYMBOL -> "(null_symbol)"
    | Key ALL -> "ALL"
    | Key EX -> "EXISTS"
    | Key LAM -> "LAMBDA"
    | ID(s) -> (Ident.string_of s)
    | PrimedID(s) -> ("'"^s)
    | NUM(n) -> n
    | BOOL(b) -> string_of_bool b
    | EOF -> "eof"
    | NULL -> "null"


(* [match_tokens]: Compare two tokens. *)
  let match_tokens x y=
    match (x, y) with
      (ID(sx), ID(sy)) -> sx=sy
    | _ -> x=y
(* 
   Support for OCaml antiquotation (Not supported)
   let antiquote_char=ref '^'
   let get_antiquote ()= !antiquote_char
   let set_antiquote c = antiquote_char:=c
*)

(***
* Symbol tables
***)

(**
   A symbol table [symtable] stores strings and the tokens they map
   to.  To assist in matching, the number of strings of each size [n]
   is also stored.
*)

  type symbol_table=(string, tok)Hashtbl.t

  type symtable=
      ((char * (int)Counter.t) list  (* (this could be made a tree) *)
	 * symbol_table)

  let mk_symtable size = ([], Hashtbl.create size)

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
      (try 
	 (Hashtbl.find tbl s; 
	  raise (Result.error ("Symbol "^s^" exists")))
       with 
	 Not_found ->
	   (Hashtbl.add tbl s tk;
	    add_char_info (String.get s 0) sz ls, tbl))
    else 
      raise (Result.error "Invalid symbol")
	
  let find_sym (_, tbl) s=
    Hashtbl.find tbl s

  let remove_sym (ls, tbl) s=
    Hashtbl.remove tbl s;
    (remove_char_info (String.get s 0) (String.length s) ls, tbl)

  let find_char_info c lst=List.assoc c lst

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

(*** 
* Utility functions 
***)
let stream_empty s = 
  try Stream.empty s; true
  with Stream.Failure -> false

let stream_peek s = 
  match (Stream.peek s) with
    None -> raise Not_found
  | Some(c) -> c

let stream_test tst s = 
  match (Stream.peek s) with
    None -> false
  | Some(c) -> tst c

let string_implode cs =
  String.concat "" (List.map (fun x -> String.make 1 x) cs)

let rec junkn n str= 
  if n=0 then ()
  else (Stream.junk str; junkn (n-1) str)

  let white_space =  [' '; '\n'; '\t'; '\r']
  let special  =  ['_'; '\''; '.'] 
  let special_alpha  =  ['_'; '?'; '\''] 

  let is_negate_char c = (c= '-')
  let is_space c = List.mem c white_space
  let is_special c = List.mem c special
  let is_special_alpha c=  List.mem c special_alpha

  let is_dot c = (c='.')

  let is_prime c = (c = '\'')

  let is_digit c =
    ((c>= '0') & (c <='9'))

  let is_alpha c =
     ((c>= 'a') & (c <='z'))
  || ((c>= 'A') & (c<='Z'))
       
  let is_identifier_char c 
      = (is_alpha c) || (is_special_alpha c) || (is_digit c)

  let is_identifier_start c = (is_alpha c) || (c='_')


(***
* Matching functions
***)

(**
  get_while test strm: 
  read the initial string made up of the characters satisfying test.
**)
let get_while test strm=
  let buff=ref []
  in 
  let rec get_aux () = 
    if(stream_empty strm)
    then ()
    else
      let c=stream_peek strm
      in 
      if (test c) 
      then
	(ignore(Stream.next strm); buff:=c::(!buff); get_aux())
      else ()
  in 
  get_aux(); 
  string_implode (List.rev (!buff))


(**
   get_sep_list init body sep strm
   read a list of strings from strm
   each string must begin with character satisfying init
   and contain characters satisfying body.
   list must be seperated by a single character satisfying sep

   the list is returned with the strings in the order that
   they appear in the stream 
   ( "abcd.efgh" -> ["abcd"; "efgh"] )
**)
  let get_sep_list init_test body_test sep_test strm=
  let strlist=ref []
  in 
    (* 
       is_sep: look for a seperator followed by 
       an initial character. 
    *)
  let is_sep stm =
    match (Stream.npeek 2 stm) with
      [x; y] -> 
	if(sep_test x) & (init_test y)
	then 
	  true
	else false
    | _ -> false
  in 
  let rec get_aux () =
    (* get the identifier from the stream *)
    strlist:=(get_while body_test strm)::!(strlist);
    (* test for a seperator *)
    if(is_sep strm)
    then 
      (* got a seperator followed by an identifier *)
	(ignore (Stream.next strm); (* drop the seperator *)
	 get_aux())                 (* and go round again *)
    else()                    (* no seperator, so bail out *)
  in 
  if(stream_test init_test strm)   (* got an identifier *)
  then 
    (get_aux();  (* read the identifiers from the stream *)
     List.rev (!strlist))
  else []

let get_alpha str = 
  let test c= (is_alpha c) || (is_special_alpha c) || (is_digit c)
  in 
  get_while test str

let rec skip_space str =
  if stream_empty str then ()
  else 
    (if (stream_test is_space str) 
    then (ignore(Stream.next str); skip_space str)
    else ())

(*** Numbers ***)
  let is_num str =
    if (stream_test is_digit str)
    then true
    else 
      if (stream_test is_negate_char str)
      then 
	(let cs=(Stream.npeek 2 str)
	in 
	match cs with
	  [n; c] -> ((is_negate_char n) & (is_digit c))
	|	 _ -> false)
      else false

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
    (if(stream_test is_negate_char str)
    then (ignore(Stream.next str); tok:=['-']; get_aux())
    else get_aux());
    NUM (string_implode (List.rev (!tok)))
      
(***
* Lexer functions
***)

  type ('a) matcher = symtable -> 'a Stream.t -> (bool * tok) 

  let first_match ll tbl strm = 
    let rec first_aux l =
      match l with 
	  [] -> (false, null_tok)
	| (f::fs) -> 
	    let (r, tok) = 
	      try (f tbl strm) with _ -> (false, null_tok)
	    in 
	      if r then (r, tok) 
	      else (first_aux fs)
    in 
      first_aux ll

(** Match numbers *)
  let match_number tbl strm = 
    if is_num strm 
    then 
      (true, get_num strm)
    else 
      (false, null_tok)

  let match_other tbl str = (false, null_tok)

(** Primed identifiers *)
  let match_primed_identifier symtable inp=
  if(stream_test is_prime inp)
  then 
    (ignore(Stream.next inp);
    let stra = get_alpha inp
    in 
    if(String.length stra)=0
    then (false, null_tok)
    else (true, PrimedID stra))
  else (false, null_tok)

(** 
    Identifiers 
   match a string 
   beginning with an alphabetic character or '_'
   and continuing with alpha-numeric or special character.
   string may have more than one identifier, seperated 
   by character satisfying is_dot
*)
let match_identifier symtable inp =
  if (stream_test is_identifier_start inp)
  then 
    match (get_sep_list is_identifier_start is_identifier_char is_dot inp) with
      [] -> (false, null_tok)
    | [n] -> 
	(* 
	   Unqualified identifier may be a symbol/keyword 
	   so check in the symbol table for a token to return 
	   if not found, make an unqualified identifier token 
	*)
	let rtok =
	  (try
	    find_sym symtable n
	  with Not_found -> mk_ident(Ident.mk_name n))
	in 
	(true, rtok)
    | [th;n] -> (true, mk_ident (Ident.mk_long th n))
    | _ -> raise (Lexing(0, Stream.count inp))
  else (false, null_tok)


(** 
   match_keywords tbl strm
   read characters from stream strm,
   try to match them with a keyword in symbol table tbl
**)
let match_keywords symtable strm =
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


(** Match the empty stream. *)
let match_empty tbl str =
  if(stream_empty str)
  then (true, eof_tok)
  else (false, null_tok)
  

(** The standard lexers **)
let std_lexers = 
  [
    match_empty; 
    match_primed_identifier;
    match_identifier;
    match_keywords;
    match_number;
    match_other
  ]

(***
* Toplevel functions
***)

(**
 lex symtab strm: read token from stream strm, with symbol table symtab
**)
let rec lex symtable str=
  skip_space str;
  let rslt, tok = first_match std_lexers symtable str
  in 
    if rslt then tok
    else raise (Lexing (0, 0))


(**
   scan symtab strm:
   make token input stream from a char stream
   with symbol table symtab
**)
  let scan symtab strm=
    Parserkit.Input.make (fun () -> lex symtab strm)

(**
   reader lex ph str: 
   parse string 
   using lexer lex
   and parser ph
**)
  let reader lex ph str=
    ph (lex (Stream.of_string str))
