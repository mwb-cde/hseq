(*----
  Copyright (c) 2005-2024 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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

module SymbolTree = Treekit.StringTree
type symbol_table= (tok) SymbolTree.t
type symtable = (char * (int)Counter.t) list * symbol_table

let mk_symtable size = ([], SymbolTree.empty)
let clear_symtable (_, tbl) = ([], SymbolTree.empty)

let add_sym_size sz lst = Counter.add sz lst

let add_char_info c sz lst=
  let rec add_aux ls =
    match ls with
    | [] -> [(c, add_sym_size sz (Counter.empty()))]
    | (ch, sizes) :: xs ->
       if(ch=c)
       then (ch, add_sym_size sz sizes)::xs
       else
         if(ch>c)
         then
           (c, add_sym_size sz (Counter.empty()))::(ch, sizes)::xs
         else
           (ch, sizes)::(add_aux xs)
  in
  add_aux lst

let find_char_info c (lst: ('b * (int)Counter.t)list) =
  match List.assoc_opt c lst with
  | Some(x) -> x
  | _ -> Counter.empty()

let remove_sym_size sz lst=Counter.remove sz lst

let remove_char_info c sz lst=
  let rec remove_aux ls =
    match ls with
    | [] -> []
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

let find_sym (_, tbl) s = SymbolTree.find tbl s

let find_sym_opt (_, tbl) s =
  try Some(SymbolTree.find tbl s)
  with Not_found -> None

let add_sym (ls, tbl) s tk =
  let sz = String.length s
  in
  if not (sz > 0)
  then raise (Report.error "Invalid symbol")
  else
    begin
      if SymbolTree.mem tbl s
      then      (ls, tbl)
      else
        begin
          (add_char_info (String.get s 0) sz ls,
           SymbolTree.add tbl s tk)
        end
    end

let remove_sym (ls, tbl) s=
  (remove_char_info (String.get s 0) (String.length s) ls,
   SymbolTree.remove tbl s)

let largest_sym c (ls, _) =
  match find_char_info c ls with
  | (sz, _)::_ -> Some(sz)
  | _ -> None

let lookup_sym ((cinfo, tbl):symtable) strng=
  let str_sz = String.length strng
  in
  let rec lookup_aux ls =
    match ls with
    | [] -> None
    | (sz, _)::(xs:((int)Counter.t)) ->
       if sz > str_sz
       then lookup_aux xs
       else
         let tok_opt = find_sym_opt (cinfo, tbl) (String.sub strng 0 sz) in
         (match tok_opt with
         | Some(tok) -> Some(sz, tok)
         | _ -> lookup_aux xs)
  in
  if str_sz > 0
  then
    let lst = find_char_info (String.get strng 0) cinfo in
    lookup_aux lst
  else None

(***
 * Utility functions
 ***)

module Stream =
  struct
    type ('a) t = ('a) ListSeq.t

    let is_empty s = ListSeq.is_empty s
    let uncons s = ListSeq.uncons s
    let cons x s = ListSeq.push_front [x] s
    let accept s = ListSeq.accept s
    let push_front lst s = ListSeq.push_front lst s

    (*
      let peek s =
      match uncons s with
      | Some(x, rest) -> Some(x, (Seq.cons x rest))
      | _ -> None
     *)
    let first n s = ListSeq.first n s
    let look n s = ListSeq.look n s

    (*
      let apply f s =
      match uncons s with
      | Some(x, rest) -> Some (f x, (Seq.cons x rest))
      | _ -> None
     *)

    let apply f s =
      match ListSeq.first 1 s with
      | ([x], s1) -> Some((f x), s1)
      | _ -> None

    let test p s =
      match ListSeq.look 1 s with
      | ([x], s1) -> (p x, s1)
      | (_, s1) -> (false, s1)

    let rec drop n s = ListSeq.drop n s

    let of_string = ListSeq.of_string

  end

let string_implode cs =
  String.concat "" (List.map (fun x -> String.make 1 x) cs)

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
  ((c>= '0') && (c <='9'))

let is_alpha c =
  ((c>= 'a') && (c <='z'))
  || ((c>= 'A') && (c<='Z'))

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
let get_while test inp =
  let rec get_aux buff strm =
    let (cs, strm1) = Stream.look 1 strm in
    match cs with
    | [c] ->
       if test c
       then get_aux (c::buff) (Stream.accept strm1)
       else (buff, strm1)
    | _ -> (buff, strm1)
  in
  let (chars, outp) = get_aux [] inp in
  (List.rev chars, outp)

(**
   get_sep_list init body sep strm

   Read a list of strings from strm.  Each string must begin with character
   satisfying init and contain characters satisfying body.  'list' must be
   seperated by a single character satisfying 'sep'

   The list is returned with the strings in the order that
   they appear in the stream
   ( "abcd.efgh" -> ["abcd"; "efgh"] )
 **)
let get_sep_list init_test body_test sep_test inp =
  (*
    get_sep: look for a seperator followed by
    an initial character.
   *)
  let get_sep strm =
    let (chrs, strm1) = Stream.look 2 strm in
    match chrs with
    | [x; y] ->
       if (sep_test x) && (init_test y)
       then (Some(x), Stream.drop 1 strm1)
       else (None, strm1)
    | _ -> (None, strm1)
  in
  let rec get_aux buff strm =
    (* get the identifier from the stream *)
    let (chrs, (strm1: (char)Stream.t)) = get_while body_test strm in
    (* test for a seperator *)
    match get_sep strm1 with
    | (Some(_), strm2) ->
       let cstr = string_implode chrs in
       get_aux (cstr::buff) strm2
    | (_, strm2) -> (List.rev buff, strm2)
  in
  match Stream.test init_test inp with
  | (true, strm1) -> get_aux [] strm1 (* got an identifier *)
  | (_, strm1)-> ([], strm1)

let get_alpha str =
  let test c= (is_alpha c) || (is_special_alpha c) || (is_digit c)
  in
  get_while test str

let rec skip_space strm =
  let (ok, strm1) = Stream.test is_space strm in
  if ok
  then skip_space (Stream.accept strm1)
  else strm1

(*** Numbers ***)
let is_num inp =
  match Stream.first 1 inp with
  | ([c], inp1) ->
     if is_digit c
     then (true, inp1)
     else if is_negate_char c
     then
       (match Stream.first 2 inp1 with
        | ([c; n], inp2) ->
           (((is_negate_char c) && (is_digit n)), inp2)
        | (_, inp2) -> (false, inp2))
     else (false, inp1)
  | (_, inp1) -> (false, inp1)

let get_num inp =
  let rec get_aux toks strm =
    let (cs, strm1) = Stream.look 1 strm in
    match cs with
    | [c] ->
       if is_digit c
       then get_aux (c::toks) (Stream.accept strm1)
       else (List.rev toks, strm1)
    | _ -> (List.rev toks, strm1)
  in
  let get_signed_num strm =
    let (cs, strm1) = Stream.first 1 strm in
    match cs with
    | [c] ->
       if is_negate_char c
       then get_aux ['-'] (Stream.accept strm1)
       else get_aux [] strm1
    | _ -> ([], strm1)
  in
  match get_signed_num inp with
  | ([], inp1) -> (None, inp1)
  | (chrs, inp1) -> (Some(string_implode chrs), inp1)

(***
 * Lexer functions
 ***)

type ('a) matcher = symtable -> ('a)Stream.t -> ((tok)option * ('a)Stream.t)

let first_match ll tbl strm =
  let rec first_aux l inp =
    match l with
      [] -> (None, inp)
    | (f::fs) ->
       let (tok_opt, inp1) = f tbl inp in
       match tok_opt with
       | None -> first_aux fs inp1
       | _ -> (tok_opt, inp1)
  in
  first_aux ll strm

(** Match numbers *)
let match_number tbl strm =
  let (nstr_opt, strm1) = get_num strm in
  match nstr_opt with
  | Some(nstr) -> (Some(NUM(nstr)), strm1)
  | _ -> (None, strm1)

let match_other tbl strm = (None, strm)

(** Primed identifiers *)
let match_primed_identifier symtable inp =
  let (ok, inp1) = Stream.test is_prime inp in
  if ok
  then
    let (stra, inp2)  = get_alpha (Stream.accept inp)
    in
    (match stra with
     | [] -> (None, inp2)
     | _ ->
        let idstr = string_implode stra in
        (Some(PrimedID idstr), inp2))
  else (None, inp1)

(**
   Identifiers
   match a string
   beginning with an alphabetic character or '_'
   and continuing with alpha-numeric or special character.
   string may have more than one identifier, seperated
   by character satisfying is_dot
 *)
let match_identifier symtable inp =
  let (ok, inp1) = Stream.test is_identifier_start inp in
  if ok
  then
    let (sep_list, inp2) =
      get_sep_list is_identifier_start is_identifier_char is_dot inp1
    in
    match sep_list with
    | [] -> (None, inp2)
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
       (Some(rtok), inp2)
    | [th;n] -> (Some(mk_ident (Ident.mk_long th n)), inp2)
    | _ -> raise (Lexing(0, 0))
  else (None, inp1)


(**
   match_keywords tbl strm
   read characters from stream strm,
   try to match them with a keyword in symbol table tbl
 **)
let match_keywords (symtable: symtable) strm =
  let get_token ch inp =
     let largest_opt = largest_sym ch symtable in
     if largest_opt = None
     then (None, inp)
     else
       let (chrs, inp1) = Stream.look (Lib.from_some largest_opt) inp in
       let strng = string_implode chrs in
       let tok_opt = lookup_sym symtable strng in
       (match tok_opt with
       | Some(sz, tok) -> (Some(tok), Stream.drop sz inp1)
       | _ -> (None, inp1))
  in
  let (first_char, strm1) = Stream.look 1 strm in
  match first_char with
  | [ch] -> get_token ch strm1
  | _ -> (None, strm1)

(** Match the empty stream. *)
let match_empty tbl strm =
  if Stream.is_empty strm
  then (Some(eof_tok), strm)
  else (None, strm)

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
let rec lex symtable strm =
  let strm1 = skip_space strm in
  let (tok_opt, strm2) = first_match std_lexers symtable strm1 in
  match tok_opt with
  | Some(tok) -> Some(tok, strm2)
  | _ -> None

(**
   scan symtab strm:
   make token input stream from a char stream
   with symbol table symtab
 **)
let scan symtab strm =
  Parserkit.Input.make (fun inp -> lex symtab inp) strm

(**
   reader lex ph str:
   parse string
   using lexer lex
   and parser ph
 **)
let reader lex ph str=
  ph (lex (Stream.of_string str))
