module Counter :
sig
(* counter: keep track of how many instances of a thing there are *)
    type 'a t = ('a * int) list

(* construct, recogniser *)
    val empty : unit -> 'a list
    val is_empty : 'a list -> bool

(* add x to list lst:
   if x is in lst, increment the count,
   other wise add x to lst, set count to 1
*)
    val add : 'a -> ('a * int) list -> ('a * int) list

(* remove x from list lst:
   if x is not in lst, do nothing

   if x is in lst, decrement the count,
   if new count is 0, remove x from the list
*)
    val remove : 'a -> ('a * int) list -> ('a * int) list

(* find x in list lst:
   if x is not in lst, raise Not_found

   if x is in lst, return the size
*)
    val find : 'a -> ('a * 'b) list -> 'b
end


exception Lexing of (int * int)

type symbols =
    DOT
  | COMMA
  | ORB
  | CRB
  | RIGHTARROW
  | PRIME
  | COLON
  | OTHER of string
  | NULL_SYMBOL
type keys = ALL | EX | LAM

type token_info = 
    Basic.ident
      * Parserkit.Info.fixity 
      * int

type tok =
    Key of keys
  | Sym of symbols
  | ID of Basic.ident (* * token_info option*)
  | NUM of string
  | BOOL of bool
  | EOF
  | NULL

exception Error
exception Lexer

val eof_tok : tok
val null_tok : tok
val string_of_token : tok -> string

val match_tokens : tok -> tok -> bool

val mk_ident : Basic.ident -> tok

type symbol_table=(string, tok)Hashtbl.t
type symtable = (char * int Counter.t) list * symbol_table

val add_sym_size : 'a -> ('a * int) list -> ('a * int) list
val add_char_info :
  'a -> 'b -> ('a * ('b * int) list) list -> ('a * ('b * int) list) list

val remove_sym_size : 'a -> ('a * int) list -> ('a * int) list
val remove_char_info :
  'a -> 'b -> ('a * ('b * int) list) list -> ('a * ('b * int) list) list

(* add_sym tbl s t: 
   add s to symtable tbl as the representation of token t
   fails if s is already in the table
*)

val add_sym :
    symtable -> string -> tok -> symtable

(* remove_sym tbl s: 
   remove s from symtable tbl
*)
val remove_sym :
    symtable -> string -> symtable

val largest_sym : 'a -> ('a * ('b * 'c) list) list * 'd -> 'b

val find_char_info : 'a -> ('a * 'b) list -> 'b

(*
   find_sym: find a symbol in symtable, 
   return associated token
   raise Not_found if symbol not in table 
*)
val find_sym : 
   symtable -> string -> tok

(*
   lookup_sym symtable strn: 
   try to match a symbol at the beginning of string str
   return symbol and size of matched string
   raise Not_found if no matching symbol

   Uses longest substring matching:
   - looks at first character of string
   - finds length of longest symbol beginning with that character
   - beginning with this length, trys initial sub-strings of 
     decreasing size until a match is found
*)
val lookup_sym :
    symtable -> string -> int * tok

val mk_symtable : int -> 'a list * ('b, 'c) Hashtbl.t

val stream_empty : 'a Stream.t -> bool
val stream_peek : 'a Stream.t -> 'a
val stream_test : ('a -> bool) -> 'a Stream.t -> bool

val string_implode : char list -> string
val junkn : int -> 'a Stream.t -> unit
val white_space : char list
val special : char list
val special_alpha : char list

val is_negate_char : char -> bool
val is_space : char -> bool
val is_special : char -> bool
val is_special_alpha : char -> bool
val is_digit : char -> bool
val is_alpha : char -> bool
val is_num : char Stream.t -> bool
val is_identifier_char : char  -> bool
val is_dot : char  -> bool

val get_num : char Stream.t -> tok
val other : char Stream.t -> tok

val get_sep_list: 
    (char->bool) -> (char->bool) -> (char->bool)
	->  char Stream.t -> string list

val get_alpha : char Stream.t -> string
val get_while: (char -> bool) -> char Stream.t -> string

(*
val match_alpha : symtable -> char Stream.t -> bool * tok
*)

val match_identifier : char Stream.t -> bool * tok

val match_keywords : symtable -> char Stream.t -> bool * tok

val skip_space : char Stream.t -> unit

val is_empty : 'a Stream.t -> bool * tok

val lex : symtable -> char Stream.t -> tok

val lexfn : symtable -> char Stream.t -> tok

val scan : symtable ->char Stream.t -> tok Parserkit.Input.t

val reader : (char Stream.t -> 'a) -> ('a -> 'b) -> string -> 'b
