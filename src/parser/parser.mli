module Pkit :
  sig
    exception ParsingError of string
    exception No_match
    type token = Lexer.tok
    type input = token Parserkit.Input.t
    type 'a phrase = input -> 'a * input
    val empty : 'a list phrase
    val get : (token -> bool) -> (token -> 'a) -> 'a phrase
    val ( !$ ) : token -> token phrase
    val ( || ) : 'a phrase -> 'a phrase -> 'a phrase
    val ( !! ) : 'a phrase -> 'a phrase
    val ( -- ) : 'a phrase -> 'b phrase -> ('a * 'b) phrase
    val ( >> ) : 'a phrase -> ('a -> 'b) -> 'b phrase
    val orl : 'a phrase list -> 'a phrase
    val optional : 'a phrase -> 'a option phrase
    val ( $-- ) : 'a phrase -> 'b phrase -> 'b phrase
    val repeat : 'a phrase -> 'a list phrase
    type token_info = {
      fixity : Parserkit.Info.fixity;
      prec : int;
    } 
    val operators :
      'a phrase * (token -> token_info) * (token -> 'a -> 'a -> 'a) *
      (token -> 'a -> 'a) -> 'a phrase
    val parse : 'a phrase -> token -> input -> 'a
  end

module Grammars :
  sig
    type input = Pkit.input
    type 'a phrase = 'a Pkit.phrase

  type infotyp = 
      { 
	scope: Gtypes.scope;
	(* term information *)
	bound_names: (string* Term.term) list ref;
	token_info: Pkit.token -> Pkit.token_info;
        (* type information *)
      	typ_indx : int ref;
	typ_names: (string, Gtypes.gtype)Lib.substype;
        type_token_info: Pkit.token -> Pkit.token_info
      }

(* preivous infotyp
    type infotyp = {
      scope : Gtypes.scope;
      typ_indx : int ref;
      typ_names : (string, Gtypes.gtype) Lib.substype;
      token_info : Pkit.token -> Pkit.token_info;
      type_token_info : Pkit.token -> Pkit.token_info;
    } 
*)
    val string_of_tok : Lexer.tok -> string
    val string_tokens : Lexer.tok list -> string
    val get_type_indx : infotyp -> int
    val mk_vartyp : infotyp -> Gtypes.gtype

    val lookup_name : string -> infotyp -> Term.term
    val add_name : string -> Term.term -> infotyp -> unit
    val drop_name : string -> infotyp -> unit
    val get_term : string -> infotyp -> Term.term
    val clear_names : infotyp -> unit

    val lookup_type_name : string -> infotyp -> Gtypes.gtype
    val add_type_name : string -> Gtypes.gtype -> infotyp -> Gtypes.gtype
    val get_type : string -> infotyp -> Gtypes.gtype
    val clear_type_names : infotyp -> unit

    val mk_token_info : Lexer.tok -> Pkit.token_info
    val mk_type_token_info : Lexer.tok -> Pkit.token_info
    val mk_empty_inf : Gtypes.scope -> infotyp
    val mk_inf : Gtypes.scope -> infotyp
    val scope_of_inf : infotyp -> Gtypes.scope

    val error : 'a phrase

    val alternates : (infotyp -> 'a phrase) list  -> infotyp -> 'a phrase

    val other_type_parsers_list : (infotyp -> (Gtypes.gtype phrase)) list ref
    val add_type_parser : 
	(infotyp -> (Gtypes.gtype phrase)) -> unit
    val other_type_parsers : infotyp -> (Gtypes.gtype phrase)

    val other_parsers_list :
      (infotyp -> Term.term phrase) list ref
    val add_parser :
      (infotyp -> Term.term phrase) -> unit
    val other_parsers : infotyp ->Term.term phrase
    val mk_type_binary_constr :
      Lexer.tok -> Gtypes.gtype -> Gtypes.gtype -> Gtypes.gtype
    val mk_type_unary_constr : Lexer.tok -> Gtypes.gtype -> Gtypes.gtype
    val mk_conn :
      'a -> 'b -> Lexer.tok -> Term.term -> Term.term -> Term.term
    val mk_prefix : 'a -> 'b -> Lexer.tok -> Term.term -> Term.term

(* basic parsers *)
    val id : Basic.fnident phrase
    val number : Num.num phrase
    val boolean : bool phrase
    val none : 'a list phrase
    val named_id : Basic.fnident -> Pkit.token phrase
    val bool_type : Gtypes.gtype phrase
    val num_type : Gtypes.gtype phrase
    val comma_list : 'a phrase -> 'a list phrase
    val listof : 'a phrase -> 'a list phrase
    val repeat_term : 'a phrase -> 'b phrase -> 'a list phrase
    val tlistof :
      'a phrase -> 'b phrase -> 'a list phrase
    val short_id : 'a -> string phrase
    val long_id : 'a -> Basic.fnident phrase
    val mk_short_id : 'a -> string phrase
    val types : infotyp -> Gtypes.gtype phrase
    val inner_types : infotyp -> Gtypes.gtype Pkit.phrase
    val atomic : infotyp -> Gtypes.gtype phrase
    val typedef :
      infotyp ->
	(string * string list option * Gtypes.gtype option)  phrase
    val mkcomb : Term.term -> Term.term list -> Term.term
    val id_type_op :
      (infotyp -> 'a Pkit.phrase) ->
      infotyp -> ('a * Gtypes.gtype) phrase
    val optional_type : infotyp -> Gtypes.gtype option phrase
    val form : infotyp -> Term.term phrase
    val formula : infotyp -> Term.term phrase
    val typed_primary : infotyp -> Term.term phrase
    val primary : infotyp -> Term.term phrase
    val lhs : infotyp -> (string * (string * Gtypes.gtype) list) phrase
    val args_opt : infotyp -> (string * Gtypes.gtype) list phrase
    val defn :
      infotyp ->
	((string * (string * Gtypes.gtype) list) * Term.term) phrase
  end

(*
   token fixity and associativity 
   (exactly the same as used by the lexer
*)

type fixity = Parserkit.Info.fixity
val nonfix : Parserkit.Info.fixity
val infix : Parserkit.Info.associativity -> Parserkit.Info.fixity
val prefix : Parserkit.Info.fixity
val suffix : Parserkit.Info.fixity

type associativity = Parserkit.Info.associativity
val left_assoc : Parserkit.Info.associativity
val right_assoc : Parserkit.Info.associativity
val non_assoc : Parserkit.Info.associativity

(* symbols_list: symbols which can't be redefined *)
val syms_list : (string * Lexer.tok) list

(* reserved_words: 
   list of reserved words, their symbols and properties *)
val reserved_words: 
    (string * Basic.fnident
    * Parserkit.Info.fixity 
       * int) list
    

(* keywords_list: symbols which are needed  but could be reused *)
val keywords_list : (string * Lexer.tok) list

val symtable_size : int
val symtable : unit -> Lexer.symtable

(* changing and querying the symbol table *)
val add_symbol :
  Basic.fnident ->
  string ->
  Parserkit.Info.fixity 
    -> int -> unit

val find_symbol : string -> Lexer.tok
val remove_symbol : string -> unit

(* init: call init() before using lexer, to install reserved words *)

val init : unit -> unit

(* 
   Parsers
   read a given phrase followed by an end of file/string
*)

(* mk_info: utility function *)
val mk_info : Gtypes.scope -> Grammars.infotyp

val parse : 'a Pkit.phrase -> Pkit.input -> 'a

val identifier_parser : Gtypes.scope -> Pkit.input -> Basic.fnident

val typedef_parser :
  Gtypes.scope ->
  Pkit.input -> string * string list option * Gtypes.gtype option
val type_parser : Gtypes.scope -> Pkit.input -> Gtypes.gtype

val defn_parser :
  Gtypes.scope ->
  Pkit.input -> (string * (string * Gtypes.gtype) list) * Term.term
val term_parser : Gtypes.scope -> Pkit.input -> Term.term

(* readers: read and parse a string *)
val read: 
    (Gtypes.scope -> (Pkit.input -> 'a)) 
  -> Gtypes.scope -> string -> 'a
val read_term : Gtypes.scope -> string -> Term.term
val read_type : Gtypes.scope -> string -> Gtypes.gtype

val test_lex : string -> Lexer.tok Parserkit.Input.t
val test : string -> Term.term
