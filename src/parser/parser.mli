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

(* default precedence, fixity and associativity  *)
val default_term_prec: int
val default_term_assoc: Parserkit.Info.associativity
val default_term_fixity: Parserkit.Info.fixity

val default_type_prec: int
val default_type_assoc: Parserkit.Info.associativity
val default_type_fixity: Parserkit.Info.fixity

module Grammars :
    sig
      type input = Pkit.input
      type 'a phrase = 'a Pkit.phrase

      type token_info =
	  (Basic.ident
	     * Parserkit.Info.fixity 
	     * int) option

      type token_table
      val token_table_new: unit -> token_table
      val token_table_reset: token_table -> unit
      val token_table_add: token_table -> Lexer.tok -> token_info -> unit
      val token_table_find: token_table -> Lexer.tok-> token_info
      val token_table_remove: token_table -> Lexer.tok -> unit

      type infotyp = 
	  { 
	    (* term information *)
	    bound_names: (string* Term.term) list ref;
	    token_info: Pkit.token -> token_info;
              (* type information *)
      	      typ_indx : int ref;
	      typ_names: (string* Gtypes.gtype)list ref;
              type_token_info: Pkit.token -> token_info
	  }

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

      val mk_token_info : token_info-> Pkit.token_info
      val mk_type_token_info : token_table -> Lexer.tok -> Pkit.token_info
      val mk_empty_inf :  token_table -> token_table -> infotyp
      val mk_inf : token_table -> token_table -> infotyp

      val error : 'a phrase

      val alternates : (infotyp -> 'a phrase) list  -> infotyp -> 'a phrase

      val other_type_parsers_list : 
	  (infotyp -> (Gtypes.gtype phrase)) Lib.named_list ref
      val other_type_parsers : infotyp -> (Gtypes.gtype phrase)

      val other_parsers_list :
	  (infotyp -> Term.term phrase) Lib.named_list ref
      val other_parsers : infotyp ->Term.term phrase

      val mk_type_binary_constr :
	  infotyp -> 
	    Lexer.tok -> Gtypes.gtype -> Gtypes.gtype -> Gtypes.gtype
      val mk_type_unary_constr : 	
	  infotyp -> 
	    Lexer.tok -> Gtypes.gtype -> Gtypes.gtype
      val mk_conn :
	  'a -> infotyp -> Lexer.tok -> Term.term -> Term.term -> Term.term
      val mk_prefix : 'a -> infotyp -> Lexer.tok -> Term.term -> Term.term

(* basic parsers *)
(*
   id: identifiers which occur in terms
 *)
      val id : infotyp -> Basic.ident phrase

(*
   type_id: identifiers which occur in types
 *)
      val type_id : infotyp -> Basic.ident phrase

(*
   named_id: get a specific identifer
 *)
      val named_id : 
	  infotyp 
	-> (infotyp -> Basic.ident phrase)
	  -> Basic.ident -> Basic.ident phrase

      val number : Num.num phrase
      val boolean : bool phrase
      val none : 'a list phrase
      val bool_type : infotyp -> Gtypes.gtype phrase
      val num_type : infotyp -> Gtypes.gtype phrase
      val comma_list : 'a phrase -> 'a list phrase
      val listof : 'a phrase -> 'a list phrase
      val repeat_term : 'a phrase -> 'b phrase -> 'a list phrase
      val tlistof :
	  'a phrase -> 'b phrase -> 'a list phrase
      val short_id : (infotyp -> Basic.ident phrase) 
	-> infotyp -> string phrase
      val long_id : (infotyp -> Basic.ident phrase)
	-> infotyp -> Basic.ident phrase
      val mk_short_id : (infotyp -> Basic.ident phrase)
	-> infotyp -> string phrase
      val types : infotyp -> Gtypes.gtype phrase
      val inner_types : infotyp -> Gtypes.gtype Pkit.phrase
      val atomic_types : infotyp -> Gtypes.gtype phrase
      val typedef :
	  infotyp ->
	    (string * string list option * Gtypes.gtype option)  phrase
      val mkcomb : Term.term -> Term.term list -> Term.term
      val id_type_op :
	  (infotyp -> 'a Pkit.phrase) ->
	    infotyp -> ('a * Gtypes.gtype) phrase
      val optional_type : infotyp -> Gtypes.gtype option phrase

(*
   term_identifier: 
   parse an identifier that might appear in a quantified term
 *)
      val term_identifier: infotyp -> Term.term phrase
(* form:
   toplevel of term phrase
 *)
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
   list of reserved words, their symbols and properties 
 *)
val reserved_words: 
    (string * Basic.ident
       * Parserkit.Info.fixity 
       * int) list
    

(* tables of symbols and token (parsing) information *)
val symtable_size : int
val symtable : unit -> Lexer.symtable

val token_table: Grammars.token_table
val type_token_table: Grammars.token_table

(* changing and querying the symbol table *)

(* add_symbol sym tok:
   add sym as symbol representing token tok.
   fail silently if sym already exists
 *)
val add_symbol :
    string -> Lexer.tok -> unit

val find_symbol : string -> Lexer.tok
val remove_symbol : string -> unit

val add_token_info : Lexer.tok -> Grammars.token_info -> unit
val add_type_token_info : Lexer.tok -> Grammars.token_info -> unit

val add_token: Basic.ident -> string -> fixity -> int -> unit
val add_type_token: Basic.ident -> string -> fixity -> int -> unit

val remove_token : string -> unit
val remove_type_token : string -> unit

(* init: call init() before using lexer, to install reserved words *)

val init : unit -> unit
val reset : unit -> unit

(* 
   Parsers
   read a given phrase followed by an end of file/string
 *)

(* mk_info: utility function *)
val mk_info : unit -> Grammars.infotyp

type 'a parse = Pkit.input -> 'a
type 'a phrase = 'a Pkit.phrase

val parse : 'a Pkit.phrase -> Pkit.input -> 'a

val identifier_parser : Pkit.input -> Basic.ident

val typedef_parser :
    Pkit.input -> string * string list option * Gtypes.gtype option
val type_parser : Pkit.input -> Gtypes.gtype

val defn_parser :
    Pkit.input -> (string * (string * Gtypes.gtype) list) * Term.term
val term_parser : Pkit.input -> Term.term

(* User defined parsers *)

(* term_parser_list: list of added term parsers *)

val term_parser_list : 
    unit -> (Grammars.infotyp -> Term.term phrase) Lib.named_list

(* add_term_parser pos n ph:
   add term parser ph named n, in relative position pos 
 *)

val add_term_parser :  Lib.position -> string
  -> (Grammars.infotyp -> Term.term phrase)
    -> unit

(* [remove_term_parser s]
   remove term parser named [s], raise [Not_found] if not present 
 *)
val remove_term_parser :  string -> unit

(* type_parser_list: list of added type parsers *)

val type_parser_list : 
    unit -> (Grammars.infotyp -> Gtypes.gtype phrase) Lib.named_list

(* add_term_parser pos n ph:
   add term parser ph named n, in relative position pos 
 *)

val add_type_parser :  Lib.position -> string
  -> (Grammars.infotyp -> Gtypes.gtype phrase)
    -> unit

	(* remove_type_parser s: 
	   remove type parser named s, raise Not_found if not present 
	 *)
val remove_type_parser :  string -> unit

(* readers: read and parse a string *)
val read:  'a parse -> string -> 'a
val read_term : string -> Term.term
val read_type : string -> Gtypes.gtype

val test_lex : string -> Lexer.tok Parserkit.Input.t
val test : string -> Term.term
