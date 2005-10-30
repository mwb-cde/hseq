(*-----
Name: parser.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

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
      val ( // ) : 'a phrase -> 'a phrase -> 'a phrase
      val ( !! ) : 'a phrase -> 'a phrase
      val ( -- ) : 'a phrase -> 'b phrase -> ('a * 'b) phrase
      val ( >> ) : 'a phrase -> ('a -> 'b) -> 'b phrase
      val alt : 'a phrase list -> 'a phrase
      val named_alt: (string, 'a -> ('b)phrase) Lib.named_list 
	-> 'a -> ('b)phrase
      val seq : ('a phrase) list -> ('a list)phrase
      val named_seq : 
	  (string, 'a -> ('b)phrase) Lib.named_list 
	-> 'a -> ('b list)phrase

      val optional : 'a phrase -> 'a option phrase
      val ( --% ) : 'a phrase -> 'b phrase -> 'b phrase
      val repeat : 'a phrase -> 'a list phrase
      type token_info = 
	  {
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


module Utility: 
    sig
(**
   Some useful parsers which can't really go into Parserkit.
 *)
      open Pkit

(**
   [?$ sym]
   Utility function for building term parsers using Parserkit.seq.
   Parse symbol [sym], return term Term.mk_short_var (Lexer.string_of_token)
 *)
      val (?$): Lexer.tok -> Basic.term phrase
(**
   [?% sym]
   Utility function for building type parsers using Parserkit.seq.
   Parse symbol [sym], return term Gtypes.mk_var (Lexer.string_of_token)
 *)
      val (?%): Lexer.tok -> Basic.gtype phrase
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
	    bound_names: (string* Basic.term) list ref;
	    token_info: Pkit.token -> token_info;

              (* type information *)
      	      typ_indx : int ref;
	      typ_names: (string* Basic.gtype)list ref;
              type_token_info: Pkit.token -> token_info
	  }

      val string_of_tok : Lexer.tok -> string
      val string_tokens : Lexer.tok list -> string
      val get_type_indx : infotyp -> int
      val mk_vartyp : infotyp -> Basic.gtype

      val lookup_name : string -> infotyp -> Basic.term
      val add_name : string -> Basic.term -> infotyp -> unit
      val drop_name : string -> infotyp -> unit
      val get_term : string -> infotyp -> Basic.term
      val clear_names : infotyp -> unit

(**
   [qnt_setup_bound_names inf qnt xs]
   Make bound variables from the name-type pairs in [xs],
   add them to [inf.bound_names]
   [qnt] is the quantifier type (All, Ex or Lambda)
 *)
      val qnt_setup_bound_names: 
	  infotyp -> Basic.quant_ty -> (string * Basic.gtype) list 
	    -> (string * Basic.term) list
(**
   [qnt_term_remove_names inf xs body]
   use bound names in [xs] to form a quantified term, with body
   as the initial term.
   simplified example:  [!x, ?y, !z] t -> (!x: (?y: (!z: t)))

   remove each name in [xs] from [inf.bound_names] as it is used.
 *)
      val qnt_term_remove_names:
	  infotyp -> (string * Basic.term) list 
	    -> Basic.term -> Basic.term

(**
   [make_term_remove_names info wrapper vs body]:

   Remove the variables in [vs] from [info]. 
   Return the term constructed by quantifying 
   [body] with the variables [vs], applying [wrapper] to 
   each constructed term.
*)
      val make_term_remove_names:
	  infotyp 
	-> (Basic.term -> Basic.term)
	  -> (string * Basic.term) list
	      -> Basic.term -> Basic.term


      val lookup_type_name : string -> infotyp -> Basic.gtype
      val add_type_name : string -> Basic.gtype -> infotyp -> Basic.gtype
      val get_type : string -> infotyp -> Basic.gtype
      val clear_type_names : infotyp -> unit

      val mk_token_info : token_info-> Pkit.token_info
      val mk_type_token_info : token_table -> Lexer.tok -> Pkit.token_info
      val mk_empty_inf :  token_table -> token_table -> infotyp
      val mk_inf : token_table -> token_table -> infotyp

      val error : ?msg:string -> 'a phrase

      val type_parsers_list : 
	  (string, infotyp -> (Basic.gtype phrase)) Lib.named_list ref
      val type_parsers : infotyp -> (Basic.gtype phrase)

      val term_parsers_list :
	  (string, infotyp -> Basic.term phrase) Lib.named_list ref
      val term_parsers : infotyp ->Basic.term phrase

      val mk_type_binary_constr :
	  infotyp -> 
	    Lexer.tok -> Basic.gtype -> Basic.gtype -> Basic.gtype
      val mk_type_unary_constr : 	
	  infotyp -> 
	    Lexer.tok -> Basic.gtype -> Basic.gtype
      val mk_conn :
	  'a -> infotyp -> Lexer.tok -> Basic.term -> Basic.term -> Basic.term
      val mk_prefix : 'a -> infotyp -> Lexer.tok -> Basic.term -> Basic.term

(*
   ANTIQUOTATION NOT SUPPORTED
   val antiquote_parser : string phrase
 *)

(* basic parsers *)

      val id_parser: (Lexer.tok -> token_info) -> Basic.ident phrase
      val id_strict: (Lexer.tok -> token_info) -> Basic.ident phrase

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
      val bool_type : infotyp -> Basic.gtype phrase
      val num_type : infotyp -> Basic.gtype phrase
      val comma_list : 'a phrase -> 'a list phrase
      val listof : 'a phrase -> 'a list phrase
      val repeat_term : 'a phrase -> 'b phrase -> 'a list phrase

      val short_id : (infotyp -> Basic.ident phrase) 
	-> infotyp -> string phrase
      val long_id : (infotyp -> Basic.ident phrase)
	-> infotyp -> Basic.ident phrase
      val mk_short_id : (infotyp -> Basic.ident phrase)
	-> infotyp -> string phrase
      val types : infotyp -> Basic.gtype phrase
      val inner_types : infotyp -> Basic.gtype Pkit.phrase
      val atomic_types : infotyp -> Basic.gtype phrase
(*
   val typedef :
   infotyp ->
   (string * string list option * Basic.gtype option)  phrase
 *)

      val mk_comb : Basic.term -> Basic.term list -> Basic.term
      val id_type_opt :
	  (infotyp -> 'a Pkit.phrase) ->
	    infotyp -> ('a * Basic.gtype) phrase
      val optional_type : infotyp -> Basic.gtype option phrase

(*
   term_identifier: 
   parse an identifier that might appear in a quantified term
 *)
      val term_identifier: infotyp -> Basic.term phrase

(* form:
   toplevel of term phrase
 *)
      val form : infotyp -> Basic.term phrase
      val formula : infotyp -> Basic.term phrase
      val typed_primary : infotyp -> Basic.term phrase
      val primary : infotyp -> Basic.term phrase
      val lhs : infotyp -> (string * (string * Basic.gtype) list) phrase
      val args_opt : infotyp -> (string * Basic.gtype) list phrase

(* Definitions *)

(* Type definition *)
      val simple_typedef :
	  infotyp ->
	    (string * string list option * Basic.gtype option)  phrase
      val subtypedef: 
	  infotyp -> 
	    (string * string list option * Basic.gtype * Basic.term)  phrase
      val typedef :
	  infotyp -> (typedef_data)  phrase


(* Term definition *)
      val defn :
	  infotyp ->
	    ((string * (string * Basic.gtype) list) * Basic.term) phrase


(** 
   [parse_as_binder f sym]:
   Construct a grammar to parse function applications
   of the form [f (%x: P)] as [sym x: P].

   Symbol [sym] should be added to the lexer seperately.
   (e.g. using [Parser.add_symbol sym (Lexer.Sym(Lexer.OTHER sym))]).
 *)   
      val parse_as_binder:
	  Basic.ident -> string 
	    -> infotyp -> Basic.term phrase

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

(**
   [add_symbol sym tok]:
   add sym as symbol representing token tok.
   fail silently if sym already exists
 *)
val add_symbol : string -> Lexer.tok -> unit
val find_symbol : string -> Lexer.tok
val remove_symbol : string -> unit

val add_token_info : Lexer.tok -> Grammars.token_info -> unit
val add_type_token_info : Lexer.tok -> Grammars.token_info -> unit

val get_token_info: Lexer.tok -> Grammars.token_info
val get_type_token_info: Lexer.tok -> Grammars.token_info


val add_token: Basic.ident -> string -> fixity -> int -> unit
val add_type_token: Basic.ident -> string -> fixity -> int -> unit

val remove_token : string -> unit
val remove_type_token : string -> unit

(* init: call init() before using lexer, to install reserved words *)

val init : unit -> unit
(*
   val reset : unit -> unit
 *)

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

(*
   val typedef_parser :
   Pkit.input -> string * string list option * Basic.gtype option
 *)
val typedef_parser :
    Pkit.input -> typedef_data

val type_parser : Pkit.input -> Basic.gtype

val defn_parser :
    Pkit.input -> (string * (string * Basic.gtype) list) * Basic.term
val term_parser : Pkit.input -> Basic.term

(* User defined parsers *)

(* term_parser_list: list of added term parsers *)

val term_parser_list : 
    unit -> (string, Grammars.infotyp -> Basic.term phrase) Lib.named_list

(**
   [add_term_parser pos n ph]:
   add term parser ph named n, at position pos 

   [remove_term_parser s]
   remove term parser named [s], raise [Not_found] if not present 
 *)
val add_term_parser :  (string) Lib.position -> string
  -> (Grammars.infotyp -> Basic.term phrase)
    -> unit
val remove_term_parser :  string -> unit

(* type_parser_list: list of added type parsers *)
val type_parser_list : 
    unit -> (string, Grammars.infotyp -> Basic.gtype phrase) Lib.named_list

(**
   [add_term_parser pos n ph]:
   add term parser ph named n, in relative position pos 

   [remove_type_parser s]: 
   remove type parser named s, raise Not_found if not present 
 *)
val add_type_parser :  
    (string)Lib.position -> string
      -> (Grammars.infotyp -> Basic.gtype phrase)
	-> unit
val remove_type_parser :  string -> unit

(* readers: read and parse a string *)
val read:  'a parse -> string -> 'a
val read_term : string -> Basic.term
val read_type : string -> Basic.gtype

val test_lex : string -> Lexer.tok Parserkit.Input.t
val test : string -> Basic.term


(* preliminary support for overloading *)
val overload_table: 
    (string,  (Basic.ident * Basic.gtype) list) Hashtbl.t
val get_overload_list: 
    string -> (Basic.ident * Basic.gtype) list
val add_overload:
    string -> Theory.sym_pos -> (Basic.ident * Basic.gtype) -> unit
val remove_overload:
    string -> Basic.ident -> unit
val print_overloads:
    Printer.ppinfo -> unit


module Resolver :
sig

(** 
   [resolve_term scp env t]: Resolve the symbols in term [t].
   For each free variable [Free(s, ty)] in [t], 
   Lookup [s] in [env] to get long identifier [id]. 
   If not found, use [Free(s, ty)].
   If found, replace [Free(s, ty)] with the identifier [Id(id, ty)].

   [env] should return an identifier-type pair where type matches (in
   some sense) [ty].

   [env] must raise Not_found if [s] is not found.
 *)
val resolve_term:
    Scope.t
  -> (string -> Basic.gtype -> (Basic.ident * Basic.gtype))
    -> Basic.term
      -> (Basic.term * Gtypes.substitution)

(**
   [make_lookup scp db]:
   make an environment suitable for resolve term from db]

   [db] must raise Not_found when items are not found.

   [make_lookup db s ty]: returns the identifier-type pair associated
   by [db] with [s] for which [ty] is unifies with type in scope [scp].

   [make_lookup db s ty] raise Not_found if [s] is not found in [db].
 *)
val make_lookup: 
    Scope.t
  -> (string -> (Basic.ident * Basic.gtype) list) 
    -> (string -> Basic.gtype -> (Basic.ident * Basic.gtype)) 
	
(* functions exposed for debugging *)
type resolve_memo =
    { 
      types : (Basic.ident, Basic.gtype)Hashtbl.t;
      idents: (string, Basic.ident)Hashtbl.t;
      symbols : (string, Basic.ident)Hashtbl.t;
      type_names: (string, Basic.thy_id)Hashtbl.t
    }

type resolve_arg =
    {
     scp: Scope.t;
     inf : int ref;
     memo: resolve_memo;
     qnts: Term.substitution;
     lookup: (string -> Basic.gtype -> (Basic.ident * Basic.gtype))
   }

val resolve_aux:
    resolve_arg
  -> Gtypes.substitution
    -> Basic.gtype
      -> Basic.term
	-> (Basic.term * Basic.gtype * Gtypes.substitution)

val memo_find:
    ('a, 'b)Hashtbl.t
  -> ('a -> 'c -> 'b) 
    -> 'c 
      -> 'a -> 'b

val find_type : 
    Scope.t 
  -> Basic.gtype -> (Basic.ident * Basic.gtype) list 
    -> (Basic.ident * Basic.gtype)


val ovl : 
    Scope.t
  -> (string -> Basic.gtype -> (Basic.ident * Basic.gtype))
end
