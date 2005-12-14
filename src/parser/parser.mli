(*-----
  Name: parser.mli
  Author: M Wahab <mwahab@users.sourceforge.net>
  Copyright M Wahab 2005
  ----*)

(** Parsers for terms and types.
    
    Top-down parsers for terms, types and their definitions using the
    ParserKit constructors, instantiated in the module {!Parser.Pkit}.
    A parser applies the rules of a grammar to the token stream
    constructed by the functions in modules {!Lexer}.  A grammar is
    described as a {!Parser.Grammars.phrase}, and a phrase can also be
    built up from one or more phrases. A parser is built from a phrase
    using function {!Parser.Pkit.parse}.

    The grammars are grouped around terms and types, with some
    miscellaneous utlity parsers. The term parsers include the
    grammars for term definitions. The type parsers include the
    grammars for type definitions.

    This module holds the standard symbol tables and token information
    needed for the toplevel parsers. The toplevel function, for
    parsing a string, is {!Parser.read}, which is a specialisation of
    {!Lexer.reader} using the standard symbol table.

    The toplevel term parsing supports operator overloading,
    implemented in module {!Parser.Resolver}. The toplevel term parser
    is made up of two parts: the first parses a string to construct an
    initial term. The second resolves names in this term, expanding
    short names by finding the theory of an identifier with a matching
    type. The name resolution compares the type inferred for the name
    with the actual type of the identifiers, chosing the first to
    succeed.
       *)

   (** Parser constructors specialised to tokens from {!Lexer}. *)
    module Pkit :
    sig
   
   (** {7 Basic types} *)
      exception ParsingError of string
		(**
	    Raised on parsing errors. A parser which fails to match
	    will raise [ParsingError].
	*)
	   
      type token = Lexer.tok
	  (** The parser tokens *)
      type input = token Parserkit.Input.t
	  (** The parser input stream *)

      type 'a phrase = input -> 'a * input
	(** A grammar rule *)

	(** Token information. *)
      type token_info = 
	  {
	    fixity : Parserkit.Info.fixity; (** Fixity of a token *)
	    prec : int;    (** Precedence of a token *)
	  } 

      (** {5 Parser constructors} *)
   
      val empty : 'a list phrase
	(** The empty phrase. Always produces the empty list. *)
      val get : (token -> bool) -> (token -> 'a) -> 'a phrase
	(**
	    [get pred fn]: Get and transform a token. If the next
	    token satisfies [pred], return the result of applying [fn].
	*)

      val ( !$ ) : token -> token phrase
	(** [ !$ tok ]: Match the token [tok]. *)

      val ( // ) : 'a phrase -> 'a phrase -> 'a phrase
	(**
	   [ph1 // ph2]: Alternation. Try to match [ph1], if it
	   fails, try	to match [ph2].  
	*)

      val ( !! ) : 'a phrase -> 'a phrase
	(**
	   [!! ph]: Require a match for [ph]. Fail completely, raising
	   [Failure], if no match. This will prevent any of the
	   combinators from trying an alternative.
	*)
	
      val ( -- ) : 'a phrase -> 'b phrase -> ('a * 'b) phrase
	(**
	   [ph1 -- ph2]: Concatenation. Match [ph1] then [ph2].
	*)
	
      val ( >> ) : 'a phrase -> ('a -> 'b) -> 'b phrase
	(**
	    [ph >> fn]: Transformation. Match [ph], applying [fn] to
	    the result.
	*)
	
      val alt : 'a phrase list -> 'a phrase
	(**
	    [alt phl]: Alternation on a list. Try each of the phrases
	    in the list [phl], using the first to match.
	*)

      val named_alt: 
	(string, 'a -> ('b)phrase) Lib.named_list -> 'a -> ('b)phrase
	(**
	    [named_alt phl arg]: Alternation on a named list. Try each
	    of the phrases in the list [phl], passing [arg] to each,
	    using the first to match.
	*)

      val seq : ('a phrase) list -> ('a list)phrase
	(**
	    [seq phl]: Concatenation on a list. Match with each of the phrases
	    in the list [phl], in order.
	*)


      val named_seq : 
	(string, 'a -> ('b)phrase) Lib.named_list 
	-> 'a -> ('b list)phrase
	(**
	    [named_seq phl arg]: Concatenation on a named list. Match each
	    of the phrases in the list [phl], in order, passing [arg] to each.
	*)

      val optional : 'a phrase -> 'a option phrase
	(**
	    [optional ph]: Optionally match [ph]. If the match
	    succeeds, producing [x] then return [Some x] otherwise
	    return [None].
	*)

      val ( --% ) : 'a phrase -> 'b phrase -> 'b phrase
	(**
	   [ph1 --% ph2]: Match [ph1] and [ph2], returning result of
	   matching [ph2].
	*)

      val repeat : 'a phrase -> 'a list phrase
	(**
	   [repeat ph]: Match [ph] 0 or more times. Results returned
	   in the order they matched.
	*)

      val multiple : 'a phrase -> 'a list phrase
	(**
	   [multiple ph]: Match [ph] 1 or more times. Results returned
	   in the order they matched.
	*)

      val operators :
	'a phrase * (token -> token_info) * (token -> 'a -> 'a -> 'a) *
	(token -> 'a -> 'a) -> 'a phrase
	(**
	   [operators (ph, info, binop, unop)]: Bottom-up precedence
	   parsing for infix, prefix and postfix operators.
	
	   @param ph Parser for arguments to operators
	   @param info Precedence and fixity information about tokens
	   @param binop Constructors for binary operators.
	   @param unop Constructors for unary operators.
	*)
	
      val parse : 'a phrase -> token -> input -> 'a
	(** 
	    [parse ph eof inp]: Parse input [inp], using phrase [ph], upto the 
	    end of input, which is marked by token [eof].
	*)

    end

(** Useful parser constructors  *)
module Utility: 
sig
  open Pkit

  val (?$): Lexer.tok -> Basic.term phrase
    (**
     [?$ sym]: Utility function for building term parsers using
     {!Parser.Pkit.seq}.  Parse symbol [sym], return term
     [Term.mk_short_var (Lexer.string_of_token)].
  *)
    
  val (?%): Lexer.tok -> Basic.gtype phrase
        (**
       [?% sym]: Utility function for building type parsers using
       {!Parser.Pkit.seq}. Parse symbol [sym], return term [Gtypes.mk_var
       (Lexer.string_of_token)].
    *)
    end

(** {5 Default token information} *)
    
val default_term_prec: int
    (** Default term precedence *)
val default_term_assoc: Parserkit.Info.associativity
    (** Default term associativity *)
val default_term_fixity: Parserkit.Info.fixity
    (** Default term fixity *)

val default_type_prec: int
    (** Default type precedence *)
val default_type_assoc: Parserkit.Info.associativity
    (** Default type associativity *)
val default_type_fixity: Parserkit.Info.fixity
    (** Default type fixity *)

    
(** {5 Parser grammars} *)

(** {7 Basic types used by the parsers. } *)

type associativity = Parserkit.Info.associativity
(** Token associativity (exactly the same as used by the lexer) *)
val left_assoc : associativity
(** Left associative *)
val right_assoc : associativity
(** Right associative *)
val non_assoc : associativity
(** Non-associative *)

type fixity = Parserkit.Info.fixity
(** Token fixity (exactly the same as used by the lexer) *)
val nonfix : fixity
(** Non-fix **)
val infix : associativity -> fixity
(** Infix of given associativity **)
val prefix : fixity
(** Prefix **)
val suffix : fixity
(** Suffix **)

(**
   Information returned by the type definition parsers.
*)
type typedef_data =
    NewType of (string * (string list)) 
      (** A new type: the type name and its arguments. *)
  | TypeAlias of (string * (string list) * Basic.gtype)
        (** 
	    A type alias: the type name, its arguments and the type it aliases
	*)
  | Subtype of (string * (string list) 
		* Basic.gtype * Basic.term)
      (** 
	  Subtype definition: The type name, its arguments, the type it
	  subtypes and the defining predicate
      *)

(** {7 Grammars} *)
  
(** The parser grammars. *)
module Grammars :
sig

  (** {7 Utility types and functions} *)
		     
  type input = Pkit.input
  type 'a phrase = 'a Pkit.phrase
  type token_info =
      (Basic.ident
       * Parserkit.Info.fixity 
       * int) option
  

  val string_of_tok : Lexer.tok -> string
  val string_tokens : Lexer.tok list -> string

  (** {7 Token tables} 

      A token table stores information about tokens. A token in the
      table is the respresentation of a symbol which can occur in an
      input stream.
  *)
      
  type token_table
    (** Token tables *)
  val token_table_new: unit -> token_table
    (** Make a token table *)
  val token_table_reset: token_table -> unit
    (** Reset a token table *)
  val token_table_add: token_table -> Lexer.tok -> token_info -> unit
    (** Add a token to token table *)
  val token_table_find: token_table -> Lexer.tok-> token_info
    (** Lookup a token in a token table *)
  val token_table_remove: token_table -> Lexer.tok -> unit
    (** Remove a token from a token table *)

    (** {7 Parser information}  *)
      
  (**
    	Information used by the parsers. Includes a record of the
    	bound names found in a term and the names found in terms and types
  *)
  type infotyp = 
      { 
	(* Term information *)
	bound_names: (string* Basic.term) list ref;
        (**
	   Names found in a term and the term they are to be
	   replaced with
	*)
	token_info: Pkit.token -> token_info;
	(** Get the information for a token found in a term *)
		     
	(* Type information *)
      	typ_indx : int ref; (** Counter to generate type names *)
	typ_names: (string* Basic.gtype)list ref;
	(** Names found in a type and their replacements *)
        type_token_info: Pkit.token -> token_info
        (** Get the information for a token found in a type *)
      }
	
  val mk_inf :  token_table -> token_table -> infotyp
    (**
     [mk_inf tbl type_tbl]: Make parsing information from tables
     [tbl] and [type_tbl] of term and type token information.
    *)

  
  val lookup_name : string -> infotyp -> Basic.term
  (**
     [lookup_name n inf]: Look up [n] in [inf.bound_names].
     raise [Not_found] if not found.
  *)
		     
  val add_name : string -> Basic.term -> infotyp -> unit
    (**
       [add_name n trm inf]: Associate [trm] with name [n] in
       [inf.bound_names].  Used to associate variable name with a bound
       variable,

       For example, at the top of a binding term [!x. t],
       [add_name x (Bound b) inf]
       is called to associate [x] with [Bound b] when parsing [t].
    *)

  val drop_name : string -> infotyp -> unit
      (**
     [drop_name n inf]: Remove [n] from the list of bound names.  For
     example, after parsing the body [t] of a binding term [! x. t].
  *)

  val get_term : string -> infotyp -> Basic.term
    (**
       [get_term n inf]: Get the term associated with bound name [n].
       if there is no term associated with [n] then return a short
       identifier made from [n] (as [mk_free n (Gtype.mk_null())]).
    *)

  val clear_names : infotyp -> unit
  (**
     [clear_names inf]: Clear the bound names of [inf].
  *)

  val get_type_indx : infotyp -> int
  (**
     [mk_vartyp inf]: Get and increment the type index [inf.typ_indx].
  *)
    
  val mk_vartyp : infotyp -> Basic.gtype
  (**
     [mk_vartyp inf]: Make a new, uniquely named, type variable.
     Increments [inf.typ_indx].
  *)

  val lookup_type_name: string -> infotyp -> Basic.gtype
  (** 
      [lookup_type_name n inf]: Lookup type variable name [n].
      If not found, raise [Not_found].
  *)
    
  val add_type_name: string -> Basic.gtype -> infotyp -> Basic.gtype
    (**
     [add_type_name n ty inf]: Add [n] as the string representation of
     gtype [ty].
  *)
    
  val get_type: string -> infotyp -> Basic.gtype
    (**
     [get_type n inf]: Get the type variable represented by name [n].
     If not found, create a type variable [ty], with a unique name,
     associate [n] with [ty] in [inf] and return [ty].
    *)

  val clear_type_names: infotyp -> unit
  (** 
      [clear_type_names inf]: Clear the record of type variable names.
  *)


  (** {7 Token information utility functions} *)

  val mk_token_info : token_info -> Pkit.token_info
    (**
     [mk_token_info x]: Extract the precedence and fixity information
     from term token [x], if any. If not, return the default term fixity
     and precedence. For use with the {!Parser.Pkit.operators}
     constructor.
  *)

  val mk_type_token_info : token_table -> Lexer.tok -> Pkit.token_info
  (**
     [mk_type_token_info x] Extract the precedence and fixity
     information from type token [x], if any. If not, return the
     default type fixity and precedence.  Used with the Parserkit.operator
     parser.
  *)

  (** {5 Utility parsers} *)

  val message : string -> 'a phrase
  (** [message m]: Fail, raising [ParsingError m]  *)

  val error : ?msg:string -> 'a phrase
    (** [error ?msg]: Fail, using [msg] as the error message  *)

  val comma_list: 'a phrase -> 'a list phrase
    (** [comma_list ph]: Parse a comma seperated list of phrases [ph] *)
  
  val repeat_term : 'a phrase -> 'b phrase -> 'a list phrase
    (** 
	[repeat_term ph1 ph2]: Repeatedly parse phrase [ph1], terminated by 
	final phrase [ph2]
    *)
  
  (** {5 Identifier Parsers} *)

  val id_parser: (Lexer.tok -> token_info) -> Basic.ident phrase
    (** 
	[id_parser info inp]:  General identifier parser.
	Matches identifiers and symbols which translate to identifiers.
    *)
  
  val id_strict: (Lexer.tok -> token_info) -> Basic.ident phrase
  (**
     [id_strict info inp]: Strict identifier parser.
     Matches (possibly qualified) identifiers only, not symbols.
  *)   

  val named_id : 
    infotyp 
    -> (infotyp -> Basic.ident phrase)
    -> Basic.ident -> Basic.ident phrase
  (**
      [named_id info ph name inp]: Parse an identifier [name].
      Fail if token doesn't match the given name. Uses parser
      [ph inf] to parse the identifier.
  *)

  val short_id : (infotyp -> Basic.ident phrase) 
    -> infotyp -> string phrase
    (**
       [short_id ph inf toks]: Parse a short (unqualified)
       identifier. Uses parser [ph inf] to parse the identifier.
    *)

  val long_id : (infotyp -> Basic.ident phrase)
    -> infotyp -> Basic.ident phrase
      (**
       [short_id ph inf toks]: Parse a long (possibly qualified)
       identifier. Uses parser [ph inf] to parse the identifier.
    *)

  val mk_short_id : (infotyp -> Basic.ident phrase)
    -> infotyp -> string phrase
  (** 
      [mk_short_id ph inf]:
      Parse a possibly qualified identifer with [ph inf], 
      make it a short identifier.
  *)	  

  (** 
      {5 Type parsers} 
     
      Types are parsed according to the following rules:

      {[
      types ::= inner_types

      inner_types ::= operators(atomic_types)
     
      operators(atom) ::=
      op atom
      | atom op atom
      | atom op

      atomic_types ::=
      type_parsers 
      | error

      type_parsers ::= 
      primed_id
      | num_type 
      | bool_type
      | type_constructor = 
        [ '(' inner_types [ ',' inner_types ]* ')' ] long_id
      | bracketed_type = '(' inner_types ')'
      | User added parsers

      primed_id ::= PrimedID token (matched by lexer)
      num_type ::= "num"
      bool_type ::= "bool"
      ]}
      
  *)

  (** {7 Utility functions} *)

  val mk_type_binary_constr :
    infotyp -> Lexer.tok -> Basic.gtype -> Basic.gtype -> Basic.gtype
    (**
    	[mk_type_binary_constr inf tok l r]: Make a gtype from binary
    	type from constructor [tok], left argument [l] and right argument [r].
    *)

  val mk_type_unary_constr : 	
    infotyp -> Lexer.tok -> Basic.gtype -> Basic.gtype
    (**
    	[mk_type_unary_constr inf tok a]: Make a gtype from a unary
    	type from constructor [tok] and argument [a].
    *)
    
  (** {7 The parsers} *)
    
  val type_id : infotyp -> Basic.ident phrase
  (**  [type_id]: Parse a type identifier. *)

  val primed_id : infotyp -> Basic.gtype phrase
  (** [primed_id inf]: Read a type variable name. *)

  val bool_type : infotyp -> Basic.gtype phrase
  (** [bool_type info]: Parse type "bool" *)
  val num_type : infotyp -> Basic.gtype phrase
  (** [num_type info]: Parse type "num" *)
    
  val inner_types : infotyp -> Basic.gtype Pkit.phrase
    (** Parse infix/prefix/suffix type operators *)
    
  val atomic_types : infotyp -> Basic.gtype phrase
    (** The atomic types (num, bool, etc) *)

  val type_parsers_list : 
    (string, infotyp -> (Basic.gtype phrase)) Lib.named_list ref
    (** 
	A record of the type parsers used by {!Parser.Grammars.inner_types}.
	Can be extended with user-defined parsers.
    *)

  val type_parsers : infotyp -> Basic.gtype phrase
    (** The parser made from {!Parser.Grammars.inner_types}) *)
    
  val types : infotyp -> Basic.gtype phrase
    (** The main type parser. *)

(** {7 Support for adding parsers} 

    Functions to add to and remove from the list of type parsers
    {!Parser.Grammars.type_parsers_list} used by
    {!Parser.Grammars.types}.

    Allows the use of user-defined type parsers.
*)

  val add_type_parser :  
    (string)Lib.position -> string
    -> (infotyp -> Basic.gtype phrase)
    -> unit
  (** 
      [add_type_parser pos n ph]:
      Add type parser [ph] at position [pos] with name [n].
  *)

  val remove_type_parser :  string -> unit
    (*
      [remove_type_parser n]: Remove the type parser named [n], raise
      [Not_found] if not present.
     *)



  (** {5 Term parsers} 

      Terms are parsed according to the following grammar:

      {[
      form ::= formula [ formula* ]

      formula ::= operators(typed_primary)

      operators(atom) ::=
      op atom
      | atom op atom
      | atom op

      typed_primary ::= primary optional_type

      primary ::= 
      term_parsers 
      | error

      term_parsers ::=
      id
      | number
      | boolean
      | bracketed_term = '(' form ')'
      | forall = ALL (short_id optional_type)+ ':' form
      | exists = EX (short_id optional_type)+ ':' form
      | lambda = LAM (short_id optional_type)+ ':' form
      | User added parsers
      ]}
  
      Note that the parser for terms is called 'form'
      ({!Parser.Grammars.form}).
  *)
  
  (** {7 Utility functions} *)
     
  val mk_conn :
    infotyp -> Lexer.tok -> Basic.term -> Basic.term -> Basic.term
    (**
     	[mk_conn inf tok l r]: Make a binary operator term from token
     	[tok] with left argument [l] and right argument [r].
    *)
     
  val mk_prefix : infotyp -> Lexer.tok -> Basic.term -> Basic.term
    (**
     	[mk_prefix inf tok a]: Make a binary operator term from token
     	[tok] with argument [a].
     *)
     
  val mk_comb : Basic.term -> Basic.term list -> Basic.term
    (**
       [mkcomb f args]: Make a combinator.  Construct the term [((((f
       a1) a2) .. ) an)] (where args = [a1; a2; ..; an]).
    *)

  val qnt_setup_bound_names: 
    infotyp -> Basic.quant_ty -> (string * Basic.gtype) list 
    -> (string * Basic.term) list
    (**
       [qnt_setup_bound_names inf qnt xs]: Make bound variables from
       the name-type pairs in [xs], add them to [inf.bound_names].
       [qnt] is the quantifier type (All, Ex or Lambda) 
    *)

  val qnt_term_remove_names:
    infotyp -> (string * Basic.term) list 
    -> Basic.term -> Basic.term
    (**
       [qnt_term_remove_names inf xs body]: Use bound names in [xs] to
       form a quantified term, with body as the initial term.
       
       Simplified example:  [[!x, ?y, !z] t] produces [(!x: (?y: (!z: t)))]
       
       Removes each name in [xs] from [inf.bound_names] as it is used.
    *)

    (**
     [make_term_remove_names info wrapper vs body]: Remove the
     variables in [vs] from [info].  Return the term constructed by
     quantifying [body] with the variables [vs], applying [wrapper] to
     each constructed term.
    *)
  val make_term_remove_names:
    infotyp 
    -> (Basic.term -> Basic.term)
    -> (string * Basic.term) list
    -> Basic.term -> Basic.term

  (** {7 The parsers} *)

  val number : Num.num phrase
  (** Read a number constant. *)
  val boolean : bool phrase 
  (** Read a boolean constant. *)
  
  val optional_type : infotyp -> Basic.gtype option phrase
  (** Parse an optional type. 
      [ optional_type ::= [ ':' types ] ]
  *)

  val id : infotyp -> Basic.ident phrase
  (** [id]: Parse identifiers which occur in terms *)

  val id_type_opt :
    (infotyp -> 'a Pkit.phrase) ->
    infotyp -> ('a * Basic.gtype) phrase
  (** Parse an optionally typed identifier. 
      [ id_type_opt ::= id optional_type ]
  *)

  val term_identifier: infotyp -> Basic.term phrase
    (**
     [term_identifer inf]: Parse an identifier that might appear in a
     quantified term. Use [id_type_opt inf] to get an identifier.
     Look-up identifier in [inf], to check if it is a bound variable.
     If not, it is a free variable.
  *)

  val form : infotyp -> Basic.term phrase
    (** The main term parser *)
  val formula : infotyp -> Basic.term phrase
    (** Parse infix/prefix/suffix operators *)
  val typed_primary : infotyp -> Basic.term phrase
    (** Parse a possibly typed atomic term *)
  
  val term_parsers_list :
    (string, infotyp -> Basic.term phrase) Lib.named_list ref
    (**
    	The list of atomic term parsers. Can be extended with
    	user-defined parsers.
    *)
  val term_parsers : infotyp ->Basic.term phrase
    (** The parser built from the list of atomic term parsers *)
  val primary : infotyp -> Basic.term phrase
    (** Parser for the atomic terms *)
  
  (** {7 Support functions} *)
    
  val add_parser: 
    string Lib.position
    -> string -> (infotyp -> Basic.term phrase) -> unit
    (**
       [add_parser pos n ph]: Add term parser [ph] with name [n] in
       position [pos] to {!Parser.Grammars.term_parsers_list}.
    *)
    
  val remove_parser: string -> unit
  (**
     [remove_parser n]:  Remove the term parser named [n] from 
     {!Parser.Grammars.term_parsers_list}.
  *)

  val parse_as_binder:
    Basic.ident -> string -> infotyp -> Basic.term phrase
    (** 
	[parse_as_binder f sym]:
	Construct a grammar to parse function applications
	of the form [f (%x: P)] as [sym x: P].

	Symbol [sym] should be added to the lexer seperately.
	(e.g. using [Parser.add_symbol sym (Lexer.Sym(Lexer.OTHER sym))]).
    *)   

  (** {5 Definitions} *)

  (** {7 Type definitions} 

    A type definition is either a simple type definition (declaring a
    new type); an aliasing definition or a subtyping definition.

    {[
      typedef ::= 
      simple_typedef
      | subtypedef

      simple_typedef ::=
      [ '(' primed_id  (',' primed_id)* ')' ] type_id [ '=' types ]

      subtypedef ::=
      [ '(' primed_id  (',' primed_id)* ')' ] type_id 
      '=' types ':' form
    ]}
*)
  
  val simple_typedef :
    infotyp ->
    (string * string list option * Basic.gtype option)  phrase
      (**
      	  Parse a type declaration or alias. If the rhs of the
      	  definition is given, it is an aliasing definition.
      *)
  val subtypedef: 
    infotyp -> 
    (string * string list option * Basic.gtype * Basic.term)  phrase
      (**
      	 Parse a subtyping definition. The term is the defining
      	 predicate for the type.
      *)
      
  val typedef : infotyp -> (typedef_data)  phrase
    (** Parse a type definition. *)

  (** {7 Term definitions} 

      Term definitions.

      {[ 
      defn ::= short_id short_id* '=' form
      ]}
      
      In the form [ f a1 ... an = rhs ], [f] is the identifier being
      defined, the [a1 ... an] are the parameters and the form [rhs]
      is the definition of [f]. Occurences of the parameters [a1
      .. an] in [rhs] are treated as short identifiers.
  *)
  
  val defn :
    infotyp ->
    ((string * (string * Basic.gtype) list) * Basic.term) phrase
      (** Parse a term definition. *)

end

(** {5 Parser Data} *)

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

*)
val add_type_parser :  
  (string)Lib.position -> string
  -> (Grammars.infotyp -> Basic.gtype phrase)
  -> unit
val remove_type_parser :  string -> unit
(**
   [remove_type_parser s]: 
   remove type parser named s, raise Not_found if not present 
*)

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

  val resolve_term:
    Scope.t
    -> (string -> Basic.gtype -> (Basic.ident * Basic.gtype))
    -> Basic.term
    -> (Basic.term * Gtypes.substitution)
    (** 
	[resolve_term scp env t]: Resolve the symbols in term [t].

	For each free variable [Free(s, ty)] in [t], lookup [s] in [env] to
	get long identifier [id].  If not found, use [Free(s, ty)].  If
	found, replace [Free(s, ty)] with the identifier [Id(id, ty)].

	[env] should return an identifier-type pair where type matches (in
	some sense) [ty].

	[env] must raise Not_found if [s] is not found.
    *)

  val make_lookup: 
    Scope.t
    -> (string -> (Basic.ident * Basic.gtype) list) 
    -> (string -> Basic.gtype -> (Basic.ident * Basic.gtype)) 
    (**
       [make_lookup scp db]:
       make an environment suitable for {!Parser.Resolver.resolve_term} from [db].

       [db] must raise Not_found when items are not found.

       [make_lookup db s ty]: returns the identifier-type pair associated
       by [db] with [s] for which [ty] is unifies with type in scope [scp].

       [make_lookup db s ty] raise Not_found if [s] is not found in [db].
    *)
    
    
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
