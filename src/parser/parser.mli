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
   described as a {!Parser.phrase}, and a phrase can also be
   built up from one or more phrases. A parser is built from a phrase
   using function {!Parserkit.T.parse}.

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
module Pkit : (Parserkit.T with type token =Lexer.tok) 

(** Useful parser constructors  *)
module Utility: 
    sig
      open Pkit

      val (?$): Lexer.tok -> Basic.term phrase
	  (**
	     [?$ sym]: Utility function for building term parsers using
	     {!Parserkit.T.seq}.  Parse symbol [sym], return term
	     [Term.mk_short_var (Lexer.string_of_token)].
	   *)
	  
      val (?%): Lexer.tok -> Basic.gtype phrase
          (**
	     [?% sym]: Utility function for building type parsers using
	     {!Parserkit.T.seq}. Parse symbol [sym], return term [Gtypes.mk_var
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

type input = Pkit.input
(** The input stream *)
type 'a parse = input -> 'a
(** A parser *)
type 'a phrase = 'a Pkit.phrase
(** A grammar *)

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
(** Non-fix *)
val infix : associativity -> fixity
(** Infix of given associativity *)
val prefix : fixity
(** Prefix *)
val suffix : fixity
(** Suffix *)

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
      
      type token_info =
	  (Ident.t
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
	     and precedence. For use with the {!Parserkit.T.operators}
	     constructor.
	   *)

      val mk_type_token_info : token_table -> Lexer.tok -> Pkit.token_info
	(**
	     [mk_type_token_info x] Extract the precedence and fixity
	     information from type token [x], if any. If not, return
	     the default type fixity and precedence.  Used with the
	     Parserkit.operator parser.
	   *)

	   	  (** {5 Utility parsers} *)

      val message : string -> 'a phrase
	  (** [message m]: Fail, raising [ParsingError m]  *)

      val error : string -> 'a phrase
	  (** [error msg]: Fail, using [msg] as the error message  *)

      val term_error : string -> 'a phrase
	  (** [term_error msg]: Fail, using [msg] as the error message  *)

      val type_error : string -> 'a phrase
	  (** [type_error msg]: Fail, using [msg] as the error message  *)

      val comma_list: 'a phrase -> 'a list phrase
	  (** [comma_list ph]: Parse a comma seperated list of phrases [ph] *)
	  
      val repeat_term : 'a phrase -> 'b phrase -> 'a list phrase
	  (** 
	     [repeat_term ph1 ph2]: Repeatedly parse phrase [ph1], terminated by 
	     final phrase [ph2]
	   *)
	  
	  (** {5 Identifier Parsers} *)

      val id_parser: (Lexer.tok -> token_info) -> Ident.t phrase
	  (** 
	     [id_parser info inp]:  General identifier parser.
	     Matches identifiers and symbols which translate to identifiers.
	   *)
	  
      val id_strict: (Lexer.tok -> token_info) -> Ident.t phrase
	  (**
	     [id_strict info inp]: Strict identifier parser.
	     Matches (possibly qualified) identifiers only, not symbols.
	   *)   

      val named_id : 
	  infotyp 
	-> (infotyp -> Ident.t phrase)
	  -> Ident.t -> Ident.t phrase
	      (**
		 [named_id info ph name inp]: Parse an identifier [name].
		 Fail if token doesn't match the given name. Uses parser
		 [ph inf] to parse the identifier.
	       *)

      val short_id : (infotyp -> Ident.t phrase) 
	-> infotyp -> string phrase
	    (**
	       [short_id ph inf toks]: Parse a short (unqualified)
	       identifier. Uses parser [ph inf] to parse the identifier.
	     *)

      val long_id : (infotyp -> Ident.t phrase)
	-> infotyp -> Ident.t phrase
	    (**
	       [short_id ph inf toks]: Parse a long (possibly qualified)
	       identifier. Uses parser [ph inf] to parse the identifier.
	     *)

      val mk_short_id : (infotyp -> Ident.t phrase)
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
	      
      val type_id : infotyp -> Ident.t phrase
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
	     A record of the type parsers used by
	     {!Parser.Grammars.inner_types}.  Can be extended with
	     user-defined parsers.
	   *)

      val type_parsers : infotyp -> Basic.gtype phrase
	(** The parser made from {!Parser.Grammars.inner_types}) *)
	  
      val types : infotyp -> Basic.gtype phrase
	  (** The main type parser. *)

      val core_type_parsers : 
	   (string, infotyp -> (Basic.gtype phrase)) Lib.named_list
	   (** The built-in type parsers *)
	   
      val init_type_parsers: unit -> unit
	(** Initialise the type parsers *)
	   
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
	  infotyp -> Basic.quant -> (string * Basic.gtype) list 
	    -> (string * Basic.term) list
		 (**
		   [qnt_setup_bound_names inf qnt xs]: Make bound
		   variables from the name-type pairs in [xs], add
		   them to [inf.bound_names].  [qnt] is the quantifier
		   type (All, Ex or Lambda)
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

      val id : infotyp -> Ident.t phrase
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
	  
      val core_term_parser_list : 
	(string, infotyp -> (Basic.term phrase)) Lib.named_list
	(** The built-in term parsers *)
	   
      val init_term_parsers: unit -> unit
	(** Initialise the term parsers *)

	   
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
	  Ident.t -> string -> infotyp -> Basic.term phrase
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

	     Each of the arguments [ai] is converted to a bound
	     variable [bi] (of kind [Basic.All]) and replaced in
	     [rhs] with [bi]. 

	     The parser returns the name [f], [b1 .. bi] and the body
	     [rhs].
	   *)

      val defn :
	  infotyp ->
	    (((string * Basic.gtype) * Basic.term list) * Basic.term) phrase
	      (** Parse a term definition. *)

(*
      val defn :
	  infotyp ->
	    ((string * (string * Basic.gtype) list) * Basic.term) phrase
	      (** Parse a term definition. *)
*)

    end

(** {7 Overloading} 

   Operator overloading works by maintaining a list of identifiers
   which have the same symbol together with their types. When the
   symbol occurs in a term, as a short name, a type is inferred for
   the name and the list of identifiers is searched for a matching
   type. The first matching identifier is used. If there is no match,
   the first identifier in the list is chosen. 
   
   The standard table for operator overloading is
   {!Parser.overload_table}, which maintains a list of identifiers and
   types for each overloaded symbols. Identifiers are normally added
   to the front of the list but a position can be passed, to prefer
   one identifier over others. (The search begins from the front of
   the list.)

   The toplevel for operator overloading is
   {!Parser.Resolver.resolve_term} which takes a function which
   carries out the search for an identifier with a matching
   type. Function {!Parser.Resolver.make_lookup} constructs a suitable
   search function, from a symbol look-up table.
*)

(** Support for operator overloading *)
module Resolver :
    sig

      val resolve_term:
	  Scope.t
	-> (string -> Basic.gtype -> (Ident.t * Basic.gtype))
	  -> Basic.term
	    -> (Basic.term * Gtypes.substitution)
		(** 
		   [resolve_term scp env t]: Resolve the symbols in
		   term [t].

		   For each free variable [Free(s, ty)] in [t], lookup
		   [s] in [env] to get long identifier [id].  If not
		   found, use [Free(s, ty)].  If found, replace
		   [Free(s, ty)] with the identifier [Id(id, ty)].

		   [env] should return an identifier-type pair where
		   type matches (in some sense) [ty].

		   [env] must raise Not_found if [s] is not found.
		 *)

      val make_lookup: 
	  Scope.t
	-> (string -> (Ident.t * Basic.gtype) list) 
	  -> (string -> Basic.gtype -> (Ident.t * Basic.gtype)) 
	      (**
		 [make_lookup scp db]: Make an environment suitable for
		 {!Parser.Resolver.resolve_term} from table [db].

		 [db] must raise [Not_found] when items are not found.

		 [make_lookup db s ty]: returns the identifier-type
		 pair associated by [db] with [s] for which [ty] is
		 unifies with type in scope [scp].

		 [make_lookup db s ty] raise Not_found if [s] is not
		 found in [db].
	       *)
	      
	      
(** {7 Debugging} *)

      val default: 
	  string -> Basic.gtype -> (Ident.t * Basic.gtype) list
	      -> (Ident.t * Basic.gtype) option

      type resolve_memo =
	  { 
	    types : (Ident.t, Basic.gtype)Hashtbl.t;
	    idents: (string, Ident.t)Hashtbl.t;
	    symbols : (string, Ident.t)Hashtbl.t;
	    type_names: (string, Ident.thy_id)Hashtbl.t
	  }

      type resolve_arg =
	  {
	   scp: Scope.t;
	   inf : int ref;
	   memo: resolve_memo;
	   qnts: Term.substitution;
	   lookup: (string -> Basic.gtype -> (Ident.t * Basic.gtype))
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
	-> string
	  -> Basic.gtype -> (Ident.t * Basic.gtype) list 
	    -> (Ident.t * Basic.gtype)


(*
   val ovl : 
   Scope.t
   -> (string -> Basic.gtype -> (Ident.t * Basic.gtype))
 *)
    end


(** {5 Parser Data} 

   The standard lexers and parsers use tables of symbols and
   tokens. The symbol table is used by the lexer and maps strings to
   tokens. Two token tables hold fixity and precedence information
   about tokens which can appearing in term and type parsers. 

   Symbols should be added to the symbol table if they are to
   recognised by the lexer. This includes any symbol used in a
   user-defined parser or as a symbolic representation of an
   identifier. A token should be added to the token table, together
   with its fixity information, if it represents an identifier. Note
   that there are

   For example, if the symbol "@" is to be used in a user-defined
   parser then it should added to the symbol table, possibly mapping to
   the token [Sym (Other "@")].

   For another example, assume the identifier "Set.in" is to be
   represented as the infix operator "in". First the symbol "in" is
   added to the symbol table, mapping to the token [Sym (Other
   "in")]. Then the token [Sym (Other "in")] is added to the token
   table, with appropriate fixity and precedence.
 *)

(** {7 Symbols} *)

val syms_list : (string * Lexer.tok) list
(** The list of builtin symbols *)

(* tables of symbols and token (parsing) information *)
val symtable_size : int ref
(** The initial size of the standard symbol table. *)

val symtable : unit -> Lexer.symtable
(** Get the standard symbol table *)

val add_symbol : string -> Lexer.tok -> unit
(**
   [add_symbol sym tok]: add [sym] as the symbol representing token [tok].
   Fails silently if [sym] already exists
 *)
val find_symbol : string -> Lexer.tok
(**
   [find_symbol sym]: Lookup [sym] in the symbol table.
   Raise [Not_found] on failure.
 *)

val remove_symbol : string -> unit
(**
   [remove_symbol sym]: Remove [sym] from the standard symbol table.
   Fails silently if [sym] already exists
 *)

val init_symtable : unit -> unit
(**
   Initialise the symbol table and add the standard symbols.
 *)

(** {7 Tokens} *)

val token_table: Grammars.token_table
(** Table of tokens which can appear in a term parser *)

val add_token_info : Lexer.tok -> Grammars.token_info -> unit
(**
   [add_token_info tok info]: add parsing information [info] for term
   token [tok] to the standard token table, fail if token information
   exists.
*)

val get_token_info: Lexer.tok -> Grammars.token_info
(**
   [get_token_info tok]: Get parsing information for term token [tok]
   from the standard table, fail if token information exists.
*)

val remove_token_info: Lexer.tok -> unit
(**
   [remove_token_info tok]: Remove parsing information for term token
   [tok] from the standard table.
*)

val type_token_table: Grammars.token_table
(** Table of tokens which can appear in a type parser *)

val add_type_token_info : Lexer.tok -> Grammars.token_info -> unit
(**
   [add_type_token_info tok info]: Add parsing information [info] for
   type token [tok] to the standard type token table, fail if token
   information exists
 *)

val get_type_token_info: Lexer.tok -> Grammars.token_info
(**
   [get_token_info tok]: Get parsing information for type token
   [tok] from the standard table.
*)

val remove_type_token_info: Lexer.tok -> unit
(**
   [remove_type_token_info tok]: Remove parsing information for type token
   [tok] from the standard table.
*)

val mk_info : unit -> Grammars.infotyp
(** 
   Make a parser information record ([infotyp)] from the standard
   term and type token tables.
*)

(** {7 Toplevel symbol and token functions} *)

val add_token: Ident.t -> string -> fixity -> int -> unit
(** 
   [add_token id sym fix prec]: Add symbol [sym] as representation for
   term identifier [id], with fixity [fix] and precedence [prec]. 
   Updates term symbol and token tables. 
*)

val remove_token : string -> unit
(**
   [remove_token sym]: Remove [sym] and associated token information
   from the term symbol and token tables.
*)

val add_type_token: Ident.t -> string -> fixity -> int -> unit
(** 
   [add_type_token id sym fix prec]: Add symbol [sym] as representation for
   type identifier [id], with fixity [fix] and precedence [prec]. 
   Updates type symbol and token tables. 
*)

val remove_type_token : string -> unit
(**
   [remove_type_token sym]: Remove [sym] and associated token information
   from the type symbol and token tables.
*)

(** {7 Overloading} *)

val overload_table_size : int ref
(** The initial size of the overloading table. *)

val overload_table: 
    (string,  (Ident.t * Basic.gtype) list) Hashtbl.t ref
(** The table of overloaded symbols and possible identifiers. *)

val init_overload: unit -> unit
(** Initialise the overloading table. *)

val add_overload:
    string -> Theory.sym_pos -> (Ident.t * Basic.gtype) -> unit
(** 
   [add_overload sym pos (id, ty)]: Overload identifier [id], with
   type [ty] on symbol [sym]. Put [id] in position [pos]. 
*)
val get_overload_list: 
    string -> (Ident.t * Basic.gtype) list
(** 
   [get_overload_list sym]: Get the list of identifiers overloaded on
   symbol [sym].
*)
val remove_overload:
    string -> Ident.t -> unit
(** 
   [remove_overload sym id]: Remove [id] from the list of identifiers
   overloading symbol [sym].
*)

val print_overloads: Printer.ppinfo -> unit
(** Print the overloads table. *)

(** {5 Initialising functions} *)

val init_parsers : unit -> unit
(** Initialise the type and term parsers *)

val init : unit -> unit
(** 
   Initialise the parser tables (symbols, tokens and overloading).
   This must be called before any of the parsers are used.
*)


(** {5 Toplevel Parser functions} 

   Parsers to read a phrase followed by an end of file/string.
*)

val parse : 'a phrase -> 'a parse
(** Make a parser from a phrase *)

val identifier_parser : input -> Ident.t
(** Read a possibly long identifier *)

val type_parser : input -> Basic.gtype
(** Read a type. *)

val typedef_parser : input -> typedef_data
(** Read a type definition *)

val term_parser : input -> Basic.term
(** Read a term *)

(*
val defn_parser :
    input -> (string * (string * Basic.gtype) list) * Basic.term
*)
val defn_parser :
    input -> ((string * Basic.gtype) * Basic.term list) * Basic.term

(** Read a term definition *)

(** {7 User defined parsers} *)

val term_parser_list : 
    unit -> (string, Grammars.infotyp -> Basic.term phrase) Lib.named_list
(** 
   The list of user defined term parsers. Parsers added to this list are
   used by the term parser {!Parser.Grammars.form}.
 *)

val add_term_parser :  (string) Lib.position -> string
  -> (Grammars.infotyp -> Basic.term phrase)
    -> unit
(**
   [add_term_parser pos n ph]: Add the term parser [ph] at position
   [pos] with name [n] to {!Parser.term_parser_list}.
*)

val remove_term_parser :  string -> unit
(**
   [remove_term_parser s]: Remove the term parser named [s] from
   {!Parser.term_parser_list}. Raise [Not_found] if not present.
 *)

val type_parser_list : 
    unit -> (string, Grammars.infotyp -> Basic.gtype phrase) Lib.named_list
(** 
   The list of user defined type parsers. Parsers added to this list are
   used by the term parser {!Parser.Grammars.types}.
 *)

val add_type_parser :  
    (string)Lib.position -> string
      -> (Grammars.infotyp -> Basic.gtype phrase)
	-> unit
(**
   [add_type_parser pos n ph]: Add the type parser [ph] at position
   [pos] with name [n] to {!Parser.type_parser_list}.
*)

val remove_type_parser :  string -> unit
(**
   [remove_type_parser s]: Remove the type parser named [s] from
   {!Parser.type_parser_list}. Raise [Not_found] if not present.
 *)


(** {7 Readers} 

   Read and parse a string
*)

val read: 'a parse -> string -> 'a
(** [read ph str]: Parse string [str] with parser [ph]. *)

val read_term : string -> Basic.term
(** [read_term str]: Parse string [str] using the standard term parser. *)

val read_type : string -> Basic.gtype
(** [read_type str]: Parse string [str] using the standard term parser. *)

(** {7 Debugging} *)

val test_lex : string -> Lexer.tok Parserkit.Input.t
val test : string -> Basic.term




