(*----
 Name: parserkit.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** 
   Toolkit for top-down parsers 

   Mostly based on the parser combinators described by Paulson ("ML
   for the working programmer", 1991, Cambridge University Press.)
*)

(** {7 Parser Input} *)

(** [Input]: Input streams for parsers *)
module Input:
  sig
(** Streams which can deal with backtracking *)

    exception Empty
    type 'a t

    val make : (unit -> 'a) -> 'a t
(**
   [make f]: make an input from function [f].
   [f] must raise [Empty] when it is empty
   If [f] is initially empty, then the constructed input
   will always be empty.
*)

    val is_empty : 'a t -> bool
(** 
   [is_empty inp]: true iff [inp] is empty 
*)

    val look : 'a t -> 'a
(** 
   [look inp]: get first element in input inp but don't remove it from
   input
*)

    val accept : 'a t -> 'a t
(**
   [accept inp]: Get a new input, formed by dropping the first element
   of inp.  This is non-destructive, the first element will still be
   available in inp
*)
  end

(** [Info]: Precedence and associativity information for tokens. *)
module Info :
  sig
    type associativity = 
	Nonassoc | Leftassoc | Rightassoc

    type fixity =
	Nonfix | Prefix | Suffix | Infix of associativity
	  
    val nonfix : fixity
    val infix : associativity -> fixity
    val prefix : fixity
    val suffix : fixity

    val left_assoc : associativity
    val right_assoc : associativity
    val non_assoc : associativity

    val is_nonfix: fixity -> bool
    val is_infix: fixity -> bool
    val is_prefix: fixity -> bool
    val is_suffix: fixity -> bool

    val is_left_assoc: fixity -> bool
    val is_right_assoc: fixity -> bool
    val is_non_assoc: fixity -> bool
    val assoc_of : fixity -> associativity
  end


(** [Tokens]: Token values and matchings. *)
module type TOKENS =
  sig 

    type tokens 

    val matches : tokens -> tokens -> bool 
(**
   [matches t1 t2]: [true] iff token [t1] should be considered a match
   for token [t2].
*)

    val string_of_token : tokens -> string
(**
   [string_of_token]: Used for error reporting only. If necessary use
   [(fun x _ -> "")].
*)

  end

(** {5 Parsers} *)

module type T =
    sig
(** The available parser constructors. *)

      exception ParsingError of string
      exception No_match
      type token 
      and input = token Input.t
      and 'a phrase = input -> 'a * input
      val empty : 'a list phrase
      val next_token : token phrase

(** {7 Parser Constructors} *)

      val error: ?msg:string -> (token -> string) -> 'a phrase
(** 
   [error msg strtok]: Error reporting.
   Read a token, fail, raising ParsingError.
   Message for ParsingError.
*)

      val get : (token -> bool) -> (token -> 'a) -> 'a phrase
(**
   [get pred f]: Match a token satisfying [pred] and apply [f]
*)

      val ( !$ ) : token -> token phrase
(**
   [!$ tok]: Match token [tok].
 *)

      val ( !! ) : 'a phrase -> 'a phrase
(**
   [!!ph]: Alternative.  Apply [ph], fail if [ph] fails.
*)

      val ( -- ) : 'a phrase -> 'b phrase -> ('a * 'b) phrase
(**
   [ph1 -- ph2]: Sequence. Apply [ph1] followed by [ph2].
*)

      val ( --% ) : 'a phrase -> 'b phrase -> 'b phrase
(**
   [ph1 $-- ph2 ]: Parse and discard [ph1] then parse [ph2].
*)

      val ( >> ) : 'a phrase -> ('a -> 'b) -> 'b phrase
(**
   [ph >> f]: Map. Parse [ph], apply result to [f].
*)


      val optional : 'a phrase -> 'a option phrase
(**
   [optional ph]: Parse [ph], return [None] on failure.
*)

      val repeat : 'a phrase -> 'a list phrase
(**
   [repeat ph]: Apply [ph] until it fails.
*)

      val multiple : 'a phrase -> 'a list phrase
(**
   [multiple ph]: Apply [ph] at least one, then until it fails.
*)

      val list0 : 'a phrase -> 'b phrase -> 'a list phrase
(**
   [list0 ph sep]: List of zero or more [ph] seperated by [sep].
*)

      val list1 : 'a phrase -> 'b phrase -> 'a list phrase
(**
   [list1 ph sep]: List of one or more [ph] seperated by [sep].
*)


      val ( // ) : 'a phrase -> 'a phrase -> 'a phrase
(**
   [ph1 // ph2]: [ph1] or [ph2]
*)

      val alt : 'a phrase list -> 'a phrase
(*
   [alt phs]: Try each of the parsers of [phs] in sequence, starting
   with the first, return the result of the first to suceed.
*)

      val named_alt : 
	  (string, 'a -> ('b)phrase) Lib.named_list 
	   -> ('a -> ('b)phrase)
(**
   [named_alt inf phs]: Try each of the named parsers of [phs] in
   sequence, starting with the first and applying each to [inf],
   return the result of the first to suceed.
*)

      val seq : ('a phrase) list -> ('a list)phrase
(**
   [seq phs]: Apply each of the parsers of [phs] in sequence, starting
   with the first, fail if any fails. Return the result as a list.
*)

      val named_seq : 
	  (string, 'a -> ('b)phrase) Lib.named_list 
	   -> ('a -> ('b list)phrase)
(**
   [named_seq phs]: Apply each of the named parsers of [phs] in
   sequence, starting with the first and applying each to [inf], fail
   if any fails. Return the result of as a list.
*)

(**
   [token_info]: Precedence and fixity information about tokens.  Used
   by operators parser.
*)
      type token_info = 
	  { 
	    fixity: Info.fixity;
	    prec: int
	  }

      val operators :
        'a phrase * (token -> token_info) * (token -> 'a -> 'a -> 'a) *
        (token -> 'a -> 'a) -> 'a phrase
  (**
      [operators(ph, info, binop, unaryopy)]: Operator precedence parser.

      [ph]: parser for basic (atomic) terms, such as numbers, bools etc
      [info]: return fixity information about a token
      [binop]: function to combine two arguments and a token
      [unaryop]: function to combine one argument and a token
  *)


      val parse : 'a phrase -> token -> input -> 'a
  (** 
     [parse ph eof inp]: Parse input [inp] using [ph] until the end of
     file token [eof].
   *)

    end

module Make:
    functor (A: TOKENS) -> (T with type token = A.tokens)
(** Generic parser constructors *)
