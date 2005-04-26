(*----
 Name: parserkit.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* toolkit for top-down parsers *)


(* Parser Input: 
   a type of stream which are suitable for backtracking 
*)

module Input:
  sig

    exception Empty
    type 'a t

(*
   make f: make an input from function f.
   f must raise Empty when it is empty
   If f is initially empty, then the constructed input
   will always be empty.
*)
    val make : (unit -> 'a) -> 'a t

(* 
   is_empty inp: true iff inp is empty 
*)
    val is_empty : 'a t -> bool

(* look inp: 
   get first element in input inp but don't remove it from input 
*)
    val look : 'a t -> 'a

(* 
   accept inp:
   Get a new input, formed by dropping the first element of inp.
   This is non-destructive, the first element will still be
   available in inp
*)
    val accept : 'a t -> 'a t
  end

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


module type TOKENS =
  sig 
    type tokens 
(*
   [matches t1 t2]
   [true] iff token [t1] should be considered a match for token [t2].
*)
    val matches : tokens -> tokens -> bool 

(*
   [string_of_token]
   Used for error reporting only.
   If necessary use [(fun x _ -> "")].
*)
    val string_of_token : tokens -> string

  end

module type GRAMMARS =
  functor (ParseTokens : TOKENS) ->
    sig
      exception ParsingError of string
      exception No_match
      type token = ParseTokens.tokens
      and input = token Input.t
      and 'a phrase = input -> 'a * input
      val empty : 'a list phrase
      val next_token : token phrase

(* 
   [error msg strtok]
   Error reporting.
   Read a token, fail, raising ParsingError.
   Message for ParsingError.
*)
      val error: ?msg:string -> (token -> string) -> 'a phrase

(* 
   [get pred f]
   match a token satisfying [pred] and apply [f]
*)
      val get : (token -> bool) -> (token -> 'a) -> 'a phrase

(*
   [!$ tok]
   Match token [tok].
 *)
      val ( !$ ) : token -> token phrase
(*
   !!ph
   Apply [ph], fail if [ph] fails.
*)
      val ( !! ) : 'a phrase -> 'a phrase
(*
   [ph1 -- ph2]
   Apply [ph1] followed by [ph2].
*)
      val ( -- ) : 'a phrase -> 'b phrase -> ('a * 'b) phrase

(*
   [ph1 $-- ph2 ]
   Parse and discard [ph1] then [ph2].
*)
      val ( --% ) : 'a phrase -> 'b phrase -> 'b phrase

(*
   [ph >> f]
   Parse [ph], apply result to [f].
*)
      val ( >> ) : 'a phrase -> ('a -> 'b) -> 'b phrase


(* 
   [optional ph]
   parse [ph], return [None] if no match.
*)
      val optional : 'a phrase -> 'a option phrase

(*
   [repeat ph]
   Apply [ph] until it fails.
*)
      val repeat : 'a phrase -> 'a list phrase

(*
   [multiple ph]
   Apply [ph] at least one, then until it fails.
*)
      val multiple : 'a phrase -> 'a list phrase

(*
   [list0 ph sep]
   List of zero or more [ph] seperated by [sep].
*)
      val list0 : 'a phrase -> 'b phrase -> 'a list phrase

(*
   [list1 ph sep]
   List of one or more [ph] seperated by [sep].
*)
      val list1 : 'a phrase -> 'b phrase -> 'a list phrase

(* parsing alternatives *)
(*
   [ph1 || ph2]
   [ph1] or [ph2]
*)
      val ( || ) : 'a phrase -> 'a phrase -> 'a phrase
(*
   [alt phs]
   Try each of the parsers of [phs] in sequence, starting with the first,
   return the result of the first to suceed.   
*)
      val alt : 'a phrase list -> 'a phrase
(*
   [named_alt inf phs]
   Try each of the named parsers of [phs] in sequence, 
   starting with the first and applying each to [inf],
   return the result of the first to suceed.
*)
      val named_alt : 
	  ('a -> ('b)phrase) Lib.named_list 
	   -> ('a -> ('b)phrase)

(*
   [seq phs]
   Apply each of the parsers of [phs] in sequence, 
   starting with the first,
   fail if any fails.
   return the result of as a list.
*)
      val seq : ('a phrase) list -> ('a list)phrase

(*
   [named_seq phs]
   Apply each of the named parsers of [phs] in sequence, 
   starting with the first and applying each to [inf],
   fail if any fails.
   return the result of as a list.
*)
      val named_seq : 
	  ('a -> ('b)phrase) Lib.named_list 
	   -> ('a -> ('b list)phrase)

(*
   token_info
   Precedence and fixity information about tokens.
   Used by operators parser.
*)
      type token_info = 
	  { 
	    fixity: Info.fixity;
	    prec: int
	  }

  (**
      [operators(ph, info, binop, unaryopy):]

      [ph]: parser for basic (atomic) terms, such as numbers, bools etc
      [info]: return fixity information about a token
      [binop]: function to combine two arguments and a token
      [unaryop]: function to combine one argument and a token
   *)
      val operators :
        'a phrase * (token -> token_info) * (token -> 'a -> 'a -> 'a) *
        (token -> 'a -> 'a) -> 'a phrase
      val parse : 'a phrase -> token -> input -> 'a
    end
module Grammars : GRAMMARS
