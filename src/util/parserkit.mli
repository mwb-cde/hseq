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
  end


module type TOKENS =
  sig 
    type tokens 
    val matches : tokens -> tokens -> bool 
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
      val named_alt : 
	  ('a -> ('b)phrase) Lib.named_list 
	   -> ('a -> ('b)phrase)

      type token_info = 
	  { fixity: Info.fixity;
	    prec: int
	  }

  (* 
      operators(ph, info, binop, unaryopy):
      ph: parser for basic (atomic) terms, such as numbers, bools etc
      info: return fixity information about a token
      binop: function to combine two arguments and a token
      unaryop: function to combine one argument and a token
  *)
      val operators :
        'a phrase * (token -> token_info) * (token -> 'a -> 'a -> 'a) *
        (token -> 'a -> 'a) -> 'a phrase
      val parse : 'a phrase -> token -> input -> 'a
    end
module Grammars : GRAMMARS
