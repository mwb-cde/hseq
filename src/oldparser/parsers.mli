(* toolkit for top-down parsers *)

module type PARSER_TOKENS =
  sig 
    type tokens
  end

module type PARSERS =
  functor (ParseTokens: PARSER_TOKENS) ->
    sig

    exception ParsingError of string
    type token = ParseTokens.tokens

    val empty : token list -> 'b list * token list
    val (||) : (token list -> 'b) -> (token list -> 'b) -> token list -> 'b
    val (!!) : (token list -> 'b) -> token list -> 'b
    val (--) : (token list -> 'a * token list) 
        -> (token list -> 'b * token list) -> token list 
	-> ('a * 'b) * token list
    val (>>) : (token list -> 'a * token list) -> ('a -> 'b) 
        -> token list 
	-> 'b * token list
    val orl : (token list -> 'b) list -> token list -> 'b
    val (!$) : token -> token list -> token * token list
    val ($--) : token -> (token list -> 'a * 'b) -> token list -> 'a * 'b
    val optional: (token list -> 'a * token list) 
      -> token list -> ('a option * token list)
    val repeat : (token list -> 'b * token list) 
        -> token list -> 'b list * token list
    val infixes :
      (token list -> 'a * token list) 
	* (token -> int) 
	* (token -> 'a -> 'a -> 'a) 
      -> token list -> 'a * token list
    val parse : (token list -> 'b * 'c list) -> token list -> 'b
    end

module Parsers:PARSERS


