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

module Parsers=
  functor (ParseTokens: PARSER_TOKENS)->
  struct

    exception ParsingError of string

    type token = ParseTokens.tokens

    let empty toks = ([], toks)

    let (||) ph1 ph2 = 
      (fun toks -> try (ph1 toks) with ParsingError _ -> (ph2 toks))
	
    let (!!) ph toks = 
      try (ph toks) with ParsingError m -> failwith("Parsing error: "^m)

    let (--) ph1 ph2 = 
      (fun toks ->
      	let (x, toks2)=ph1 toks
      	in 
      	let (y, toks3)=ph2 toks2
      	in ((x, y), toks3))

    let (>>) ph f =
      (fun toks ->
      	let (x, toks2) = ph toks
      	in (f x, toks2))

    let rec orl phl toks= 
      match phl with
	[] -> raise (ParsingError "No alternative parsers")
      |	(ph::phs) -> 
	  (try ph toks 
	  with ParsingError _ -> (orl phs toks))

    let (!$) a toks =
      match toks with 
       	(t::ts) -> 
	  (if a = t
	  then (a, ts) 
	  else raise (ParsingError "No match"))
      | _ -> raise (ParsingError "Empty input")

    let ($--) a ph = ((!$a) -- (!!ph)) >> snd
    
    let rec repeat ph toks =
      (((ph -- (repeat ph)) >> (fun (x, y) -> x ::y))
     || empty) toks

    let optional ph toks =
      (try ((ph >> (fun x->(Some x))) toks)
      with (ParsingError _)  -> (None, toks))

    let infixes (ph, prec_of, apply) tokns =
      let rec over k toks = next k (ph toks)
      and next k ts=
      	match ts with
	  (x, []) -> (x, [])
      	| (x, t::toks) ->
      	    (if (prec_of t) < k then (x, t::toks)
	    else next k ((over (prec_of t)>>apply t x) toks))
      in over 0 tokns

    let parse ph a = 
      match (ph a) with
      	(x, []) -> x
      | (_, _) -> raise (ParsingError "Too many tokens")

end;;
