module OClexers =
struct 

  module type TOKENS=
    sig 
    type tok

    exception Error
      val add_keyword : string -> tok -> unit
      val remove_keyword: string -> unit
      val add_symbol: string -> tok -> unit
      val remove_symbol : string -> unit

    val keywords : unit -> (string * tok) list
    val syms : unit -> (string * tok) list
    val set_other: (string -> int -> tok * int) -> unit
    val other : unit -> string -> int -> tok * int
    val white_space: char list
    val special: char list
    val mk_id: string -> tok


    val string_of_token: tok -> string
    val print_token: tok -> unit
    end


module type LEX=
  functor (Tokens: TOKENS) -> 
  sig 
    type tok = Tokens.tok

    val lex: stream -> tok

      val add_keyword : string -> tok -> unit
      val remove_keyword: string -> unit
      val add_symbol: string -> tok -> unit
      val remove_symbol : string -> unit
  end


module Lex=
  functor (Tokens_arg: TOKENS) -> 
  struct 

  type tok = Tokens_arg.tok

    let add_keyword = Tokens_arg.add_keyword
    let remove_keyword = Tokens_arg.remove_keyword

    let add_symbol = Tokens_arg.add_symbol
    let remove_symbol = Tokens_arg.remove_symbol


  exception Lexer
  exception Found_match of (bool * int * tok)

  let all_qnts = ["!"; "all"; "forall"]
  let ex_qnts = ["?"; "exists "]
  let not_ops = ["~"; "not"]
  let and_ops = ["&"; "/\\"; "and"]
  let or_ops =  ["|"; "\\/"; "or"]
  let implies_ops =  ["=>"; "implies"]
  let iff_ops =  ["<=>"; "iff"]

  let is_space c = List.mem c Tokens_arg.white_space

  let is_special c = List.mem c Tokens_arg.special

  let is_num c =
    ((c>= '0') & (c <='9'))

  let is_alpha c =
     ((c>= 'a') & (c <='z'))
    or ((c>= 'A') & (c<='Z'))

  let is_idstr c =
    (is_alpha c) or (is_num c) or (is_special c)

  let rec first p str i=
    if i<(String.length str) then
      (if p(str.[i]) then i else first p str (i+1))
    else raise Not_found

  let test_key ky str i sz szs=
    let tk = String.sub str i szs 
    in 
    if tk = ky
    then if (is_alpha tk.[0]) & sz>(i+szs)
	then (not (is_idstr (str.[i+szs])))
	else true
    else false

    let match_str str i size s =
      let szs= String.length s
      in 
      (if (size >= (i+szs))
      then 
	if (test_key s str i size szs)
	then raise (Found_match 
		      (true, i+szs, Tokens_arg.mk_id  ""))
	else ()
      else ())

    let match_with_list str i sz kls =
      try 
	((List.iter 
	   (fun (x, y) -> 
	     (try 
	       (match_str str i sz x)
	     with Found_match (_, nxt, _) ->
	       raise (Found_match (true, nxt, y))))
	   kls); (false, i, Tokens_arg.mk_id ""))
      with Found_match x -> x


    let match_keywords str i sz = 
      match_with_list str i sz (Tokens_arg.keywords())
    let match_syms str i sz = 
      match_with_list str i sz (Tokens_arg.syms())

  let get_alpha str i =
    let end_ind = 
      try 
      	first (fun c -> not (is_idstr c)) str i
      with Not_found -> (String.length str)
    in 
    (String.sub str i (end_ind-i), end_ind)

  let get_num str i =
    let end_ind = 
      try 
      	first (fun c -> (not (is_num c))) str i
      with Not_found -> (String.length str)
    in 
    (String.sub str i (end_ind-i), end_ind)

  let rec scan str i=
    if (str = "") or (i=String.length str) then []
    else if is_space (str.[i]) then scan str (i+1)
    else 
      (let key, nxt, tok= match_keywords str i (String.length str)
      in 
      if key then (tok::(scan str nxt))
      else if (is_alpha str.[i]) 
      then 
	(let stra, nxt = get_alpha str i
      	in (Tokens_arg.mk_id stra)::(scan str nxt))
      else 
	(let sym, nxt, symtok = match_syms str i (String.length str)
	in 
	if sym then (symtok::scan str nxt)
	else 
	  try 
	    (let othtok, nxt = (Tokens_arg.other()) str i
	    in othtok::(scan str nxt))
	  with Tokens_arg.Error -> raise Lexer))

  let lex str = scan str 0
  end 
end
