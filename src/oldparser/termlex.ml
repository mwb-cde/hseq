
open Logicterm

  type symbols =
      DOT | COMMA | ORB | CRB | RIGHTARROW | PRIME | COLON 
    | OTHER of string

  type keys = ALL | EX | LAM (* | AND | OR | NOT | IMPLIES | EQUALS | IFF*)
	
  type tok = 
      Key of keys | Sym of symbols | ID of Basic.fnident
    | NUM of string | BOOL of bool

  exception Error

  let all_qnts = [("!", Key ALL); ("all", Key ALL); ("forall", Key ALL)]
  let ex_qnts = [("?", Key EX); ("exists", Key EX)]
  let lam_qnts = [("%", Key LAM); ("lambda", Key LAM)]
  let not_ops = [("~", ID notid); ("not", ID notid)]
  let and_ops = [("&", ID andid); ("and", ID andid)]
  let or_ops =  [("|", ID orid); ("or", ID orid)]
  let implies_ops = [("=>", ID impliesid); ("implies", ID impliesid)]
  let iff_ops = [("<=>", ID iffid); ("iff", ID iffid)]
  let equals_ops = [("=", ID equalsid)]

  let keywords_list = 
    ref  (List.concat 
	    [[("true", BOOL true)]; [("false", BOOL false)];
	      all_qnts; ex_qnts; lam_qnts; not_ops;
	      and_ops; or_ops; implies_ops; 
	      iff_ops; equals_ops])

  let keywords ()= !keywords_list

  let rec remove s ls =
    Lib.filter (fun (x, y) -> x=s) ls

  let rec token_of s ls =
    match ls with
      [] -> raise Not_found
    | (key, t)::keys -> 
	if s=key then t else token_of s keys

  let rec add_to_list lessp x ls=
    match ls with 
      [] -> [x]
    | (y::ys) -> 
	if (lessp x y) 
	then (x::y::ys)
	else y::(add_to_list lessp x ys)

(*    try (ignore(token_of k (keywords())); failwith ("Keyword "^k^" exists"))
    with Not_found ->*)

  let add_keyword k t =
      keywords_list:=add_to_list 
	   (fun x y -> 
	     (fst y) < (fst x)) (k, t) (!keywords_list)

  let remove_keyword k = keywords_list:=remove k (!keywords_list)


  let white_space =  [' '; '\n'; '\t'; '\r']
(*  let special  =  ['_'; '?' ; '!'; '\''] *)
  let special  =  ['_'; '\''; '.'] 

  let syms_list = ref [(".", Sym DOT); ("(", Sym ORB);
	       (")", Sym CRB); (",", Sym COMMA);
	       ("->", Sym RIGHTARROW); ("'", Sym PRIME);
	       (":", Sym COLON)]

  let syms () = !syms_list

  let add_symbol k t =
    try (ignore(token_of k (syms())); failwith ("Symbol "^k^" exists"))
    with Not_found ->
      syms_list:=add_to_list 
	   (fun x y -> (fst y) < (fst x)) (k, t) (!syms_list)
  let remove_symbol k = syms_list:=remove k (!syms_list)


  let is_num_char c =
    ((c>= '0') & (c <='9'))

  let is_negate_char c = (c= '-')

let is_num str i =
  if is_num_char str.[i] 
  then true
  else 
    if (String.length str)<=(i+1) then false
    else 
      (is_negate_char str.[i]) & (is_num_char str.[i+1])


  let rec first p str i=
    if i<(String.length str) then
      (if p(str.[i]) then i else first p str (i+1))
    else raise Not_found

  let get_num str i =
    let end_ind = 
      try 
	if is_negate_char str.[i]
	then 
      	  first (fun c -> (not (is_num_char c))) str (i+1)
	else 
      	  first (fun c -> (not (is_num_char c))) str i
      with Not_found -> (String.length str)
    in 
    (NUM (String.sub str i (end_ind-i)), end_ind)

  let other_lex = ref (
    fun str i -> 
      if is_num str i 
      then get_num str i
      else raise Error)

  let set_other l = other_lex:=l
  let other () str i= 
    if is_num str i 
    then get_num str i
    else raise Error

  let mk_id str = ID(Lib.chop_at '.' str)
    
      
  let string_of_token tok =
    match tok with 
    | Sym DOT -> "."
    | Sym COMMA -> ","
    | Sym ORB -> "("
    | Sym CRB -> ")"
    | Sym RIGHTARROW -> "->"
    | Sym PRIME -> "'"
    | Sym COLON -> ":"
    | Sym (OTHER s) -> s
    | Key ALL -> "ALL"
    | Key EX -> "EXISTS"
    | Key LAM -> "LAMBDA"
    | ID(s) -> (Basic.string_fnid s)
    | NUM(n) -> n
    | BOOL(b) -> string_of_bool b
(*     | _ -> "(other)" *)

  let print_token t= 
    Format.open_box 0; 
    Format.print_string (string_of_token t);
    Format.close_box()



