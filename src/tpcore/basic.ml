type thy_id = string
type fnident = (thy_id* string)

let null_thy = ""
let null_id = ("", "")
let is_null_id x = x=null_id
let thy_of_id (t, _) = t
let name (_, n) = n
let mklong t n = (t, n)
let mkname n = (null_thy, n)
let is_short_id (t, _) = t=null_thy

type id_selector = bool
let fn_id = true
let type_id = not fn_id

let string_fnid n =
  if (thy_of_id n)=null_thy then name n
  else (thy_of_id n)^"."^(name n)


let dest_fnid (t, n) = (t, n)

type conns_ty =
    Not
  | And
  | Or
  | Implies
  | Iff
  | Equal

type quant_ty =
    All
  | Ex
  | Lambda

type const_ty =
    Null_const of int
(*       | Cnum of int   *)
(* for big numbers *)
  | Cnum of Num.num    (* big numbers *)
  | Cbool of bool

let const_lt x y=
    match (x, y) with
      Cbool(true), _ -> true
    | Cbool(false), _ -> true
    | Cnum(_), Cbool(_) -> false
    | Cnum(a), Cnum(b) -> a<b
    | _,_ -> false

let const_leq x y=
  if x=y then true
  else
    match (x, y) with
      Cbool(true), _ -> true
    | Cbool(false), _ -> true
    | Cnum(_), Cbool(_) -> false
    | Cnum(a), Cnum(b) -> a<=b
    | _,_ -> false


let string_const c=
    match c with 
      Null_const _ -> "null_constant"
(*    | Cnum n -> (string_of_int n) *)
  | Cnum n -> (Num.string_of_num n) 
  | Cbool b -> (string_of_bool b)

type fns =
    Name of fnident

let prec_con c =
  match c with
    Equal -> 1
  | Not -> 2
  | And -> 3
  | Or -> 3
  | Iff -> 4
  | Implies -> 5

let std_prec f =
  match f with
    "equals" -> 1
  | "not" -> 2
  | "and" -> 3
  | "or" -> 3
  | "iff" -> 4
  | "implies" -> 5
  | _ -> -1


let prec_qnt q = 
  match q with 
    Lambda -> -1
  | _ -> 0

let conns_string x =
  match x with
    Not -> "not"
  | And -> "and"
  | Or -> "or"
  | Implies -> "=>"
  | Iff -> "<=>"
  | Equal -> "="

let connc_string c args =
  match c with 
    Not -> "not "^(List.hd args)^""
  | And -> (List.hd args)^" and "^(List.hd (List.tl args))
  | Or -> (List.hd args)^" or "^(List.hd (List.tl args))
  | Implies -> (List.hd args)^" => "^(List.hd (List.tl args))
  | Iff -> (List.hd args)^" <=> "^(List.hd (List.tl args))
  | Equal -> (List.hd args)^" = "^(List.hd (List.tl args))


let quant_string x =
  match x with 
    All -> "!"
  | Ex -> "?" 
  | Lambda -> "%"

let fns_string x = 
  match x with
    Name n -> string_fnid n

(* types *)

  type base_typ =
      Bool
    | Num
    | Ind

  type typ_const =
      Func
    | Defined of fnident

 let string_btype x =
    match x with 
      Bool -> "bool"
    | Num  -> "num"
    | Ind -> "ind"

  let string_tconst x l =
    match x with 
      Func -> "("^(List.nth l 0)^"->"^(List.nth l 1)^")"
    | Defined n -> ((string_fnid n)^"("^
		    (Lib.list_string (fun x-> x) ", " l)^")")


let date ()= Unix.time()

