(*-----
 Name: basic.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

type thy_id = string
type ident = (thy_id * string)

let null_thy = ""
let null_id = ("", "")
let is_null_id x = x=null_id
let thy_of_id (t, _) = t
let name (_, n) = n
let mk_long t n = (t, n)
let mk_name n = (null_thy, n)
let is_short_id (t, _) = t=null_thy

type id_selector = bool
let fn_id = true
let type_id = not fn_id

let string_fnid n =
  if (thy_of_id n)=null_thy then name n
  else (thy_of_id n)^"."^(name n)


let dest_fnid (t, n) = (t, n)

type quant_ty =
    All
  | Ex
  | Lambda
  | Meta (* used for skolem constants *)

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
    Lambda -> 100
  | All -> 101
  | Ex -> 102
  | _ -> 0

let quant_string x =
  match x with 
    All -> "!"
  | Ex -> "?" 
  | Lambda -> "%"
  | Meta -> "*??*"


(* Types *)

type base_typ = Bool | Num | Ind
type typ_const = (* Func |*) Defined of ident

type ('idtyp, 'tfun, 'tcons) pre_typ =
    Var of 'idtyp
  | Constr of 'tfun * ('idtyp, 'tfun, 'tcons) pre_typ list
  | Base of 'tcons
  | WeakVar of 'idtyp

type gtype = (string ref, typ_const, base_typ)pre_typ
(*
(* representation of types for storage on disk *)
type stype = 
    ((string * int), typ_const, base_typ) pre_typ
*)

(* Terms *)

type q_type = {quant: quant_ty; qvar: string; qtyp: gtype}
type binders = q_type ref
type term =
    Id of ident* gtype  
  | Bound of q_type ref
  | Free of string * gtype
  | App of term * term
  | Qnt of quant_ty * binders * term 
  | Const of const_ty
  | Typed of term * gtype

(* Binder operations *)
let binder_equality x y = x==y
let mk_binding qn qv qt 
    = ref{quant=qn; qvar=qv; qtyp=qt}
let dest_binding b = ((!b.quant), (!b.qvar), (!b.qtyp))
let binder_kind b = 
  let (x, _, _) = dest_binding b
  in x
let binder_name b = 
  let (_, x, _) = dest_binding b
  in x
let binder_type b = 
  let (_, _, x) = dest_binding b
  in x


let string_btype x =
  match x with 
    Bool -> "bool"
  | Num  -> "num"
  | Ind -> "ind"

let string_tconst x l =
  match x with 
(*    Func -> "("^(List.nth l 0)^"->"^(List.nth l 1)^")" *)
  | Defined n -> ((string_fnid n)^"("^
		  (Lib.list_string (fun x-> x) ", " l)^")")

