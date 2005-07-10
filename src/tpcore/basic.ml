(*-----
 Name: basic.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(***
*   Basic constants and data structures
***)

(***
* Identifiers for functions and types
***)

type thy_id = string
type ident = (thy_id * string)

let null_thy = ""
let null_id = ("", "")
let is_null_id x = x=null_id

let thy_of_id (t, _) = t
let name (_, n) = n
let mk_long t n = (t, n)
let mk_name n = (null_thy, n)
let dest_fnid (t, n) = (t, n)
let is_short_id (t, _) = t=null_thy
let string_fnid n =
  if (thy_of_id n)=null_thy then name n
  else (thy_of_id n)^"."^(name n)

(***
 *   id_selector: Choose whether to interpret an identifier as a
 *   function or type identifier. (Should be removed).
 ***)
type id_selector = bool
let fn_id = true
let type_id = not fn_id

(***
* Base Representation of logic types 
***)

(** 
   [base_type]: The built-in types (should be removed) 

   [typ_const]: Representation of user-defined type constructors
   (could merged into [pre_typ] 
*) 
type base_typ = Bool | Num | Ind
type typ_const = (* Func |*) Defined of ident

(** [pre_typ]: The base representation of types *)
type ('idtyp, 'tfun, 'tcons) pre_typ =
    Var of 'idtyp
  | Constr of 'tfun * ('idtyp, 'tfun, 'tcons) pre_typ list
  | Base of 'tcons
  | WeakVar of 'idtyp

(** [gtype]: The actual representation of types *)
type gtype = (string ref, typ_const, base_typ)pre_typ

(** 
   String representation of types
*)
let string_btype x =
  match x with 
    Bool -> "bool"
  | Num  -> "num"
  | Ind -> "ind"

let string_tconst x l =
  match x with 
    Defined n -> 
      ((string_fnid n)^"("^
       (Lib.list_string (fun x-> x) ", " l)^")")

(***
* Base Representation of logic terms
***)

(** [const_ty]: Built-in constants that can appear in terms *)
type const_ty =
    Cnum of Num.num    (* big numbers *)
  | Cbool of bool

let const_lt x y=
  match (x, y) with
    Cbool(true), _ -> true
  | Cbool(false), _ -> true
  | Cnum(_), Cbool(_) -> false
  | Cnum(a), Cnum(b) -> a<b

let const_leq x y=
  if x=y then true
  else
    match (x, y) with
      Cbool(true), _ -> true
    | Cbool(false), _ -> true
    | Cnum(_), Cbool(_) -> false
    | Cnum(a), Cnum(b) -> a<=b

let string_const c=
  match c with 
    Cnum n -> (Num.string_of_num n) 
  | Cbool b -> (string_of_bool b)

(***
* Basis of quantified terms 
***)

(** 
   [quant_ty]: Quantifiers for terms 

   [quant_string q]: The string representation of [q].
*)
type quant_ty =
    All
  | Ex
  | Lambda
  | Meta (* used for skolem constants *)

let quant_string x =
  match x with 
    All -> "!"
  | Ex -> "?" 
  | Lambda -> "%"
  | Meta -> "*??*"

(**
   [q_type]: The data stored in a binder.

   [binders]: Associating bound variables with their binding term.

   [mk_binding k n ty]: Make a binder of kind [k], with name [n] and
   type [ty].

   [dest_binding b]: Destructor for binders.

   [binder_kind b]: The kind of binder binding variable [b].

   [binder_name b]: The name of bound variable [b].

   [binder_type b]: The type of bound variable [b].

   [binder_equality]: Equality of binders.
*)
type q_type = {quant: quant_ty; qvar: string; qtyp: gtype}
type binders = q_type ref

(* Binder operations *)
let binder_equality x y = x==y
let mk_binding qn qv qt = ref{quant=qn; qvar=qv; qtyp=qt}
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

(** [term]: The representation of a term *)
type term =
    Id of ident* gtype  
  | Bound of q_type ref
  | Free of string * gtype
  | App of term * term
  | Qnt of binders * term 
  | Const of const_ty
  | Typed of term * gtype



