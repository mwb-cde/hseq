(*-----
 Name: basic.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005, 2006
----*)

(***
*   Basic constants and data structures
***)

(***
* Base Representation of logic types 
***)

(** 
   [base_type]: The built-in types (should be removed) 

   [typ_const]: Representation of user-defined type constructors
   (could merged into [pre_typ] 
*) 
type typ_const = Ident.t

(** [pre_typ]: The base representation of types *)
type ('idtyp, 'tfun) pre_typ =
    Var of 'idtyp
  | Constr of 'tfun * ('idtyp, 'tfun) pre_typ list
  | WeakVar of 'idtyp

(** [gtype]: The actual representation of types *)
type gtype = (string ref, typ_const)pre_typ

(** 
   String representation of types
*)
let string_tconst n l =
  ((Ident.string_of n)^"("^
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
    Id of Ident.t* gtype  
  | Bound of q_type ref
  | Free of string * gtype
  | App of term * term
  | Qnt of binders * term 
  | Const of const_ty
  | Typed of term * gtype



