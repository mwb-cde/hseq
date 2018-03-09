(*----
  Name: basic.ml
  Copyright Matthew Wahab 2005-2018
  Author: Matthew Wahab <mwb.cde@gmail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under
  the terms of the Lesser GNU General Public License as published by
  the Free Software Foundation; either version 3, or (at your option)
  any later version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
  License for more details.

  You should have received a copy of the Lesser GNU General Public
  License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
----*)

(*
 *   Basic constants and data structures
 *)

(* Base Representation of logic types *)

(** [pre_typ]: The base representation of types. *)
type ('a) pre_typ =
  | Atom of 'a
  | TApp of (('a) pre_typ * ('a) pre_typ)
  | Constr of Ident.t * (('a)pre_typ)list

(** [gtype_id]: The type of gtype identifiers. *)
type gtype_id = (string)Tag.t

let mk_gtype_id s = Tag.make s
let gtype_id_string i = Tag.contents i
let gtype_id_copy i = mk_gtype_id (gtype_id_string i)

let gtype_id_equal x y = Tag.equal x y

let gtype_id_compare x y =
  if gtype_id_equal x y
  then Order.Equal
  else
    let xc = Tag.contents x
    and yc = Tag.contents y
    in
    if (Order.Util.compare xc yc) = Order.GreaterThan
    then Order.GreaterThan
    else Order.LessThan

let gtype_id_greaterthan x y = (gtype_id_compare x y) = Order.GreaterThan
let gtype_id_lessthan x y = (gtype_id_compare x y) = Order.LessThan

(** [atomtype] Kinds of atomic type *)
type atomtype =
  | Var of gtype_id
  | Weak of gtype_id
  | Ident of Ident.t

(** [gtype]: The actual representation of types. *)
type gtype = (atomtype)pre_typ

let mk_vartype x = Atom(Var(x))
let mk_weakvartype x = Atom(Weak(x))
let mk_identtype x = Atom(Ident(x))
let mk_apptype x y = TApp(x, y)

(**
   [flatten_apptype ty]: flatten an application in [ty] to a list of
   types.
*)
let rec flatten_apptype ty =
  let rec flat_aux t rslt =
    match t with
      | TApp(l, r) -> flat_aux l (r::rslt)
      | _ -> t::rslt
  in
  flat_aux ty []

let split_apptype ty =
  match flatten_apptype ty with
  | x::xs -> (x, xs)
  | _ -> raise (Invalid_argument "split_apptype")

(* [map_atomtype f ty] Apply [f] to each [Atom(x)] in [ty] returning the
   resulting type. *)
let rec map_atomtype f ty =
  match ty with
  | Atom(_) -> f ty
  | TApp(l, r) -> TApp(map_atomtype f l, map_atomtype f r)
  | Constr(x, args) -> Constr(x, List.map (map_atomtype f) args)

(** String representation of types *)
let string_tconst n l =
  (Ident.string_of n)
  ^"("
  ^(Lib.list_string (fun x-> x) ", " l)
  ^")"

(*
 * Base Representation of logic terms
 *)

(** [const_ty]: Built-in constants that can appear in terms. *)
type const_ty =
  | Cnum of Num.num    (* Arbitrary precision numbers. *)
  | Cbool of bool

let const_compare x y =
  if x = y then Order.Equal
  else
    match (x, y) with
    | Cbool(a), Cbool(b) -> Order.Util.compare a b
    | Cbool(_), _ -> Order.LessThan
    | Cnum(_), Cbool(_) -> Order.GreaterThan
    | Cnum(a), Cnum(b) -> Order.Util.compare a b

let const_lt x y =
  match (x, y) with
    | Cbool(true), _ -> true
    | Cbool(false), _ -> true
    | Cnum(_), Cbool(_) -> false
    | Cnum(a), Cnum(b) -> a < b

let const_leq x y =
  if x = y then true
  else
    match (x, y) with
      | Cbool(true), _ -> true
      | Cbool(false), _ -> true
      | Cnum(_), Cbool(_) -> false
      | Cnum(a), Cnum(b) -> a <= b

let string_const c =
  match c with
    | Cnum n -> Num.string_of_num n
    | Cbool b -> string_of_bool b

(*
 * Basis of quantified terms
 *)

(** [quant]: Quantifiers for terms. *)
type quant =
  | All
  | Ex
  | Lambda
  | Gamma

(** [quant_string q]: The string representation of [q]. *)
let quant_string x =
  match x with
    | All -> "!"
    | Ex -> "?"
    | Lambda -> "%"
    | Gamma -> "_"

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

type q_type = {quant: quant; qvar: string; qtyp: gtype}
type binders = (q_type)Tag.t

(* Binder operations *)

let mk_binding qn qv qt =
  Tag.make { quant=qn; qvar=qv; qtyp=qt }
let dest_binding b =
  let q = Tag.contents b
  in
  (q.quant, q.qvar, q.qtyp)

let binder_kind b =
  let (x, _, _) = dest_binding b
  in x
let binder_name b =
  let (_, x, _) = dest_binding b
  in x
let binder_type b =
  let (_, _, x) = dest_binding b
  in x

let binder_equality x y = Tag.equal x y
let binder_compare x y =
  if binder_equality x y
  then Order.Equal
  else
    let xc = Tag.contents x
    and yc = Tag.contents y
    in
    if (Order.Util.compare xc yc) = Order.GreaterThan
    then Order.GreaterThan
    else Order.LessThan

let binder_greaterthan x y = (binder_compare x y) = Order.GreaterThan
let binder_lessthan x y = (binder_compare x y) = Order.LessThan

(** The representation of a term *)
type term =
  | Id of Ident.t * gtype
  | Bound of binders
  | Free of string * gtype
  | Meta of binders
  | App of term * term
  | Qnt of binders * term
  | Const of const_ty
