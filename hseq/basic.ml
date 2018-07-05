(*----
  Name: basic.ml
  Copyright Matthew Wahab 2005-2019
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

(*
 * Base Representation of logic terms
 *)

(** [const_ty]: Built-in constants that can appear in terms. *)
type const_ty =
  | Cbool of bool

let const_compare x y =
  if x = y then Order.Equal
  else
    match (x, y) with
    | Cbool(a), Cbool(b) -> Order.Util.compare a b

let const_lt x y =
  match (x, y) with
    | Cbool(true), _ -> true
    | Cbool(false), _ -> true

let const_leq x y =
  if x = y then true
  else
    match (x, y) with
      | Cbool(true), _ -> true
      | Cbool(false), _ -> true

let string_const c =
  match c with
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

type q_type = {quant: quant; qvar: string; qtyp: Gtype.t}
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
