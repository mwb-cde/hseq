(*----
  Name: basic.ml
  Copyright Matthew Wahab 2005-2016
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
 * Base Representation of logic types
 *)

(** [typ_const]: Representation of user-defined type constructors
    (could merged into [pre_typ]). *)
type typ_const = Ident.t

(** [pre_typ]: The base representation of types. *)
type ('idtyp, 'tfun) pre_typ =
  | Var of 'idtyp
  | Constr of 'tfun * ('idtyp, 'tfun) pre_typ list
  | WeakVar of 'idtyp

(** [gtype_id]: The type of gtype identifiers. *)
type gtype_id = string ref

let mk_gtype_id s = ref s
let gtype_id_string i = (!i)
let gtype_id_copy i = mk_gtype_id (gtype_id_string i)
let gtype_id_equal x y = (x == y)

let gtype_id_compare x y =
  if gtype_id_equal x y
  then Order.Equal
  else
    begin
      match Order.Util.compare (gtype_id_string x) (gtype_id_string y) with
      | Order.GreaterThan -> Order.GreaterThan
      | _ -> Order.LessThan
    end

(** [gtype]: The actual representation of types. *)
type gtype = (gtype_id, typ_const)pre_typ

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
type binders = q_type ref

(*
 * Binder operations
 *)

let mk_binding qn qv qt = ref {quant=qn; qvar=qv; qtyp=qt}
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

let binder_compare x y =
  if x == y then Order.Equal
  else
    begin
      match Order.Util.compare x y with
      | Order.GreaterThan -> Order.GreaterThan
      | _ -> Order.LessThan
    end

let binder_equality x y = (x == y)

(** The representation of a term *)
type term =
  | Id of Ident.t * gtype
  | Bound of binders
  | Free of string * gtype
  | Meta of binders
  | App of term * term
  | Qnt of binders * term
  | Const of const_ty
