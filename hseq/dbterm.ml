(*----
  Name: dbterm.ml
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

open Gtype

type binder = {quant: Basic.quant; qvar: string; qtyp: Gtype.stype}

let mk_binder q v t = {quant=q; qvar=v; qtyp=t}
let binder_kind q = q.quant
let binder_name q = q.qvar
let binder_type q = q.qtyp

type dbterm =
  | Id of Ident.t * Gtype.stype
  | Free of string * Gtype.stype
  | Qnt of binder * dbterm
  | Bound of int
  | App of dbterm * dbterm
  | Const of Basic.const_ty

(*** Conversion functions *)

let of_atom env qnts t =
  match t with
    | Basic.Id(n, ty) ->
      let (ty1, env1) = Gtype.to_save_env env ty
      in
      (Id(n, ty1), env1)
    | Basic.Free(n, ty) ->
      let (ty1, env1) = Gtype.to_save_env env ty
      in
      (Free(n, ty1), env1)
    | Basic.Const(c) -> (Const(c), env)
    | Basic.Bound(q) ->
      let q_idx = Lib.index (fun x -> (x == q)) qnts
      in
      (Bound(q_idx), env)
    | Basic.Meta(q) ->
      raise
        (Term.term_error
           "Can't convert meta variables to DB terms"
           [Basic.Atom(t)])

let rec of_term_aux env qnts t =
  match t with
  | Basic.(Atom(a)) -> of_atom env qnts a
  | Basic.App(f, a) ->
      let f1, env1 = of_term_aux env qnts f in
      let a1, env2 = of_term_aux env1 qnts a
      in
      (App(f1, a1), env2)
    | Basic.Qnt(q, b) ->
      let (tqnt, tqvar, tqtyp) = Basic.dest_binding q
      and (b1, env1) = of_term_aux env (q::qnts) b
      in
      let (ty1, env2) = Gtype.to_save_env env1 tqtyp in
      let q1 = mk_binder tqnt tqvar ty1
      in
      (Qnt(q1, b1), env2)

let of_term t =
  let (t1, _) = of_term_aux [] [] t
  in
  t1

let rec to_term_aux env qnts t =
  match t with
    | Id(n, ty) ->
      let (ty1, env1) = Gtype.from_save_env env ty
      in
      (Term.mk_typed_ident n ty1, env1)
    | Free(n, ty) ->
      let (ty1, env1) = Gtype.from_save_env env ty
      in
      (Term.mk_free n ty1, env1)
    | Const(c) -> (Term.mk_const c, env)
    | Bound(q) ->
      let q_binder = List.nth qnts q
      in
      (Term.mk_bound q_binder, env)
    | App(f, a) ->
      let (f1, env1) = to_term_aux env qnts f in
      let (a1, env2) = to_term_aux env1 qnts a
      in
      (Term.mk_app f1 a1, env2)
    | Qnt(q, b) ->
      let (ty1, env1) = Gtype.from_save_env env q.qtyp in
      let q1 =
        Basic.mk_binding (binder_kind q) (binder_name q) ty1
      in
      let (b1, env2) = to_term_aux env1 (q1::qnts) b
      in
      (Term.mk_qnt q1 b1, env2)

let to_term t =
  let (t1, _) = to_term_aux [] [] t
  in
  t1
