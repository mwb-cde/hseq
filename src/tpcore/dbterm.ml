(*----
 Name: dbterm.ml
 Copyright M Wahab 2005-2009
 Author: M Wahab  <mwb.cde@googlemail.com>

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

open Gtypes

type binder = {quant: Basic.quant; qvar: string; qtyp: Gtypes.stype}

let mk_binder q v t = {quant=q; qvar=v; qtyp=t}
let binder_kind q = q.quant
let binder_name q = q.qvar
let binder_type q = q.qtyp

type dbterm =
    Id of Ident.t * Gtypes.stype
  | Free of string * Gtypes.stype
  | Qnt of binder * dbterm
  | Bound of int
  | App of dbterm * dbterm
  | Const of Basic.const_ty

(*** Conversion functions *)

let rec of_term_aux env qnts t= 
  match t with
    Basic.Id(n, ty) -> Id(n, (Gtypes.to_save_env env ty))
  | Basic.Free(n, ty) -> Free(n, (Gtypes.to_save_env env ty))
  | Basic.Const(c) -> Const(c)
  | Basic.App(f, a) -> App(of_term_aux env qnts f, of_term_aux env qnts a)
  | Basic.Bound(q) ->
      Bound(Lib.index (fun x -> (x==q)) qnts)
  | Basic.Qnt(q, b) -> 
      let (tqnt, tqvar, tqtyp) = Basic.dest_binding q
      and nb = of_term_aux env (q::qnts) b
      in 
      Qnt(mk_binder tqnt tqvar (Gtypes.to_save_env env tqtyp), nb)
  | Basic.Meta(q) ->
      raise 
	(Term.term_error "Can't convert meta variables to DB terms" [t])
	
let of_term t = of_term_aux (ref []) [] t

let rec to_term_aux env qnts t =
  match t with
    Id(n, ty) -> Basic.Id(n, Gtypes.from_save_env env ty)
  | Free(n, ty) -> Basic.Free(n, Gtypes.from_save_env env ty)
  | Const(c) -> Basic.Const(c)
  | App(f, a) -> Basic.App(to_term_aux env qnts f, to_term_aux env qnts a)
  | Bound(q) -> Basic.Bound(List.nth qnts q)
  | Qnt(q, b) ->
      let nq = 
	Basic.mk_binding 
	  (binder_kind q)
	  (binder_name q)
	  (Gtypes.from_save_env env q.qtyp)
      in 
      Basic.Qnt(nq, to_term_aux env (nq::qnts) b)

let to_term t = to_term_aux (ref[]) [] t

(** Additional I/O functions *)
let output oc d = output_value oc d 
let input ic = ((input_value ic):dbterm)


