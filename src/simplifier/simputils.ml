(*----
  Name: simputils.ml
  Copyright M Wahab 2005-2010
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

(** Utility functions for the simplifier *)

open Term
open Lterm

(** [is_variable qnts x]: Test for variables (universal quantifiers)
    in an entry
*)
let is_variable qnts x= Rewrite.is_free_binder qnts x

(** [equal_upto_vars varp x y]: Terms [x] and [y] are equal upto the
    position of the terms for which [varp] is true (which are
    considered to be variables.)

    This is used to determine whether a rewrite- or simp-rule could
    lead to an infinite loop (e.g. |- (x and y) = (y and x) ).
*)
let rec equal_upto_vars varp x y =
  if (varp x) & (varp y)
  then true
  else 
    match (x, y) with
      | (Basic.App(f1, arg1), Basic.App(f2, arg2))->
	(equal_upto_vars varp f1 f2) && (equal_upto_vars varp arg1 arg2)
      | (Basic.Qnt(qn1, b1), Basic.Qnt(qn2, b2)) -> 
	(qn1 == qn2) && (equal_upto_vars varp b1 b2)
      | (_, _) -> Term.equals x y

(** [find_variables is_var vars trm]: find all subterms [t] of [trm]
    s.t. [(is_var t)] is true, add [t] to [vars] then return [vars]
*)
let find_variables is_var vars trm =
  let rec find_aux env t =
    match t with
      | Basic.Qnt(_, b) -> find_aux env b
      | Basic.Bound(q) ->
	if is_var q
	then 
	  try ignore(Term.find t env); env
	  with Not_found -> Term.bind t t env
	else env
      | Basic.App(f, a) -> 
	let nv = find_aux env f
	in 
        find_aux nv a
      | _ -> env
  in find_aux vars trm

(** [check_variables is_var vars trm]: Check that all subterms [t] of
    [trm] s.t. [is_var t] are in [vars].  *)
let check_variables is_var vars trm =
  let rec check_aux t =
    match t with
      | Basic.Qnt(_, b) -> check_aux b
      | Basic.Bound(q) ->
	if is_var q
	then ignore(Term.find t vars)
	else ()
      | Basic.App(f, a) -> check_aux f; check_aux a
      | _ -> ()
  in check_aux trm

(** [strip_qnt_cond trm]: split rule [trm] into variable binders,
    condition, equality rules are of the form: a=>c c
*)
let strip_qnt_cond t =
  (* get leading quantifiers *)
  let (qs, t1) = Term.strip_qnt (Basic.All) t in 
  if Lterm.is_implies t1  (* deal with conditional equalities *)
  then 
    let (_, a, c) = Term.dest_binop t1
    in 
    (qs, Some a, c)
  else (qs, None, t1)

(** [apply_merge_list f lst]: Apply [f] to each element [x] in [lst]
    and repeat for the resulting list. Concatenate the list of lists
    that result. If [f x] fails, keep [x] as the result.
*)
let apply_merge_list f ls = 
  let rec app_aux ys result =
    match ys with 
      | [] -> result
      | x::xs -> 
        begin
	  try 
	    let nlst = f x in 
	    let nresult = app_aux nlst result
	    in 
	    app_aux xs nresult
	  with _ -> app_aux xs (x::result)
        end
  in
  app_aux ls []

(** [fresh_thm th]: Test whether theorem [th] is fresh in the global
    scope.
*)
let fresh_thm th = Logic.is_fresh (Global.scope()) th

(** [simp_beta_conv scp t]: Apply {!Logic.Conv.beta_conv} to [t] if
    [t] is of the form << (% x: F) a >>.  Raise [Failure] if [t] is not
    an application.
*)
let simp_beta_conv scp t =
  match t with
    | Basic.App(Basic.Qnt(q, _), a) ->
      if Basic.binder_kind q = Basic.Lambda
      then Logic.Conv.beta_conv scp t
      else failwith "simp_beta_conv"
    | _ -> failwith "simp_beta_conv"
