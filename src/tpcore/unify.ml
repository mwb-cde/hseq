(*----
  Name: unify.ml
  Copyright M Wahab 2005-2009, 2010
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

open Basic
open Term
open Report

(** Unification of terms. *)

exception Occurs
exception Unify of string

(** Occurs check *)
let rec occurs s t =
  if Term.equals s t 
  then raise Occurs
  else 
    match t with
      | App(f, a) -> occurs s f; occurs s a
      | Qnt(_, b) -> occurs s b
      | _ -> ()

let bind_occs s t env =
  try occurs t s; Term.bind s t env
  with Occurs -> raise (term_error "occurs:" [t; s])

(*
 * General Unification 
 *)

(** Unify terms w.r.t given type and term contexts *)

let unify_fullenv scp typenv trmenv varp trm1 trm2 =
  let lookup q sbs = 
    let r = Bound q
    in 
    try Term.find r sbs 
    with Not_found -> r
  in
  let eq_binder tyenv b1 b2 = 
    let qnt1, _, qty1 = dest_binding b1
    and qnt2, _, qty2 = dest_binding b2
    in 
    if qnt1 = qnt2 
    then 
      try (true, Gtypes.unify_env scp qty1 qty2 tyenv)
      with _ -> (false, tyenv)
    else (false, tyenv)
  in 
  let rec unify_aux tyenv env qntenv t1 t2 = 
    let s = Term.chase_var varp t1 env
    and t = Term.chase_var varp t2 env
    in 
    if varp s 
    then
      if equals s t
      then (tyenv, env)
      else (tyenv, bind_occs s t env)
    else 
      if varp t 
      then (tyenv, bind_occs t s env)
      else
	match (s, t) with
	  | (App(f1, a1), App(f2, a2)) ->
	    let tyenv1, env1 = unify_aux tyenv env qntenv f1 f2 in 
	    let tyenv2, env2= unify_aux tyenv1 env1 qntenv a1 a2
	    in 
	    (tyenv2, env2)
	  | (Qnt(q1, b1), Qnt(q2, b2)) ->
	    let qtst, qtyenv = eq_binder tyenv q1 q2
	    in 
	    if qtst 
	    then 
	      let nqntenv = bind (Bound q1) (Bound q2) qntenv
	      in 
	      unify_aux qtyenv env nqntenv b1 b2
	    else raise (term_error "unify_aux: qnt" [t1;t2])
	  | (Id(n1, ty1), Id(n2, ty2)) ->
	    if n1 = n2 
	    then (Gtypes.unify_env scp ty1 ty2 tyenv, env)
	    else raise (term_error "unify_aux: var" [t1;t2])
	  | (Free(n1, ty1), Free(n2, ty2)) ->
	    if n1 = n2 
	    then (Gtypes.unify_env scp ty1 ty2 tyenv, env)
	    else raise (term_error "unify_aux: var" [t1;t2])
	  | (Meta(q1), Meta(q2)) ->
	    (if binder_equality q1 q2
	     then (tyenv, env)
	     else raise (term_error"unify_aux: meta" [t1;t2]))
	  | (Bound(q1), Bound(q2)) ->
	    let nq1 = dest_bound (lookup q1 qntenv)
	    in 
	    if binder_equality nq1 q2
	    then (tyenv, env)
	    else raise (term_error "unify_aux: bound" [t1;t2])
	  | (Const(c1), Const(c2)) ->
	    if c1 = c2 
            then (tyenv, env)
	    else raise (term_error "unify_aux: const" [t1;t2])
	  | (_, _) -> 
	    if Term.equals s t 
	    then (tyenv, env)
	    else raise (term_error "unify_aux: default" [t1;t2])
  in 
  unify_aux typenv trmenv (Term.empty_subst()) trm1 trm2

(**  Unify terms in a given term context. *)
let unify_env ?typenv scp env varp trm1 trm2 =
  let tye = 
    match typenv with 
      | None -> Gtypes.empty_subst()
      | Some x -> x
  in 
  let (_, retenv) = unify_fullenv scp tye env varp trm1 trm2
  in 
  retenv

(** Unify terms and in scope. *)

let unify ?typenv ?initial scp varp trm1 trm2 = 
  let tye = 
    match typenv with 
      | None -> Gtypes.empty_subst()
      | Some x -> x
  and subst = 
    match initial with
      | None -> Term.empty_subst()
      | Some x -> x
  in 
  let (_, retenv) = 
    unify_fullenv scp tye subst varp trm1 trm2
  in 
  retenv

(*** Matching ***)

let retype tyenv t=
  let rec retype_aux qenv t =
    match t with
      | Id(n, ty) -> Id(n, Gtypes.mgu ty tyenv)
      | Free(n, ty) -> Free(n, Gtypes.mgu ty tyenv) 
      | Bound( _ ) -> 
	(try table_find t qenv
	 with Not_found -> t)
      | Meta( _ ) -> t
      | Const( _ ) -> t
      | App(f, a) -> App(retype_aux qenv f, retype_aux qenv a)
      | Qnt(q, b) ->
        let (oqnt, oqnm, oqty) = Basic.dest_binding q in 
        let nty = Gtypes.mgu oqty tyenv in 
        let nq = mk_binding oqnt oqnm nty 
        in 
        let qenv1 = table_add (Bound(q)) (Bound(nq)) qenv; qenv in
        let new_term = Qnt(nq, retype_aux qenv1 b)
        in 
        let _ = table_remove (Bound(q)) qenv1; qenv 
        in 
        new_term
  in 
  retype_aux (empty_table()) t

(* Rename the type variables in a term. *)
let term_copy_type env term = 
  let rec copy_aux qntenv tyenv trm = 
    match trm with
      | Id(n, ty) -> 
	  let ty1, tyenv1 = Gtypes.rename_type_vars_env tyenv ty
	  in 
          (Id(n, ty1), tyenv1)
      | Free(n, ty) -> 
	let ty1, tyenv1 = Gtypes.rename_type_vars_env tyenv ty
	in 
        (Free(n, ty1), tyenv1)
      | Bound(q) -> 
	let qtrm = 
          try Term.find trm qntenv 
          with Not_found -> trm
	in
	(qtrm, tyenv)
      | Meta(q) -> (trm, tyenv)
      | Const(c) -> (trm, tyenv)
      | App(f, a) -> 
        let ftrm, ftyenv = copy_aux qntenv tyenv f in 
        let atrm, atyenv = copy_aux qntenv ftyenv a
        in
        (App(ftrm, atrm), atyenv)
      | Qnt(q, b) ->
        let (oqnt, oqnm, oqty) = Basic.dest_binding q in
        let nty, ntyenv = Gtypes.rename_type_vars_env tyenv oqty in
        let nq = mk_binding oqnt oqnm nty in
        let nqntenv = Term.bind (Bound(q)) (Bound(nq)) qntenv in
        let btrm, btyenv = copy_aux nqntenv ntyenv b 
        in 
        (Qnt(nq, btrm), btyenv)
  in 
  copy_aux (Term.empty_subst()) env term
    
(** Match terms w.r.t given type and term contexts *)
let matches_full scp typenv trmenv varp trm1 trm2 =
  let lookup q sbs = 
    let r = Bound q
    in 
    try Term.find r sbs
    with Not_found -> r
  in
  let eq_binder tyenv b1 b2 = 
    let (qnt1, _, qty1) = dest_binding b1
    and (qnt2, _, qty2) = dest_binding b2
    in 
    if qnt1 = qnt2
    then 
      try (true, Gtypes.matching_env scp tyenv qty1 qty2)
      with _ -> (false, tyenv)
    else (false, tyenv)
  in 
  let rec matches_aux tyenv env qntenv t1 t2 = 
    let s = Term.chase_var varp t1 env
    in 
    if varp s
    then
      if equals s t2 
      then (tyenv, env)
      else (tyenv, bind_occs s t2 env)
    else 
      match (s, t2) with
	| (App(f1, a1), App(f2, a2)) ->
	    let (tyenv1, env1) = matches_aux tyenv env qntenv f1 f2 in 
	    let (tyenv2, env2) = matches_aux tyenv1 env1 qntenv a1 a2
	    in 
	    (tyenv2, env2)
	| (Qnt(q1, b1), Qnt(q2, b2)) ->
	  let (qtst, qtyenv) = eq_binder tyenv q1 q2
	  in 
	  if qtst 
	  then 
	    let nqntenv = bind (Bound q1) (Bound q2) qntenv
	    in 
	    matches_aux qtyenv env nqntenv b1 b2
	  else 
            raise (term_error "matches_aux: qnt" [t1;t2])
	| (Id(n1, ty1), Id(n2, ty2)) ->
	  if n1 = n2 
	  then (Gtypes.matching_env scp tyenv ty1 ty2, env)
	  else raise (term_error "matches_aux: var" [t1;t2])
	| (Free(n1, ty1), Free(n2, ty2)) ->
	  if n1 = n2 
	  then (Gtypes.matching_env scp tyenv ty1 ty2, env)
	  else raise (term_error "matches_aux: var" [t1;t2])
	| (Meta(q1), Meta(q2)) ->
	  if binder_equality q1 q2
	  then (tyenv, env)
	  else raise (term_error"matches_aux: meta" [t1;t2])
	| (Bound(q1), Bound(q2)) ->
	  let nq1 = dest_bound (lookup q1 qntenv)
	  in 
	  if binder_equality nq1 q2
	  then (tyenv, env)
	  else raise (term_error "matches_aux: bound" [t1;t2])
	| (Const(c1), Const(c2)) ->
	  if c1 = c2
          then (tyenv, env)
	  else raise (term_error "matches_aux: const" [t1;t2])
	| (_, _) -> 
	  if Term.equals s t2
	  then (tyenv, env)
	  else raise (term_error "matches_aux: default" [t1;t2])
  in 
  matches_aux typenv trmenv (Term.empty_subst()) trm1 trm2

let matches_rewrite scp typenv env varp trm1 trm2 =
  let (trm1a, _) = term_copy_type (Gtypes.empty_subst()) trm1
  in
  matches_full scp typenv env varp trm1a trm2

