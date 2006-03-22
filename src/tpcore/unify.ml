(*-----
 Name: unify.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)


open Basic
open Term
open Result

(** Unification of terms. *)

exception Occurs
exception Unify of string

(** Occurs check *)
let rec occurs s t =
  if Term.equals s t then raise Occurs
  else 
    match t with
      App(f, a) -> occurs s f; occurs s a
    | Typed(tt, _) -> occurs s t
    | Qnt(_, b) -> occurs s b
    | _ -> ()

let bind_occs s t env =
  try occurs t s; Term.bind s t env
  with Occurs -> raise (term_error "occurs:" [t; s])

(***
* General Unification 
***)

(** Unify terms w.r.t given type and term contexts *)

let unify_fullenv scp typenv trmenv varp trm1 trm2 =
  let eq_binder tyenv b1 b2 = 
    let qnt1, _, qty1=dest_binding b1
    and qnt2, _, qty2=dest_binding b2
    in 
    if (qnt1=qnt2) then 
      (try 
	(true, Gtypes.unify_env scp qty1 qty2 tyenv)
      with _ -> (false, tyenv))
    else (false, tyenv)
  in 
  let rec unify_aux tyenv env t1 t2 = 
    let s= Term.chase_var varp t1 env
    and t= Term.chase_var varp t2 env
    in 
    if (varp s) 
    then
      if (equals s t) then (tyenv, env)
      else (tyenv, bind_occs s t env)
    else 
      if (varp t) 
      then (tyenv, bind_occs t s env)
      else
	(match (s, t) with
	  (App(f1, a1), App(f2, a2)) ->
	    let tyenv1, env1= unify_aux tyenv env f1 f2
	    in 
	    let tyenv2, env2= unify_aux tyenv1 env1 a1 a2
	    in 
	    (tyenv2, env2)
	| (Qnt(q1, b1), Qnt(q2, b2)) ->
	    if(Basic.binder_kind q1=Basic.binder_kind q2)
	    then 
	      let qtst, qtyenv = eq_binder tyenv q1 q2
	      in 
	      if qtst 
	      then unify_aux qtyenv env b1 b2
	      else raise (term_error "unify_aux: qnt" [t1;t2])
	    else raise (term_error "unify_aux: qnt" [t1;t2])
	| (Typed(tt1, ty1), Typed(tt2, ty2)) ->
	    (try
	      let tyenv1=Gtypes.unify_env scp ty1 ty2 tyenv
	      in 
	      unify_aux tyenv1 env tt1 tt2
	    with x -> 
	      raise (add_error (term_error "unify_aux: typed" [t1; t2]) x))
	| (Typed(tt1, _), x) -> unify_aux tyenv env tt1 x
	| (x, Typed(tt2, _)) -> unify_aux tyenv env x tt2
	| (Id(n1, ty1), Id(n2, ty2)) ->
	    if n1=n2 
	    then (Gtypes.unify_env scp ty1 ty2 tyenv, env)
	    else raise (term_error "unify_aux: var" [t1;t2])
	| (Free(n1, ty1), Free(n2, ty2)) ->
	    if n1=n2 
	    then (Gtypes.unify_env scp ty1 ty2 tyenv, env)
	    else raise (term_error "unify_aux: var" [t1;t2])
	| (Bound(q1), Bound(q2)) ->
	    if ((Term.is_meta s) || (Term.is_meta t))
	    then 
	      (if (binder_equality q1 q2)
	      then (tyenv, env)
	      else raise (term_error"unify_aux: meta" [t1;t2]))
	    else 
	      let qtst, qtyenv=eq_binder tyenv q1 q2
	      in 
		if qtst
		then (qtyenv, env)
		else raise (term_error "unify_aux: bound" [t1;t2])
	| (Const(c1), Const(c2)) ->
	    if c1=c2 then (tyenv, env)
	    else raise (term_error "unify_aux: const" [t1;t2])
	| (_, _) -> 
	    if Term.equals s t 
	    then (tyenv, env)
	    else raise (term_error "unify_aux: default" [t1;t2]))
  in 
  unify_aux typenv trmenv trm1 trm2

(**  Unify terms in a given term context. *)
let unify_env ?typenv scp env varp trm1 trm2 =
  let tye = 
    match typenv with 
      None -> Gtypes.empty_subst()
    | Some x -> x
  in 
  let rettypenv, retenv= 
    unify_fullenv scp tye env varp trm1 trm2
  in retenv

(** Unify terms and in scope. *)

let unify ?typenv ?initial scp varp trm1 trm2 = 
  let tye = 
    match typenv with 
      None -> Gtypes.empty_subst()
    | Some x -> x
  and subst = 
    match initial with
      None -> Term.empty_subst()
    | Some x -> x
  in 
  let rettypenv, retenv= 
    unify_fullenv scp tye subst varp trm1 trm2
  in 
  retenv



(*** Matching ***)

let matches_rewrite scp typenv trmenv varp trm1 trm2 =
  let eq_binder tydata b1 b2 = 
    let qnt1, _, qty1=dest_binding b1
    and qnt2, _, qty2=dest_binding b2
    in 
    if (qnt1=qnt2)
    then 
      (try 
	(true, Gtypes.matches_rewrite scp qty1 qty2 tydata)
      with _ -> (false, tydata))
    else (false, tydata)
  in 
  let var_type x = 
    match x with 
	Bound(q) -> Basic.binder_type q
      | Free(_, ty) -> ty
      | _ -> 
	  raise 
	    (term_error "matches_rewrite: invalid match variable" [x])
  in 
  let rec matches_aux tydata env t1 t2 = 
    let s= Term.chase_var varp t1 env
    and t = t2
    in 
    if (varp s) 
    then 
      (let (_, tydata1) = Gtypes.copy_set_ty (var_type s) tydata
       in 
	 if (equals s t) 
	 then (tydata1, env)
	 else (tydata1, bind_occs s t env))
    else 
      (match (s, t) with
	   (App(f1, a1), App(f2, a2)) ->
	     let tydata1, env1=matches_aux tydata env f1 f2
	     in 
	     let tydata2, env2= matches_aux tydata1 env1 a1 a2
	     in 
	       (tydata2, env2)
	 | (Qnt(q1, b1), Qnt(q2, b2)) ->
	     if(Basic.binder_kind q1=Basic.binder_kind q2)
	    then 
	      let qtst, qtydata=eq_binder tydata q1 q2
	      in 
	      if qtst
	      then matches_aux qtydata env b1 b2
	      else raise (term_error "matches_rewrite: qnts" [t1;t2])
	    else raise (term_error "matches_rewrite: qnts" [t1;t2])
	| (Typed(tt1, ty1), Typed(tt2, ty2)) ->
	    (try
	      let tydata1=Gtypes.matches_rewrite scp ty1 ty2 tydata
	      in 
	      matches_aux tydata1 env tt1 tt2
	    with x -> 
	      raise 
		(add_error (term_error "matches_rewrite: typed" [t1;t2]) x))
	| (Typed(tt1, _), x) -> matches_aux tydata env tt1 x
	| (x, Typed(tt2, _)) -> matches_aux tydata env x tt2
	| (Id(n1, ty1), Id(n2, ty2)) ->
	    if n1=n2 
	    then 
	      let tydata1=Gtypes.matches_rewrite scp ty1 ty2 tydata
	      in 
	      (tydata1, env)
	    else raise (term_error "matches_rewrite: var"[t1;t2])
	| (Free(n1, ty1), Free(n2, ty2)) ->
	    if n1=n2 
	    then 
	      let tydata1=Gtypes.matches_rewrite scp ty1 ty2 tydata
	      in 
	      (tydata1, env)
	    else raise (term_error "matches_rewrite: var"[t1;t2])
	| (Bound(q1), Bound(q2)) ->
	    if ((Term.is_meta s) || (Term.is_meta t))
	    then 
	      (if (binder_equality q1 q2)
	      then (tydata, env)
	      else raise (term_error"matches_rewrite: meta" [t1;t2]))
	    else 
	      let qtst, qtydata=eq_binder tydata q1 q2
	      in 
		if qtst
		then (qtydata, env)
		else raise (term_error"matches_rewrite: bound" [t1;t2])
	| (Const(c1), Const(c2)) ->
	    if c1=c2 then (tydata, env)
	    else raise (term_error "matches_rewrite: const" [t1;t2])
	| (_, _) -> 
	    if Term.equals s t 
	    then (tydata, env) 
	    else raise (term_error "matches_rewrite: default" [t1;t2]))
  in 
  let tydata = 
    {Gtypes.vars = Gtypes.empty_subst(); 
     Gtypes.tyenv= typenv}
  in 
  let (tydata1, trmenv1) =
    matches_aux tydata trmenv trm1 trm2
  in 
    (tydata1.Gtypes.tyenv, trmenv1)



(*
let matches_rewrite scp typenv trmenv varp trm1 trm2 =
  let eq_binder tydata b1 b2 = 
    let qnt1, _, qty1=dest_binding b1
    and qnt2, _, qty2=dest_binding b2
    in 
    if (qnt1=qnt2)
    then 
      (try 
	(true, Gtypes.matches_rewrite scp qty1 qty2 tydata)
      with _ -> (false, tydata))
    else (false, tydata)
  in 
  let rec matches_aux tydata env t1 t2 = 
    let s= Term.chase_var varp t1 env
    and t= Term.chase_var varp t2 env
    in 
    if (varp s) 
    then 
      (if (equals s t) then (tydata, env)
      else (tydata, bind_occs s t env))
    else 
      if (varp t) 
      then if(equals s t) then (tydata, env) 
      else (tydata, bind_occs t s env)
      else
	(match (s, t) with
	  (App(f1, a1), App(f2, a2)) ->
	    let tydata1, env1=matches_aux tydata env f1 f2
	    in 
	    let tydata2, env2= matches_aux tydata1 env1 a1 a2
	    in 
	      (tydata2, env2)
	| (Qnt(q1, b1), Qnt(q2, b2)) ->
	    if(Basic.binder_kind q1=Basic.binder_kind q2)
	    then 
	      let qtst, qtydata=eq_binder tydata q1 q2
	      in 
	      if qtst
	      then matches_aux qtydata env b1 b2
	      else raise (term_error "matches_rewrite: qnts" [t1;t2])
	    else raise (term_error "matches_rewrite: qnts" [t1;t2])
	| (Typed(tt1, ty1), Typed(tt2, ty2)) ->
	    (try
	      let tydata1=Gtypes.matches_rewrite scp ty1 ty2 tydata
	      in 
	      matches_aux tydata1 env tt1 tt2
	    with x -> 
	      raise 
		(add_error (term_error "matches_rewrite: typed" [t1;t2]) x))
	| (Typed(tt1, _), x) -> matches_aux tydata env tt1 x
	| (x, Typed(tt2, _)) -> matches_aux tydata env x tt2
	| (Id(n1, ty1), Id(n2, ty2)) ->
	    if n1=n2 
	    then 
	      let tydata1=Gtypes.matches_rewrite scp ty1 ty2 tydata
	      in 
	      (tydata1, env)
	    else raise (term_error "matches_rewrite: var"[t1;t2])
	| (Free(n1, ty1), Free(n2, ty2)) ->
	    if n1=n2 
	    then 
	      let tydata1=Gtypes.matches_rewrite scp ty1 ty2 tydata
	      in 
	      (tydata1, env)
	    else raise (term_error "matches_rewrite: var"[t1;t2])
	| (Bound(q1), Bound(q2)) ->
	    if ((Term.is_meta s) || (Term.is_meta t))
	    then 
	      (if (binder_equality q1 q2)
	      then (tydata, env)
	      else raise (term_error"matches_rewrite: meta" [t1;t2]))
	    else 
	      let qtst, qtydata=eq_binder tydata q1 q2
	      in 
		if qtst
		then (qtydata, env)
		else raise (term_error"matches_rewrite: bound" [t1;t2])
	| (Const(c1), Const(c2)) ->
	    if c1=c2 then (tydata, env)
	    else raise (term_error "matches_rewrite: const" [t1;t2])
	| (_, _) -> 
	    if Term.equals s t 
	    then (tydata, env) 
	    else raise (term_error "matches_rewrite: default" [t1;t2]))
  in 
  let tydata = 
    {Gtypes.vars = Gtypes.empty_subst(); 
     Gtypes.tyenv= typenv}
  in 
  let (tydata1, trmenv1) =
    matches_aux tydata trmenv trm1 trm2
  in 
    (tydata1.Gtypes.tyenv, trmenv1)
*)



module Retired =
struct

(***
* Unification for rewriting
***)

(*
   unify_rewrite:
   A version of unify_fullenv for rewriting.
   [unify_rewrite scp tenv env varp trm1 trm2]
   is equivalent to
   [unify_fullenv scp tenv env varp trm1' trm2]
   where [trm1'] is obtained from [trm1] by applying [Gtypes.copy_type]
   to each type in [trm1].
   usage: trm1 is normally the lhs of a rewrite rule which is to be
   applied to trm2. 
 *)
let unify_rewrite scp typenv trmenv varp trm1 trm2 =
  let eq_binder tyenv b1 b2 = 
    let qnt1, _, qty1=dest_binding b1
    and qnt2, _, qty2=dest_binding b2
    in 
    if (qnt1=qnt2)
    then 
      (try 
	(true, Gtypes.Retired.unify_for_rewrite scp qty1 qty2 tyenv)
      with _ -> (false, tyenv))
    else (false, tyenv)
  in 
  let rec unify_aux tyenv env t1 t2 = 
    let s= Term.chase_var varp t1 env
    and t= Term.chase_var varp t2 env
    in 
    if (varp s) 
    then 
      (if (equals s t) then (tyenv, env)
      else (tyenv, bind_occs s t env))
    else 
      if (varp t) 
      then if(equals s t) then (tyenv, env) 
      else (tyenv, bind_occs t s env)
      else
	(match (s, t) with
	  (App(f1, a1), App(f2, a2)) ->
	    let tyenv1, env1=unify_aux tyenv env f1 f2
	    in let tyenv2, env2= unify_aux tyenv1 env1 a1 a2
	    in (tyenv2, env2)
	| (Qnt(q1, b1), Qnt(q2, b2)) ->
	    if(Basic.binder_kind q1=Basic.binder_kind q2)
	    then 
	      let qtst, qtyenv=eq_binder tyenv q1 q2
	      in 
	      if qtst
	      then unify_aux qtyenv env b1 b2
	      else raise (term_error "unify_full: qnts" [t1;t2])
	    else raise (term_error "unify_full: qnts" [t1;t2])
	| (Typed(tt1, ty1), Typed(tt2, ty2)) ->
	    (try
	      let tyenv1=Gtypes.Retired.unify_for_rewrite scp ty1 ty2 tyenv
	      in 
	      unify_aux tyenv1 env tt1 tt2
	    with x -> 
	      raise (add_error (term_error "unify_full: typed" [t1;t2]) x))
	| (Typed(tt1, _), x) -> unify_aux tyenv env tt1 x
	| (x, Typed(tt2, _)) -> unify_aux tyenv env x tt2
	| (Id(n1, ty1), Id(n2, ty2)) ->
	    if n1=n2 
	    then 
	      let tyenv1=Gtypes.Retired.unify_for_rewrite scp ty1 ty2 tyenv
	      in 
	      (tyenv1, env)
	    else raise (term_error "unify_full: var"[t1;t2])
	| (Free(n1, ty1), Free(n2, ty2)) ->
	    if n1=n2 
	    then 
	      let tyenv1=Gtypes.Retired.unify_for_rewrite scp ty1 ty2 tyenv
	      in 
	      (tyenv1, env)
	    else raise (term_error "unify_full: var"[t1;t2])
	| (Bound(q1), Bound(q2)) ->
	    if ((Term.is_meta s) || (Term.is_meta t))
	    then 
	      (if (binder_equality q1 q2)
	      then (tyenv, env)
	      else raise (term_error"unify_full: meta" [t1;t2]))
	    else 
	      let qtst, qtyenv=eq_binder tyenv q1 q2
	      in 
		if qtst
		then (qtyenv, env)
		else raise (term_error"unify_full: bound" [t1;t2])
	| (Const(c1), Const(c2)) ->
	    if c1=c2 then (tyenv, env)
	    else raise (term_error "unify_full: const" [t1;t2])
	| (_, _) -> 
	    if Term.equals s t 
	    then (tyenv, env) 
	    else raise (term_error "unify_full: default" [t1;t2]))
  in 
  unify_aux typenv trmenv trm1 trm2

(** Top-level unification for rewriting. *)
let unify_env_rewrite scp env varp trm1 trm2 =
  let _, nenv= 
    unify_rewrite scp (Gtypes.empty_subst()) env varp trm1 trm2
  in nenv


end
