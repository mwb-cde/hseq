
open Term
open Result

exception Occurs

exception Unify of string

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
  with Occurs -> raise (termError "occurs:" [t; s])

(*
let remove_bindings ls env =
  let rec remove_aux ds =
    match ds with
      [] -> env
    | (s::bs) -> Term.remove s env; remove_aux bs
  in remove_aux ls
*)

(*
let unify_fullenv scp tyenv env varp trm1 trm2 =
  let bindings = ref([])
  in 
  let add_binding a = bindings:=(a::!bindings)
  in 
  let eqqnt_env scp s t tenv = 
    let _, qnt1, _, qty1, _=dest_qnt s
    and _, qnt2, _, qty2, _=dest_qnt t
    in 
    if qnt1=qnt2 then 
      (try 
	Gtypes.unify_env scp qty1 qty2 tenv; true
      with _ -> false)
    else false
  in 
  let eq_binder b1 b2 = 
    let qnt1, _, qty1=dest_binding b1
    and qnt2, _, qty2=dest_binding b2
    in 
    if (qnt1=qnt2) then 
      (try 
	(Gtypes.unify_env scp qty1 qty2 tyenv; true)
      with _ -> false)
    else false
  in 
  let rec unify_aux t1 t2 = 
    let s= Term.chase_var varp t1 env
    and t= Term.chase_var varp t2 env
    in 
    if (varp s) 
    then
      if (equals s t) then ()
      else (bind_occs s t env; add_binding s; ())
    else 
      if (varp t) 
      then (bind_occs t s env; add_binding t; ())
      else
	(match (s, t) with
	  (App(f1, a1), App(f2, a2)) ->
	    unify_aux f1 f2;
	    unify_aux a1 a2;()
	|	(Qnt(q1, b1), Qnt(q2, b2)) ->
	    if (eqqnt_env scp s t tyenv)
	    then (unify_aux b1 b2;())
	    else raise (termError "unify_aux: qnt" [t1;t2])
	|	(Typed(tt1, ty1), Typed(tt2, ty2)) ->
	    (try
	      (Gtypes.unify_env scp ty1 ty2 tyenv;
	       unify_aux tt1 tt2; ())
	    with x -> 
	      raise (catchError (mktermError "unify_aux: typed" [t1; t2]) x))
	|	(Typed(tt1, _), x) -> unify_aux tt1 x; ()
	|	(x, Typed(tt2, _)) -> unify_aux x tt2; ()
	| (Var(n1, ty1), Var(n2, ty2)) ->
	    if n1=n2 
	    then (Gtypes.unify_env scp ty1 ty2 tyenv; ())
	    else raise (termError "unify_aux: var" [t1;t2])
	|	(Bound(q1), Bound(q2)) ->
	    if eq_binder q1 q2
	    then ()
	    else raise (termError "unify_aux: bound" [t1;t2])
	|	(Const(c1), Const(c2)) ->
	    if c1=c2 then ()
	    else raise (termError "unify_aux: const" [t1;t2])
	| (_, _) -> 
	    if Term.equals s t 
	    then () else raise (termError "unify_aux: default" [t1;t2]))
  in 
  try (unify_aux trm1 trm2; env)
  with x -> ignore(remove_bindings (!bindings) env); raise x

*)

let unify_fullenv scp typenv trmenv varp trm1 trm2 =
  let eqqnt_env tyenv s t = 
    let _, qnt1, _, qty1, _=dest_qnt s
    and _, qnt2, _, qty2, _=dest_qnt t
    in 
    if qnt1=qnt2 then 
      (try 
	(true, Gtypes.unify_env scp qty1 qty2 tyenv)
      with _ -> (false, tyenv))
    else (false, tyenv)
  in 
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
	    in let tyenv2, env2= unify_aux tyenv1 env1 a1 a2
	    in (tyenv1, env1)
	| (Qnt(q1, b1), Qnt(q2, b2)) ->
	    let qtst, qtyenv = eqqnt_env tyenv s t
	    in 
	    if qtst 
	    then unify_aux qtyenv env b1 b2
	    else raise (termError "unify_aux: qnt" [t1;t2])
	| (Typed(tt1, ty1), Typed(tt2, ty2)) ->
	    (try
	      let tyenv1=Gtypes.unify_env scp ty1 ty2 tyenv
	      in 
	      unify_aux tyenv1 env tt1 tt2
	    with x -> 
	      raise (catchError (mktermError "unify_aux: typed" [t1; t2]) x))
	| (Typed(tt1, _), x) -> unify_aux tyenv env tt1 x
	| (x, Typed(tt2, _)) -> unify_aux tyenv env x tt2
	| (Var(n1, ty1), Var(n2, ty2)) ->
	    if n1=n2 
	    then (Gtypes.unify_env scp ty1 ty2 tyenv, env)
	    else raise (termError "unify_aux: var" [t1;t2])
	| (Bound(q1), Bound(q2)) ->
	    let qtst, qtyenv=eq_binder tyenv q1 q2
	    in 
	    if qtst
	    then (qtyenv, env)
	    else raise (termError "unify_aux: bound" [t1;t2])
	| (Const(c1), Const(c2)) ->
	    if c1=c2 then (tyenv, env)
	    else raise (termError "unify_aux: const" [t1;t2])
	| (_, _) -> 
	    if Term.equals s t 
	    then (tyenv, env)
	    else raise (termError "unify_aux: default" [t1;t2]))
  in 
  unify_aux typenv trmenv trm1 trm2

let unify_env scp env varp trm1 trm2 =
  let rettypenv, retenv= 
    unify_fullenv scp (Gtypes.empty_subst()) env varp trm1 trm2
  in retenv

let unify scp varp trm1 trm2 = 
  unify_env scp (Term.empty_subst()) varp trm1 trm2

(*   
   let matches_full scp varp tyenv env term1 term2 =
   let eqqnt_env scp s t tenv = 
   let _, qnt1, _, qty1, _=dest_qnt s
   and _, qnt2, _, qty2, _=dest_qnt t
   in 
   if qnt1=qnt2 then 
   (try 
   (Gtypes.unify_env scp qty1 qty2 tenv; true)
   with _ -> false)
   else false
   in 
   let rec find_match trm1 trm2=
   let s= Term.chase varp trm1 env
   and t=term2
   in 
   if (varp s) 
   then (bind_occs s t env; ())
   else 
   (match (s, t) with
   (Bound(q1), Bound(q2)) ->
   (if  (Term.equals s t)                     (* q1==q2  *)
   then ()
   else 
   if Term.binder_equiv scp s t then ()
   else 
   raiseError ("Can't match bound variable "^(string_term s)
   ^" with bound variable "^(string_term t)))
   | (App(f1, a1), App(f2, a2)) ->
   find_match f1 f2;
   find_match a1 a2
   | (Qnt(q1, b1), Qnt(q2, b2)) ->
   if (eqqnt_env scp s t tyenv)
   then find_match b1 b2
   else 
   raiseError ("Can't match quantifier "^(string_term s)
   ^" with quantifier "^(string_term t))
   | (Typed(trm1, ty1), Typed(trm2, ty2)) ->
   find_match trm1 trm2
   | (Var(n1, ty1), Var(n2, ty2)) ->
   if n1=n2 
   then (Gtypes.unify scp ty1 ty2; ())
   else 
   raiseError ("Can't match name "^(string_term s)
   ^" with name "^(string_term t))
   | (_, _) -> 
   if Term.equals s t then  ()
   else raiseError ("Can't match term "^(string_term s)
   ^" with term "^(string_term t)))
   in find_match term1 term2
 *)

(*
   unify_fullenv_rewrite:
   A version of unify_fullenv for rewriting.
   [unify_fullenv_rewrite scp tenv env varp trm1 trm2]
   is equivalent to
   [unify_fullenv scp tenv env varp trm1' trm2]
   where [trm1'] is obtained from [trm1] by applying [Gtypes.copy_type]
   to each type in [trm1].
   usage: trm1 is normally the lhs of a rewrite rule which is to be
   applied to trm2. 
 *)

(*
let unify_fullenv_rewrite scp tyenv env varp trm1 trm2 =
  let bindings = ref([])
  and type_bindings = ref []
  in 
  let add_binding a = bindings:=(a::!bindings)
  in 
  let eqqnt_env scp s t= 
    let _, qnt1, _, qty1, _=dest_qnt s
    and _, qnt2, _, qty2, _=dest_qnt t
    in 
    if qnt1=qnt2 then 
      (try 
 	(Gtypes.unify_for_rewrite scp qty1 qty2 tyenv type_bindings;
	 true) 
      with _ -> false)
    else false
  in 
  let eq_binder b1 b2 = 
    let qnt1, _, qty1=dest_binding b1
    and qnt2, _, qty2=dest_binding b2
    in 
    if (qnt1=qnt2)
    then 
      (try 
	(Gtypes.unify_for_rewrite scp qty1 qty2 tyenv type_bindings;
	 true)
      with _ -> false)
    else false
  in 
  let rec unify_aux t1 t2 = 
    let s= Term.chase_var varp t1 env
    and t= Term.chase_var varp t2 env
    in 
    if (varp s) 
    then 
      (if (equals s t) then () 
      else (bind_occs s t env; add_binding s; ()))
    else 
      if (varp t) 
      then if(equals s t) then () 
      else (bind_occs t s env; add_binding t; ())
      else
	(match (s, t) with
	  (App(f1, a1), App(f2, a2)) ->
	    unify_aux f1 f2;
	    unify_aux a1 a2;()
	|	(Qnt(q1, b1), Qnt(q2, b2)) ->
	    if (eqqnt_env scp s t)
	    then (unify_aux b1 b2;())
	    else raise (termError "unify_full: qnts" [t1;t2])
	|	(Typed(tt1, ty1), Typed(tt2, ty2)) ->
	    (try
	      (Gtypes.unify_for_rewrite scp ty1 ty2 tyenv type_bindings;
	       unify_aux tt1 tt2; ())
	    with x -> 
	      raise (catchError (mktermError "unify_full: typed" [t1;t2]) x))
	|	(Typed(tt1, _), x) -> unify_aux tt1 x; ()
	|	(x, Typed(tt2, _)) -> unify_aux x tt2; ()
	| (Var(n1, ty1), Var(n2, ty2)) ->
	    if n1=n2 
	    then 
	      Gtypes.unify_for_rewrite scp ty1 ty2 tyenv type_bindings
	    else raise (termError "unify_full: var"[t1;t2])
	|	(Bound(q1), Bound(q2)) ->
	    if eq_binder q1 q2
	    then ()
	    else raise (termError"unify_full: bound" [t1;t2])
	|	(Const(c1), Const(c2)) ->
	    if c1=c2 then ()
	    else raise (termError "unify_full: const" [t1;t2])
	| (_, _) -> 
	    if Term.equals s t 
	    then () else raise (termError "unify_full: default" [t1;t2]))
  in 
  try 
    (unify_aux trm1 trm2; env)
  with x -> 
    ignore(remove_bindings (!bindings) env); 
    ignore(Gtypes.remove_bindings (!type_bindings) tyenv);
    raise x
*)

let unify_fullenv_rewrite scp typenv trmenv varp trm1 trm2 =
  let eqqnt_env tyenv s t= 
    let _, qnt1, _, qty1, _=dest_qnt s
    and _, qnt2, _, qty2, _=dest_qnt t
    in 
    if qnt1=qnt2 then 
      (try 
 	(true, Gtypes.unify_for_rewrite scp qty1 qty2 tyenv)
      with _ -> (false, tyenv))
    else (false, tyenv)
  in 
  let eq_binder tyenv b1 b2 = 
    let qnt1, _, qty1=dest_binding b1
    and qnt2, _, qty2=dest_binding b2
    in 
    if (qnt1=qnt2)
    then 
      (try 
	(true, Gtypes.unify_for_rewrite scp qty1 qty2 tyenv)
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
	    let qtst, qtyenv=eqqnt_env tyenv s t
	    in 
	    if qtst
	    then unify_aux qtyenv env b1 b2
	    else raise (termError "unify_full: qnts" [t1;t2])
	| (Typed(tt1, ty1), Typed(tt2, ty2)) ->
	    (try
	      let tyenv1=Gtypes.unify_for_rewrite scp ty1 ty2 tyenv
	      in 
	      unify_aux tyenv1 env tt1 tt2
	    with x -> 
	      raise (catchError (mktermError "unify_full: typed" [t1;t2]) x))
	| (Typed(tt1, _), x) -> unify_aux tyenv env tt1 x
	| (x, Typed(tt2, _)) -> unify_aux tyenv env x tt2
	| (Var(n1, ty1), Var(n2, ty2)) ->
	    if n1=n2 
	    then 
	      let tyenv1=Gtypes.unify_for_rewrite scp ty1 ty2 tyenv
	      in 
	      (tyenv1, env)
	    else raise (termError "unify_full: var"[t1;t2])
	| (Bound(q1), Bound(q2)) ->
	    let qtst, qtyenv=eq_binder tyenv q1 q2
	    in 
	    if qtst
	    then (qtyenv, env)
	    else raise (termError"unify_full: bound" [t1;t2])
	| (Const(c1), Const(c2)) ->
	    if c1=c2 then (tyenv, env)
	    else raise (termError "unify_full: const" [t1;t2])
	| (_, _) -> 
	    if Term.equals s t 
	    then (tyenv, env) 
	    else raise (termError "unify_full: default" [t1;t2]))
  in 
  unify_aux typenv trmenv trm1 trm2

let unify_env_rewrite scp env varp trm1 trm2 =
  let _, nenv= 
    unify_fullenv_rewrite scp (Gtypes.empty_subst()) env varp trm1 trm2
  in nenv
