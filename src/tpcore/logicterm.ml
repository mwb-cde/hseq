(*-----
 Name: logicterm.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Basic
open Gtypes
open Term

let base_thy = "base"


(* from typing.ml *)

(* Types *)

(* Type ind *)
let mk_ind_ty() = Gtypes.mk_base Ind
let is_ind_ty t = 
  match t with 
    Base Ind -> true
  | _ -> false

(* Type of functions *)

let fun_ty_id = Basic.mk_long base_thy "FUN"

let mk_fun_ty l r = Gtypes.mk_constr (Defined fun_ty_id) [l; r]
let is_fun_ty t = 
  match t with
    Constr (Defined x, _) -> x=fun_ty_id
  | _ -> false

let dest_fun_ty t = 
  if(is_fun_ty t)
  then 
    match t with
      Constr(Defined _, [a1; a2]) -> (a1, a2)
    | _ -> raise (Failure "Not function type")
  else raise (Failure "Not function type")
      

let rec mk_fun_ty_from_list l r = 
  match l with
    [] -> raise (Failure "No argument types")
  | [t] -> mk_fun_ty t r
  | t::ts -> mk_fun_ty t (mk_fun_ty_from_list ts r)

let arg_type t = 
  let (l, _) =  dest_fun_ty t
  in l

let ret_type t = 
  let (_, r) =  dest_fun_ty t
  in r

let rec chase_ret_type t=
  if(is_fun_ty t)
  then 
    chase_ret_type (ret_type t)
  else t


let bool_ty_id = Basic.mk_long base_thy "bool"
let mk_bool_ty = Gtypes.mk_base (Basic.Bool)
let is_bool_ty t = (t = mk_bool_ty)


let typeof_cnst c =
  match c with
    Cnum _ -> Gtypes.mk_num
  | Cbool _ -> mk_bool_ty

let bin_ty a1 a2 r = (mk_fun_ty_from_list [a1; a2] r)

(*
let typeof_conn c =
  match c with
    Not -> mk_fun_ty_from_list [Gtypes.mk_bool] Gtypes.mk_bool
  | x -> mk_fun_ty_from_list 
	[Gtypes.mk_bool; Gtypes.mk_bool] Gtypes.mk_bool
*)

(* Terms *)

let trueid = Basic.mk_long base_thy "true"
let falseid = Basic.mk_long base_thy "false"
let notid = Basic.mk_long base_thy "not"
let andid = Basic.mk_long base_thy "and"
let orid = Basic.mk_long base_thy "or"
let impliesid = Basic.mk_long base_thy "implies"
let iffid = Basic.mk_long base_thy "iff"
let equalsid = Basic.mk_long base_thy "equals"
let equalssym = "="

(*
let someid = Basic.mk_long base_thy "any"
*)
let anyid = Basic.mk_long base_thy "any"

let mk_not t = mk_fun notid [t]
let mk_and l r = mk_fun andid [l; r]
let mk_or l r = mk_fun orid [l; r]
let mk_implies l r = mk_fun impliesid [l; r]
let mk_iff l r = mk_fun iffid [l; r]
let mk_equality l r = mk_fun equalsid [l; r]

let is_neg t = try(fst(dest_fun t) = notid) with _ -> false
let is_conj t = try(fst(dest_fun t) = andid) with _ -> false
let is_disj t = try( fst(dest_fun t) = orid) with _ -> false
let is_implies t = try (fst(dest_fun t) = impliesid) with _ -> false
let is_equality t = try (fst(dest_fun t) = equalsid) with _ -> false

(*
   let is_false t = 
   try(fst(dest_fun t) = falseid) with _ -> false
*)


let is_true t = 
  match t with 
    (Const (Cbool true)) -> true 
  | _ -> false

let is_false t = 
  match t with 
    (Const (Cbool false)) -> true 
  | _ -> false

let mk_true = mk_const(Cbool true)
let mk_false = mk_const(Cbool false)

let mk_bool b = 
  if b then mk_true else mk_false

let dest_bool b = 
  if (b = mk_true) then true
  else 
    (if b = mk_false then false
    else raise (Failure "Not a boolean"))

let dest_equality t = 
  if is_equality t 
  then 
    (match snd(dest_fun t) with
      [l; r] -> (l, r)
    |	_ -> raise (term_error "Badly formed equality" [t]))
  else raise (Result.error "Not an equality")

let mk_all tyenv n b= mk_qnt_name tyenv Basic.All n b
let mk_ex tyenv n b= mk_qnt_name tyenv Basic.Ex n b
let mk_lam tyenv n b= mk_qnt_name tyenv Basic.Lambda n b

let mk_all_ty tyenv n ty b= mk_typed_qnt_name tyenv Basic.All ty n b
let mk_ex_ty tyenv n ty b= mk_typed_qnt_name tyenv Basic.Ex ty n b
let mk_lam_ty tyenv n ty b= mk_typed_qnt_name tyenv Basic.Lambda ty n b

let mk_any=Term.mk_var anyid

let is_all t = 
  ((is_qnt t) &
   (match (get_binder_kind t) with Basic.All -> true | _ -> false))

let is_exists t = 
  (is_qnt t) &
  (match (get_binder_kind t) with Basic.Ex -> true | _ -> false)

let is_lambda t = 
  ((is_qnt t) &
   (match (get_binder_kind t) with Basic.Lambda -> true | _ -> false))

let alpha_convp_full scp tenv t1 t2 =
  let rec alpha_aux t1 t2 tyenv trmenv =
    match (t1, t2) with
      (Id(n1, ty1), Id(n2, ty2)) -> 
	if (n1=n2) 
	then (trmenv, Gtypes.matches_env scp tyenv ty1 ty2)
	else raise (term_error "alpha_convp_aux" [t1;t2])
    | (Bound(q1), Bound(q2)) ->
	let q1trm= (chase (fun x->true) t1 trmenv) 
	and q2trm = (chase (fun x->true) t2 trmenv) 
	in 
	if equals q1trm q2trm
	then (trmenv, tyenv)
	else raise (term_error "alpha_convp_aux" [t1;t2])
    | (App(f1, a1), App(f2, a2)) ->
	let (trmenv1, tyenv1)=alpha_aux f1 f2 tyenv trmenv
	in 
	alpha_aux a1 a2 tyenv1 trmenv1
    | (Qnt(q1, b1), Qnt(q2, b2)) ->
	(let qty1=Basic.binder_type q1
	and qty2=Basic.binder_type q2
	and qn1=Basic.binder_kind q1
	and qn2=Basic.binder_kind q2
	in 
	if (qn1=qn2) 
	then 
	  let tyenv1=Gtypes.matches_env scp tyenv qty1 qty2
	  in 
	  alpha_aux b1 b2 tyenv1 (bind (Bound(q1)) (Bound(q2)) trmenv)
	else raise (term_error "alpha_convp_aux" [t1;t2]))
    | (Typed(trm, _), _) -> alpha_aux trm t2 tyenv trmenv
    | (_, Typed(trm, _)) -> alpha_aux t1 trm tyenv trmenv
    | _ -> 
	(if equals t1 t2 then (trmenv, tyenv)
	else raise (term_error "alpha_convp_aux" [t1;t2]))
  in 
  let env = Term.empty_subst() 
  in 
  try 
    let _, ret = alpha_aux t1 t2 tenv env
    in ret
  with _ -> raise (term_error "alpha_convp" [t1; t2])

let alpha_convp scp t1 t2=
  let tyenv=Gtypes.empty_subst()
  in 
  alpha_convp_full scp tyenv t1 t2

(*
let alpha_convp scp t1 t2 =
  let env=empty_subst()
  in 
  try ignore(alpha_convp_aux scp env t1 t2); true
  with _ -> false
*)

let alpha_equals scp t1 t2 =
  try ignore(alpha_convp scp t1 t2); true
  with _ -> false

(* beta reduction *)

let beta_convp  =
  function
      App(f, a) -> is_lambda f
    | _ -> false

(* beta reduction: assuming well-typed expression *)
let beta_conv t =
  match t with
    App(f, a) -> 
      if is_lambda f
      then 
	(let (q, b)=dest_qnt f
	in 
	subst_quick (Bound(q)) a  b)
      else raise (term_error "Can't apply beta-reduction" [t])
  | _ -> raise (term_error "Can't apply beta-reduction" [t])

let beta_reduce t = 
  let rec beta_reduce_aux chng t =
    match t with
      App(f, a) -> 
	(let nf= beta_reduce_aux chng f
	and na = beta_reduce_aux chng a
	in 
	(try (let nt=beta_conv (App(nf, na)) in chng:=true; nt)
	with _ -> (App(nf, na))))
    |	Qnt(q, b) -> Qnt(q, beta_reduce_aux chng b)
    |	Typed(tr, ty) -> Typed(beta_reduce_aux chng tr, ty)
    |	x -> x
  in let flag = ref false
  in let nt = beta_reduce_aux flag t
  in if !flag then nt else raise (Result.error "No change")

(* eta-abstraction *)
(* abstract x, of type ty, from term t *)
let eta_conv x ty t=
  let name="a" 
  in let q= mk_binding Basic.Lambda name ty 
  in App((Qnt(q, subst_quick x (Bound(q)) t)), x)
    


(* closed terms *)

exception TermCheck of term

let rec is_closed_aux env t =
  match t with
    App(l, r) -> 
      is_closed_aux env l;
      is_closed_aux env r
  | Typed(a, _) -> 
      is_closed_aux env a
  | Qnt(q, b) -> 
      let nenv=bind (Bound(q)) (mk_bool true) env
      in 
      is_closed_aux nenv b
  | Bound(q) -> 
    (try ignore(find t env)
    with Not_found -> raise (TermCheck t))
  | _ -> ()

let is_closed_scope env t =
  try is_closed_aux env t; true
  with TermCheck _ -> false

let is_closed t = 
  try is_closed_aux (empty_subst()) t; true
  with TermCheck _ -> false

      
(*
   let close_term t = 
   let qnts = Term.get_free_binders t
   in 
 *)

let close_term t = 
  let memo = empty_table()
  and qnts = ref []
  and mk_univ q = 
    let (_, n, ty) = Basic.dest_binding q
    in Basic.mk_binding Basic.All n ty
  in 
  let rec close_aux x =
    match x with
      Qnt(q, b) ->
	ignore(table_add (Bound q) (Bound q) memo);
	close_aux b
    | Bound(q) ->
	(try ignore(table_find x memo)
	with Not_found ->
	  let nq = mk_univ q
	  in 
	  ignore(table_add x (Bound nq) memo);
	  qnts:=nq::!qnts)
    | Typed(tr, _) -> close_aux tr
    | App(f, a) -> close_aux f; close_aux a
    | _ -> ()
  in 
  close_aux t; 
  List.fold_left 
    (fun b q-> Qnt(q, b)) t !qnts


(**
   [gen_term qnts trm]: generalise term [trm]. Replace bound variables
   occuring outside their binder and free variables with universally
   quantified variables. Binders in [qnts] are ignored.

   belongs in Logicterm
*)
let gen_term bs trm =
  let rec gen_aux qnts known vars t=
  let get_bound t = 
    try 
      (Term.find t known, qnts, known, vars)
    with Not_found -> 
      try 
	(Term.find t vars, qnts, known, vars)
      with _ -> 
	let q = Term.dest_bound t
	in 
	let (_, name, ty) = Basic.dest_binding q
	in 
	let q1 = Basic.mk_binding Basic.All name ty
	in 
	(Basic.Bound(q1), q1::qnts, known,
	 Term.bind t (Basic.Bound(q1)) vars)
  and get_free t = 
    try 
      (Term.find t known, qnts, known, vars)
    with Not_found -> 
      try 
	(Term.find t vars, qnts, known, vars)
      with _ -> 
	let (name, ty) = Term.dest_free t
	in 
	let q = Basic.mk_binding Basic.All name ty
	in 
	(Basic.Bound(q), q::qnts, known, 
	 Term.bind t (Basic.Bound(q)) vars)
  in 
    match t with 
      Basic.Bound _ -> get_bound t
    | Basic.Free _ -> get_free t
    | Basic.Qnt(q, body) -> 
	let qnts1 = qnts
	and known1 = Term.bind (Basic.Bound(q)) (Basic.Bound(q)) known
	and vars1 = vars
	in 
	let (body1, qnts1, known1, vars1) = 
	  gen_aux qnts (Term.bind (Basic.Bound(q)) (Basic.Bound(q)) known)
	    vars body
	in 
	(Basic.Qnt(q, body1), qnts1, known, vars1)
    | Basic.App(f, a) ->
	let (f1, qnts1, known1, vars1) = 
	  gen_aux qnts known vars f
	in 
	let (a1, qnts2, known2, vars2) = 
	  gen_aux qnts1 known1 vars1 a
	in 
	(Basic.App(f1, a1), qnts2, known2, vars2)
    | Basic.Typed(t1, ty) -> 
	let (t2, qnts1, known1, vars1) = 
	  gen_aux qnts known vars t
	in 
	(Basic.Typed(t2, ty), qnts1, known1, vars1)
    | _ -> (t, qnts, known, vars)
  in 
  let (trm1, qnts, _, vars) = 
    gen_aux [] (Term.empty_subst()) 
      (List.fold_left 
	 (fun s q -> Term.bind (Basic.Bound(q)) (Basic.Bound(q)) s)
	 (Term.empty_subst()) bs)
      trm
  in 
  Term.rebuild_qnt qnts trm1
