(*-----
 Name: logicterm.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Basic
open Gtypes
open Term

(*** 
* Theories
***)

let base_thy = "base"
let nums_thy = "nums"

(***
* Types
***)

(*** Identifiers for base types ***)

let bool_ty_id = Ident.mk_long base_thy "bool"
let fun_ty_id = Ident.mk_long base_thy "FUN"
let ind_ty_id = Ident.mk_long base_thy "ind"
let num_ty_id = Ident.mk_long nums_thy "num"

(** The type ind *)

let ind_ty = Gtypes.mk_constr ind_ty_id []
let mk_ind_ty () = ind_ty
let is_ind_ty t = 
  match t with
    Constr(x, []) -> (x = ind_ty_id)
  | _ -> false

(** The type nums.num *)

let num_ty = Gtypes.mk_constr num_ty_id []
let mk_num_ty () = num_ty
let is_num_ty t = 
  match t with
    Constr(x, []) -> (x = num_ty_id)
  | _ -> false

(*** Type bool ***)

let bool_ty = Gtypes.mk_constr bool_ty_id []
let mk_bool_ty () = bool_ty
let is_bool_ty t = 
  match t with
    Constr(x, []) -> (x = bool_ty_id)
  | _ -> false

(*** Type of functions ***)

let mk_fun_ty l r = Gtypes.mk_constr fun_ty_id [l; r]

let is_fun_ty t = 
  match t with
    Constr (x, _) -> x=fun_ty_id
  | _ -> false

let rec mk_fun_ty_from_list l r = 
  match l with
    [] -> raise (Failure "No argument types")
  | [t] -> mk_fun_ty t r
  | t::ts -> mk_fun_ty t (mk_fun_ty_from_list ts r)

let dest_fun_ty t = 
  if(is_fun_ty t)
  then 
    match t with
      Constr(_, [a1; a2]) -> (a1, a2)
    | _ -> raise (Failure "Not function type")
  else raise (Failure "Not function type")
      
(***
* Terms 
***)

(*** Identifiers for logic functions and constants *)

let trueid = Ident.mk_long base_thy "true"
let falseid = Ident.mk_long base_thy "false"
let notid = Ident.mk_long base_thy "not"
let andid = Ident.mk_long base_thy "and"
let orid = Ident.mk_long base_thy "or"
let impliesid = Ident.mk_long base_thy "implies"
let iffid = Ident.mk_long base_thy "iff"
let equalsid = Ident.mk_long base_thy "equals"
let equalssym = "="
let anyid = Ident.mk_long base_thy "any"

(*** Recognisers ***)

let is_true t = 
  match t with 
    (Const (Cbool true)) -> true 
  | _ -> false

let is_false t = 
  match t with 
    (Const (Cbool false)) -> true 
  | _ -> false

let is_neg t = try(fst(dest_fun t) = notid) with _ -> false
let is_conj t = try(fst(dest_fun t) = andid) with _ -> false
let is_disj t = try( fst(dest_fun t) = orid) with _ -> false
let is_implies t = try (fst(dest_fun t) = impliesid) with _ -> false
let is_equality t = try (fst(dest_fun t) = equalsid) with _ -> false

(*** Constructors ***)

let mk_true = mk_const(Cbool true)
let mk_false = mk_const(Cbool false)
let mk_bool b = 
  if b then mk_true else mk_false

let mk_not t = mk_fun notid [t]
let mk_and l r = mk_fun andid [l; r]
let mk_or l r = mk_fun orid [l; r]
let mk_implies l r = mk_fun impliesid [l; r]
let mk_iff l r = mk_fun iffid [l; r]
let mk_equality l r = mk_fun equalsid [l; r]
let mk_any=Term.mk_var anyid

(*** Destructors ***)

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

(*** Quantified terms *)

let is_all t = 
  ((is_qnt t) &
   (match (get_binder_kind t) with Basic.All -> true | _ -> false))

let is_exists t = 
  (is_qnt t) &
  (match (get_binder_kind t) with Basic.Ex -> true | _ -> false)

let is_lambda t = 
  ((is_qnt t) &
   (match (get_binder_kind t) with Basic.Lambda -> true | _ -> false))

let mk_all tyenv n b= mk_qnt_name tyenv Basic.All n b
let mk_all_ty tyenv n ty b= mk_typed_qnt_name tyenv Basic.All ty n b

let mk_ex tyenv n b= mk_qnt_name tyenv Basic.Ex n b
let mk_ex_ty tyenv n ty b= mk_typed_qnt_name tyenv Basic.Ex ty n b

let mk_lam tyenv n b= mk_qnt_name tyenv Basic.Lambda n b
let mk_lam_ty tyenv n ty b= mk_typed_qnt_name tyenv Basic.Lambda ty n b

(***
* Lambda Conversions
***)

(*** Alpha conversion ***)

let alpha_convp_full scp tenv t1 t2 =
(*
  let type_matches scp env x y = Gtypes.matches_env scp env x y
  in 
*)
  let type_matches scp env x y = Gtypes.unify_env scp x y env
  in 
  let rec alpha_aux t1 t2 tyenv trmenv =
    match (t1, t2) with
      (Id(n1, ty1), Id(n2, ty2)) -> 
	if (n1=n2) 
	then (trmenv, type_matches scp tyenv ty1 ty2)
	else raise (term_error "alpha_convp_aux" [t1;t2])
    | (Bound(q1), Bound(q2)) ->
	let q1trm= try Term.find t1 trmenv with Not_found -> t1
	and q2trm = try Term.find t2 trmenv with Not_found -> t2
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
	  let tyenv1=type_matches scp tyenv qty1 qty2
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

let alpha_equals scp t1 t2 =
  try ignore(alpha_convp scp t1 t2); true
  with _ -> false

let subst_equiv scp term lst = 
  let repl t ls = 
    Lib.try_app (Lib.assocp (alpha_equals scp t)) ls
  in 
  let rec subst_aux qntenv trm = 
    match (repl trm lst) with
      Some(x) -> 
	if (Term.is_closed_env qntenv x)
	then x
	else raise (Failure "subst_equiv: Not closed")
    | None -> 
	(match trm with
	  Qnt(q, b) ->  
	    let qntenv1=Term.bind (Bound q) (Term.mk_short_var "") qntenv
	    in 
	    Qnt(q, subst_aux qntenv1 b)
	| App(f, a) -> App(subst_aux qntenv f, subst_aux qntenv a)
	| Typed(t, ty) -> Typed(subst_aux qntenv t, ty)
	| _ -> trm)
  in 
  subst_aux (Term.empty_subst()) term

(*** Beta conversion ***)

let beta_convp  =
  function
      App(f, a) -> is_lambda f
    | _ -> false

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


let safe_beta_reduce trm =
  let rec beta_aux t env =
    match t with
	App(f, a) -> 
	  let (na, achng) = beta_aux a env
	  in 
	    if is_lambda f 
	    then
	      let (q, b) = dest_qnt f
	      in 
	      let env1 = bind (Bound q) na env
	      in 
	      let (nb, _) = beta_aux b env1
	      in 
		(nb, true)
	    else
	      let (nf, fchng) = beta_aux f env
	      in 
		if fchng && (is_lambda nf)
		then 
		  beta_aux (App(nf, na)) env
		else 
		  (App(nf, na), achng || fchng)
      | Qnt(q, b) -> 
	  let nb, chng = beta_aux b env
	  in 
	    (Qnt(q, nb), chng)
      | Typed(tr, ty) -> 
	  let ntr, chng = beta_aux tr env
	  in 
	    (Typed(ntr, ty), chng)
      | Bound(q) -> 
	  (try (Term.find t env, true)
	   with Not_found -> (t, false))
      | x -> (x, false)
  in
  let (nt, chng) = beta_aux trm (Term.empty_subst())
  in 
    if chng then nt 
    else
      raise (Result.error "beta_reduce: No change")

let beta_reduce trm =
  let rebuild_app t l = mk_comb t l
  in 
  let rec beta_app t env args = 
    match t with
	App(f, a) ->
	  let (na, achng) = beta_aux a env
	  in 
	  let nt, tchng = 
	    beta_app f env (na::args)
	  in
	    (nt, achng || tchng)
      | Qnt(q, b) -> 
	  if (not (args = []) && (is_lambda t))
	  then 
	    let na, nargs = List.hd args, List.tl args
	    in 
	    let env1 = bind (Bound q) na env
	    in 
	    let (nb, _) = beta_app b env1 nargs
	    in 
	      (nb, true)
	  else 
	    let nt, tchng = beta_aux t env
	    in 
	      (rebuild_app nt args, tchng)
      | _ -> 
	  let nt, tchng = beta_aux t env
	  in 
	    (rebuild_app nt args, tchng)
  and beta_aux t env =
    match t with
	App(f, a) -> 
	  let (na, achng) = beta_aux a env
	  in 
	  let nt, tchng = 
	    beta_app f env [na]
	  in
	    (nt, achng || tchng)
      | Qnt(q, b) -> 
	  let nb, chng = beta_aux b env 
	  in 
	    (Qnt(q, nb), chng)
      | Typed(tr, ty) -> 
	  let ntr, chng = beta_aux tr env 
	  in 
	    (Typed(ntr, ty), chng)
      | Bound(q) -> 
	  (try (Term.find t env, true)
	   with Not_found -> (t, false))
      | x -> (x, false)
  in
  let (nt, chng) = beta_aux trm (Term.empty_subst())
  in 
    if chng then nt 
    else
      raise (Result.error "beta_reduce: No change")


(*** Eta-abstraction ***)

let eta_conv x ty t=
  let name="a" 
  in let q= mk_binding Basic.Lambda name ty 
  in App((Qnt(q, subst_quick x (Bound(q)) t)), x)
    
(***
* Utility functions
***)

let typeof_cnst c =
  match c with
    Cnum _ -> mk_num_ty ()
  | Cbool _ -> mk_bool_ty ()

(*** closed terms ***)

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
      if(is_meta t) then () 
      else (try ignore(find t env)
	    with Not_found -> raise (TermCheck t))
  | _ -> ()

let is_closed_scope env t =
  try is_closed_aux env t; true
  with TermCheck _ -> false

let is_closed t = 
  try is_closed_aux (empty_subst()) t; true
  with TermCheck _ -> false
      
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
	if is_meta x then () 
	else 
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

(*** Generalising terms ***)

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
      Basic.Bound _ -> 
	if is_meta t 
	then (t, qnts, known, vars)
	else get_bound t
    | Basic.Free _ -> get_free t
    | Basic.Qnt(q, body) -> 
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
