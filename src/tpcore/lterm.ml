(*----
 Name: lterm.ml
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

open Lib
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
      
let typeof_cnst c =
  match c with
    Cnum _ -> mk_num_ty ()
  | Cbool _ -> mk_bool_ty ()

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
let mk_any=Term.mk_ident anyid

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
  else raise (Report.error "Not an equality")

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
	qsubst [(Bound(q), a)]  b)
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
      | Bound(q) -> 
	  (try (Term.find t env, true)
	   with Not_found -> (t, false))
      | x -> (x, false)
  in
  let (nt, chng) = beta_aux trm (Term.empty_subst())
  in 
    if chng then nt 
    else
      raise (Report.error "beta_reduce: No change")

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
      | Bound(q) -> 
	  (try (Term.find t env, true)
	   with Not_found -> (t, false))
      | x -> (x, false)
  in
  let (nt, chng) = beta_aux trm (Term.empty_subst())
  in 
    if chng then nt 
    else
      raise (Report.error "beta_reduce: No change")



(*** Eta-abstraction ***)

(***
let eta_conv x ty t=
  let name="a" 
  in let q= mk_binding Basic.Lambda name ty 
  in App((Qnt(q, qsubst [x, Bound(q)] t)), x)
***)
    
let eta_conv ts term=
  let rec eta_aux ctr xs (rslt, env) =
   match xs with 
       [] -> (rslt, env)
     | (x::xss) -> 
	 let name = Lib.int_to_name ctr
	 in 
	 let ty = Gtypes.mk_var (name^"_ty")
	 in 
	 let binder = Basic.mk_binding Basic.Lambda name ty
	 in 
	 let nv = Term.mk_bound binder
	 in 
	 let env1 = Term.bind x nv env
	 in 
	   eta_aux (ctr+1) xss ((x, nv)::rslt, env1)
  in 
  let (nvars, env) = eta_aux 0 (List.rev ts) ([], Term.empty_subst())
  in 
  let body = Term.subst env term
  in 
  let fterm = 
    List.fold_left
      (fun trm (_, v) -> Term.mk_qnt (Term.dest_bound v) trm) 
      body nvars
  in 
  let rterm = 
    List.fold_left 
      (fun trm (a, _) -> Term.mk_app trm a) 
      fterm (List.rev nvars)
  in 
    rterm


(*** Closed terms ***)

(***
* Closed terms
***)

let rec is_closed_env env t =
  match t with
    Basic.App(l, r) -> 
      (is_closed_env env l && is_closed_env env r)
  | Basic.Qnt(q, b) -> 
      let env1 = bind (Basic.Bound(q)) (mk_free "" (Gtypes.mk_null())) env
      in 
      is_closed_env env1 b
  | Basic.Meta(_) -> true
  | Basic.Bound(_) -> member t env
  | Basic.Free(_) -> member t env
  | _ -> true

let is_closed vs t = 
  (* add bound terms of [vs] to tbl *)
  let env = 
    List.fold_left 
      (fun env x -> 
	if ((is_bound x) or (is_free x))
	then bind x (mk_free "" (Gtypes.mk_null())) env
	else env) (empty_subst()) vs
  in 
  try is_closed_env env t
  with _ -> false


(**
   [close_term qnt free trm]: Close term [trm]. Make variables bound
   to quantifiers of kind [qnt] to replace free variables and bound
   variables with no binding quantifier and for which [free] is true.
 *)
let ct_free _ = true

let close_term ?(qnt=Basic.All) ?(free= ct_free) trm=
  let rec close_aux env vs t=
    match t with
      Basic.Id _ -> (t, env, vs)
    | Basic.Meta _ -> (t, env, vs)
    | Basic.Bound(_) -> 
	if(member t env)
	then (t, env, vs)
	else 
	  if(free t) 
	  then (t, env, t::vs) 
	  else (t, env, vs)
    | Basic.Free(_) -> (t, env, t::vs)
    | Basic.App(f, a) ->
	let f1, env1, vs1 = close_aux env vs f
	in 
	let a1, env2, vs2= close_aux env1 vs1 a
	in 
	(Basic.App(f1, a1), env2, vs2)
    | Basic.Qnt(q, b) ->
	let (b1, env1, vs1) = 
	  close_aux (bind (Basic.Bound(q)) (Basic.Bound(q)) env) vs b
	in (Basic.Qnt(q, b1), env, vs1)
    | Basic.Const _ -> (t, env, vs)
  in 
  let (nt, env, vars) =  close_aux (empty_subst()) [] trm
  in 
  let make_qnts qnt (env, ctr, bs) t =
    let qname = Lib.int_to_name ctr
    in 
    let qty = Gtypes.mk_var qname
    in 
    let qbind = Basic.mk_binding qnt qname qty
    in 
    let qtrm = mk_bound qbind
    in 
    (bind t qtrm env, ctr+1, qbind::bs)
  in 
  let sb, _, binders = 
    List.fold_left (make_qnts qnt) (empty_subst(), 0, []) vars
  in 
  rebuild_qnt (List.rev binders) (subst sb trm)



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
      Basic.Bound _ -> get_bound t
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


(***
* Resolving names 
***)

(**
 [in_scope]: Check that term is in scope.
*)
let in_scope memo scp trm =
  let lookup_id n = 
    (try (Lib.find n memo)
    with Not_found -> 
      if (Scope.in_scope scp n) 
      then Lib.add n true memo else raise Not_found)
  in
  let rec in_scp_aux t =
    match t with
      Id(id, ty) -> 
	ignore(lookup_id (Ident.thy_of id));
	Gtypes.in_scope memo scp ty
    | Qnt(_, b) ->
	ignore(Gtypes.in_scope memo scp (get_binder_type t));
	in_scp_aux b
    | Bound(_) ->
	Gtypes.in_scope memo scp (get_binder_type t)
    | Meta(q) -> 
	if Scope.is_meta scp q 
	then true
	else raise Not_found
    | App(a, b) ->
	ignore(in_scp_aux a);
	in_scp_aux b
    | Free(_) -> raise Not_found
    | _ -> true
  in 
  try ignore(in_scp_aux trm); true
  with Not_found -> false


(**
   [binding_set_names_types ?memo scp binding]
   Find and set names for types in a binding.
*)
let binding_set_names ?(strict=false) ?memo scp binding =
  let (qnt, qname, qtype) = Basic.dest_binding binding
  in 
  Basic.mk_binding qnt qname 
    (Gtypes.set_name ~strict:false ?memo:memo scp qtype)

(**
   [set_names scp thy trm]
   find and set long identifiers and types for variables in [trm]
   theory is [thy] if no long identifier can be found in scope [scp]
*)
let set_names scp trm=
  let set_type_name memo s t =
    Gtypes.set_name ~strict:false ~memo:memo s t
  in 
  let id_memo = Lib.empty_env()
  and type_memo = Lib.empty_env()
  and type_thy_memo = Lib.empty_env()
  in 
  let lookup_id n = 
    try Lib.find n id_memo
    with Not_found -> 
      let nth = Scope.thy_of_term scp n
      in 
	(ignore(Lib.add n nth id_memo); nth)
  in 
  let lookup_type id = 
    try 
      Gtypes.rename_type_vars (Lib.find id type_memo)
    with Not_found -> 
      let nty = 
	try Scope.type_of scp id 
	with Not_found -> Gtypes.mk_null()
      in 
      (ignore(Lib.add id nty type_memo); nty)
  in 
  let unify_types ty1 ty2 = 
    try 
      let env = Gtypes.unify scp ty1 ty2
      in 
	Gtypes.mgu ty1 env
    with _ -> ty1
  in
  let rec set_aux qnts t=
    match t with
      Id(id, ty) -> 
	let th, n = Ident.dest id
	in 
	let nid = 
	  if(th = Ident.null_thy)
	  then 
	    let nth = lookup_id n
	    in 
	    Ident.mk_long nth n
	  else id
	in 
	let ty1= set_type_name type_thy_memo scp ty
	in 
	let nty =  
	  (try Some(lookup_type id)
	  with Not_found -> None)
	in 	
	let ret_id = 
	  match nty with
	    None -> Id(nid, ty1)
	  | Some(xty) -> Id(nid, unify_types xty ty1)
	in 
	ret_id
    | Free(n, ty) -> 
	(let ty1= set_type_name type_thy_memo scp ty
	 in 
	   match try_find (Scope.find_meta scp) n with
	       None -> 
		 (match try_find lookup_id n with
		      None -> Free(n, ty1)
		    | Some (nth1) ->
			let nid = Ident.mk_long nth1 n
			in 
			  match try_find lookup_type nid with
			      None -> Id(nid, ty1)
			    | Some(xty) -> Id(nid, unify_types xty ty1))
	     | Some(q) -> Meta(q))
    | Qnt(q, b) -> 
	let nq = binding_set_names ~memo:type_thy_memo scp q
	in 
	let qnts1 = bind (Bound(q)) (Bound(nq)) qnts
	in 
	Qnt(nq, set_aux qnts1 b)
    | App(f, a) -> App(set_aux qnts f, set_aux qnts a)
    | Meta(q) -> 
	if(Scope.is_meta scp q) 
	then t 
	else 
	  (raise 
	     (term_error "Meta variable occurs outside binding" [t]))
    | Bound(q) -> 
	(match Lib.try_find (find (Bound(q))) qnts with
	    Some(x) -> x
	  | None -> 
	      (raise 
		 (term_error "Bound variable occurs outside binding" [t])))
    | _ -> t
  in 
    set_aux (empty_subst()) trm

(**
   [resolve_terms scp trmlist]: resolve names and types in each term
   in the list [trmlist], in scope [scp]. The terms in the list are
   resolved as if they were all parts of the same term.
*)
let resolve_term scp vars varlist trm =
  let id_memo = Lib.empty_env()
  and scope_memo = Lib.empty_env() 
  and type_memo = Lib.empty_env()
  and type_thy_memo = Lib.empty_env()
  in 
  let lookup_id scp ident ty =
    let lookup_name n = 
      try Lib.find n id_memo
      with Not_found -> 
	let nth = Scope.thy_of_term scp n
	in 
	  (ignore(Lib.add n nth id_memo); nth)
    in 
    let lookup_type id = 
      try Gtypes.rename_type_vars (Lib.find id type_memo)
      with Not_found -> 
	let ty = 
	  try Scope.type_of scp id
	  with Not_found -> Gtypes.mk_null()
	in (ignore(Lib.add id ty type_memo); ty)
    in 
    let th, name = Ident.dest ident
    in 
    let nid = 
      if(th = Ident.null_thy)
      then Ident.mk_long (lookup_name name) name
      else ident
    in 
    let ty0 = lookup_type ident
    in 	
    let ntyenv = Gtypes.unify scp ty0 ty
    in 
    let nty = Gtypes.mgu ty0 ntyenv
    in 
      Id(nid, nty)
  in 
  let set_type_name memo s t =
    Gtypes.set_name ~strict:true ~memo:memo s t
  in 
  let lookup_var vars t =
    match Lib.try_find (find t) vars with
	Some(x) -> (x, vars)
      | None -> 
	  let nt = 
	    (match t with
		Free(n, ty) -> mk_bound (mk_binding All n ty)
	      | Bound(q) -> 
		  mk_bound (mk_binding All (binder_name q) (binder_type q))
	      | _ -> 
		  mk_bound (mk_binding All "x" (Gtypes.mk_var "ty")))
	  in 
	    (nt, bind t nt vars)
  in 
  let rec set_aux (qnts, vars) t lst=
    match t with
      Id(id, ty) -> 
	let ty1=
	  try set_type_name type_thy_memo scp ty
	  with err -> raise (add_term_error "Invalid type" [t] err)
	in 
	let ret_id = 
	  match (Lib.try_find (lookup_id scp id) ty1) with
	      None -> raise (term_error "Term not in scope" [t])
(*	      None -> Free(Ident.name_of id, ty1) *)
	    | Some x -> x
	in 
	(if (in_scope scope_memo scp ret_id)
	then (ret_id, vars, lst)
	else raise (term_error "Term not in scope" [t]))
    | Free(n, ty) -> 
	(match Lib.try_find (Scope.find_meta scp) n with
	     Some(b) -> (Meta(b), vars, lst)
	   | None -> 
	       set_aux (qnts, vars) (Id (Ident.mk_name n, ty)) lst)
    | Qnt(q, b) -> 
	let nq = binding_set_names ~memo:type_thy_memo scp q
	in 
	let qnts1 = bind (Bound(q)) (Bound(nq)) qnts
	in 
	let (nb, nvars, nlst) = set_aux (qnts1, vars) b lst
	in 
	(Qnt(nq, nb), nvars, nlst)
    | App(f, a) -> 
	let nf, fvars, flst = set_aux (qnts, vars) f lst
	in 
	let (na, avars, alst) = set_aux (qnts, fvars) a flst
	in 
	  (App(nf, na), avars, alst)
    | Meta(q) -> 
	if (Scope.is_meta scp q) 
	then (t, vars, lst)
	else 
	  (let nt, nvars = lookup_var vars t
	  in (nt, nvars, ((nt, t)::lst)))
    | Bound(q) -> 
	(match Lib.try_find (find (Bound(q))) qnts with
	    Some x -> (x, vars, lst)
	  | None ->
	      let nt, nvars = lookup_var vars t
	      in 
		(nt, nvars, ((nt, t)::lst)))
    | _ -> (t, vars, lst)
  in 
    set_aux (empty_subst(), vars) trm varlist

(**
   [resolve scp trm]: resolve names and types in term [trm] in [lst]
   in scope [scp].
*)
let resolve scp trm = 
  let (ntrm, vars, lst) = resolve_term scp (empty_subst()) [] trm
  in 
    (ntrm, lst)

(***
let resolve scp trm=
  let id_memo = Lib.empty_env()
  and scope_memo = Lib.empty_env() 
  and type_memo = Lib.empty_env()
  and type_thy_memo = Lib.empty_env()
  in 
  let lookup_id scp ident ty =
    let lookup_name n = 
      try Lib.find n id_memo
      with Not_found -> 
	let nth = Scope.thy_of_term scp n
	in 
	  (ignore(Lib.add n nth id_memo); nth)
    in 
    let lookup_type id = 
      try Gtypes.rename_type_vars (Lib.find id type_memo)
      with Not_found -> 
	let ty = Scope.type_of scp id
	in (ignore(Lib.add id ty type_memo); ty)
    in 
    let th, name = Ident.dest ident
    in 
    let nid = 
      if(th = Ident.null_thy)
      then Ident.mk_long (lookup_name name) name
      else ident
    in 
    let ty0 = lookup_type ident
    in 	
    let ntyenv = Gtypes.unify scp ty0 ty
    in 
    let nty = Gtypes.mgu ty0 ntyenv
    in 
      Id(nid, nty)
  in 
  let set_type_name memo s t =
    Gtypes.set_name ~strict:true ~memo:memo s t
  in 
  let lookup_var vars t =
    match Lib.try_find (find t) vars with
	Some(x) -> (x, vars)
      | None -> 
	  let nt = 
	    (match t with
		Free(n, ty) -> mk_bound (mk_binding All n ty)
	      | Bound(q) -> 
		  mk_bound (mk_binding All (binder_name q) (binder_type q))
	      | _ -> 
		  mk_bound (mk_binding All "x" (Gtypes.mk_var "ty")))
	  in 
	    (nt, bind t nt vars)
  in 
  let rec set_aux (qnts, vars) t lst=
    match t with
      Id(id, ty) -> 
	let ty1=
	  try set_type_name type_thy_memo scp ty
	  with err -> raise (add_term_error "Invalid type" [t] err)
	in 
	let ret_id = lookup_id scp id ty1
	in 
	(if (in_scope scope_memo scp ret_id)
	then (ret_id, vars, lst)
	else raise (term_error "Term not in scope" [t]))
    | Free(n, ty) -> 
	(match Lib.try_find (Scope.find_meta scp) n with
	     Some(b) -> (Meta(b), vars, lst)
	   | None -> 
	       set_aux (qnts, vars) (Id (Ident.mk_name n, ty)) lst)
    | Qnt(q, b) -> 
	let nq = binding_set_names ~memo:type_thy_memo scp q
	in 
	let qnts1 = bind (Bound(q)) (Bound(nq)) qnts
	in 
	let (nb, nvars, nlst) = set_aux (qnts1, vars) b lst
	in 
	(Qnt(nq, nb), nvars, nlst)
    | App(f, a) -> 
	let nf, fvars, flst = set_aux (qnts, vars) f lst
	in 
	let (na, avars, alst) = set_aux (qnts, fvars) a flst
	in 
	  (App(nf, na), avars, alst)
    | Meta(q) -> 
	if (Scope.is_meta scp q) 
	then (t, vars, lst)
	else 
	  (let nt, nvars = lookup_var vars t
	  in (nt, nvars, ((nt, t)::lst)))
    | Bound(q) -> 
	(match Lib.try_find (find (Bound(q))) qnts with
	    Some x -> (x, vars, lst)
	  | None ->
	      let nt, nvars = lookup_var vars t
	      in 
		(nt, nvars, ((nt, t)::lst)))
    | _ -> (t, vars, lst)
  in 
  let (nt, nvars, nlst) =
    set_aux (empty_subst(), empty_subst()) trm []
  in 
    (nt, nlst)
***)


(***
* Substitution
***)

let rec subst_closed qntenv sb trm =
  try 
    let nt = replace sb trm 
    in 
    if (is_closed_env qntenv nt)
    then subst_closed qntenv sb nt
    else raise (Failure "subst_closed: Not closed")
  with Not_found ->
    (match trm with
      Qnt(q, b) -> 
	let qntenv1 = bind (Bound q) (mk_free "" (Gtypes.mk_null())) qntenv
	in 
	Qnt(q, subst_closed qntenv1 sb b)
    | App(f, a) -> 
	App(subst_closed qntenv sb f, subst_closed qntenv sb a)
    | _ -> trm)

let subst_equiv scp term lst = 
  let repl t ls = 
    Lib.try_app (Lib.assocp (alpha_equals scp t)) ls
  in 
  let rec subst_aux qntenv trm = 
    match (repl trm lst) with
      Some(x) -> 
	if (is_closed_env qntenv x)
	then x
	else raise (Failure "subst_equiv: Not closed")
    | None -> 
	(match trm with
	  Qnt(q, b) ->  
	    let qntenv1=Term.bind (Bound q) (Term.mk_short_ident "") qntenv
	    in 
	    Qnt(q, subst_aux qntenv1 b)
	| App(f, a) -> App(subst_aux qntenv f, subst_aux qntenv a)
	| _ -> trm)
  in 
  subst_aux (Term.empty_subst()) term
