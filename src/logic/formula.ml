(*-----
 Name: formula.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Basic
exception Error

type form =  Basic.term
type saved_form =  Dbterm.dbterm

exception TermCheck of Basic.term

let term_of x = x
let string_form = Term.string_term

let to_save x =  Dbterm.of_term x
let from_save x = Dbterm.to_term x

let rec check_term p t =
  if (p t) then 
    (match t with
      Basic.Qnt(_, q, b) -> check_term p b
    | Basic.App(f, a) -> check_term p f; check_term p a
    | Basic.Typed(trm, ty) -> check_term p trm
    | _ -> ())
  else raise (Term.term_error "Term check failed" [t])

(*
let dest_form  x =  x
*)

(* Substitution primitives *)

type substitution = Term.substitution
let empty_subst () = Term.empty_subst()

let find x subst = Term.find x subst
let remove x subst = Term.remove x subst
let chase = Term.chase
let fullchase = Term.fullchase
let replace x subst = Term.replace x subst

(* closed terms and conversion to formula *)

let rec is_closed_scope env t =
  match t with
    Basic.App(l, r) -> is_closed_scope env l; is_closed_scope env r
  | Basic.Typed(a, _) -> is_closed_scope env a
  | Basic.Qnt(k, q, b) -> 
      if not((Basic.binder_kind q)=k)
      then raise (Term.term_error "Not closed" [t])
      else 
	(Term.table_add (Basic.Bound(q)) (Term.mk_bool true) env;
	 is_closed_scope env b;
	 Term.table_remove (Basic.Bound(q)) env)
  | Basic.Bound(_) -> 
      (try ignore(Term.table_find t env)
      with Not_found -> 
	raise (Term.term_error  "Not closed"  [t]))
  | Basic.Free(_) -> 
      (try ignore(Term.table_find t env)
      with Not_found -> 
	raise (Term.term_error  "Not closed"  [t]))
  | _ -> ()

let is_closed vs t = 
  let tbl=Term.empty_table()
  in 
  (* add bound terms of [vs] to tbl *)
  List.iter 
    (fun x -> 
      if ((Term.is_bound x) or (Term.is_free x))
      then ignore(Term.table_add x (Term.mk_bool true) tbl)
      else ()) vs;
  try is_closed_scope tbl t; true
  with _ -> false

let in_scope_memo memo scp th f =
  if (Term.in_scope memo scp th (term_of f))
  then true
  else raise (Term.term_error "Badly formed formula" [term_of f])

let in_scope scp th f =
  if (Term.in_scope (Lib.empty_env()) scp th (term_of f))
  then true
  else raise (Term.term_error "Badly formed formula" [term_of f])

let retype tenv x = Term.retype tenv x

(*
   [resolve_closed_term scp trm]: 
   resolve names and types in closed term [trm] in scope [scp].
*)
let binding_set_names memo scp binding =
  let (qnt, qname, qtype) = Basic.dest_binding binding
  in 
  Basic.mk_binding qnt qname 
    (Gtypes.set_name ~strict:true ~memo:memo scp qtype)

let resolve_closed_term scp trm=
  let set_type_name memo s t =
    Gtypes.set_name ~strict:true ~memo:memo s t
  in 
  let true_term = Term.mk_short_var "true"
  and curr_thy = Scope.thy_of scp
  in 
  let id_memo = Lib.empty_env()
  and scope_memo = Lib.empty_env()
  and type_memo = Lib.empty_env()
  and type_thy_memo = Lib.empty_env()
  in 
  let lookup_id n = 
    try 
      Lib.find n id_memo
    with Not_found -> 
      let nth = Scope.thy_of_term scp n
      in (ignore(Lib.add n nth id_memo); nth)
  in 
  let lookup_type id = 
    try 
      Gtypes.copy_type (Lib.find id type_memo)
    with Not_found -> 
      let ty = Scope.type_of scp id
      in (ignore(Lib.add id ty type_memo); ty)
  in 
  let rec set_aux qnts t=
    match t with
      Id(id, ty) -> 
	let th, n = Basic.dest_fnid id
	in 
	let nid = 
	  if(th = Basic.null_thy)
	  then 
	    try 
	      let nth = lookup_id n
	      in 
	      Basic.mk_long nth n
	    with Not_found -> 
	      raise 
		(Term.term_error "Formula.make: term not in scope" [t])
	  else id
	in 
	let nty =  
	  try 
	    lookup_type id
	  with Not_found -> 
	    raise 
	      (Term.term_error 
		 "Formula.make: Can't find type for term" [t])
	in 	
	let ty1=
	  try
	    set_type_name type_thy_memo scp ty
	  with err ->
	    raise (Term.add_term_error "Invalid type" [t] err)
	in 
	let ret_id = Typed(Id(nid, nty), ty1)
	in 
	(if (Term.in_scope scope_memo scp curr_thy ret_id)
	then ret_id
	else 
	  raise 
	    (Term.term_error 
	       "Formula.make: term not in scope" [t]))
    | Free(n, ty) -> 
	(try 
	  let nth = lookup_id n
	  in 
	  let nid = Basic.mk_long nth n
	  in 
	  let nty = 
	    try lookup_type nid
	    with Not_found -> 
	      raise 
		(Term.term_error 
		   "Formula.make: Can't find type for term" [t])
	  in 
	  let ty1=
	    try
	      set_type_name type_thy_memo scp ty
	    with err ->
	      raise (Term.add_term_error "Invalid type" [t] err)
	  in 
	  set_aux qnts (Typed(Id(nid, nty), ty1))
	with Not_found -> 
	  raise 
	    (Term.term_error 
	       "Formula.make: can't resolve free variable" [t]))
    | Qnt(qnt, q, b) -> 
	let nq = binding_set_names type_thy_memo scp q
	in 
	let qnts1 = Term.bind (Bound(q)) (Bound(nq)) qnts
	in 
	Qnt(qnt, nq, set_aux qnts1 b)
    | Typed(tt, tty) -> Typed(set_aux qnts tt, tty)
    | App(f, a) -> App(set_aux qnts f, set_aux qnts a)
    | Bound(q) -> 
	(try
	  (Term.find (Bound(q)) qnts)
	with Not_found -> 
	  raise (Term.term_error 
		   "Bound variable occurs outside binding" [t]))

    | _ -> t
  in set_aux (Term.empty_subst()) trm

let make ?env scp t= 
  let t1=
    try resolve_closed_term scp t
    with x -> raise
	(Result.add_error x
	   (Term.term_error 
	      "Formula.make: Can't make formula, not a closed term" [t]))
  in 
  try
    let tyenv = Lib.apply_option (fun x -> !x) env (Gtypes.empty_subst())
    in 
    let tyenv1 = 
      Typing.typecheck_env scp tyenv t1 (Gtypes.mk_null())
    in 
    Term.retype tyenv1 t1

(*    (Lib.apply_option (fun x -> x:=tyenv1) env ();
    Term.retype_pretty tyenv1 t1) *)
  with x -> 
    raise (Result.add_error x 
	     (Term.term_error "Formula.make: incorrect types" [t1]))

let dest f = f

(* Formula recognisers, constructors and destructors *)

let is_fun= Term.is_fun
let mk_fun = Term.mk_fun
let dest_fun = Term.dest_fun

let is_var  = Term.is_var
let mk_typed_var = Term.mk_typed_var
let mk_var n = mk_typed_var n (Gtypes.mk_null())
let dest_var  =  Term.dest_var 
let get_var_id vt= fst (dest_var vt)
let get_var_type vt= snd (dest_var vt)

let is_app = Term.is_app 
let mk_app f a= (Basic.App(f, a))
let dest_app t = 
  match t with 
    (Basic.App(f, a)) -> (f, a)
  | _ -> raise (Term.term_error "Not an application" [t])

let rec mk_comb x y = 
  match y with 
    [] -> x
  | t::ts -> mk_comb (mk_app x t) ts

let is_const = Term.is_const 
let mk_const = Term.mk_const
let dest_const =  Term.dest_const

let mk_num = Term.mk_num
let dest_num = Term.destnum 

let mk_bool = Term.mk_bool
let dest_bool = Term.destbool 

let is_true x= try dest_bool x with _ -> false

let is_false x = try not (dest_bool x) with _ -> false

let is_fun_name s t =
  try (Term.is_fun t) & (fst(Term.dest_fun t) = s)
  with _ -> false

let is_neg = Logicterm.is_neg 
let mk_neg  = Logicterm.mk_not
let dest_neg f = 
  if is_neg f
  then match dest_fun f with (_, x) -> x
  else raise (Term.term_error "dest_neg" [f])
      
let is_conj = Logicterm.is_conj 
let mk_conj = Logicterm.mk_and
let dest_conj f = 
  if is_conj f
  then match dest_fun f with (_, x) -> x
  else raise (Term.term_error "dest_conj" [f])

let is_disj = Logicterm.is_disj 
let mk_disj = Logicterm.mk_or
let dest_disj f = 
  if is_disj f
  then match dest_fun f with (_, x) -> x
  else raise (Term.term_error "dest_disj" [f])

let mk_implies = Logicterm.mk_implies
let is_implies = Logicterm.is_implies 
let dest_implies f = 
  if is_implies f
  then match dest_fun f with (_, x) -> x
  else raise (Term.term_error "dest_implies" [f])

let mk_equality  = Logicterm.mk_equality
let is_equality = Logicterm.is_equality
let dest_equality f =  
  if is_equality f
  then match dest_fun f with 
    (_, [a; b]) -> (a, b)
  |	_ -> raise (Term.term_error "dest_equality" [f])
  else raise (Term.term_error "dest_equality" [f])

(*
   if is_equals f
   then match dest_fun f with (_, x) -> x
   else raise (Failure "dest_equals")
 *)
let get_binder_name = Term.get_binder_name 
let get_binder_type = Term.get_binder_type 

let dest_qnt f = 
  let (q, _, _, _, b) = Term.dest_qnt f
  in (q, b)

let is_all = Logicterm.is_all 
let mk_all scp x f = 
  make scp (Logicterm.mk_all scp x f)
let mk_typed_all scp x ty f= 
  make scp (Logicterm.mk_all_ty scp x ty f)

let is_exists = Logicterm.is_exists 
let mk_exists = Logicterm.mk_ex
let mk_typed_exists = Logicterm.mk_ex_ty 

let is_qnt t = (is_all t) or (is_exists t)

let is_lambda = Logicterm.is_lambda
let mk_lambda = Logicterm.mk_lam
let mk_typed_lambda = Logicterm.mk_lam_ty

(* Typecheck and reset types of formula *)

let typecheck_env scp tenv f expty = 
  let t = term_of f
  in 
  Typing.typecheck_env scp (Gtypes.empty_subst()) t expty

let typecheck scp f expty= 
  let tyenv = typecheck_env scp (Gtypes.empty_subst()) f expty
  in 
  Term.retype_pretty tyenv f 

let simple_typecheck scp f expty= 
  ignore(typecheck_env scp (Gtypes.empty_subst()) f expty)

let mk_iff = Logicterm.mk_iff

let unify scp asmf conclf =
  let asm = term_of asmf
  and concl = term_of conclf
  in 
  let (avars, abody)=Term.strip_qnt Basic.All asm
  and (cvars, cbody)= Term.strip_qnt Basic.Ex concl
  in let varp x = 
    (match x with 
      (Basic.Bound q) -> (List.memq q avars) or (List.memq q cvars)
    | _ -> false)
  in (Unify.unify scp varp abody cbody)

let unify_env scp tyenv asmf conclf =
  let asm = term_of asmf
  and concl = term_of conclf
  in 
  let (avars, abody)=Term.strip_qnt Basic.All asm
  and (cvars, cbody)= Term.strip_qnt Basic.Ex concl
  in let varp x = 
    (match x with 
      (Basic.Bound q) -> (List.memq q avars) or (List.memq q cvars)
    | _ -> false)
  in 
  Unify.unify_fullenv scp tyenv (Term.empty_subst()) varp abody cbody


(* manipulation *)

(* substitution: term substitution followed by check for closed term *)

let subst scp env t = 
  let nt = Term.subst env (term_of t)
  in 
  make scp nt

let rename t = Term.rename t

(*
let inst_env scp vs env t r =
  if (Term.is_qnt t) 
  then 
    if (is_closed vs r)
    then 
      (let (q, qnt, n, ty, b) = Term.dest_qnt (term_of t)
      in 
      let nr0 = Typing.assign_types scp r
      in 
      let nenv = Typing.simple_typecheck_env scp env nr0 ty
      in 
      let nr= Term.subst_quick (Basic.Bound(q)) nr0 b
      in 
      let f =Term.retype nenv nr
      in 
      (f, nenv))
    else raise (Term.term_error "inst: replacement not closed " [r])
  else raise (Term.term_error "inst: not a quantified formula" [t])
*)

let inst_env scp vs env t r =
  if (Term.is_qnt t) 
  then 
    try
      (let (q, qnt, n, ty, b) = Term.dest_qnt (term_of t)
      in 
      let nr0, nenv=
	let penv = ref env
	in 
	let r1=make ~env:penv scp r
	in 
	(r1, !penv)
      in 
      let nr= Term.subst_quick (Basic.Bound(q)) nr0 b
      in 
      let f =Term.retype nenv nr
      in 
      (f, nenv))
    with err -> 
      raise
	(Term.add_term_error "inst: replacement not closed " [r] err)
  else raise (Term.term_error "inst: not a quantified formula" [t])

let inst scp vs t r =
  let f, _ = inst_env scp vs (Gtypes.empty_subst()) t r
  in f


let equals = Term.equals

(* let alpha_equals_match = Logicterm.alpha_convp_full*)
let alpha_equals_match scp tyenv asmf conclf= 
  let asm = term_of asmf
  and concl = term_of conclf
  and varp x = false
  in 
  let ret, _ = 
    Unify.unify_fullenv scp tyenv (Term.empty_subst()) varp asm concl
  in ret

let alpha_equals = Logicterm.alpha_equals

let beta_convp = Logicterm.beta_convp
let beta_conv scp x =  make scp (Logicterm.beta_conv x)
let beta_reduce scp x = make scp (Logicterm.beta_reduce x)

let eta_conv scp f ty x = 
  make scp (Logicterm.eta_conv f ty x)

(* Rewriting: normal rewrite followed by check for close_term *)

let default_rr_control= Rewrite.default_control

let rewrite scp ?ctrl rrs t = 
  let c = Lib.get_option ctrl default_rr_control
  in 
  let nt = Rewrite.rewrite scp c rrs t
  in 
  make scp nt

let rewrite_env scp ?ctrl tyenv rrs t = 
  let c = Lib.get_option ctrl default_rr_control
  in 
  let nt, ntyenv = 
    Rewrite.rewrite_env scp c tyenv rrs t
  in 
  (make scp nt, ntyenv)


let print inf x = Term.print inf (term_of x)


