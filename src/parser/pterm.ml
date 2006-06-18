(*-----
 Name: pterm.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2006
----*)


(***
* Parsed terms 
***)

open Basic

(** The representation of a parsed term *)
type t =
    PId of Ident.t* gtype   (** Identifiers *)
  | PBound of binders     (** Bound variables *)
  | PFree of string * gtype  (** Free variables *)
  | PMeta of binders       (** Meta variables (use for skolem constants) *)
  | PApp of t * t    (** Function application *)
  | PQnt of binders * t (** Binding terms *)
  | PConst of const_ty     (** Constants *)
  | PTyped of t * gtype  (** Typed terms *)

(***
* Operations on terms
***)

(*** Recognisers ***)

let is_qnt x = 
  match x with 
    PQnt _ -> true
  | _ -> false

let is_app x = 
  match x with 
    PApp _ -> true
  | _ -> false

let is_bound x = 
  match x with 
    PBound _ -> true
  | _ -> false

let is_free x = 
  match x with 
    PFree _ -> true
  | _ -> false

let is_ident x = 
  match x with 
    PId _ -> true
  | _ -> false

let is_typed x = 
  match x with 
    PTyped (_, _) -> true
  | _ -> false

let is_const x = 
  match x with 
    PConst _ -> true
  | _ -> false

let is_true t = 
  match t with 
    (PConst (Cbool true)) -> true 
  | _ -> false

(* Constructors *)

let mk_qnt b t= PQnt(b, t)
let mk_bound n = PBound n
let mk_free n ty= PFree(n, ty)
let mk_app f a= PApp(f, a)
let mk_typed t ty= PTyped(t, ty)
let mk_const c = PConst c

let mk_typed_ident n t= PId(n, t)
let mk_ident n = mk_typed_ident n (Gtypes.mk_null ())
let mk_short_ident n = mk_free n (Gtypes.mk_null())

(* Destructors *)

let dest_qnt t=
  match t with 
    (PQnt(q, b)) -> (q, b)
  | _  -> raise (Failure "Not a quantifier")

let dest_bound t = 
  match t with 
    PBound(n) -> n
  | _ -> raise (Failure "dest_bound: Not a binder")

let dest_free t = 
  match t with 
    PFree(n, ty) -> (n, ty)
  | _ -> raise (Failure "Not a free variable")

let dest_ident vt =
  match vt with
    (PId (n, t)) -> (n, t)
  | _ -> raise (Failure "Not a variable")

let dest_app t = 
  match t with 
    (PApp(f, a)) -> (f, a)
  | _ -> raise (Failure "Not a function")

let dest_typed t = 
  match t with 
    PTyped(trm, ty) -> (trm, ty)
  | _ -> raise (Failure "Not a typed term")

let dest_const t =
  match t with
    PConst c -> c
  | _ -> raise (Failure "Not a constant")

(* Specialised Manipulators *)

let is_meta trm = 
  match trm with
    PMeta _ -> true
  | _ -> false

let mk_meta n ty = PMeta (mk_binding Gamma n ty)

let dest_meta t  = 
  match t with
      PMeta(q) -> q
    | _ -> raise (Failure "Not a meta variable")


(** Constants *)

let destnum n = 
  match (dest_const n) with
    (Cnum c) -> c
  | _ -> raise (Failure "Not a number")

let destbool b = 
  match (dest_const b) with 
    (Cbool c) -> c
  | _ -> raise (Failure "Not a boolean")

let mk_num n = mk_const(Cnum n)
let mk_int n = mk_const(Cnum (Num.num_of_int n))

let mk_bool b = mk_const(Cbool b)

(***
* Function application
***)

(**
   [mk_comb x y]: Make a function application from [x] and [y].
   [mk_comb f [a1;a2;...;an]] is [((((f a1) a2) ...) an)]
*)
let rec mk_comb x y = 
  match y with 
    [] -> x
  | t::ts -> mk_comb (mk_app x t) ts

let mk_fun f args = 
  mk_comb (PId(f, Gtypes.mk_var 
		("_"^(Ident.string_of f)^"_ty"))) args



(*** Operator Overloading ***)

module Resolver =
struct

    open Basic
      
(** memo_find: Memoised lookup function. *)
    let memo_find cache find table n =
      try Lib.find n cache
      with Not_found -> 
	let ret = find n table 
	in 
	(ignore(Lib.bind n ret cache); ret)

(** reolve_memo: Memoised tables *)
    type resolve_memo =
	{ 
	  types : (Ident.t, Basic.gtype)Hashtbl.t;
	  idents: (string, Ident.t)Hashtbl.t;
	  symbols : (string, Ident.t)Hashtbl.t;
	  type_names: (string, Ident.thy_id) Hashtbl.t
	}

(** resolve_arg: The argument to the resolver *)
    type resolve_arg =
	{
	 scp: Scope.t;
	 inf : int ref;
	 memo: resolve_memo;
	 qnts: Term.substitution;
	 lookup: (string -> gtype -> (Ident.t * gtype))
       }
	  
(** 
   [resolve_aux data env expty term]: Resolve names in [term].
   
   Each type name is expanded to a long name. 

   Each free variable [n] (which may be an overloaded symbol
   or name) is resolved to an exact identifier.
   
   [expty] is the type the term is expected to have (can be
   a variable type).

   Returns the renamed term, the actual type and a type
   substitution from which the exact type can be obtained
   (using Gtypes.mgu).

   Never fails.
*)
    let rec resolve_aux data env expty term =
      let bind_qnt t1 t2 =
	{ data with qnts=(Term.bind t1 t2 data.qnts) }
      in 
      let binding_set_names binding =
	let (qnt, qname, qtype) = Basic.dest_binding binding
	in 
	Basic.mk_binding qnt qname 
	  (Gtypes.set_name ~memo:(data.memo.type_names) (data.scp) qtype)
      in 
      let binding_set_types tyenv binding =
	let (qnt, qname, qtype) = Basic.dest_binding binding
	in 
	Basic.mk_binding qnt qname 
	  (Gtypes.mgu qtype tyenv)
      in 
      let set_type_name t =
	Gtypes.set_name ~memo:(data.memo.type_names) data.scp t
      in 
      let find_ident n = 
	let ident_find n s = 
	  let thy = Scope.thy_of_term s n
	  in 
	  Ident.mk_long thy n
	in 
	Lib.try_find (memo_find data.memo.idents ident_find data.scp) n
      in 
      let find_type n = 
	let type_find n s = Scope.type_of s n
	in 
	Lib.apply_option
	  (fun x -> Some (Gtypes.rename_type_vars x))
	  (Lib.try_find (memo_find data.memo.types type_find data.scp) n)
	  None
      in 
      let find_sym n ty= 
	Lib.try_find 
	  (fun atyp -> 
	    let (x, xty) = data.lookup n atyp
	    in 
	    (x, Gtypes.rename_type_vars xty)) ty
      in 
      let unify_types ty1 ty2 env = 
	try Gtypes.unify_env data.scp ty1 ty2 env
	with _ -> env
      in
      match term with
	PId(n, ty) -> 
	  if(Ident.is_short n)
	  then resolve_aux data env expty (PFree((Ident.name_of n), ty))
	  else
	    (let id_ty = find_type n
	    in 
	    let nty = set_type_name ty
	    in 
	    let (ty0, env0)=
	      (nty, unify_types expty nty env)
	    in 
	    let (ty1, env1)=
	      (match id_ty with
		None -> (ty0, env0)
	      | Some(d_ty) -> 
		  (d_ty, unify_types ty0 d_ty env0))
	    in 
	    (Id(n, (Gtypes.mgu ty1 env1)), ty1, env1))
      | PFree(n, ty) -> 
	  let nty = set_type_name ty
	  in 
	  let (ty0, env0)=
	    (nty, unify_types expty nty env)
	  in 
	  let ty1=
	    try Gtypes.mgu ty0 env0 
	    with _ -> ty0
	  in 
	  (match (find_sym n ty1) with
	    None -> 
	      (match (find_ident n) with
		None -> (Free(n, ty1), ty1, env0) 
	      | Some(id3) -> 
		  (resolve_aux data env0 ty1 (PId(id3, ty1))))
	  | Some(id2, ty2) -> 
	      resolve_aux data env0 ty1 (PId(id2, ty2)))
      | PBound(q) -> 
	  let term0 = Bound(q)
	  in 
	  let term1=
	    try Term.find term0 data.qnts
	    with Not_found -> term0
	  in 
	  let ty = Term.get_binder_type term1
	  in
	  let (ty0, env0)=
	    (ty, unify_types expty ty env)
	  in 
	  (term1, ty0, env0)
      | PMeta(q) -> 
	  let ty = binder_type q
	  in
	  let (ty0, env0)=
	    (ty, unify_types expty ty env)
	  in 
	  (Meta(q), ty0, env0)
      | PConst(c) ->
	  let ty = Lterm.typeof_cnst c
	  in
	  let (ty0, env0)=
	    (ty, unify_types expty ty env)
	  in 
	  (Const(c), ty0, env0)
      | PTyped(trm, ty) -> 
	  let nty = set_type_name ty
	  in 
	  let (ty0, env0)=
	    (nty, unify_types expty nty env)
	  in 
	  let trm1, nty1, env1 = 
	    resolve_aux data env0 nty trm
	  in 
	  let (nty2, env2)=
	    (nty1, unify_types nty nty1 env)
	  in 
	  (trm1, nty2, env2)
      | PApp(lf, a) -> 
	  let argty = Gtypes.mk_typevar data.inf
	  in 
	  let rty0 = Gtypes.mk_typevar data.inf
	  in 
	  let (rty1, env1)=
	    (rty0, unify_types expty rty0 env)
	  in 
	  let fty0 = Lterm.mk_fun_ty argty rty1
	  in 
	  let (atrm, aty, aenv) = 
	    resolve_aux data env1 (Gtypes.mgu argty env1) a
	  in  
	  let (ftrm, fty, fenv) = 
 	    resolve_aux data aenv (Gtypes.mgu fty0 aenv) lf
	  in 
	  (App(ftrm, atrm), (Gtypes.mgu rty1 fenv), fenv)
      | PQnt(qnt, body) ->
	  (match Basic.binder_kind qnt with
	    Lambda -> 
	      let qnt1=binding_set_names qnt
	      in 
	      let aty = Term.get_binder_type (Bound qnt1)
	      and rty = Gtypes.mk_typevar data.inf
	      in 
	      let nty0 = Lterm.mk_fun_ty aty rty
	      in 
	      let (nty1, env1)=
		(nty0, unify_types expty nty0 env)
	      in 
	      let qnt2 = binding_set_types env1 qnt1
	      in 
	      let data1=bind_qnt (Bound(qnt)) (Bound(qnt2))
	      in 
	      let (body1, bty, benv) = 
		resolve_aux data1 env1 rty body
	      in
	      (Qnt(qnt2, body1), nty1, benv)
	  | _ -> 
	      let qnt1=binding_set_names qnt
	      in 
	      let (nty1, env1)=
		let bool_ty = Lterm.mk_bool_ty()
		in 
		  (bool_ty, unify_types expty bool_ty env)
	      in 
	      let qnt2 = binding_set_types env1 qnt1
	      in
	      let data1=bind_qnt (Bound(qnt)) (Bound(qnt2))
	      in 
	      let (body1, bty, benv)=
		resolve_aux data1 env1 nty1 body
	      in 
	      (Qnt(qnt2, body1), nty1, benv))


(**
   [default str ty lst]: Get the default identifier for symbol [str]
   of type [ty] from list [lst] of identifiers when no identifier
   matches. 

   Currently, this just raises Not_found
*)
    let default str ty list = None
(*
    let default str ty list= 
      match list with 
	[] -> None
      | (x::_) -> Some x
*)

(**
   [resolve_term env t]: Resolve the symbols in term [t].
   For each free variable [Free(s, ty)] in [t], Lookup [s]
   in [env] to get long identifier [id].  If not found,
   use [Free(s, ty)].  If found, replace [Free(s, ty)]
   with the identifier [Id(id, ty)].
*)
    let resolve_term scp lookup term=
      let rmemo=
	{ 
	  types = Lib.empty_env(); idents=Lib.empty_env();
	  symbols=Lib.empty_env(); type_names = Lib.empty_env()
	}
      in 
      let data = 
	{ 
	  scp = scp;
	  inf= ref 0;
	  memo = rmemo;
	  qnts = Term.empty_subst();
	  lookup = lookup
	}
      in 
      let expty = Gtypes.mk_null()
      in 
      let (term1, ty1, subst) = 
	resolve_aux data (Gtypes.empty_subst()) expty term
      in 
      (term1, subst)

(** 
   find_type scp sym ty list: 
   return first identifier-type pair for symbol sym in list 
   where ty is the type of sym.
	   
   Matching is by unification.
*)
    let find_type scp sym ty list =
      let matching_types t1 t2 = 
	try
	  ignore(Gtypes.unify scp t1 t2); true
	with _ -> false
      in 
      let rec find_aux l = 
	match l with
	  [] -> 
	    (match default sym ty list with
	      None ->  raise Not_found 
	    | Some x -> x)
	| ((id, id_type)::xs) -> 
	    if matching_types ty id_type
	    then (id, id_type)
	    else find_aux xs
      in 
      find_aux list

    let make_lookup scp db s ty= 
      let type_list = db s
      in 
      let (id, id_type) = find_type scp s ty type_list
      in 
      (id, id_type)

end

(***
* Conversion to-from terms
***)

let from_term trm =
  let rec from_aux t =
    match t with
	Id(n, ty) -> PId (n, ty)
      | Bound(q) -> PBound(q)
      | Free(n, ty) -> PFree(n, ty)
      | Meta(q) -> PMeta(q)
      | App(f, a) -> PApp(from_aux f, from_aux a)
      | Qnt(q, b) -> PQnt(q, from_aux b)
      | Const(c) -> PConst(c)
  in 
    from_aux trm

let to_term ptrm =
  let scp = Scope.empty_scope()
  in 
  let unify_types ty1 ty2 env = 
    try Gtypes.unify_env scp ty1 ty2 env
    with _ -> env
  in
  let tyvar_ctr = ref 0
  in
  let rec to_aux tyenv trmenv expty pt =
    match pt with
	PId(n, ty) -> 
	  let env1 = unify_types expty ty tyenv
	  in 
	  (Id (n, Gtypes.mgu ty env1), env1)
      | PFree(n, ty) -> 
	  let env1 = unify_types expty ty tyenv
	  in 
	  (Free (n, Gtypes.mgu ty env1), env1)
      | PBound(q) -> 
	  let pt1, env1 = 
	    match (Lib.try_find (Term.find (Bound(q))) trmenv) with
		None -> 
		  let ty = binder_type q
		  in
		  let env1 = unify_types expty ty tyenv
		  in 
		    (Bound(q), env1)
	      | Some(x) -> (x, tyenv)
	  in 
	    (pt1, env1)
      | PMeta(q) -> 
	  let ty = binder_type q
	  in 
	  let env1 = unify_types expty ty tyenv
	  in 
	    (Meta(q), env1)
      | PQnt(q, b) -> 
	  let qnt, qname, qty = dest_binding q
	  in 
	  (match qnt with
	       Lambda -> 
		 let rty = Gtypes.mk_typevar tyvar_ctr
		 in 
		 let nty = Lterm.mk_fun_ty qty rty
		 in 
		 let env1 = unify_types expty nty tyenv
		 in 
		 let q1 = 
		   mk_binding qnt qname (Gtypes.mgu qty env1)
		 in 
		 let trmenv1 = Term.bind (Bound(q)) (Bound(q1)) trmenv
		 in 
		 let (b1, env2) = to_aux env1 trmenv1 nty b
		 in 
		   (Qnt(q1, b1), env2)
	     | _ ->
		 let nty = Lterm.mk_bool_ty()
		 in 
		 let env1 =
		   unify_types expty nty tyenv
		 in 
		 let q1 = 
		   mk_binding qnt qname (Gtypes.mgu qty env1)
		 in 
		 let trmenv1 = Term.bind (Bound(q)) (Bound(q1)) trmenv
		 in 
		 let (b1, env2) = to_aux env1 trmenv1 nty b
		 in 
		   (Qnt(q1, b1), env2))
      | PApp(f, a) -> 
	  let arg_ty = Gtypes.mk_typevar tyvar_ctr
	  in 
	  let ret_ty = Gtypes.mk_typevar tyvar_ctr
	  in
	  let (a1, env1) = to_aux tyenv trmenv arg_ty a
	  in 
	  let fn_ty = Lterm.mk_fun_ty arg_ty ret_ty
	  in 
	  let env2 = unify_types expty ret_ty env1
	  in 
	  let (f1, env3) = to_aux env2 trmenv fn_ty f
	  in 
	    (App(f1, a1), env3)
      | PConst(c) -> 
	  let ty = Lterm.typeof_cnst c
	  in 
	  let env1 = unify_types expty ty tyenv
	  in 
	    (Const(c), env1)
      | PTyped(x, ty) -> 
	  let env1 = unify_types expty ty tyenv
	  in 
	    (to_aux env1 trmenv ty x)
  in 
  let (trm1, _) =
    to_aux 
      (Gtypes.empty_subst()) (Term.empty_subst()) 
      (Gtypes.mk_typevar tyvar_ctr) ptrm
  in 
    trm1


(*** Conversion with overloading ***)

    let resolve = Resolver.resolve_term

    let make_lookup = Resolver.make_lookup
