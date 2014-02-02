(*----
  Name: resolver.ml
  Copyright M Wahab 2005-2014
  Author: M Wahab  <mwb.cde@gmail.com>

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

(*** Resolver for operator overloading ***)

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
      types: (Ident.t, Basic.gtype)Hashtbl.t;
      idents: (string, Ident.t)Hashtbl.t;
      symbols: (string, Ident.t)Hashtbl.t;
      type_names: (string, Ident.thy_id) Hashtbl.t
    }

(** resolve_arg: The argument to the resolver *)
type resolve_arg =
    {
      scp: Scope.t;
      inf: int ref;
      memo: resolve_memo;
      qnts: Term.substitution;
      lookup: (string -> gtype -> (Ident.t * gtype))
    }
      
(** [resolve_aux data env expty term]: Resolve names in [term].
    
    Each type name is expanded to a long name. 

    Each free variable [n] (which may be an overloaded symbol or name)
    is resolved to an exact identifier.
    
    [expty] is the type the term is expected to have (can be a
    variable type).

    Returns the renamed term, the actual type and a type substitution
    from which the exact type can be obtained (using Gtypes.mgu).

    Never fails.
*)
let rec resolve_aux data env expty term =
  let bind_qnt t1 t2 =
    { data with qnts = Term.bind t1 t2 data.qnts }
  in 
  let binding_set_names binding =
    let (qnt, qname, qtype) = Basic.dest_binding binding
    in 
    Basic.mk_binding qnt qname 
      (Gtypes.set_name ~memo:(data.memo.type_names) (data.scp) qtype)
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
    let find_fn atyp = 
      let (x, xty) = data.lookup n atyp
      in 
      (x, Gtypes.rename_type_vars xty)
    in 
    Lib.try_find find_fn ty
  in 
  match term with
    | Id(n, ty) -> 
      if Ident.is_short n
      then resolve_aux data env expty (Free(Ident.name_of n, ty))
      else
	let id_ty = find_type n in 
	let nty = set_type_name ty in 
	let (ty0, env0)=
	  try (nty, Gtypes.unify_env data.scp expty nty env)
	  with _ -> (nty, env)
	in 
	let (ty1, env1)=
	  match id_ty with
	    | None -> (ty0, env0)
	    | Some(d_ty) -> 
	      try (d_ty, Gtypes.unify_env data.scp ty0 d_ty env0)
	      with _ -> (d_ty, env0)
	 in 
	(Id(n, Gtypes.mgu ty1 env1), ty1, env1)
    | Free(n, ty) -> 
      let nty = set_type_name ty
      in 
      let (ty0, env0)=
	try (nty, Gtypes.unify_env data.scp expty nty env)
	with _ -> (nty, env)
      in 
      let ty1=
	try Gtypes.mgu ty0 env0 
	with _ -> ty0
      in 
      begin
        match (find_sym n ty1) with
	  | None -> 
	    begin
              match (find_ident n) with
		| None -> (Free(n, ty1), ty1, env0) 
	        | Some(id3) -> 
		  resolve_aux data env0 ty1 (Id(id3, ty1))
            end
	  | Some(id2, ty2) -> 
	    resolve_aux data env0 ty1 (Id(id2, ty2))
      end
    | Bound(q) -> 
      let term1=
	try Term.find term data.qnts
	with Not_found -> term
      in 
      let ty = Term.get_binder_type term1 in
      let (ty0, env0) =
	try (ty, Gtypes.unify_env data.scp expty ty env)
	with _ -> (ty, env)
      in 
      (term1, ty0, env0)
    | Meta(q) -> 
      let ty = Term.get_binder_type term in
      let (ty0, env0) =
	try (ty, Gtypes.unify_env data.scp expty ty env)
	with _ -> (ty, env)
      in 
      (term, ty0, env0)
    | Const(c) ->
      let ty = Lterm.typeof_cnst c in
      let (ty0, env0)=
	try (ty, Gtypes.unify_env data.scp expty ty env)
	with _ -> (ty, env)
      in 
      (term, ty0, env0)
    | Typed(trm, ty) -> 
      let nty = set_type_name ty in 
      let (ty0, env0) =
	try (nty, Gtypes.unify_env data.scp expty nty env)
	with _ -> (nty, env)
      in 
      let trm1, nty1, env1 = resolve_aux data env0 nty trm in 
      let (nty2, env2)=
	try (nty1, Gtypes.unify_env data.scp nty nty1 env1)
	with _ -> (nty1, env1)
      in 
      (Typed(trm1, nty2), nty2, env2)
    | App(lf, a) -> 
      let argty = Gtypes.mk_typevar data.inf in 
      let rty0 = Gtypes.mk_typevar data.inf in 
      let (rty1, env1)=
	try (rty0, Gtypes.unify_env data.scp expty rty0 env)
	with _ -> (rty0, env)
      in 
      let fty0 = Lterm.mk_fun_ty argty rty1 in 
      let (atrm, aty, aenv) = 
	resolve_aux data env1 (Gtypes.mgu argty env1) a
      in  
      let (ftrm, fty, fenv) = 
 	resolve_aux data aenv (Gtypes.mgu fty0 aenv) lf
      in 
      (App(ftrm, atrm), Gtypes.mgu rty1 fenv, fenv)
    | Qnt(qnt, body) ->
      begin
        match Basic.binder_kind qnt with
          | Lambda -> 
	    let qnt1 = binding_set_names qnt in 
	    let data1 = bind_qnt (Bound(qnt)) (Bound(qnt1)) in 
	    let aty = Term.get_binder_type (Bound qnt1)
	    and rty = Gtypes.mk_typevar data1.inf
	    in 
	    let nty0 = Lterm.mk_fun_ty aty rty in 
	    let (nty1, env1) =
	      try (nty0, Gtypes.unify_env data1.scp expty nty0 env)
	      with _ -> (nty0, env)
	    in 
	    let (body1, bty, benv) = resolve_aux data1 env1 rty body
	    in
	    (Qnt(qnt1, body1), nty1, benv)
	| _ -> 
	  let qnt1 = binding_set_names qnt in 
	  let data1 = bind_qnt (Bound(qnt)) (Bound(qnt1))  in 
	  let (nty1, env1) =
	    try (Lterm.mk_bool_ty(), 
		 Gtypes.unify_env data1.scp expty (Lterm.mk_bool_ty()) env)
	    with _ -> (Lterm.mk_bool_ty(), env)
	  in 
	  let (body1, bty, benv) = resolve_aux data1 env1 nty1 body
	  in 
	  (Qnt(qnt1, body1), nty1, benv)
      end


(** [default str ty lst]: Get the default identifier for symbol [str]
    of type [ty] from list [lst] of identifiers when no identifier
    matches.

    Currently, this just raises Not_found
*)
let default str ty list = None

(** [resolve_term env t]: Resolve the symbols in term [t].  For each
    free variable [Free(s, ty)] in [t], Lookup [s] in [env] to get long
    identifier [id].  If not found, use [Free(s, ty)].  If found,
    replace [Free(s, ty)] with the identifier [Id(id, ty)].
*)
let resolve_term scp lookup term =
  let rmemo =
    { 
      types = Lib.empty_env();
      idents = Lib.empty_env();
      symbols = Lib.empty_env(); 
      type_names = Lib.empty_env()
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

(** find_type scp sym ty list: return first identifier-type pair for
    symbol sym in list where ty is the type of sym.
    
    Matching is by unification.
*)
let find_type scp sym ty list =
  let matching_types t1 t2 = 
    try ignore(Gtypes.unify scp t1 t2); true
    with _ -> false
  in 
  let rec find_aux l = 
    match l with
      |	[] -> 
        begin
	  match default sym ty list with
	    | None ->  raise Not_found 
	    | Some x -> x
        end
      | (id, id_type)::xs -> 
	if matching_types ty id_type
	then (id, id_type)
	else find_aux xs
  in 
  find_aux list

let make_lookup scp db s ty= 
  let type_list = db s in 
  let (id, id_type) = find_type scp s ty type_list
  in 
  (id, id_type)


