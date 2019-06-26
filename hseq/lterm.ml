(*----
  Name: lterm.ml
  Copyright Matthew Wahab 2005-2019
  Author: Matthew Wahab <mwb.cde@gmail.com>

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
open Term

(*
 * Theories
 *)

let base_thy = "base"
let nums_thy = "nums"

(*
 * Types
 *)

(*** Identifiers for base types ***)

let bool_ty_id = Ident.mk_long base_thy "bool"
let fun_ty_id = Ident.mk_long base_thy "FUN"
let ind_ty_id = Ident.mk_long base_thy "ind"
let num_ty_id = Ident.mk_long nums_thy "num"

(** The type ind *)

let ind_ty = Gtype.mk_constr ind_ty_id []
let mk_ind_ty () = ind_ty
let is_ind_ty t =
  let (f, args) = Gtype.dest_constr t in
  (f = ind_ty_id) && (args = [])

(** The type nums.num *)

let num_ty = Gtype.mk_constr num_ty_id []
let mk_num_ty () = num_ty
let is_num_ty t =
  let (f, args) = Gtype.dest_constr t in
  (f = num_ty_id) && (args = [])

(*** Type bool ***)

let bool_ty = Gtype.mk_constr bool_ty_id []
let mk_bool_ty () = bool_ty
let is_bool_ty t =
  let (f, args) = Gtype.dest_constr t in
  (f = bool_ty_id) && (args = [])

(*** Type of functions ***)

let mk_fun_ty l r = Gtype.mk_constr fun_ty_id [l; r]

let is_fun_ty t =
  let (f, _) = Gtype.dest_constr t in
  f = fun_ty_id

let rec mk_fun_ty_from_list l r =
  match l with
    | [] -> raise (Failure "No argument types")
    | [t] -> mk_fun_ty t r
    | t::ts -> mk_fun_ty t (mk_fun_ty_from_list ts r)

let dest_fun_ty t =
  if (is_fun_ty t)
  then
    let (_, args) = Gtype.dest_constr t in
    match args with
      | [a1; a2] -> (a1, a2)
      | _ -> raise (Failure "Not function type")
  else
    raise (Failure "Not function type")

let typeof_cnst c =
  match c with
    | Const.Cbool _ -> mk_bool_ty ()

(*
 * Terms
 *)

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
    | Atom(Const(Const.Cbool true)) -> true
    | _ -> false

let is_false t =
  match t with
    | Atom(Const(Const.Cbool false)) -> true
    | _ -> false

let is_neg t =
  try fst(dest_fun t) = notid with _ -> false
let is_conj t =
  try fst(dest_fun t) = andid with _ -> false
let is_disj t =
  try fst(dest_fun t) = orid with _ -> false
let is_implies t =
  try fst(dest_fun t) = impliesid with _ -> false
let is_equality t =
  try fst(dest_fun t) = equalsid with _ -> false

(*** Constructors ***)

let mk_true = mk_const(Const.Cbool true)
let mk_false = mk_const(Const.Cbool false)
let mk_bool b = if b then mk_true else mk_false

let mk_not t = mk_fun notid [t]
let mk_and l r = mk_fun andid [l; r]
let mk_or l r = mk_fun orid [l; r]
let mk_implies l r = mk_fun impliesid [l; r]
let mk_iff l r = mk_fun iffid [l; r]
let mk_equality l r = mk_fun equalsid [l; r]
let mk_any = Term.mk_ident anyid

(*** Destructors ***)

let dest_bool b =
  if b = mk_true
  then true
  else
    (if b = mk_false
     then false
     else raise (Failure "Not a boolean"))

let dest_equality t =
  if is_equality t
  then
    (match snd(dest_fun t) with
      | [l; r] -> (l, r)
      |	_ -> raise (term_error "Badly formed equality" [t]))
  else raise (Report.error "Not an equality")

(*** Quantified terms *)

let is_all t =
  (is_qnt t) &&
    match get_binder_kind t with
      | Term.All -> true
      | _ -> false

let is_exists t =
  (is_qnt t) &&
    match get_binder_kind t with
      | Term.Ex -> true
      | _ -> false

let is_lambda t =
  (is_qnt t) &&
    match get_binder_kind t with
      | Term.Lambda -> true
      | _ -> false

(* [subst_qnt_var]: Specialised form of substitution for constructing
   quantified terms. Replaces only free variables and only if name and
   type match the quantifying term.
*)
let subst_qnt_var scp env trm =
  let rec subst_aux t =
    match t with
    | Atom(Id(n, ty)) ->
       begin
         try
           let replacement = Ident.Tree.find env n in
           ignore(Gtype.unify (Scope.types_scope scp)
                    ty (Term.Binder.type_of replacement));
           (mk_bound replacement)
         with _ -> t
       end
    | Atom(Free(n, ty)) ->
       begin
         try
           let replacement = Ident.Tree.find env (Ident.mk_name n) in
           ignore(Gtype.unify (Scope.types_scope scp)
                    ty (Term.Binder.type_of replacement));
           (mk_bound replacement)
         with _ -> t
       end
    | App(f, a) -> App(subst_aux f, subst_aux a)
    | Qnt(q, b) -> Qnt(q, subst_aux b)
    | _ -> t
  in
  subst_aux trm

(** [mk_typed_qnt_name scp qnt ty n t]: Make a quantified term, of
    kind [qnt], from term [t], binding all free variables named
    [n]. Set the type of the quantifier to [ty].
*)
let mk_typed_qnt_name scp q ty n b =
  let bndr = Binder.make q n ty in
  let bnd_env = Ident.Tree.add (Ident.Tree.empty) (Ident.mk_name n) bndr
  in
  let nb = subst_qnt_var scp bnd_env b
  in
  Qnt(bndr, nb)

(** [mk_qnt_name scp qnt n t]: Make a quantified term, with quantifier
    [qnt], from term [t], binding free variables named [n].
*)
let mk_qnt_name tyenv q n b =
  mk_typed_qnt_name tyenv q (Gtype.mk_null()) n b

let mk_all tyenv n b = mk_qnt_name tyenv Term.All n b
let mk_all_ty tyenv n ty b = mk_typed_qnt_name tyenv Term.All ty n b

let mk_ex tyenv n b = mk_qnt_name tyenv Term.Ex n b
let mk_ex_ty tyenv n ty b = mk_typed_qnt_name tyenv Term.Ex ty n b

let mk_lam tyenv n b = mk_qnt_name tyenv Term.Lambda n b
let mk_lam_ty tyenv n ty b = mk_typed_qnt_name tyenv Term.Lambda ty n b

(*
 * Lambda Conversions
 *)

(*** Alpha conversion ***)

let alpha_convp_full scp tenv t1 t2 =
  let type_matches scp env x y = Ltype.unify_env scp x y env
  in
  let rec alpha_atom t1 t2 tyenv trmenv =
    match (t1, t2) with
      | (Id(n1, ty1), Id(n2, ty2)) ->
        if n1 = n2
        then (trmenv, type_matches scp tyenv ty1 ty2)
        else raise (term_error "alpha_convp_aux" [Atom(t1); Atom(t2)])
      | _ ->
        if equals (Atom(t1)) (Atom(t2))
        then (trmenv, tyenv)
        else raise (term_error "alpha_convp_aux" [Atom(t1); Atom(t2)])
  in
  let rec alpha_aux t1 t2 tyenv trmenv =
    match (t1, t2) with
      | Atom(a1), Atom(a2) -> alpha_atom a1 a2 tyenv trmenv
      | (Bound(q1), Bound(q2)) ->
        let q1trm =
          (try Term.Subst.find t1 trmenv with Not_found -> t1)
        and q2trm =
          (try Term.Subst.find t2 trmenv with Not_found -> t2)
        in
        if equals q1trm q2trm
        then (trmenv, tyenv)
        else raise (term_error "alpha_convp_aux" [t1; t2])
      | (App(f1, a1), App(f2, a2)) ->
        let (trmenv1, tyenv1) = alpha_aux f1 f2 tyenv trmenv
        in
        alpha_aux a1 a2 tyenv1 trmenv1
      | (Qnt(q1, b1), Qnt(q2, b2)) ->
        let qty1 = Term.Binder.type_of q1
        and qty2 = Term.Binder.type_of q2
        and qn1 = Term.Binder.kind_of q1
        and qn2 = Term.Binder.kind_of q2
        in
        if (qn1=qn2)
        then
          let tyenv1 = type_matches scp tyenv qty1 qty2
          and trmenv1 =
            Term.Subst.bind (Bound q1) (Bound q2) trmenv
          in
          alpha_aux b1 b2 tyenv1 trmenv1
        else raise (term_error "alpha_convp_aux" [t1;t2])
      | _ ->
        if equals t1 t2
        then (trmenv, tyenv)
        else raise (term_error "alpha_convp_aux" [t1;t2])
  in
  let env = Term.Subst.empty()
  in
  try
    let (_, ret) = alpha_aux t1 t2 tenv env
    in
    ret
  with _ -> raise (term_error "alpha_convp" [t1; t2])

let alpha_convp scp t1 t2 =
  let tyenv = Gtype.Subst.empty()
  in
  alpha_convp_full scp tyenv t1 t2

let alpha_equals scp t1 t2 =
  try ignore(alpha_convp scp t1 t2); true
  with _ -> false

(*** Beta conversion ***)

let beta_convp  =
  function
    | App(f, a) -> is_lambda f
    | _ -> false

let beta_conv t =
  match t with
    | App(f, a) ->
      if is_lambda f
      then
        let (q, b) = dest_qnt f
        in
        qsubst [Bound(q), a]  b
      else raise (term_error "Can't apply beta-reduction" [t])
    | _ -> raise (term_error "Can't apply beta-reduction" [t])

let safe_beta_reduce trm =
  let rec beta_aux t env =
    match t with
      | App(f, a) ->
        let (na, achng) = beta_aux a env
        in
        if is_lambda f
        then
          let (q, b) = dest_qnt f in
          let env1 = Term.Subst.bind (Bound q) na env in
          let (nb, _) = beta_aux b env1
          in
          (nb, true)
        else
          let (nf, fchng) = beta_aux f env
          in
          if fchng && (is_lambda nf)
          then beta_aux (App(nf, na)) env
          else (App(nf, na), achng || fchng)
      | Qnt(q, b) ->
        let nb, chng = beta_aux b env
        in
        (Qnt(q, nb), chng)
      | Bound(q) ->
        (try (Term.Subst.find t env, true)
         with Not_found -> (t, false))
      | _ -> (t, false)
  in
  let (nt, chng) = beta_aux trm (Term.Subst.empty())
  in
  if chng
  then nt
  else raise (Report.error "beta_reduce: No change")

let beta_reduce trm =
  let rebuild_app t l = mk_comb t l
  in
  let rec beta_app t env args =
    match t with
      |	App(f, a) ->
          let (na, achng) = beta_aux a env in
          let (nt, tchng) = beta_app f env (na::args)
          in
          (nt, achng || tchng)
      | Qnt(q, b) ->
        if (not (args = []) && (is_lambda t))
        then
          let (na, nargs) = (List.hd args, List.tl args) in
          let env1 = Term.Subst.bind (Bound q) na env in
          let (nb, _) = beta_app b env1 nargs
          in
          (nb, true)
        else
          let (nt, tchng) = beta_aux t env
          in
          (rebuild_app nt args, tchng)
      | _ ->
        let (nt, tchng) = beta_aux t env
        in
        (rebuild_app nt args, tchng)
  and beta_aux t env =
    match t with
      |	App(f, a) ->
          let (na, achng) = beta_aux a env in
          let (nt, tchng) = beta_app f env [na]
          in
          (nt, achng || tchng)
      | Qnt(q, b) ->
        let (nb, chng) = beta_aux b env
        in
        (Qnt(q, nb), chng)
      | Bound(q) ->
        (try (Term.Subst.find t env, true)
         with Not_found -> (t, false))
      | _ -> (t, false)
  in
  let (nt, chng) = beta_aux trm (Term.Subst.empty())
  in
  if chng
  then nt
  else raise (Report.error "beta_reduce: No change")

(*** Eta-abstraction ***)

let eta_conv ts term=
  let rec eta_aux ctr xs (rslt, env) =
    match xs with
      | [] -> (rslt, env)
      | (x::xss) ->
        let name = Lib.int_to_name ctr in
        let ty = Gtype.mk_var (name^"_ty") in
        let binder = Term.Binder.make Term.Lambda name ty in
        let nv = Term.mk_bound binder in
        let env1 = Term.Subst.bind x nv env
        in
        eta_aux (ctr + 1) xss ((x, nv)::rslt, env1)
  in
  let (nvars, env) = eta_aux 0 (List.rev ts) ([], Term.Subst.empty()) in
  let body = Term.subst env term in
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


(*
 * Closed terms
 *)

let rec is_closed_env env t =
  match t with
    | App(l, r) ->
        (is_closed_env env l) && (is_closed_env env r)
    | Qnt(q, b) ->
      let env1 =
        Term.Subst.bind (Bound(q)) (mk_free "" (Gtype.mk_null())) env
      in
      is_closed_env env1 b
    | Atom(Meta(_)) -> true
    | Bound(_) -> Term.Subst.member t env
    | Atom(Free(_)) -> Term.Subst.member t env
    | _ -> true

let is_closed vs t =
  (* add bound terms of [vs] to tbl *)
  let env =
    List.fold_left
      (fun env x ->
        if (is_bound x) || (is_free x)
        then Term.Subst.bind x (mk_free "" (Gtype.mk_null())) env
        else env)
      (Term.Subst.empty()) vs
  in
  try is_closed_env env t
  with _ -> false

(**
   [close_term qnt free trm]: Close term [trm]. Make variables bound
   to quantifiers of kind [qnt] to replace free variables and bound
   variables with no binding quantifier and for which [free] is true.
*)
let ct_free _ = true

let close_term ?(qnt=Term.All) ?(free=ct_free) trm =
  let rec close_aux env vs t=
    match t with
      | Atom(Id(_)) -> (t, env, vs)
      | Atom(Meta(_)) -> (t, env, vs)
      | Bound(_) ->
        if Term.Subst.member t env
        then (t, env, vs)
        else
          if free t
          then (t, env, t::vs)
          else (t, env, vs)
      | Atom(Free(_)) -> (t, env, t::vs)
      | Atom(Const _) -> (t, env, vs)
      | App(f, a) ->
        let f1, env1, vs1 = close_aux env vs f in
        let a1, env2, vs2 = close_aux env1 vs1 a
        in
        (App(f1, a1), env2, vs2)
      | Qnt(q, b) ->
        let (b1, env1, vs1) =
          close_aux (Subst.bind (Bound(q)) (Bound(q)) env) vs b
        in
        (Qnt(q, b1), env, vs1)
  in
  let (nt, env, vars) = close_aux (Subst.empty()) [] trm in
  let make_qnts qnt (env, ctr, bs) t =
    let qname = Lib.int_to_name ctr in
    let qty = Gtype.mk_var qname in
    let qbind = Term.Binder.make qnt qname qty in
    let qtrm = mk_bound qbind
    in
    (Subst.bind t qtrm env, ctr+1, qbind::bs)
  in
  let (sb, _, binders) =
    List.fold_left (make_qnts qnt) (Subst.empty(), 0, []) vars
  in
  rebuild_qnt (List.rev binders) (subst sb trm)

(*** Generalising terms ***)

(** [gen_term qnts trm]: generalise term [trm]. Replace bound
    variables occuring outside their binder and free variables with
    universally quantified variables. Binders in [qnts] are ignored.

    belongs in Logicterm
*)
let gen_term bs trm =
  let rec gen_aux qnts known vars t =
    let get_bound t =
      try (Term.Subst.find t known, qnts, known, vars)
      with Not_found ->
        try (Term.Subst.find t vars, qnts, known, vars)
        with _ ->
          let q = Term.dest_bound t in
          let (_, name, ty) = Term.Binder.dest q in
          let q1 = Term.Binder.make Term.All name ty
          in
          (Bound(q1), q1::qnts, known,
           Term.Subst.bind t (Bound(q1)) vars)
    and get_free t =
      try (Term.Subst.find t known, qnts, known, vars)
      with Not_found ->
        try (Term.Subst.find t vars, qnts, known, vars)
        with _ ->
          let (name, ty) = Term.dest_free t in
          let q = Term.Binder.make Term.All name ty
          in
          (Bound(q), q::qnts, known,
           Term.Subst.bind t (Bound(q)) vars)
    in
    match t with
      | Bound(_) -> get_bound t
      | Atom(Free(_)) -> get_free t
      | Qnt(q, body) ->
        let (body1, qnts1, known1, vars1) =
          gen_aux qnts
            (Term.Subst.bind (Bound(q)) (Bound(q)) known)
            vars body
        in
        (Qnt(q, body1), qnts1, known, vars1)
      | App(f, a) ->
        let (f1, qnts1, known1, vars1) =
          gen_aux qnts known vars f
        in
        let (a1, qnts2, known2, vars2) =
          gen_aux qnts1 known1 vars1 a
        in
        (App(f1, a1), qnts2, known2, vars2)
      | _ -> (t, qnts, known, vars)
  in
  let (trm1, qnts, _, vars) =
    gen_aux [] (Term.Subst.empty())
      (List.fold_left
         (fun s q -> Term.Subst.bind (Bound(q)) (Bound(q)) s)
         (Term.Subst.empty())
         bs)
      trm
  in
  Term.rebuild_qnt qnts trm1


(*
 * Resolving names
 *)

(** [in_scope]: Check that term is in scope.
*)
let in_scope memo scp trm =
  let lookup_id n =
    try Lib.find n memo
    with Not_found ->
      if (Scope.in_scope scp n)
      then Lib.add n true memo
      else raise Not_found
  in
  let rec in_scp_aux t =
    match t with
      | Atom(Id(id, ty)) ->
          ignore(lookup_id (Ident.thy_of id));
          Ltype.in_scope memo scp ty
      | Bound(_) -> Ltype.in_scope memo scp (get_binder_type t)
      | Atom(Meta(q)) ->
        if Scope.is_meta scp q
        then true
        else raise Not_found
      | Atom(Free(_)) -> raise Not_found
      | Qnt(_, b) ->
        ignore(Ltype.in_scope memo scp (get_binder_type t));
        in_scp_aux b
      | App(a, b) ->
        ignore(in_scp_aux a);
        in_scp_aux b
      | _ -> true
  in
  try ignore(in_scp_aux trm); true
  with Not_found -> false

(** [binding_set_names_types ?memo scp binding] Find and set names for
    types in a binding.
*)
let binding_set_names ?(strict=false) ?memo scp binding =
  let (qnt, qname, qtype) = Term.Binder.dest binding
  in
  Term.Binder.make
    qnt qname
    (Ltype.set_name ?memo:memo (Scope.relaxed scp) qtype)

(** [set_names scp thy trm] find and set long identifiers and types
    for variables in [trm] theory is [thy] if no long identifier can be
    found in scope [scp]
*)
let set_names scp trm =
  let set_type_name memo s t =
    Ltype.set_name ~memo:memo (Scope.relaxed s) t
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
    try Gtype.rename_type_vars (Lib.find id type_memo)
    with Not_found ->
      let nty =
        try Scope.type_of scp id
        with Not_found -> Gtype.mk_null()
      in
      (ignore(Lib.add id nty type_memo); nty)
  in
  let unify_types ty1 ty2 =
    try
      let env = Ltype.unify scp ty1 ty2
      in
      Gtype.mgu ty1 env
    with _ -> ty1
  in
  let rec set_aux qnts t=
    match t with
    | Atom(Id(id, ty)) ->
       let th, n = Ident.dest id in
       let nid =
         if th = Ident.null_thy
         then Ident.mk_long (lookup_id n) n
         else id
       in
       let ty1 = set_type_name type_thy_memo scp ty in
       let nty =
         try Some(lookup_type id)
         with Not_found -> None
       in
       let ret_id =
         match nty with
         | None -> Atom(Id(nid, ty1))
         | Some(xty) -> Atom(Id(nid, unify_types xty ty1))
       in
       ret_id
    | Atom(Free(n, ty)) ->
       let ty1= set_type_name type_thy_memo scp ty
       in
       begin
         match try_find (Scope.find_meta scp) n with
         | Some(q) -> Atom(Meta(q))
         | None ->
            begin
              match try_find lookup_id n with
              | None -> Atom(Free(n, ty1))
              | Some (nth1) ->
                 let nid = Ident.mk_long nth1 n
                 in
                 match try_find lookup_type nid with
                 | None -> Atom(Id(nid, ty1))
                 | Some(xty) -> Atom(Id(nid, unify_types xty ty1))
            end
       end
    | Qnt(q, b) ->
       let nq = binding_set_names ~memo:type_thy_memo scp q in
       let qnts1 = Subst.bind (Bound(q)) (Bound(nq)) qnts
       in
       Qnt(nq, set_aux qnts1 b)
    | App(f, a) -> App(set_aux qnts f, set_aux qnts a)
    | Atom(Meta(q)) ->
       if Scope.is_meta scp q
       then t
       else
         raise (term_error "Meta variable occurs outside binding" [t])
    | Bound(q) ->
       begin
         match Lib.try_find (Subst.find (Bound(q))) qnts with
         | Some(x) -> x
         | None ->
            raise (term_error "Bound variable occurs outside binding" [t])
       end
    | _ -> t
  in
  set_aux (Subst.empty()) trm

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
      try Gtype.rename_type_vars (Lib.find id type_memo)
      with Not_found ->
        let ty =
          try Scope.type_of scp id
          with Not_found -> Gtype.mk_null()
        in (ignore(Lib.add id ty type_memo); ty)
    in
    let (th, name) = Ident.dest ident
    in
    let nid =
      if th = Ident.null_thy
      then Ident.mk_long (lookup_name name) name
      else ident
    in
    let ty0 = lookup_type ident in
    let ntyenv = Ltype.unify scp ty0 ty in
    let nty = Gtype.mgu ty0 ntyenv
    in
    Term.mk_typed_ident nid nty
  in
  let set_type_name memo s t =
    Ltype.set_name ~memo:memo s t
  in
  let lookup_var vars t =
    match Lib.try_find (Subst.find t) vars with
      | Some(x) -> (x, vars)
      | None ->
        let nt =
          (match t with
            | Atom(Free(n, ty)) -> mk_bound (Term.Binder.make All n ty)
            | Bound(q) ->
               mk_bound (Term.Binder.make All
                           (Term.Binder.name_of q) (Binder.type_of q))
            | _ -> mk_bound (Term.Binder.make All "x" (Gtype.mk_var "ty")))
        in
        (nt, Subst.bind t nt vars)
  in
  let rec set_aux (qnts, vars) t lst =
    match t with
      | Atom(Id(id, ty)) ->
          let ty1 =
            try set_type_name type_thy_memo scp ty
            with err -> raise (add_term_error "Invalid type" [t] err)
          in
          let ret_id =
            match (Lib.try_find (lookup_id scp id) ty1) with
              | Some x -> (x: Term.term)
              | None -> raise (term_error "Term not in scope" [t])
          in
          if in_scope scope_memo scp ret_id
          then (ret_id, vars, lst)
          else raise (term_error "Term not in scope" [t])
      | Atom(Free(n, ty)) ->
        (match Lib.try_find (Scope.find_meta scp) n with
            Some(b) -> (Atom(Meta b), vars, lst)
          | None ->
             set_aux (qnts, vars) (mk_typed_ident (Ident.mk_name n) ty) lst)
      | Qnt(q, b) ->
        let nq = binding_set_names ~memo:type_thy_memo scp q in
        let qnts1 = Subst.bind (mk_bound q) (mk_bound nq) qnts in
        let (nb, nvars, nlst) = set_aux (qnts1, vars) b lst
        in
        (Qnt(nq, nb), nvars, nlst)
      | App(f, a) ->
        let (nf, fvars, flst) = set_aux (qnts, vars) f lst in
        let (na, avars, alst) = set_aux (qnts, fvars) a flst
        in
        (App(nf, na), avars, alst)
      | Atom(Meta(q)) ->
        if Scope.is_meta scp q
        then (t, vars, lst)
        else
          let nt, nvars = lookup_var vars t
           in
          (nt, nvars, ((nt, t)::lst))
      | Bound(q) ->
         begin
           match Lib.try_find (Subst.find (mk_bound q)) qnts with
           | Some x -> (x, vars, lst)
           | None ->
              let (nt, nvars) = lookup_var vars t
              in
              (nt, nvars, ((nt, t)::lst))
         end
      | _ -> (t, vars, lst)
  in
  set_aux (Subst.empty(), vars) trm varlist

(** [resolve scp trm]: resolve names and types in term [trm] in [lst]
    in scope [scp].
*)
let resolve scp trm =
  let (ntrm, vars, lst) = resolve_term scp (Subst.empty()) [] trm
  in
  (ntrm, lst)

(*
 * Substitution
 *)

let rec subst_closed qntenv sb trm =
  try
    let nt = Subst.replace sb trm
    in
    if is_closed_env qntenv nt
    then subst_closed qntenv sb nt
    else raise (Failure "subst_closed: Not closed")
  with Not_found ->
    (match trm with
      | Qnt(q, b) ->
        let qntenv1 =
          Subst.bind (mk_bound q) (mk_free "" (Gtype.mk_null())) qntenv
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
      | Some(x) ->
        if (is_closed_env qntenv x)
        then x
        else raise (Failure "subst_equiv: Not closed")
      | None ->
        (match trm with
          | Qnt(q, b) ->
             let qntenv1 =
               Term.Subst.bind (mk_bound q) (Term.mk_short_ident "") qntenv
             in
             Qnt(q, subst_aux qntenv1 b)
          | App(f, a) -> App(subst_aux qntenv f, subst_aux qntenv a)
          | _ -> trm)
  in
  subst_aux (Term.Subst.empty()) term
