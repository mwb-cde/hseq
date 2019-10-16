(*----
  Name: pterm.ml
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


(*** Parsed terms ***)

(** The representation of a parsed term *)
type t =
  | PId of Ident.t* Gtype.t   (** Identifiers *)
  | PBound of Term.Binder.t    (** Bound variables *)
  | PFree of string * Gtype.t (** Free variables *)
  | PMeta of Term.Binder.t     (** Meta variables (use for skolem constants) *)
  | PApp of t * t    (** Function application *)
  | PQnt of Term.Binder.t * t (** Binding terms *)
  | PConst of Term.Const.t     (** Constants *)
  | PTyped of t * Gtype.t  (** Typed terms *)

(*
 * Operations on terms
 *)

(*** Recognisers ***)

let is_qnt x =
  match x with
    | PQnt _ -> true
    | _ -> false

let is_app x =
  match x with
    | PApp _ -> true
    | _ -> false

let is_bound x =
  match x with
    | PBound _ -> true
    | _ -> false

let is_free x =
  match x with
    | PFree _ -> true
    | _ -> false

let is_ident x =
  match x with
    | PId _ -> true
    | _ -> false

let is_typed x =
  match x with
    | PTyped(_, _) -> true
    | _ -> false

let is_const x =
  match x with
    | PConst _ -> true
    | _ -> false

let is_true t =
  match t with
    | PConst(Term.Const.Cbool true) -> true
    | _ -> false

(* Constructors *)

let mk_qnt b t= PQnt(b, t)
let mk_bound n = PBound n
let mk_free n ty = PFree(n, ty)
let mk_app f a = PApp(f, a)
let mk_typed t ty = PTyped(t, ty)
let mk_const c = PConst c

let mk_typed_ident n t = PId(n, t)
let mk_ident n = mk_typed_ident n (Gtype.mk_null ())
let mk_short_ident n = mk_free n (Gtype.mk_null())

(* Destructors *)

let dest_qnt t =
  match t with
    | PQnt(q, b) -> (q, b)
    | _  -> raise (Failure "Not a quantifier")

let dest_bound t =
  match t with
    | PBound(n) -> n
    | _ -> raise (Failure "dest_bound: Not a binder")

let dest_free t =
  match t with
    | PFree(n, ty) -> (n, ty)
    | _ -> raise (Failure "Not a free variable")

let dest_ident vt =
  match vt with
    | PId(n, t) -> (n, t)
    | _ -> raise (Failure "Not a variable")

let dest_app t =
  match t with
    | PApp(f, a) -> (f, a)
    | _ -> raise (Failure "Not a function")

let dest_typed t =
  match t with
    | PTyped(trm, ty) -> (trm, ty)
    | _ -> raise (Failure "Not a typed term")

let dest_const t =
  match t with
    | PConst(c) -> c
    | _ -> raise (Failure "Not a constant")

(* Specialised Manipulators *)

let is_meta trm =
  match trm with
    | PMeta _ -> true
    | _ -> false

let mk_meta n ty = PMeta (Term.Binder.make Term.Gamma n ty)

let dest_meta t =
  match t with
    | PMeta(q) -> q
    | _ -> raise (Failure "Not a meta variable")


(** Constants *)

let destbool b =
  match dest_const b with
    | Term.Const.Cbool(c) -> c

let mk_bool b = mk_const(Term.Const.Cbool b)

(*
 * Function application
 *)

(** [mk_comb x y]: Make a function application from [x] and [y].
    [mk_comb f [a1;a2;...;an]] is [((((f a1) a2) ...) an)].
*)
let rec mk_comb x y =
  match y with
    | [] -> x
    | t::ts -> mk_comb (mk_app x t) ts

let mk_fun f args =
  mk_comb (PId(f, Gtype.mk_var ("_"^(Ident.string_of f)^"_ty"))) args

(*** Operator Overloading ***)

module Resolver =
struct

  (** memo_find: Memoised lookup function. *)
  let memo_find cache find table n =
    try Lib.find n cache
    with Not_found ->
      let ret = find n table
      in
      ignore(Lib.bind n ret cache); ret

  (** reolve_memo: Memoised tables *)
  type resolve_memo =
      {
        types: (Ident.t, Gtype.t)Hashtbl.t;
        idents: (string, Ident.t)Hashtbl.t;
        symbols: (string, Ident.t)Hashtbl.t;
        type_names: (string, Ident.thy_id) Hashtbl.t
      }

  (** resolve_arg: The argument to the resolver *)
  type resolve_arg =
      {
        scp: Scope.t;
        inf: int;
        memo: resolve_memo;
        qnts: Term.Subst.t;
        lookup: (string -> Gtype.t -> (Ident.t * Gtype.t))
      }

  (** [resolve_aux data env expty term]: Resolve names in [term].

      Each type name is expanded to a long name.

      Each free variable [n] (which may be an overloaded symbol
      or name) is resolved to an exact identifier.

      [expty] is the type the term is expected to have (can be
      a variable type).

      Returns the renamed term, the actual type and a type
      substitution from which the exact type can be obtained
      (using Gtype.mgu).

      Never fails.
  *)
  let rec resolve_aux data env expty term =
    let bind_qnt t1 t2 rdata =
      { rdata with qnts = Term.Subst.bind t1 t2 rdata.qnts }
    and binding_set_names binding rdata =
      let (qnt, qname, qtype) = Term.Binder.dest binding
      in
      Term.Binder.make
        qnt qname
        (Ltype.set_name (Some(rdata.memo.type_names)) (rdata.scp) qtype)
    and binding_set_types tyenv binding =
      let (qnt, qname, qtype) = Term.Binder.dest binding
      in
      Term.Binder.make qnt qname (Gtype.mgu qtype tyenv)
    and set_type_name t rdata =
      Ltype.set_name (Some(rdata.memo.type_names)) rdata.scp t
    and find_ident n rdata =
      let ident_find n s =
        let thy = Scope.thy_of_term s n
        in
        Ident.mk_long thy n
      in
      Lib.try_find (memo_find rdata.memo.idents ident_find rdata.scp) n
    and find_type n rdata =
      let type_find n s = Scope.type_of s n
      in
      Lib.apply_option
        (fun x -> Some (Gtype.rename_type_vars x))
        (Lib.try_find (memo_find rdata.memo.types type_find rdata.scp) n)
        None
    and find_sym n ty rdata =
      let find_fn atyp =
        let (x, xty) = rdata.lookup n atyp
        in
        (x, Gtype.rename_type_vars xty)
      in
      Lib.try_find find_fn ty
    and unify_types ty1 ty2 env (rdata: resolve_arg) =
      try Ltype.unify_env rdata.scp ty1 ty2 env
      with _ -> env
    and mk_typevar_ref rdata =
      let (ctr, nty) = Gtype.mk_typevar (rdata.inf) in
      ({ rdata with inf = ctr }, nty)
    in
    match term with
      | PId(n, ty) ->
          if Ident.is_short n
          then resolve_aux data env expty (PFree(Ident.name_of n, ty))
          else
            let id_ty = find_type n data in
            let nty = set_type_name ty data in
            let (ty0, env0) = (nty, unify_types expty nty env data)
            in
            let (ty1, env1) =
              if id_ty = None
              then (ty0, env0)
              else
                let d_ty = Lib.from_some id_ty in
                (d_ty, unify_types ty0 d_ty env0 data)
            in
            (Term.mk_typed_ident n (Gtype.mgu ty1 env1), ty1, env1, data)
      | PFree(n, ty) ->
        let nty = set_type_name ty data in
        let (ty0, env0) = (nty, unify_types expty nty env data ) in
        let ty1 =
          try Gtype.mgu ty0 env0
          with _ -> ty0
        in
        begin
          let id2_opt = find_sym n ty1 data in
          if id2_opt = None
          then
              begin
                let id3_opt = find_ident n data in
                if id3_opt = None
                then (Term.mk_free n ty1, ty1, env0, data)
                else
                  resolve_aux data env0 ty1 (PId(Lib.from_some id3_opt, ty1))
              end
          else
            let (id2, ty2) = Lib.from_some id2_opt in
            resolve_aux data env0 ty1 (PId(id2, ty2))
        end
      | PBound(q) ->
        let term0 = Term.mk_bound(q) in
        let term1 =
          try Term.Subst.find term0 data.qnts
          with Not_found -> term0
        in
        let ty = Term.get_binder_type term1 in
        let (ty0, env0) = (ty, unify_types expty ty env data)
        in
        (term1, ty0, env0, data)
      | PMeta(q) ->
        let ty = Term.Binder.type_of q in
        let (ty0, env0) = (ty, unify_types expty ty env data)
        in
        (Term.Atom(Term.Meta(q)), ty0, env0, data)
      | PConst(c) ->
        let ty = Lterm.typeof_cnst c in
        let (ty0, env0) = (ty, unify_types expty ty env data)
        in
        (Term.mk_const(c), ty0, env0, data)
      | PTyped(trm, ty) ->
        let nty = set_type_name ty data in
        let (ty0, env0) = (nty, unify_types expty nty env data) in
        let (trm1, nty1, env1, data1) = resolve_aux data env0 nty trm in
        let (nty2, env2) = (nty1, unify_types nty nty1 env data1)
        in
        (trm1, nty2, env2, data1)
      | PApp(lf, a) ->
        let (data1, argty) = mk_typevar_ref data in
        let (data2, rty0) = mk_typevar_ref data1 in
        let (rty1, env1) = (rty0, unify_types expty rty0 env data2) in
        let fty0 = Lterm.mk_fun_ty argty rty1 in
        let (atrm, aty, aenv, data3) =
          resolve_aux data2 env1 (Gtype.mgu argty env1) a
        in
        let (ftrm, fty, fenv, data4) =
          resolve_aux data3 aenv (Gtype.mgu fty0 aenv) lf
        in
        (Term.App(ftrm, atrm), Gtype.mgu rty1 fenv, fenv, data4)
      | PQnt(qnt, body) ->
        begin
          match Term.Binder.kind_of qnt with
            | Term.Lambda ->
              let qnt1 = binding_set_names qnt data in
              let aty = Term.Binder.type_of qnt1
              and (data1, rty) = mk_typevar_ref data
              in
              let nty0 = Lterm.mk_fun_ty aty rty in
              let (nty1, env1) = (nty0, unify_types expty nty0 env data) in
              let qnt2 = binding_set_types env1 qnt1 in
              let data2 =
                bind_qnt (Term.mk_bound qnt) (Term.mk_bound qnt2) data1
              in
              let (body1, bty, benv, data3) =
                resolve_aux data2 env1 rty body
              in
              (Term.Qnt(qnt2, body1), nty1, benv, data3)
          | _ ->
            let qnt1 = binding_set_names qnt data in
            let (nty1, env1) =
              let bool_ty = Lterm.mk_bool_ty()
              in
              (bool_ty, unify_types expty bool_ty env data)
            in
            let qnt2 = binding_set_types env1 qnt1 in
            let data1 =
              bind_qnt (Term.mk_bound qnt) (Term.mk_bound qnt2) data
            in
            let (body1, bty, benv, data2) =
              resolve_aux data1 env1 nty1 body
            in
            (Term.Qnt(qnt2, body1), nty1, benv, data2)
        end

  (** [default str ty lst]: Get the default identifier for symbol
      [str] of type [ty] from list [lst] of identifiers when no
      identifier matches.

      Currently, this just raises [Not_found].
  *)
  let default str ty list = None

  (** [resolve_term env t]: Resolve the symbols in term [t].  For
      each free variable [Free(s, ty)] in [t], Lookup [s] in [env] to
      get long identifier [id].  If not found, use [Free(s, ty)].  If
      found, replace [Free(s, ty)] with the identifier [Id(id,
      ty)].  *)
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
        inf = 0;
        memo = rmemo;
        qnts = Term.Subst.empty();
        lookup = lookup
      }
    in
    let expty = Gtype.mk_null() in
    let (term1, _, subst, _) =
      resolve_aux data (Gtype.Subst.empty()) expty term
    in
    (term1, subst)

  (** [find_type scp sym ty list]: return first identifier-type pair for
      symbol [sym] in list where [ty] is the type of [sym].

      Matching is by unification.
  *)
  let find_type scp sym ty list =
    let matching_types t1 t2 =
      try ignore(Ltype.unify scp t1 t2); true
      with _ -> false
    in
    let rec find_aux l =
      match l with
        | [] ->
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

  let make_lookup scp db s ty =
    let type_list = db s in
    let (id, id_type) = find_type scp s ty type_list
    in
    (id, id_type)

end

(*
 * Conversion to-from terms
 *)


let from_term trm =
  let from_atom a =
    match a with
      | Term.Id(n, ty) -> PId (n, ty)
      | Term.Free(n, ty) -> PFree(n, ty)
      | Term.Meta(q) -> PMeta(q)
      | Term.Const(c) -> PConst(c)
  in
  let rec from_aux t =
    match t with
      | Term.Atom(a) -> from_atom a
      | Term.Bound(q) -> PBound(q)
      | Term.App(f, a) -> PApp(from_aux f, from_aux a)
      | Term.Qnt(q, b) -> PQnt(q, from_aux b)
  in
  from_aux trm

let to_term ptrm =
  let scp = Scope.empty_scope() in
  let unify_types ty1 ty2 env =
    try Ltype.unify_env scp ty1 ty2 env
    with _ -> env
  in
  let rec to_aux typenv trmenv expty pt =
    let (ctr, tyenv) = typenv
    in
    match pt with
      |	PId(n, ty) ->
          let env1 = unify_types expty ty tyenv
          in
          (Term.mk_typed_ident n (Gtype.mgu ty env1), (ctr, env1))
      | PFree(n, ty) ->
        let env1 = unify_types expty ty tyenv
        in
        (Term.mk_free n (Gtype.mgu ty env1), (ctr, env1))
      | PBound(q) ->
        let pt1, env1 =
          match (Lib.try_find (Term.Subst.find (Term.mk_bound(q))) trmenv) with
            | None ->
                let ty = Term.Binder.type_of q in
                let env1 = unify_types expty ty tyenv
                in
                (Term.mk_bound(q), (ctr, env1))
            | Some(x) -> (x, typenv)
        in
        (pt1, env1)
      | PMeta(q) ->
        let ty = Term.Binder.type_of q in
        let env1 = unify_types expty ty tyenv
        in
        (Term.Atom(Term.Meta(q)), (ctr, env1))
      | PQnt(q, b) ->
        let qnt, qname, qty = Term.Binder.dest q
        in
        begin
          match qnt with
            | Term.Lambda ->
              let (ctr1, rty) = Gtype.mk_typevar ctr in
              let nty = Lterm.mk_fun_ty qty rty in
              let env1 = unify_types expty nty tyenv in
              let q1 = Term.Binder.make qnt qname (Gtype.mgu qty env1) in
              let trmenv1 =
                Term.Subst.bind (Term.mk_bound q) (Term.mk_bound q1) trmenv
              in
              let (b1, typenv1) = to_aux (ctr1, env1) trmenv1 nty b
              in
              (Term.Qnt(q1, b1), typenv1)
            | _ ->
              let nty = Lterm.mk_bool_ty() in
              let env1 = unify_types expty nty tyenv in
              let q1 = Term.Binder.make qnt qname (Gtype.mgu qty env1) in
              let trmenv1 =
                Term.Subst.bind (Term.mk_bound(q)) (Term.mk_bound(q1)) trmenv
              in
              let (b1, env2) = to_aux (ctr, env1) trmenv1 nty b
              in
              (Term.Qnt(q1, b1), env2)
        end
      | PApp(f, a) ->
        let (ctr1, arg_ty) = Gtype.mk_typevar ctr in
        let (ctr2, ret_ty) = Gtype.mk_typevar ctr1 in
        let (a1, (ctr3, env1)) = to_aux (ctr2, tyenv) trmenv arg_ty a in
        let fn_ty = Lterm.mk_fun_ty arg_ty ret_ty in
        let env2 = unify_types expty ret_ty env1 in
        let (f1, typenv1) = to_aux (ctr3, env2) trmenv fn_ty f
        in
        (Term.App(f1, a1), typenv1)
      | PConst(c) ->
        let ty = Lterm.typeof_cnst c in
        let env1 = unify_types expty ty tyenv
        in
        (Term.mk_const(c), (ctr, env1))
      | PTyped(x, ty) ->
        let env1 = unify_types expty ty tyenv
        in
        to_aux (ctr, env1) trmenv ty x
  in
  let tyvar_ctr = 0 in
  let (tyvar_ctr1, typ1) = Gtype.mk_typevar tyvar_ctr in
  let (trm1, _) =
    to_aux (tyvar_ctr1, Gtype.Subst.empty()) (Term.Subst.empty()) typ1 ptrm
  in
  trm1

(*** Conversion with overloading ***)

let resolve = Resolver.resolve_term
let make_lookup = Resolver.make_lookup
