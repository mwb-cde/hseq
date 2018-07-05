(*----
  Name: term.ml
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
open Basic

(** {6 Representation of a term} *)

module Const =
  struct
    (** [const_ty]: Built-in constants that can appear in terms. *)
    type t =
      | Cbool of bool

    let compare x y =
      if x = y then Order.Equal
      else
        match (x, y) with
        | Cbool(a), Cbool(b) -> Order.Util.compare a b

    let lt x y =
      match (x, y) with
      | Cbool(true), _ -> true
      | Cbool(false), _ -> true

    let leq x y =
      if x = y then true
      else
        match (x, y) with
        | Cbool(true), _ -> true
        | Cbool(false), _ -> true

    let to_string c =
      match c with
      | Cbool b -> string_of_bool b
end

(** Atomic terms *)
type atom =
  | Id of Ident.t * Gtype.t
  | Bound of binders
  | Free of string * Gtype.t
  | Meta of binders
  | Const of Const.t

type term =
  | Atom of atom
  | App of term * term
  | Qnt of binders * term

(* Recognisers *)

let is_atom x =
  match x with
  | Atom(_) -> true
  | _ -> false

let is_qnt x =
  match x with
    | Qnt _ -> true
    | _ -> false

let is_app x =
  match x with
    | App _ -> true
    | _ -> false

let is_bound x =
  match x with
    | Atom(Bound _) -> true
    | _ -> false

let is_free x =
  match x with
    | Atom(Free _) -> true
    | _ -> false

let is_ident x =
  match x with
    | Atom(Id _) -> true
    | _ -> false

let is_const x =
  match x with
    | Atom(Const _) -> true
    | _ -> false

let is_true t =
  match t with
    | Atom(Const (Const.Cbool true)) -> true
    | _ -> false

(* Constructors *)

let mk_atom x = Atom(x)

let mk_qnt b t = Qnt(b, t)
let mk_bound n = mk_atom (Bound n)
let mk_free n ty = mk_atom (Free(n, ty))
let mk_const c = mk_atom(Const c)

let mk_app f a = (App(f, a))

let mk_typed_ident n t = mk_atom (Id(n, t))
let mk_ident n = mk_typed_ident n (Gtype.mk_null ())
let mk_short_ident n = mk_ident (Ident.mk_name n)

(* Destructors *)

let dest_atom x =
  match x with
  | Atom(y) -> y
  | _ -> raise (Failure "dest_atom: Not an atomic term")

let dest_qnt t =
  match t with
    | (Qnt(q, b)) -> (q, b)
    | _  -> raise (Failure "Not a quantifier")

let dest_bound t =
  match t with
    | Atom(Bound(n)) -> n
    | _ -> raise (Failure "dest_bound: Not a binder")

let dest_free t =
  match t with
    | Atom(Free(n, ty)) -> (n, ty)
    | _ -> raise (Failure "Not a free variable")

let dest_ident vt =
  match vt with
    | Atom((Id (n, t))) -> (n, t)
    | _ -> raise (Failure "Not a variable")

let dest_app t =
  match t with
    | (App(f, a)) -> (f, a)
    | _ -> raise (Failure "Not a function")

let dest_const t =
  match t with
    | Atom(Const c) -> c
    | _ -> raise (Failure "Not a constant")


(* Specialised Manipulators *)

(* Meta variables *)

let is_meta trm =
  match trm with
    | Atom(Meta _) -> true
    | _ -> false

let mk_meta n ty = mk_atom(Meta (mk_binding Gamma n ty))

let dest_meta t  =
  match t with
    | Atom(Meta(q)) -> q
    | _ -> raise (Failure "Not a meta variable")

(** Constants *)

let destbool b =
  match dest_const b with
    | (Const.Cbool c) -> c

let mk_bool b = mk_const (Const.Cbool b)

(*
 * Function application
 *)

(**
   [mk_comb x y]: Make a function application from [x] and [y].
   [mk_comb f [a1;a2;...;an]] is [((((f a1) a2) ...) an)].
*)
let rec mk_comb x y =
  match y with
    | [] -> x
    | t::ts -> mk_comb (mk_app x t) ts

let mk_fun f args =
  let f_ty = Gtype.mk_var ("_"^(Ident.string_of f)^"_ty")
  in
  mk_comb (mk_typed_ident f f_ty) args

(**
   [flatten_app trm]: flatten an application in [trm] to a list of
   terms.  [flatten_app (((f a1) a2) a3)] is [[f; a1; a2; a3]] and
   [flatten_app (((f a1) (g a2)) a3)] is [[f; a1; (g a2); a3]]
*)
let rec flatten_app trm =
  let rec flat_aux t rslt =
    match t with
      | App(l, r) -> flat_aux l (r::rslt)
      | _ -> t::rslt
  in
  flat_aux trm []

let get_fun_args t =
  match (flatten_app t) with
    | [] -> failwith "Term.get_fun_args"
    | f::args -> (f,args)

let get_fun t =
  let (f, _) = get_fun_args t
  in f

let get_args t =
  let (_, args) = get_fun_args t
  in args

let is_fun t =
  (is_app t) && is_ident (get_fun t)

let rator t =
  match t with
    | App(f, _) -> f
    | _ -> raise (Failure "rator: Not an application")

let rand t =
  match t with
    | App(_, a) -> a
    | _ -> raise (Failure "rand: Not an application")

let dest_fun t =
  try
    let (f, args) = get_fun_args t
    in
    (fst(dest_ident f), args)
  with _ -> raise (Failure "Not a function")

let dest_unop t =
  match (dest_fun t) with
    | (id, x::_) -> (id, x)
    | _ -> raise (Failure "not a unary operator")

let dest_binop t =
  match (dest_fun t) with
    | (id, x::y::_) -> (id, x, y)
    | _ -> raise (Failure "not a binary operator")


(** Comparisons and orderings. *)

(** Ordering on atomic-terms: Const < Id < Bound *)
let compare_bound (x: Basic.binders) y = Basic.binder_compare x y

let compare_atom_strict typed t1 t2 =
  let compare_types rslt ty1 ty2 =
    if rslt <> Order.Equal
    then rslt
    else
      begin
        if typed
        then Gtype.compare ty1 ty2
        else Order.Equal
      end
  in
  match (t1, t2) with
  | (Const c1, Const c2) -> Const.compare c1 c2
  | (Const _ , _) -> Order.LessThan
  | (Id _, Const _) -> Order.GreaterThan
  | (Id(n1, ty1), Id(n2, ty2)) ->
     compare_types (Ident.compare n1 n2) ty1 ty2
  | (Id _, _) -> Order.LessThan
  | (Meta _, Const _) -> Order.GreaterThan
  | (Meta _, Id _) -> Order.GreaterThan
  | (Meta b1, Meta b2) -> compare_bound b1 b2
  | (Meta _, _) -> Order.LessThan
  | (Bound _, Const _) -> Order.GreaterThan
  | (Bound _, Id _) -> Order.GreaterThan
  | (Bound _, Meta _) -> Order.GreaterThan
  | (Bound b1, Bound b2) -> compare_bound b1 b2
  | (Bound _ , _ ) -> Order.LessThan
  | (Free _, Const _) -> Order.GreaterThan
  | (Free _, Id _) -> Order.GreaterThan
  | (Free _, Meta _) -> Order.GreaterThan
  | (Free _, Bound _) -> Order.GreaterThan
  | (Free(n1, ty1), Free(n2, ty2)) ->
     compare_types (Order.Util.compare n1 n2) ty1 ty2

(** Ordering on terms.  Const < Id < Bound < App < Qnt < t2 iff t1<t2 *)
let rec compare_term_strict typed t1 t2 =
  match (t1, t2) with
  | (Atom(a1), Atom(a2)) -> compare_atom_strict typed a1 a2
  | (Atom _, _) -> Order.LessThan
  | (App _, Atom _) -> Order.GreaterThan
  | (App(f1, a1), App (f2, a2)) ->
     begin
       match compare_term_strict typed f1 f2 with
       | Order.Equal -> compare_term_strict typed a1 a2
       | x -> x
     end
  | (App _, _) -> Order.LessThan
  | (Qnt _, Atom _) -> Order.GreaterThan
  | (Qnt _, App _) -> Order.GreaterThan
  | (Qnt(q1, b1), Qnt(q2, b2)) ->
     begin
       match compare_term_strict typed b1 b2 with
       | Order.Equal -> compare_bound q1 q2
       | x -> x
     end

let compare_term = compare_term_strict true
let compare = compare_term

(* Equality and relationships. *)

let equals x y = (compare_term x y) = Order.Equal
let term_leq x y =
  match compare_term x y with
  | Order.Equal | Order.LessThan -> true
  | _ -> false

let lessthan x y = (compare_term x y) = Order.LessThan
let term_lt x y = (compare_term_strict false x y) = Order.LessThan
let term_gt t1 t2 = not (term_leq t2 t1)
let term_leq t1 t2 =
  match compare_term_strict false t1 t2 with
  | Order.Equal | Order.LessThan -> true
  | _ -> false

let rec is_subterm x y =
  if (equals x y)
  then true
  else
    match y with
      | App (f, a) -> (is_subterm x f) || (is_subterm x a)
      | Qnt (_, b) -> (is_subterm x b)
      | _ -> false

(** More manipulators. *)

let rec strip_fun_qnt f term qs =
  let is_ident_match t =
    if (is_ident t)
    then
      let (n, _) = dest_ident t
      in
      f = n
    else false
  in
  let is_lambda b =
    (let (q, _, _) = Basic.dest_binding b
     in
     q = Basic.Lambda)
  in
  match term with
    | App(l, Qnt(q, body)) ->
      if not((is_ident_match l) && (is_lambda q))
      then (qs, term)
      else strip_fun_qnt f body (q::qs)
    | _ -> (List.rev qs, term)

(* Identifier (Id) terms *)

let get_ident_id vt = fst (dest_ident vt)
let get_ident_type vt = snd (dest_ident vt)

(* Free variables *)

let get_free_name t =
  match t with
    | Atom(Free(n, ty)) -> n
    | _ -> raise (Failure "Not a free variable")

let get_free_vars trm =
  let rec get_free_vars_aux t ts =
    match t with
      | Atom(Free(x, ty)) -> t::ts
      | Qnt(q, b) -> get_free_vars_aux b ts
      | App(f, a) -> get_free_vars_aux a (get_free_vars_aux f ts)
      | _ -> ts
  in get_free_vars_aux trm []

(* Quantified and bound terms *)

let get_binder t =
  match t with
    | Atom(Bound(b)) -> b
    | Qnt(b, _) -> b
    | Atom(Meta(b)) -> b
    | _ -> raise (Failure "get_binder: No binder in term")

let get_binder_name x =
  match x with
    | Atom(Bound(n)) -> binder_name n
    | Qnt(n, _) -> binder_name n
    | Atom(Meta(n)) -> binder_name n
    | _ -> raise (Failure "get_binder_name: Not a binder")

let get_binder_type x =
  match x with
    | Atom(Bound(n)) -> Basic.binder_type n
    | Qnt(n, _) -> Basic.binder_type n
    | Atom(Meta(n)) -> Basic.binder_type n
    | _ -> raise (Failure "get_binder_type: Not a binder")

let get_binder_kind x =
  match x with
    | Atom(Bound(n)) -> Basic.binder_kind n
    | Qnt(n, _) -> Basic.binder_kind n
    | Atom(Meta(n)) -> Basic.binder_kind n
    | _ -> raise (Failure "get_binder_kind: Not a binder")

let get_qnt_body t =
  match t with
    | Qnt(_, b) -> b
    | _ -> raise (Failure "Not a quantified formula")


let strip_qnt q trm =
  let rec strip_aux t qs =
    if is_qnt t
    then
      let (bind, b) = dest_qnt t
      in
      if (Basic.binder_kind bind) = q
      then (strip_aux b (bind::qs))
      else (qs, t)
    else (qs, t)
  in
  let (qnts, nt) = strip_aux trm []
  in
  (List.rev qnts, nt)

let rec rebuild_qnt qs b =
  match qs with
    | [] -> b
    | (x::xs) -> Qnt(x, rebuild_qnt xs b)


(*
 * Error handling
 *)

type error = { msg: string; terms: (term)list; next: (exn)option }
exception Error of error

let mk_error s ts = { msg = s; terms = ts; next = None }
let term_error s ts = Error (mk_error s ts)
let add_term_error s ts n =
  Error{ msg = s; terms = ts; next = Some(n) }

(*
 * Simple pretty printing
 *)

let print_atom_simple trm =
  match trm with
  | Id(n, ty) ->
     let (th, x) = Ident.dest n
     in
     Format.printf "@[%s@]" (th^"."^x)
  | Bound(n) ->
     Format.printf "@[%s@]" (".."^(binder_name n))
  | Free(n, ty) -> Format.printf "@[%s@]"  n
  | Meta(n) ->
     Format.printf "@[%s@]" (binder_name n)
  | Const(c) -> Format.printf "@[%s@]" (Const.to_string c)


let print_simple trm =
  let rec print_aux t =
    match t with
      | Atom(x) -> print_atom_simple x
      | App(t1, t2) ->
        Format.printf "@[<2>(";
        print_aux t1;
        Format.printf "@ ";
        print_aux t2;
        Format.printf ")@]"
      | Qnt(q, body) ->
        Format.printf "@[<2>%s"  (Basic.quant_string (get_binder_kind t));
        Format.printf "(%s:@ %s) :@ "
          (binder_name q)
          (Gtype.string_gtype (binder_type q));
        print_aux body;
        Format.printf "@]"
  in
  print_aux trm


(* [('a)Tree.t]: Balanced trees indexed by terms *)
module Tree =
  struct
    module TermTreeData =
      struct
        type key = term
        let compare = compare
      end

    module TermTree = Treekit.BTree(TermTreeData)
    type ('a)t = ('a) TermTree.t

    let empty() = TermTree.empty
    let find x env = TermTree.find env x
    let bind t r env = TermTree.replace env t r
    let remove t env = TermTree.delete env t
    let member t env =
      try ignore(find t env); true
      with Not_found -> false
  end


(** [rename t], [rename_env tyenv trmenv t]: Rename terms.

    Renames the variables in a term [t] which are bound by a binder in
    [t] needs substitutions
*)

let rename_env typenv trmenv trm =
  let copy_binder q tyenv =
    let qnt, qv, qty = Basic.dest_binding q in
    let nt, nev = Gtype.rename_type_vars_env tyenv qty
    in
    (mk_binding qnt qv nt, nev)
  in
  let rec rename_aux t r_env =
    let (tyenv, env, qntd) = r_env
    in
    match t with
      | Atom(Bound(_)) ->
        (try (Tree.find t env, tyenv, env, qntd)
         with Not_found -> (t, tyenv, env, qntd))
      | Qnt(q, b) ->
        let nq, tyenv1 = copy_binder q tyenv in
        let env1 = Tree.bind (Atom(Bound(q))) (Atom(Bound(nq))) env in
        let (nb, tyenv2, env2, _) = rename_aux b (tyenv1, env1, qntd)
        in
        (Qnt(nq, nb), tyenv2, env2, true)
      | App(f, a) ->
        let (nf, tyenv1, env1, qntd1) = rename_aux f (tyenv, env, qntd) in
        let (na, tyenv2, env2, qntd2) = rename_aux a (tyenv1, env1, qntd1)
        in
        (App(nf, na), tyenv2, env2, qntd2)
      | _ -> (t, tyenv, env, qntd)
  in
  let (t, ntyenv, nenv, qntd) = rename_aux trm (typenv, trmenv, false)
  in
  if qntd
  then Some(t, ntyenv, nenv)
  else None

(* [rename_opt t] Rename term [t] (carry out alpha conversion on [t]).
   Return [None] if no change is needed (no binders or bound terms) *)
let rename_opt t =
  let rslt_opt = rename_env (Gtype.Subst.empty()) (Tree.empty()) t
  in
  if rslt_opt = None
  then None
  else
    begin
      let new_term, _, _ = Lib.from_some rslt_opt in
      Some(new_term)
    end

(* [rename t] Rename term [t] (carry out alpha conversion on [t]) *)
let rename t =
  let rslt_opt = rename_opt t in
  if rslt_opt = None
  then t
  else Lib.from_some rslt_opt

(*
 * Substitution in terms
 *)
module Subst =
  struct

    (** [type subst_terms]: Terms stored in a substitution.
     *)
    type subst_alt = Rename | No_rename | Unknown
    type subst_terms = ST of term * (subst_alt ref)

    let set_subst_alt (ST(_, x)) a = x := a
    let st_term (ST(t, _)) = t
    let st_choice (ST(_, a)) = !a
    let sterm t a = ST(t, ref a)

    (* [substitution]: the data structure holding the substitution to be
       made in a term.  *)
    type t = (subst_terms)Tree.t

    let empty() = ((Tree.empty()): t)
    let find x env = st_term (Tree.find x env)
    let bind t r env = Tree.bind t (sterm r Unknown) env
    let remove = Tree.remove
    let member = Tree.member

    (* [do_rename t]: Get the term of subst_term [t], renaming if
   necessary.
     *)
    let do_rename nb =
      match st_choice nb with
      | Rename -> rename (st_term nb)
      | No_rename -> st_term nb
      | Unknown ->
         let nt_opt = rename_opt (st_term nb)
         in
         if nt_opt = None
         then (set_subst_alt nb No_rename; (st_term nb))
         else (set_subst_alt nb Rename; (Lib.from_some nt_opt))

    let replace env x = do_rename (Tree.find x env)

    (*
     * Chase functions
     *)
    let rec chase varp x env =
      try
        let t = find x env
        in
        if (varp t)
        then (chase varp t env)
        else t
      with Not_found -> x

    let fullchase varp x env =
      let t1 = chase varp x env
      in
      if varp t1 then x else t1

    let chase_var varp x env =
      let rec chase_var_aux r =
        let y = st_term r
        in
        if varp y
        then
          try chase_var_aux (Tree.find y env)
          with Not_found -> r
        else r
      in
      let nb = chase_var_aux (sterm x Unknown)
      in
      if not (equals x (st_term nb))
      then do_rename nb
      else x
end
(**
   Retyping.

   [retype tyenv t]: Retype term [t], substituting type variables
   occuring in [t] with types in type substitution [tyenv].
*)

let retype_atom tyenv qenv t =
  match t with
  | Id(n, ty) -> Id(n, Gtype.mgu ty tyenv)
  | Free(n, ty) -> Free(n, Gtype.mgu ty tyenv)
  | Bound(q) ->
     (try Tree.find (Atom(t)) qenv with Not_found -> t)
  | _ -> t

let retype tyenv t =
  let rec retype_aux t qenv =
    match t with
    | Atom(a) ->
       let a1 = retype_atom tyenv qenv a
       in
       (Atom(a1), qenv)
    | App(f, a) ->
       let f1, env1 = retype_aux f qenv in
       let a1, env2 = retype_aux a env1 in
       (App(f1, a1), env2)
    | Qnt(q, b) ->
       let (oqnt, oqnm, oqty) = Basic.dest_binding q in
       let nty = Gtype.mgu oqty tyenv in
       let q1 = mk_binding oqnt oqnm nty
       in
       let qenv1 = Tree.bind (Atom(Bound(q))) (Bound(q1)) qenv in
       let (b1, _) = retype_aux b qenv1
       in
       (Qnt(q1, b1), qenv)
  in
  let (ret, _) = retype_aux t (Tree.empty()) in
  ret

(* retype_pretty: as for retype, make substitution for type variables
   but also replace other type variables with new, prettier names
*)
let retype_pretty_env typenv trm =
  let retype_binder q ctr name_env qenv =
    let (qnt, qnm, qty) = Basic.dest_binding q in
    let (nty, (ctr1, nenv1)) =
      Gtype.mgu_rename_env (ctr, typenv) name_env qty
    in
    let q1 = mk_binding qnt qnm nty in
    let qenv1 = Tree.bind (Atom(Bound q)) (Bound q1) qenv
    in
    (q1, ctr1, nenv1, qenv1)
  in
  let retype_atom t ctr name_env qenv =
    match t with
      | Id(n, ty) ->
        let (nt, (ctr1, nenv1)) =
          Gtype.mgu_rename_env (ctr, typenv) name_env ty
        in
        (Id(n, nt), ctr1, nenv1, qenv)
      | Free(n, ty) ->
        let (nt, (ctr1, nenv1)) =
          Gtype.mgu_rename_env (ctr, typenv) name_env ty
        in
        (Free(n, nt), ctr, nenv1, qenv)
      | Meta(q) -> (t, ctr, name_env, qenv)
      | Const(c) -> (t, ctr, name_env, qenv)
      | Bound(q) ->
         begin
           try (Tree.find (Atom t) qenv, ctr, name_env, qenv)
           with
             Not_found ->
              let (q1, ctr1, nenv1, qenv1) =
                retype_binder q ctr name_env qenv
              in
              (Bound(q1), ctr1, nenv1, qenv1)
         end
  in
  let rec retype_aux t ctr name_env qenv =
    match t with
    | Atom(a) ->
       let (a1, ctr1, nenv1, qenv1) = retype_atom a ctr name_env qenv in
       (Atom(a1), ctr1, nenv1, qenv1)
      | App(f, a) ->
        let nf, ctr1, nenv1, qenv1 = retype_aux f ctr name_env qenv in
        let na, ctr2, nenv2, qenv2 = retype_aux a ctr1 nenv1 qenv1
        in
        (App(nf, na), ctr2, nenv2, qenv2)
      | Qnt(q, b) ->
         let (q1, ctr1, nenv1, qenv1) = retype_binder q ctr name_env qenv
         in
         let (b1, ctr2, nenv2, qenv2) = retype_aux b ctr1 nenv1 qenv1
         in
         (Qnt(q1, b1), ctr2, nenv2, qenv)
  in
  let (retyped, _, new_nenv, _) =
    retype_aux trm 0 (Gtype.Subst.empty()) (Tree.empty())
  in
  (retyped, new_nenv)

let retype_pretty typenv trm =
  let new_term, _ = retype_pretty_env typenv trm
  in
  new_term

(*
 * Combined type/binder renaming.
 *)

(** [full_rename env t]: Rename all type variables and bound variables
    in term [t]. *)
let full_rename_env type_env term_env trm =
  let rename_type tyenv trm =
    Gtype.rename_type_vars_env tyenv trm
  in
  let rename_binder q tyenv =
    let (qnt, qv, qty) = Basic.dest_binding q in
    let (nt, nev) = rename_type tyenv qty
    in
    (mk_binding qnt qv nt, nev)
  in
  let rename_atom t tyenv env =
    match t with
    | Id(n, ty) ->
       let (ty1, tyenv1) = rename_type tyenv ty
       in
       (Id(n, ty1), tyenv1)
    | Free(n, ty) ->
       let (ty1, tyenv1) = rename_type tyenv ty
       in
       (Free(n, ty1), tyenv1)
    | Meta(q) -> (t, tyenv)
    | Const(c) -> (t, tyenv)
    | Bound(q) ->
       let t1 = try (Tree.find (Atom(t)) env) with Not_found -> t
       in
       (t1, tyenv)
  in
  let rec rename_aux t tyenv env =
    match t with
      | Atom(a) ->
         let (a1, tyenv1) = rename_atom a tyenv env
         in
         (Atom(a1), tyenv1)
      | Qnt(q, b) ->
        let (q1, tyenv1) = rename_binder q tyenv in
        let env1 = Tree.bind (Atom(Bound(q))) (Bound(q1)) env in
        let (b1, tyenv2) = rename_aux b tyenv1 env1
        in
        (Qnt(q1, b1), tyenv2)
      | App(f, a) ->
        let f1, tyenv1 = rename_aux f tyenv env in
        let a1, tyenv2 = rename_aux a tyenv1 env
        in
        (App(f1, a1), tyenv2)
  in
  rename_aux trm type_env term_env

let full_rename tyenv trm =
  let trmenv = Tree.empty()
  in
  let (ntrm, ntyenv) = full_rename_env tyenv trmenv trm
  in
  (ntrm, ntyenv)


let retype_index idx trm =
  let rename_type ctr tyenv ty =
    Gtype.rename_index ctr tyenv ty
  in
  let rename_binder q ctr tyenv =
    let (qnt, qv, qty) = Basic.dest_binding q in
    let (nty, ctr1, tyenv1) = rename_type ctr tyenv qty in
    let nbnd = mk_binding qnt qv nty
    in
    (nbnd, ctr1, tyenv1)
  in
  let rename_atom t ctr tyenv env =
    match t with
      | Id(n, ty) ->
        let (ty1, ctr1, tyenv1) = rename_type ctr tyenv ty
        in
        (Id(n, ty1), ctr1, tyenv1)
      | Free(n, ty) ->
        let (ty1, ctr1, tyenv1) = rename_type ctr tyenv ty
        in
        (Free(n, ty1), ctr1, tyenv1)
      | Meta(q) -> (t, ctr, tyenv)
      | Const(c) -> (t, ctr, tyenv)
      | Bound(q) ->
        let t1 = try (Tree.find (Atom(t)) env) with Not_found -> t
        in
        (t1, ctr, tyenv)
  in
  let rec rename_aux t ctr tyenv env =
    match t with
      | Atom(a) ->
         let (a1, ctr1, tyenv1) = rename_atom a ctr tyenv env
         in
         (Atom(a1), ctr1, tyenv1)
      | Qnt(q, b) ->
        let (q1, ctr1, tyenv1) = rename_binder q ctr tyenv in
        let env1 = Tree.bind (Atom(Bound(q))) (Bound(q1)) env in
        let (b1, ctr2, tyenv2) = rename_aux b ctr1 tyenv1 env1
        in
        (Qnt(q1, b1), ctr2, tyenv2)
      | App(f, a) ->
        let f1, ctr1, tyenv1 = rename_aux f ctr tyenv env in
        let a1, ctr2, tyenv2 = rename_aux a ctr1 tyenv1 env
        in
        (App(f1, a1), ctr2, tyenv2)
  in
  rename_aux trm idx (Gtype.Subst.empty()) (Tree.empty())

let rename_env typenv trmenv trm =
  let copy_binder q tyenv =
    let qnt, qv, qty = Basic.dest_binding q in
    let nt, nev = Gtype.rename_type_vars_env tyenv qty
    in
    (mk_binding qnt qv nt, nev)
  in
  let rec rename_aux t renv =
    let (tyenv, env, qntd) = renv
    in
    match t with
      | Atom(Bound(_)) ->
        (try (find t env, tyenv, env, qntd)
         with Not_found -> (t, tyenv, env, qntd))
      | Qnt(q, b) ->
        let nq, tyenv1 = copy_binder q tyenv in
        let env1 = bind (Atom(Bound(q))) (Atom((Bound(nq)))) env in
        let (nb, tyenv2, env2, _) = rename_aux b (tyenv1, env1, qntd)
        in
        (Qnt(nq, nb), tyenv2, env2, true)
      | App(f, a) ->
        let (nf, tyenv1, env1, qntd1) = rename_aux f (tyenv, env, qntd) in
        let (na, tyenv2, env2, qntd2) = rename_aux a (tyenv, env1, qntd1)
        in
        (App(nf, na), tyenv2, env2, qntd2)
      | _ -> (t, tyenv, env, qntd)
  in
  let (t, ntyenv, nenv, qntd) = rename_aux trm (typenv, trmenv, false)
  in
  if not qntd
  then None
  else Some(t, ntyenv, nenv)

(*
 * Substitution functions
 *)
let rec subst env trm =
  try
    let nt= Subst.replace env trm
    in
    subst env nt
  with Not_found ->
    match trm with
      | Qnt(q, b) -> Qnt(q, subst env b)
      | App(f, a) -> App(subst env f, subst env a)
      | _ -> trm

let qsubst ts trm =
  let env =
    List.fold_left (fun e (t, r) -> Subst.bind t r e) (Subst.empty()) ts
  in
  subst env trm

let subst_mgu varp env trm =
  let rec mgu_aux t=
    try
      let nt = Subst.replace env (Subst.chase_var varp t env)
      in
      mgu_aux nt
    with Not_found ->
      match trm with
        | Qnt(q, b) -> Qnt(q, mgu_aux b)
        | App(f, a) -> App(mgu_aux f, mgu_aux a)
        | _ -> trm
  in mgu_aux trm

(*
 * Operations using substitution.
 *)

let inst t r =
  if is_qnt t
  then
    let (q, b) = dest_qnt t
    in
    qsubst [((mk_bound q), r)] b
  else raise (Failure "inst: not a quantified formula")

(*
 * Debugging and printing
 *)

let string_typed_name n t =
  ("("^n^": "^(Gtype.string_gtype t)^")")

let string_atom t =
  match t with
  | Id(n, ty) -> (Ident.string_of n)
  | Free(n, ty) -> n
  | Bound(q) -> "?"^(Basic.binder_name q)
  | Meta(q) -> Basic.binder_name q
  | Const(c) -> Const.to_string c

let rec string_term_basic t =
  match t with
  | Atom(a) -> string_atom a
  | App(t1, t2) ->
     let f = get_fun t
     and args = get_args t
     in
     ("("^(string_term_basic f)^" ("
      ^(Lib.list_string (string_term_basic ) ", " args)^"))")
  | Qnt(q, body) ->
     (quant_string (get_binder_kind t))
     ^" ("^(string_typed_name (Basic.binder_name q)
              (Basic.binder_type q))^"): "
     ^(string_term_basic body)

(** Precedence of Quantifiers *)
let prec_qnt q =
  match q with
      Lambda -> 60
    | All -> 55
    | Ex -> 55
    | _ -> 55

let rec string_term_prec i x =
  match x with
  | Atom(a) -> string_atom a
  | Qnt(q, body) ->
     let qnt = Basic.binder_kind q in
     let ti = prec_qnt qnt
     in
     if ti <= i
     then ("("^(quant_string qnt)
           ^" "^(string_typed_name
                   (Basic.binder_name q)
                   (Basic.binder_type q))^". "
           ^(string_term_prec ti (body))^")")
     else
       ((quant_string qnt)
        ^" "^(string_typed_name (Basic.binder_name q)
                (Basic.binder_type q))^". "
        ^(string_term_prec ti (body)))
  | App(t1, t2) ->
     let f = get_fun x
     and args = get_args x
     in
     ("("^(string_term_prec i f)^" "
      ^(list_string (string_term_prec i) " " args)^")")

let string_term x = string_term_prec 0 x

let string_infix f ls =
  match ls with
    | [l;r] -> (l^f^r)
    | _ -> raise (Failure ("Too many arguments to infix "^f^"."))

let cfun_string c =
  match c with
    | "not" -> "not "
    | "and" -> " and "
    | "or" -> " or "
    | "implies" -> " => "
    | "iff" -> "<=>"
    | "equals" -> "="
    | x -> x

let string_atom_inf t =
  match t with
  | Id(n, ty) -> cfun_string (Ident.string_of n)
  | Free(n, ty) -> n
  | Bound(q) -> (Basic.binder_name q)
  | Meta(q) -> Basic.binder_name q
  | Const(c) -> Const.to_string c

let rec string_term_inf inf i x =
  match x with
  | Atom(a) -> string_atom a
  | App(t1, t2) ->
     let f=get_fun x
     and args = get_args x
     in
     if is_ident f
     then
       let name = fst (dest_ident f) in
       let pr = try (fst(inf) name) with _ -> -1 in
       let ti = if pr <= i then pr else i
       in
       if try (snd(inf)) name with _ -> false
       then
         string_infix
           (cfun_string (Ident.string_of name))
           (List.map (string_term_inf inf ti) args)
       else
         ("("^(string_term_inf inf i f)^" "
          ^(list_string (string_term_inf inf i) " " args)^")")
     else
       ("("^(string_term_inf inf i f)^" "
        ^(list_string (string_term_inf inf i) " " args)^")")
  | Qnt(q, body) ->
     let qnt = Basic.binder_kind q in
     let (qnts, b) = strip_qnt qnt x in
     let qnts_str =
       (quant_string qnt)
       ^(list_string
           (fun x -> (string_typed_name (Basic.binder_name x)
                        (Basic.binder_type x)))
           " " qnts)^": "
     in
     let ti = prec_qnt qnt
     in
     if ti < i
     then ("("^qnts_str^(string_term_inf inf ti (b))^")")
     else (qnts_str^(string_term_inf inf ti (b)))

let string_inf_term inf x = string_term_inf inf 0 x


let least ts =
  let rec less_aux l xs =
    match xs with
      | [] -> l
      | y::ys ->
        if term_lt y l
        then less_aux y ys
        else less_aux l ys
  in
  match ts with
    | [] -> raise (term_error "least: No arguments" [])
    | (x::xs) -> less_aux x xs
