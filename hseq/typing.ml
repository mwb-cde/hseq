(*----
  Name: typing.ml
  Copyright Matthew Wahab 2005-2018
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

open Term

(*** Type Errors ***)

let print_typing_error s tr expty ty fmt pinfo =
    Format.fprintf fmt "@[%s@ expected type@ " s;
    Printers.print_type pinfo expty;
    Format.fprintf fmt "@ got type@ ";
    Printers.print_type pinfo ty;
    Format.fprintf fmt "@ in term@ ";
    Printers.print_term pinfo tr;
    Format.fprintf fmt "@]"

let typing_error s tr expty ty =
  Report.mk_error(print_typing_error s tr expty ty)
let add_typing_error s tr expty ty es =
  Report.add_error (typing_error s tr expty ty) es

(*
 * Typing a term
 *)

let typeof_env scp (inf, typenv) trm =
  let typeof_atom t tyenv =
    let (ctr, env) = tyenv in
    match t with
    | Id(n, ty) -> (Gtype.mgu ty env, tyenv)
    | Free(n, ty) -> (Gtype.mgu ty env, tyenv)
    | Meta(q) -> (Gtype.mgu (Term.Binder.type_of q) env, tyenv)
    | Bound(q) -> (Gtype.mgu (Term.Binder.type_of q) env, tyenv)
    | Const(c) -> (Lterm.typeof_cnst c, tyenv)
  in
  let rec typeof_aux t tyenv =
    let (ctr, env) = tyenv
    in
    match t with
    | Atom(a) -> typeof_atom a tyenv
    | Qnt(q, b) ->
       begin
         match Term.Binder.kind_of q with
         | Term.Lambda ->
            let (btyp, (ctr1, benv)) = typeof_aux b (ctr, env)
            in
            (Lterm.mk_fun_ty (Term.get_binder_type t) btyp, (ctr1, benv))
         | _ -> (Lterm.mk_bool_ty(), (ctr, env))
       end
    | App(f, a) ->
       let fty, (ctr1, fenv) = typeof_aux f (ctr, env) in
       let aty, (ctr2, aenv) = typeof_aux a (ctr1, fenv)
       in
       let (ctr3, retty) = Gtype.mk_typevar ctr2 in
       let nty = Lterm.mk_fun_ty aty retty in
       let renv = Ltype.unify_env scp fty nty aenv
       in
       (Gtype.mgu retty renv, (ctr3, renv))
  in
  typeof_aux trm (inf, typenv)

let typeof scp ?env t =
  let tenv =
    match env with
      | None -> Gtype.Subst.empty()
      | Some x -> x
  in
  let (ty, _) = typeof_env scp (0, tenv) t
  in ty

(*
 * Type-checking
 *)

(** Two flavours:

    [settype] looks up the type of each identifier in the scope before
    type checking.

    [typecheck] assumes that identifiers have already been assigned
    their correct types.
*)

(* Wrapper around Gtype.well_formed to raise an error on failure *)
let check_type_valid scp ty =
  if not (Ltype.well_formed scp ty)
  then raise (Gtype.type_error "malformed type" [ty])
  else ()

(*** typecheck based type checking ****)

let typecheck_aux scp (inf, cache) typenv exty et =
  let test_type scope env trm ty expty =
    (* Check given type is valid *)
    check_type_valid scope ty;
    (* unify with expected type *)
    try Ltype.unify_env scope ty expty env
    with err -> raise (add_typing_error
                         "Typechecking: " trm
                         (Gtype.mgu expty env)
                         (Gtype.mgu ty env) err)
  in
  let type_atom expty atm trm tyenv =
    let (ctr, env) = tyenv
    in
    match atm with
      | Id(n, ty) -> (ctr, test_type scp env trm ty expty)
      | Free(n, ty) -> (ctr, test_type scp env trm ty expty)
      | Meta(q) ->
        let ty = Term.Binder.type_of q
        in
        (ctr, test_type scp env trm ty expty)
      | Bound(q) ->
        let ty = Term.Binder.type_of q
        in
        (ctr, test_type scp env trm ty expty)
      | Const(c) ->
        let ty = Lterm.typeof_cnst c
        in
        (ctr, test_type scp env trm ty expty)
  in
  let rec type_aux expty trm tyenv =
    let (ctr, env) = tyenv
    in
    match trm with
      | Atom(a) -> type_atom expty a trm tyenv
      | App(f, a) ->
        (* Make an argument type *)
        let (ctr1, aty) = Gtype.mk_plain_typevar ctr in
        (* Check argument type *)
        let (ctr2, aenv) =
          try type_aux aty a (ctr1, env)
          with err -> raise (Term.add_term_error "Typechecking: " [trm] err)
        in
        (* Expect a function type *)
        let fty = Lterm.mk_fun_ty aty expty in
        (* Check function type *)
        let (ctr3, fenv) =
          try type_aux fty f (ctr2, aenv)
          with err -> raise (Term.add_term_error "Typechecking:" [trm] err)
        in
        (ctr3, fenv)
      | Qnt(q, b) ->
        begin
          match Term.Binder.kind_of q with
            | Term.Lambda ->
              let (ctr1, rty) = Gtype.mk_plain_typevar ctr     (* range *)
              and fty = Term.get_binder_type trm  (* domain *)
              in
              (* check domain *)
              check_type_valid scp fty;
              let bty = Lterm.mk_fun_ty fty rty in (* type of term *)
              let env1=
                try Ltype.unify_env scp bty expty env
                with err ->
                  raise (add_typing_error
                           "Typechecking: "
                           trm
                           (Gtype.mgu expty env) (Gtype.mgu bty env)
                           err)
              in
              begin
                try type_aux rty b (ctr1, env1)
                with err -> raise (Term.add_term_error "Typecheck: " [trm] err)
              end
            | _ ->
              let (ctr1, env1) = type_aux (Lterm.mk_bool_ty()) b tyenv in
              let env2 =
                try Ltype.unify_env scp (Lterm.mk_bool_ty()) expty env1
                with err ->
                  raise (add_typing_error "Typechecking: " trm
                           (Gtype.mgu expty env)
                           (Gtype.mgu (Lterm.mk_bool_ty()) env)
                           err)
              in
              (ctr1, env2)
        end
  in
  try type_aux exty et (inf, typenv)
  with err ->
    raise (Term.add_term_error "Typecheck: badly typed" [et] err)

let typecheck_top scp env t expty =
  let inf = (0, Lib.empty_env()) in
  let (_, env1) = typecheck_aux scp inf env expty t
  in
  env1

let typecheck scp t expty =
  ignore(typecheck_top scp (Gtype.Subst.empty()) t expty)

(*** Settype based type checking ****)

type debugType = TermType of (string * Term.term * (Gtype.t list))
let debug_flag = ref false
let debug_list = ref []

let init_list list = list := []
let add_to_list t list =
  if (!debug_flag)
  then list := (t:: (!list))
  else ()

let rec test_type scp env trm ty expty =
  check_type_valid scp ty;
  try Ltype.unify_env scp ty expty env
  with err ->
    raise (add_typing_error "Typechecking: " trm
                            (Gtype.mgu expty env) (Gtype.mgu ty env)
                            err)
and settype_atom scp (inf, appfn) expty atm tyenv =
  let trm = (Atom atm) in
  let (ctr, env) = tyenv in
  match atm with
  | Id(n, ty) ->
     begin
       check_type_valid scp ty;
       let env1 =
         let nty = Lib.try_app (Scope.type_of scp) n in
         if nty = None
         then appfn env expty trm
         else test_type scp env trm (Lib.from_some nty) ty
       in
       let env2 = Ltype.unify_env scp ty expty env1 in
       add_to_list (TermType("Id", trm, [Gtype.mgu expty env2])) debug_list;
       (ctr, env2)
     end
  | Free(n, ty) ->
     check_type_valid scp ty;
     let env1 = test_type scp env trm ty expty
     in
     add_to_list (TermType("Free", trm, [Gtype.mgu expty env1])) debug_list;
     (ctr, env1)
  | Meta(q) ->
     let ty = Binder.type_of q in
     let env1 = test_type scp env trm ty expty
     in
     add_to_list (TermType("Meta", trm, [Gtype.mgu expty env1])) debug_list;
     (ctr, env1)
  | Bound(q) ->
     let ty = Binder.type_of q in
     let env1 = test_type scp env trm ty expty
     in
     add_to_list (TermType("Bound", trm, [Gtype.mgu expty env1])) debug_list;
     (ctr, env1)
  | Const(c) ->
     let ty = Lterm.typeof_cnst c in
     let env1 = test_type scp env trm ty expty
     in
     add_to_list (TermType("Const", trm, [Gtype.mgu expty env1])) debug_list;
     (ctr, env1)
and settype_aux scp (inf, appfn) expty t tyenv =
  let (ctr, env) = tyenv in
  match t with
  | Atom(a) -> settype_atom scp (inf, appfn) expty a tyenv
  | App(f, arg) ->
     (* Make an argument type. *)
     let (ctr1, arg_ty) = Gtype.mk_plain_typevar ctr in
     (* Check argument type *)
     let (ctr2, env2) =
       try settype_aux scp (inf, appfn) arg_ty arg (ctr1, env)
       with err ->
         raise (Term.add_term_error "Typechecking: " [t] err)
     in
     (* Check function type *)
     let fty = Lterm.mk_fun_ty arg_ty expty in
     let (ctr3, env3) =
       try settype_aux scp (inf, appfn) fty f (ctr2, env2)
       with err ->
         raise (Term.add_term_error "Typechecking failure " [t] err)
     in
     add_to_list (TermType("App", t, [Gtype.mgu expty env3])) debug_list;
     (ctr3, env3)
  | Qnt(q, b) ->
     begin
       match Term.Binder.kind_of q with
       | Term.Lambda ->
          let (ctr1, rty) = Gtype.mk_plain_typevar ctr
          and fty = Term.get_binder_type t in
          let bty = Lterm.mk_fun_ty fty rty
          in
          check_type_valid scp fty;
          let env1= test_type scp env t bty expty
          in
          let (ctr2, env2) =
            try settype_aux scp (inf, appfn) rty b (ctr1, env1)
            with err ->
              raise (Term.add_term_error "Typechecking: " [t] err)
          in
          add_to_list
            (TermType("Lambda", t, [Gtype.mgu expty env2])) debug_list;
          (ctr2, env2)
       | _ ->
          let (ctr1, env1) =
            try settype_aux scp (inf, appfn)
                            (Lterm.mk_bool_ty()) b (ctr, env)
            with err ->
              raise (Term.add_term_error "Typechecking: " [t] err)
          in
          let env2 = test_type scp env t (Lterm.mk_bool_ty()) expty
          in
          add_to_list
            (TermType("Qnt", t, [Gtype.mgu expty env2])) debug_list;
          (ctr1, env2)
     end
and settype_top scp inf errfn typenv exty et =
  init_list debug_list;
  settype_aux scp (inf, errfn) exty et (inf, typenv)

let settype scp ?env t =
  let inf = 0
  and f env expty trm =
    match trm with
      | Atom(Id(n, ty)) -> Ltype.unify_env scp ty expty env
      | _ -> env
  in
  let tyenv =
    match env with
      | None -> Gtype.Subst.empty()
      | Some(x) -> x
  in
  let (inf1, tyvar) = Gtype.mk_plain_typevar inf in
  let (_, env1) = settype_top scp inf1 f tyenv tyvar t
  in
  env1

(**
    [typecheck_env tyenv scp t ty]: Check, w.r.t type context [tyenv],
    that term [t] has type [ty] in scope [scp]. Type variables in [t]
    take their assigned value from [tyenv], if they have one.

    The type of an identifier [Id(n, ty)] is looked for in scope [scp].
    The given type [ty] is discarded.
*)
let typecheck_env scp env t expty =
  let inf = 0
  and f  _ _ t =
    match t with
      | Atom(Id(n, ty)) ->
        raise (Term.term_error ("Typecheck: unknown identifier ") [t])
      | _ ->
        raise (Term.term_error "Typecheck: unknown error" [t] )
  in
  let (_, env1) = settype_top scp inf f env expty t
  in
  env1

(*
 * Well-definedness of types.
 *)

let rec check_types scp t =
  match t with
    | Atom(Id(_, ty)) -> Ltype.well_defined scp [] ty
    | App(f, a) -> check_types scp f; check_types scp a
    | Qnt(q, b) ->
      Ltype.well_defined scp [] (Term.get_binder_type t);
      check_types scp b
    | _ -> ()
