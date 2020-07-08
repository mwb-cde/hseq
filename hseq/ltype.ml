(*----
  Name: ltype.ml
  Copyright Matthew Wahab 2018-2020
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

(** Manipulating types of the logic. *)

open Gtype

(**
   [type_in_scope memo scp ty]: Check that [ty] is in scope by checking
   that every type constructor is decared or defined in scope [scp].

   The function is memoised: if a constructor name is found to be
   in scope, it is added to [memo].
*)
let in_scope_memoized memo scp ty =
  let lookup_id n =
    try Some(Lib.find n memo)
    with Not_found -> None
  in
  let rec in_scp_aux t tbl =
    match t with
    | Gtype.Atom(Gtype.Var(_)) -> (true, tbl)
    | Gtype.Atom(Gtype.Weak(_)) -> (true, tbl)
    | Gtype.Atom(Gtype.Ident(f)) ->
       let thy_id = Ident.thy_of f in
       let rslt_opt = lookup_id thy_id in
       if rslt_opt <> None
       then (true, tbl)
       else
         begin
           if Scope.in_scope scp thy_id
           then (true, Lib.add thy_id true tbl)
           else (false, tbl)
         end
    | Gtype.App(l, r) ->
       let (lrslt, ltbl) = in_scp_aux l tbl in
       if lrslt
       then in_scp_aux r ltbl
       else (lrslt, ltbl)
  in
  in_scp_aux ty memo

let in_scope scp t =
  let (ret, _) = in_scope_memoized (Lib.empty_env()) scp t in
  ret


(** [set_name ?strict ?memo scp typ]: Set names in type [typ] to their
    long form.

    If [strict=true], fail if any type name doesn't occur in scope [scp].
*)
let set_name scp trm =
  Gtype.set_name (Scope.types_scope scp) trm

let set_name_memoized memo scp trm =
  Gtype.set_name_memoized memo (Scope.types_scope scp) trm

let unfold scp ty = Gtype.unfold (Scope.types_scope scp) ty

let well_formed_full f scp ty =
  Gtype.well_formed_full f (Scope.types_scope scp) ty

let well_formed scp ty =
  Gtype.well_formed (Scope.types_scope scp) ty

let well_defined scp ls ty =
  Gtype.well_defined (Scope.types_scope scp) ls ty

let check_decl_type scp ty =
  Gtype.check_decl_type (Scope.types_scope scp) ty

let unify_env scp a b sb =
  Gtype.unify_env (Scope.types_scope scp) a b sb

let unify scp a b =
  Gtype.unify (Scope.types_scope scp) a b

let matching_env scp sb a b =
  Gtype.matching_env (Scope.types_scope scp) sb a b

let matches_env scp sb a b =
  Gtype.matches_env (Scope.types_scope scp) sb a b

let matches scp a b =
  Gtype.matches (Scope.types_scope scp) a b
