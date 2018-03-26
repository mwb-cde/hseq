(*----
  Name: ltype.ml
  Copyright Matthew Wahab 2018
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

open Gtypes

(**
   [type_in_scope memo scp ty]: Check that [ty] is in scope by checking
   that every type constructor is decared or defined in scope [scp].

   The function is memoised: if a constructor name is found to be
   in scope, it is added to [memo].
*)
let in_scope memo scp ty =
  let lookup_id n =
    try Lib.find n memo
    with Not_found ->
      if Scope.in_scope scp n
      then Lib.add n true memo
      else raise Not_found
  in
  let rec in_scp_aux t =
    match t with
    | Gtypes.Atom(Gtypes.Var(_)) -> ()
    | Gtypes.Atom(Gtypes.Weak(_)) -> ()
    | Gtypes.Atom(Gtypes.Ident(f)) ->
       ignore(lookup_id (Ident.thy_of f))
    | Gtypes.App(l, r) -> (in_scp_aux l; in_scp_aux r)
  in
  try in_scp_aux ty; true
  with Not_found -> false

(** [set_name ?strict ?memo scp typ]: Set names in type [typ] to their
    long form.

    If [strict=true], fail if any type name doesn't occur in scope [scp].
*)
let set_name ?(memo=Lib.empty_env()) scp trm =
  Gtypes.set_name ~memo:memo (Scope.types_scope scp) trm

let unfold scp ty = Gtypes.unfold (Scope.types_scope scp) ty

let well_formed_full f scp ty =
  Gtypes.well_formed_full f (Scope.types_scope scp) ty

let well_formed scp ty =
  Gtypes.well_formed (Scope.types_scope scp) ty

let well_defined scp ls ty =
  Gtypes.well_defined (Scope.types_scope scp) ls ty

let check_decl_type scp ty =
  Gtypes.check_decl_type (Scope.types_scope scp) ty

let unify_env scp a b sb =
  Gtypes.unify_env (Scope.types_scope scp) a b sb

let unify scp a b =
  Gtypes.unify (Scope.types_scope scp) a b

let matching_env scp sb a b =
  Gtypes.matching_env (Scope.types_scope scp) sb a b

let matches_env scp sb a b =
  Gtypes.matches_env (Scope.types_scope scp) sb a b

let matches scp a b =
  Gtypes.matches (Scope.types_scope scp) a b
