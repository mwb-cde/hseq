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

(** [set_name ?strict ?memo scp typ]: Set names in type [typ] to their
    long form.

    If [strict=true], fail if any type name doesn't occur in scope [scp].
*)
let set_name ?(memo=Lib.empty_env()) scp trm =
  Gtypes.set_name ~memo:memo scp trm

let unfold scp ty = Gtypes.unfold scp ty

let well_formed_full f scp ty =
  Gtypes.well_formed_full f scp ty

let well_formed scp ty =
  Gtypes.well_formed scp ty

let well_defined scp ls ty =
  Gtypes.well_defined scp ls ty

let check_decl_type scp ty =
  Gtypes.check_decl_type scp ty

let unify_env scp a b sb =
  Gtypes.unify_env scp a b sb

let unify scp a b =
  Gtypes.unify scp a b

let matching_env scp sb a b =
  Gtypes.matching_env scp sb a b

let matches_env scp sb a b =
  Gtypes.matches_env scp sb a b

let matches scp a b =
  Gtypes.matches scp a b

