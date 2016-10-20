(*----
  Name: prop.mli
  Copyright Matthew Wahab 2005-2016
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

type ('a, 'b) boolexpr =
  | Bool of bool
  | Not of ('a, 'b) boolexpr
  | And of ('a, 'b) boolexpr * ('a, 'b) boolexpr
  | Or of ('a, 'b) boolexpr * ('a, 'b) boolexpr
  | Implies of ('a, 'b) boolexpr * ('a, 'b) boolexpr
  | Iff of ('a, 'b) boolexpr * ('a, 'b) boolexpr
  | Equals of ('a, 'b) boolexpr * ('a, 'b) boolexpr
  | Bexpr of 'a
  | Var of 'b

val dest_var: ('a, 'b) boolexpr -> 'b

val mk_true: unit -> ('a, 'b) boolexpr
val mk_false: unit -> ('a, 'b) boolexpr
val mk_and: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_not: ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_or: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_implies: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_iff: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_equals: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_bexpr: 'a -> ('a, 'b) boolexpr

val is_true: ('a, 'b) boolexpr -> bool
val is_false: ('a, 'b) boolexpr -> bool


val conj_to_list: ('a, 'b) boolexpr -> ('a, 'b) boolexpr list
val disj_to_list: ('a, 'b) boolexpr -> ('a, 'b) boolexpr list
val filter_empty: 'a list list -> 'a list list

val conj_to_cnf: ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val disj_to_dnf: ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list

val list_to_conj: ('a, 'b) boolexpr list -> ('a, 'b) boolexpr
val list_to_disj: ('a, 'b) boolexpr list -> ('a, 'b) boolexpr

val mk_dnf: ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val dnf_to_disj: ('a, 'b) boolexpr list list -> ('a, 'b) boolexpr

val mk_cnf: ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val cnf_to_conj: ('a, 'b) boolexpr list list -> ('a, 'b) boolexpr

(* replace boolean variables with (true or false) *)
val strip_vars: ('a, 'b) boolexpr -> ('a, 'b) boolexpr

(* Debugging *)

val dest_var: ('a, 'b) boolexpr -> 'b
val mk_and: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_not: ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_or: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_true: unit -> ('a, 'b) boolexpr
val mk_false: unit -> ('a, 'b) boolexpr
val mk_implies: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_iff: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_equals: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_bexpr: 'a -> ('a, 'b) boolexpr
val conj_to_list: ('a, 'b) boolexpr -> ('a, 'b) boolexpr list
val disj_to_list: ('a, 'b) boolexpr -> ('a, 'b) boolexpr list
val filter_empty: 'a list list -> 'a list list
val conj_to_cnf: ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val disj_to_dnf: ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val push_conj: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val dnf_top:
  bool -> ('c, 'd) boolexpr -> ('c, 'd) boolexpr
val mk_dnf: ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val list_to_conj: ('a, 'b) boolexpr list -> ('a, 'b) boolexpr
val list_to_disj: ('a, 'b) boolexpr list -> ('a, 'b) boolexpr
val dnf_to_disj: ('a, 'b) boolexpr list list -> ('a, 'b) boolexpr
val push_disj: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val cnf_top:
  bool -> ('c, 'd) boolexpr -> ('c, 'd) boolexpr
val mk_cnf: ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val cnf_to_conj: ('a, 'b) boolexpr list list -> ('a, 'b) boolexpr
val strip_vars: ('a, 'b) boolexpr -> ('a, 'b) boolexpr

val reduce: ('a, 'b) boolexpr -> ('a, 'b)boolexpr
