(*----
  Name: exprs.mli
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

(**
   [expr]: interface to decision procedure
   integer expressions 
   made up of variables, constants, addition, multiplication, max and min

   Variables are represented by numbers.
   A variable is therefore an index into a table of terms in the 
   calling program. The calling program determines what is to be 
   treated as a variable.

   The index identified by [const_key] is reserved for internal use.
*)

val zero_num: Num.num
val one_num: Num.num
type expr =
    PosInf
  | NegInf
  | Val of Num.num
  | Var of int
  | Plus of expr list
  | Mult of Num.num * expr
  | Max of expr list
  | Min of expr list

module ExprSet: Set.S with type elt = expr
type set = ExprSet.t

val set_to_list: set -> expr list

exception Unknown
exception Occurs
val const_key: int
val dest_val: expr -> Num.num
val dest_var: expr -> int
val dest_cnstr: expr -> expr list
val strip_cnstr: expr -> expr list
val dest_plus: expr -> expr list
val dest_max: expr -> expr list
val dest_min: expr -> expr list
val is_val: expr -> bool
val is_any_val: expr -> bool
val is_const_key: int -> bool
val is_const: expr -> bool
val is_var: expr -> bool
val is_cnstr: expr -> bool
val is_plus: expr -> bool
val is_max: expr -> bool
val is_min: expr -> bool
val mk_mult: Num.num -> expr -> expr
val mk_add: expr list -> expr -> expr
module PP:
sig
  val list_string: ('a -> string) -> string -> 'a list -> string
  val expr_string: expr -> string
  val print: expr -> unit
end
type substitution = (expr * expr) list
val find: 'a -> ('a * 'b) list -> 'b
val member: 'a -> ('a * 'b) list -> bool
val bind: 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
val set: 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
val subst: expr -> (expr * expr) list -> expr
val occurs: expr -> expr -> bool
val inc: expr -> expr
val add: expr -> expr -> expr
val add_const: Num.num -> expr -> expr
val mult: Num.num -> expr -> expr
val is_neg: expr -> bool
val leq: expr -> expr -> bool
val lt: expr -> expr -> bool
val eval: expr -> Num.num
val seper: expr list -> expr list * expr list * expr list * expr list
val delete_trivial: expr -> expr
module Poly:
sig
  val add_expr: expr -> expr -> expr
  val mult_expr: Num.num -> expr -> expr
  val make_term: int -> expr -> expr
  val dest_term: expr -> int * Num.num
  val set_term: int -> expr -> expr -> expr
  val value_of: int -> expr -> expr
  val is_var_of: expr -> int -> bool
  val reform_empty: expr -> expr
  val remove_empty: expr list -> expr list
  val add: expr -> int -> expr -> expr
  val mult: Num.num -> expr -> expr
  val poly_of_expr: expr -> expr
  val expr_of_poly: expr -> expr
  val reduce: expr -> expr
  val unbundle: ('a -> bool) -> ('a -> 'a list) -> 'a list -> 'a list
  val unbundle_max: expr list -> expr list
  val unbundle_min: expr list -> expr list
  val unbundle_plus: expr list -> expr list
  val distrib: expr -> expr
  val simp: expr -> expr
end
module IneqSolver:
sig
  val remove_var_rhs: int -> expr * expr -> expr * expr
  val do_subs: int -> expr * expr -> expr * expr
  val do_div: int -> expr * expr -> expr * expr
  val find_soln: int -> expr * expr -> (expr * expr) option
  val solve:
    (expr * expr) list ->
    int -> (expr * expr) list * (expr * expr) list * (expr * expr) list
  val solve_for:
    int 
    -> (expr * expr) list 
    -> ((expr * expr) list  * (expr * expr) list * (expr * expr) list)
end


