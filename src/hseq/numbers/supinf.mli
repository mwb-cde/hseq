(*----
  Name: supinf.mli
  Copyright M Wahab 2005-2010
  Author: M Wahab  <mwb.cde@googlemail.com>

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

(*
  SupInf decision procedure for Presburger Formulas.
  Based on Shostaks method.
  see R.E.Shostak, Journal of the ACM, vol 24, no 4, Oct. 1977
*)



type compfn = | Equals | Leq | Lt | Gt | Geq

type boolexpr = ((compfn * Exprs.expr * Exprs.expr), int) Prop.boolexpr

exception Has_solution of (Exprs.expr * Exprs.expr) list
exception Possible_solution of (Exprs.expr * Exprs.expr) list
exception Infeasible
exception Unknown
exception No_value of Exprs.expr * Exprs.expr

val upper: (Exprs.expr *Exprs.expr) list -> Exprs.expr -> Exprs.expr
val lower: (Exprs.expr *Exprs.expr) list -> Exprs.expr -> Exprs.expr

val supp: Exprs.expr -> Exprs.expr -> Exprs.expr
val inff: Exprs.expr -> Exprs.expr -> Exprs.expr
val sup_aux:
  (Exprs.expr * Exprs.expr) list ->
  Exprs.expr -> Exprs.set -> Exprs.expr
val inf_aux:
  (Exprs.expr * Exprs.expr) list ->
  Exprs.expr -> Exprs.set -> Exprs.expr
val sup: (Exprs.expr * Exprs.expr) list -> Exprs.expr -> Exprs.expr
val inf: (Exprs.expr * Exprs.expr) list -> Exprs.expr -> Exprs.expr
  
val has_integer_solns: (Exprs.expr * Exprs.expr) list -> bool
val solve_inequalities: 
  (Exprs.expr * Exprs.expr) list list -> (Exprs.expr * Exprs.expr) list

(* decide t: 
   top level function.
   convert negation of t to a set of ilps
   test for validity
   if invalid give true (t is valid).
   if valid raise Has_solutions with solutions.
   if undecidable raise Possible_solutions.
*)
val decide: 
  (compfn * Exprs.expr * Exprs.expr, 'a) Prop.boolexpr -> bool

(* Debugging *)

module Lang:
sig

  val mk_leq: 'a -> 'b -> (compfn * 'a * 'b, 'c) Prop.boolexpr
  val mk_and:
    ('a, 'b) Prop.boolexpr ->
    ('a, 'b) Prop.boolexpr -> ('a, 'b) Prop.boolexpr
  val mk_not: ('a, 'b) Prop.boolexpr -> ('a, 'b) Prop.boolexpr
  val mk_or:
    ('a, 'b) Prop.boolexpr ->
    ('a, 'b) Prop.boolexpr -> ('a, 'b) Prop.boolexpr
  val expand_comp:
    compfn * Exprs.expr * Exprs.expr 
    -> (compfn * Exprs.expr * Exprs.expr, 'a) Prop.boolexpr
  val expand_neg_comp:
    compfn * Exprs.expr * Exprs.expr 
    -> (compfn * Exprs.expr * Exprs.expr, 'a) Prop.boolexpr
  val push_conj:
    ('a, 'b) Prop.boolexpr ->
    ('a, 'b) Prop.boolexpr -> ('a, 'b) Prop.boolexpr
  val mk_dnf_as_bexpr:
    (compfn * Exprs.expr * Exprs.expr, 'a) Prop.boolexpr ->
    (compfn * Exprs.expr * Exprs.expr, 'a) Prop.boolexpr
  val mk_dnf_as_expr:
    (compfn * Exprs.expr * Exprs.expr, 'a) Prop.boolexpr ->
    (compfn * Exprs.expr * Exprs.expr, 'a) Prop.boolexpr
  val mk_ilp: (compfn * 'a * 'b, 'c) Prop.boolexpr -> ('a * 'b) list list
end


module Bledsoe:
sig
  val member: Exprs.expr -> Exprs.set -> bool
  val insert: Exprs.expr -> Exprs.set -> Exprs.set
  val remove_trivial_ineqs:
    (Exprs.expr * Exprs.expr) list 
    -> Exprs.set * (Exprs.expr * Exprs.expr) list

  val vars_of_env: Exprs.expr -> Exprs.set -> Exprs.set
  val vars_of: Exprs.expr -> Exprs.set
  val get_vars: (Exprs.expr * Exprs.expr) list -> Exprs.set
  val combine: Exprs.set -> Exprs.set -> Exprs.set
  val solve_for_var: 
    int -> (Exprs.expr * Exprs.expr) list 
    -> ((Exprs.expr * Exprs.expr) list 
	* (Exprs.expr * Exprs.expr) list 
	* (Exprs.expr * Exprs.expr) list )
  val upper: (Exprs.expr * Exprs.expr) list -> Exprs.expr -> Exprs.expr
  val lower: (Exprs.expr * Exprs.expr) list -> Exprs.expr -> Exprs.expr
  exception Invalid of string * Exprs.expr
  val get_mult_var: Exprs.expr list -> Exprs.expr * Exprs.expr list
  val get_mult_of: 
    Exprs.expr -> Exprs.expr list -> Exprs.expr * Exprs.expr list
  val dest_multexpr: Exprs.expr * 'a -> Num.num * Exprs.expr * 'a
  val supp: Exprs.expr -> Exprs.expr -> Exprs.expr
  val inff: Exprs.expr -> Exprs.expr -> Exprs.expr
  val sup_aux: 
    (Exprs.expr * Exprs.expr) list 
    -> Exprs.expr -> Exprs.set -> Exprs.expr
  val inf_aux: 
    (Exprs.expr * Exprs.expr) list 
    -> Exprs.expr -> Exprs.set -> Exprs.expr
  val sup: (Exprs.expr * Exprs.expr) list -> Exprs.expr -> Exprs.expr
  val inf: (Exprs.expr * Exprs.expr) list -> Exprs.expr -> Exprs.expr
end

module Shostak: 
sig

  val chose_int: Num.num * Num.num -> Num.num
  val chose_val: Exprs.expr * Exprs.expr -> Exprs.expr
  val elim_var: 
    (Exprs.expr * Exprs.expr) list 
    -> int -> Exprs.expr * (Exprs.expr * Exprs.expr) list
  val apply_elim: 
    (Exprs.expr * Exprs.expr) list 
    -> int list -> (Exprs.expr * Exprs.expr) list
  val is_integer: Exprs.expr -> bool
  val has_integer_solns: (Exprs.expr * Exprs.expr) list -> bool
  val solve_ineq_conj: 
    (Exprs.expr * Exprs.expr) list -> (Exprs.expr * Exprs.expr) list
  val solve_inequalities: (Exprs.expr * Exprs.expr) list list -> 
    (Exprs.expr * Exprs.expr) list
  val decide: (compfn * Exprs.expr * Exprs.expr, 'a) Prop.boolexpr -> bool
  val soln: 
    ('a -> (Exprs.expr * Exprs.expr) list) 
    -> 'a -> (Exprs.expr * Exprs.expr) list

end


