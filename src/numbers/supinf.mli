(*-----
 Name: supinf.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(*
   SupInf decision procedure for Presburger Formulas.
   Based on Shostaks method.
   see R.E.Shostak, Journal of the ACM, vol 24, no 4, Oct. 1977
*)

(* [expr]: interface to decision procedure
   integer expressions 
   made up of variables, constants, addition, multiplication, max and min

   Variables are represented by numbers.
   A variable is therefore an index into a table of terms in the 
   calling program. The calling program determines what is to be 
   treated as a variable.

   The index identified by [const_key] is reserveed for internal use.
*)
   
type expr =              
  | PosInf
  | NegInf
  | Val of Num.num
  | Var of int            
  | Plus of expr list
  | Mult of Num.num * expr
  | Max of expr list
  | Min of expr list
and compfn = | Equals | Leq | Lt | Gt | Geq

type boolexpr = ((compfn * expr * expr), int) Prop.boolexpr

exception Has_solution of (expr * expr) list
exception Possible_solution of (expr * expr) list

val zero_num: Num.num
val one_num: Num.num

(*
val expr_string : expr -> string
val comp_string: compfn -> string
val boolexpr_string : boolexpr -> string
*)

val poly_of_expr : expr -> expr
val expr_of_poly : expr -> expr

val evalexpr : expr -> Num.num
val const_key : int
val reduce : expr -> expr
val distrib : expr -> expr
val simp : expr -> expr

val upper : (expr *expr) list -> expr -> expr
val lower : (expr *expr) list -> expr -> expr

val supp : expr -> expr -> expr
val inff : expr -> expr -> expr
val sup_aux :
    (expr * expr) list ->
      expr -> expr list -> expr
val inf_aux :
    (expr * expr) list ->
      expr -> expr list -> expr
val sup : (expr * expr) list -> expr -> expr
val inf : (expr * expr) list -> expr -> expr
    
val has_integer_solns : (expr * expr) list -> bool
val solve_inequalities : (expr * expr) list list -> 'a list

(* decide t: 
   top level function.
   convert negation of t to a set of ilps
   test for validity
   if invalid give true (t is valid).
   if valid raise Has_solutions with solutions.
   if undecidable raise Possible_solutions.
*)

val decide : boolexpr -> bool
