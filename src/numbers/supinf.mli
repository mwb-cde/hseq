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

(**
   [expr]: interface to decision procedure
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
val solve_inequalities : (expr * expr) list list -> (expr * expr) list

(* decide t: 
   top level function.
   convert negation of t to a set of ilps
   test for validity
   if invalid give true (t is valid).
   if valid raise Has_solutions with solutions.
   if undecidable raise Possible_solutions.
*)

val decide : boolexpr -> bool


(* Debugging *)

module Lang :
  sig
    type substitution = (expr * expr) list
    val find : 'a -> ('a * 'b) list -> 'b
    val member : 'a -> ('a * 'b) list -> bool
    val bind : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
    val subst : expr -> (expr * expr) list -> expr
    exception Occurs
    val occurs : expr -> expr -> bool
    val dest_val : expr -> Num.num
    val dest_var : expr -> int
    val is_val : expr -> bool
    val is_any_val : expr -> bool
    val dest_cnstr : expr -> expr list
    val is_plus : expr -> bool
    val is_max : expr -> bool
    val is_min : expr -> bool
    exception Unknown
    val add_expr : expr -> expr -> expr
    val mult_expr : Num.num -> expr -> expr
    val is_neg : expr -> bool
    val leq_expr : expr -> expr -> bool
    val lt_expr : expr -> expr -> bool
    val mk_leq : 'a -> 'b -> (compfn * 'a * 'b, 'c) Prop.boolexpr
    val inc : expr -> expr
    val mk_and :
      ('a, 'b) Prop.boolexpr ->
      ('a, 'b) Prop.boolexpr -> ('a, 'b) Prop.boolexpr
    val mk_not : ('a, 'b) Prop.boolexpr -> ('a, 'b) Prop.boolexpr
    val mk_or :
      ('a, 'b) Prop.boolexpr ->
      ('a, 'b) Prop.boolexpr -> ('a, 'b) Prop.boolexpr
    val expand_comp :
      compfn * expr * expr -> (compfn * expr * expr, 'a) Prop.boolexpr
    val expand_neg_comp :
      compfn * expr * expr -> (compfn * expr * expr, 'a) Prop.boolexpr
    val push_conj :
      ('a, 'b) Prop.boolexpr ->
      ('a, 'b) Prop.boolexpr -> ('a, 'b) Prop.boolexpr
    val mk_dnf_as_bexpr :
      (compfn * expr * expr, 'a) Prop.boolexpr ->
      (compfn * expr * expr, 'a) Prop.boolexpr
    val mk_dnf_as_expr :
      (compfn * expr * expr, 'a) Prop.boolexpr ->
      (compfn * expr * expr, 'a) Prop.boolexpr
    val mk_ilp : (compfn * 'a * 'b, 'c) Prop.boolexpr -> ('a * 'b) list list
  end

module Shostak: 
sig

    module VarSet :
      sig
        type elt = int
        type t 
        val empty : t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val diff : t -> t -> t
        val compare : t -> t -> int
        val equal : t -> t -> bool
        val subset : t -> t -> bool
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val filter : (elt -> bool) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val max_elt : t -> elt
        val choose : t -> elt
        val split : elt -> t -> t * bool * t
      end
    type var_set = VarSet.t
    val set_to_list : VarSet.t -> VarSet.elt list
    val vars_of_env : expr -> VarSet.t -> VarSet.t
    val vars_of : expr -> VarSet.t
    val get_vars : (expr * expr) list -> VarSet.t
    val combine : VarSet.t -> VarSet.t -> VarSet.t
    val chose_int : Num.num * Num.num -> Num.num
    exception No_value of expr * expr
    val chose_val : expr * expr -> expr
    exception Infeasible
    val elim_var : (expr * expr) list -> int -> expr * (expr * expr) list
    val apply_elim : (expr * expr) list -> int list -> (expr * expr) list
    val remove_trivial_ineqs :
      (expr * expr) list -> VarSet.t * (expr * expr) list
    val is_integer : expr -> bool
    val has_integer_solns : (expr * expr) list -> bool
    val solve_ineq_conj : (expr * expr) list -> (expr * expr) list
    val solve_inequalities : (expr * expr) list list -> 
      (expr * expr) list
    val decide : (compfn * expr * expr, 'a) Prop.boolexpr -> bool
    val soln : ('a -> (expr * expr) list) -> 'a -> (expr * expr) list

end

module Simp: 
sig 
    val realmax : float -> float -> float
    val realmin : float -> float -> float
    val evalexpr : expr -> Num.num
    val filter : ('a -> bool) -> 'a list -> 'a list
    val seper : expr list -> expr list * expr list * expr list * expr list
    val unbundle : ('a -> bool) -> ('a -> 'a list) -> 'a list -> 'a list
    val unbundle_max : expr list -> expr list
    val unbundle_min : expr list -> expr list
    val unbundle_plus : expr list -> expr list
    val mk_mult : Num.num -> expr -> expr
    val mk_add : expr list -> expr -> expr
    val delete_trivial : expr -> expr
    val find_env : 'a -> ('a * 'b) list -> 'b
    val member_env : 'a -> ('a * 'b) list -> bool
    val add_to_val : Num.num -> expr -> expr
    val make_poly_term : int -> expr -> expr
    val dest_poly_term : expr -> int * Num.num
    val set : int -> expr -> expr -> expr
    val value_of : int -> expr -> expr
    val delete_empty_poly : expr -> expr
    val poly_add : expr -> int -> expr -> expr
    val poly_mult : Num.num -> expr -> expr
    val poly_of_expr : expr -> expr
    val expr_of_poly : expr -> expr
    val split : ('a -> bool) -> 'a list -> 'a list * 'a list
    val split_pair : ('a -> bool) -> 'a list -> 'a * 'a list
    val order : ('a -> 'a -> bool) -> 'a list -> 'a list
    val delete_empty : expr list -> expr list
    val reduce : expr -> expr
    val distrib : expr -> expr
    val simp : expr -> expr
end

