(*----
 Name: supinf.ml
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

open Num
open Exprs

type compfn = Equals | Leq | Lt | Gt | Geq

type boolexpr = ((compfn * Exprs.expr * Exprs.expr), int) Prop.boolexpr

exception Has_solution of (Exprs.expr* Exprs.expr) list
exception Possible_solution of (Exprs.expr*Exprs.expr) list
exception Infeasible
exception Unknown
exception No_value of expr*expr


module Lang=      (* Basic manipulation of expressions *)
  struct

    let mk_leq a b = Prop.mk_bexpr(Leq, a, b)
    let mk_and a b = Prop.mk_and a b
    let mk_not a = Prop.mk_not a
    let mk_or a b = Prop.mk_or a b

(* put comparisons in form using only less-than-or-equals *)

    let expand_comp t = 
      match t with
	(Leq, a, b) -> mk_leq a b
      | (Lt, a, b) -> mk_leq (inc a) b
      | (Equals, a, b) -> mk_and (mk_leq a b) (mk_leq b a)
      | (Geq, a, b) -> mk_leq b a
      | (Gt, a, b) -> mk_leq (inc b) a

    let expand_neg_comp t =   (* not (a comp b) *)
      match t with
	(Leq, a, b) -> expand_comp (Gt, a, b)         (* not (a<=b) -> a>b *)
      | (Lt, a, b) -> mk_leq b a                  (* not (a<b) -> b<=a *)
      | (Equals, a, b) ->                         (* not (a=b) 
						     -> 
						     not (a<=b and b<=a)
					             -> a>b or b>a
						   *)
	  (mk_or 
	     (expand_comp (Gt, a, b)) 
	     (expand_comp (Gt, b, a)))
      | (Geq, a, b) -> expand_comp (Lt, a, b)         (* not (a>=b) -> a<b *)
      | (Gt, a, b) -> mk_leq a b                  (* not (a>b) -> a<=b *)


(* 
   Put boolexpr into DNF form (expanding comparisons on the fly) 
   DNF is list of list of exprs:
   (a&b&c) | (d&e&f) is [[a;b;c];[d;e;f]]
   (DNF is a boolexpr for now)
 *)

(* [push_conj x y]: [x or y] with conjunctions pushed into [x] and [y] *)

    let rec push_conj x y =          
      match (x, y) with
	Prop.Or(a, b), _ -> Prop.Or(push_conj a y, push_conj b y)
      | _, Prop.Or(a, b) -> Prop.Or(push_conj x a, push_conj x b)
      | Prop.Bool (true), _ -> y
      | _, Prop.Bool(true) -> x
      | Prop.Bool(false), _ -> Prop.Bool(false)
      | _, Prop.Bool(false) -> Prop.Bool(false)
      | _ -> Prop.And(x, y)

(* make a dnf as a bool expr rather than a list of lists *)
    let mk_dnf_as_bexpr x =
      let rec dnf_top n t =
	match t with
	  Prop.Bool b -> Prop.Bool(if n then (not b) else b)
	| Prop.Bexpr(f) -> 
      	    if n 
	    then (expand_neg_comp f)
      	    else (expand_comp f)
	| Prop.Var a -> t
	| Prop.Equals(a, b) ->
	    dnf_top n (Prop.Iff(a, b))
	| Prop.Not a -> dnf_top (not n) a
	| Prop.And(a, b) -> 
      	    if n 
      	    then dnf_top (not n) (mk_or (mk_not a) (mk_not b))
      	    else 
	      let na = (dnf_top n a)
	      in push_conj na (dnf_top n b)
	| Prop.Or(a, b) -> 
      	    if n
      	    then 
	      dnf_top (not n) (mk_and (mk_not a) (mk_not b))
      	    else
	      (mk_or(dnf_top n a) (dnf_top n b))
	| Prop.Implies(a, b) -> dnf_top n (mk_or (mk_not a) b)
	| Prop.Iff(a, b) -> 
	    dnf_top n 
	      (Prop.mk_and (Prop.mk_implies a b) (Prop.mk_implies b a))
      in dnf_top false x 

    let mk_dnf_as_expr x =  mk_dnf_as_bexpr x

    let mk_ilp x =
      let rec ilp_conj x rs =
	match x with
	  Prop.Bool(b) -> if b then rs else []
	| Prop.Bexpr(Leq, a, b) -> (a, b)::rs
	| Prop.And(a, b) -> ilp_conj b (ilp_conj a rs)
	| _ -> raise (Invalid_argument "mk_ilp")
      and 
	  ilp_top x rs =
	match x with
	  Prop.Or(a, b) -> ilp_top b (ilp_top a rs)
	| _ -> 
	    (match ilp_conj x [] with 
	      [] -> rs 
	    | a -> a::rs)
      in 
      ilp_top x []

  end


(* Bledsoe's version of SupInf *)

module Bledsoe=
  struct

    open Lang

(*type 'a set = 'a list*)
(*
    let rec member a b = 
      match b with
	[] -> false
      | (x::xs) -> 
	  if a = x 
	  then true 
	  else member a xs
    let add_set a b = 
      if member a b then b else insert a b
*)

    let insert a b = ExprSet.add a b
    let member a b = ExprSet.mem a b

(**
   [seperate x g]:
   given a list [g] of conjuctions and variable [x]
   return the lists [Ga], [Gb], [Gc] such that [x] occurs on 
   the left in the conjunctions of [Ga], on the right in those of [Gb]
   and doesn't occur in those of [Gc].
 *)
(*
    let seperate x g =
      let rec sep_aux xs ga gb gc=
	match xs with
	  [] -> (ga, gb, gc)
	| ((a, b)::ys) ->
	    let oca = occurs x a
	    and ocb = occurs x b
	    in 
	    if oca && ocb then raise (Invalid_argument "seperate")
	    else
	      let nga=
		if oca then ((a, b)::ga) else ga
	      and ngb=
		if ocb then ((a, b)::gb) else gb
	      and ngc=
		if not (oca or ocb) then ((a, b):: gc) else gc
	      in sep_aux ys nga ngb ngc
      in 
      sep_aux g [] [] [] 
*)


(* get list of variables in expression e *)

    let vars_of_env e env= 
      let rec list_aux xs rs = 
	match xs with 
	  [] -> rs
	| (y::ys) -> list_aux ys (term_aux y rs)
      and term_aux e rs =
	match e with
	  Var x -> (ExprSet.add e rs)
	| Mult(_, t) -> term_aux t rs
	| Plus(xs) -> list_aux xs rs
	| Max(xs) -> list_aux xs rs
	| Min(xs) -> list_aux xs rs
	| _ -> rs
      in term_aux e env

    let vars_of e = vars_of_env e ExprSet.empty

(* get list of variables in conjunction of inequalities *)

    let get_vars s = 
      let vs = ref ExprSet.empty
      in 
      let get_aux (x, y) =
	vs:=vars_of_env x (!vs);
	vs:=vars_of_env y (!vs)
      in 
      List.iter get_aux s;
      !vs

    let rec combine xs ys =  ExprSet.union xs ys

(*
   [remove_trivial_ineqs s]:
   for inequalities [s]:
   simp inequalities,
   solve and remove those which do not variables,
   get variables
   return remaining inequalities and the list of variables
   raise Infeasible if any inequality cannot be solved 
 *)
    let remove_trivial_ineqs s =
      let rec remove_aux xs vs rs=
	match xs with
	  [] -> (vs, rs)
	| ((a, b)::ys) ->
	    let (na, nb) = (Poly.simp a, Poly.simp b)
	    in 
	    let is_valid = 
	      (try leq na nb 
	      with Unknown -> true)
	    and nvs = (vars_of_env nb (vars_of_env  na ExprSet.empty))
	    in 
	    if not(is_valid)
	    then raise Infeasible
	    else 
	      if ExprSet.is_empty nvs then
		remove_aux ys vs rs
	      else remove_aux ys (combine nvs vs) ((a, b)::rs)
      in remove_aux s ExprSet.empty []


(* UPPER and LOWER functions *)

(**
   [all_feasible s]: test that all inequalities in [s] are feasible
   (there is no [(a, b)] in [s] such that [(a>b)].
*)
let all_feasible s = 
  let rec test_aux ls =
    match ls with
      [] -> true
    | (a, b)::xs -> (leq a b) && (test_aux xs)
  in 
  test_aux s


(**
   [solve_for_var x s]: 
   solve inequalities [s] for variable [x]. 
   Return
   [gL]: list of inequalities  [L <= x]
   [gU]: list of inequalities  [x <= U]
   [gO]: list of inequalities  [A <= B]
   where the [x] doesn't occur in [L], [U], [A] or [B].
   
   remove trivial inequalities in [gO].
   raise [Infeasible] if any of the [gO] are trivially infeasible.
*)
    let solve_for_var x s=
      let gL, gU, gO= IneqSolver.solve_for x s 
      in 
      let _ = remove_trivial_ineqs gO
      in 
      (gL, gU, gO)
      

(**
   UPPER(S, x), LOWER(S, x): 
   S is the set of inequalities/conjunctions to be solved for x

 *)
    let upper s x =
      let _, gU, gO = solve_for_var (dest_var x) s (* seperate s x *)
      in 
      let us=List.map (fun (_, b) -> b) gU
      in 
      match us with
	[] -> 
	  (match gO with
	    [] -> PosInf  (* No upper bound, no alternative solution *)
	  | _ -> 
	      if(all_feasible gO) 
	      then PosInf    (* No upper bound *)
	      else NegInf)    (* No feasible solution *)
      | (u::[]) -> u
      | _ -> Min us
	    
    let lower s x =
      let gL, _, gO = solve_for_var (dest_var x) s (* seperate s x *)
      in 
      let ls=List.map (fun (a, _) -> a) gL
      in 
      match ls with
	[] -> 
	  (match gO with
	    [] -> NegInf  (* No Lower bound, no alternative solution *)
	  | _ -> 
	      if(all_feasible gO) 
	      then NegInf    (* No Lower bound *)
	      else PosInf)    (* No feasible solution *)
      | (l::[]) -> l
      | _ -> (Max ls)

(* Supp and Inff functions *)

(*
   supp(x, y) 
   inff(x, y)
   where x and y are exprs
 *)

    exception Invalid of string * expr

    let get_mult_var xs = 
      match 
	(try
	  (Lib.extract
	     (fun x->
	       match x with 
		 Mult(_, Var(_)) -> true
	       | Var(_) -> true
	       | _ -> false) xs) 
	with Not_found -> (Mult(zero_num, Var const_key), xs))
      with
	(Mult(m, v), l) -> Mult(m, v), l
      | (Var(v)), l -> Mult(one_num, Var(v)), l
      | _ -> raise (Invalid_argument "get_mult_var")


    let get_mult_of n xs = 
      match 
	(try
	  (Lib.extract
	     (fun x->
	       match x with 
		 Mult(_, i) -> i=n
	       |	i -> i=n) xs) 
	with Not_found -> ((Mult(zero_num, n)), xs))
      with
	(Mult(m, v), l) -> Mult(m, v), l
      | (Var(v)), l -> Mult(one_num, Var(v)), l
      | _ -> raise (Invalid_argument "get_mult_var")

    let dest_multexpr (t, a)=
      match t with
	Mult(m, b) -> (m, b, a)
      | _ -> raise (Invalid ("dest_multexpr", t))

    let rec supp x y =
      if is_any_val y then y
      else if x=y then PosInf
      else 
	match y with
	  Min(bs) -> Min(List.map (supp x) bs)
	| Plus(bs) ->
	    (try
	      let (b, _, c0) = dest_multexpr (get_mult_of x bs)
	      in let c = delete_trivial (Plus(Poly.unbundle_plus c0))
	      in 
	      List.iter 
		(fun a -> if occurs x a 
		then raise (Invalid ("supp: plus", a)) else ()) c0;
	      if b>one_num then PosInf
	      else if b<one_num then Mult(one_num//(one_num-/b), c)
	      else 
		if not (is_any_val c) then PosInf
		else 
		  if (is_neg c) 
		  then NegInf 
		  else PosInf
	    with Not_found -> raise (Invalid ("supp: plus", y)))
	| Var(_) -> Val(zero_num)                              (* x=1x+0 *)
	| _ -> raise (Invalid ("supp", y))

    let rec inff x y =
      if is_any_val y then y
      else if x=y then NegInf
      else 
	match y with
	  Max(bs) -> Min(List.map (inff x) bs)
	| Plus(bs) ->
	    (try
	      let (b, _, c0) = dest_multexpr (get_mult_of x bs)
	      in let c=delete_trivial (Plus(Poly.unbundle_plus c0))
	      in 
	      List.iter 
		(fun a -> 
		  if occurs x a 
		  then raise (Invalid ("inff: plus", a)) 
		  else ()) c0;
	      if b>one_num then NegInf
	      else if b<one_num then Mult(one_num//(one_num-/b), c)
	      else 
		if not (is_any_val c) 
		then NegInf
		else 
		  if (leq c (Val(zero_num)))
		  then NegInf 
		  else PosInf
	    with Not_found -> raise (Invalid ("inff: plus", y)))
	| Var(_) -> Val(zero_num)                              (* x = 1x+0 *)
	| _ -> raise (Invalid ("inff", y))


(* Sup and Inf functions *)  
(*
   sup_aux s j h 
   inf_aux s j h
   where s is  a set of inequalities
   j is an expr
   and h is a set of exprs 

   sup s j = sup_aux s j []
   inf s j = inf_aux s j []
 *)
    let rec sup_aux s j h=
      match j with 
	Val(_) -> j
      | PosInf -> j
      | NegInf -> j
      | Var(_) -> 
	  if member j h then j
	  else 
	    (let q = upper s j
	    in let z = sup_aux s q (insert j h)
	    in supp j (Poly.simp z))
      | Mult(m, a) ->
	  if m<zero_num 
	  then Mult(m, inf_aux s a h)
	  else Mult(m, sup_aux s a h)
      | Plus(xs)  ->
	  (let (r, v, b0)= 
	    (try dest_multexpr (get_mult_var xs)
      	    with Not_found -> raise (Invalid ("sup_aux: plus", j)))
	  in 
	  let b = delete_trivial (Plus(b0))
	  in 
	  let b1=sup_aux s b (insert v h)
	  in 
	  if occurs v b1 
	  then sup_aux s (Poly.simp(Plus([Mult(r, v); b1]))) h
	  else (Plus[(sup_aux s (Mult(r, v)) h); b1]))
      | Min(xs) ->
	  Min(List.map(fun x-> sup_aux s x h) xs)
      | _ -> raise (Invalid_argument "sup_aux")
    and 
	inf_aux s j h =
      match j with 
	Val(_) -> j
      | PosInf -> j
      | NegInf -> j
      | Var(_) -> 
	  if member j h then j
	  else 
	    (let q = lower s j
	    in let z = inf_aux s q (insert j h)
	    in inff j (Poly.simp z))
      | Mult(m, a) ->
	  if m<zero_num then Mult(m, sup_aux s a h)
	  else Mult(m, inf_aux s a h)
      | Plus(xs)  ->
      	  (let (r, v, b0)=
	    (try dest_multexpr(get_mult_var xs)
	    with Not_found -> raise (Invalid ("inf_aux: plus", j)))
	  in 
	  let b = delete_trivial (Plus(b0))
	  in 
	  let b1=inf_aux s b (insert v h)
	  in 
	  if occurs v b1 
	  then inf_aux s (Poly.simp(Plus([Mult(r, v); b1]))) h
	  else (Plus[inf_aux s (Mult(r, v)) h; b1]))
      | Max(xs) ->
	  Max(List.map(fun x-> inf_aux s x h) xs)
      | _ -> raise (Invalid_argument "inf_aux")

    let sup s j = Poly.reduce (sup_aux s j ExprSet.empty)
    let inf s j = Poly.reduce (inf_aux s j ExprSet.empty)
	
  end

module Shostak = 
  struct

    open Lang
    open Bledsoe


(*
   [chose_int (x, y)]:
   choose an integer in the interval [[x, y]].
   If no integer, choose a real
   [x] must be less than or equal to [y] (not [x>y])
 *)
    let chose_int (x, y)=
      let i = ceiling_num x
      and j = floor_num y
      in 
      if i <=/ y then i         (* find integer between x and y *)
      else 
	if x<=/j then j         (* find integer between y and x *)
	else y                 (* no integer: use a float *)

(* 
   given expressions x and y, try to find an integer between
   the two 
 *)


    let chose_val (x, y)=
      if x=y then x
      else
	match (x, y) with
	  (NegInf, PosInf) -> Val(zero_num)
	| (NegInf, Val(b)) -> Val(floor_num b)
	| (Val(a), PosInf) -> Val(ceiling_num a)
	| (Val(a), Val(b)) -> Val(chose_int (a, b))
	| _ -> raise (No_value (x, y))

(**
   [elim_var s x]:
   eliminate variable [x] from set of inequalities [s] 
   raise Infeasible if no solutions can be found 
   otherwise return list of inequalities with x eliminated 
   and new value for [x]
 *)

    let elim_var s x =
      let inf_x = inf s (Var x)
      and sup_x = sup s (Var x)
      in 
      if lt sup_x inf_x then raise Infeasible
      else
	let nv = chose_val (inf_x, sup_x)
	in 
	let binding = bind (Var x) nv []
	in 
	(nv, 
	 List.map 
	   (fun (a, b) -> 
	     (Poly.reduce (subst a binding), 
	      Poly.reduce (subst b binding))) s)

(* for inequalities s and variables vs: 
   repeatedly eliminate vars in vs from s
   if procedure completes then return 
   solution as variable/value list.
   raise Infeasible if no solution
 *)
    let apply_elim s vs =
      let rec apply_aux sa vsa bindings =
	match vsa with
	  [] -> bindings
	| (y::ys) -> 
	    let (nv, ns) = elim_var s y 
	    in 
	    apply_aux ns ys (bind (Var y) nv bindings)
      in 
      apply_aux s vs []

(*
   let bindings = ref []
   in 
   let apply_aux y =
   let (nv, ns) = elim_var s y 
   in 
   bindings:=bind (Var y) nv (!bindings)
   in
   ExprSet.iter apply_aux vs; !bindings

 *)


(* 
   for inequalities s:
   try to find and return integer solution
   raise Infeasible if not found
   raise Unknown if real solution but not integer solutions
 *)

    let is_integer x = 
      if not (is_any_val x) then false
      else 
	match x with
	  Val(y) -> (y -/ (floor_num y))=zero_num
	| _ -> true


    let has_integer_solns xs =
      match xs with
	[] -> false
      | ((_, x)::xs) -> 
	  if not (is_integer x) (* first variable selected is not integer *)
	  then false            (* so no integer solutions (see Shostak) *)
	  else                (* if first is integer then all others must
				 also be integers *)
	    ((List.iter        (* if a real is found then inconclusive *)
		(fun (_, x) -> 
		  if (is_integer x) then () 
		  else raise (Possible_solution xs)) xs);
	     true)
	      
    let solve_ineq_conj s =
      let vs, ns =remove_trivial_ineqs s
      in 
      let nvs = List.map dest_var (set_to_list vs)
      in 
      let solns = apply_elim ns nvs
      in 
      match solns with
	[] -> []
      | _ -> 
	  if (has_integer_solns solns)
	  then solns
	  else raise Infeasible

(*
   For disjunction list of conjunctions of inequalities ineqs:
   try to solve each 
   if any succeeds then list of disjunctions is valid 
   (and therefore original formula is invalid)
   if all fail then list is invalid and original is valid.

   return first set of solutions to make one of the conjunctions valid
   (and original formula invalid).

   raise Infeasible if all disjunctions are invalid (and original 
   formula is valid).

   raise Possible_solution if any conjunction has real but not integer solns.
 *)
    let solve_inequalities ineqs =
      let rec solve_aux eqs =
	match eqs with
	  [] -> raise Infeasible
	| (x::xs) ->
	    try 
	      solve_ineq_conj x
	    with 
	      Infeasible -> solve_aux xs 
	    | Has_solution soln -> soln
	    | err -> raise err
      in 
      solve_aux ineqs 

(**
   [decide t]: 
   top level function.
   convert negation of t to a set of ilps
   test for validity
   if invalid give true (t is valid).
   if valid return false (t is not valid)
   if undecidable, raise unknown
 *)
    let decide t =
      let dnf = Prop.reduce(Lang.mk_dnf_as_expr (Prop.mk_not t))
      in 
      if(Prop.is_true dnf)
      then false
      else 
	if(Prop.is_false dnf)
	then true
	else
	  let ilps = Lang.mk_ilp dnf
	  in 
	  (try 
	    ignore(solve_inequalities ilps); 
	    false (* Has a solution *)
	  with 
	    Infeasible -> true
	  | Has_solution _ -> false
	  | Possible_solution _ -> raise Unknown)

    let soln f a =
      try f a 
      with 
	Has_solution x -> x
      | Possible_solution x -> x

  end


(*
let expr_string =PPexprs.expr_string 
let boolexpr_string = PPexprs.boolexpr_string
let comp_string =PPexprs.comp_string
*)

let poly_of_expr = Poly.poly_of_expr
let expr_of_poly = Poly.expr_of_poly
let reduce = Poly.reduce
let distrib = Poly.distrib
let simp = Poly.simp

let upper = Bledsoe.upper
let lower = Bledsoe.lower
let supp = Bledsoe.supp
let inff = Bledsoe.inff
let sup_aux = Bledsoe.sup_aux
let inf_aux = Bledsoe.inf_aux
let sup= Bledsoe.sup
let inf= Bledsoe.inf


let has_integer_solns = Shostak.has_integer_solns
let solve_inequalities = Shostak.solve_inequalities
let decide = Shostak.decide

