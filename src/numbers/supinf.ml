(*-----
 Name: supinf.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Num

let zero_num = num_of_int 0
let one_num = num_of_int 1

type expr = 
    PosInf
  | NegInf
  | Val of Num.num
  | Var of int
  | Plus of expr list
  | Mult of Num.num * expr
  | Max of  expr list
  | Min of  expr list

type compfn = Equals | Leq | Lt | Gt | Geq

type boolexpr = ((compfn * expr * expr), int) Prop.boolexpr

exception Has_solution of (expr* expr) list
exception Possible_solution of (expr*expr) list

(*
 const_key: identifier of constants in a polynomial expressions 
   e.g. in polynomial expressions `1+2*x';
   term 2*x has key x and term 1 has key const_key
*)

let const_key = 0  

module PPexprs = 
struct
let rec list_string f sep x =
  match x with 
    [] -> ""
  | (b::[]) -> (f b)
  | (b::bs) -> (f b)^sep^(list_string f sep bs);;

let rec expr_string t =
  match t with
    (Val x) -> (Num.string_of_num x)
  | (Var x) -> ("var"^(string_of_int x))
  | (PosInf) -> "pinf"
  | (NegInf) -> "ninf"
  | (Plus xs)
    -> ("Plus ("^(list_string expr_string ", " xs)^")")
  | (Mult(a, b)) 
    -> ("Mult ("^(Num.string_of_num a)^"* "^(expr_string b)^")")
  | (Max xs)
    -> ("Max ("^(list_string expr_string ", " xs)^")")
  | (Min xs) 
    -> ("Min ("^(list_string expr_string ", " xs)^")")

let comp_string f =
  match f with 
    Equals -> "="
  | Lt-> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="

let rec boolexpr_string t =
  match t with
    (Prop.Bool b) -> string_of_bool b
  | Prop.Not b -> "-"^(boolexpr_string b)
  | Prop.And(a, b) -> (boolexpr_string a)^"&"^(boolexpr_string b)
  | Prop.Or(a, b) -> (boolexpr_string a)^"|"^(boolexpr_string b)
  | Prop.Implies(a, b) -> (boolexpr_string a)^"->"^(boolexpr_string b)
  | Prop.Iff(a, b) -> (boolexpr_string a)^"<->"^(boolexpr_string b)
  | Prop.Equals(a, b) -> (boolexpr_string a)^"="^(boolexpr_string b)
  | Prop.Bexpr(f, a, b) -> (expr_string a)^(comp_string f)^(expr_string b)
  | Prop.Var x -> ("var("^(string_of_int x)^")")

end

module Lang=      (* Basic manipulation of expressions *)
struct

type substitution = (expr * expr) list

let rec find x env =
  match env with
    [] -> raise Not_found
  | ((a, b)::ys) -> if a = x then b else find x ys

let member x env = 
  try find x env; true 
  with Not_found -> false

let bind x y env = (x, y)::env

let subst e env =
  let rec subst_aux t =
    try 
      find t env
    with Not_found ->
      (match t with
      	(Plus xs) -> Plus(List.map subst_aux xs)
      |	Mult(a, b) -> Mult(a, subst_aux b)
      |	(Max xs) -> Max(List.map subst_aux xs)
      |	(Min xs) -> Min(List.map subst_aux xs)
      |	_ -> t)
in subst_aux e

exception Occurs

let occurs e t = (* e occurs in t*)
  let rec occs_aux x=
    if e=x then raise Occurs
    else 
      (match x with
      	Plus xs -> (List.iter occs_aux xs)
      |	Mult(a, b) -> (occs_aux b)
      |	Max xs -> List.iter occs_aux xs
      |	Min xs -> List.iter occs_aux xs
      |	_ -> ())
  in try occs_aux t; false with Occurs -> true
  
let dest_val t = 
  match t with
    Val(v) -> v
  | _ -> raise (Invalid_argument "dest_val")

let dest_var t = 
  match t with
    Var(v) -> v
  | _ -> raise (Invalid_argument "dest_var")

let is_val t =
  match t with
    Val(_) -> true | _ -> false

let is_any_val x =
  match x with 
    Val(_) -> true
  | PosInf -> true
  | NegInf -> true
  | _ -> false

let dest_cnstr t=
  match t with
    Plus xs -> xs
  | Max xs -> xs
  | Min xs -> xs
  | _ -> raise (Invalid_argument "dest_cnstr")

let is_plus t = 
  match t with 
    Plus _ -> true
  | _ -> false

let is_max t = 
  match t with 
    Max _ -> true
  | _ -> false

let is_min t = 
  match t with 
    Min _ -> true
  | _ -> false

exception Unknown

let add_expr a b =
  match (a, b) with
      (Val(x), Val(y)) -> Val(x+/y)
  | _ -> 
      if (a=PosInf & b=NegInf) or (a=NegInf & b=PosInf) 
      then Val(num_of_int 0)
      else if a=PosInf or b=PosInf then PosInf
      else if a=NegInf or b=NegInf then NegInf
      else Plus[a; b]

let rec mult_expr v b =
  if v=zero_num then  Val(num_of_int 0)
  else if v = (num_of_int 1) then b
  else 
    (match b with
      Val(y) -> Val(v*/y)
    | Mult(n, Val(zero_num)) -> Val(zero_num)
    | Mult(n, b) -> mult_expr (v*/n) b
    | Var(_) -> Mult(v, b)
    | _ -> 
      	if b=PosInf then if v<zero_num then NegInf else b
      	else 
	  if b=NegInf then if v<zero_num then PosInf else b
	  else Mult(v, b))

let is_neg v = 
  match v with
    NegInf -> true
  | PosInf -> false
  | Val(v) -> v<zero_num
  | _ -> raise Unknown

let rec leq_expr a b =
  if a=b then true
  else
  match (a, b) with
    (Val(x), Val(y)) -> x <= y
  | (NegInf, _) -> true
  | (PosInf, _ ) -> false
  | (_, NegInf) -> false
  | (_, PosInf) -> true
  | _ -> raise Unknown

let lt_expr a b = (not (a =b)) & leq_expr a b

let mk_leq a b = Prop.mk_bexpr(Leq, a, b)
let inc a = Plus[a; Val(num_of_int 1)]
let mk_and a b = Prop.mk_and a b
let mk_not a = Prop.mk_not a
let mk_or a b = Prop.mk_or a b

(* put comparisons in form using only less-than-or-equals *)

let expand_comp t a b = 
  match t with
    Leq -> mk_leq a b
  | Lt -> mk_leq (inc a) b
  | Equals -> mk_and(mk_leq a b) (mk_leq b a)
  | Geq -> mk_leq b a
  | Gt -> mk_leq (inc b) a

let expand_neg_comp t a b =   (* not (a comp b) *)
  match t with
    Leq -> expand_comp Gt a b         (* not (a<=b) -> a>b *)
  | Lt -> mk_leq b a                  (* not (a<b) -> b<=a *)
  | Equals ->                         (* not (a=b) -> a<b or b<a *)
	(mk_or (expand_comp Lt a b) (expand_comp Lt b a))
  | Geq -> expand_comp Lt a b         (* not (a>=b) -> a<b *)
  | Gt -> mk_leq a b                  (* not (a>b) -> a<=b *)


(* put boolexpr into DNF form (expanding comparisons on the fly) *)
(*
  DNF is list of list of exprs:
   (a&b&c) | (d&e&f) is [[a;b;c];[d;e;f]]
  (DNF is a boolexpr for now)
*)

(* push_conj x y -> x or y with conjunctions pushed into x and y *)

(*
open Prop

let rec push_conj x y =          
  match (x, y) with
    Or(a, b), _ -> Or(push_conj a y, push_conj b y)
  | _, Or(a, b) -> Or(push_conj x a, push_conj x b)
  | Bool (true), _ -> y
  | _, Bool(true) -> x
  | Bool(false), _ -> Bool(false)
  | _, Bool(false) -> Bool(false)
  | _ -> And(x, y)

(* make a dnf as a bool expr rather than a list of lists *)
let mk_dnf_as_bexpr x =
  let rec dnf_top n p t =
    match t with
      Bool b -> Bool(if n then (not b) else b)
    | Comp(f, a, b) -> 
      	if n then expand_neg_comp f a b
      	else expand_comp f a b
    | Not a -> dnf_top (not n) p a
    | And(a, b) -> 
      	if n 
      	then dnf_top (not n) p (mk_or (mk_not a) (mk_not b))
      	else 
	  let na = (dnf_top n p a)
	  in push_conj na (dnf_top n (Bool true) b)
    | Or(a, b) -> 
      	if n
      	then 
	  dnf_top (not n) p (mk_and (mk_not a) (mk_not b))
      	else
	  (mk_or(dnf_top n p a) (dnf_top n p b))
    | Implies(a, b) -> dnf_top n p (mk_or (mk_not a) b)
    | Iff(a, b) -> 
	dnf_top n p (Prop.mk_and (Prop.mk_implies a b) (Prop.mk_implies b a))
  in dnf_top false (mk_true()) x
*)

let mk_dnf_as_expr x =  Prop.dnf_to_disj (Prop.mk_dnf x)

let mk_ilp x =
let rec ilp_conj x rs =
  match x with
    Prop.Bool(b) -> if b then rs else []
  | Prop.Bexpr(Leq, a, b) -> (a, b)::rs
  | Prop.And(a, b) -> ilp_conj b (ilp_conj a rs)
  | _ -> raise (Invalid_argument "ilp_conj")
and 
    ilp_top x rs =
  match x with
    Prop.Or(a, b) -> ilp_top b (ilp_top a rs)
  | _ -> match ilp_conj x [] with [] -> rs | a -> a::rs
in ilp_top x []

end



module Simp=  (* Simplifiers for expressions *)
struct

open Lang

let realmax (x:float) (y:float) = 
  if x>y then x else y
let realmin (x:float) (y:float) = 
  if x>y then y else x

let rec evalexpr t =
  match t with
    (Val x) -> x
  | (Plus(y::ys)) -> 
      evalexpr (List.fold_left
	 (fun a b -> Val ((evalexpr a)+/ (evalexpr b))) y ys)
  | (Mult(x, y)) -> x */ (evalexpr y)
  | (Max (y::ys)) -> 
      evalexpr(
      List.fold_left (fun a b -> Val (max (evalexpr a) (evalexpr b))) y ys)
  | (Min (y::ys)) ->
      evalexpr(
      List.fold_left (fun a b -> Val(min (evalexpr a) (evalexpr b))) y ys)
  | _ -> raise (Invalid_argument "evalexpr")

let rec filter p ls =
  match ls with
    [] -> []
  | y::ys -> if (p y) then filter p ys else (y::filter p ys)

let seper xs =
  let rec sep_aux ys mxs mns ps rs=
    match ys with
      [] -> (mxs, mns, ps, rs)
    | (Max(y)::ts) -> sep_aux ts (Max(y)::mxs) mns ps rs
    | (Min(y)::ts) -> sep_aux ts mxs (Min(y)::mns) ps rs
    | (Plus(y)::ts) -> sep_aux ts mxs mns (Plus(y)::ps) rs
    | y::ts -> sep_aux ts mxs mns ps (y::rs)
  in sep_aux xs [] [] [] []

let unbundle prd dst xs =
    (let rec unbund ts rs=
    match ts with 
      [] -> rs
    | (y::ys) -> 
	if prd y then unbund ys ((dst y)@rs)
	    else unbund ys (y::rs)
    in unbund xs [])

let unbundle_max ts = unbundle is_max dest_cnstr ts
let unbundle_min ts = unbundle is_min dest_cnstr ts
let unbundle_plus ts = unbundle is_plus dest_cnstr ts

  let mk_mult m b =
    if m=zero_num then Val(zero_num)
    else if m=one_num then b
    else Mult(m, b)

  let mk_add rs b  =
    match b with
      NegInf -> b
    | PosInf -> b
    | Val(zero_num) -> Plus rs
    | _ -> 
    	(match rs with
	  [] -> b
    	| _ -> Plus (b::rs))


let delete_trivial t =
  try 
    (match (dest_cnstr t) with
     (x::[]) -> x | _ -> t)
  with _ -> t


let rec find_env x env =
  match env with
    [] -> raise Not_found
  | ((a, b)::ys) -> if a = x then b else find_env x ys

let rec member_env x env =
  try find_env x env; true with Not_found -> false

let rec set x y env = 
  match env with
    [] -> [(x, y)]
  | ((a, b)::ys) -> if x = a then (x, y)::ys else (a, b)::(set x y ys)

let add_to_val x v = add_expr (Val x) v

(*
  polynomial representation: 
   an expr of type (Plus [Val(c);Mult(m1, Var(v1)); Mult(m2, Var(v2))])
   where v are the variable namse
   m is the multiplicant.
  constants are represented as Val(c)
  each variable occurs at most once.
*)

(* make a polynomial representation of t *)

let make_poly_term n v =
  if n=const_key then v
  else 
    if is_val v then Mult((dest_val v), Var(n))
    else raise (Invalid_argument "make_poly_term")

let dest_poly_term t =
  match t with
    Val(v) -> (const_key, v)
  | Mult(m, Var(n)) -> (n, m)
  | _ -> raise (Invalid_argument "dest_poly_term")

let set a b poly =   (* set value of key a to b in polynomial poly *)
  let rec set_aux ts =
    match ts with
      [] -> [make_poly_term a b]
    | (y::ys)->
	let n, v = dest_poly_term y
	in 
	if a = n 
	then (make_poly_term n b)::ys 
	else y::(set_aux ys)
  in Plus(set_aux (dest_cnstr poly))

let value_of x poly = (* get value of x in poly *)
  let rec val_of ts =
    match ts with 
      [] -> raise Not_found
    | Var(n)::ys -> if x=n then (Val one_num) else val_of ys
    | (Mult(m, Var(n)))::ys ->
	if x=n then (Val m) else val_of ys
    | (y::ys) -> if x = const_key then y else val_of ys
  in 
  val_of (dest_cnstr poly)


(* operations on polynomials *)

(*
  poly_add v n (c+ax+by+..)
   -> (v*n) + (c+ax+by+..)
*)

let delete_empty_poly t=
  match t with
    Plus[] -> Plus[Val(zero_num)]
  | _ -> t

let rec poly_add v n poly =
  let rec add_aux xs  =
    match xs with 
      [] -> [make_poly_term n v]
    | (Mult(m, Var(a))::ys) ->
	if n = a
	then 
	  (let nb = add_expr v (Val m)
	  in 
	  if nb=Val(zero_num)
	  then ys else ((make_poly_term a nb)::ys))
	else (Mult(m, Var(a)))::(add_aux ys)
    | (Var(a)::ys) -> add_aux ((Mult(one_num, Var(a))::ys))
    | (y::ys) ->
	if (is_any_val y) & (n = const_key)
	then 
	  (let nb = add_expr v y
	  in 
	  if nb=(Val zero_num) then ys else (nb::ys))
	else (y::(add_aux ys))
  in 
  match poly with 
    Plus[] ->  Plus[make_poly_term n v]
  | _ -> delete_empty_poly (Plus(add_aux (dest_cnstr poly)))


(* poly_mult v (c+ax+by+...)
   -> (v*c+(v*ax)+(v*by)+..)
*)

let poly_mult v poly =
  let rec mult_aux xs  =
    match xs with 
      [] -> []
    | b::ys ->
	let nb=mult_expr v b
	in 
	if nb=Val(zero_num) then mult_aux ys
	else 
	  (match nb with
	    Var(_) -> (Mult(one_num, nb)::(mult_aux ys))
	  |  _ -> (nb::(mult_aux ys)))
  in delete_empty_poly (Plus(mult_aux (dest_cnstr poly)))

let poly_of_expr t =
  let poly = ref(Plus [])
  in 
  let bind_env x y = (poly:=set x y (!poly))
  in 
  let add_to_key n v =  poly:=poly_add (Val v) n !poly
  in 
  let rec redex x =
    match x with
      Max(_) -> raise (Invalid_argument ("poly_of_expr: max"))
    | Min(_) -> raise (Invalid_argument ("poly_of_expr: min"))
    | PosInf -> bind_env const_key PosInf 
    | NegInf -> bind_env const_key NegInf 
    | Mult(m, Var(n)) -> add_to_key n m
    | Var(n) -> add_to_key n one_num
    | Val(v) -> add_to_key const_key v
    | Plus(xs) ->List.iter redex xs
    | Mult(m, _) -> raise (Invalid_argument ("poly_of_expr: mult"))
  in redex t; !poly

let rec expr_of_poly t = 
  match t with 
  | Plus[] -> Val(zero_num)
  | Plus(xs) -> delete_trivial (Plus (List.map expr_of_poly xs))
  | Max(xs) -> delete_trivial (Max(List.map expr_of_poly xs))
  | Min(xs) -> delete_trivial (Min(List.map expr_of_poly xs))
  | Mult(m, b) -> Mult(m, expr_of_poly b)
  | _ -> t


(* make an expression from polynomial representation*)
(* has side effect of simplifying the expression *)


let split p ls =
  let rec split_aux xs ts fs =
    match xs with
      [] -> (ts, fs)
    | y::ys -> 
	if (p y) 
	then split_aux ys (y::ts) fs else split_aux ys ts (y::fs)
  in split_aux ls [] []

let split_pair p ls =
  let rec split_aux xs fs =
    match xs with
      [] -> raise Not_found
    | y::ys -> 
	if (p y) then (y, (ys@fs))
	else split_aux ys (y::fs)
  in split_aux ls []


let order p ls = (* find least x in list ls where p is the order relation *)
  let rec ord_aux r xs =
    match xs with
      [] -> r
    | (y::ys) -> if p y r then ord_aux y ys else ord_aux r ys
  in match ls with
    [] -> []
  | (x::xs) -> [ord_aux x xs]

let delete_empty ls =
  filter (fun x -> try (dest_cnstr x)=[] with _ -> false) ls

let reduce t=
  let rec reduce_aux t=
    match t with
      Max(xs) -> 
	let (vs, nvs) = 
	  split 
	    (fun x -> is_any_val (delete_trivial x))
	    (List.map reduce_aux xs)
	in 
	delete_trivial 
	  (Max(delete_empty(
	       (order (fun x y -> leq_expr y x) vs)@nvs)))

    | Min(xs) -> 
	let (vs, nvs) =
	  split 
	    (fun x -> is_any_val (delete_trivial x))
	    (List.map reduce_aux xs)
	in 
	delete_trivial 
	  (Min(delete_empty ((order leq_expr vs)@nvs)))
    | Plus(xs) ->
	let (vs, nvs) =
	  split 
	    (fun x -> is_any_val (delete_trivial x))
	    (List.map reduce_aux xs)
	in 
	let nt =
	     (List.fold_left add_expr (Val(zero_num)) vs)
	in 
	if nt = Val(zero_num) then 
	  delete_trivial
	    (Plus(delete_empty (nvs)))
	else 
	  delete_trivial
	    (Plus(delete_empty (nt::nvs)))
    | Mult(m, b) -> 
	mult_expr m (reduce_aux b)
    | _ -> t
  in expr_of_poly (reduce_aux t)

let distrib trm = 
  let rec push_plus y t =
    match (y, t) with
    | (_, Max(x2)) -> Max(unbundle_max (List.map (push_plus y) x2))
    | (Max(x1), _) -> Max(unbundle_max (List.map (push_plus t) x1))
    | (_, Min(x2)) -> Min(unbundle_min (List.map (push_plus y) x2))
    | (Min(x1), _) -> Min(unbundle_min (List.map (push_plus t) x1))
    | (Plus(x1), _) -> Plus(unbundle_plus (t::x1))
    | (_, Plus(x2)) -> Plus(unbundle_plus (y::x2))
    | _ -> Plus[y;t]
  and top m adds t =
    match t with
      (Val _) -> (mk_mult m t)
    | (Var _) -> (mk_mult m t )
    | PosInf -> (mk_mult m t)
    | NegInf -> (mk_mult m t)
    | (Mult(x, y)) -> (top (m*/x) adds y)
    | Max([]) -> adds
    | Max(x::xs) -> 
	Max(List.map (top m adds) xs)
    | Min(xs) -> 
	Min(List.map (top m adds) xs)
    | Plus([]) -> adds
    | Plus(x::xs) ->
	let i = top m (Plus[]) x
	in 
	top m (push_plus adds i) (Plus xs)
  in 
  delete_trivial (top one_num (Plus[]) trm)

let simp t = reduce (distrib t)

end

(* Bledsoe's version of SupInf *)

module Bledsoe=
struct

open Lang
open Simp

(* solving inequalities for a given variable *) 

let swap (a, b) = (b, a)

let remove_var x (a, b) = (* subtract variable x from poly b *)
  try
    let bv = (dest_val (value_of x b))
    in 
    (poly_add (Val(minus_num bv)) x a, poly_add (Val(minus_num bv)) x b)
  with Not_found -> (a, b)

let do_subs x (a, b) =  (* x occurs only in a *)
  let rec sub ts na nb =
    match ts with 
      [] -> ((delete_empty_poly (Plus na)), nb)
    | y::ys -> 
	let (n, e) = dest_poly_term y
	in 
      	if n = x 
	then sub ys ((make_poly_term n (Val e))::na) nb
	else sub ys na (poly_add (Val(minus_num e)) n nb)
  in sub (dest_cnstr a) [] b
  
let do_div x (a, b) =  (* x occurs only in a *)
  (try
    (let v = one_num//(dest_val (value_of x a))
    in 
    (poly_mult v a, poly_mult v b))
  with Not_found -> (a, b))


let find_soln x (a, b) = 
  let na, nb = remove_var x (a, b) (* remove x from b, adding to a *)
  in 
  let na1, nb1 = do_subs x (na, nb) (* remove c+av+.. from na, adding to nb *)
  in 
  try 
    let nv = dest_val(value_of x na1)
    in 
    if nv=zero_num then None
    else 
      let na2, nb2= 
      	if nv<zero_num then 
	  (poly_mult (one_num//nv) nb1, poly_mult (one_num//nv) na1) 
	else 
	  (poly_mult (one_num//nv) na1, poly_mult (one_num//nv) nb1) 
      in Some(na2, nb2)
  with Not_found -> Some(na1, nb1)
  
(* converts terms to polynomials then solves for polynomials *)

let is_var x poly =
  try ignore(value_of x poly); true
  with Not_found -> false

let solve ts x= 
  let rec sol_aux ps ls us oths=
  match ps with
    [] -> (ls, us, oths)
  | (y::ys) ->
     let sl = find_soln x y
      in 
      match sl with
	None -> sol_aux ys ls us oths
      |	Some(a, b) -> 
	  if is_var x a          (* test for (x<b) *)
	  then sol_aux ys ls ((a, b)::us) oths
	  else 
	    if is_var x b      (* test for (a<x) *)
	    then sol_aux ys ((a, b)::ls) us oths
	    else sol_aux ys ls us ((a,b)::oths)
  in sol_aux ts [] [] []

(*
  list of inequalities  is [(a, b); ...; ]
  where (a<= b)  and a and b are exprs

  solve_of ts x 
  results in lists [(L1, x); (L2, x); ...], [(x, U1); (x, u2)], [(a, b);...]
  where x doe not occur in the a, b, L or U.
*)

  let from_exprs rs = 
    List.map (fun (x, y) -> (poly_of_expr x,  poly_of_expr y)) rs
  let to_exprs rs =
    List.map(fun (x,y) -> 
      (reduce (expr_of_poly x), reduce (expr_of_poly y))) rs

let solve_for x ts=
  let lt, gt, oths = 
    solve (from_exprs ts) x
  in 
  (to_exprs lt, to_exprs gt, to_exprs oths)

(* given a formula F to be proved:

   1. Its negation is reduced to a set S in DNF.
   2. The set S is made up of sets of conjunctions G1,..., Gn
   3. Each conjunction Gi is an inequality represented as a pair
      (a, b) s.t. a<=b.
   4. For each pair (a, b), a variable x appears only in a or only in b.

   Formula F is assumed to have been reduced to the set S.
*)

(*type 'a set = 'a list*)
let rec member a b = 
  match b with
    [] -> false
  | (x::xs) -> 
      if a = x 
      then true 
      else member a xs

let insert a b =  (a::b)
let add_set a b = 
  if member a b then b else insert a b

(*
  given a list G of conjuctions and variable x
  return the lists Ga, Gb, Gc such that x occurs on 
  the left in the conjunctions of Ga, on the right in those of Gb
  and doesn't occur in those of Gc.
*)

let seperate x g =
  let rec sep_aux xs ga gb gc=
    match xs with
      [] -> (ga, gb, gc)
    | ((a, b)::ys) ->
	let oca = occurs x a
	and ocb = occurs x b
	in 
	if oca & ocb then raise (Invalid_argument "seperate")
	else
	let nga=if oca then (insert (a, b) ga) else ga
	and ngb=if ocb then (insert (a, b) gb) else gb
	and ngc=if not (oca or ocb) then insert (a, b) gc else gc
	in sep_aux ys nga ngb ngc
  in sep_aux g [] [] [] 

(* UPPER and LOWER functions *)

(* UPPER(S, x), LOWER(S, x): 
   S is the set of inequalities/conjunctions to be solved for x
*)


let upper s x =
  let _, ga, _ = solve_for (dest_var x) s (* seperate s x *)
  in 
  let l=List.length ga
  and us=List.map (fun (_, b) -> b) ga
  in 
  match us with
    [] -> PosInf
  | (u::[]) -> u
  | _ -> Min us
  

let lower s x =
  let gb, _, _ = solve_for (dest_var x) s (* seperate s x *)
  in 
  let l=List.length gb
  and ls=List.map (fun (a, _) -> a) gb
  in 
  match ls with
    [] -> NegInf
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
      (split_pair 
	 (fun x->
	   match x with 
	     Mult(_, Var(_)) -> true
	   | Var(_) -> true
	   |	_ -> false) xs) 
    with Not_found -> (Mult(zero_num, Var const_key), xs))
  with
    (Mult(m, v), l) -> Mult(m, v), l
  | (Var(v)), l -> Mult(one_num, Var(v)), l
  | _ -> raise (Invalid_argument "get_mult_var")


let get_mult_of n xs = 
  match 
    (try
      (split_pair 
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
	  in let c = delete_trivial (Plus(unbundle_plus c0))
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
	  in let c=delete_trivial (Plus(unbundle_plus c0))
	  in 
	  List.iter 
	    (fun a -> if occurs x a 
	    then raise (Invalid ("inff: plus", a)) else ()) c0;
	  if b>one_num then NegInf
	  else if b<one_num then Mult(one_num//(one_num-/b), c)
	  else 
	    if not (is_any_val c) 
	    then NegInf
	    else 
	      if (leq_expr c (Val(zero_num)))
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
	in supp j (simp z))
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
      then sup_aux s (simp(Plus([Mult(r, v); b1]))) h
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
	in inff j (simp z))
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
	then inf_aux s (simp(Plus([Mult(r, v); b1]))) h
	else (Plus[inf_aux s (Mult(r, v)) h; b1]))
  | Max(xs) ->
      Max(List.map(fun x-> inf_aux s x h) xs)
  | _ -> raise (Invalid_argument "inf_aux")

let sup s j = reduce (sup_aux s j [])
let inf s j = reduce (inf_aux s j [])

		   
end

module Shostak = 
struct

open Lang
open Simp
open Bledsoe

(* get list of variables in expression e *)

module VarSet=Set.Make(
  struct 
    type t = int
    let compare = compare
  end)
type var_set=VarSet.t

let set_to_list s =
  let bs=ref []
  in
  VarSet.iter (fun x-> bs:=x::!bs) s;
  !bs


let vars_of_env e env= 
  let rec list_aux xs rs = 
    match xs with 
      [] -> rs
    | (y::ys) -> list_aux ys (term_aux y rs)
  and term_aux e rs =
    match e with
      Var x -> (VarSet.add x rs)
    | Mult(_, t) -> term_aux t rs
    | Plus(xs) -> list_aux xs rs
    | Max(xs) -> list_aux xs rs
    | Min(xs) -> list_aux xs rs
    | _ -> rs
  in term_aux e env

let vars_of e = vars_of_env e VarSet.empty

(* get list of variables in conjunction of inequalities *)

let get_vars s = 
  let vs = ref VarSet.empty
  in 
  let get_aux (x, y) =
    vs:=vars_of_env x (!vs);
    vs:=vars_of_env y (!vs)
  in 
  List.iter get_aux s;
  !vs

let rec combine xs ys =  VarSet.union xs ys
(*
  match xs with
    [] -> ys
  | (b::bs) -> 
      if List.mem b ys then combine bs ys 
      else combine bs (b::ys)
*)

(*
  given interval (x, y), choose an integer between x and y 
  if no integer, choose a real
  x must be less than or equal to y (not x>y)
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

exception No_value of expr*expr

let chose_val (x, y)=
  if x=y then x
  else
    match (x, y) with
      (NegInf, PosInf) -> Val(zero_num)
    | (NegInf, Val(b)) -> Val(floor_num b)
    | (Val(a), PosInf) -> Val(ceiling_num a)
    | (Val(a), Val(b)) -> Val(chose_int (a, b))
    | _ -> raise (No_value (x, y))

(* eliminate variable x from set of inequalities s 
   raise Infeasible if no solutions can be found 
   otherwise return list of inequalities with x eliminated 
   and new value for x
*)

exception Infeasible


let elim_var s x =
  let inf_x = inf s (Var x)
  and sup_x = sup s (Var x)
  in 
  if lt_expr sup_x inf_x then raise Infeasible
  else
    let nv = chose_val (inf_x, sup_x)
    in let binding = bind (Var x) nv []
    in 
    (nv, 
     List.map 
      (fun (a, b) -> 
	(reduce (subst a binding), 
	 reduce (subst b binding))) s)

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
  VarSet.iter apply_aux vs; !bindings

*)

(* for inequalities s:
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
	let (na, nb) = (simp a, simp b)
	in 
	let is_valid = 
	  (try leq_expr na nb 
	  with Unknown -> true)
	and nvs = (vars_of_env nb (vars_of_env  na VarSet.empty))
	in 
	if is_valid = false
	then raise Infeasible
	else 
	  if VarSet.is_empty nvs then
	  remove_aux ys vs rs
	  else remove_aux ys (combine nvs vs) ((a, b)::rs)
  in remove_aux s VarSet.empty []

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
  let nvs = set_to_list vs
  in 
  let solns = apply_elim ns nvs
  in 
  if has_integer_solns solns
  then solns
  else raise Infeasible

(*
  for list of conjunction of inequalities ineqs:
   try to solve each 
   if any fails then list of conjunctions is invalid 
   (and therefore original formula is valid)
   if all succeed then list is valid and original is invalid.
   raise Has_solution if any conjunction is valid
   raise Possible_solution if any conjunction has real but not integer solns.
  return true if all conjunctions are invalid.
*)

let rec solve_inequalities ineqs =
  match ineqs with
    [] -> []
  | (x::xs) ->
      try
	(let sln =solve_ineq_conj x
	in 
	raise (Has_solution sln))
      with
	Infeasible -> solve_inequalities xs
      |	a -> raise a

(* decide t: 
   top level function.
   convert negation of t to a set of ilps
   test for validity
   if invalid give true (t is valid).
   if valid raise Has_solutions with solutions.
   if undecidable raise Possible_solutions.
*)

let decide t =
  let ilps = 
    Lang.mk_ilp (Lang.mk_dnf_as_expr (Prop.mk_not  t))
  in 
  try 
    ignore(solve_inequalities ilps); true
  with 
    (Has_solution x) -> raise (Has_solution x)
  | (Possible_solution x) -> raise (Possible_solution x)
  | x -> raise x

let soln f a =
  try f a 
  with 
    Has_solution x -> x
  | Possible_solution x -> x

end


let expr_string =PPexprs.expr_string 
let boolexpr_string = PPexprs.boolexpr_string
let comp_string =PPexprs.comp_string


let poly_of_expr = Simp.poly_of_expr
let expr_of_poly = Simp.expr_of_poly
let evalexpr = Simp.evalexpr
let reduce = Simp.reduce
let distrib = Simp.distrib
let simp = Simp.simp

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

