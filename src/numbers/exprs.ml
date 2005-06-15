(*-----
   Name: exprs.ml
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


(* Sets *)

module ExprSet=Set.Make(
  struct 
    type t = expr
    let compare = compare
  end)
type set=ExprSet.t

let set_to_list s =
  let bs=ref []
  in
  ExprSet.iter (fun x-> bs:=x::!bs) s; (!bs)



(* Exceptions *)
(* 
   [Unknown]: result of an operation can be determined 

   [Occurs]: Occurs check.
 *)
exception Unknown
exception Occurs

(*
   const_key: identifier of constants in a polynomial expressions 
   e.g. in polynomial expressions `1+2*x';
   term 2*x has key x and term 1 has key const_key
 *)

let const_key = 0

(* Destrucutors *)

let dest_val t = 
  match t with
    Val(v) -> v
  | _ -> raise (Invalid_argument "dest_val")

let dest_var t = 
  match t with
    Var(v) -> v
  | _ -> raise (Invalid_argument "dest_var")

let dest_cnstr t=
  match t with
    Plus xs -> xs
  | Max xs -> xs
  | Min xs -> xs
  | _ -> raise (Invalid_argument "dest_cnstr")

let strip_cnstr t =
  match t with
    (Var _) -> [t]
  | PosInf -> [t]
  | NegInf -> [t]
  | (Val _) -> [t]
  | (Mult _) -> [t]
  | _ -> (dest_cnstr t)

let dest_plus t=
  match t with
    Plus xs -> xs
  | _ -> raise (Invalid_argument "dest_plus")
let dest_max t=
  match t with
    Max xs -> xs
  | _ -> raise (Invalid_argument "dest_max")
let dest_min t=
  match t with
    Min xs -> xs
  | _ -> raise (Invalid_argument "dest_min")

(* Recognisers *)

let is_val t =
  match t with
    Val(_) -> true 
  | _ -> false

let is_any_val x =
  match x with 
    Val(_) -> true
  | PosInf -> true
  | NegInf -> true
  | _ -> false

(**
   [is_const_key x]: true if integer [x] is the constant identifier.
 *)
let is_const_key x = (x=const_key)


(**
   [is_const x]: true if variable [x] is the constant identifier.
 *)
let is_const t =
  match t with
    Var(x) -> (x = const_key)
  | _ -> false

let is_var t =
  match t with
    Var(x) -> true
  | _ -> false

let is_cnstr t=
  match t with
    Plus _ -> true
  | Max _ -> true
  | Min _ -> true
  | _ -> false

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

(* Constructors *)
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

(* Pretty printing *)
module PP =
  struct

    let rec list_string f sep x =
      match x with 
	[] -> ""
      | (b::[]) -> (f b)
      | (b::bs) -> (f b)^sep^(list_string f sep bs)

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


    let print t = 
      let rec print_aux t = 
	match t with 
	  PosInf -> Format.printf "posinf@,"
	| NegInf -> Format.printf "neginf@,"
	| Val(n) -> Format.printf "%s@," (Num.string_of_num n) 
	| Var(v) -> Format.printf "v%d@," v
	| Mult(m, e) -> 
	    Format.printf "@[<2>%s*" (Num.string_of_num m);
	    print_aux e;
	    Format.printf "@]@,"
	| Plus tl -> 
	    Format.printf "@[<2>";
	    Printer.print_list 
	      (print_aux, (fun _ -> Format.printf "@,+@,")) tl;
	    Format.printf "@]@,"
	| Max tl ->
	    Format.printf "@[<2>max(";
	    Printer.print_sep_list 
	      (print_aux, ",") tl;
	    Format.printf ")@]@,"
	| Min tl -> 
	    Format.printf "@[<2>min(";
	    Printer.print_sep_list 
	      (print_aux, ",") tl;
	    Format.printf ")@]@,"
      in 
      Format.printf "@[<2>";
      print_aux t;
      Format.printf "@]"

  end

(* Substitution *)

type substitution = (expr * expr) list

let rec find x env =
  match env with
    [] -> raise Not_found
  | ((a, b)::ys) -> 
      if a = x 
      then b 
      else find x ys

let member x env = 
  try find x env; true 
  with Not_found -> false

let bind x y env = (x, y)::env

(* [set x y env]: set value of [x] in [env] to [y] *)
let set x y env = 
  let rec set_aux ls rst = 
    match env with
      [] -> (x, y)::rst
    | (a, b)::ys -> 
	if x = a 
	then List.rev_append ys ((x, y)::rst)
	else set_aux ys ((a, b)::rst)
  in 
  List.rev(set_aux env [])

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
  in 
  subst_aux e

(*
   [occur e t]: occurs check, true if [e] occurs in [t] 
 *)
let occurs e t = 
  let rec occs_aux x=
    if e=x then raise Occurs
    else 
      (match x with
      	Plus xs -> (List.iter occs_aux xs)
      |	Mult(a, b) -> (occs_aux b)
      |	Max xs -> List.iter occs_aux xs
      |	Min xs -> List.iter occs_aux xs
      |	_ -> ())
  in 
  (try (occs_aux t; false)
  with Occurs -> true)


(* Arithmetic operations *)

let inc a = Plus[a; Val(num_of_int 1)]

let add a b =
  match (a, b) with
    (Val(x), Val(y)) -> Val(x+/y)
  | _ -> 
      if (a=PosInf & b=NegInf) or (a=NegInf & b=PosInf) 
      then Val(num_of_int 0)
      else if a=PosInf or b=PosInf then PosInf
      else if a=NegInf or b=NegInf then NegInf
      else Plus[a; b]

(** [add_const c e]: Add constant [c] to [e] *)
let add_const c e = add (Val c) e

(** [mult c e]: Multiply [e] by constant [c]  *)
let rec mult v b =
  if v=zero_num 
  then Val(num_of_int 0)
  else 
    if v = (num_of_int 1) 
    then b
    else 
      (match b with
	Val(y) -> Val(v*/y)
      | Mult(n, Val(zero_num)) -> Val(zero_num)
      | Mult(n, b) -> mult (v*/n) b
      | Var(_) -> Mult(v, b)
      | _ -> 
      	  if b=PosInf then if v<zero_num then NegInf else b
      	  else 
	    if b=NegInf then if v<zero_num then PosInf else b
	    else Mult(v, b))


(**
   Comparisons and tests.

   [is_neg e]: expression [e] is <0.

   [leq a b]: implements [a<=b].

   [lt a b]: implements [a<b]
 *)
let is_neg v = 
  match v with
    NegInf -> true
  | PosInf -> false
  | Val(v) -> v<zero_num
  | _ -> raise Unknown

let rec leq a b =
  if a=b then true
  else
    match (a, b) with
      (Val(x), Val(y)) -> x <= y
    | (NegInf, _) -> true
    | (PosInf, _ ) -> false
    | (_, NegInf) -> false
    | (_, PosInf) -> true
    | _ -> raise Unknown

let lt a b = (not (a = b)) & leq a b

(**
   Manipulating and Simplifying expressions
 *)

(**
   [eval t]: try to reduce [t] to a value.
 *)
let rec eval t =
  match t with
    (Val x) -> x
  | (Plus(y::ys)) -> 
      eval (List.fold_left
	      (fun a b -> Val ((eval a)+/ (eval b))) y ys)
  | (Mult(x, y)) -> x */ (eval y)
  | (Max (y::ys)) -> 
      eval (List.fold_left 
	      (fun a b -> Val (max (eval a) (eval b))) y ys)
  | (Min (y::ys)) ->
      eval (List.fold_left 
	      (fun a b -> Val(min (eval a) (eval b))) y ys)
  | _ -> raise (Invalid_argument "eval")


(**
   [seper xs]: Seperate the list of expressions [xs] into the 
   lists [mxs] of expressions built from [Max],
   [mns] of expressions built from [Min],
   [pls] of expressions built from [Plus] and
   [rs] of other expressions.
   Returns [(mxs, mns, pls, rs)].
 *)
let seper xs =
  let rec sep_aux ys mxs mns ps rs=
    match ys with
      [] -> (mxs, mns, ps, rs)
    | (Max(y)::ts) -> sep_aux ts (Max(y)::mxs) mns ps rs
    | (Min(y)::ts) -> sep_aux ts mxs (Min(y)::mns) ps rs
    | (Plus(y)::ts) -> sep_aux ts mxs mns (Plus(y)::ps) rs
    | y::ts -> sep_aux ts mxs mns ps (y::rs)
  in 
  sep_aux xs [] [] [] []


(**
   [delete_trivial t]: If [t] is a constructor with a single element
   then strip the constructor off. For example, [delete_trivial (Plus
   [x])] is [x].
 *)
let delete_trivial t =
  match t with
  | Plus [x] -> x
  | Max [x] -> x
  | Min [x] -> x
  | _ -> t

module Poly=
  struct

(* Set up some aliases to avoid ambiguity *)
    let add_expr = add 
    let mult_expr = mult

(**
   Polynomial representation: 

   An expr of type [ Plus [Val(c); Mult(m1, Var(v1)); Mult(m2,
   Var(v2))] ] where [v] are the variable names [m] is the
   multiplicant. Constants are represented as [Val(c)]. Each variable
   occurs at most once.
 *)

(* Constructor *)

(** 
   [make_term n c]: Make a term of a polynomial from variable [n] and
   value [c].

   If [is_const n] then returns [c].
   If [~is_const n] then returns [c*n]

   If [n] is not a variable or [c] is not a value then fails with
   [Invalid_argument].
 *)
    let make_term n c =
      if (is_const_key n)
      then c
      else 
	(if (is_val c)
	then (Mult((dest_val c), (Var n)))
	else (raise (Invalid_argument "make_term")))

(** 
   [dest_term t]: Destruct a term of a polynomial.
   
   If [t] is [(c*n)], returns [(n, c)] where [n] is the variable and
   [c] the constant.

   If [t] is [c], returns [(const_key, c)] (where [c] is a constant).

   In all other cases, raises [Invalid_argument].
 *)
    let dest_term t =
      match t with
	Val(v) -> (const_key, v)
      | Mult(m, Var(n)) -> (n, m)
      | _ -> raise (Invalid_argument "dest_poly_term")


(**
   [set_term a c poly]: Set multiplicant of variable [a] to value [c]
   in polynomial [poly].
 *)
    let set_term a c poly =  
      let rec set_aux ts rs =
	match ts with
	  [] -> (make_term a c)::rs
	| (y::ys)->
	    let n, v = dest_term y
	    in 
	    if a = n 
	    then 
	      List.rev_append ys ((make_term n c)::ys)
	    else set_aux ys (y::rs)
      in 
      match poly with
	Plus xs -> 
	  Plus(List.rev (set_aux xs []))
      | _ -> raise (Invalid_argument "set_term")

(**
   [value_of x poly]: Get the multiplicant of variable [x] in
   polynomial [poly] as an expression.

   If [x] doesn't occur in [poly], returns [Val 0].

   Fails with [Invalid_argument] if [poly] isn't well formed.
 *)
    let value_of x poly = (* get value of x in poly *)
      let rec val_of ts =
	match ts with 
	  [] -> (Val zero_num)
	| Var(n)::ys -> if x=n then (Val one_num) else val_of ys
	| (Mult(m, Var(n)))::ys ->
	    if x=n then (Val m) else val_of ys
	| (y::ys) -> if x = const_key then y else val_of ys
      in 
      match poly with
	Plus xs -> val_of xs
      | Max _ -> raise (Invalid_argument "value_of: Max")
      | Min _ -> raise (Invalid_argument "value_of: Min")
      | _ -> val_of [poly]


(**
   [is_var_of poly x]: true if variable [x] occurs in [poly].

   Fails with [Invalid_argument] if [poly] isn't well formed.
 *)
    let is_var_of poly x =
      let rec var_of ts =
	match ts with 
	  [] -> false
	| Var(n)::ys -> ((x=n) || (var_of ys))
	| (Mult(m, Var(n)))::ys ->
	    ((x=n) || (var_of ys))
	| (Plus xs)::ys -> ((var_of xs) || (var_of ys))
	| (Max xs)::ys -> ((var_of xs) || (var_of ys))
	| (Min xs)::ys -> ((var_of xs) || (var_of ys))
	| (y::ys) -> (var_of ys)
      in 
      match poly with
	Plus xs -> var_of xs
      | Max _ -> raise (Invalid_argument "value_of: Max")
      | Min _ -> raise (Invalid_argument "value_of: Min")
      | _ -> var_of [poly]

(**
   [reform_empty t]: If [t] is an empty polynomial, make it
   minimally correct.
 *)
    let reform_empty t=
      match t with
	Plus[] -> Plus[Val(zero_num)]
      | _ -> t

(**
   [remove_empty ts]: Remove empty terms from list [ts].
 *)
    let remove_empty ls =
      let pred t = 
	not ((is_cnstr t)  && ((dest_cnstr t)=[]))
      in 
      List.filter pred ls

(* 
   [add c n poly]: If [poly = d + ax + by + ...], [add c n poly] calculates
   [(c*n) + poly].
 *)
    let add v n poly =
      let rec add_aux xs  =
	match xs with 
	  [] -> [make_term n v]
	| (Mult(m, Var(a))::ys) ->
	    if n = a
	    then 
	      (let nb = add_expr v (Val m)
	      in 
	      if nb=Val(zero_num)
	      then ys else ((make_term a nb)::ys))
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
	Plus[] ->  Plus[make_term n v]
      | Plus xs -> reform_empty (Plus(add_aux xs)) 
      | Max _ -> raise (Invalid_argument "add")
      | Min _ -> raise (Invalid_argument "add")
      | _ -> reform_empty (Plus(add_aux [poly]))

(**
   [mult c poly]: If [poly = d + ax + by + ...], [mult c poly] calculates
   [c * poly], where [c] is a constant.
 *)
    let mult v poly =
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
      in 
      match poly with 
	Plus xs -> reform_empty (Plus(mult_aux xs)) 
      | Max _ -> raise (Invalid_argument "mult")
      | Min _ -> raise (Invalid_argument "mult")
      | _ -> reform_empty (Plus(mult_aux [poly]))

(**
   Conversions between a simple expression and its polynomial form.
 *)

(**
   [poly_of_expr t]: Convert expression [t] to its polynomial form.
 *)
    let poly_of_expr t =
      let add_to_key n v poly = add (Val v) n poly
      in 
      let rec redex x poly=
	match x with
	  PosInf -> set_term const_key PosInf poly
	| NegInf -> set_term const_key NegInf poly
	| Mult(m, Var(n)) -> add_to_key n m poly
	| Var(n) -> add_to_key n one_num poly
	| Val(v) -> add_to_key const_key v poly
	| Plus(xs) -> List.fold_left redex poly xs
	| Mult(m, _) -> raise (Invalid_argument ("poly_of_expr: mult"))
	| Max(_) -> raise (Invalid_argument ("poly_of_expr: max"))
	| Min(_) -> raise (Invalid_argument ("poly_of_expr: min"))
      in 
      redex t (Plus [])

(**
   [poly_of_expr t]: Convert expression [t] to its polynomial form.
 *)
    let rec expr_of_poly t = 
      match t with 
      | Plus[] -> Val(zero_num)
      | Plus(xs) -> delete_trivial (Plus (List.map expr_of_poly xs))
      | Max(xs) -> delete_trivial (Max(List.map expr_of_poly xs))
      | Min(xs) -> delete_trivial (Min(List.map expr_of_poly xs))
      | Mult(m, b) -> Mult(m, expr_of_poly b)
      | _ -> t


(**
   [reduce t]: Convert expression [t] to polynomial form. (As as
   side-effect, [t] is simplified.)
 *)
    let reduce t=
      let rec reduce_aux t=
	match t with
	 Max(xs) -> 
	    let (vs, nvs) = 
	      List.partition
		(fun x -> is_any_val (delete_trivial x))
		(List.map reduce_aux xs)
	    in 
	    let nvs1=
	      match vs with 
		[] -> nvs
	      | _ -> (Lib.least (fun x y -> leq y x) vs)::nvs
	    in 
	    delete_trivial (Max(remove_empty nvs1))
	| Min(xs) -> 
	    let (vs, nvs) =
	      List.partition
		(fun x -> is_any_val (delete_trivial x))
		(List.map reduce_aux xs)
	    in 
	    let nvs1=
	      match vs with 
		[] -> nvs
	      | _ -> (Lib.least leq vs)::nvs
	    in 
	    delete_trivial (Min(remove_empty nvs1))
	| Plus(xs) ->
	    let (vs, nvs) =
	      List.partition
		(fun x -> is_any_val (delete_trivial x))
		(List.map reduce_aux xs)
	    in 
	    let nt =
	      (List.fold_left add_expr (Val(zero_num)) vs)
	    in 
	    if nt = Val(zero_num) 
	    then delete_trivial (Plus(remove_empty (nvs)))
	    else delete_trivial	(Plus(remove_empty (nt::nvs)))
	| Mult(m, b) -> mult_expr m (reduce_aux b)
	| _ -> t
      in 
      expr_of_poly (reduce_aux t)

(* 
   [unbundle prd dst xs]: Apply destructor [dst] to every [x] in [xs]
   which satisfies [prd] to obtain a list [y]. Return the list of
   elements of [xs which don't satisfy [prd] concatenated to the lists
   resulting from the destructor.

   A utility function for [simp].
 *)
    let unbundle prd dst xs =
      let rec unbund ts rs=
	match ts with 
	  [] -> rs
	| (y::ys) -> 
	    if prd y then unbund ys ((dst y)@rs)
	    else unbund ys (y::rs)
      in 
      unbund xs []

    let unbundle_max ts = unbundle is_max dest_max ts
    let unbundle_min ts = unbundle is_min dest_min ts
    let unbundle_plus ts = unbundle is_plus dest_plus ts

(** 
   [simp trm]: Simplify expression [trm], return as a polynomial.

   [distrib trm]: Distribute multiplication across addition in
   expressions [trm]. (used by [simp])
 *)
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


module IneqSolver=
  struct
    (* Solving inequalities for a given variable *)

    (** 
       [remove_var_rhs x (a, b)]: subtract variable [x] from polynomials
       [a] and [b] if it occurs in [b].
     *)
    let remove_var_rhs x (a, b) = 
      let bv = 
	try Some(dest_val (Poly.value_of x b))
	with Not_found -> None
      in 
      (match bv with
	Some(bc) -> 
	  (Poly.add (Val(minus_num bc)) x a, Poly.add (Val(minus_num bc)) x b)
      | _ -> (a, b))


(**
   [do_subs x (a, b)]:
   
   Remove variable [Var x] from [a=<b], by subtracting from
   polynomial [a] and adding to polynomial [b].
 *)
    let do_subs x (a, b) =  (* x occurs only in a *)
      let rec sub ts na nb =
	match ts with 
	  [] -> ((Poly.reform_empty (Plus na)), nb)
	| y::ys -> 
	    let (n, c) = Poly.dest_term y
	    in 
      	    if n = x 
	    then sub ys ((Poly.make_term n (Val c))::na) nb
	    else sub ys na (Poly.add (Val(minus_num c)) n nb)
      in 
      sub (strip_cnstr a) [] b 


(**
   [do_div x (a, b)]:
   
   Divide [a=<b] by the multiplicant of variable [x] in [a].
 *)
    let do_div x (a, b) =  
      let ac =
	try Some(dest_val (Poly.value_of x a))
	with Not_found -> None
      in 
      match ac with
	None -> (a, b)
      | Some(av) -> 
	  let v = one_num//(dest_val (Poly.value_of x a))
	  in 
	  (Poly.mult v a, Poly.mult v b)


(**
   [find_soln x (a, b)]:
   Find a value for variable [x] which makes [a=<b] true.
 *)
    let find_soln x (a, b) = 
(* remove x from b, adding to a *)
      let na, nb = remove_var_rhs x (a, b) 
      in 
(* remove c+ax+.. from na, adding to nb *)
      let na1, nb1 = do_subs x (na, nb) 
      in 
      let nc = dest_val (Poly.value_of x na1)
      in 
      if(nc = zero_num)
      then Some(na1, nb1)
      else 
	let na2, nb2= 
	  if nc<zero_num then 
	    (Poly.mult (one_num//nc) nb1, Poly.mult (one_num//nc) na1) 
	  else 
	    (Poly.mult (one_num//nc) na1, Poly.mult (one_num//nc) nb1) 
	in 
	Some(na2, nb2)

(**
   [is_var_of poly x]: true if [x] is a variable of polynomial [poly]
*)
(*
    let is_var_of poly x =
      try ignore(Poly.value_of x poly); true
      with Not_found -> false
*)
(**
   [solve ts x]: try to solve list of inequalities [ts] for [x].
*)
    let solve ts x= 
      let rec sol_aux ps ls us oths=
	match ps with
	  [] -> (ls, us, oths)
	| (y::ys) ->
	    let sl = find_soln x y
	    in 
	    match sl with
	      None -> sol_aux ys ls us oths
	    | Some(a, b) -> 
		if Poly.is_var_of a x          (* test for (x<b) *)
		then sol_aux ys ls ((a, b)::us) oths
		else 
		  if Poly.is_var_of b x      (* test for (a<x) *)
		  then sol_aux ys ((a, b)::ls) us oths
		  else sol_aux ys ls us ((a,b)::oths)
      in 
      sol_aux ts [] [] []

(**
   [solve_for x ts]: Solve list of inequalities [ts] for variable [x].

   The list of inequalities  is of the form [(a, b); ...; ]
   where [(a, b)] represents [(a=<b)] and [a] and [b] are exprs.

   If [x] has a solution, results in lists [(L1, x); (L2, x); ...],
   [(x, U1); (x, U2), ... ] [(a, b);...]  where variable [x] does not occur
   in the [a], [b], [L] or [U].
 *)
    let solve_for x ts=
      let from_exprs rs = 
	List.map 
	  (fun (x, y) -> (Poly.poly_of_expr x, Poly.poly_of_expr y)) rs
      in 
      let to_exprs rs =
	List.map(fun (x,y) -> 
	  (Poly.reduce (Poly.expr_of_poly x), 
	   Poly.reduce (Poly.expr_of_poly y))) rs
      in 
      let lt, gt, oths = 
	solve (from_exprs ts) x
      in 
      (to_exprs lt, to_exprs gt, to_exprs oths)

  end



