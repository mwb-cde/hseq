
type ('a,'b)boolexpr =
    Bool of bool
  | Not of ('a, 'b)boolexpr
  | And of ('a, 'b)boolexpr * ('a, 'b)boolexpr
  | Or of ('a, 'b)boolexpr * ('a, 'b)boolexpr
  | Implies of ('a, 'b)boolexpr * ('a, 'b)boolexpr
  | Iff of ('a, 'b)boolexpr * ('a, 'b)boolexpr
  | Equals of ('a, 'b) boolexpr * ('a, 'b) boolexpr
  | Bexpr of 'a                  (* boolean expressions, such as equality *)
  | Var of 'b                     (* variables *)

(* manipulation *)

let dest_var t = 
  match t with
    Var(v) -> v
  | _ -> raise (Invalid_argument "dest_var")


let mk_and a b = And(a, b)
let mk_not a = Not a
let mk_or a b = Or(a, b)

let mk_true() = Bool(true)
let mk_false() = Bool(false)

let mk_implies a b = mk_or (mk_not a) b
let mk_iff a b = mk_and (mk_implies a b) (mk_implies b a)
let mk_equals a b = Equals(a, b)

let mk_bexpr a = Bexpr(a)

(* boolexpr to CNF or DNF *)

let conj_to_list x = 
  let rec mk_clist t rs =  (* list of conjunctions *)
    match t with 
      And(a, b) -> mk_clist a (mk_clist b rs)
    | Bool(b) -> if b then rs else []
    | _ -> t::rs
  in mk_clist x []

let disj_to_list x = 
  let rec mk_dlist t rs =  (* list of disjunctions *)
    match t with 
      Or(a, b) -> mk_dlist a (mk_dlist b rs)
    | Bool(b) -> if b then [] else rs
    | _ -> t::rs
  in mk_dlist x []

let filter_empty xs =
  let rec filter_emp ys =
    match ys with 
      [] -> []
    | ([]::yys) -> filter_emp yys
    | (y::yys) -> y::(filter_emp yys)
  in filter_emp xs

let conj_to_cnf t =
  let tl = conj_to_list t
  in 
  let cl = List.map disj_to_list tl
  in filter_empty cl
  
let disj_to_dnf t =
  let tl = disj_to_list t
  in 
  let cl = List.map conj_to_list tl
  in filter_empty cl

(* put boolexpr into DNF form *)

(*
  DNF is list of list of exprs:
   (a&b&c) | (d&e&f) is [[a;b;c];[d;e;f]]
  (DNF is a boolexpr for now)
*)

(* push_conj x y -> x or y with conjunctions pushed into x and y *)
let rec push_conj x y =          
  match (x, y) with
    Or(a, b), _ -> Or(push_conj a y, push_conj b y)
  | _, Or(a, b) -> Or(push_conj x a, push_conj x b)
  | Bool (true), _ -> y
  | _, Bool(true) -> x
  | Bool(false), _ -> Bool(false)
  | _, Bool(false) -> Bool(false)
  | _ -> And(x, y)

let mk_dnf x =
let rec dnf_top n p t =
  match t with
    Bool b -> Bool((not n) & b)
  | Var(a) -> if n then (Not t) else t
  | Bexpr(a) -> if n then (Not t) else t
  | Not a -> dnf_top (not n) p a
  | And(a, b) -> 
      if n 
      then dnf_top false p (mk_or (mk_not a) (mk_not b))
      else 
	let na = (dnf_top n p a)
	and nb = (dnf_top n (Bool true) b)
	in push_conj na nb
  | Or(a, b) -> 
      if n
      then 
	dnf_top false p (mk_and (mk_not a) (mk_not b))
      else
	(mk_or(dnf_top n p a) (dnf_top n p b))
  | Implies(a, b) -> dnf_top n p (mk_or (mk_not a) b)
  | Iff(a, b) -> dnf_top n p (mk_and (Implies(a,b)) (Implies(b, a)))
  | Equals(a, b) -> dnf_top n p (mk_and (Implies(a,b)) (Implies(b, a)))
in disj_to_dnf(dnf_top false (Bool true) x)


let list_to_conj xs =
  let rec mk_conj ys=
    match ys with
    | [] -> failwith "list_to_conj: invalid argument"
    |  y::[] -> y
    | (y::yys) -> And(y, mk_conj yys)
  in mk_conj xs 

let list_to_disj xs =
  let rec mk_disj ys=
    match ys with
    | [] -> failwith "list_to_disj: invalid argument"
    |  y::[] -> y
    | (y::yys) -> Or(y, mk_disj yys)
  in mk_disj xs 

let dnf_to_disj x =
  let cl = List.map list_to_conj x
  in list_to_disj cl


(* put boolexpr into CNF form *)
(*
  CNF is list of list of exprs:
   (a|b|c) & (d|e|f) is [[a;b;c];[d;e;f]]
  (CNF is a boolexpr for now)
*)

(* push_disj x y -> x and y with disjunctions pushed into x and y *)

let rec push_disj x y =          
  match (x, y) with
  | And(a, b), _ -> And(push_disj a y, push_disj b y)
  | _, And(a, b) -> And(push_disj x a, push_disj x b)
  | Bool (true), _ -> Bool(true)
  | _, Bool(true) -> Bool(true)
  | Bool(false), _ -> y
  | _, Bool(false) -> x
  | _ -> Or(x, y)

  
let mk_cnf x =
  let rec cnf_top n p t =
    match t with
      Bool b -> Bool((not n) & b)
    | Var(a) -> if n then Not(t) else t
    | Bexpr(a) -> if n then Not t else t
    | Not a -> cnf_top (not n) p a
    | And(a, b) -> 
      	if n
      	then
	  cnf_top false p (mk_or (mk_not a) (mk_not b))
      	else 
       	  mk_and (cnf_top false p a) (cnf_top false p b)
    | Or(a, b) -> 
      	if n
      	then 
	  cnf_top false p (mk_and (mk_not a) (mk_not b))
      	else
	  let na = (cnf_top false p a)
	  and nb = (cnf_top false (Bool false) b)
	  in push_disj na nb
    | Implies(a, b) -> cnf_top n p (mk_or (mk_not a) b)
    | Iff(a, b) -> cnf_top n p (mk_and (Implies(a,b)) (Implies(b, a)))
    | Equals(a, b) -> cnf_top n p (mk_and (Implies(a,b)) (Implies(b, a)))
  in conj_to_cnf(cnf_top false (Bool true) x)


let cnf_to_conj x =
  let dl = List.map list_to_disj x
  in list_to_conj dl;;


let rec strip_vars t =
  match t with
    Var(_) -> mk_or(mk_true()) (mk_false())
  | Not(a) -> Not(strip_vars a)
  | And(a, b) -> And(strip_vars a, strip_vars b)
  | Or(a, b) -> Or(strip_vars a, strip_vars b)
  | Implies(a, b) ->Implies(strip_vars a, strip_vars b)
  | Iff(a, b) -> Iff(strip_vars a, strip_vars b)
  | _ -> t

