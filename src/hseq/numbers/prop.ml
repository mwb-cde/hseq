(*----
  Name: prop.ml
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


type ('a,'b)boolexpr =
  | Bool of bool
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
    | Var(v) -> v
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

(* Recognisers *)

let is_true a = 
  match a with
    | Bool(true) -> true
    | _ -> false

let is_false a = 
  match a with
    | Bool(false) -> true
    | _ -> false

(* boolexpr to CNF or DNF *)

let conj_to_list x = 
  let rec mk_clist t rs =  (* list of conjunctions *)
    match t with 
      | And(a, b) -> mk_clist a (mk_clist b rs)
      | Bool(b) -> 
	if b & not (rs == [])
	then rs
	else [t]
      | _ -> t::rs
  in mk_clist x []

let disj_to_list x = 
  let rec mk_dlist t rs =  (* list of disjunctions *)
    match t with 
      | Or(a, b) -> mk_dlist a (mk_dlist b rs)
      | Bool(b) -> 
	if b or (rs == [])
	then [t] 
        else rs
      | _ -> t::rs
  in mk_dlist x []

let filter_empty xs = List.filter (fun x -> not (x = [])) xs

let conj_to_cnf t =
  let tl = conj_to_list t in 
  let cl = List.map disj_to_list tl
  in
  filter_empty cl
  
let disj_to_dnf t =
  let tl = disj_to_list t in 
  let cl = List.map conj_to_list tl
  in
  filter_empty cl

(* put boolexpr into DNF form *)

(*
  DNF is list of list of exprs:
  (a&b&c) | (d&e&f) is [[a;b;c];[d;e;f]]
  (DNF is a boolexpr for now)
*)

(* push_conj x y -> x or y with conjunctions pushed into x and y *)
let rec push_conj x y =          
  match (x, y) with
    | (Or(a, b), _) -> Or(push_conj a y, push_conj b y)
    | (_, Or(a, b)) -> Or(push_conj x a, push_conj x b)
    | (Bool(true), _) -> y
    | (_, Bool(true)) -> x
    | (Bool(false), _) -> Bool(false)
    | (_, Bool(false)) -> Bool(false)
    | _ -> And(x, y)

(* [dnf_top n p t]: Reduce [t] to dnf form.

   [n]: whether to negate [t].
*)
let rec dnf_top n t =
  match t with
    | Bool b -> if n then Bool(not b) else t
    | Var(a) -> if n then Not(t) else t
    | Bexpr(a) -> if n then Not(t) else t
    | Not a -> dnf_top (not n) a
    | And(a, b) -> 
      if n 
      then dnf_top false (mk_or (mk_not a) (mk_not b))
      else 
	let na = dnf_top n a
	and nb = dnf_top n b
	in
        push_conj na nb
    | Or(a, b) -> 
      if n
      then dnf_top false (mk_and (mk_not a) (mk_not b))
      else mk_or (dnf_top n a) (dnf_top n b)
    | Implies(a, b) -> dnf_top n (mk_or (mk_not a) b)
    | Iff(a, b) -> dnf_top n (mk_and (Implies(a,b)) (Implies(b, a)))
    | Equals(a, b) -> dnf_top n (mk_and (Implies(a,b)) (Implies(b, a)))

let mk_dnf x = disj_to_dnf(dnf_top false x) 

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
    | (And(a, b), _) -> And(push_disj a y, push_disj b y)
    | (_, And(a, b)) -> And(push_disj x a, push_disj x b)
    | (Bool (true), _) -> Bool(true)
    | (_, Bool(true)) -> Bool(true)
    | (Bool(false), _) -> y
    | (_, Bool(false)) -> x
    | _ -> Or(x, y)

      
let rec cnf_top n t =
  match t with
    | Bool b -> if n then Bool(not b) else t
    | Var(a) -> if n then Not(t) else t
    | Bexpr(a) -> if n then Not t else t
    | Not a -> cnf_top (not n) a
    | And(a, b) -> 
      if n
      then cnf_top false (mk_or (mk_not a) (mk_not b))
      else mk_and (cnf_top n a) (cnf_top n b)
    | Or(a, b) -> 
      if n
      then cnf_top false (mk_and (mk_not a) (mk_not b))
      else
	let na = cnf_top n a
	and nb = cnf_top n  b
	in 
        push_disj na nb
    | Implies(a, b) -> cnf_top n (mk_or (mk_not a) b)
    | Iff(a, b) -> cnf_top n (mk_and (Implies(a,b)) (Implies(b, a)))
    | Equals(a, b) -> cnf_top n (mk_and (Implies(a,b)) (Implies(b, a)))

let mk_cnf x = conj_to_cnf(cnf_top false x) 

let cnf_to_conj x =
  let dl = List.map list_to_disj x
  in
  list_to_conj dl

let rec strip_vars t =
  match t with
    | Var(_) -> mk_or(mk_true()) (mk_false())
    | Not(a) -> Not(strip_vars a)
    | And(a, b) -> And(strip_vars a, strip_vars b)
    | Or(a, b) -> Or(strip_vars a, strip_vars b)
    | Implies(a, b) ->Implies(strip_vars a, strip_vars b)
    | Iff(a, b) -> Iff(strip_vars a, strip_vars b)
    | _ -> t

(** [reduce p]: reduce proposition [p] as far as possible.
*)
let get_value p = 
  match p with
    | Bool(b) -> Some(b)
    | _ -> None

let rec reduce p = 
  match p with
    | Bool _ -> p
    | Not b -> 
      let p1 = reduce b in 
      begin
        match get_value p1 with
	    Some(x) -> Bool(not x)
          | _ -> Not(p1)
      end
    | And(l, r) -> 
      let l1 = reduce l
      and r1 = reduce r
      in 
      let lv = get_value l1
      and rv = get_value r1
      in 
      begin
        match (lv, rv) with
	    (None, None) -> And (l1, r1)
          | (None, Some(x)) -> 
            if x then l1 else Bool(false)
          | (Some(x), None) ->
            if x then r1 else Bool(false)
          | (Some(x), Some(y)) -> 
            if x && y
            then Bool(true)
            else Bool(false)
      end
    | Or(l, r) -> 
      let l1 = reduce l
      and r1 = reduce r
      in 
      let lv = get_value l1
      and rv = get_value r1
      in 
      begin
        match (lv, rv) with
          | (None, None) -> Or(l1, r1)
          | (None, Some(x)) -> 
            if x then Bool(true) else l1
          | (Some(x), None) -> 
            if x then Bool(true) else r1
          | (Some(x), Some(y)) -> 
            if (x || y) 
            then Bool(true) 
            else Bool(false)
      end
    | Implies(l, r) -> reduce (mk_implies l r)
    | Iff(l, r) -> reduce (mk_iff l r)
    | Equals(l, r) -> reduce (mk_iff l r)
    | _ -> p
