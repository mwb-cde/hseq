(*
   typetree: replacement for type substitution, 
   based on trees rather than hashtable

   functions are intended to have the same semantics as those for Hashtbl

   Not a balanced tree, so may be slow.
*)


module Typetree=
struct

open Basic
open Lib
open Corepp
open Result
open Gtypes

type tree_node = gtype * (gtype list)

type tree = 
    Empty | Node of (tree_node list) * tree * tree

let rec add_rightmost tree t=
  match tree with 
    Empty -> t
  | Node(x, l, r) -> Node(x, l, add_rightmost r t)

let rec remove_node ns ty=
  match ns with
    [] -> raise Not_found
  | (g, rs)::rst -> 
      if(equality g ty) 
      then 
	match rs with
	  [] -> rst
	| x::[] -> rst
	| x::xs -> (g, xs)::rst
      else (g, rs)::(remove_node rst ty)

let rec remove tree ty=
  match tree with 
    Empty -> Empty
  | Node([], l, r) -> 
      remove (add_rightmost l r) ty
  | Node((t, xs)::ts, l, r) ->  
      if(ty=t)
      then 
	match (remove_node ((t, xs)::ts) ty) with
	  [] -> add_rightmost l r
	| ys -> Node(ys, l, r)
      else 
	if(ty<t)
	then Node((t, xs)::ts, remove l ty, r)
	else Node((t, xs)::ts, l, remove r ty)


let rec find_node ns ty =
  match ns with
    [] -> raise Not_found
  | (g, rs)::rst -> 
      if(equality g ty) 
      then 
	(try List.hd rs
	with _ -> raise Not_found)
      else find_node rst ty

let rec find tree ty=
  match tree with 
    Empty -> raise Not_found
  | Node([], l, r) -> 
      (try find l ty
      with Not_found -> find r ty)
  | Node((t, xs)::ts, l, r) ->  
      if(ty=t)
      then find_node ((t, xs)::ts) ty
      else 
	if(ty<t)
	then find l ty
	else find r ty
	    
let rec add_node ns ty nr=
  match ns with
    [] -> [(ty, [nr])]
  | (g, rs)::rst -> 
      if(equality g ty) 
      then (g, nr::rs)::rst
      else (g, rs)::(add_node rst ty nr)

let rec add tree ty nr=
  match tree with 
    Empty -> Node(add_node [] ty nr, Empty, Empty)
  | Node([], l, r) -> 
      add (add_rightmost l r) ty nr
  | Node((t, xs)::ts, l, r) ->  
      if(ty=t)
      then Node(add_node ((t, xs)::ts) ty nr, l, r)
      else 
	if(ty<t)
	then Node((t, xs)::ts, add l ty nr, r)
	else Node((t, xs)::ts, l, add r ty nr)


(* replace: add, but overwrite any existing binding *)
let rec replace_node ns ty nr=
  match ns with
    [] -> [(ty, [nr])]
  | (g, rs)::rst -> 
      if(equality g ty) 
      then 
	match rs with 
	  [] -> (g, [nr])::rst
	| x::xs -> (g, nr::xs)::rst
      else (g, rs)::(replace_node rst ty nr)

let rec replace tree ty nr=
  match tree with 
    Empty -> Node(replace_node [] ty nr, Empty, Empty)
  | Node([], l, r) -> 
      replace (add_rightmost l r) ty nr
  | Node((t, xs)::ts, l, r) ->  
      if(ty=t)
      then Node(replace_node ((t, xs)::ts) ty nr, l, r)
      else 
	if(ty<t)
	then Node((t, xs)::ts, replace l ty nr, r)
	else Node((t, xs)::ts, l, replace r ty nr)
	    
let rec iter_node f ns =
  match ns with 
    [] -> ()
  | (_, [])::rst -> 
      iter_node f rst
  | (g, r::rs)::rst -> 
      (f g r; iter_node f rst)
      
let rec iter f tree=
  match tree with
    Empty -> ()
  | Node(ns, l, r) -> 
      (iter_node f ns;
       iter f l;
       iter f r)


end;;


(* balanced type tree *)

module Btypetree=
struct

open Basic
open Lib
open Corepp
open Result
open Gtypes

type tree_node = gtype * (gtype list)

type tree = 
    Empty | Node of (tree_node list) * tree * tree * int

let height t= 
  match t with
    Empty -> 0
  | Node(_, _, _, h) -> h


let mk_node ns l r =
  let lh = height l
  and rh = height r
  in 
  let nh = if (lh<rh) then rh+1 else lh+1
  in 
  Node(ns, l, r, nh)

let rec add_rightmost tree t=
  match tree with 
    Empty -> t
  | Node(x, l, r, h) -> 
      Node(x, l, add_rightmost r t, height t)

let rec remove_node ns ty=
  match ns with
    [] -> raise Not_found
  | (g, rs)::rst -> 
      if(equality g ty) 
      then 
	match rs with
	  [] -> rst
	| x::[] -> rst
	| x::xs -> (g, xs)::rst
      else (g, rs)::(remove_node rst ty)

let rec remove tree ty=
  match tree with 
    Empty -> Empty
  | Node([], l, r, h) -> 
      remove (add_rightmost l r) ty
  | Node((t, xs)::ts, l, r, h) ->  
      if(ty=t)
      then 
	match (remove_node ((t, xs)::ts) ty) with
	  [] -> add_rightmost l r
	| ys -> Node(ys, l, r, h)
      else 
	if(ty<t)
	then Node((t, xs)::ts, remove l ty, r, h)
	else Node((t, xs)::ts, l, remove r ty, h)


let rec find_node ns ty =
  match ns with
    [] -> raise Not_found
  | (g, rs)::rst -> 
      if(equality g ty) 
      then 
	(try List.hd rs
	with _ -> raise Not_found)
      else find_node rst ty

let rec find tree ty=
  match tree with 
    Empty -> raise Not_found
  | Node([], l, r) -> 
      (try find l ty
      with Not_found -> find r ty)
  | Node((t, xs)::ts, l, r) ->  
      if(ty=t)
      then find_node ((t, xs)::ts) ty
      else 
	if(ty<t)
	then find l ty
	else find r ty
	    
let rec add_node ns ty nr=
  match ns with
    [] -> [(ty, [nr])]
  | (g, rs)::rst -> 
      if(equality g ty) 
      then (g, nr::rs)::rst
      else (g, rs)::(add_node rst ty nr)

let rec add tree ty nr=
  match tree with 
    Empty -> Node(add_node [] ty nr, Empty, Empty)
  | Node([], l, r) -> 
      add (add_rightmost l r) ty nr
  | Node((t, xs)::ts, l, r) ->  
      if(ty=t)
      then Node(add_node ((t, xs)::ts) ty nr, l, r)
      else 
	if(ty<t)
	then Node((t, xs)::ts, add l ty nr, r)
	else Node((t, xs)::ts, l, add r ty nr)


(* replace: add, but overwrite any existing binding *)
let rec replace_node ns ty nr=
  match ns with
    [] -> [(ty, [nr])]
  | (g, rs)::rst -> 
      if(equality g ty) 
      then 
	match rs with 
	  [] -> (g, [nr])::rst
	| x::xs -> (g, nr::xs)::rst
      else (g, rs)::(replace_node rst ty nr)

let rec replace tree ty nr=
  match tree with 
    Empty -> Node(replace_node [] ty nr, Empty, Empty)
  | Node([], l, r) -> 
      replace (add_rightmost l r) ty nr
  | Node((t, xs)::ts, l, r) ->  
      if(ty=t)
      then Node(replace_node ((t, xs)::ts) ty nr, l, r)
      else 
	if(ty<t)
	then Node((t, xs)::ts, replace l ty nr, r)
	else Node((t, xs)::ts, l, replace r ty nr)
	    
let rec iter_node f ns =
  match ns with 
    [] -> ()
  | (_, [])::rst -> 
      iter_node f rst
  | (g, r::rs)::rst -> 
      (f g r; iter_node f rst)
      
let rec iter f tree=
  match tree with
    Empty -> ()
  | Node(ns, l, r) -> 
      (iter_node f ns;
       iter f l;
       iter f r)


end;;

open Gtypes;;
open Typetree;;

let a = mk_var "a";;
let a1 = mk_var "a";;
let a2 = mk_var "a";;

let b= mk_var "b";;
let c= mk_var "c";;

let f1=mk_fun a b;;
let f2=mk_fun f1 b;;
let f3=mk_fun c f2;;

let t=add Empty a b;;
let t1=add t a c;;
let t2=add t1 a1 f1;;
let t3=add t2 b c;;
let t4=add t3 b f2;;
let t5=add t4 f1 a;;
let t6=add t5 f1 c;;
let t7=add t6 f2 a;;
