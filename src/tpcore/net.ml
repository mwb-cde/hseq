(*----
  Name: net.ml
  Copyright M Wahab 2005-2009, 2010
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
   Term Nets

   Store data indexed by a term.  

   Lookup is by inexact matching of a given term against those
   indexing the data. Resulting list of terms-data pair would then be
   subject to more exact mactching (such as unification) to select
   required data.

   Used to cut the number of terms that need to be
   considered by (more expensive) exact matching.
*) 

(*
 * Labels
 *)

type label = 
  | Var 
  | App
  | Bound of Basic.quant
  | Quant of Basic.quant
  | Const of Basic.const_ty 
  | Cname of Hident.t
  | Cmeta of string
  | Cfree of string

(*** Operations ***)

(*
  [term_to_label varp t rst]:

  Return the label for term [t] together with the remainder
  of the term as a list of terms.

  [varp] determines which terms are treated as variables.

  [rst] is the list of terms built up by repeated calls
  to term_to_label. Initially, it should be [].

  Examples:
  
  ?y: ! x: (x or z) and y  (with variable z)
  -->
  [Qnt(?); Qnt(!); App; App; Bound(!); Var; Bound(?)]

  ?y: ! x: (x or z) and y  (with no variables,  z is free)
  -->
  [Qnt(?); Qnt(!); App; App; Bound(!); Cname(z); Bound(?)]
*)
let rec term_to_label varp trm rst =
  if varp trm
  then (Var, rst)
  else 
    match trm with
      | Basic.Id(id, _) -> (Cname(id), rst)
      | Basic.Meta(q) -> (Cmeta(Term.get_binder_name trm), rst)
      | Basic.Free(n, _) -> (Cfree(n), rst)
      | Basic.Qnt(q, b) -> (Quant(Basic.binder_kind q), b::rst)
      | Basic.Bound(q) -> 
	let (qnt, _, _) = Basic.dest_binding q
	in 
	(Bound(qnt), rst)
      | Basic.Const(c) -> (Const(c), rst)
      | Basic.App(l, r) -> (App, l::r::rst)

(*
 * Nets
 *)

(* 'a net : Node data, rest of net, Var tagged net (if any) *)
type 'a net =  
    Node of ('a list                  (* data held at this node *)
	     * (label * 'a net) list  
	     * ('a net) option)       (* net tagged by Var *)


(*** Operations ***)

let empty() = Node([], [], None)

let is_empty n = 
  match n with
    | Node ([], [], None) -> true
    | _ -> false

(*** Look-up ***)

(**
   [lookup varp n t]: lookup term [t] in net [n],
   returning list of possible replacements
*)
let rec get_from_list lbl netl =
  match netl with
    | [] -> raise Not_found
    | ((key, d)::xs) -> 
      if lbl = key
      then d
      else get_from_list lbl xs

let get_from_net lbl net =
  match (lbl, net) with
    | (Var, Node(_, _, None)) -> raise Not_found
    | (Var, Node(_, _, Some(vn))) -> vn
    | (_, Node(data, nl, _)) -> get_from_list lbl nl

(* lookup_list: Lookup using a list of terms to construct the path of
   labels to the required data.  List of data is returned in in
   reverse order that it is built.  Variables in the net are matched
   against terms after other labels are tried. This means that the
   terms best matching the indexing term are returned first.  t1 is a
   better match than t2 if variables in t1 occur deeper in the term
   structure than in t2.  terms are found first.  Returns the empty
   list if no data found
*)
let rec lookup_list varp net trms = 
  let rec lookup_aux nt tlist rslt =
    match (tlist, nt) with
      | ([], Node(data, _, _)) -> List.rev_append data rslt
      | (t::ts, Node(_, [], None)) -> rslt
      | (t::ts, Node(ds, ns, Some(vn))) ->
	(* look up in the labels first *)
	let slist = lookup_aux (Node(ds, ns, None)) (t::ts) rslt
	in 
	(* then lookup in the variable tagged net *)
	lookup_aux vn ts slist
      | (t::ts, Node(_, ns, _)) ->
	let label, nxt = term_to_label varp t ts in 
	let nnet = 
	  try get_from_net label nt
	  with Not_found -> empty()
	in 
	lookup_aux nnet nxt rslt
  in 
  List.rev (lookup_aux net trms [])

(* lookup net t:

   Return the list of items indexed by terms matching term t.
   Orderd with the best matches first. 

   Term t1 is a better match than term t2 if
   variables in t1 occur deeper in its term structure than
   those for t2.

   e.g. with variable x and t=(f 1 2), t1=(f x y) is a better match
   than t2=(x 1 2) because x occurs deeper in t1 than in t2. (t1 is
   likely to be rejected by exact matching more quickly than t2 would
   be.)

*)
let lookup net trm = lookup_list (fun x -> false) net [trm]

(*** Updating a net ***)

(* update f net trm:

   Apply function f to the subnet of net identified by trm to update
   the subnet. Propagate the changes through the net. 
   If applying function f results in an empty subnet, than remove
   these subnets.
*)

(* update_label lbl f net:

   apply function f to the subnet of net labeled lbl.

   If lbl=Var, it is apply to the variable net part of the Node tuple.
   If applying f to a labelled subnet results in an empty net
   then that label-net pair is removed from net.

   Usage: f should be function of the form
   f (Node data, lnet, vnet) = Node(g data, lnet vnet)
   (applying a function g to the data part of the net)
*)

let update_label lbl (f: 'a net -> 'a net) net =
  let rec app_aux nl =
    match nl with
      | [] ->  
        let nnet = f (empty())
	in 
	if is_empty nnet 
        then []
	else [(lbl, nnet)]
      | (l, n)::rst -> 
	if(l=lbl) 
	then 
	  let nnet = f n 
	  in 
	  if is_empty nnet 
	  then rst
	  else (l, nnet)::rst
	else 
	  (l, n)::(app_aux rst)
  in 
  match (lbl, net) with
    | (Var, Node(d, xs, Some (vn))) -> 
        let nnet = f vn 
        in 
        if is_empty nnet 
        then Node(d, xs, None)
        else Node(d, xs, Some nnet)
    | (Var, Node(d, xs, None)) ->
      let nnet = f (empty())
      in 
      if is_empty nnet 
      then Node(d, xs, None)
      else Node(d, xs, Some nnet)
    | (_, Node(d, xs, vn)) -> Node(d, app_aux xs, vn)

let update f varp net trm =
  let rec update_aux rst nt =
    match rst with
      | [] -> f nt
      | t::ts -> 
	let lbl, nxt = term_to_label varp t ts
	in 
	update_label lbl (update_aux nxt) nt
  in 
  update_aux [trm] net

(* 
   add varp net t r: Add term r, indexed by term t with variables
   identified by varp to net.  Replaces but doesn't remove previous
   bindings of t
*)
let add_to_list t r ls = r::ls
let add varp net t r=
  let add_aux net = 
    match net with 
      | Node(ds, ls, vn) -> 
	Node(add_to_list t r ds, ls, vn)
  in 
  update add_aux varp net t 

(*
  insert order varp net t r: Add data r, indexed b term t with
  variables identified by varp to net. Store in order given by
  predicate order. Replaces but doesn't remove previous bindings of t
*)
let insert_in_list order r ls = 
  let rec insert_aux ts =
    match ts with
      | [] -> [r]
      | x::tts -> 
	if (order x r)  (* x<r *)
	then x::(insert_aux tts)
	else r::ts
  in insert_aux ls

let insert order varp net t r =
  let order_aux net = 
    match net with 
      | Node(ds, ls, vn) -> 
	Node(insert_in_list order r ds, ls, vn)
  in 
  update order_aux varp net t 

(*
  delete varp net t test: Remove data indexed by t in net and
  satisfying test. Fails silently if t is not found. Needs the same
  varp as used to add the term to the net.
*)
let rec delete_from_list trm test ls =
  match ls with
    | [] -> []
    | (t::ts) -> 
      if (test t)
      then ts
      else t::(delete_from_list trm test ts)

let delete varp net trm test = 
  let delete_aux nt=
    match nt with
      | Node(ds, ls, vn) -> 
	Node(delete_from_list trm test ds, ls, vn)
  in 
  update delete_aux varp net trm

(* [iter f net]: apply [f] to each data item stored in a net. *)
let rec iter f (Node(ds, lnets, vnet)) =
  List.iter f ds;
  List.iter (fun (_, ln) -> iter f ln) lnets;
  match vnet with 
    | None -> ()
    | Some vn -> iter f vn
      
(* [print p net] Print the contents of [net] using printer [p]. *)
let print p net = iter p net
