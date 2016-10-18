(*----
  Name: treekit.ml
  Copyright Matthew Wahab 2005-2016
  Author: Matthew Wahab <mwb.cde@gmail.com>

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

(* Lookup trees. *)

module type TreeData =
sig
  (* Type of keys *)
  type key

  (**
      Comparisons between keys

      [equals x y]: True if [x] is the same as [y]. Must be accurate
      since this is used to find the data associated with a key.

      [lessthan x y]: True if [x] is considered less-than [y]. Does not
      need to be accurate since it is only used to narrow the list of
      possible matches for a key.
  *)
  val equals : key -> key -> bool
  val lessthan : key -> key -> bool
end


module type TreeType=
sig

  type key

  val eql : key -> key -> bool
  val lessthan : key -> key -> bool

  type ('a)t=
    Nil
  | Branch of ( (key * 'a) list * ('a)t * ('a)t)

  (* data tr: get the data at the current branch *)
  val data : 'a t -> (key * 'a) list

  (* left tr: get left branch of tree *)
  val left : 'a t -> 'a t

  (* right tr: get right branch of tree *)
  val right : 'a t -> 'a t

  (* nil: make an empty tree *)
  val nil : 'a t

  val empty: unit -> 'a t
  (* Make an empty tree *)

  (* create: make a branch with data *)
  val create : (key * 'a) list -> 'a t -> 'a t -> 'a t

  (*
    add tr k d:
    add binding of d to k in tree tr
    previous bindings to k are hidden
    but not removed
  *)
  val add : 'a t -> key -> 'a -> 'a t

  (*
    replace tr k d:
    replace binding of k with d in tree tr
    previous bindings to k are hidden
    but not removed
    add binding if necessary
  *)
  val replace : 'a t -> key -> 'a -> 'a t

  (*
     find tree key
     finds the current binding of key in tree
  *)

  val find : 'a t -> key -> 'a

  (*
     find_all tree key
     finds all bindings of key in tree
     with last binding first in list
     raise Not_found if no bindings in tree
  *)
  val find_all : 'a t -> key -> 'a list

  (* mem tree key:
     return true if key is bound in tree
     false otherwise
  *)

  val mem : 'a t -> key -> bool

  (*
    remove tree key

    removes the data currently bound to key in tree
    does nothing if key is not in tree
  *)
  (*      val remove : 'a t -> key -> 'a t *)

  (*
    delete tree key

    removes the data currently bound to key in tree
    does nothing if key is not in tree
  *)
  val delete : 'a t -> key -> 'a t
  val remove : 'a t -> key -> 'a t

  (*
     iter tree fn:

     apply fn to the data bound to each key
     only the current key bindings are used.
  *)

  val iter : (key -> 'a -> 'b) -> 'a t -> unit

  val fold: (key -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold tree fn]: Apply [fn] to the data bound to each key.  Only
      the current key bindings are used.  *)

  (* to_list tree:
     return a list of the (lists of) elements in the
     tree in descending order
  *)
  val to_list: 'a t -> (key * 'a) list list

end



module Tree = functor (A: TreeData)->
struct

  type key = A.key
  (*
    let eql x y =  (Pervasives.compare x y) = 0
    let lessthan x y= (Pervasives.compare x y) < 0
  *)
  let eql = A.equals
  let lessthan = A.lessthan

  type ('a)t=
    Nil
  | Branch of ( (key * 'a) list * ('a)t * ('a)t)


  let nil = Nil
  let create x l r = Branch(x, l, r)

  let empty () = nil
  (** Make an empty tree *)

  (*
    tree information/manipulation
  *)

  let data tr =
    match tr with
      Nil -> (failwith "Tree.data: invalid argument")
    | Branch(x, _, _) -> x

  let left tr =
    match tr with
      Nil -> (failwith "Tree.left: invalid argument")
    | Branch(_, l, _) -> l

  let right tr =
    match tr with
      Nil -> (failwith "Tree.right: invalid argument")
    | Branch(_, _, r) -> r

  (*
    add tr k d:
    add binding of d to k in tree tr
    previous bindings to k are hidden
    but not removed
  *)

  let list_add k d ys = (k, d)::ys

  let add tree key data=
    let rec add_aux tr=
      match tr with
        Nil -> Branch([(key, data)], Nil, Nil)
      | Branch(((y, z)::ys), l, r) ->
        if(eql key y)
        then Branch(list_add key data ((y, z)::ys), l, r)
        else
          if(lessthan key y)
          then Branch(((y, z)::ys), (add_aux l), r)
          else Branch(((y, z)::ys), l, (add_aux r))
      | _ -> failwith "Tree.add"
    in
    add_aux tree

  (*
    replace tr k d:
    replace binding of k with d in tree tr
    previous bindings to k are hidden
    but not removed
    add binding if necessary
  *)

  let list_replace key data lst =
    let rec replace_aux xs =
      match xs with
        [] -> [(key, data)]
      | ((k, d)::ys) ->
        if(A.equals key k)
        then (k, data)::ys
        else (k, d)::(replace_aux ys)
    in
    replace_aux lst

  let replace tree key data=
    let rec replace_aux tr=
      match tr with
        Nil ->
          Branch([(key, data)], Nil, Nil)
      | Branch(((y, z)::ys), l, r) ->
        if(eql key y)
        then Branch(list_replace key data ((y, z)::ys), l, r)
        else
          if(lessthan key y)
          then Branch(((y, z)::ys), (replace_aux l), r)
          else Branch(((y, z)::ys), l, (replace_aux r))
      | _ -> failwith "Tree.replace"
    in
    replace_aux tree

  (*
     find tree key
     finds the current binding of key in tree
  *)

  let list_find key ys =
    let rec find_aux xs =
      match xs with
        [] -> raise Not_found
      | ((x, d)::ds) ->
        if(A.equals key x)
        then d
        else find_aux ds
    in
    find_aux ys

  let find tree key =
    let rec find_aux tr =
      match tr with
        Nil -> raise Not_found
      | Branch((y, z)::ys, l, r) ->
        if(eql key y)
        then list_find key ((y, z)::ys)
        else
          if(lessthan key y)
          then (find_aux l )
          else (find_aux r)
      | _ -> failwith "Tree.find"
    in
    find_aux tree


  let list_find_all key ys =
    let rec find_aux xs fnd=
      match xs with
        [] ->
          (match fnd with
            [] -> raise Not_found
          | _ -> List.rev fnd)
      | ((x, d)::ds) ->
        if(A.equals key x)
        then find_aux ds (d::fnd)
        else find_aux ds fnd
    in
    find_aux ys []

  let find_all tree key =
    let rec find_aux tr =
      match tr with
        Nil -> raise Not_found
      | Branch((y, z)::ys, l, r) ->
        if(eql key y)
        then list_find_all key ((y, z)::ys)
        else
          if(lessthan key y)
          then (find_aux l )
          else (find_aux r)
      | _ -> failwith "Tree.find_all"
    in
    find_aux tree


  (* mem tree key:
     return true if key is bound in tree
     false otherwise
  *)
  let mem tree key =
    try (ignore(find tree key); true)
    with
      Not_found -> false

  (* delete tree key

     removes the data currently bound to key in tree
     does nothing if key is not in tree
  *)

  let list_delete key ys =
    let rec delete_aux xs=
      match xs with
        [] -> []
      | (k, d):: ds ->
        if(A.equals key k)
        then ds
        else (k, d)::(delete_aux ds)
    in delete_aux ys

  (* split tr:
     remove greatest data d' from tree, giving tree tr'
     returning (d', tr')
     if node n storing d' has a left branch b, node n is replaced
     with b.
     node n will never have a right branch since this would mean
     that there is an element greater than d'.
  *)

  let rec split tr=
    match tr with
      Nil -> failwith "Tree.split"
    | Branch(data, tr1, Nil) -> (data, tr1)
    | Branch(data, tr1, tr2) ->
      let rdata, rtr=split tr2
      in
      (rdata, Branch(data, tr1, rtr))

  (* join tr1 tr2:
     form a new tree Branch(d, ntr, tr2)
     where
     d is the largest element in tr1
     and ntr is the tree formed by extracting d from tr1
     (using split)
  *)

  let join tr1 tr2 =
    match tr1 with
      Nil -> tr2
    | _ ->
      let data, ntr=split tr1
      in
      Branch(data, ntr, tr2)

  let rec delete tr key =
    match tr with
      Nil -> tr
    | Branch((k, y)::data, l, r) ->
      if (eql key k)
      then
        (match (list_delete key ((k, y)::data)) with
          [] -> join l r
        | nlst -> Branch(nlst, l, r))
      else
        if(lessthan key k)
        then Branch((k, y)::data, delete l key, r)
        else Branch((k, y)::data, l, delete r key)
    | Branch([], _, _) -> failwith ("Tree.delete")
  let remove = delete

  (*
     iter tree fn:

     apply fn to the data bound to each key
     only the current key bindings are used.
  *)

  let list_iter fn lst=
    let key_mem k ls =
      try(ignore(List.find (fun x -> A.equals k x) ls);
          true)
      with  Not_found -> false
    in
    let rec iter_aux xs seen=
      match xs with
        [] -> ()
      | (k, d)::ys ->
        if(key_mem k seen)   (* has key already been seen *)
        then
          iter_aux ys seen  (* if yes, ignore it *)
        else
          (fn k d;          (* if no, apply function *)
           iter_aux ys (k::seen))
    in
    iter_aux lst []

  let iter fn tree=
    let rec iter_aux tr todo =
      match tr with
      | Nil ->
        (match todo with
        | [] ->()
        | (t::ts) -> iter_aux t ts)
      | Branch(data, Nil, r) ->
        list_iter fn data;
        iter_aux r todo
      | Branch(data, l, r) ->
        iter_aux l (Branch(data, Nil, r)::todo)
    in
    iter_aux tree []

  let list_fold fn init lst=
    let key_mem k ls =
      try ignore(List.find (fun x -> A.equals k x) ls); true
      with Not_found -> false
    in
    let rec lfold_aux total xs seen =
      match xs with
      | [] -> total
      | (k, d)::ys ->
        if key_mem k seen
        then lfold_aux total ys seen
        else
          let new_total = fn k total d
          in
          lfold_aux new_total ys (k::seen)
    in
    lfold_aux init lst []

  let fold fn init tree =
    let rec fold_aux total tr todo =
      match tr with
      | Nil ->
        (match todo with
        | [] -> total
        | (t::ts) -> fold_aux total t ts)
      | Branch(data, Nil, r) ->
        let branch_total = list_fold fn total data
        in
        fold_aux branch_total r todo
      | Branch(data, l, r) ->
        fold_aux total l (Branch(data, Nil, r)::todo)
    in
    let result =  fold_aux init tree []
    in
    result

  (* to_list tree:
     return a list of the (lists of) elements in the
     tree in descending order
  *)

  let to_list tree=
    let rec to_list_aux tr rslt=
      match tr with
        Nil -> rslt
      | Branch(data, l, r)
        -> to_list_aux r (data::(to_list_aux l rslt))
    in
    to_list_aux tree []

end


(* Balanced lookup trees *)

module type BTreeType=
sig
  type key
  val eql : key -> key -> bool
  val lessthan : key -> key -> bool

  type depth_t = int
  type ('a)t=
    Nil
  | Branch of ((key * 'a) list * ('a)t * ('a)t * depth_t)


  (* nil: make an empty tree *)
  val nil : 'a t

  (* make an empty tree *)
  val empty : unit -> 'a t

  (* create: make a branch with data *)
  val create : (key * 'a) list -> 'a t -> 'a t -> 'a t


  (*
    tree information/manipulation
  *)

  (* data tr: get the data at the current branch *)

  val data : 'a t -> (key * 'a) list

  (* left tr: get left branch of tree *)
  val left : 'a t -> 'a t

  (* right tr: get right branch of tree *)
  val right : 'a t -> 'a t

  (* depth: get depth of tree *)
  val depth : 'a t -> depth_t

  (* balance tr: balance tree tr *)
  val balance : 'a t -> 'a t

  (*
    add tr k d:
    add binding of d to k in tree tr
    previous bindings to k are hidden
    but not removed
  *)
  val add : 'a t -> key -> 'a -> 'a t


  (*
    replace tr k d:
    replace binding of k with d in tree tr
    previous bindings to k are hidden
    but not removed
    add binding if necessary
  *)
  val replace : 'a t -> key -> 'a -> 'a t

  val delete : 'a t -> key -> 'a t
  val remove : 'a t -> key -> 'a t

  (*
     find tree key
     finds the current binding of key in tree
  *)
  val find : 'a t -> key -> 'a

  (*
     find_all tree key
     finds all bindings of key in tree
     with last binding first in list
     raise Not_found if no bindings in tree
  *)
  val find_all : 'a t -> key -> 'a list

  (* mem tree key:
     return true if key is bound in tree
     false otherwise
  *)
  val mem : 'a t -> key -> bool

  (*
     iter tree fn:

     apply fn to the data bound to each key
     only the current key bindings are used.
  *)

  val iter : (key -> 'a -> 'b) -> 'a t -> unit

  val fold: (key -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold tree fn]: Apply [fn] to the data bound to each key.  Only
      the current key bindings are used.  *)

  (* to_list tree:
     return a list of the (lists of) elements in the
     tree in descending order
  *)
  val to_list: 'a t -> (key * 'a) list list
end


module BTree=
  functor (A: TreeData)->
struct

  type key = A.key
  let eql =  A.equals
  let lessthan = A.lessthan

  type depth_t = int

  type ('a)t=
    Nil
  | Branch of ((key * 'a) list * ('a)t * ('a)t
               * depth_t)

  let nil = Nil
  let create x l r = Branch(x, l, r, 1)
  let empty () = nil

  (*
    tree information/manipulation
  *)

  let data tr =
    match tr with
      Nil -> (failwith "BTree.data: invalid argument")
    | Branch(x, _, _, _) -> x

  let left tr =
    match tr with
      Nil -> (failwith "BTree.left: invalid argument")
    | Branch(_, l, _, _) -> l

  let right tr =
    match tr with
      Nil -> (failwith "BTree.right: invalid argument")
    | Branch(_, _, r, _) -> r

  let depth t =
    match t with
      Nil -> 0
    | Branch(_, _, _, d) -> d

  let inc_depth t i =
    match t with
      Nil -> (failwith "BTree.dec_depth: invalid argument")
    | Branch(x, l, r, d) -> Branch(x, l, r, d+i)

  let dec_depth t i =
    match t with
      Nil -> (failwith "BTree.dec_depth: invalid argument")
    | Branch(x, l, r, d) -> Branch(x, l, r, d-i)


  let max_depth t1 t2=max (depth t1) (depth t2)

  (*
    tree rotation
  *)

  (*
    rotl: rotate left


    rotl Branch(x, l, Branch(y, a, b, e) , d)
    ->
    Branch(y, nl, nr, nd)
    where
    nl=Branch(x, l, a, max(depth l, depth a)+1)
    nr=b
    nd=(max(depth nl) (depth nr))+1
  *)

  let rotl tr =
    match tr with
      Branch(x, l, Branch(y, a, b, e), d) ->
        let nl=Branch(x, l, a, (max (depth l) (depth a))+1)
        in
        let nd=(max (depth nl) (depth b))+1
        in
        Branch(y, nl, b, nd)
    | _ -> tr


  (*
    rotr: rotate right

    rotr Branch(x, Branch(y, a, b, e), r, d)
    ->
    Branch(y, nl, nr, nd)
    where
    nl=a
    nr=Branch(x, b, r, (max(depth b) (depth nr))+1)
    nd=(max(depth nl) (depth nr))+1
  *)

  let rotr tr =
    match tr with
      Branch(x, Branch(y, a, b, e), r, d) ->
        let nr=Branch(x, b, r, (max(depth b) (depth r))+1)
        in
        let nd=(max(depth a) (depth nr))+1
        in
        Branch(y, a, nr, nd)
    | _ -> tr

  (*
     shiftl: shift left

     rotate tree left,
     making sure that the branch rotated into the left hand side
     is the larger of the two branches of the right hand side

     For t=Branch(x, l, r, d)

     if (depth (left r)) >= (depth (right r))
     then rotl t
     else rotl(Branch(x, l, rotr r, d))
  *)

  let shiftl t=
    match t with
      Branch(_, _, Nil, _) -> t
    | Branch(x, l, r, d) ->
      if(depth (left r))>=(depth (right r))
      then rotl t
      else rotl(Branch(x, l, rotr r, d))
    | _ -> t


  let rec shiftln t n=
    match n with
      0 -> t
    | i -> shiftln (shiftl t) (i-1)

  (*
     shiftr: shift right

     rotate tree right
     making sure that the branch rotated into the right hand side
     is the larger of the two branches of the left hand side

     For t=Branch(x, l, r, d)

     if (depth (right l)) >= (depth (left l))
     then rotr t
     else rotr(Branch(x, rotl l, r, d))
  *)

  let shiftr t=
    match t with
      Branch(_, Nil, _, _) -> t
    | Branch(x, l, r, d) ->
      if(depth (right l))>=(depth (left l))
      then rotr t
      else rotr(Branch(x, rotl l, r, d))
    | _ -> t


  let rec shiftrn t n=
    match n with
      0 -> t
    | i -> shiftrn (shiftr t) (i-1)

  (*
     tree balance
  *)

  let is_balanced t =
    match t with
      Nil -> true
    | Branch(_, l, r, d) ->
      let leftd=depth l
      and rightd=depth r
      in
      if (abs(leftd-rightd)<=1)
      then true
      else false

  let balance t=
    match t with
      Nil -> t
    | Branch(_, l, r, _) ->
      let leftd=depth l
      and rightd=depth r
      in
      let sl = leftd-rightd
      in
      let absl=abs sl
      in
      if (absl<=1) then t
      else
        if(sl<=0)
        then (* rhs is deeper *)
          shiftln t (absl-1)
        else (* lhs is deeper *)
          shiftrn t (absl-1)

  (*
    add tr k d:
    add binding of d to k in tree tr
    previous bindings to k are hidden
    but not removed
  *)

  let list_add k d ys = (k, d)::ys

  let add tree key data=
    let rec add_aux tr=
      match tr with
        Nil ->
          Branch([(key, data)], Nil, Nil, 1)
      | Branch(((y, z)::ys), l, r, d) ->
        if(eql key y)
        then
            (* add data to existing list, no rebalancing needed *)
          Branch(list_add key data ((y, z)::ys), l, r, d)
        else
          if(lessthan key y)
          then
              (* add data to lhs tree, rebalancing may be needed *)
            let nl=(add_aux l)
            in
            balance(Branch(((y, z)::ys), nl, r, (depth nl)+1))
          else
              (* add data to rhs tree, rebalancing may be needed *)
            let nr=add_aux r
            in
            balance(Branch(((y, z)::ys), l, nr, (depth nr)+1))
      | _ -> failwith "Tree.add"
    in
    (* add data into tree *)
    add_aux tree

  (*
    replace tr k d:
    replace binding of k with d in tree tr
    previous bindings to k are hidden
    but not removed
    add binding if necessary
  *)

  let list_replace key data lst =
    let rec replace_aux xs =
      match xs with
        [] -> [(key, data)]
      | ((k, d)::ys) ->
        if(A.equals key k)
        then (k, data)::ys
        else (k, d)::(replace_aux ys)
    in
    replace_aux lst

  let replace tree key data=
    let rec replace_aux tr=
      match tr with
        Nil ->
          Branch([(key, data)], Nil, Nil, 1)
      | Branch(((y, z)::ys), l, r, d) ->
        if(eql key y)
        then
            (* replace data in existing list, no rebalancing needed *)
          Branch(list_replace key data ((y, z)::ys), l, r, d)
        else
          if(lessthan key y)
          then
              (* replace data in lhs tree, rebalancing may be needed *)
            balance(Branch(((y, z)::ys), (replace_aux l), r, d))
          else
              (* replace data in rhs tree, rebalancing may be needed *)
            balance(Branch(((y, z)::ys), l, (replace_aux r), d))
      | _ -> failwith "Tree.replace"
    in
    (* replace data in tree *)
    replace_aux tree

  (* delete tree key

     removes the data currently bound to key in tree
     does nothing if key is not in tree
  *)

  let list_delete key ys =
    let rec remove_aux xs=
      match xs with
        [] -> []
      | (k, d):: ds ->
        if(A.equals key k)
        then ds
        else (k, d)::(remove_aux ds)
    in remove_aux ys


  (* split tr:
     remove greatest data d' from tree, giving tree tr'
     returning (d', tr')
     if node n storing d' has a left branch b, node n is replaced
     with b.
     node n will never have a right branch since this would mean
     that there is an element greater than d'.
  *)

  let rec split tr=
    match tr with
      Nil -> failwith "Tree.split"
    | Branch(data, tr1, Nil, _) ->
      (data, tr1)
    | Branch(data, tr1, tr2, _) ->
      let ndata, nright=split tr2
      and nleft=tr1
      in
      let ntree=Branch(data, nleft, nright, (max_depth nleft nright)+1)
      in
      (ndata, balance ntree)

  (* join tr1 tr2:
     form a new tree Branch(d, ntr, tr2)
     where
     d is the largest element in tr1
     and ntr is the tree formed by extracting d from tr1
     (using split)
  *)

  let join tr1 tr2 =
    match tr1 with
      Nil -> tr2
    | _ ->
      let data, nleft=split tr1
      and nright=tr2
      in
      let ntree=Branch(data, nleft, nright,
                       (max_depth nleft nright)+1)
      in
      balance ntree

  let delete tree key =
    let rec delete_aux tr =
      match tr with
        Nil -> Nil
      | Branch((k, y)::data, l, r, dp) ->
        let ntree=
          if (eql key k)
          then
            (match (list_delete key ((k, y)::data)) with
              [] -> join l r
            | nlst -> Branch(nlst, l, r, dp))
          else
            (let nleft, nright=
               if(lessthan key k)
               then (delete_aux l, r)
               else (l, delete_aux r)
             in
             Branch((k, y)::data, nleft, nright,
                    (max_depth nleft nright)+1))
        in
        balance ntree
      | Branch([], _, _, _) -> failwith ("Tree.delete")
    in
    delete_aux tree
  let remove = delete

  (*
     find tree key
     finds the current binding of key in tree
  *)

  let list_find key ys =
    let rec find_aux xs =
      match xs with
        [] -> raise Not_found
      | ((x, d)::ds) ->
        if(A.equals key x)
        then d
        else find_aux ds
    in
    find_aux ys

  let find tree key =
    let rec find_aux tr =
      match tr with
        Nil -> raise Not_found
      | Branch((y, z)::ys, l, r, _) ->
        if(eql key y)
        then list_find key ((y, z)::ys)
        else
          if(lessthan key y)
          then (find_aux l )
          else (find_aux r)
      | _ -> failwith "Tree.find"
    in
    find_aux tree


  (*
     find_all tree key
     finds all bindings of key in tree
     with last binding first in list
     raise Not_found if no bindings in tree
  *)

  let list_find_all key ys =
    let rec find_aux xs fnd=
      match xs with
        [] ->
          (match fnd with
            [] -> raise Not_found
          | _ -> List.rev fnd)
      | ((x, d)::ds) ->
        if(A.equals key x)
        then find_aux ds (d::fnd)
        else find_aux ds fnd
    in
    find_aux ys []

  let find_all tree key =
    let rec find_aux tr =
      match tr with
        Nil -> raise Not_found
      | Branch((y, z)::ys, l, r, _) ->
        if(eql key y)
        then list_find_all key ((y, z)::ys)
        else
          if(lessthan key y)
          then (find_aux l )
          else (find_aux r)
      | _ -> failwith "Tree.find_all"
    in
    find_aux tree


  (* mem tree key:
     return true if key is bound in tree
     false otherwise
  *)
  let mem tree key =
    try (ignore(find tree key); true)
    with
      Not_found -> false

  (*
     iter tree fn:

     apply fn to the data bound to each key
     only the current key bindings are used.
  *)

  let list_iter fn lst=
    let key_mem k ls =
      try(ignore(List.find (fun x -> A.equals k x) ls);
          true)
      with  Not_found -> false
    in
    let rec iter_aux xs seen=
      match xs with
        [] -> ()
      | (k, d)::ys ->
        if(key_mem k seen)   (* has key already been seen *)
        then
          iter_aux ys seen  (* if yes, ignore it *)
        else
          (fn k d;          (* if no, apply function *)
           iter_aux ys (k::seen))
    in
    iter_aux lst []

  let iter fn tree=
    let rec iter_aux tr todo =
      match tr with
        Nil ->
          (match todo with
            [] ->()
          | (t::ts) -> iter_aux t ts)
      | Branch(data, Nil, r, _) ->
        list_iter fn data;
        iter_aux r todo
      | Branch(data, l, r, d) ->
        iter_aux l (Branch(data, Nil, r, d)::todo)
    in
    iter_aux tree []

  let list_fold fn init lst=
    let key_mem k ls =
      try ignore(List.find (fun x -> A.equals k x) ls); true
      with Not_found -> false
    in
    let rec lfold_aux total xs seen =
      match xs with
      | [] -> total
      | (k, d)::ys ->
        if key_mem k seen
        then lfold_aux total ys seen
        else
          let new_total = fn k total d
          in
          lfold_aux new_total ys (k::seen)
    in
    lfold_aux init lst []

  let fold fn init tree =
    let rec fold_aux total tr todo =
      match tr with
      | Nil ->
        (match todo with
        | [] -> total
        | (t::ts) -> fold_aux total t ts)
      | Branch(data, Nil, r, _) ->
        let branch_total = list_fold fn total data
        in
        fold_aux branch_total r todo
      | Branch(data, l, r, d) ->
        fold_aux total l (Branch(data, Nil, r, d)::todo)
    in
    let result =  fold_aux init tree []
    in
    result

  (* to_list tree:
     return a list of the (lists of) elements in the
     tree in descending order
  *)

  let to_list tree=
    let rec to_list_aux tr rslt=
      match tr with
        Nil -> rslt
      | Branch(data, l, r, _)
        -> to_list_aux r (data::(to_list_aux l rslt))
    in
    to_list_aux tree []

end

(* tests *)

(*
  module IntTreeData=
  struct
  type key = string
  let equals x y = (x=y)
  end;;

  module IntTree=BTree(IntTreeData);;


  open IntTree;;

  let t=ref Nil;;

  t:=add (!t) "g" 10;;
  t:=add (!t) "k" 5;;
  t:=add (!t) "i" 8;;
  t:=add (!t) "l" 12;;
  t:=add (!t) "d" 6;;
  t:=add (!t) "c" 5;;
  t:=add (!t) "e" 1;;
  t:=add (!t) "f" 4;;
  t:=add (!t) "n" 5;;
  t:=add (!t) "a" 9;;

  let iterfn k d =
  Format.open_box 1;
  Format.print_string "(";
  Format.print_string k;
  Format.print_string ",";
  Format.print_int d;
  Format.print_string ")";
  Format.close_box();
  Format.print_newline ();;


  let addfn sum k d =
  sum:=(!sum)+d;;

  let t1=delete (!t) "i" ;;
  let t2=delete t1 "g";;
  let t3=delete (delete t1 "e") "a";;
  let t4=right t3;;
*)

(***
    * Simple trees, indexed by Pervasives.compare.
***)

module type Data =
sig
  type key
end

module type SimpleTreeType =
sig

  type key

  val eql : key -> key -> bool
  val lessthan : key -> key -> bool

  type depth_t = int
  type ('a)t=
    Nil
  | Branch of ((key * 'a) list * ('a)t * ('a)t * depth_t)


  val nil : 'a t
  (** The empty tree *)

  val empty: unit -> 'a t
  (** Make an empty tree *)

  val create : (key * 'a) list -> 'a t -> 'a t -> 'a t
  (** Make a branch with data *)

  val data : 'a t -> (key * 'a) list
  (** Get the data at the current branch *)

  val left : 'a t -> 'a t
  (** Get the left branch of tree *)

  val right : 'a t -> 'a t
  (** Get the right branch of tree *)

  val depth : 'a t -> depth_t
  (** Get the depth (number of levels) of the tree *)

  val balance : 'a t -> 'a t
  (** Balance the tree. *)

  val add : 'a t -> key -> 'a -> 'a t
  (**
     [add tr k d]: Add binding of [d] to [k] in tree [tr].  Previous
     bindings to [k] are hidden but not removed.
  *)

  val replace : 'a t -> key -> 'a -> 'a t
  (**
     [replace tr k d]: Replace binding of [k] with [d] in tree [tr].
     Adds the binding even if [k] is not already bound in [tr].
  *)

  val delete : 'a t -> key -> 'a t
  (**
     [delete tree key]: Remove the data currently bound to [key] in
     [tree].  Does nothing if key is not in tree
  *)
  val remove : 'a t -> key -> 'a t

  val find : 'a t -> key -> 'a
  (**
     [find_all tree key]: Finds all bindings of [key] in [tree]
     with last binding first in list.
     Raise [Not_found] if no bindings in tree.
  *)

  val find_all : 'a t -> key -> 'a list
  (**
     [find_all tree key]: Finds all bindings of [key] in [tree]
     with last binding first in list.
     Raise [Not_found] if no bindings in tree.
  *)

  val mem : 'a t -> key -> bool
  (**
     [mem tree key]: test whether [key] is bound in [tree].
  *)

  val iter : (key -> 'a -> 'b) -> 'a t -> unit
  (**
     [iter tree fn]: Apply [fn] to the data bound to each key.
     Only the current key bindings are used.
  *)

  val fold: (key -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold tree fn]: Apply [fn] to the data bound to each key.  Only
      the current key bindings are used.  *)

  val to_list: 'a t -> (key * 'a) list list
(**
    [to_list tree]: Return a list of the (lists of) elements in the
    tree, in descending order.
*)

end

module SimpleTree =
  functor (A:Data) ->
    BTree
      (struct
        type key = A.key
        let equals x y =
          ((x == y) || (Pervasives.compare x y) = 0)
        let lessthan x y =
          ((not (x == y)) && ((Pervasives.compare x y) < 0))
       end)


module StringTree = SimpleTree(struct type key = string end)
