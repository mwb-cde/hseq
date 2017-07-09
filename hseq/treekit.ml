(*----
  Name: treekit.ml
  Copyright Matthew Wahab 2005-2017
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

  (** [compare x y]: Compare [x] and [y], returning their order. *)
  val compare: key -> key -> Order.t
end

module type TreeType =
sig
  open Order

  type key
  val compare: key -> key -> Order.t

  type ('a)t
  (** Type of trees. *)

(*
  (* data tr: get the data at the current branch *)
  val data : 'a t -> (key * ('a) list)

  (* left tr: get left branch of tree *)
  val left : 'a t -> 'a t

  (* right tr: get right branch of tree *)
  val right : 'a t -> 'a t

  (* nil: make an empty tree *)
  val nil : 'a t
 *)

  (** Get the depth (number of levels) of the tree. *)
  val depth: 'a t -> int

  val empty: 'a t
  (* The empty tree *)

(*
  (* create: make a branch with data *)
  val create : (key * ('a) list) -> 'a t -> 'a t -> 'a t
 *)

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

  val mem: 'a t -> key -> bool

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

  val iter: (key -> 'a -> unit) -> 'a t -> unit

  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold tree fn]: Apply [fn] to the data bound to each key.  Only
      the current key bindings are used.  *)

  (* to_list tree:
     return a list of the (lists of) elements in the
     tree in descending order
  *)
  val to_list: 'a t -> (key * 'a) list
end

module Tree = functor (A: TreeData)->
struct
  open Order

  type key = A.key
  (*
    let eql x y =  (Pervasives.compare x y) = 0
    let lessthan x y= (Pervasives.compare x y) < 0
  *)
  let compare = A.compare

  type ('a)t =
    Nil
  | Branch of ((key * ('a) list) * ('a)t * ('a)t)

  let create x l r = Branch(x, l, r)

  let empty = Nil
  (** Make an empty tree *)

  (* Tree information/manipulation. *)

  let rec depth tr =
    match tr with
    | Nil -> 0
    | Branch(_, l, r) -> max (depth l) (depth r)

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

  let add tree key data =
    let rec add_aux tr =
      match tr with
        Nil -> Branch((key, [data]), Nil, Nil)
      | Branch((k, ys), l, r) ->
         begin
           match compare key k with
           | Equal -> Branch((key, data::ys), l, r)
           | LessThan -> Branch((k, ys), (add_aux l), r)
           | GreaterThan -> Branch((k, ys), l, add_aux r)
         end
    in
    add_aux tree

  (*
    replace tr k d:
    replace binding of k with d in tree tr
    previous bindings to k are hidden
    but not removed
    add binding if necessary
  *)

  let replace tree key data =
    let rec replace_aux tr=
      match tr with
      | Nil -> Branch((key, [data]), Nil, Nil)
      | Branch((k, ys), l, r) ->
         begin
           match compare key k with
           | Equal -> Branch((key, data::ys), l, r)
           | LessThan -> Branch((k, ys), (replace_aux l), r)
           | GreaterThan -> Branch((k, ys), l, (replace_aux r))
         end
    in
    replace_aux tree

  (*
     find tree key
     finds the current binding of key in tree
  *)

  let find tree key =
    let rec find_aux tr =
      match tr with
      | Nil -> raise Not_found
      | Branch((k, ys), l, r) ->
         begin
           match compare key k with
           | Equal -> List.hd ys
           | LessThan -> find_aux l
           | GreaterThan -> find_aux r
         end
    in
    find_aux tree

  let find_all tree key =
    let rec find_aux tr =
      match tr with
        Nil -> raise Not_found
      | Branch((k, ys), l, r) ->
         begin
           match compare key k with
           | Equal -> ys
           | LessThan -> find_aux l
           | GreaterThan -> find_aux r
         end
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

  (* split tr:
     remove greatest data d' from tree, giving tree tr'
     returning (d', tr')
     if node n storing d' has a left branch b, node n is replaced
     with b.
     node n will never have a right branch since this would mean
     that there is an element greater than d'.
  *)

  let rec split tr =
    match tr with
    | Nil -> failwith "Tree.split"
    | Branch(data, tr1, Nil) -> (data, tr1)
    | Branch(data, tr1, tr2) ->
      let rdata, rtr = split tr2
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
    | Nil -> tr2
    | _ ->
      let data, ntr = split tr1
      in
      Branch(data, ntr, tr2)

  (* delete tree key

     removes the data currently bound to key in tree
     does nothing if key is not in tree
  *)

  let rec delete tr key =
    match tr with
    | Nil -> tr
    | Branch((_, []), _, _) -> failwith ("Tree.delete")
    | Branch((k, ys), l, r) ->
       begin
         match compare key k with
         | Equal ->
            begin
              match ys with
              | [] -> failwith ("Tree.delete: invalid tree.")
              | (_::[]) -> join l r
              | (_::nlst) -> Branch((k, nlst), l, r)
            end
         | LessThan -> Branch((k, ys), delete l key, r)
         | GreaterThan -> Branch((k, ys), l, delete r key)
       end
  let remove = delete

  (*
     iter tree fn:

     apply fn to the data bound to each key
     only the current key bindings are used.
  *)

  let iter fn tree =
    let rec iter_aux tr todo =
      match tr with
      | Nil ->
         begin
           match todo with
           | [] -> ()
           | (t::ts) -> iter_aux t ts
         end
      | Branch((k, data::_), Nil, r) ->
         begin
           fn k data;
           iter_aux r todo
         end
      | Branch(data, l, r) ->
        iter_aux l (Branch(data, Nil, r)::todo)
    in
    iter_aux tree []

  let fold fn tree init =
    let rec fold_aux total tr todo =
      match tr with
      | Nil ->
         begin
           match todo with
           | [] -> total
           | (t::ts) -> fold_aux total t ts
         end
      | Branch((k, data::_), Nil, r) ->
        let branch_total = fn k data total
        in
        fold_aux branch_total r todo
      | Branch((_, []), _, _) -> (failwith "Invalid tree.")
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
      | Nil -> rslt
      | Branch((k, data::_), l, r)
        -> to_list_aux r ((k, data)::(to_list_aux l rslt))
      | Branch((_, []), _, _)
        -> failwith "invalid tree"
    in
    to_list_aux tree []

end


(* Balanced lookup trees *)

module type BTreeType = TreeType

module BTree = functor (A: TreeData) ->
struct
  open Order

  type key = A.key
  let compare = A.compare

  type ('a)t=
    Nil
  | Branch of ((key * ('a) list) * ('a)t * ('a)t * int)

  let create x l r = Branch(x, l, r, 1)
  let empty = Nil

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

  let add tree key data=
    let rec add_aux tr=
      match tr with
      | Nil -> Branch((key, [data]), Nil, Nil, 1)
      | Branch((k, ys), l, r, d) ->
         begin
           match compare key k with
           | Equal ->
              (* Add data to existing list, no rebalancing needed. *)
              Branch((k, data::ys), l, r, d)
           | LessThan ->
              (* Add data to lhs tree, rebalancing may be needed. *)
              let nl = add_aux l in
              balance (Branch((k, ys), nl, r, (depth nl) + 1))
           | GreaterThan ->
              (* Add data to rhs tree, rebalancing may be needed. *)
              let nr = add_aux r in
              balance (Branch((k, ys), l, nr, (depth nr) + 1))
         end
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

  let replace tree key data =
    let swap_first x l =
      match l with
      | [] -> [x]
      | (_::ys) -> (x::ys)
    in
    let rec replace_aux tr =
      match tr with
      | Nil -> Branch((key, [data]), Nil, Nil, 1)
      | Branch((k, ys), l, r, d) ->
         begin
           match compare key k with
           | Equal ->
              (* replace data in existing list, no rebalancing needed *)
              Branch ((k, (swap_first data ys)), l, r, d)
           | LessThan ->
              (* replace data in lhs tree, rebalancing may be needed *)
              balance (Branch ((k, ys), (replace_aux l), r, d))
           | GreaterThan ->
              (* replace data in rhs tree, rebalancing
                 may be needed *)
              balance (Branch((k, ys), l, (replace_aux r), d))
         end
    in
    (* replace data in tree *)
    replace_aux tree

  (* split tr:
     remove greatest data d' from tree, giving tree tr'
     returning (d', tr')
     if node n storing d' has a left branch b, node n is replaced
     with b.
     node n will never have a right branch since this would mean
     that there is an element greater than d'.
  *)
  let rec split tr =
    match tr with
      Nil -> failwith "Tree.split"
    | Branch((key, data), tr1, Nil, _) ->  ((key, data), tr1)
    | Branch((key, data), tr1, tr2, _) ->
      let ndata, nright = split tr2
      and nleft = tr1
      in
      let ntree = Branch((key, data), nleft, nright,
                         (max_depth nleft nright)+1)
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
      let data, nleft = split tr1
      and nright = tr2
      in
      let ntree = Branch(data, nleft, nright,
                         (max_depth nleft nright)+1)
      in
      balance ntree

  (* delete tree key

     removes the data currently bound to key in tree
     does nothing if key is not in tree
  *)
  let delete tree key =
    let rec delete_node (k, data) l r dp =
      begin
        match data with
        | [] -> failwith "Invalid tree."
        | (_::[]) -> join l r
        | (_::nlst) -> Branch((k, nlst), l, r, dp)
      end
    and delete_aux tr =
      match tr with
      | Nil -> Nil
      | Branch((k, data), l, r, dp) ->
         begin
           let order = compare key k in
           if order = Equal
           then balance(delete_node (k,data) l r dp)
           else
             begin
               let nleft, nright =
                 if order = LessThan
                 then delete_aux l, r
                 else (l, delete_aux r)
               in
               let ndepth = (max_depth nleft nright) + 1 in
               balance(Branch((k,data), nleft, nright, ndepth))
             end
         end
    in
    delete_aux tree

  let remove = delete

  (*
     find tree key
     finds the current binding of key in tree
  *)

  let find tree key =
    let rec find_aux tr =
      match tr with
        Nil -> raise Not_found
      | Branch((_, []), _, _, _) -> failwith "Invalid tree."
      | Branch((k, ys), l, r, _) ->
         begin
           match compare key k with
           | Equal -> List.hd ys
           | LessThan -> find_aux l
           | GreaterThan -> find_aux r
         end
    in
    find_aux tree

  (*
     find_all tree key
     finds all bindings of key in tree
     with last binding first in list
     raise Not_found if no bindings in tree
  *)

  let find_all tree key =
    let rec find_aux tr =
      match tr with
      | Nil -> raise Not_found
      | Branch((_, []), _, _, _) -> failwith "Invalid tree."
      | Branch((k, ys), l, r, _) ->
         begin
           match compare key k with
           | Equal -> ys
           | LessThan -> find_aux l
           | GreaterThan -> find_aux r
         end
    in
    find_aux tree


  (* mem tree key:
     return true if key is bound in tree
     false otherwise
  *)
  let mem tree key =
    try (ignore(find tree key); true)
    with Not_found -> false

  (*
     iter tree fn:

     apply fn to the data bound to each key
     only the current key bindings are used.
  *)

  let iter fn tree=
    let rec iter_aux tr todo =
      match tr with
        Nil ->
        begin
          match todo with
          |  [] ->()
          | (t::ts) -> iter_aux t ts
        end
      | Branch((k, data::_), Nil, r, _) ->
        fn k data;
        iter_aux r todo
      | Branch((k, []), Nil, r, _) -> (failwith "Invalid tree.")
      | Branch(data, l, r, d) ->
        iter_aux l (Branch(data, Nil, r, d)::todo)
    in
    iter_aux tree []

  let fold fn tree init =
    let rec fold_aux total tr todo =
      match tr with
      | Nil ->
        (match todo with
        | [] -> total
        | (t::ts) -> fold_aux total t ts)
      | Branch((k, data::_), Nil, r, _) ->
        let branch_total = fn k data total
        in
        fold_aux branch_total r todo
      | Branch((_, []), _, _, _) -> failwith "Invalid tree."
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
      | Branch((k, data::_), l, r, _)
        -> to_list_aux r ((k, data)::(to_list_aux l rslt))
      | Branch((_, []), _, _, _)
        -> failwith "invalid tree"
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

(** Fake trees based on maps. *)
module MapTree = functor (A: TreeData) ->
struct
  type key = A.key
  let compare = A.compare

  module MTree =
    Map.Make
      (struct
        type t = A.key
        let compare x y= Order.Util.order_to_int (A.compare x y)
      end)

  type ('a)t = ('a)MTree.t

  let depth _ = failwith "Wrong kind of tree."

  let empty = MTree.empty
  let add tr k v = MTree.add k v tr
  let replace = add
  let find tr k = MTree.find k tr
  let find_all tr k = [find tr k]
  let mem tr k = MTree.mem k tr
  let remove tr k = MTree.remove k tr
  let delete tr k = MTree.remove k tr
  let iter fn tr = MTree.iter fn tr
  let fold fn tr v = MTree.fold fn tr v
  let to_list tr =
    let fn key v lst = (key, v)::lst
    in
    fold fn tr []
end

(** Simple trees, indexed by Pervasives.compare. *)

module type Data =
sig
  type key
end

module type SimpleTreeType = TreeType

module SimpleTree =
  functor (A:Data) ->
    BTree
      (struct
        type key = A.key
        let compare = Order.Util.compare
       end)


module StringTree = SimpleTree(struct type key = string end)
