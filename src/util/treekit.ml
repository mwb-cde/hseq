
(* lookup trees *)

module type TreeData =
  sig
    (* type of keys *)
    type key
	  (* comparisons functions between keys *)
    val equals : key -> key -> bool
  end


module type TreeType=
functor (A : TreeData) ->
  sig

    val eql : 'a -> 'a -> bool
    val lessthan : 'a -> 'a -> bool

    type ('a)t= 
	Nil 
      | Branch of ( (A.key * 'a) list * ('a)t * ('a)t)

(* data tr: get the data at the current branch *)
    val data : 'a t -> (A.key * 'a) list

(* left tr: get left branch of tree *)
    val left : 'a t -> 'a t

(* right tr: get right branch of tree *)
    val right : 'a t -> 'a t

(* nil: make an empty tree *)
    val nil : 'a t

(* create: make a branch with data *)
    val create : (A.key * 'a) list -> 'a t -> 'a t -> 'a t

(*
   add tr k d:
   add binding of d to k in tree tr
   previous bindings to k are hidden 
   but not removed
 *)
    val add : 'a t -> A.key -> 'a -> 'a t

(*
   replace tr k d:
   replace binding of k with d in tree tr
   previous bindings to k are hidden 
   but not removed
   add binding if necessary
 *)
    val replace : 'a t -> A.key -> 'a -> 'a t

(* 
   find tree key
   finds the current binding of key in tree
 *)

    val find : 'a t -> A.key -> 'a

(* 
   find_all tree key
   finds all bindings of key in tree
   with last binding first in list
   raise Not_found if no bindings in tree
 *)
    val find_all : 'a t -> A.key -> 'a list

(* mem tree key:
   return true if key is bound in tree
   false otherwise
 *)

    val mem : 'a t -> A.key -> bool

(*
   remove tree key

   removes the data currently bound to key in tree
   does nothing if key is not in tree

 *)
(*      val remove : 'a t -> A.key -> 'a t *)

(*
   delete tree key

   removes the data currently bound to key in tree
   does nothing if key is not in tree
 *)
    val delete : 'a t -> A.key -> 'a t

(* 
   iter tree fn:

   apply fn to the data bound to each key
   only the current key bindings are used.
 *)

    val iter : (A.key -> 'a -> 'b) -> 'a t -> unit


(* to_list tree:
   return a list of the (lists of) elements in the
   tree in descending order
 *)
    val to_list: 'a t -> (A.key * 'a) list list

  end



module Tree=
  functor (A: TreeData)->
  struct

    let eql x y =  (Pervasives.compare x y) = 0
    let lessthan x y= (Pervasives.compare x y) < 0

    type ('a)t= 
	Nil 
      | Branch of ( (A.key * 'a) list * ('a)t * ('a)t)


    let nil = Nil
    let create x l r = Branch(x, l, r)


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

(*
   remove tree key

   removes the data currently bound to key in tree
   does nothing if key is not in tree

 *)


(*
   let add_rightmost dst src =
   let rec add_aux tr =
   match tr with 
   Nil -> src
   | Branch(d, l, Nil) -> Branch(d, l, src)
   | Branch(d, l, r) -> Branch (d, l, add_aux r)
   in 
   match src with
   Nil -> dst
   | _ -> add_aux dst

   let remove tree key =
   let rec remove_aux tr =
   match tr with
   Nil -> tr
   | Branch(((y, z)::ys), l, r) ->
   if(eql key y)
   then 
   (match (list_remove key ((y, z)::ys)) with
   []  -> add_rightmost l r
   | nlst -> Branch(nlst, l, r))
   else 
   if (lessthan key y)
   then Branch((y, z)::ys, remove_aux l, r)
   else Branch((y, z):: ys, l, remove_aux r)
   | _ -> failwith "Tree.remove"
   in 
   remove_aux tree
 *)
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
	| Branch(data, Nil, r) ->
	    list_iter fn data;
	    iter_aux r todo
	| Branch(data, l, r) ->
	    iter_aux l (Branch(data, Nil, r)::todo)
      in 
      iter_aux tree []   

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
functor (A : TreeData) ->
  sig
    val eql : 'a -> 'a -> bool
    val lessthan : 'a -> 'a -> bool

    type depth_t = int
    type ('a)t= 
	Nil 
      | Branch of ((A.key * 'a) list * ('a)t * ('a)t * depth_t)


(* nil: make an empty tree *)
    val nil : 'a t

(* create: make a branch with data *)
    val create : (A.key * 'a) list -> 'a t -> 'a t -> 'a t

(*
   tree information/manipulation
 *)

(* data tr: get the data at the current branch *)

    val data : 'a t -> (A.key * 'a) list

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
    val add : 'a t -> A.key -> 'a -> 'a t


(*
   replace tr k d:
   replace binding of k with d in tree tr
   previous bindings to k are hidden 
   but not removed
   add binding if necessary
 *)
    val replace : 'a t -> A.key -> 'a -> 'a t

(*
   remove tree key

   removes the data currently bound to key in tree
   does nothing if key is not in tree
   removal of Branch(x, l, r, d) 
   is by putting subtree r at the rightmost point of subtree l,
   then rebalancing at every level
 *)
(*       val remove : 'a t -> A.key -> 'a t*)

(*
   delete tree key

   deletes the data currently bound to key in tree
   does nothing if key is not in tree.
 *)

    val delete : 'a t -> A.key -> 'a t

(* 
   find tree key
   finds the current binding of key in tree
 *)
    val find : 'a t -> A.key -> 'a

(* 
   find_all tree key
   finds all bindings of key in tree
   with last binding first in list
   raise Not_found if no bindings in tree
 *)
    val find_all : 'a t -> A.key -> 'a list

(* mem tree key:
   return true if key is bound in tree
   false otherwise
 *)
    val mem : 'a t -> A.key -> bool

(* 
   iter tree fn:

   apply fn to the data bound to each key
   only the current key bindings are used.
 *)

    val iter : (A.key -> 'a -> 'b) -> 'a t -> unit

(* to_list tree:
   return a list of the (lists of) elements in the
   tree in descending order
 *)
    val to_list: 'a t -> (A.key * 'a) list list
  end


module BTree=
  functor (A: TreeData)->
  struct

    let eql x y =  (Pervasives.compare x y) = 0
    let lessthan x y= (Pervasives.compare x y) < 0

    type depth_t = int

    type ('a)t= 
	Nil 
      | Branch of ((A.key * 'a) list * ('a)t * ('a)t
		     * depth_t)

    let nil = Nil
    let create x l r = Branch(x, l, r, 1)

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

(*
   remove tree key

   removes the data currently bound to key in tree
   does nothing if key is not in tree
   removal of Branch(x, l, r, d) 
   is by putting subtree r at the rightmost point of subtree l,
   then rebalancing at every level
 *)

(* add_rightmost dst src: 
   add src to the rightmost tip of dst
   rebalancing on the way back up
   both src and dst are assumed to be balanced
 *)

(*
   let add_rightmost dst src =
   let nd = depth src
   in 
   let rec add_aux tr =
   match tr with 
   Nil -> src
   | Branch(x, l, Nil, d) -> 
   balance (Branch(x, l, src, d+nd))
   | Branch(x, l, r, d) -> 
   balance (Branch (x, l, add_aux r, d+nd))
   in 
   match src with
   Nil -> dst
   | _ -> add_aux dst

   let remove tree key =
   let rec remove_aux tr =
   match tr with
   Nil -> tr
   | Branch(((y, z)::ys), l, r, d) ->
   if(eql key y)
   then 
   (match (list_delete key ((y, z)::ys)) with
   []  -> add_rightmost l r
   | nlst -> Branch(nlst, l, r, d))
   else 
   if (lessthan key y)
   then 
   let nl=remove_aux l
   in 
   balance(Branch((y, z)::ys, 
   nl, r, 
   (max (depth nl) (depth r))+1))
   else 
   let nr=remove_aux r
   in 
   balance(Branch((y, z):: ys, 
   l, nr,
   (max (depth l) (depth nr)) +1))
   | _ -> failwith "Tree.remove"
   in 
   remove_aux tree
 *)

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
