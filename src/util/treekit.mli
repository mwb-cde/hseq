(*-----
 Name: treekit.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* lookup trees *)

module type TreeData =
  sig
    (* type of keys *)
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
functor (A : TreeData) ->
  sig

    val eql : A.key -> A.key -> bool
    val lessthan : A.key -> A.key -> bool

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

(* Standard Trees *)
module Tree : TreeType

(* BTree: balanced lookup trees *)

module type BTreeType=
functor (A : TreeData) ->
  sig

    val eql : A.key -> A.key -> bool
    val lessthan : A.key -> A.key -> bool

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

module BTree : BTreeType
