(*-----
 Name: treekit.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** Balanced and unbalanced trees

   A tree is built from less-than (<) and equality (=) relations. Each
   node of the tree is a list of items whose indices are equal under
   the less-than relation (not (x<y) and not (y<x)). The equality
   gives the exact match for an item in the list. 

   The simplest trees, {!Treekit.SimpleTree}, use [Pervasive.compare]
   for both less-than and equality and behave like a standard
   implementation of balanced trees.
*)

(**
   The comparison relations and index type used to build a tree.
*)
module type TreeData =
  sig
    type key
    (** type of keys by which data is indexed. *)

    val equals : key -> key -> bool
  (** 
     [equals x y]: True if [x] is the same as [y]. Must be accurate
     since this is used to find the data associated with a key.
   *)

    val lessthan : key -> key -> bool
(**
     [lessthan x y]: True if [x] is considered less-than [y]. Does not
     need to be accurate since it is only used to narrow the list of
     possible matches for a key.
  *)
  end

(** {5 Unbalanced trees} *)

(** The type of balanced trees *)
module type TreeType=
functor (A : TreeData) ->
  sig

    type key = A.key
    (** type of keys by which data is indexed. *)

    val eql : A.key -> A.key -> bool
    val lessthan : A.key -> A.key -> bool

    type ('a)t= 
	Nil 
      | Branch of ( (A.key * 'a) list * ('a)t * ('a)t)

    val data : 'a t -> (A.key * 'a) list
(** Get the data at the current branch *)

    val left : 'a t -> 'a t
(** Get the left branch of tree *)

    val right : 'a t -> 'a t
(** Get the right branch of tree *)

    val nil : 'a t
(** Make an empty tree *)

    val create : (A.key * 'a) list -> 'a t -> 'a t -> 'a t
(** Make a branch with data *)

    val add : 'a t -> A.key -> 'a -> 'a t
(**
   [add tr k d]: Add binding of [d] to [k] in tree [tr].  Previous
   bindings to [k] are hidden but not removed.
*)

    val replace : 'a t -> A.key -> 'a -> 'a t
(**
   [replace tr k d]: Replace binding of [k] with [d] in tree [tr].
   Adds the binding even if [k] is not already bound in [tr].
*)

    val find : 'a t -> A.key -> 'a
(**
   [find tree key]: Finds the current binding of [key] in [tree]
*)

    val find_all : 'a t -> A.key -> 'a list
(**
   [find_all tree key]: Finds all bindings of [key] in [tree]
   with last binding first in list.
   Raise [Not_found] if no bindings in tree.
 *)

    val mem : 'a t -> A.key -> bool
(**
   [mem tree key]: test whether [key] is bound in [tree].
*)

(*      val remove : 'a t -> A.key -> 'a t *)
(**
   remove tree key

   removes the data currently bound to key in tree
   does nothing if key is not in tree

 *)

    val delete : 'a t -> A.key -> 'a t
(**
   [delete tree key]: Remove the data currently bound to [key] in
   [tree].  Does nothing if key is not in tree
*)

    val iter : (A.key -> 'a -> 'b) -> 'a t -> unit
(**
   [iter tree fn]: Apply [fn] to the data bound to each key.
   Only the current key bindings are used.
*)

    val to_list: 'a t -> (A.key * 'a) list list
(** 
   [to_list tree]: Return a list of the (lists of) elements in the
   tree, in descending order.
*)

  end

(** Unbalanced Trees *)
module Tree : TreeType

(** {5 Balanced trees} *)

(** The type of balanced trees *)
module type BTreeType=
    functor (A : TreeData) ->
  sig

    type key = A.key
    (** type of keys by which data is indexed. *)

    val eql : A.key -> A.key -> bool
    val lessthan : A.key -> A.key -> bool

    type depth_t = int
    type ('a)t= 
	Nil 
      | Branch of ((A.key * 'a) list * ('a)t * ('a)t * depth_t)


    val nil : 'a t
(** Make an empty tree *)

    val create : (A.key * 'a) list -> 'a t -> 'a t -> 'a t
(** Make a branch with data *)

    val data : 'a t -> (A.key * 'a) list
(** Get the data at the current branch *)

    val left : 'a t -> 'a t
(** Get the left branch of tree *)

    val right : 'a t -> 'a t
(** Get the right branch of tree *)

    val depth : 'a t -> depth_t
(** Get the depth (number of levels) of the tree *)

    val balance : 'a t -> 'a t
(** Balance the tree. *)

    val add : 'a t -> A.key -> 'a -> 'a t
(**
   [add tr k d]: Add binding of [d] to [k] in tree [tr].  Previous
   bindings to [k] are hidden but not removed.
*)

    val replace : 'a t -> A.key -> 'a -> 'a t
(**
   [replace tr k d]: Replace binding of [k] with [d] in tree [tr].
   Adds the binding even if [k] is not already bound in [tr].
*)

(*       val remove : 'a t -> A.key -> 'a t*)
(*
   remove tree key

   removes the data currently bound to key in tree
   does nothing if key is not in tree
   removal of Branch(x, l, r, d) 
   is by putting subtree r at the rightmost point of subtree l,
   then rebalancing at every level
 *)

    val delete : 'a t -> A.key -> 'a t
(**
   [delete tree key]: Remove the data currently bound to [key] in
   [tree].  Does nothing if key is not in tree
*)

    val find : 'a t -> A.key -> 'a
(**
   [find_all tree key]: Finds all bindings of [key] in [tree]
   with last binding first in list.
   Raise [Not_found] if no bindings in tree.
 *)

    val find_all : 'a t -> A.key -> 'a list
(**
   [find_all tree key]: Finds all bindings of [key] in [tree]
   with last binding first in list.
   Raise [Not_found] if no bindings in tree.
 *)

    val mem : 'a t -> A.key -> bool
(**
   [mem tree key]: test whether [key] is bound in [tree].
*)

    val iter : (A.key -> 'a -> 'b) -> 'a t -> unit
(**
   [iter tree fn]: Apply [fn] to the data bound to each key.
   Only the current key bindings are used.
*)

    val to_list: 'a t -> (A.key * 'a) list list
(** 
   [to_list tree]: Return a list of the (lists of) elements in the
   tree, in descending order.
*)

  end

(** {7 Unbalanced Trees} *)
module BTree : BTreeType


(** {5 Simple trees} 

   Trees ordered by [Pervasives.compare]. Example: a balanced tree
   indexed by strings is constructed by [SimpleTree (struct type key
   = string end)].
*)

(** The type of the index. *)
module type Data =
sig
  type key 
end

(** Balanced trees, ordered by [Pervasives.compare] *)

module type SimpleTreeType =
  sig

    type key 
    (** type of keys by which data is indexed. *)

    val eql : key -> key -> bool
    val lessthan : key -> key -> bool

    type depth_t = int
    type ('a)t= 
	Nil 
      | Branch of ((key * 'a) list * ('a)t * ('a)t * depth_t)

    val nil : 'a t
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

(*       val remove : 'a t -> key -> 'a t*)
(*
   remove tree key

   removes the data currently bound to key in tree
   does nothing if key is not in tree
   removal of Branch(x, l, r, d) 
   is by putting subtree r at the rightmost point of subtree l,
   then rebalancing at every level
 *)

    val delete : 'a t -> key -> 'a t
(**
   [delete tree key]: Remove the data currently bound to [key] in
   [tree].  Does nothing if key is not in tree
*)

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

    val to_list: 'a t -> (key * 'a) list list
(** 
   [to_list tree]: Return a list of the (lists of) elements in the
   tree, in descending order.
*)

  end

(** Balanced Trees indexed by type A.key *)
module SimpleTree : 
   functor (A:Data) -> SimpleTreeType with type key = A.key


(** Balanced Trees indexed by strings *)
module StringTree: SimpleTreeType with type key = string
