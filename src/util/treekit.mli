(*----
  Name: treekit.mli
  Copyright M Wahab 2005-2010
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

(** Balanced and arbitrary trees

    A tree is built from less-than (<) and equality ( = ) relations. Each
    node of the tree is a list of items whose indices are equal under
    the less-than relation (not (x < y) and not (y < x)). The equality
    gives the exact match for an item in the list. 

    The simplest trees, {!Treekit.SimpleTree}, use [Pervasive.compare]
    for both less-than and equality and behave like a standard
    implementation of balanced trees.
*)

(** Comparison relations and index type used to build a tree. *)
module type TreeData =
sig
  type key
  (** Type of keys by which data is indexed. *)

  val equals: key -> key -> bool
  (** [equals x y]: True if [x] is the same as [y]. Must be accurate
      since this is used to find the data associated with a key.  *)

  val lessthan: key -> key -> bool
(** [lessthan x y]: True if [x] is considered less-than [y]. Does not
    need to be accurate since it is only used to narrow the list of
    possible matches for a key.
*)
end

(** {5 Trees} *)

(** The type of trees *)
module type TreeType=
sig

  type key 
  (** Type of keys by which data is indexed. *)

  val eql: key -> key -> bool
  val lessthan: key -> key -> bool

  type ('a)t= 
    | Nil 
    | Branch of ((key * 'a) list * ('a)t * ('a)t)

  val data: 'a t -> (key * 'a) list
  (** Get the data at the current branch. *)

  val left: 'a t -> 'a t
  (** Get the left branch of tree. *)

  val right: 'a t -> 'a t
  (** Get the right branch of tree. *)

  val nil: 'a t
  (** Make an empty tree. *)

  val create: (key * 'a)list -> 'a t -> 'a t -> 'a t
  (** Make a branch with data. *)

  val add: 'a t -> key -> 'a -> 'a t
  (** [add tr k d]: Add binding of [d] to [k] in tree [tr].  Previous
      bindings to [k] are hidden but not removed.  *)

  val replace: 'a t -> key -> 'a -> 'a t
  (** [replace tr k d]: Replace binding of [k] with [d] in tree [tr].
      Adds the binding even if [k] is not already bound in [tr].  *)

  val find: 'a t -> key -> 'a
  (** [find tree key]: Finds the current binding of [key] in [tree]. *)

  val find_all: 'a t -> key -> 'a list
  (** [find_all tree key]: Finds all bindings of [key] in [tree] with
      last binding first in list.  

      @raise [Not_found] if there are no bindings in [tree].  *)

  val mem: 'a t -> key -> bool
  (** [mem tree key]: Test whether [key] is bound in [tree].  *)

  val delete: 'a t -> key -> 'a t
  (** [delete tree key]: Remove the data currently bound to [key] in
      [tree].  Does nothing if key is not in tree.  *)

  val iter: (key -> 'a -> 'b) -> 'a t -> unit
  (** [iter tree fn]: Apply [fn] to the data bound to each key.  Only
      the current key bindings are used.  *)

  val fold: (key -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold tree fn]: Apply [fn] to the data bound to each key.  Only
      the current key bindings are used.  *)

  val to_list: 'a t -> ((key * 'a)list)list
(** [to_list tree]: Return a list of the (lists of) elements in the
    tree, in descending order.
*)

end

(** Trees *)
module Tree: 
  functor (A: TreeData) -> (TreeType with type key = A.key)

(** {5 Balanced trees} *)

(** The type of balanced trees *)
module type BTreeType=
sig

  type key 
  (** Type of keys by which data is indexed. *)

  val eql: key -> key -> bool
  val lessthan: key -> key -> bool

  type depth_t = int
  type ('a)t= 
    | Nil 
    | Branch of ((key * 'a) list * ('a)t * ('a)t * depth_t)

  val nil: 'a t
  (** Make an empty tree. *)

  val create: (key * 'a) list -> 'a t -> 'a t -> 'a t
  (** Make a branch with data. *)

  val data: 'a t -> (key * 'a) list
  (** Get the data at the current branch. *)

  val left: 'a t -> 'a t
  (** Get the left branch of tree. *)

  val right: 'a t -> 'a t
  (** Get the right branch of tree. *)

  val depth: 'a t -> depth_t
  (** Get the depth (number of levels) of the tree. *)

  val balance: 'a t -> 'a t
  (** Balance the tree. *)

  val add: 'a t -> key -> 'a -> 'a t
  (** [add tr k d]: Add binding of [d] to [k] in tree [tr].  Previous
      bindings to [k] are hidden but not removed.  *)

  val replace: 'a t -> key -> 'a -> 'a t
  (** [replace tr k d]: Replace binding of [k] with [d] in tree [tr].
      Adds the binding even if [k] is not already bound in [tr].  *)

  val delete: 'a t -> key -> 'a t
  (** [delete tree key]: Remove the data currently bound to [key] in
      [tree].  Does nothing if [key] is not in [tree]. *)

  val find: 'a t -> key -> 'a
  (** [find_all tree key]: Finds all bindings of [key] in [tree] with
      last binding first in list.  

      @raise [Not_found] if [key] is not bound in [tree].  *)

  val find_all: 'a t -> key -> 'a list
  (** [find_all tree key]: Finds all bindings of [key] in [tree] with
      last binding first in list.

      @raise [Not_found] if [key] is not bound in [tree].  *)

  val mem: 'a t -> key -> bool
  (** [mem tree key]: Test whether [key] is bound in [tree].  *)

  val iter: (key -> 'a -> 'b) -> 'a t -> unit
  (**
     [iter tree fn]: Apply [fn] to the data bound to each key.
     Only the current key bindings are used.
  *)

  val fold: (key -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold tree fn]: Apply [fn] to the data bound to each key.  Only
      the current key bindings are used.  *)

  val to_list: 'a t -> ((key * 'a)list) list
(** [to_list tree]: Return a list of the (lists of) elements in the
    tree, in descending order.
*)

end

(** {7 Balanced Trees} *)
module BTree: 
  functor (A: TreeData) -> (BTreeType with type key = A.key)


(** {5 Simple Trees} 

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
  (** Type of keys by which data is indexed. *)

  val eql: key -> key -> bool
  val lessthan: key -> key -> bool

  type depth_t = int
  type ('a)t= 
    | Nil 
    | Branch of ((key * 'a) list * ('a)t * ('a)t * depth_t)

  val nil: 'a t
  (** Make an empty tree. *)

  val create: (key * 'a)list -> 'a t -> 'a t -> 'a t
  (** Make a branch with data. *)

  val data: 'a t -> (key * 'a)list
  (** Get the data at the current branch. *)

  val left: 'a t -> 'a t
  (** Get the left branch of tree. *)

  val right: 'a t -> 'a t
  (** Get the right branch of tree. *)

  val depth: 'a t -> depth_t
  (** Get the depth (number of levels) of the tree. *)

  val balance: 'a t -> 'a t
  (** Balance the tree. *)

  val add: 'a t -> key -> 'a -> 'a t
  (** [add tr k d]: Add binding of [d] to [k] in tree [tr].  Previous
      bindings to [k] are hidden but not removed.  *)

  val replace: 'a t -> key -> 'a -> 'a t
  (** [replace tr k d]: Replace binding of [k] with [d] in tree [tr].
      Adds the binding even if [k] is not already bound in [tr].  *)

  val delete: 'a t -> key -> 'a t
  (** [delete tree key]: Remove the data currently bound to [key] in
      [tree].  Does nothing if key is not in tree. *)

  val find: 'a t -> key -> 'a
  (** [find_all tree key]: Finds all bindings of [key] in [tree] with
      last binding first in list.

      @raise [Not_found] if [key] is not bound in [tree].
  *)

  val find_all: 'a t -> key -> 'a list
  (** [find_all tree key]: Finds all bindings of [key] in [tree] with
      last binding first in list.

      @raise [Not_found] if [key] is not bound in [tree].
  *)

  val mem: 'a t -> key -> bool
  (** [mem tree key]: test whether [key] is bound in [tree].  *)

  val iter: (key -> 'a -> 'b) -> 'a t -> unit
  (** [iter tree fn]: Apply [fn] to the data bound to each key.  Only
      the current key bindings are used.  *)

  val fold: (key -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold tree fn]: Apply [fn] to the data bound to each key.  Only
      the current key bindings are used.  *)

  val to_list: 'a t -> (key * 'a) list list
(** [to_list tree]: Return a list of the (lists of) elements in the
    tree, in descending order.
*)

end

(** Balanced Trees indexed by type A.key *)
module SimpleTree: 
  functor (A:Data) -> (SimpleTreeType with type key = A.key)


(** Balanced Trees indexed by strings *)
module StringTree: SimpleTreeType with type key = string
