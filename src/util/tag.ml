(*----
 Name: tag.ml
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

(* 
   Tags: 
   unique identifiers with which to identify
   sequents in a goal and formulas in a sequent
 *)
type tag_type = string ref

type t= tag_type

let named x = ref x
let name x = !x

let null=ref ""
let create() = ref ""

let equal x y = x==y

(**
   [fuzzy_compare x y]: Compare references [x] and [y].
   (ported from HOL pointer_compare).

   Not guarenteed (or expected) to be accurate.
*)
   let fuzzy_compare x y=
   let cast x = ((Obj.magic x):int)
   in 
   compare (cast x) (cast y)

(***
   Trees
*)

module TagTreeData=
  struct
    type key = tag_type
    let equals = equal
    let lessthan x y= (fuzzy_compare x y)<0
  end

module TagTree = Treekit.BTree(TagTreeData)
type ('a)tree = ('a)TagTree.t

