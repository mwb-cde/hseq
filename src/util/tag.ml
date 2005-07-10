(*----
 Name: tag.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
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

