(*----
 Name: tag.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(**
   Unique identifiers. 

   Unique identifiers with which to identify sequents in a goal and
   formulas in a sequent.
*)

type tag_type
type t=tag_type

val create: unit->t
(**
   [create]: Make an unnamed tag.
*)

val named: string->t
(**
   [named s]: Make a tag with name [s].
*)
  
val name: t->string
(**
   [name x]: Get the name of tag [x].
*)

val null:t
(**
   [null]: A constant, unnamed tag.
*)

val equal: t->t->bool
(**
   [equal]: Equality of tags.
*)

(*
(**
   {5 Trees}

   Trees indexed by tags depend on being able to use [Obj.magic] to
   cast a reference to an integer.
*)

module TagTreeData: Treekit.TreeData
module TagTree:
    sig
      val eql : tag_type -> tag_type -> bool
      val lessthan : tag_type -> tag_type -> bool
      type 'a t 
      val nil : 'a t
      val create : (tag_type * 'a) list -> 'a t -> 'a t -> 'a t
      val data : 'a t -> (tag_type * 'a) list
      val left : 'a t -> 'a t
      val right : 'a t -> 'a t
      val balance : 'a t -> 'a t
      val add : 'a t -> tag_type -> 'a -> 'a t
      val replace : 'a t -> tag_type -> 'a -> 'a t
      val delete : 'a t -> tag_type -> 'a t
      val find : 'a t -> tag_type -> 'a
      val find_all : 'a t -> tag_type -> 'a list
      val mem : 'a t -> tag_type -> bool
      val iter : (tag_type -> 'a -> 'b) -> 'a t -> unit
      val to_list : 'a t -> (tag_type * 'a) list list
    end 

type ('a)tree = ('a)TagTree.t
(** The type of tree indexed by tags *)
*)
