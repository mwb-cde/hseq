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
type tag_type

type t=tag_type

val named: string->t
val name: t->string
val null:t
val create: unit->t

val equal: t->t->bool

(**
   Trees 

   (Depends on [Obj.magic]).
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


(* sets *)

(*
module OrdTag: 
  sig
    type t=tag_type
    val compare : t -> t -> int
  end 
module Set : Set.S with type elt = t
*)

(* Tables *)
(*
module HashedTag: 
  sig
    type t=tag_type
    val equal : t -> t -> bool
    val hash: t -> int
  end 
module Table : Hashtbl.S with type key = t
*)
