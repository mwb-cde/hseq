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

(*
(** 
   [compare x y]: Compare tags [x] and [y].
*)
val compare: 'a ref -> 'a ref -> int
*)

module OrdTag: 
  sig
    type t=tag_type
    val compare : t -> t -> int
  end 
module Set : Set.S with type elt = t

(* Tables *)

module HashedTag: 
  sig
    type t=tag_type
    val equal : t -> t -> bool
    val hash: t -> int
  end 
module Table : Hashtbl.S with type key = t

