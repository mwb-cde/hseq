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
   [compare x y]: Compare references [x] and [y].
   (ported from HOL pointer_compare).
*)
   let compare x y=
   let cast x = ((Obj.magic x):int)
   in 
   compare (cast x) (cast y)

(* Sets *)

let tag_compare = compare

module OrdTag =
struct
  type t = tag_type

  let compare x y = tag_compare x y
end

module Set = Set.Make(OrdTag)

(* Tables *)

module HashedTag= 
struct
  type t = tag_type
  let equal x y = (x == y)
  let hash = Hashtbl.hash
end

module Table = Hashtbl.Make(HashedTag)
