(*-----
 Name: counter.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* counter: keep track of how many instances of a thing there are *)
    type 'a t = ('a * int) list

(* construct, recogniser *)
    val empty : unit -> 'a list
    val is_empty : 'a list -> bool

(* add x to list lst:
   if x is in lst, increment the count,
   other wise add x to lst, set count to 1
*)
    val add : 'a -> ('a * int) list -> ('a * int) list

(* [update x lst]:
   if x is in lst, increment the count 
   and return the previous value with the new list
   other wise add x to lst, set count to 1, return 0 with the new list
*)
    val update : 'a -> ('a * int) list -> (int* ('a * int) list)

(* remove x from list lst:
   if x is not in lst, do nothing

   if x is in lst, decrement the count,
   if new count is 0, remove x from the list
*)
    val remove : 'a -> ('a * int) list -> ('a * int) list

(* find x in list lst:
   if x is not in lst, raise Not_found

   if x is in lst, return the size
*)
    val find : 'a -> ('a * 'b) list -> 'b

(* [find_after x lst]:
   if x is not in lst, raise Not_found
   if x is in lst, return the next in the list
   if x is last, return None
*)
  val find_after: 'a -> ('a * 'b) list -> 'a option

(* [find_before x lst]:
   if x is not in lst, raise Not_found
   if x is in lst, return the previous in the list
   if x is first, return None
*)
  val find_before: 'a -> ('a * 'b) list -> 'a option
