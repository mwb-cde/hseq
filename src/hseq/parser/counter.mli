(*----
  Name: counter.mli
  Copyright M Wahab 2005-2014
  Author: M Wahab  <mwb.cde@gmail.com>

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

(** Counter.t: Keep track of how many instances of a thing there are. *)
type 'a t = ('a * int) list

(** Constructor, recogniser. *)
val empty: unit -> 'a list
val is_empty: 'a list -> bool

(** add [x] to list [lst]: If [x] is in [lst], increment the count, other
    wise add [x] to [lst], setting count to [1].
*)
val add: 'a -> ('a * int) list -> ('a * int) list

(** [update x lst]: if [x] is in [lst], increment the count and return
   the previous value with the new list other wise add [x] to [lst],
   setting count to 1, return 0 with the new list.
*)
val update: 'a -> ('a * int) list -> (int* ('a * int) list)

(** remove x from list lst: If [x] is not in [lst], do nothing. If [x] is
    in [lst], decrement the count, if new count is 0, remove [x] from the
    list.
*)
val remove: 'a -> ('a * int) list -> ('a * int) list

(** find x in list lst: If [x] is not in [lst], raise [Not_found].  If
    [x] is in [lst], return the size.
*)
val find: 'a -> ('a * 'b) list -> 'b

(** [find_after x lst]: If [x] is not in [lst], raise [Not_found]. If
    [x] is in [lst], return the next element in the list.  If [x] is
    last, return [None].
*)
val find_after: 'a -> ('a * 'b) list -> 'a option

(** [find_before x lst]: If [x] is not in [lst], raise [Not_found].
    If [x] is in [lst], return the previous element in the list.  If
    [x] is first, return [None].
*)
val find_before: 'a -> ('a * 'b) list -> 'a option
