(*----
  Name: lib.mli
  Copyright Matthew Wahab 2005-2018
  Author: Matthew Wahab <mwb.cde@gmail.com>

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

(** General purpose functions *)

(** Operators **)
module Ops :
sig

  val (<+) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** Syntax for function composition. [(f <+ g)(x)] is [f(g(x))]. *)

end

(* Simple utility functions *)

val iteri : (int -> 'a array -> 'b) -> 'a array -> unit
val list_string : ('a -> string) -> string -> 'a list -> string

val take : int * 'a list -> 'a list
val drop : int * 'a list -> 'a list
val delete_nth : int -> 'a list -> 'a list
val replace_nth : int -> 'a list -> 'a -> 'a list
val insert : ('a -> 'a -> bool) -> 'a -> 'b -> ('a * 'b) list
  -> ('a * 'b) list
val replace : 'a -> 'b -> ('a*'b) list -> ('a*'b) list
val wrap_around: 'a list -> int -> int
val get_nth: 'a list -> int -> 'a
val splice_nth : int -> 'a list -> 'a list -> 'a list
val move_right : 'a list * 'a list -> 'a list * 'a list
val move_left : 'a list * 'a list -> 'a list * 'a list
val index: ('a -> bool) -> 'a list -> int
val filter: ('a -> bool) -> 'a list -> 'a list
val assocp: ('a -> bool) -> ('a* 'b) list -> 'b

type ('a, 'b) substype = ('a, 'b) Hashtbl.t
val empty_env : unit -> ('a, 'b) substype
val env_size : int -> ('a, 'b) substype
val find : 'a -> ('a, 'b) substype -> 'b
val bind_env : 'a -> 'b -> ('a, 'b) substype -> unit
val bind : 'a -> 'b -> ('a, 'b) substype -> ('a, 'b) substype
val add : 'a -> 'b -> ('a, 'b) substype -> 'b
val chase : ('a -> bool) -> 'a -> ('a, 'a) substype -> 'a
val fullchase : ('a -> bool) -> 'a -> ('a, 'a) substype -> 'a
val member : 'a -> ('a, 'b)substype -> bool
val remove: 'a -> ('a, 'b)substype -> unit
val remove_dups: 'a list -> 'a list

val table_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
(** Convert from a table to a list. *)

val table_from_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
(** Convert from a list to a table. *)

val find_char : char -> int -> string -> int
(** [find_char c max str]: Find the the first index of character [c]
    in string [str], returning [max] if not found.. *)

val chop_at : char -> string -> string * string
(** [chop_at c str]: Cut [str] into pair of strings [(x, y)] with [x] the
    substring before char [c], [y] the substring after [c] if no char [c] in
    [str], then [x] is the empty string.
*)

val int_to_name: int -> string
(** [int_to_name i]: convert i to a string, with
    0 -> a, 25-> z, 26 -> a1, etc.
*)

(**
   Named Lists

   Lists in which elements are named and data can be
   added by relative position.

   Note that it is better to use association lists functions for
   operations other than add.
*)
type ('a, 'b)named_list = ('a * 'b) list

(** Relative position markers *)
type ('a)position =
    First | Last | Before of 'a | After of 'a | Level of 'a


val named_add:
  ('a, 'b)named_list->('a)position
  -> 'a -> 'b -> ('a, 'b) named_list
(** [named_add l p n x]:
    add [(n, x)] to named list [l] at position [p].
*)

val from_option : 'a option -> 'a -> 'a
(** [from_option a b]: if [a] is [Some(x)], return [x] otherwise return
    [b]. *)

val from_some: ('a)option -> 'a
(** [from_some a]: if [a] is [Some(x)], returns [x] otherwise fails with an
    exception. *)

val set_option:  'a option ref -> 'a -> unit
(**
   [set_option x d]:
   If [x] is [Some(y)] then [y:=d].
*)

val dest_option: ?err:exn -> 'a option -> 'a
(** [dest_option ?err x]: If [x] is [Some(y)] then [y], otherwise
    raise [err] or [Failure].
*)

val set_int_option : int -> int option
val get_int_option : int option -> int
val compare_int_option: int option -> int -> bool
val dec_int_option: int option -> int option

val apply_option: ('a -> 'b) -> 'a option -> 'b -> 'b
(** [apply_option f a d]: Apply to [f] to option [a].  If [a] is [Some
    i] then return [f i] else return [d].
*)

val date: unit -> float
(** [date]: Get the current date. *)

val nice_date: float -> (int * int * int * int * int)
(** [nice_date f]: Return date [f] in form [(year, month, day, hour,
    min)].
*)

val get_one : 'a list -> exn -> 'a
(**
   [get_one l e]: Get first element of list [l].

   @raise exception [e] if [l] is empty.
*)

val get_two : 'a list -> exn -> ('a * 'a)
(**
   [get_two l e]: Get first two elements of list [l].

   @raise exception [e] if length of [l] < 2.
*)

val split_at_index : int -> 'a list -> ('a list * 'a list)
(**
   [split_at_index i x]: Split [x] into [(l, r)] so that
   [x=List.append l r] and [List.hd r] is the [i]th element of [x]
   (counting from [0]).

   @raise Not_found if [i] >= [length x].
*)

val full_split_at: ('a -> bool) -> 'a list -> ('a list * 'a * 'a list)
(**
   [split_at p x]: Split [x] into [(l, c, r)] so that
   [x=List.rev_append l (c::r)] and [c] is element satisfying [p].

   @raise Not_found if no element of [x] satisifies the condition.
*)

val full_split_at_index: int -> 'a list -> ('a list * 'a * 'a list)
(**
   [split_at_index i x]: Split [x] into [(l, c, r)] so that
   [x=List.rev_append l (c::r)] and [c] is the [i]th element of [x]
   (counting from [0]).

   @raise Not_found if [i] >= [length x].
*)

val rotate_left : int -> 'a list -> 'a list
(**
   [rotate_left n l]: Rotate list [l] [n] places left.
*)

val rotate_right : int -> 'a list -> 'a list
(**
   [rotate_right n l]: Rotate list [l] [n] places right.
*)

val apply_nth : int -> ('a -> 'b) -> 'a list -> 'b -> 'b
(**
    [apply_nth n f l d]: Apply [f] to [n]th element of list.
    If list [l] is empty, return [d].
*)

(***
val fold_map :
  ('a -> 'b -> ('a * 'c )) -> 'a -> 'b list -> ('a * 'c list)
(** Combined fold and map. [fold_map f a [b1; ... ; bn]] is [ (fst(f
    (std f a bn-1) bn), [snd(f a b1); snd(f (fst f a b1) b2) ; ... ;
    snd(f (fstf a bn-1) bn)])].
*)
***)

val swap: ('a * 'b) -> ('b * 'a)
(** [swap (a, b)] is [(b, a)] *)

val map_find: ('a -> 'b) -> 'a list -> 'b list
(**
   [map_find f l]: map function [f] to list [l]. Silently discard
   elements for which [f] raises [Not_found].
*)

val extract : ('a -> bool) -> 'a list -> ('a * 'a list)
(**
   [extract p ls]: Extract the first element [x] of [ls] satisfying [p].
   return [(x, ls')], where [ls'] is [ls] with [x] removed.

   @raise [Not_found] if no element of [ls] satisfies [p].
*)

val least: ('a -> 'a -> bool) -> 'a list -> 'a
(**
   [least ord ls]: return least [x] in [ls].

   @raise [Invalid_argument] if [ls] is empty.
*)

val try_find: ('a -> 'b) -> 'a -> 'b option
(**
   [try_find f p]: Return [Some (f p)]. If [(f p)] raises [Not_found],
   return [None].
*)

val try_app: ('a -> 'b) -> 'a -> 'b option
(**
   [try_app f p]: Return [Some (f p)]. If [(f p)] raises an exception,
   return [None]. This is a generalisation of [try_find].
*)

val test_app: ('a -> 'b) -> 'a -> bool
(**
   [test_app f p]: Apply (f p), returning [true] if it succeeds and
   [false] if it raises an exception.
*)

val find_first: ('a -> 'b) -> 'a list -> 'b
(**
   [find_first f lst]: Apply f to each element in the list, returning
   the result of the first application to succeed. An application (f
   x) fails if it raises an exception.
*)

val first: ('a -> bool) -> 'a list -> 'a
(**
   [first p lst]: Return the first element of [lst] for which [p] is true.

   @raise [Not_found] if [p] is false for all elements.
*)

val apply_first : ('a -> 'b) list -> 'a -> 'b
(**
    [apply_first lst x]: Apply each function in [lst], return the
    result of the first to succeed. Fail if all functions in [lst]
    fail.

    @raise Failure on failure.
*)

val apply_flatten: ('a -> ('b list)) -> 'a list -> 'b list
(**
   [apply_flatten f lst]: Apply [f] to each element of [lst],
   concatenating the resulting list of lists.
*)

val apply_split: ('a -> ('b * 'c)) -> 'a list -> (('b list) * ('c list))
(**
   [apply_split f lst]: Apply [f] to each element of [lst],
   splitting the resulting list of pairs.
*)

val list_exists_data:
  ('a -> (bool * ('b)option)) -> ('a)list -> (bool * ('b)option)

(** {5 Sets of strings} *)

module StringSet : Set.S with type elt=string

(** {5 Lazy Evaluation} *)

type ('a)deferred
(**
    The type of lazy evaluation. A deferred function is evaluated once
    and the result stored and used for subsequent evaluation.
*)

val freeze: (unit -> 'a) -> ('a)deferred
(** [freeze fn]: Defer the evaluation of [fn]. *)

val thaw: ?fresh:('a -> bool) -> ('a) deferred -> 'a
(**
    Get the value [x] of the deferred function, evaluating the function
    if necessary or if [fresh x] is false.
*)

val stringify : string -> string
(**
   [stringify str]: Make [str] suitable for passing to OCaml on the
   command line.  Escapes the string using [String.escaped] then
   replaces ' ' with '\ '.
*)
