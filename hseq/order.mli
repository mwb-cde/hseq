(*----
  Name: order.mli
  Copyright Matthew Wahab 2017
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

(** {5 Orderings.} *)

(** Type of orders. *)
type t = LessThan | Equal | GreaterThan

module Util:
sig
  (** [int_to_order i]: Map an integer to an order. The mapping is [<0 ->
      LessThan], [0 -> Equals] and [>0 -> GreaterThan]. *)
  val int_to_order: int -> t

  (** [wrap cmp]: Wrap basic comparison function [cmp], making it return an
    [Order.t]. The mapping is [<0 -> LessThan], [=0 -> Equals] and [>0 ->
    GreaterThan]. *)
  val wrap: ('a -> 'a -> int) -> 'a -> 'a -> t

  (** [compare x y]: This is [wrap Pervasives.compare x y]. *)
  val compare: 'a -> 'a -> t
end
