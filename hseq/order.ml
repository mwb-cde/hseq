(*----
  Name: order.ml
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

module Util =
struct

  (** [int_to_order i]: Map an integer to an order. The mapping is [<0 ->
      LessThan], [0 -> Equals] and [>0 -> GreaterThan]. *)
  let int_to_order rslt =
    if rslt = 0 then Equal
    else if rslt < 0 then LessThan
    else GreaterThan

  (** [wrap cmp]: Wrap basic comparison function [cmp], making it return an
      [Order.t]. The mapping is [<0 -> LessThan], [=0 -> Equals] and [>0 ->
      GreaterThan]. *)
  let wrap cmp x y = int_to_order (cmp x y)

(** [compare x y]: This is [wrap Pervasives.compare x y]. *)
  let compare x y =
    wrap Pervasives.compare x y

end
