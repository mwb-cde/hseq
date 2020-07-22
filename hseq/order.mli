(*----
  Copyright (c) 2017-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** {5 Orderings.} *)

(** Type of orders. *)
type t = LessThan | Equal | GreaterThan

module Util:
sig
  (** [int_to_order i]: Map an integer to an order. The mapping is [<0 ->
      LessThan], [0 -> Equals] and [>0 -> GreaterThan]. *)
  val int_to_order: int -> t

  (** [order_to_int i]: Map an order to an integer. This is the reverse of
      [int_to_order]. *)
  val order_to_int: t -> int

  (** [wrap cmp]: Wrap basic comparison function [cmp], making it return an
    [Order.t]. The mapping is [<0 -> LessThan], [=0 -> Equals] and [>0 ->
    GreaterThan]. *)
  val wrap: ('a -> 'a -> int) -> 'a -> 'a -> t

  (** [compare x y]: This is [wrap Stdlib.compare x y]. *)
  val compare: 'a -> 'a -> t
end
