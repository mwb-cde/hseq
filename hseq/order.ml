(*----
  Copyright (c) 2017-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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

  (** [order_to_int i]: Map an order to an integer. This is the reverse of
      [int_to_order]. *)
  let order_to_int rslt =
    begin
      match rslt with
      | Equal -> 0
      | LessThan -> -1
      | GreaterThan -> 1
    end

  (** [wrap cmp]: Wrap basic comparison function [cmp], making it return an
      [Order.t]. The mapping is [<0 -> LessThan], [=0 -> Equals] and [>0 ->
      GreaterThan]. *)
  let wrap cmp x y = int_to_order (cmp x y)

(** [compare x y]: This is [wrap compare x y]. *)
  let compare x y =
    wrap Stdlib.compare x y

end
