(*----
  Name: basic.mli
  Copyright Matthew Wahab 2005-2019
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

(** Basic constants and data structures. *)

open Gtype

(** {5 Base Representation of logic terms} *)

(** {7 Basis of quantified terms} *)

(** Quantifiers for terms. *)
type quant =
  | All | Ex | Lambda
  | Gamma (** Meta-constants *)

val quant_string: quant -> string
(** The string representation of quantifiers. *)

(** {7 Binders} *)

type binders
(** Associating bound variables with their binding term. *)
val mk_binding: quant -> string -> Gtype.t -> binders
(**
    [mk_binding k n ty] makes a binder of kind [k], with name [n] and
    type [ty]. This binder will be distinct from any other under
    [binder_equality].
*)
val dest_binding: binders -> (quant * string * Gtype.t)
(** Destructor for binders. *)

val binder_kind: binders -> quant
(** [binder_kind b]: The kind of binder binding variable [b]. *)

val binder_name: binders -> string
(** [binder_name b]: The name of bound variable [b]. *)

val binder_type: binders -> Gtype.t
(** [binder_type b]: The type of bound variable [b]. *)

val binder_equality: binders -> binders -> bool
(** Equality of binders. *)

val binder_greaterthan: binders -> binders -> bool
val binder_lessthan: binders -> binders -> bool
val binder_compare: binders -> binders -> Order.t
(** Orderings on binders.

    Maintains the invariant:
    - [binder_equality x y]
      =>
      [not (binder_lessthan x y)] and [not (binder_greaterthan x y)].
    - [binder_lessthan x y = not(binder_greaterthan x y)]
*)
