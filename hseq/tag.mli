(*----
  Name: tag.mli
  Copyright Matthew Wahab 2005-2016
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

(**
   Unique identifiers


   Generate, compare and optionally name [tags] which can be used as
   unique identifiers.
*)

type tag_type
type t = tag_type
(** The type of unique identifiers. *)

val create: unit -> t
(** [create]: Make a new, unnamed tag. The tag is guarenteed to be
    unique w.r.t function [Tag.equal]. *)

val named: string -> t
(** [named s]: Make a new tag named [s]. The tag is guarenteed to be
    unique w.r.t function [Tag.equal]. *)

val equal: t -> t -> bool
(** [equal]: Compare tags. [equal t1 t2] is true iff [t1] and [t2] are
    created by the same invocation of [create] or [named]. *)

val name: t -> string
(** [name x]: Get the name of tag [x]. *)

val null: t
(** [null]: A constant, unnamed tag. [equal null null] is always true. *)
