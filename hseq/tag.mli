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

type ('a)t
(** The type of unique identifiers. Each tag can carry an immutable item of
    data. *)

val make: 'a -> ('a) t
(** [make d]: Make a tag, with contents [d]. The tag is guarenteed to be unique
    w.r.t function [compare]. *)

val create: unit -> (string)t
(** [create]: Make a new, unit tag using [make] . *)

(** [named s]: Make a new tag named [s], using [make]. The tag is guarenteed to
    be unique w.r.t function [Tag.equal]. *)

val contents: ('a)t -> 'a
(** [contents tag]: Extract the data from the tag. *)

val named: string -> (string)t
val name: (string)t -> string
(** [name x]: Get the name of tag [x]. *)

val equal: ('a)t -> ('a)t -> bool
(** [equal t1 t2]: Compare tags returning true iff [t1] and
    [t2] are the same (created by the same invocation of [make]).
 *)
