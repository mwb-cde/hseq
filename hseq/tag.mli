(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
