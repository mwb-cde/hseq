(*----
  Name: ident.mli
  Copyright Matthew Wahab 2005-2017
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
    Identifiers for functions and types

    An identifier is made up of a theory identifier and a name. A {e
    short} identifier is an identifier without a theory identifier
    (equivalently, the theory identifier is null).
*)

type thy_id = string
(** The type of theory identifiers *)

type t = (thy_id * string)
(** General, qualified identifiers. Made up of a theory identifier and
    name.
*)

val null_thy: thy_id
(** The empty theory identifier *)

val null: t
(** The empty identifier *)

val is_null: t -> bool
(** test for the empty identifier *)

val is_short: t -> bool
(** [is_short i] is true if [i] is a short identifier (having an empty
    theory part).
*)

(** {7 Constructors} *)

val mk_long: thy_id -> string -> t
(** [mk_long t n] makes a long identifier with theory part [t] and
    name part [n].
*)

val mk_name: string -> t
(** Make an identifier with an empty theory part. These are called
    short identifiers.
*)

(** {7 Destructors} *)

val dest: t -> (thy_id * string)
(** [dest id]: Destructor for identifiers. Returns [(th, n)] where
    [th] is the theory identifier and [n] is the name.
*)

val thy_of : t -> thy_id
(** The theory identifier of long identifier [i]. *)

val name_of : t -> string
(** The name portion of identifier [i]. *)

val compare: t -> t -> Order.t
(** Total order on identifiers.
    This is a pair-wise ordering with the [thy_id] the major component.
*)

val equals: t -> t -> bool
val lessthan: t -> t -> bool

(** {7 Utility functions} *)

val string_of: t -> string
(** String representation of identifier [i]. *)

(** Maps indexed by identifiers *)
module Map: (Map.S with type key = t)
type ('a)map = ('a)Map.t

(** Sets indexed by identifiers *)
module Set: (Set.S with type elt = t)
type set = Set.t
