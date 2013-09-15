(*----
  Name: baseTheory.mli
  Copyright M Wahab 2006-2010
  Author: M Wahab  <mwb.cde@googlemail.com>

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

(** A minimal base theory, used if no other theory can be found. *)

val builder: ?save:bool -> Context.t -> Context.t
(** Build the minimal theory. If [?save] is true, save the theory.
    (default: save=false)
*)

(** {7 Initialising function} *)
(*
val init: unit -> unit
*)
(** Set {!Global.Init.set_base_thy_builder} to {!BaseTheory.builder}.
*)

