(*----
  Copyright (c) 2006-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** A minimal base theory, used if no other theory can be found. *)

(** The minimal context needed to build the base theory. Contains the parser
    and printer information needed to declare terms and types. *)
val context: unit -> Context.t


(** [builder save ctxt] Build the minimal theory. If [save] is true, save the
    theory to file *)
val builder: bool -> Context.t -> Context.t
