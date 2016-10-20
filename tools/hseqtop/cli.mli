(*----
  Name: cli.mli
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

open HSeq

(** Simple command line interface.

   Provides basic support for recording the commands used to prove a
   goal.

   Call {!Cli.clear} to start recording, {!Cli.stop} to stop
   recording, {!Cli.restart} to restart recording. To print the list
   of commands, call {!Cli.print} and to get the commands as a list of
   strings, {!Cli.get}.
*)

val clear: unit -> unit
(** Clear recording, erasing any recorded input. *)

val get: unit -> string list
(** Get the list of recorded commands, in order of entry. *)

val start: unit -> unit
(** Start recording, erasing any previously recorded input. *)

val stop : unit -> unit
(** Stop recording. *)

val restart: unit -> unit
(** Restart recording, preserving any previously recorded input. *)

val print : unit -> unit
(** Print the list of recorded commands. *)

(** {5 Debugging information} *)

val repl : unit -> unit   (* read-eval-print loop *)


(*
val save : string -> unit
(** Append the list recorded commands to a named file. *)
*)
