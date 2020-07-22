(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(**
   {5 Error and message reporting}

   Errors and messages are objects, allowing new errors to be derived.
   An error is raised as an exception with  a list of one or more objects.
*)

(** {6 Exceptions for reporting errors} *)
type error_printer = Format.formatter -> Printers.ppinfo -> unit
type error =
  {
    printer: error_printer
  }
exception Error of error

val mk_error: error_printer -> exn
(** Construct an error. *)

type errors = { err: exn; next: (exn)option }
exception Errors of errors

val add_error: exn -> exn -> exn
(** Add an error to a list of errors. *)

val print_error: Printers.ppinfo -> int -> exn -> unit
(** [print_error info n err]: Print the first [n] errors from
    exception [err].
*)

val catch_error: Printers.ppinfo -> int -> ('a -> unit ) -> 'a -> unit
(** [catch_error info depth f a]: Apply [f a], catching any error and
    printing it with [print_error].
*)

(*
val error_of_str: string -> error
(** [error msg]: Raise exception [Error] with message [msg]. *)
 *)

val error: string -> exn
(** [error msg]: Raise exception [Error] with message [msg]. *)

val warning: string -> unit
(** [warning msg]: Print warning message [msg]. *)

val report: string -> unit
(** [report msg]: Print message [msg]. *)
