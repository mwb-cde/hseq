(*----
  Name: report.mli
  Copyright Matthew Wahab 2005-2018
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
   {5 Error and message reporting}

   Errors and messages are objects, allowing new errors to be derived.
   An error is raised as an exception with  a list of one or more objects.
*)

(** {6 Exceptions for reporting errors} *)
type error_printer = Format.formatter -> Printer.ppinfo -> unit
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

val print_error: Printer.ppinfo -> int -> exn -> unit
(** [print_error info n err]: Print the first [n] errors from
    exception [err].
*)

val catch_error: Printer.ppinfo -> int -> ('a -> unit ) -> 'a -> unit
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
