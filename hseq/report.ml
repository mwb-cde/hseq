(*----
  Name: report.ml
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

open Format

(* Objects for reporting information *)

(*
 * Exceptions for reporting errors
 *)

(** A single error. *)
type error = Format.formatter ->Printer.ppinfo -> unit
exception Error of error
let mk_error (e: error) = Error e

(** A list of errors. *)
exception Errors of exn list

(** Construct an error. *)

(** Add an error to a list of errors. *)
let add_error e x =
  match x with
  | Errors es -> Errors(e::es)
  | _ -> Errors[e; x]

(** [print_error info depth err]: Print the first [depth] errors from
    the exception.
*)
let print_error info depth errs =
  let rec print_aux ctr x =
    if ctr = 1
    then ()
    else
      begin
        match x with
        | (Error e) ->
           begin
             Format.fprintf Format.std_formatter "@[";
             e std_formatter info;
             Format.printf "@]@,"
           end
        | (Errors errs) ->
           begin
             match errs with
             | [] -> ()
             | (e::es) ->
                (ignore(print_aux (ctr - 1) e);
                 print_aux (ctr - 1) (Errors es))
           end
        | _ -> Format.printf "@[%s@]@," (Printexc.to_string x)
      end
  in
  Format.printf "@[<v>";
  List.iter (print_aux (depth + 1)) [errs];
  Format.printf "@]"

(** [catch_error info depth f a]: Apply [f a], catching any error and
    printing it with [print_error].
*)
let catch_error info depth f a =
  try (f a)
  with x -> print_error info depth x

let error_of_str (str: string) (fmt: Format.formatter) (inf: Printer.ppinfo) =
  Format.fprintf fmt "@[error: %s@]@." str

let error s = Error (error_of_str s)

let warning str =
  Format.fprintf std_formatter "@[warning: @[%s@]@]@." str

let report str =
  Format.fprintf std_formatter "@[%s@]@." str
