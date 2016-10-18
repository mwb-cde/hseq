(*----
  Name: report.ml
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

open Format

(*
 * Objects for reporting information
 *)

(** Message objects, for reporting non-fatal information. *)
class message s=
object (self)
  method msg () = s
  method print (x: Printer.ppinfo) =
    Format.printf "@[%s@]" (self#msg())
end

(** Error objects, for reporting fatal information. *)
class error s =
object
  inherit message s
end

(*
 * Exceptions for reporting errors
 *)

(** A single error. *)
exception Error of error
(** A list of errors. *)
exception Errors of exn list

(** Construct an error. *)
let mk_error e = Error e

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
            Format.printf "@[";
            e#print info;
            Format.printf "@]@,"
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

let error s = Error (new error s)

let warning str =
  Format.printf "@[Warning: @[%s@]@]@." str

let report str =
  Format.printf "@[%s@]@." str
