(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

open Format

(* Objects for reporting information *)

(*
 * Exceptions for reporting errors
 *)

(** A single error. *)
type error_printer = Format.formatter -> Printers.ppinfo -> unit
type error =
  {
    printer: error_printer;
  }
exception Error of error
let mk_error p = Error{ printer = p }

type errors = { err: exn; next: (exn)option }
exception Errors of errors

let add_error e n = Errors ({err = e; next = Some(n) })

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
        | (Error err) ->
           begin
             Format.fprintf Format.std_formatter "@[";
             err.printer std_formatter info;
             Format.printf "@]@,"
           end
        | Errors(errs) ->
           begin
             print_aux ctr (errs.err);
             if errs.next <> None
             then print_aux (ctr - 1) (Lib.from_some errs.next)
             else ()
           end
        | Gtype.Error(err) ->
           begin
             Format.fprintf Format.std_formatter "@[";
             Printers.print_type_error Format.std_formatter info err;
             Format.printf "@]@,";
             if err.Gtype.next <> None
             then print_aux (ctr - 1) (Lib.from_some err.Gtype.next)
             else ()
           end
        | Term.Error(err) ->
           begin
             Format.fprintf Format.std_formatter "@[";
             Printers.print_term_error Format.std_formatter info err;
             Format.printf "@]@,";
             if err.Term.next <> None
             then print_aux (ctr - 1) (Lib.from_some err.Term.next)
             else ()
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

let error_of_str (str: string) =
  (fun fmt inf -> Format.fprintf fmt "@[error: %s@]@." str)

let error s = mk_error(error_of_str s)

let warning str =
  Format.fprintf std_formatter "@[warning: @[%s@]@]@." str

let report str =
  Format.fprintf std_formatter "@[%s@]@." str
