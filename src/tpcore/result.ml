(*-----
 Name: result.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Format

(***
* Objects for reporting information
***)

(** Message objects, for reporting non-fatal information *)
class message s=
  object (self)
    method msg () = s
    method print (x: Printer.ppinfo) = 
      Format.printf "@[%s@]" (self#msg())
  end

(** Error objects, for reporting fatal information *)
class error s =
  object 
    inherit message s
  end

(***
* Exceptions for reporting errors
***)

(** A single error. *)
exception Error of error
(** A list of errors *)
exception Errors of exn list

(** Construct an error. *)
let mk_error e = Error e

(** Add an error to a list of errors. *)
let add_error e x=
  match x with
    Errors es -> Errors(e::es)
  | _ -> Errors[e; x]


(** 
   [print_error info depth err]: Print the first [depth] errors
   from the exception.
*)
let print_error info depth errs=
  let ctr=ref (depth+1)
  in 
  let rec print_aux x=
    if (!ctr)=1 then ()
    else 
      (ctr:=(!ctr)-1;
       match x with
      (Error e) -> 
	Format.printf "@[";
	e#print info;
	Format.printf "@]@,"
    | (Errors l) -> 
	List.iter print_aux l
    | _ -> 
	Format.printf "@[%s@]@," (Printexc.to_string x))
  in 
  Format.printf "@[<v>";
  List.iter print_aux [errs];
  Format.printf "@]"

(** 
   [catch_error info depth f a]: Apply [f a], catching any error and
   printing it with [print_error].
*)
let catch_error info depth f a = 
  try (f a)
  with x -> print_error info depth x

let error s = Error (new error s)

let warning s =
  Format.printf "@[<v>%s@,@]@\n@?" s
