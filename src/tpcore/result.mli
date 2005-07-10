(*-----
 Name: result.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(**
   {5 Error and message construction and reporting} 

   Errors and messages are objects, allowing new errors to be derived.
   An error is raised as an exception with  a list of one or more objects.
*)

(** {6 Objects for reporting information} *)

(** Message objects, for reporting non-fatal information *)
class message :
    string ->
      object 
	method msg : unit -> string 
	method print : Printer.ppinfo -> unit 
      end

(** Error objects, for reporting fatal information *)
class error :
    string ->
      object 
	method msg : unit -> string 
	method print : Printer.ppinfo -> unit 
      end

(** {6 Exceptions for reporting errors} *)

exception Error of error
(** A single error. *)

exception Errors of exn list
(** A list of errors *)

val mk_error : error -> exn
(** Construct an error. *)
val add_error : exn -> exn -> exn
(** Add an error to a list of errors. *)

val print_error: Printer.ppinfo -> int -> exn -> unit 
(** 
   [print_error info n err]: Print the first [n] errors
   from exception [err].
*)

val catch_error : Printer.ppinfo -> int -> ('a -> unit ) -> 'a -> unit
(** 
   [catch_error info depth f a]: Apply [f a], catching any error and
   printing it with [print_error].
*)

val error : string -> exn
(** [error msg]: Raise exception [Error] with message [msg] *)

val warning: string -> unit
(** [warning msg]: Print warning message [msg] *)
