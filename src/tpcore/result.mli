(*-----
 Name: result.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* Error and message construction and reporting *)
(* errors and messages are objects, allowing new errors to be derived *)
(* an error raised as an exception with  a list of one or more objects *)

type kind (* kind=int *)
val mk_new_kind : unit -> kind
val errorkind: kind

class message :
    string ->
      object 
	method msg : unit -> string 
	method print : Printer.ppinfo -> unit 
      end
class error :
    string ->
      object 
	method msg : unit -> string 
	method print : Printer.ppinfo -> unit 
      end
class errormsg :
  object 
    method msg : unit -> string 
    method print : Printer.ppinfo -> unit 
  end

exception Error of error
exception Errors of exn list

(* construct, add and raise errors *)
val mk_error : error -> exn
val add_error : exn -> exn -> exn

val catch_error : Printer.ppinfo -> int -> ('a -> unit )  -> 'a -> unit

val error : string -> exn

(*
val catchError : error -> exn -> exn
val raiseError : string -> 'a
*)

(* basic printer *)
val print_error :  Printer.ppinfo -> int -> exn -> unit


(* warnings *)
val warning: string -> unit
