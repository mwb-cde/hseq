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
exception Error of error list

(* construct, add and raise errors *)
val mkError : error -> exn
val addError : error -> exn -> exn
val catchError : error -> exn -> exn
val raiseError : string -> 'a

(* basic printer *)
val print_error : Printer.ppinfo -> int -> exn -> unit


(* warnings *)
val warning: string -> unit
