(* Error and message construction and reporting *)
(* errors and messages are objects, allowing new errors to be derived *)
(* an error raised as an exception with  a list of one or more objects *)

    class message :
      string ->
      object 
      method msg : unit -> string 
      method print : Printer.info -> unit 
    end
    class error :
      string ->
      object 
      method msg : unit -> string 
      method print : Printer.info -> unit 
    end
    class errormsg :
      object 
      method msg : unit -> string 
      method print : Printer.info -> unit 
    end
    exception Error of error list

(* construct, add and raise errors *)
    val mkError : error -> exn
    val addError : error -> exn -> exn
    val catchError : error -> exn -> exn
    val raiseError : string -> 'a

(* basic printer *)
val print_error : Printer.info -> int -> exn -> unit


(* warnings *)
val warning: string -> unit
