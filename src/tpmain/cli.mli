(* simple user interface *)

(* saves list of input strings which make a change to the sequent *)

val use_string : string -> unit 

  val start: unit -> unit  (* start recording input strings *)
  val stop : unit -> unit   (* stop recording *)
  val restart: unit -> unit  (* continue recording input strings *)


  val repl : unit -> unit   (* read-eval-print loop *)

  val clear: unit -> unit   (* clear the list of strings *)

  val print : unit -> unit  (* print list of string to the screen *)
  val save : string -> unit (* append list to the named file *)
