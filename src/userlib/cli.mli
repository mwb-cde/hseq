(*-----
 Name: cli.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* simple user interface *)

(* saves list of input strings which make a change to the sequent *)

  val start: unit -> unit  (* start recording input strings *)
  val stop : unit -> unit   (* stop recording *)
  val restart: unit -> unit  (* continue recording input strings *)


  val repl : unit -> unit   (* read-eval-print loop *)

  val clear: unit -> unit   (* clear the list of strings *)

  val print : unit -> unit  (* print list of string to the screen *)
  val save : string -> unit (* append list to the named file *)

