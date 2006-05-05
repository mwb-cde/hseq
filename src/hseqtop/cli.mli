(*-----
 Name: cli.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** Simple command line interface.

   Provides basic support for recording the commands used to prove a
   goal.

   Call {!Cli.clear} to start recording, {!Cli.stop} to stop
   recording, {!Cli.restart} to restart recording. To print the list
   of commands, call {!Cli.print} and to get the commands as a list of
   strings, {!Cli.get}.
*)

val clear: unit -> unit   
(** Clear recording, erasing any recorded input. *)

val get: unit -> string list
(** Get the list of recorded commands, in order of entry. *)

val start: unit -> unit  
(** Start recording, erasing any previously recorded input. *)

val stop : unit -> unit   
(** Stop recording. *)

val restart: unit -> unit  
(** Restart recording, preserving any previously recorded input. *)

val print : unit -> unit  
(** Print the list of recorded commands. *)

(** {5 Debugging information} *)

val repl : unit -> unit   (* read-eval-print loop *)


(*
val save : string -> unit 
(** Append the list recorded commands to a named file. *)
*)



