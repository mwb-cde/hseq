(*-----
 Name: init.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)


(*
   Code to initialise TP

   For Ocaml 3.06:
   Use Toploop.parse_toplevel_phrase to call init()
   then restore Toploop.parse_toplevel_phrase to original (ocaml) value
   once init() has been called.

   For Ocaml 3.07:
   it may be possible to use Toploop.toplevel_startup_hook
   for a rather less tricksy approach
*)


(**
   [load_init()]: Load the initialising file.
*)
val load_init: unit -> unit

(* 
   [init()]: Initialise the system. This is normally called from the
   initialising file loaded with [load_init()].
 *)
val init: unit -> unit

