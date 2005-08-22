(*-----
 Name: init.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)


(**
   System initialisation.

   With Ocaml 3.06, uses Toploop.parse_toplevel_phrase to call init()
   then restores Toploop.parse_toplevel_phrase to original (ocaml)
   value once init() has been called.

   Note that for Ocaml 3.07 it may be possible to use
   Toploop.toplevel_startup_hook for a rather less tricksy approach.
*)


val load_init: unit -> unit
(**
   [load_init()]: Load the initialising file.
*)

val init: unit -> unit
(** 
   [init()]: Initialise the system. This is normally called from the
   initialising file loaded by [load_init()].
*)

