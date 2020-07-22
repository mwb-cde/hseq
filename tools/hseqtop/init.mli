(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

open HSeq

(**
   System initialisation for interactive use.

   {ul
   {- Set the base directory: if {!Settings.base_dir_var} is set,
   uses that as the base directory}
   {- Add HSeq system directories {!Settings.include_dirs} and
   {!Settings.libs_dir} to OCaml search directories}
   {- Set the file-handling hook {!Global.Hooks.load_file} to
   {!Unsafe.load_file} and {!Global.Hooks.use_file} to
   {!Unsafe.use_file}}
   {- Set {!Init.load_init} to run when the OCaml toplevel has
   completed its' own initialisation.}
   }

   Uses [Toploop.parse_toplevel_phrase] to call {!Init.init}
   then restores Toploop.parse_toplevel_phrase to original (ocaml)
   value once {!Init.init} has been called.
   *)

val load_init_file: unit -> unit
(**
   [load_init()]: Load the initialising file named
   {!Settings.init_file}, found in directory {!Settings.libs_dir}.
*)

val init: unit -> unit
(**
   [init()]: Initialise the system. This is should be called by the
   initialising file loaded by [load_init()].
*)
