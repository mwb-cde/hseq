(*----
  Name: init.mli
  Copyright Matthew Wahab 2005-2016
  Author: Matthew Wahab <mwb.cde@gmail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under
  the terms of the Lesser GNU General Public License as published by
  the Free Software Foundation; either version 3, or (at your option)
  any later version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
  License for more details.

  You should have received a copy of the Lesser GNU General Public
  License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
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


val load_init: unit -> unit
(**
   [load_init()]: Load the initialising file named
   {!Settings.init_file}, found in directory {!Settings.libs_dir}.
*)

val init: unit -> unit
(**
   [init()]: Initialise the system. This is should be called by the
   initialising file loaded by [load_init()].
*)
