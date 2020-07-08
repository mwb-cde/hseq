(*----
  Name: init.ml
  Copyright Matthew Wahab 2005-2020
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
open HSeqUser
open Userlib

(**
   Initialising TP for interactive use.

   For Ocaml 3.06:
   Use Toploop.parse_toplevel_phrase to call init()
   then restore Toploop.parse_toplevel_phrase to original (ocaml) value
   once init() has been called.
*)

(* Theorem Prover initialising functions *)


(**Set the file-handling functions *)
let set_file_hooks() =
  Userlib.set_load_file_func (Unsafe.load_file);
  Userlib.set_use_file_func (Unsafe.use_file)


(** [set_base_dir()]: Get the installation directory *)
let set_base_dir()=
    try
      let d = Sys.getenv Settings.base_dir_var
      in Settings.set_base_dir d
    with Not_found -> ()

(**
    [set_directorys()]: Add tp directories to the system search path.
*)
let set_directorys () =
  set_base_dir();
  Unsafe.add_directory (Settings.libs_dir())

(**
   [starting_mesg()]: Print a Start Up message.
*)
let starting_mesg() =
  Format.printf "@[\tHSeq (%s)\n@]@." Defaults.version

let load_init_file () =
  let initfile=
    Settings.make_filename (Some(Settings.libs_dir())) Settings.init_file
  in
  if Sys.file_exists initfile
  then Unsafe.use_file false initfile
  else Report.warning ("Can't find initialising file "^initfile)

(**
   [user_state_init()]: Initialise the default HSeq user-state
*)
let user_state_init() =
  let tmp_thy_level = !Settings.load_thy_level
  in
  Settings.load_thy_level := 0;
  Userlib.init();
  Settings.load_thy_level := tmp_thy_level

(**
   Start up function, called when system first begins.
   Initialise TP and load the startup file.
*)
let basic_init() =
  user_state_init();
  set_directorys();
  set_file_hooks();
  load_init_file()

let init() =
  basic_init();
  starting_mesg()

(*** The code to run when this module is loaded. **)
let _ =
  Unsafe.add_init basic_init;
  init()


