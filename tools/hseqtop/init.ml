(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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


