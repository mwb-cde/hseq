(*----
 Name: init.ml
 Copyright M Wahab 2005-2010
 Author: M Wahab  <mwb.cde@googlemail.com>

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
   Initialising TP for interactive use.

   For Ocaml 3.06:
   Use Toploop.parse_toplevel_phrase to call init()
   then restore Toploop.parse_toplevel_phrase to original (ocaml) value
   once init() has been called.

   For Ocaml 3.07:
   it may be possible to use Toploop.toplevel_startup_hook
   for a rather less tricksy approach
*)

(* Theorem Prover initialising functions *)

(**
   [tp_init()]: Theorem Prover specific initialisation.
*)
let tp_init() = 
  let tmp = !Settings.load_thy_level
  in 
    Settings.load_thy_level := 0;
    Userlib.init();
    Userlib.set_load_file_func Unsafe.load_file;
    Userlib.set_use_file_func Unsafe.use_file;
    Settings.load_thy_level := tmp

(**
   [init()]:  Start up function, called when system first begins.
   Initialise TP and load the startup file.
*)


(** [set_hooks()]: Set the file-handling hooks
    
    [Global.Hooks.load_file := Unsafe.load_file]
    
    [Global.Hooks.use_file := Unsafe.use_file]
*)
let set_hooks() = 
  Userlib.set_load_file_func (Unsafe.load_file);
  Userlib.set_use_file_func (Unsafe.use_file)
      
(** [set_base_dir()]: Get the installation directory *)
let set_base_dir()=
    try 
      let d = Sys.getenv Settings.base_dir_var 
      in Settings.set_base_dir d
    with Not_found -> ()

(***
* Set OCaml toplevel search path
***)

(** 
    [set_directorys()]: Add tp directories to the system search path.
*)
let set_directorys ()=
  List.iter Unsafe.add_directory (!Settings.include_dirs);
  Unsafe.add_directory (Settings.libs_dir())
  
(**
   [starting_mesg()]: Print a Start Up message.
*)
let starting_mesg()=
  Format.printf "@[\tHSeq (%s)\n@]@." Defaults.version

let load_init () = 
  let initfile=
    Settings.make_filename ~dir:(Settings.libs_dir()) Settings.init_file
  in
  if (Sys.file_exists initfile)
  then Unsafe.use_file ~silent:false initfile
  else
    Report.warning ("Can't find initialising file "^initfile)

let init() = 
  starting_mesg(); 
  tp_init()

let init_commands = 
  [
    "open HSeq";
    "open Goals";
    "open Tactics";
    "open Boollib";
    "open Simplib";
    "open Userlib";
  ];;

let run_command c = 
  try Unsafe.use_string (c^";;") 
  with _ -> ()

let run_commands () = 
  List.iter run_command init_commands

let new_add_init f =
  let ocaml_init = !Toploop.toplevel_startup_hook
  in 
  let startup () = (f(); ocaml_init(); tp_init())
  in 
  Toploop.toplevel_startup_hook:=startup


(*** The code to run when this module is loaded. **)  
let _ = 
  set_base_dir();
  set_directorys();
  set_hooks();
  Unsafe.add_init load_init;
  init()
(**
  run_commands();
  init();
  new_add_init load_init;
**)
