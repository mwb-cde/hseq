(*-----
 Name: init.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)


(*
   Initialising TP

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
    Settings.load_thy_level:=0;
    Global.init();
    Settings.load_thy_level:=tmp


(**
   [init()]:  Start up function, called when system first begins.
   Initialise TP and load the startup file.
*)

let set_base_dir()=
    try 
      let d = Sys.getenv Settings.base_dir_var 
      in Settings.set_base_dir d
    with Not_found -> ()

(***
* Set OCaml toplevel search path
***)

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
  then Unsafe.use_file initfile
  else
    Result.warning ("Can't find initialising file "^initfile)

let init() = 
  load_init();
  starting_mesg(); 
  tp_init()

(*** The code to run when this module is loaded. **)  
let _ = 
  set_base_dir();
  set_directorys();
  Unsafe.add_init init

