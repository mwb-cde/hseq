
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

(*
   tp_init()
   Theorem Prover specific initialisation.
   
   Initialises the TP environment.
*)

let tp_init() = 
  Tpenv.init()

(*
   init()
   Start up function, called when system first begins.

   Initialise TP and load the startup file.
*)

let init() = 
  let initfile=Settings.make_filename Settings.init_file
  in
  tp_init();
  Toploop.use_file
    Format.std_formatter initfile

(* 
   ocaml-3.06 specific code.

   Use Toploop.parse_toplevel_phrase to call init()
   then restore Toploop.parse_toplevel_phrase to original (ocaml) value
   once init() has been called.
*)


let init_3_06() = 
  let tmp_parse_toplevel_phrase = !Toploop.parse_toplevel_phrase
  in 
   Toploop.parse_toplevel_phrase :=
   (fun l ->
   ignore(init());
   Toploop.parse_toplevel_phrase:=tmp_parse_toplevel_phrase;
   tmp_parse_toplevel_phrase l)


(* 
   ocaml-3.07 specific code.

   init_3_07()
   Call init() after all other OCaml initialisation has been done.
*)

(*
let init_3_07() = 
   let startup () = 
     !Toploop.toplevel_startup_hook(); 
      init()
   in 
   Toploop.toplevel_startup_hook:=startup()
*)
(* 
   starting_mesg().
   Print a Starting Up message.
*)

let starting_mesg()=
  Format.open_box 0;
  Format.print_string "Starting up...";
  Format.close_box()
    

(*
   The function to call when this module is loaded.
*)  

let _ = 
  starting_mesg();
  init_3_06()        (* use ocaml-3.06 code *)
  (* init_3_07() *)   (* use ocaml-3.07 code *)
