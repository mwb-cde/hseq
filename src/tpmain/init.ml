
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

let tp_init() = 
  Tpenv.init();
  Tpenv.add_thy_path "thys";
  Tpenv.add_thy_path "../thys"

let init() = 
  let initfile=Settings.make_filename Settings.init_file
  in
  tp_init();
  Toploop.use_file
    Format.std_formatter initfile

let tmp_parse_toplevel_phrase = !Toploop.parse_toplevel_phrase

let _ = 
   Toploop.parse_toplevel_phrase :=
   (fun l ->
   ignore(init());
   Toploop.parse_toplevel_phrase:=tmp_parse_toplevel_phrase;
   tmp_parse_toplevel_phrase l)
  
