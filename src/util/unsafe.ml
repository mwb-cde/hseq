(*-----
 Name: unsafe.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(*
   Utility functions which depend on unsafe/undocumented features.
*)

(* Ocaml 3.07 version
let use_string st =  
  ignore(List.map 
     (Toploop.execute_phrase true Format.std_formatter) 
     ((!Toploop.parse_use_file ) (Lexing.from_string st)))
*)

let use_string st =  
 try
   ignore
     (Toploop.execute_phrase true Format.std_formatter 
     ((!Toploop.parse_toplevel_phrase) (Lexing.from_string st)))
 with _ -> failwith ("use_string "^st);;


let use_file ?(silent=false) f = 
  if(silent)
  then ignore(Toploop.use_silently Format.std_formatter f)
  else ignore(Toploop.use_file Format.std_formatter f)

let load_file f = Topdirs.dir_load Format.std_formatter f

let object_suffix = [".cmo"; ".cmi"]

let load_use_file ?silent f=
  if(List.exists (fun x -> Filename.check_suffix f x) object_suffix)
  then load_file f
  else use_file ?silent f


let add_directory s = Topdirs.dir_directory s

(** 
   Functions to add an init function [f] to be called when
   the OCaml toplevel starts. 

   For Ocaml 3.06: Use Toploop.parse_toplevel_phrase to call [f] then
   restore Toploop.parse_toplevel_phrase to original (ocaml) value
   once [f] has been called.

   For Ocaml 3.07: it may be possible to use
   Toploop.toplevel_startup_hook for a rather less tricksy approach
*)


(**
   [add_init_306 f]:

   ocaml-3.06 specific code.

   Use Toploop.parse_toplevel_phrase to call function [f].
   then restore Toploop.parse_toplevel_phrase to original (ocaml) value
   once init() has been called.
*)
let add_init_306 f = 
  let tmp_parse_toplevel_phrase = !Toploop.parse_toplevel_phrase
  in 
   Toploop.parse_toplevel_phrase :=
   (fun l ->
   ignore(f());
   Toploop.parse_toplevel_phrase:=tmp_parse_toplevel_phrase;
   tmp_parse_toplevel_phrase l)


(* 
   [add_init_306 f]:

   ocaml-3.07 specific code.

   Call init() after all other OCaml initialisation has been done.
*)
let add_init_307 f =  
  let ocaml_init = !Toploop.toplevel_startup_hook
  in 
  let startup () = (ocaml_init(); f())
  in 
  Toploop.toplevel_startup_hook:=startup


(**
   [add_init f]:
   Setup function [f] to be called when OCaml starts.
*)
let add_init f = add_init_306 f
