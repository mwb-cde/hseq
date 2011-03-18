(*----
 Name: unsafe.ml
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
  let ocaml_init = !Toploop.parse_toplevel_phrase
  in 
   Toploop.parse_toplevel_phrase :=
     (fun l ->
       ignore(f());
       Toploop.parse_toplevel_phrase:=ocaml_init;
       ocaml_init l)

(* 
   [add_init_309 f]:

   ocaml-3.09 specific code.

   Call init() after all other OCaml initialisation has been done.
*)
let add_init_309 f =  
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
(*
let add_init f = add_init_309 f
*)
