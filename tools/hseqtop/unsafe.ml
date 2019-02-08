(*----
  Name: unsafe.ml
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

(** Utility functions which depend on unsafe/undocumented features. *)

let use_string st =
 try
   ignore (Toploop.execute_phrase true Format.std_formatter
             ((!Toploop.parse_toplevel_phrase) (Lexing.from_string st)))
 with _ -> failwith ("use_string "^st);;


let use_file silent f =
  if silent
  then ignore(Toploop.use_silently Format.std_formatter f)
  else ignore(Toploop.use_file Format.std_formatter f)

(* Dynlink.loadfile causes a segmentation fault in some circumstances. *)
(*let load_file f = Dynlink.loadfile f*)
let load_file f = ignore (Topdirs.load_file Format.std_formatter f)

let add_directory s = Topdirs.dir_directory s

(** [add_init f]: Setup function [f] to be called when OCaml
    starts.

    Use Toploop.parse_toplevel_phrase to call function [f] then
    restore Toploop.parse_toplevel_phrase to original (ocaml) value
    once init() has been called.
*)
let add_init f =
  let ocaml_init = !Toploop.parse_toplevel_phrase
  in
  Toploop.parse_toplevel_phrase :=
    (fun l ->
      ignore(f());
      Toploop.parse_toplevel_phrase:=ocaml_init;
      ocaml_init l)
