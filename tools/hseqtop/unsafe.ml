(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
let load_file f =
  if (!Sys.interactive)
  then ignore (Topdirs.dir_load Format.std_formatter f)
  else Dynlink.loadfile f

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
