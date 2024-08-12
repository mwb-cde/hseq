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

let use_file silent fname =
  if silent
  then ignore(Toploop.use_silently Format.std_formatter (Toploop.File(fname)))
  else ignore(Toploop.use_input Format.std_formatter (Toploop.File(fname)))

(* Dynlink.loadfile causes a segmentation fault in some circumstances. *)
let load_file f =
  if (!Sys.interactive)
  then ignore (Topdirs.dir_load Format.std_formatter f)
  else
    try Dynlink.loadfile f
    with Dynlink.Error(Dynlink.Module_already_loaded(_)) -> ()
    | e -> raise e

let add_directory s = Topdirs.dir_directory s

(** [add_init f]: Setup function [f] to be called when OCaml
    starts up.
*)
let add_init f =
  let module Persistent_signature = Persistent_env.Persistent_signature in
  let toplevel_hook = (function
    | Toploop.After_setup -> f()
    | _ -> ())
  in
  Toploop.add_hook toplevel_hook

