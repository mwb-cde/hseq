(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(**
   Create theory "Main"
   Theory Main is the top-level theory (the "base theory") for the
   standard theory library.  It is imported into all theories (except
   those in the standard library).

   To avoid having theory Main imported into the standard theories,
   the base theory name is zero'd out while the standard library is built.

   This script is called 0MainScript to avoid it being called from the
   theories that Main depends on.
*)

(* Clear the base theory name. *)
let mainScript_base_name =
  let str =
    try Context.base_name(Global.context())
    with _ -> "Main"
  in
  Global.set_context(Context.clear_base_name (Global.context()));
  str;;

(** Build theory Main and the theories it depends on *)
let _ =
  begin_theory
    "Main"
    ["Set"; "Sum"; "Pair"; "Fun"; "Relation"; "Bool"; "base"];;

let _ = end_theory();;
let _ = Display.print_theory (theory "");;

(* Reset the base theory name *)
let _ =
  Global.set_context(Context.set_base_name
                       (Global.context()) mainScript_base_name);;
