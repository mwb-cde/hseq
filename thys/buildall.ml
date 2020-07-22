(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(**
   Build all theories.

   Run from a shell with
   `hseq -I INCLUDEDIR buildall.ml`
   where INCLUDEDIR is the path to the hseq include directory.
   e.g. hseq -I ../include buildall
   or hseq -I HSEQ/include buildall
   where HSEQ is the hseq install directory.
*)

(** Open the theorem prover *)

open HSeq
open HSeqUser
open Userlib
open Userlib.Tactics

(** Include the local configuration file. This is created by the Makefile in
    this directory. It must have value (local_include_dirs:: ('string)list)
    with the list of directories to include in the hseq search path. These
    directories should have the hseq and hsequser libraries and their
    dependencies (other than the OCaml standad libraries).
*)

(** Initialise hseq *)
#use "local_thy_config.ml";;

(** Add the source include directories and reset the system. *)
let _ =
  Settings.set_include_dirs local_include_dirs;
  reset();;

(**
   This is part of the standard library so clear the theory base name.
*)
let buildall_base_name =
  let str =
    try Context.base_name(Global.context())
    with _ -> "Main"
  in
  Global.set_context(Context.clear_base_name (Global.context()));
  str;;

#use "0MainScript.ml";;
