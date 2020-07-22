(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(*
 * File and directory settings
 *)

let base_dir_var = "HSEQ"

let base_dir = ref Defaults.basedir
let get_base_dir () = !base_dir
let set_base_dir d = base_dir := d

let make_filename dir f =
  let d =
    match dir with
      | Some x -> x
      | _ -> (get_base_dir())
  in
  (d^"/"^f)

let make_directory f = (get_base_dir())^"/"^f

let libs_dir_v = ref Defaults.libdir
let libs_dir () = !libs_dir_v
let set_libs_dir l = libs_dir_v := l

let thys_dir_v = ref Defaults.thydir
let thys_dir () = !thys_dir_v
let set_thys_dir l = thys_dir_v := l

let include_dirs_v = ref [libs_dir(); thys_dir()]
let include_dirs () = !include_dirs_v
let set_include_dirs l = include_dirs_v := l

let init_file = "hseqstart.ml"

(*** File suffixes ***)

let thy_suffix = ".tho"
let script_suffix = "Script.ml"

(*
 * Sequent display
 *)

(** [set_nice_sequent x] if [x] is [true], print sequents with
    assumption indices prefixed by [!nice_sequent_prefix].  if [x] is
    false, print sequent assumption indices as negative numbers.
*)
let nice_sequent_prefix = ref "~"
let nice_sequent = ref true


let long_identifier = ref false
let print_type_level = ref 1

let load_thy_level=ref 0
