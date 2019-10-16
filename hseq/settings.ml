(*----
  Name: settings.ml
  Copyright Matthew Wahab 2005-2019
  Author: Matthew Wahab <mwb.cde@gmail.com.com>

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
