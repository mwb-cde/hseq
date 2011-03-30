(*----
  Name: settings.ml
  Copyright M Wahab 2005-2009, 2010
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
 * File and directory settings
 *)

let base_dir_var = "HSEQ"

let base_dir = ref Defaults.basedir
let get_base_dir () = !base_dir
let set_base_dir d = base_dir := d

let make_filename ?dir f = 
  let d = 
    match dir with
      | None -> (get_base_dir())
      | Some x -> x
  in 
  (d^"/"^f)

let make_directory f = (get_base_dir())^"/"^f

let libs_dir () = Defaults.libdir
let thys_dir () = Defaults.thydir
let include_dirs = ref [libs_dir()]

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

