(*----
  Name: unsafe.mli
  Copyright Matthew Wahab 2005-2010
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

(**
  Functions which depend on unsafe/undocumented features of OCaml.
*)

val use_string: string -> unit
(**
   [use_string str]: Evaluate string [str] as an ML expression.
   Raises [Failure] if evaluating [str] fails for any reason.
*)

val use_file : bool -> string -> unit
(**
   [use_file silent name]: Use file [name] as a script. Equivalent to
   [#use name].  If [silent] is [true], no output, otherwise emits
   output like [#use].
*)

val load_file : string -> unit
(**
   [load_file name]: Load file [name] as a script. Equivalent to
   [#load name].
*)

val add_directory : string -> unit
(**
   [add_directory s]: Add directory [s] to the OCaml path.
   Equivalent to [#directory s].
*)

val add_init: (unit -> unit) -> unit
(**
   [add_init f]:  Setup function [f] to be called when OCaml starts.
   [f] is typically an initialisation function.
*)
