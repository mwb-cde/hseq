(**----
   Name: thyloader.mli
   Copyright M Wahab 2013
   Author: M Wahab  <mwb.cde@gmail.com>

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

(** {5 Theory building and loading} *)

(** Data to pass to ThyDB loader. *)

(** Get and set functions to load/use a file *)
val set_load_file:
  (string -> unit) -> unit
val get_load_file:
  unit -> (string -> unit)
val set_use_file:
  (?silent:bool -> string -> unit) -> unit
val get_use_file:
  unit -> (?silent:bool -> string -> unit)

(** Update a context with the functions to load/use a file *)
val set_file_handlers: Context.t -> Context.t

(** Default functions *)
val default_thy_fn: 
  Context.t -> Thydb.thydb -> Theory.contents -> unit
val default_load_fn: 
  Context.t -> Thydb.Loader.info -> Theory.saved_thy
val default_build_fn: 
  Context.t -> Thydb.thydb -> string -> Thydb.thydb

(** Function to build or load a theory *)
val build_fn: Context.t -> Thydb.thydb -> string -> Thydb.thydb
val default_loader: Context.t -> Thydb.Loader.data

val load_file: string -> unit
val script_file: ?silent:bool -> string -> unit

val set_file_handlers: Context.t -> Context.t

(** {5 Debugging} *)
val null_thy_fn: 
  Context.t -> Thydb.thydb -> Theory.contents -> unit
val null_load_file: string -> unit
val null_use_file: ?silent:bool -> string -> unit

(**
val inc_canary: unit -> unit
val canary: unit -> int
val set_canary: int -> unit
**)
