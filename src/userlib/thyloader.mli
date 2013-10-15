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


(* builder <whether-to-save> <context> *)
type builder = bool -> Context.t -> Context.t

module LoaderDB :
sig
  type t = 
    {
      (** Search path for theories and build scripts *)
      thy_path_f: string list;
      (** Search path for libraries *)
      lib_path_f: string list;
      (** Builders: indexed by theory name *)
      builders_f: (string, builder)Hashtbl.t;
    }

  (** An empty DB*)
  val empty: unit -> t

  (** Find a builder for a theory name, raising Not_found *)
  val builder: t -> string -> builder
  (** Add a builder for a theory name *)
  val add_builder: t -> string -> builder -> t
  (** Remove a builder for a theory name *)
  val remove_builder: t -> string -> t

  (** Search paths *)
(*
  val thy_path: t -> string list
  val set_thy_path: t -> string list -> t

  val lib_path: t -> string list
  val set_lib_path: t -> string list -> t
*)
end


(** Data to pass to ThyDB loader. *)
module Old: 
sig
  type load_data = 
    {
    (** Record a theory name being loaded *)
      record_name: string -> unit;
    (** Test whether a name has been recorded *)
      seen_name: string -> bool;
    }
end

val set_load_file:
  (string -> unit) -> unit
val get_load_file:
  unit -> (string -> unit)
val set_use_file:
  (?silent:bool -> string -> unit) -> unit
val get_use_file:
  unit -> (?silent:bool -> string -> unit)

val null_thy_fn: 
  Context.t -> Thydb.thydb -> Theory.contents -> unit
val null_load_file: string -> unit
val null_use_file: ?silent:bool -> string -> unit

val default_thy_fn: 
  Context.t -> Thydb.thydb -> Theory.contents -> unit
val default_load_fn: 
  Context.t -> Thydb.Loader.info -> Theory.saved_thy
val default_build_fn: 
  Context.t -> Thydb.thydb -> string -> Thydb.thydb

val default_loader:
  Context.t -> Thydb.Loader.data
val loader_data:
  Context.t -> Thydb.Loader.data
