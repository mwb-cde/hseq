(**----
   Name: thyloader.ml
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

(***
(* builder <whether-to-save> <context> *)
type builder = bool -> Context.t -> Context.t

type load_data = Thydb.Loader.data

module LoaderDB = 
struct
  type t = 
    {
      (** Search path for theories and build scripts *)
      thy_path_f: string list;
      (** Search path for libraries *)
      lib_path_f: string list;
      (** Builders: indexed by theory name *)
      builders_f: (string, builder)Hashtbl.t;
    }

  let empty() =
    {
      thy_path_f = [];
      lib_path_f = [];
      builders_f = Hashtbl.create(13);
    }

  let builder db n = 
    try Hashtbl.find db.builders_f n
    with _ -> raise Not_found

  let add_builder db (n:string) b = 
    Hashtbl.add db.builders_f n b;
    db

  let remove_builder db n = 
    Hashtbl.remove db.builders_f n; 
    db

  (* Search paths *)
  let thy_path db = db.thy_path_f
  let set_thy_path x y = 
    {x with thy_path_f = y;}

  let lib_path db = (db.lib_path_f)
  let set_lib_path db pl = 
    { db with lib_path_f = pl }
end

module Old =
struct 
(** Data to pass to ThyDB loader. *)
  type load_data = 
    {
    (** Record a theory name being loaded *)
      record_name: string -> unit;
    (** Test whether a name has been recorded *)
      seen_name: string -> bool;
    }
end
*)
let null_thy_fn 
    (ctxt: Context.t) (db: Thydb.thydb) (thy: Theory.contents) =
  raise (Failure ("Thyloader.default_thy_fn("^(thy.Theory.cname)^")"))
     
let null_load_file (fname: string) =
  raise (Failure ("Thyloader.null_load_file("^fname^")"))

let null_use_file ?silent:bool (fname: string) = 
  raise (Failure ("Thyloader.null_use_file"^fname^")"))

module Var =
struct
  let load_file = ref null_load_file
  let use_file = ref null_use_file
end

let get_load_file() = !(Var.load_file)
let set_load_file f = Var.load_file := f
let get_use_file() = !(Var.use_file)
let set_use_file f = Var.use_file := f

let build_fn
    (ctxt: Context.t) (db: Thydb.thydb) (thyname: string) =
  let scripter = get_use_file() in
  let script_name = Context.Files.script_of_thy ctxt thyname in
  let saved_state = Userstate.state() in
  let st1 = Userstate.set_context saved_state ctxt in
  begin
    Userstate.set_state st1;
    scripter ~silent:false script_name;
    let st2 = Userstate.state() in
    Context.thydb (Userstate.context st2)
  end

let default_build_fn 
    (ctxt: Context.t) (db: Thydb.thydb) (thyname: string) =
  if (thyname = Lterm.base_thy)
  then Context.Thys.theories (BaseTheory.builder ctxt)
  else raise (Failure ("Thyloader.default_load_fn("^thyname^")"))

let default_load_fn 
    (ctxt: Context.t) (file_data: Thydb.Loader.info) =
  Context.Files.load_thy_file ctxt file_data

let default_thy_fn 
    (ctxt: Context.t) (db: Thydb.thydb) (thy: Theory.contents) =
  ()


let default_loader ctxt = 
  Thydb.Loader.mk_data 
    (default_thy_fn ctxt)
    (default_load_fn ctxt)
    (build_fn ctxt)

let load_file fname = (get_load_file()) fname
let script_file ?(silent=false) fname = 
  (get_use_file()) ~silent fname

let set_file_handlers ctxt = 
  let ctxt1 = Context.set_loader ctxt load_file  in
  let ctxt2 = Context.set_scripter ctxt1 script_file in
  ctxt2
