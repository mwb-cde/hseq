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
  then 
    begin
      let ctxt1 = BaseTheory.builder ~save:true ctxt in
      let st1 = Userstate.set_context (Userstate.state()) ctxt1 in
      Context.thydb (Userstate.context st1)
    end
  else build_fn ctxt db thyname
(*
  else raise (Failure ("Thyloader.default_load_fn("^thyname^")"))
*)

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
    (default_build_fn ctxt)

let load_file fname = (get_load_file()) fname
let script_file ?(silent=false) fname = 
  (get_use_file()) ~silent fname

let set_file_handlers ctxt = 
  let ctxt1 = Context.set_loader ctxt load_file  in
  let ctxt2 = Context.set_scripter ctxt1 script_file in
  ctxt2
