(*----
  Name: userlib.ml
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

open Userstate

(** Simplifier functions *)
let simp_thy_fn ctxt thy = 
  let set = Userstate.Access.simpset() in
  let set1 = Simplib.on_load ctxt set thy in
  begin
    Userstate.Access.set_simpset set1;
    Userstate.Access.context()
  end

(** State initializer *)
let init_context st = 
  let ctxt0 = Default.context() in
  let ctxt1 = Context.set_loader_data ctxt0 Thyloader.default_loader in
  let ctxt2 = Context.add_load_functions ctxt1 [simp_thy_fn]  in
  let ctxt3 = Context.set_path ctxt2 [Settings.thys_dir()] in
  let ctxt4 = Thyloader.set_file_handlers ctxt3 in 
  set_context st ctxt4

let init_scope st = 
  set_scope st (Default.scope())
let init_ppinfo st = 
  let ppinf0 = BoolPP.init_bool_printers (Default.printers()) in
  let ppinf1 = 
    BoolPP.init_bool_ppinfo 
      ppinf0 
      (BoolPP.quote_type_symbols, BoolPP.quote_term_symbols) 
  in 
  set_ppinfo st ppinf1

let init_parsers st = 
  let ptable0 = BoolPP.init_bool_parsers (Parser.init ()) in 
  let ptable1 = 
    BoolPP.init_bool_tokens 
      ptable0 
      (BoolPP.quote_type_symbols, BoolPP.quote_term_symbols)       
  in 
  set_parsers st ptable1

let init_simpset st = 
  set_simpset st (Default.simpset())
let init_proofstack st = 
  set_proofstack st (Default.proofstack())

let init_base_thy_builder st = st

(** {5 Initialising functions} *)

let init () = 
  let st = 
    List.fold_left (fun a f -> f a) (State.empty())
      [init_context; init_scope;
       init_ppinfo; init_parsers; 
       init_simpset; init_proofstack;
       init_base_thy_builder]
  in
  set_state st

let reset() = init()
