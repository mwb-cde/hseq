(**----
   Name: userlib.ml
   Copyright M Wahab 2013-2014
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

open HSeq

(** {5 Default values} *)
module Default =
struct

  let context() = 
    let ctxt = BaseTheory.context() in
    let ctxt1 = Context.set_obj_suffix ctxt [".cmo"; "cmi"] in
    let ctxt2 = Context.set_script_suffix ctxt1 (Settings.script_suffix) in
    let ctxt3 = Context.set_thy_suffix ctxt2 (Settings.thy_suffix) in  
    let ctxt4 = Context.set_base_name ctxt3 (Context.Default.base_thy_name) in  
    ctxt4

  let scope () = Thydb.mk_scope(Context.thydb(context()))

  (* Printer tables *)
  let printers () = Printer.empty_ppinfo()

  (* Parser tables *)
  let parsers () = BoolPP.init_bool_parsers (Parser.init ())

  (* Simplifier set *)
  let simpset() = Simplib.init_std_ss()

  (* Proof stack *)
  let proofstack() = Goals.ProofStack.empty()

  (* base_thy_builder *)
  let base_thy_builder () = (fun x -> x)

  (* Theory set *)
  let thyset() = Lib.StringSet.empty
end

(** {5 Global state} *)
module State = 
struct 
  type t =
    {
      context_f : Context.t;
      simpset_f : Simpset.simpset;
      proofstack_f: Goals.ProofStack.t; 
      base_thy_builder_f: t -> t;
      thyset_f: Lib.StringSet.t;
    }

    (** Initializer *)
  let empty() = 
    {
      context_f = Default.context();
      simpset_f = Default.simpset();
      proofstack_f = Default.proofstack();
      base_thy_builder_f = Default.base_thy_builder();
      thyset_f = Default.thyset();
    }

  let context st = st.context_f
  let set_context st ctxt = 
    { st with context_f = ctxt }

  let scope st = Context.scope_of (context st)
  let set_scope st scp = 
    set_context st (Context.set_scope (context st) scp)

  let ppinfo st = Context.ppinfo (context st)
  let set_ppinfo st pp = 
    let ctxt = context st in
    set_context st (Context.set_ppinfo ctxt pp)

  let parsers st = Context.parsers (context st)
  let set_parsers st pp = 
    set_context st (Context.set_parsers (context st) pp)

  let simpset st = st.simpset_f
  let set_simpset st s = 
    { st with simpset_f = s }

  let proofstack st = st.proofstack_f
  let set_proofstack st s = 
    { st with proofstack_f = s }

  let base_thy_builder st = st.base_thy_builder_f
  let set_base_thy_builder st f = 
    { st with base_thy_builder_f = f }

  let thyset st = st.thyset_f
  let set_thyset st s = { st with thyset_f = s }

end

(**
(** {5 Variables } *)
module Var = 
struct
  let (state_v: State.t ref) = ref (State.empty())

  let state () = !state_v
  let set st = state_v := st
  let init() = set (State.empty())
end
  ***)

(****
(** The global state *)
let state = Var.state
let set_state = Var.set
let init_state = Var.init
***)


(** The global context *)
let context = State.context
let set_context = State.set_context

(** The global scope *)
let scope = State.scope
let set_scope = State.set_scope

(** Global pretty printer *)
let ppinfo = State.ppinfo
let set_ppinfo = State.set_ppinfo

(** Parser tables *)
let parsers = State.parsers
let set_parsers = State.set_parsers

(** The simpset *)
let simpset = State.simpset
let set_simpset = State.set_simpset

(** The proofstack *)
let proofstack = State.proofstack
let set_proofstack = State.set_proofstack

(** The base theory builder *)
let base_thy_builder = State.base_thy_builder
let set_base_thy_builder = State.set_base_thy_builder

(** Theory set *)
let thyset = State.thyset
let set_thyset = State.set_thyset

let thyset_add st t = 
  let set1 = Lib.StringSet.add t (thyset st) in
  set_thyset st set1
let thyset_mem st t =
  Lib.StringSet.mem t (thyset st)

(***
***)

module Init = 
struct

(** State initializer *)
  let init_context st = 
    let ctxt0 = Default.context() in
    let ctxt1 = Context.set_path ctxt0 [Settings.thys_dir()] in
    set_context st ctxt1

(***
    let ctxt1 = Context.set_loader_data ctxt0 Loader.default_loader in
    let ctxt2 = Context.set_load_functions ctxt1 Loader.thy_fn_list in
    let ctxt4 = Loader.set_file_handlers ctxt3 in 
    set_context st ctxt4
***)
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

  let init st = 
    let st1 = 
      List.fold_left (fun a f -> f a) (State.empty())
        [init_context; init_scope;
         init_ppinfo; init_parsers; 
         init_simpset; init_proofstack;
         init_base_thy_builder]
    in
    st1

  let reset = init
end

let init_context = Init.init_context
let init_scope = Init.init_scope
let init_ppinfo = Init.init_ppinfo
let init_parsers = Init.init_parsers
let init_simpset = Init.init_simpset
let init_proofstack =  Init.init_proofstack
let init_base_thy_builder = Init.init_base_thy_builder
let init = Init.init
let reset = Init.reset
