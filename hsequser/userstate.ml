(*----
  Copyright (c) 2013-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
  let printers () = Printers.empty_ppinfo()

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
      simpset_f : (Simpset.simpset)option;
      proofstack_f: (Goals.ProofStack.t)option;
      base_thy_builder_f: (t -> t)option;
      thyset_f: (Lib.StringSet.t)option;
      loader_f: (string -> unit) option;
      scripter_f: (bool -> string -> unit) option;
    }

  (** Initializer *)
  let empty() =
    {
      context_f = Default.context();
      simpset_f = None;
      proofstack_f = None;
      base_thy_builder_f = None;
      thyset_f = None;
      loader_f = None;
      scripter_f = None;
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

  let has_simpset st = Option.is_some (st.simpset_f)
  let simpset st = Option.get (st.simpset_f)
  let set_simpset st s =
    { st with simpset_f = Some(s) }

  let has_proofstack st = Option.is_some (st.proofstack_f)
  let proofstack st = Option.get (st.proofstack_f)
  let set_proofstack st s =
    { st with proofstack_f = Some(s) }

  let has_base_thy_builder st = Option.is_some (st.base_thy_builder_f)
  let base_thy_builder st = Option.get (st.base_thy_builder_f)
  let set_base_thy_builder st f =
    { st with base_thy_builder_f = Some(f) }

  let has_thyset st = Option.is_some (st.thyset_f)
  let thyset st = Option.get (st.thyset_f)
  let set_thyset st s = { st with thyset_f = Some(s) }

  let has_loader st = Option.is_some (st.loader_f)
  let loader st = st.loader_f
  let set_loader st ld =
    { st with loader_f = ld }

  let has_scripter st = Option.is_some (st.scripter_f)
  let scripter st = st.scripter_f
  let set_scripter st ld =
    { st with scripter_f = ld }
end

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
let has_simpset = State.has_simpset
let simpset = State.simpset
let set_simpset = State.set_simpset

(** The proofstack *)
let has_proofstack = State.has_proofstack
let proofstack = State.proofstack
let set_proofstack = State.set_proofstack

(** The base theory builder *)
let has_base_thy_builder = State.has_base_thy_builder
let base_thy_builder = State.base_thy_builder
let set_base_thy_builder = State.set_base_thy_builder

(** Theory set *)
let has_thyset = State.has_thyset
let thyset = State.thyset
let set_thyset = State.set_thyset

let thyset_add st t =
  let set1 = Lib.StringSet.add t (thyset st) in
  set_thyset st set1
let thyset_mem st t =
  Lib.StringSet.mem t (thyset st)

let has_loader = State.has_loader
let loader st =
  if has_loader st
  then Option.get (State.loader st)
  else (fun _ -> ())
let set_loader = State.set_loader

let has_scripter = State.has_scripter
let scripter st =
  if has_scripter st
  then Option.get (State.scripter st)
  else (fun silent _ -> ())
let set_scripter st f = State.set_scripter st f

module Init =
struct

(** State initializer *)
  let init_context st =
    let ctxt0 = Default.context() in
    let ctxt1 = Context.set_path ctxt0 (Settings.include_dirs()) in
    set_context st ctxt1

  let init_loader st = State.set_loader st None
  let init_scripter st = State.set_scripter st None

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
    List.fold_left
      (fun a f -> f a) (State.empty())
      [init_context; init_loader; init_scripter;
       init_scope; init_ppinfo; init_parsers;
       init_simpset; init_proofstack; init_base_thy_builder]

  let reset = init
end

let init_context = Init.init_context
let init_scope = Init.init_scope
let init_loader = Init.init_loader
let init_scripter = Init.init_scripter
let init_ppinfo = Init.init_ppinfo
let init_parsers = Init.init_parsers
let init_simpset = Init.init_simpset
let init_proofstack =  Init.init_proofstack
let init_base_thy_builder = Init.init_base_thy_builder
let init = Init.init
let reset = Init.reset
