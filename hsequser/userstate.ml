(**----
   Name: userlib.ml
   Copyright Matthew Wahab 2013-2016
   Author: Matthew Wahab  <mwb.cde@gmail.com>

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
      simpset_f : Simpset.simpset;
      proofstack_f: Goals.ProofStack.t;
      base_thy_builder_f: t -> t;
      thyset_f: Lib.StringSet.t;
      loader_f: (string -> unit) option;
      scripter_f: (bool -> string -> unit) option;
    }

  (** Initializer *)
  let empty() =
    {
      context_f = Default.context();
      simpset_f = Default.simpset();
      proofstack_f = Default.proofstack();
      base_thy_builder_f = Default.base_thy_builder();
      thyset_f = Default.thyset();
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

  let loader st = st.loader_f
  let set_loader st ld =
    { st with loader_f = ld }

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

let loader st =
  match State.loader st with
  | None -> (fun _ -> ())
  | Some(f) -> f

let set_loader st f = State.set_loader st (Some(f))

let scripter st =
  match State.scripter st with
  | None -> (fun silent _ -> ())
  | Some(f) -> f
let set_scripter st f = State.set_scripter st (Some(f))

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
