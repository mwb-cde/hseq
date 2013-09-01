(**----
   Name: userlib.mli
   Copyright M Wahab 2005-2013
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

(** {5 Default values} *)
module Default =
struct
  let context() = 
    let ctxt = Context.empty() in
    let ctxt1 = Context.set_obj_suffix ctxt [".cmo"; "cmi"] in
    let ctxt2 = Context.set_script_suffix ctxt1 (Settings.script_suffix) in
    let ctxt3 = Context.set_thy_suffix ctxt2 (Settings.thy_suffix) in  
    ctxt3

  let scope () = Thydb.mk_scope(Context.Thys.theories(context()))

  (* Printer tables *)
  let printers () = Printer.empty_ppinfo()

  (* Parser tables *)
  let parsers () = Parser.Table.empty Parser.Table.default_size

  (* Simplifier set *)
  let simpset() = Simplib.empty_simp()

  (* Proof stack *)
  let proofstack() = Goals.ProofStack.empty()
end

(** {5 Global state} *)
module State = 
struct 
  type t =
    {
      context_f : Context.t;
      scope_f : Scope.t;
      simpset_f : Simpset.simpset;
      proofstack_f: Goals.ProofStack.t
    }

    (** Initializer *)
    let empty() = 
      {
        context_f = Default.context();
        scope_f = Default.scope();
        simpset_f = Default.simpset();
        proofstack_f = Default.proofstack();
      }

    let context st = st.context_f
    let set_context st ctxt = 
      { st with context_f = ctxt }

    let scope st = st.scope_f
    let set_scope st scp = 
      { st with scope_f = scp }

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
end

(** {5 Variables } *)
module Var = 
struct
  let (state_v: State.t ref) = ref (State.empty())

  let state () = !state_v
  let set st = state_v := st
  let init() = set (State.empty())
end

(** The global state *)
let state = Var.state
let set_state = Var.set
let init_state = Var.init

(** The global context *)
let context = State.context
let set_context = State.set_context
let init_context st = 
  set_context st (Default.context())

(** The global scope *)
let scope = State.scope
let set_scope = State.set_scope
let init_scope st = 
  set_scope st (Default.scope())

(** Global pretty printer *)
let ppinfo = State.ppinfo
let set_ppinfo = State.set_ppinfo
let init_ppinfo st = 
  set_ppinfo st (Default.printers())

(** Parser tables *)
let parsers = State.parsers
let set_parsers = State.set_parsers
let init_parsers st = 
  set_parsers st (Default.parsers())

(** The simpset *)
let simpset = State.simpset
let set_simpset = State.set_simpset
let init_simpset st = 
  set_simpset st (Default.simpset())

(** The proofstack *)
let proofstack = State.proofstack
let set_proofstack = State.set_proofstack
let init_proofstack st = 
  set_proofstack st (Default.proofstack())

module Access = 
struct
  let context() = context (state())
  let set_context ctxt = set_state (set_context (state()) ctxt)
  let init_context () = set_context (Default.context())

  let scope() = scope (state())
  let set_scope scp = set_state (set_scope (state()) scp)
  let init_scope () = set_scope (Default.scope())

  let ppinfo() = ppinfo (state())
  let set_ppinfo pp = set_state (set_ppinfo (state()) pp)
  let init_ppinfo () = set_ppinfo (Default.printers())

  let parsers() = parsers (state())
  let set_parsers pp = set_state (set_parsers (state()) pp)
  let init_parsers () = set_parsers (Default.parsers())

  let simpset() = simpset (state())
  let set_simpset s = set_state (set_simpset (state()) s)
  let init_simpset () = set_simpset (Default.simpset())

  let proofstack() = proofstack (state())
  let set_proofstack s = set_state (set_proofstack (state()) s)
  let init_proofstack () = set_proofstack (Default.proofstack())
end

(** State initializer *)
let init () = 
  let st = 
    List.fold_left (fun a f -> f a) (State.empty())
      [init_context; init_scope;
       init_ppinfo; init_parsers;
       init_simpset; init_proofstack]
  in
  set_state st
