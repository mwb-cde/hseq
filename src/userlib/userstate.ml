(**----
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

(** {5 Default values} *)
module Default =
struct

  let context() = 
    let ctxt = BaseTheory.context() in
    let ctxt1 = Context.set_obj_suffix ctxt [".cmo"; "cmi"] in
    let ctxt2 = Context.set_script_suffix ctxt1 (Settings.script_suffix) in
    let ctxt3 = Context.set_thy_suffix ctxt2 (Settings.thy_suffix) in  
    ctxt3

  let scope () = Thydb.mk_scope(Context.Thys.theories(context()))

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

  let thyset() = thyset (state())
  let set_thyset s = set_state (set_thyset (state()) s)
  let init_thyset () = set_thyset (Default.thyset())
  let thyset_add t = 
    let set1 = Lib.StringSet.add t (thyset ()) in
    set_thyset set1
  let thyset_mem t = thyset_mem (state()) t

  let simpset() = simpset (state())
  let set_simpset s = set_state (set_simpset (state()) s)
  let init_simpset () = 
    set_thyset (Default.thyset());
    set_simpset (Default.simpset())

  let proofstack() = proofstack (state())
  let set_proofstack s = set_state (set_proofstack (state()) s)
  let init_proofstack () = set_proofstack (Default.proofstack())
end


(** Build theories from a script *)
module TheoryScriptReader =
struct
  let forbidden = ref(Lib.StringSet.empty)
  let init_forbidden() = forbidden := Lib.StringSet.empty
  and is_forbidden s = Lib.StringSet.mem s (!forbidden)
  and add_forbidden s = forbidden := Lib.StringSet.add s (!forbidden)
  and drop_forbidden s = forbidden := Lib.StringSet.remove s (!forbidden)

  let build_thy_file thyname =
    let ctxt = context (state()) in 
    let thydb0 = Context.Thys.theories ctxt in
    let build_aux file =
      let script = 
        Context.Files.find_file 
          (Context.Files.script_of_thy ctxt file) 
          (Context.Files.get_thy_path ctxt) 
      in 
      let usefile = Context.scripter ctxt ~silent:false
      in 
      Report.report ("Trying to build theory "^file);
      begin
        try
          begin
            add_forbidden file; 
            usefile script; 
            drop_forbidden file
          end
        with err -> (drop_forbidden file; raise err)
      end;
      Report.report ("Built theory "^file);
      let ctxt1 = context(state()) in
      let thydb1 = Context.Thys.theories ctxt1 in 
      ignore(set_context (state()) (Context.Thys.set_theories ctxt thydb0));
      thydb1
    in 
    if (is_forbidden thyname)
    then raise (Report.error ("Circular importing, theory "^thyname))
    else ();
    try build_aux thyname
    with Not_found ->
      (Report.warning ("Failed to build theory "^thyname);
       raise (Report.error ("Can't find script to build theory "^thyname)))

  let builder ?silent name = 
    ignore(build_thy_file name)
end

module Loader =
struct

(** Remember loaded theories *)
  let record_thy_fn ctxt thy = 
    let n = thy.Theory.cname in
    Access.thyset_add n; 
    ctxt

(** Simplifier functions *)
  let simp_thy_fn ctxt thy = 
    let thyname = thy.Theory.cname in
    if (Access.thyset_mem thyname) then ctxt 
    else
      let set = Access.simpset() in
      let set1 = Simplib.on_load ctxt set thy in
      begin
        Access.set_simpset set1;
        record_thy_fn ctxt thy
      end

  let thy_fn_list = [simp_thy_fn]

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

  let default_thy_fn 
      (ctxt: Context.t) (db: Thydb.thydb) (thy: Theory.contents) =
    Report.report 
      ("Thyloader.default_thy_fn("^thy.Theory.cname^")");
    let thy_fn_list = (Context.load_functions ctxt) in
    let ctxt1 = List.fold_left (fun ctxt0 f -> f ctxt0 thy) ctxt thy_fn_list
    in 
    ignore(ctxt1)

  let rec thy_importing_list ret thydb thy = 
    let ret = find_thy_parents ret thydb thy in
    ret
  and work_thy ret thydb thyname = 
    let thy = Thydb.get_thy thydb thyname in
    find_thy_parents (thy::ret) thydb thy
  and find_thy_parents ret thydb thy =
    let ps = Theory.get_parents thy in
    let thylist = 
      List.fold_left 
        (fun tlst name -> work_thy tlst thydb name)
        ret ps
    in 
    thylist
      
  let build_fn
      (ctxt: Context.t) (db: Thydb.thydb) (thyname: string) =
    let scripter = get_use_file() in
    let script_name = Context.Files.script_of_thy ctxt thyname in
    let saved_state = state() in
    let st1 = set_context saved_state ctxt in
    begin
      set_state st1;
      scripter ~silent:false script_name;
      let st2 = state() in
      Context.thydb (context st2)
    end

  let buildthy (ctxt: Context.t) (thyname: string) =
    let saved_state = state() in
    let db1 = 
      if (thyname = Lterm.base_thy)
      then 
        begin
          let ctxt1 = BaseTheory.builder ~save:true ctxt in
          Context.thydb ctxt1
        end
      else build_fn ctxt (Context.thydb ctxt) thyname
    in 
    (set_state saved_state; db1)

  let default_build_fn 
      (ctxt: Context.t) (db: Thydb.thydb) (thyname: string) =
    let db1 = buildthy (Context.set_thydb ctxt db) thyname in
    let thy = Thydb.get_thy db1 thyname in
    let thylist = thy_importing_list [] db1 thy in
    (db1, thylist)

  let default_load_fn 
      (ctxt: Context.t) (file_data: Thydb.Loader.info) =
    Context.Files.load_thy_file ctxt file_data

      
  let default_loader ctxt = 
    Thydb.Loader.mk_data 
      (default_load_fn ctxt)
      (default_build_fn ctxt)

  let load_file fname = 
    (get_load_file()) fname

  let script_file ?(silent=false) fname = 
    (get_use_file()) ~silent fname

  let set_file_handlers ctxt = 
    let ctxt1 = Context.set_loader ctxt load_file  in
    let ctxt2 = Context.set_scripter ctxt1 script_file in
    ctxt2

end

(** State initializer *)
let init_context st = 
  let ctxt0 = Default.context() in
  let ctxt1 = ctxt0 in
  let st1 = set_parsers (set_context st ctxt1) (Parser.init ()) in
  st1

let init_scope st = 
  set_scope st (Default.scope())
let init_ppinfo st = 
  set_ppinfo st (Default.printers())
let init_parsers st = 
  set_parsers st (Default.parsers())
let init_simpset st = 
  let st1 = set_thyset st (Default.thyset()) in
  set_simpset st1 (Default.simpset())
let init_proofstack st = 
  set_proofstack st (Default.proofstack())

let init_base_thy_builder st = st


