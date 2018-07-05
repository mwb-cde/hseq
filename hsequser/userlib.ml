(*----
  Name: userlib.ml
  Copyright Matthew Wahab 2013-2018
  Author: Matthew Wahab <mwb.cde@gmail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under
  the terms of the Lesser GNU General Public License as published by the
  Free Software Foundation; either version 3, or (at your option) any
  later version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
  License for more details.

  You should have received a copy of the Lesser GNU General Public
  License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
  ----*)

open HSeq
open Parser
open Lib.Ops

(** {5 Variables } *)
module Var =
struct

  let (state_v: Userstate.State.t ref) = ref (Userstate.State.empty())
  let state () = !state_v
  let set_state st = state_v := st
  let init() = set_state (Userstate.State.empty())

end

(** {5 Global access } *)
module Global =
struct

  type state_t = Userstate.State.t

  (** State *)
  let state = Var.state
  let set_state = Var.set_state

  (** Initialise the global state. *)
  let init st = set_state (Userstate.init (state()))

  let context () = Userstate.context (state())
  let set_context ctxt =
    set_state (Userstate.set_context (state()) ctxt)

  let scope () = Userstate.scope (state())
  let set_scope ctxt =
    set_state (Userstate.set_scope (state()) ctxt)

  let ppinfo () = Userstate.ppinfo (state())
  let set_ppinfo ctxt =
    set_state (Userstate.set_ppinfo (state()) ctxt)

  let parsers () = Userstate.parsers (state())
  let set_parsers ctxt =
    set_state (Userstate.set_parsers (state()) ctxt)

  let simpset () = Userstate.simpset (state())
  let set_simpset ctxt =
    set_state (Userstate.set_simpset (state()) ctxt)

  let proofstack () = Userstate.proofstack (state())
  let set_proofstack ctxt =
    set_state (Userstate.set_proofstack (state()) ctxt)

  let theories() = Context.thydb (context ())
  let current() = Context.current (context ())
  let current_name () = Context.current_name (context ())

  let thyset() = Userstate.thyset (state())
  let set_thyset s =
    set_state (Userstate.set_thyset (state()) s)
  let thyset_add t =
    let set1 = Lib.StringSet.add t (thyset ()) in
    set_thyset set1
  let thyset_mem t = Userstate.thyset_mem (state()) t

  let proofstack() = Userstate.proofstack (state())
  let set_proofstack s =
    set_state (Userstate.set_proofstack (state()) s)

  let path () = Context.path (context())
  let set_path p =
    set_context (Context.set_path (context()) p)
end

let state_opt st =
  match st with
    None -> Global.state()
  | Some(x) -> x

module Access =
struct
  let context() = Userstate.context (Global.state())
  let set_context ctxt =
    Global.set_state (Userstate.set_context (Global.state()) ctxt)
  let init_context () = Global.set_context (Userstate.Default.context())

  let scope() = Userstate.scope (Global.state())
  let set_scope scp =
    Global.set_state (Userstate.set_scope (Global.state()) scp)
  let init_scope () = Global.set_scope (Userstate.Default.scope())

  let ppinfo() = Userstate.ppinfo (Global.state())
  let set_ppinfo pp =
    Global.set_state (Userstate.set_ppinfo (Global.state()) pp)
  let init_ppinfo () = set_ppinfo (Userstate.Default.printers())

  let parsers() = Userstate.parsers (Global.state())
  let set_parsers pp =
    Global.set_state (Userstate.set_parsers (Global.state()) pp)
  let init_parsers () =
    set_parsers (Userstate.Default.parsers())

  let thyset() = Userstate.thyset (Global.state())
  let set_thyset s =
    Global.set_state (Userstate.set_thyset (Global.state()) s)
  let init_thyset () =
    set_thyset (Userstate.Default.thyset())
  let thyset_add t =
    let set1 = Lib.StringSet.add t (thyset ()) in
    set_thyset set1
  let thyset_mem t = Userstate.thyset_mem (Global.state()) t

  let simpset() = Userstate.simpset (Global.state())
  let set_simpset s =
    Global.set_state (Userstate.set_simpset (Global.state()) s)
  let init_simpset () =
    set_thyset (Userstate.Default.thyset());
    set_simpset (Userstate.Default.simpset())

  let proofstack() = Userstate.proofstack (Global.state())
  let set_proofstack s =
    Global.set_state (Userstate.set_proofstack (Global.state()) s)
  let init_proofstack () = set_proofstack (Userstate.Default.proofstack())
end

(** {5 File loader} *)
module Loader =
struct

  (** {5 Theory building and loading} *)

  (** Remember loaded theories *)
  let record_thy_fn ctxt thy =
    let n = thy.Theory.cname in
    Global.set_state (Userstate.thyset_add (Global.state()) n);
    ctxt

  (** Set up simpset from loaded theory *)
  let simp_thy_fn ctxt thy =
    let thyname = thy.Theory.cname in
    let st = Global.state() in
    if Userstate.thyset_mem st thyname then ctxt
    else
      let set = Userstate.simpset st in
      let set1 = Simplib.on_load ctxt set thy in
      Global.set_state (Userstate.set_simpset st set1);
      ctxt

  (** Set up type  printer/parser recoreds from a loaded theory. *)
  let type_pp_thy_fn = Context.PP.add_theory_type_pp

  (** Set up term printer/parser recoreds from a loaded theory. *)
  let term_pp_thy_fn = Context.PP.add_theory_term_pp

  (** Load files listed in the theory. *)
  let load_files_thy_fn ctxt thy =
    let file_list = thy.Theory.cfiles in
    let path = Context.path ctxt in
    let loader ctxt f =
      let fqn =
        try Context.Files.find_file f path
        with Not_found ->
          raise (Report.error ("Can't find file "^f^" in path."))
      in
      begin
        Global.set_context ctxt;
        Context.loader ctxt fqn;
        Global.context()
      end
    in
    let ctxt0 = Global.context() in
    let ctxt1 = List.fold_left loader ctxt file_list in
    Global.set_context ctxt0;
    ctxt1

  (** List of functions to apply to a loaded theory *)
  let thy_fn_list =
    [ type_pp_thy_fn; term_pp_thy_fn;
      simp_thy_fn; load_files_thy_fn;
      record_thy_fn ]

  let default_thy_fn
      (ctxt: Context.t) (db: Thydb.thydb) (thy: Theory.contents) =
    Report.report
      ("Thyloader.default_thy_fn("^thy.Theory.cname^")");
    let thy_fn_list = Context.load_functions ctxt in
    let _ = List.fold_left (fun ctxt0 f -> f ctxt0 thy) ctxt thy_fn_list
    in
    ()

  (** Generate the list of theories imported by a theory. For use in
      Thydb.Loader, when applying the theory functions *)
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

  let build_fn (ctxt: Context.t) (db: Thydb.thydb) (thyname: string) =
    let scripter = Context.scripter (Global.context()) in
    let script_name = Context.Files.script_of_thy ctxt thyname in
    let saved_state = Global.state() in
    let st1 = Userstate.set_context saved_state ctxt in
    begin
      Global.set_state st1;
      scripter ~silent:false script_name;
      let st2 = Global.state() in
      Context.thydb (Userstate.context st2)
    end

  let buildthy (ctxt: Context.t) (thyname: string) =
    let saved_state = Global.state() in
    let db1 =
      if (thyname = Lterm.base_thy)
      then
        begin
          let ctxt1 = BaseTheory.builder ~save:true ctxt in
          Context.thydb ctxt1
        end
      else build_fn ctxt (Context.thydb ctxt) thyname
    in
    (Global.set_state saved_state; db1)

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

  let set_load_file ctxt loader =
    Context.set_loader ctxt loader

  let set_use_file ctxt scripter =
    Context.set_scripter ctxt scripter

  let init ctxt =
    let st = Global.state() in
    let loader = Userstate.loader st
    and scripter = Userstate.scripter st in
    let ctxt1 = set_load_file ctxt loader
    in
    set_use_file ctxt1 scripter

end

(** Parsers and printers *)
module PP =
struct
  (** Tables access *)
  let overloads () =
    Parser.Table.overloads (Global.parsers ())

  let catch_parse_error e a =
    try (e a)
    with
    | Parser.ParsingError x -> raise (Report.error x)
    | Lexer.Lexing _ -> raise (Report.error ("Lexing error: "^a))

  let mk_term = Context.PP.mk_term

  let read str =
    Context.PP.read (Global.context ()) str

  let read_unchecked str =
    Context.PP.read_unchecked (Global.context ()) str

  let read_defn str =
    Context.PP.read_defn (Global.context ()) str

  let read_type_defn str =
    Context.PP.read_type_defn (Global.context ()) str

  let read_type str =
    Context.PP.read_type (Global.context ()) str

  let read_identifier str =
    Context.PP.read_identifier (Global.context ()) str
end

(** {6 Utility functions} *)

let load_file_func () = Userstate.loader (Global.state())

let set_load_file_func loader =
  Global.set_state (Userstate.set_loader (Global.state()) loader);
  Global.set_context
    (Context.set_loader (Global.context()) (load_file_func()))

let path = Global.path
let set_path = Global.set_path
let add_to_path d =
  let p0 = Global.path () in
  let p1 = d::p0
  in
  Global.set_path p1

let use_file_func () = Userstate.scripter (Global.state())

let set_use_file_func scripter =
  Global.set_state (Userstate.set_scripter (Global.state()) scripter);
  Global.set_context
    (Context.set_scripter (Global.context()) (use_file_func()))

let get_proof_hook () =
  Goals.save_hook (Global.proofstack ())

let set_proof_hook f =
  Global.set_proofstack (Goals.set_hook f (Global.proofstack()))

(** String utilities **)
let compile dirs name =
  let compile_aux () =
    let inc_dirs =
      Lib.list_string (fun x -> ("-I \""^x^"\"")) " " dirs
    in
    let inc_std_dirs =
      Lib.list_string
        (fun x -> ("-I \""^x^"\"")) " " (Global.path())
    in
    let inc_string = inc_std_dirs^" "^inc_dirs in
    let com_string = "ocamlc -c"
    in
    Sys.command (com_string ^" "^ inc_string ^" "^name)
  in
  compile_aux()

let catch_errors x = Commands.catch_errors (Global.ppinfo()) x

(** {6 Printing and parsing} *)

type fixity = Commands.fixity
let nonfix = Commands.nonfix
let prefix = Commands.prefix
let suffix = Commands.suffix
let infixl = Commands.infixl
let infixr = Commands.infixr
let infixn = Commands.infixn

let mk_term = PP.mk_term

let read = PP.read
let hterm = read
let (!%) = hterm
let read_unchecked = PP.read_unchecked

let read_defn = PP.read_defn
let hdefn = read_defn
let (?<%) = hdefn

let read_type = PP.read_type
let htype = read_type
let (!:) = htype

let read_type_defn = PP.read_type_defn
let htype_defn = read_type_defn
let (?<:) = htype_defn

let read_identifier = PP.read_identifier

let first_pos = Lib.First
let last_pos = Lib.Last
let before_pos s = Lib.Before (read_identifier s)
let after_pos s = Lib.After (read_identifier s)
let at_pos s = Lib.Level (read_identifier s)

let add_symbol str sym =
  let nctxt =
    Commands.add_symbol (Global.context ()) str sym
  in
  Global.set_context nctxt

let add_term_pp str ?(pos=Lib.First) pr fx sym =
  let nctxt =
    Commands.add_term_pp (Global.context ())
      (Ident.mk_long (Global.current_name()) str) ~pos:pos pr fx sym
  in
  Global.set_context nctxt

let get_term_pp s =
  Commands.get_term_pp (Global.context ())
    (Ident.mk_long (Global.current_name()) s)

let remove_term_pp s =
  let id = Ident.mk_long (Global.current_name()) s in
  let nctxt = Commands.remove_term_pp_rec (Global.context ()) id
  in
  Global.set_context nctxt

let add_type_pp s prec fixity repr =
  let id = Ident.mk_long (Global.current_name()) s in
  let ctxt0 = Global.context () in
  let ctxt1 = Commands.add_type_pp ctxt0 id prec fixity repr
  in
  Global.set_context ctxt1

let get_type_pp s =
  Commands.get_type_pp
    (Global.context ())
    (Ident.mk_long (Global.current_name()) s)

let remove_type_pp s =
  let ctxt0 = Global.context () in
  let nctxt =
    Commands.remove_type_pp ctxt0
      (Ident.mk_long (Global.current_name()) s)
  in
  Global.set_context nctxt

(** {6 Theories} *)
let begin_theory n ps =
  Access.init_simpset();
  let nctxt = Commands.begin_theory (Global.context ()) n ps in
  Global.set_context nctxt

let end_theory ?save () =
  let nctxt = Commands.end_theory (Global.context ()) ?save:save () in
  Global.set_context nctxt

let open_theory n =
  let nctxt = Commands.open_theory (Global.context ()) n  in
  Global.set_context nctxt

let close_theory () =
  let nctxt = Commands.close_theory (Global.context ()) in
  Global.set_context nctxt

let load_theory th =
  let nctxt = Commands.load_theory_as_cur (Global.context ()) th  in
  Global.set_context nctxt

(** {6 Theory properties} *)

let parents ps =
  let nctxt = Commands.parents (Global.context ()) ps in
  Global.set_context nctxt

let add_file ?(use=false) n =
  let nctxt = Commands.add_file (Global.context ()) n in
  begin
    Global.set_context nctxt;
    if use
    then Context.Files.load_use_file (Global.context()) n
    else ()
  end

let remove_file n =
  let nctxt = Commands.remove_file (Global.context ()) n in
  Global.set_context nctxt

(** {6 Type declaration and definition} *)

let typedef ?pp ?(simp=true) ?thm ?rep ?abs tydef =
  let scpd = Global.context () in
  let (scpd1, defn) =
    Commands.typedef scpd ?pp:pp ~simp:simp ?thm:thm ?rep:rep ?abs:abs tydef
  in
  begin
    Global.set_context scpd1;
    if simp && (Logic.Defns.is_subtype defn)
    then
      let tyrec = Logic.Defns.dest_subtype defn in
      let rt_thm = tyrec.Logic.Defns.rep_type
      and rti_thm = tyrec.Logic.Defns.rep_type_inverse
      and ati_thm = tyrec.Logic.Defns.abs_type_inverse
      in
      let nsimpset =
        Simplib.add_simps scpd1 (Global.simpset ())
          [rt_thm; rti_thm; ati_thm]
      in
      Global.set_simpset nsimpset
    else ()
  end;
  defn

(** {6 Term declaration and definition} *)

let define ?pp ?(simp=false) df =
  let scpd = Global.context () in
  let scpd1, ret = Commands.define scpd ?pp ~simp:simp df in
  Global.set_context scpd1;
  if simp
  then
    let (_, _, thm) = Logic.Defns.dest_termdef ret in
    let nsimpset =
      Simplib.add_simp scpd1 (Global.simpset ()) thm
    in
    (Global.set_simpset nsimpset); ret
  else
    ret

let declare ?pp trm =
  let nscpd, id, ty = Commands.declare (Global.context ()) ?pp trm in
  Global.set_context nscpd;
  (id, ty)

(** {6 Axioms and theorems} *)

let axiom ?(simp=false) n t =
  let ctxt0, thm = Commands.axiom (Global.context ()) ~simp:simp n t in
  Global.set_context ctxt0;
  if simp
  then
    let nsimp =
      Simplib.add_simp (Global.context ()) (Global.simpset ()) thm
    in
    Global.set_simpset nsimp
  else ();
  thm

let save_thm ?(simp=false) n thm =
  let ctxt, ret = Commands.save_thm (Global.context ()) ~simp:simp n thm
  in
  Global.set_context ctxt;
  if simp
  then
    let nsimp =
      Simplib.add_simp (Global.context ()) (Global.simpset ()) ret
    in
    Global.set_simpset nsimp
  else ();
  ret

let prove_thm ?(simp=false) n t tac =
  let ctxt, thm =
    Commands.prove_thm (Global.context ()) ~simp:simp n t tac
  in
  Global.set_context ctxt;
  if simp
  then
    let nsimp =
      Simplib.add_simp (Global.context ()) (Global.simpset ()) thm
    in
    Global.set_simpset nsimp
  else ();
  thm

let theorem = prove_thm
let lemma = theorem

(** {6 Information access} *)

let theory n =
  Commands.theory (Global.context ()) n

let theories () =
  Commands.theories (Global.context ())

let defn n =
  Commands.defn (Global.context ()) n

let thm n =
  Commands.thm (Global.context ()) n

let state = Global.state
let scope () = Global.scope ()

(** {6 Goals and proofs} *)

let proofstack() = Global.proofstack ()
let set_proofstack pstk = Global.set_proofstack pstk
let top() = Goals.top (proofstack())
let top_goal() = Goals.top_goal (proofstack())
let drop() = set_proofstack (Goals.drop (proofstack())); top()
let goal_scope () = Goals.goal_scope (proofstack ())
let curr_sqnt () = Goals.curr_sqnt (proofstack())
let get_asm i = Goals.get_asm (proofstack()) i
let get_concl i = Goals.get_concl (proofstack()) i

let prove a tac =
  Commands.prove (Global.context ()) a tac

let prove_goal trm tac =
  Goals.prove_goal (Global.context()) trm tac

let drop () =
  set_proofstack (Goals.drop (proofstack()));
  top()

let lift i =
  set_proofstack (Goals.lift (proofstack()) i);
  top()

let undo () =
  set_proofstack (Goals.undo (proofstack()));
  top()

let postpone () =
  set_proofstack (Goals.postpone (proofstack()));
  top()


let goal trm =
  set_proofstack (Goals.goal (proofstack()) (scope()) trm);
  top()

let result () = Goals.result (proofstack())

let by_com tac =
  let nprfstk =
    catch_errors
      (Goals.by_com (Global.context()) (Global.proofstack())) tac
  in
  set_proofstack nprfstk

let by tac =
  by_com tac; top()

let by_list trm tl = Goals.by_list (Global.context()) trm tl

let qed n =
  let (ctxt, thm) = Commands.qed (Global.context()) (proofstack()) n
  in
  Global.set_context ctxt;
  thm

let apply ?report tac g =
  Goals.apply ?report (Global.context()) tac g

(** Top-level pretty printers *)
module Display =
struct

  let print_fnident = Display.print_fnident

  let print_term x = Display.print_term (Global.ppinfo()) x

  let print_formula x = Display.print_formula (Global.ppinfo()) x

  let rec print_type x = Display.print_type (Global.ppinfo()) x

  let print_sqnt x = Display.print_sqnt (Global.ppinfo()) x
  let print_node x = Display.print_node (Global.ppinfo()) x
  let print_branch x = Display.print_branch (Global.ppinfo()) x
  let print_thm t = Logic.print_thm (Global.ppinfo()) t

  let print_prf p = Display.print_prf (Global.ppinfo()) p
  let print_prfstk p = Display.print_prfstk (Global.ppinfo()) p

  let print_defn def = Display.print_defn (Global.ppinfo()) def
  let print_subst = Display.print_subst

  let print_error r = Display.print_error (Global.ppinfo()) r
  let print_type_error r = Display.print_type_error (Global.ppinfo()) r
  let print_report depth r = Display.print_report (Global.ppinfo()) depth r

  let print_theory x = Display.print_theory (Global.ppinfo()) x

  let print_simpset x = Simpset.print (Global.ppinfo()) x

end (* Display *)

(** {6 Initialising functions} *)

let init () =
  let st0 = Userstate.init (Global.state()) in
  let ctxt0 = Userstate.context st0 in
  let ctxt1 = Context.set_loader_data ctxt0 Loader.default_loader in
  let ctxt2 = Context.set_load_functions ctxt1 Loader.thy_fn_list in
  let ctxt3 = Loader.init ctxt2 in
  let st1 = Userstate.set_context st0 ctxt3 in
  Global.set_state st1;
  (try ignore(load_theory (Context.base_name (Global.context())))
   with _ -> ())

let reset = init

(** {6 Simplifier} *)

let add_simps thms =
  let ctxt0 = Global.context() in
  let set0 = Global.simpset() in
  let set1 = Simplib.add_simps ctxt0 set0 thms in
  Global.set_simpset set1

let add_simp  thm =
  let ctxt0 = Global.context() in
  let set0 = Global.simpset() in
  let set1 = Simplib.add_simp ctxt0 set0 thm in
  Global.set_simpset set1

let add_conv trms conv =
  let set0 = Global.simpset() in
  let set1 = Simplib.add_conv set0 trms conv
  in
  Global.set_simpset set1

(** [add_conv trms conv]: Add conversion [conv] to the standard
    simpset, with [trms] as the representative keys.  Example:
    [add_conv [<< !x A: (%y: A) x >>] Logic.Conv.beta_conv] applies
    [beta_conv] on all terms matching [(%y: A) x].
*)

let simpA_tac ?cntrl ?ignore ?set ?add ?a rules =
  let set0 = Lib.from_option set (Global.simpset()) in
  Simplib.simpA_tac ?cntrl ?ignore set0 ?add ?a rules
(** [simpA_tac ?cntrl ?ignore ?asms ?set ?add ?a rules goal]

    Simplify assumptions.
*)

let simpA ?set ?a rules =
  let set0 = Lib.from_option set (Global.simpset()) in
  Simplib.simpA_tac set0 ?a rules

let simpC_tac ?cntrl ?ignore ?set ?add ?c rules =
  let set0 = Lib.from_option set (Global.simpset()) in
  Simplib.simpC_tac ?cntrl ?ignore set0 ?add ?c rules

let simpC ?set ?c rules =
  let set0 = Lib.from_option set (Global.simpset()) in
  Simplib.simpC_tac set0 ?c rules

let simp_all_tac ?cntrl ?ignore ?set ?add thms =
  let set0 = Lib.from_option set (Global.simpset()) in
  Simplib.simp_all_tac ?cntrl ?ignore set0 ?add thms

let simp_all_tac ?cntrl ?ignore ?set ?add thms =
  let set0 = Lib.from_option set (Global.simpset()) in
  Simplib.simp_all_tac ?cntrl ?ignore set0 ?add thms
(** [simp_all_tac ?cntrl ?ignore ?asms ?set ?add rules goal]

    Simplify each formula in the subgoal.
*)

let simp_all ?set thms =
  let set0 = Lib.from_option set (Global.simpset()) in
  Simplib.simp_all_tac set0 thms
(** [simp_all]: Shorthand for {!Simplib.simp_all_tac}.

    @raise No_change If no change is made.
*)


let simp_tac ?cntrl ?ignore ?set ?add ?f thms =
  let set0 = Lib.from_option set (Global.simpset()) in
  Simplib.simp_tac ?cntrl ?ignore set0 ?add ?f thms
(** [simp_tac ?cntrl ?ignore ?asms ?set ?add ?f rules goal]

    Simplifier tactic.
*)


let simp ?set ?f g =
  let set0 = Lib.from_option set (Global.simpset()) in
  Simplib.simp set0 ?f g
(** [simp ?f]: Shorthand for {!Simplib.simp_tac}.

    @raise No_change If no change is made.
*)
