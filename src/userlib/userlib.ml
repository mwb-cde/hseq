(*----
  Name: userlib.ml
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

open Basic
open Parser
open Lib.Ops

(** {5 Global state} *)

module Global =
struct

  type state_t = Userstate.State.t

  (** State *)
  let state = Userstate.state
  let set_state = Userstate.set_state

  (** Initialise the global state. *)
  let init () = Userstate.init()

  (** Scope *)
  let scope = Userstate.scope
  let set_scope = Userstate.set_scope

  (** Context *)
  let context = Userstate.context
  let set_context = Userstate.set_context

  let scoped = Userstate.scoped
  let set_scoped = Userstate.set_scoped

  (** Pretty-printing *)
  let ppinfo = Userstate.ppinfo
  let set_ppinfo = Userstate.set_ppinfo

  (** Parser tables *)
  let parsers = Userstate.parsers
  let set_parsers = Userstate.set_parsers

  (** Simpset *)
  let simpset = Userstate.simpset
  let set_simpset = Userstate.set_simpset

  let theories() = Context.Thys.get_theories (context (state()))
  let current() = Context.Thys.current (context (state()))
  let current_name () = Context.Thys.current_name (context (state()))

end

let state_opt st =
  match st with
    None -> Global.state()
  | Some(x) -> x

(** Parsers and printers *)
module PP =
struct
  (** Tables access *)
  let overloads () = 
    Parser.Table.overloads (Global.parsers (Global.state()))

  let catch_parse_error e a = 
    try (e a)
    with 
    | Parser.ParsingError x -> raise (Report.error x)
    | Lexer.Lexing _ -> raise (Report.error ("Lexing error: "^a))
      
  let mk_term = Context.NewPP.mk_term

  let read str = 
    Context.NewPP.read (Global.scoped (Global.state())) str

  let read_unchecked str =
    Context.NewPP.read_unchecked (Global.context (Global.state())) str

  let read_defn str =
    Context.NewPP.read_defn (Global.scoped (Global.state())) str

  let read_type_defn str =
    Context.NewPP.read_type_defn (Global.scoped (Global.state())) str
      
  let read_type str = 
    Context.NewPP.read_type (Global.scoped (Global.state())) str

  let read_identifier str = 
    Context.NewPP.read_identifier (Global.context (Global.state())) str
end

(** {6 Utility functions} *)

(** String utilities **)
let compile dirs name = 
  let compile_aux () =
    let inc_dirs = 
      Lib.list_string (fun x -> ("-I \""^x^"\"")) " " dirs
    in 
    let inc_std_dirs =
      Lib.list_string 
	(fun x -> ("-I \""^x^"\"")) " " (!Settings.include_dirs)
    in 
    let inc_string = inc_std_dirs^" "^inc_dirs in 
    let com_string = "ocamlc -c"
    in 
    Sys.command (com_string ^" "^ inc_string ^" "^name)
  in 
  if !Sys.interactive
  then compile_aux()
  else (-1)
    
let catch_errors = Commands.catch_errors

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
let read_unchecked = PP.read_unchecked
let read_defn = PP.read_defn
let read_type_defn = PP.read_type_defn
let read_type = PP.read_type
let read_identifier = PP.read_identifier

let first_pos = Lib.First
let last_pos = Lib.Last
let before_pos s = Lib.Before (read_identifier s)
let after_pos s = Lib.After (read_identifier s)
let at_pos s = Lib.Level (read_identifier s)

let add_term_pp str ?(pos=Lib.First) pr fx sym = 
  let st = Global.state() in 
  let nctxt = 
    Commands.add_term_pp (Global.context st)
      (Ident.mk_long (Global.current_name()) str) ~pos:pos pr fx sym
  in 
  Global.set_state (Global.set_context (Global.state()) nctxt)

let get_term_pp s = 
  Commands.get_term_pp (Global.context (Global.state()))
    (Ident.mk_long (Global.current_name()) s)

let remove_term_pp s = 
  let st = Global.state() in
  let id = Ident.mk_long (Global.current_name()) s in
  let nctxt = Commands.remove_term_pp_rec (Global.context st) id
  in
  Global.set_state (Global.set_context st nctxt)  

let add_type_pp s prec fixity repr = 
  let st = Global.state() in 
  let id = Ident.mk_long (Global.current_name()) s in
  let ctxt0 = Global.context st in
  let ctxt1 = Commands.add_type_pp ctxt0 id prec fixity repr
  in 
  Global.set_state (Global.set_context st ctxt1)

let get_type_pp s = 
  Commands.get_type_pp 
    (Global.context (Global.state()))
    (Ident.mk_long (Global.current_name()) s)

let remove_type_pp s =
  let st = Global.state() in
  let ctxt0 = Global.context st in
  let nctxt = 
    Commands.remove_type_pp ctxt0 
      (Ident.mk_long (Global.current_name()) s); 
    ctxt0
  in 
    Global.set_state (Global.set_context st nctxt)

(** {6 Theories} *)
let begin_theory n ps =
  let st = Global.state() in 
  let nctxt = Commands.begin_theory (Global.context st) n ps in
  Global.set_state (Global.set_context st nctxt)

let end_theory ?save () = 
  let st = Global.state() in 
  let nctxt = Commands.end_theory (Global.context st) ?save:save () in
  Global.set_state (Global.set_context st nctxt)

let open_theory n = 
  let st = Global.state() in 
  let nctxt = Commands.open_theory (Global.context st) n  in
  Global.set_state (Global.set_context st nctxt)

let close_theory () = 
  let st = Global.state() in 
  Commands.close_theory (Global.context st)

(** {6 Theory properties} *)

let parents ps = 
  let st = Global.state() in 
  let nctxt = Commands.parents (Global.context st) ps in
  Global.set_state (Global.set_context st nctxt)

let add_file ?use n = 
  let st = Global.state() in 
  Commands.add_file (Global.context st) ?use:use n

let remove_file n = 
  let st = Global.state() in 
  Commands.remove_file (Global.context st) n

(** {6 Type declaration and definition} *)

let typedef ?pp ?(simp=true) ?thm ?rep ?abs tydef = 
  let state = Global.state() in 
  let scpd = Global.scoped state in
  let (scpd1, defn) = 
    Commands.typedef scpd ?pp:pp ~simp:simp ?thm:thm ?rep:rep ?abs:abs tydef
  in 
  let state1 = Global.set_scoped state scpd1 in
  begin
    if simp && (Logic.Defns.is_subtype defn)
    then 
      let tyrec = Logic.Defns.dest_subtype defn in 
      let rt_thm = tyrec.Logic.Defns.rep_type
      and rti_thm = tyrec.Logic.Defns.rep_type_inverse
      and ati_thm = tyrec.Logic.Defns.abs_type_inverse
      in 
      let nsimpset = 
        Simplib.add_simps scpd1 (Global.simpset state1)
          [rt_thm; rti_thm; ati_thm]
      in 
      Global.set_state (Global.set_simpset state1 nsimpset)
    else 
      Global.set_state state1
  end;
  defn

(** {6 Term declaration and definition} *)

let define ?pp ?(simp=false) df =
  let st = Global.state() in
  let scpd = Global.scoped st in
  let scpd1, ret = Commands.define scpd ?pp ~simp:simp df
  in 
  let st1 = Global.set_scoped st scpd1 in
  if simp
  then 
    let (_, _, thm) = Logic.Defns.dest_termdef ret in 
    let nsimpset = 
      Simplib.add_simp (Global.scoped st1)(Global.simpset st1) thm 
    in
    begin
      Global.set_state 
        (Global.set_simpset st1 nsimpset);
      ret
    end
  else 
    ret

let declare ?pp trm = 
  let st = Global.state() in 
  let nscpd, id, ty = Commands.declare (Global.scoped st) ?pp trm in
  Global.set_state (Global.set_scoped st nscpd);
  (id, ty)

(** {6 Axioms and theorems} *)

let axiom ?(simp=false) n t =
  let st = Global.state() in 
  let ctxt0, thm = Commands.axiom (Global.context st) ~simp:simp n t in
  let st0 = Global.set_context st ctxt0 in 
  let st1 = 
    if simp
    then 
      let nsimp = 
        Simplib.add_simp (Global.scoped st0) (Global.simpset st0) thm
      in
      Global.set_simpset st0 nsimp
    else st0
  in
  Global.set_state st1;
  thm

let save_thm ?(simp=false) n thm =
  let st = Global.state() in 
  let ctxt, ret = Commands.save_thm (Global.context st) ~simp:simp n thm
  in 
  let st0 = Global.set_context st ctxt in 
  let st1 = 
    if simp 
    then 
      let nsimp = 
        Simplib.add_simp (Global.scoped st0) (Global.simpset st0) ret
      in
      Global.set_simpset st0 nsimp
    else st0
  in
  Global.set_state st1;
  ret

let prove_thm ?(simp=false) n t tac =
  let st = Global.state() in 
  let ctxt, thm = 
    Commands.prove_thm (Global.scoped st) ~simp:simp n t tac
  in 
  let st0 = Global.set_context st ctxt in 
  let st1 = 
    if simp 
    then 
      let nsimp = 
        Simplib.add_simp (Global.scoped st0) (Global.simpset st0) thm
      in
      Global.set_simpset st0 nsimp
    else st0
  in
  Global.set_state st1;
  thm

let theorem = prove_thm
let lemma = theorem

(** {6 Information access} *)

let theory n = 
  let st = Global.state() in 
  Commands.theory (Global.context st) n

let theories () = 
  let st = Global.state() in 
  Commands.theories (Global.context st)

let defn n = 
  let st = Global.state() in 
  Commands.defn (Global.context st) n
let thm n =
  let st = Global.state() in
  Commands.thm (Global.context st) n

let state = Global.state()
let scope () = Global.scope (Global.state())
let goal_scope = Goals.goal_scope

(** {6 Proof commands} *)

let prove a tac = 
  let st = Global.state() in 
  Commands.prove (Global.scoped st) a tac

let by x = (catch_errors Goals.by_com) x
let qed = Commands.qed

(** {6 Initialising functions} *)

let init = Global.init
let reset = Global.init
