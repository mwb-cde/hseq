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

  let context () = Userstate.context (state())
  let set_context ctxt = 
    set_state (Userstate.set_context (state()) ctxt)

  let scope () = Userstate.scope (state())
  let set_scope ctxt = 
    set_state (Userstate.set_scope (state()) ctxt)

  let scoped () = Userstate.scoped (state())
  let set_scoped x = 
    set_state (Userstate.State.set_scoped (state()) x)

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

  let theories() = Context.Thys.get_theories (context ())
  let current() = Context.Thys.current (context ())
  let current_name () = Context.Thys.current_name (context ())

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
    Parser.Table.overloads (Global.parsers ())

  let catch_parse_error e a = 
    try (e a)
    with 
    | Parser.ParsingError x -> raise (Report.error x)
    | Lexer.Lexing _ -> raise (Report.error ("Lexing error: "^a))
      
  let mk_term = Context.NewPP.mk_term

  let read str = 
    Context.NewPP.read (Global.scoped ()) str

  let read_unchecked str =
    Context.NewPP.read_unchecked (Global.context ()) str

  let read_defn str =
    Context.NewPP.read_defn (Global.scoped ()) str

  let read_type_defn str =
    Context.NewPP.read_type_defn (Global.scoped ()) str
      
  let read_type str = 
    Context.NewPP.read_type (Global.scoped ()) str

  let read_identifier str = 
    Context.NewPP.read_identifier (Global.context ()) str
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
      (Ident.mk_long (Global.current_name()) s); 
    ctxt0
  in 
  Global.set_context nctxt

(** {6 Theories} *)
let begin_theory n ps =
  let nctxt = Commands.begin_theory (Global.context ()) n ps in
  Global.set_context nctxt

let end_theory ?save () = 
  let nctxt = Commands.end_theory (Global.context ()) ?save:save () in
  Global.set_context nctxt

let open_theory n = 
  let nctxt = Commands.open_theory (Global.context ()) n  in
  Global.set_context nctxt

let close_theory () = 
  Commands.close_theory (Global.context ())

(** {6 Theory properties} *)

let parents ps = 
  let nctxt = Commands.parents (Global.context ()) ps in
  Global.set_context nctxt

let add_file ?use n = 
  Commands.add_file (Global.context ()) ?use:use n

let remove_file n = 
  Commands.remove_file (Global.context ()) n

(** {6 Type declaration and definition} *)

let typedef ?pp ?(simp=true) ?thm ?rep ?abs tydef = 
  let scpd = Global.scoped () in
  let (scpd1, defn) = 
    Commands.typedef scpd ?pp:pp ~simp:simp ?thm:thm ?rep:rep ?abs:abs tydef
  in 
  begin
    Global.set_scoped scpd1;
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
  let scpd = Global.scoped () in
  let scpd1, ret = Commands.define scpd ?pp ~simp:simp df in 
  Global.set_scoped scpd1;
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
  let nscpd, id, ty = Commands.declare (Global.scoped ()) ?pp trm in
  Global.set_scoped nscpd;
  (id, ty)

(** {6 Axioms and theorems} *)

let axiom ?(simp=false) n t =
  let ctxt0, thm = Commands.axiom (Global.scoped ()) ~simp:simp n t in
  Global.set_scoped ctxt0;
  if simp
  then 
    let nsimp = 
      Simplib.add_simp (Global.scoped ()) (Global.simpset ()) thm
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
      Simplib.add_simp (Global.scoped ()) (Global.simpset ()) ret
    in
    Global.set_simpset nsimp
  else ();
  ret

let prove_thm ?(simp=false) n t tac =
  let ctxt, thm = 
    Commands.prove_thm (Global.scoped ()) ~simp:simp n t tac
  in 
  Global.set_context ctxt;
  if simp 
  then 
    let nsimp = 
      Simplib.add_simp (Global.scoped ()) (Global.simpset ()) thm
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
let drop() = set_proofstack (Goals.drop (proofstack()))
let goal_scope () = Goals.goal_scope (proofstack ())
let curr_sqnt () = Goals.curr_sqnt (proofstack())
let get_asm i = Goals.get_asm (proofstack()) i
let get_concl i = Goals.get_concl (proofstack()) i

let prove a tac = 
  Commands.prove (Global.scoped ()) a tac

let prove_goal trm tac = 
  Goals.prove_goal (Global.scope()) trm tac

let drop () = 
  set_proofstack (Goals.drop (proofstack()))

let lift i = 
  set_proofstack (Goals.lift (proofstack()) i)

let undo () =
  set_proofstack (Goals.undo (proofstack()))

let postpone () =
  set_proofstack (Goals.postpone (proofstack()))

let goal trm = 
  set_proofstack (Goals.goal (proofstack()) (scope()) trm)

let result () = Goals.result (proofstack())

let by_com tac = 
  let nprfstk = 
    catch_errors 
      (Goals.by_com (Global.context()) (Global.proofstack())) tac
  in
  set_proofstack nprfstk

let by tac = 
  by_com tac; top()

let by_list trm tl = Goals.by_list (Global.scope()) trm tl

let qed n = 
  let (ctxt, thm) = Commands.qed (Global.context()) (proofstack()) n 
  in
  Global.set_context ctxt;
  thm

let apply = Goals.apply

(** {6 Initialising functions} *)

let init = Global.init
let reset = Global.init
