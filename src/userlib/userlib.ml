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

module Files =
struct
  let set_load_file loader = 
    begin
      Userstate.Loader.set_load_file loader;
      let ctxt = Userstate.Loader.set_file_handlers (Global.context()) in
      Global.set_context ctxt
    end

  let set_use_file scripter = 
    begin
      Userstate.Loader.set_use_file scripter;
      let ctxt = Userstate.Loader.set_file_handlers (Global.context()) in
      Global.set_context ctxt
    end
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
      
  let mk_term = Context.NewPP.mk_term

  let read str = 
    Context.NewPP.read (Global.context ()) str

  let read_unchecked str =
    Context.NewPP.read_unchecked (Global.context ()) str

  let read_defn str =
    Context.NewPP.read_defn (Global.context ()) str

  let read_type_defn str =
    Context.NewPP.read_type_defn (Global.context ()) str
      
  let read_type str = 
    Context.NewPP.read_type (Global.context ()) str

  let read_identifier str = 
    Context.NewPP.read_identifier (Global.context ()) str
end

(** {6 Utility functions} *)

let load_file_func () = 
  Context.loader (Global.context())
let set_load_file_func f = 
  Files.set_load_file f

let use_file_func () = 
  Context.scripter (Global.context())
let set_use_file_func f = 
  Files.set_use_file f

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
  Userstate.Access.init_simpset();
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

(***
  let print_termdefn def = Display.print_termdefn (Global.ppinfo()) def
  let print_termdecln def = Display.print_termdecln (Global.ppinfo()) def
**)

  let print_defn def = Display.print_defn (Global.ppinfo()) def
  let print_subst = Display.print_subst
      
  let print_error r = Display.print_error (Global.ppinfo()) r

  let print_theory x = Display.print_theory (Global.ppinfo()) x

  let print_simpset x = Simpset.print (Global.ppinfo()) x

end (* Display *)

(** {6 Initialising functions} *)

let init = Global.init
let reset = Global.init


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
  let set0 = Lib.get_option set (Global.simpset()) in
  Simplib.simpA_tac ?cntrl ?ignore set0 ?add ?a rules 
(** [simpA_tac ?cntrl ?ignore ?asms ?set ?add ?a rules goal]
    
    Simplify assumptions.
*)
let simpA ?set ?a rules = 
  let set0 = Lib.get_option set (Global.simpset()) in
  Simplib.simpA_tac set0 ?a rules 

let simpC_tac ?cntrl ?ignore ?set ?add ?c rules =
  let set0 = Lib.get_option set (Global.simpset()) in
  Simplib.simpC_tac ?cntrl ?ignore set0 ?add ?c rules 

let simpC ?set ?c rules = 
  let set0 = Lib.get_option set (Global.simpset()) in
  Simplib.simpC_tac set0 ?c rules 

let simp_all_tac ?cntrl ?ignore ?set ?add thms =
  let set0 = Lib.get_option set (Global.simpset()) in
  Simplib.simp_all_tac ?cntrl ?ignore set0 ?add thms

let simp_all_tac ?cntrl ?ignore ?set ?add thms =
  let set0 = Lib.get_option set (Global.simpset()) in
  Simplib.simp_all_tac ?cntrl ?ignore set0 ?add thms
(** [simp_all_tac ?cntrl ?ignore ?asms ?set ?add rules goal]
    
    Simplify each formula in the subgoal.
*)

let simp_all ?set thms =
  let set0 = Lib.get_option set (Global.simpset()) in
  Simplib.simp_all_tac set0 thms
(** [simp_all]: Shorthand for {!Simplib.simp_all_tac}.
    
    @raise No_change If no change is made.
*)


let simp_tac ?cntrl ?ignore ?set ?add ?f thms =
  let set0 = Lib.get_option set (Global.simpset()) in
  Simplib.simp_tac ?cntrl ?ignore set0 ?add ?f thms
(** [simp_tac ?cntrl ?ignore ?asms ?set ?add ?f rules goal]
    
    Simplifier tactic.
*)


let simp ?set ?f g =
  let set0 = Lib.get_option set (Global.simpset()) in
  Simplib.simp set0 ?f g
(** [simp ?f]: Shorthand for {!Simplib.simp_tac}.
    
    @raise No_change If no change is made.
*)

