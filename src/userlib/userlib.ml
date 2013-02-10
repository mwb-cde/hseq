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

  (** State *)
  let state = Userstate.state

  (** Initialise the global state. *)
  let init () = Userstate.init()

  (** Scope *)
  let scope = Userstate.scope
  let set_scope = Userstate.set_scope

  (** Context *)
  let context = Userstate.context
  let scoped = Userstate.scoped
  let set_context = Userstate.set_context

  let theories() = Context.Thys.get_theories (context())
  let current() = Context.Thys.current (context())
  let current_name () = Context.Thys.current_name (context())

  (** Pretty-printing *)
  let ppinfo = Userstate.ppinfo
  let set_ppinfo = Userstate.set_ppinfo

  (** Parser tables *)
  let parsers = Userstate.parsers
  let set_parsers = Userstate.set_parsers

end

(** Parsers and printers *)
module PP =
struct

  (** Tables access *)
  let overloads () = Parser.Table.get_overloads (Global.parsers())

  let catch_parse_error e a = 
    try (e a)
    with 
    | Parser.ParsingError x -> raise (Report.error x)
    | Lexer.Lexing _ -> raise (Report.error ("Lexing error: "^a))
      
  let overload_lookup s = 
    let thydb s = Thydb.get_id_options s (Global.theories())
    and parserdb s = 
      Parser.get_overload_list ~ovltbl:(overloads()) s
    in 
    try parserdb s
    with Not_found -> thydb s

  let expand_term scp t = 
    let lookup = Pterm.Resolver.make_lookup scp overload_lookup in 
    let (new_term, env) = Pterm.Resolver.resolve_term scp lookup t
    in 
    new_term

  let expand_type_names scp t = Gtypes.set_name ~strict:false scp t

  let expand_typedef_names scp t=
    match t with
    | Grammars.NewType (n, args) -> 
      Defn.Parser.NewType (n, args) 
    | Grammars.TypeAlias (n, args, def) ->
      Defn.Parser.TypeAlias(n, args, expand_type_names scp def)
    | Grammars.Subtype (n, args, def, set) ->
      Defn.Parser.Subtype(n, args, 
			  expand_type_names scp def, 
			  expand_term scp set)

  let expand_defn scp (plhs, prhs) =
    let rhs = expand_term scp prhs
    and ((name, ty), pargs) = plhs
    in 
    let args = List.map Pterm.to_term pargs
    in 
    (((name, ty), args), rhs)

  let mk_term scp pt = expand_term scp pt

  let read str= 
    mk_term (Global.scope()) (catch_parse_error Parser.read_term str)

  let read_unchecked x =
    catch_parse_error (Pterm.to_term <+ Parser.read_term) x

  let read_defn x =
    let (lhs, rhs) = catch_parse_error (Parser.read defn_parser) x
    in 
    expand_defn (Global.scope()) (lhs, rhs)

  let read_type_defn x =
    let pdefn = catch_parse_error (Parser.read Parser.typedef_parser) x
    in 
    expand_typedef_names (Global.scope()) pdefn
      
  let read_type x = 
    expand_type_names (Global.scope()) (catch_parse_error Parser.read_type x)

  let read_identifier x = 
    catch_parse_error (Parser.read Parser.identifier_parser) x

end

(***
    Utility functions
***)

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

(***
    Printing and parsing 
***)

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


let add_term_pp s ?(pos=Lib.First) i f sym = 
  Commands.add_term_pp (Global.context())
    (Ident.mk_long (Global.current_name()) s) ~pos:pos i f sym
let get_term_pp s = 
  Commands.get_term_pp (Ident.mk_long (Global.current_name()) s)
let remove_term_pp s = 
  Commands.remove_term_pp (Global.context())
    (Ident.mk_long (Global.current_name()) s)

let add_type_pp s = 
  Commands.add_type_pp (Global.context())
    (Ident.mk_long (Global.current_name()) s)
let get_type_pp s = 
  Commands.get_type_pp (Ident.mk_long (Global.current_name()) s)
let remove_type_pp s =
  Commands.remove_type_pp (Global.context())
    (Ident.mk_long (Global.current_name()) s)

(***
    Theories 
***)

let begin_theory = Commands.begin_theory
let end_theory = Commands.end_theory
let open_theory = Commands.open_theory
let close_theory = Commands.close_theory

(*** Theory properties ***)

let parents = Commands.parents
let add_file = Commands.add_file
let remove_file = Commands.remove_file

(*** Type declaration and definition ***)

let typedef ?pp ?(simp=true) ?thm ?rep ?abs tydef = 
  let defn = 
    Commands.typedef ?pp:pp ~simp:simp ?thm:thm ?rep:rep ?abs:abs tydef
  in 
  begin
    if simp && (Logic.Defns.is_subtype defn)
    then 
      let tyrec = Logic.Defns.dest_subtype defn in 
      let rt_thm = tyrec.Logic.Defns.rep_type
      and rti_thm = tyrec.Logic.Defns.rep_type_inverse
      and ati_thm = tyrec.Logic.Defns.abs_type_inverse
      in 
      List.iter Simplib.add_simp [rt_thm; rti_thm; ati_thm]
    else ()
  end;
  defn

(*** Term declaration and definition ***)

let define ?pp ?(simp=false) df =
  let ret = Commands.define ?pp ~simp:simp df
  in 
  if simp
  then 
    let (_, _, thm) = Logic.Defns.dest_termdef ret
    in 
    Simplib.add_simp thm; ret
  else ret

let declare = Commands.declare

(*** Axioms and theorems ***)

let axiom ?(simp=false) n t =
  let thm = Commands.axiom ~simp:simp n t
  in 
  if simp
  then (Simplib.add_simp thm; thm)
  else thm


let save_thm ?(simp=false) n thm =
  let ret = Commands.save_thm ~simp:simp n thm
  in 
  if simp 
  then (Simplib.add_simp ret; ret)
  else ret

let prove_thm ?(simp=false) n t tac =
  let thm = Commands.prove_thm ~simp:simp n t tac
  in 
  if simp 
  then (Simplib.add_simp thm; thm)
  else thm

let theorem = prove_thm
let lemma = theorem

(***
    Information access
***)

let theory = Commands.theory
let theories = Commands.theories

let defn = Commands.defn
let thm = Commands.thm

let scope = Commands.scope
let goal_scope = Goals.goal_scope

(***
    Proof commands
***)

let prove = Commands.prove
let by x = (catch_errors Goals.by_com) x
let qed = Commands.qed

(*** 
     Initialising functions
***)

let init = Global.init
let reset = Global.init
