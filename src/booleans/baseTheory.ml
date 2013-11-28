(*----
  Name: baseTheory.ml
  Copyright M Wahab 2006-2010
  Author: M Wahab  <mwb.cde@googlemail.com>

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

open Commands
open Tactics
open Lib.Ops

(** {5 The minimal context} 

    Has the minimal parser and printer information needed for the base theory.
*)

let context() = BoolPP.basethy_context()

(*** A minimal base theory ***)

let swap f = (fun x y -> f y x)
let read ctxt x = Commands.read ~ctxt:ctxt x
let read_defn ctxt x = Commands.read_defn ~ctxt:ctxt x
let name_of = Ident.name_of

let builder ?(save=false) ctxt =
  begin
    let ctxt0 =
      Context.set_thy_suffix 
        (BoolPP.basethy_context()) (Context.thy_suffix ctxt)
    in
    let ctxt1 = begin_theory ctxt0 Lterm.base_thy []
    in
    (** Types *)
    let (ctxt2, _) = 
      typedef ctxt1 <:def<: ('a, 'b)FUN >>
        ~pp:(100, infixr, Some("->"))
    in
    let (ctxt3, _) = typedef ctxt2 <:def<: bool >> in 
    let (ctxt4, _) = typedef ctxt3 <:def<: ind >>  in

    (** Terms *)
    let (ctxt5, _, _) = 
      let prec = BoolPP.negation_pprec.Printer.prec
      and fixity = BoolPP.negation_pprec.Printer.fixity
      in 
      declare ctxt4
        (read ctxt4
	   ((name_of Lterm.notid)^": bool -> bool"))
        ~pp:(prec, fixity, Some "~")  
    in 
    let ctxt6 = 
        let prec = BoolPP.negation_pprec.Printer.prec
        and fixity = BoolPP.negation_pprec.Printer.fixity
        in 
        add_term_pp ctxt5 Lterm.notid prec fixity (Some "not")
    in 
    (** Equality *)
    let (ctxt7, _, _) =
      declare ctxt6
        (Commands.read ~ctxt:ctxt6
	   ((name_of Lterm.equalsid)^": 'a -> 'a -> bool"))
        ~pp:(200, infixl, (Some "="))
    in 
    (** Conjunction *)
    let (ctxt8, _, _) =
      declare ctxt7
        (read ctxt7
	   ((name_of Lterm.andid)^": bool -> bool -> bool"))
        ~pp:(185, infixr, Some "and")  
    in  
    let ctxt9 = 
      add_term_pp ctxt8 Lterm.andid 185 infixr (Some "&") 
    in 
    (** Disjunction *)
    let (ctxt10, _) =
      define ctxt9
        (read_defn ctxt9
           ((name_of Lterm.orid)
	    ^" x y = (not ((not x) and (not y)))"))
        ~pp:(190, infixr, Some "or")  
    in 
    let ctxt11 = 
      add_term_pp ctxt10 Lterm.orid 190 infixr (Some "|") 
    in 

    (** Implication *)
    let (ctxt12, _) = 
      define ctxt11
        (read_defn ctxt11
           ((name_of Lterm.impliesid)^" x y = (or (not x) y)"))
        ~pp:(195, infixr, Some "=>") 
    in 
    (** Equivalance *)
    let (ctxt13, _) = 
      define ctxt12
        (read_defn ctxt12
           ((name_of Lterm.iffid)^" x y = (x => y) and (y => x)"))
        ~pp:(180, infixn, Some "iff")  
    in 

    (** Axioms *)

    (** False definition *)
    let (ctxt14, _) = 
      axiom ctxt13 "false_def" 
        (read ctxt13 
           ((name_of Lterm.falseid)^" = (not true)"))
    in 

    (** Boolean cases *)
    let (ctxt15, _) = 
      axiom ctxt14 "bool_cases" 
        (read ctxt14  "!x: (x = true) or (x = false)")
    in 

    (** Equality *)
    let (ctxt16, _) = 
      axiom ctxt15 "eq_refl" << !x: x = x >> 
    in 

    let (ctxt17, _) =
      define ctxt16
      <:def< one_one f = !x1 x2: ((f x1) = (f x2)) => (x1 = x2) >>
    in 

    let (ctxt18, _) = 
      define ctxt17 
      <:def< onto f = !y: ?x: y = (f x) >>
    in 
    let (ctxt19, _) = 
      axiom ctxt18 "infinity_ax" 
      << ?(f: ind -> ind): (one_one f) and (onto f) >>
    in 
    let (ctxt20, _) = 
      axiom ctxt19 "extensionality" 
      << !f g: (!x: (f x) = (g x)) => (f = g) >>
    in 
    (** Specification operator (epsilon) *)
    let (ctxt21, _, _) = 
      declare ctxt20
      << epsilon: ('a -> bool) -> 'a >>
    in 
    let (ctxt22, _) = 
      axiom ctxt21 "epsilon_ax"
      << !P: (?x: P x) => (P(epsilon P)) >>
    in 
    (** Conditional *)
    let (ctxt23, _) = 
      define ctxt22 <:def<
    IF b t f = (epsilon (%z: (b => (z = t)) and ((not b) => (z = f)))) >>
    in 
    (** Any value *)
    let (ctxt24, _) = 
      define ctxt23 
      <:def< any = epsilon (%a: true) >>
    in 
    (** Unique existence *)
    let (ctxt25, _) =
      define ctxt24
      <:def< 
           EXISTS_UNIQUE p = 
      (? x: (p x)) and (! x y : ((p x) and (p y)) => (x = y)) >>
    in 
    let ctxt26 = end_theory ctxt25 ~save:save ()
    in 
    ctxt26

  end

