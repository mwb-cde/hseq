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

(*** A minimal base theory ***)

let rec swap f = (fun x y -> f y x)

let builder ?(save=false) ctxt =
  begin
    let sctxt1 = begin_theory ctxt Lterm.base_thy []
    in
    (** Types *)
    let (sctxt2, _) = 
      typedef sctxt1 <:def<: ('a, 'b)FUN >>
        ~pp:(100, infixr, Some("->"))
    in
    let (sctxt3, _) = typedef sctxt2 <:def<: bool >> in 
    let (sctxt4, _) = typedef sctxt3 <:def<: ind >>  in

    (** Terms *)
    let (sctxt5, _, _) = 
      let prec = BoolPP.negation_pprec.Printer.prec
      and fixity = BoolPP.negation_pprec.Printer.fixity
      in 
      declare sctxt4
        (Commands.read_unchecked 
	   ((Ident.name_of Lterm.notid)^": bool -> bool"))
        ~pp:(prec, fixity, Some "~") 
    in 
    let sctxt6 = 
      let prec = BoolPP.negation_pprec.Printer.prec
      and fixity = BoolPP.negation_pprec.Printer.fixity
      in 
      add_term_pp sctxt5
        Lterm.notid prec fixity (Some "not")
    in 
    (** Equality *)
    let (sctxt7, _, _) =
      declare sctxt6
        (Commands.read_unchecked 
	   ((Ident.name_of Lterm.equalsid)^": 'a -> 'a -> bool"))
        ~pp:(200, infixl, (Some "=")) 
    in 
    (** Conjunction *)
    let (sctxt8, _, _) =
      declare sctxt7
        (Commands.read_unchecked 
	   ((Ident.name_of Lterm.andid)^": bool -> bool -> bool"))
        ~pp:(185, infixr, Some "and") 
    in  
    let sctxt9 = 
      add_term_pp sctxt8 Lterm.andid 185 infixr (Some "&")
    in 
    (** Disjunction *)
    let (sctxt10, _) =
      define sctxt9
        (Commands.read_defn ((Ident.name_of Lterm.orid)
			     ^" x y = (not ((not x) and (not y)))"))
        ~pp:(190, infixr, Some "or") 
    in 
    let sctxt11 = 
      add_term_pp sctxt10 Lterm.orid 190 infixr (Some "|")
    in 
    (** Implication *)
    let (sctxt12, _) = 
      define sctxt11
        (Commands.read_defn ((Ident.name_of Lterm.impliesid)
			     ^" x y = (not x) or y"))
        ~pp:(195, infixr, Some "=>") 
    in 
    (** Equivalance *)
    let (sctxt13, _) = 
      define sctxt12
        (Commands.read_defn ((Ident.name_of Lterm.iffid)
			     ^" x y = (x => y) and (y => x)"))
        ~pp:(180, infixn, Some "iff") 
    in 

    (** Axioms *)

    (** False definition *)
    let (ctxt14, _) = axiom sctxt13
      "false_def" << false = (not true)>> in 

    (** Boolean cases *)
    let (ctxt15, _) = 
      axiom ctxt14 "bool_cases" 
      << !x: (x = true) or (x = false) >> 
    in 
    (** Equality *)
    let (ctxt16, _) = axiom ctxt15 "eq_refl" << !x: x = x >> in 
    let (sctxt17, _) =
      define ctxt16
      <:def< one_one f = !x1 x2: ((f x1) = (f x2)) => (x1 = x2) >> 
    in 
    let (sctxt18, _) = 
      define sctxt17 <:def< onto f = !y: ?x: y = (f x) >> 
    in 
    let (ctxt19, _) = 
      axiom sctxt18 "infinity_ax" 
      << ?(f: ind -> ind): (one_one f) and (onto f)>> 
    in 
    let (ctxt20, _) = 
      axiom ctxt19 "extensionality" 
      << !f g: (!x: (f x) = (g x)) => (f = g)>> 
    in 
    (** Specification operator (epsilon) *)
    let (sctxt21, _, _) = 
      declare ctxt20
      <<epsilon: ('a -> bool) -> 'a>> 
    in 
    let (ctxt22, _) = 
      axiom sctxt21 "epsilon_ax"
      << !P: (?x: P x) => (P(epsilon P))>> 
    in 

    (** Conditional *)
    let (sctxt23, _) = 
      define ctxt22
      <:def< 
        IF b t f = (epsilon (%z: (b => (z = t)) and ((not b) => (z = f))))
      >> 
    in 
    (** Any value *)
    let (sctxt24, _) = define sctxt23 <:def< any = epsilon (%a: true)>> 
    in 
    (** Unique existence *)
    let (sctxt25, _) =
      define sctxt24
      <:def<
        EXISTS_UNIQUE p = 
      (? x: (p x)) and (! x y : ((p x) and (p y)) => (x = y))
        >> in 

    let ctxt26 = end_theory sctxt25 ~save:save ()
    in 
    ctxt26
  end

(*
let init() = 
  Global.Init.set_base_thy_builder (builder ~save:false)
*)
let init() = failwith("baseTheory.init no longer supported")
