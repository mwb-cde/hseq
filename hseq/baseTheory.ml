(*----
  Copyright (c) 20062021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
let read ctxt x = Commands.read ctxt x
let read_type ctxt x = Commands.read_type ctxt x

let read_defn ctxt x = Commands.read_defn ctxt x
let read_type_defn ctxt x = Commands.read_type_defn ctxt x

let name_of = Ident.name_of

let builder save ctxt =
  begin
    let ctxt0 =
      Context.set_thy_suffix
        (BoolPP.basethy_context()) (Context.thy_suffix ctxt)
    in
    let ctxt1 = begin_theory ctxt0 Lterm.base_thy []
    in
    (** Types *)
    let (ctxt2, _) =
      typedef ctxt1 [Option.Symbol(100, infixr, Some("->"))]
        (read_type_defn ctxt1 "('a, 'b)FUN")
    in
    let (ctxt3, _) = typedef ctxt2 [] (read_type_defn ctxt2 "bool") in
    let (ctxt4, _) = typedef ctxt3 [] (read_type_defn ctxt3 "ind")  in

    (** Terms *)
    let (ctxt5, _, _) =
      let prec = BoolPP.negation_pprec.Printkit.prec
      and fixity = BoolPP.negation_pprec.Printkit.fixity
      in
      declare ctxt4 [Option.Symbol(prec, fixity, Some "~")]
        (read ctxt4
           ((name_of Lterm.notid)^": bool -> bool"))
    in
    let ctxt6 =
        let prec = BoolPP.negation_pprec.Printkit.prec
        and fixity = BoolPP.negation_pprec.Printkit.fixity
        in
        add_term_pp ctxt5 Lterm.notid prec fixity (Some "not")
    in
    (** Equality *)
    let (ctxt7, _, _) =
      declare ctxt6
        [Option.Symbol(200, infixl, Some "=")]
        (Commands.read ctxt6
           ((name_of Lterm.equalsid)^": 'a -> 'a -> bool"))
    in
    (** Conjunction *)
    let (ctxt8, _, _) =
      declare ctxt7
        [Option.Symbol(185, infixr, Some "and")]
        (read ctxt7
           ((name_of Lterm.andid)^": bool -> bool -> bool"))
    in
    let ctxt9 =
      add_term_pp ctxt8 Lterm.andid 185 infixr (Some "&")
    in
    (** Disjunction *)
    let (ctxt10, _) =
      define ctxt9
        [Option.Symbol(190, infixr, Some "or")]
        (read_defn ctxt9
           ((name_of Lterm.orid)
            ^" x y = (not ((not x) and (not y)))"))
    in
    let ctxt11 =
      add_term_pp ctxt10 Lterm.orid 190 infixr (Some "|")
    in

    (** Implication *)
    let (ctxt12, _) =
      define ctxt11
        [Option.Symbol(195, infixr, Some "=>")]
        (read_defn ctxt11
           ((name_of Lterm.impliesid)^" x y = (or (not x) y)"))
    in
    (** Equivalance *)
    let (ctxt13, _) =
      define ctxt12
        [Option.Symbol(180, infixn, Some "iff")]
        (read_defn ctxt12
           ((name_of Lterm.iffid)^" x y = (x => y) and (y => x)"))
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
      axiom ctxt15 "eq_refl" (read ctxt15 "!x: x = x")
    in

    let (ctxt17, _) =
      define ctxt16 []
        (read_defn ctxt16 "one_one f = !x1 x2: ((f x1) = (f x2)) => (x1 = x2)")
    in

    let (ctxt18, _) =
      define ctxt17 []
        (read_defn ctxt17 "onto f = !y: ?x: y = (f x)")
    in
    let (ctxt19, _) =
      axiom ctxt18 "infinity_ax"
        (read ctxt18 "?(f: ind -> ind): (one_one f) and (onto f)")
    in
    let (ctxt20, _) =
      axiom ctxt19 "extensionality"
        (read ctxt19 "!f g: (!x: (f x) = (g x)) => (f = g)")
    in
    (** Specification operator (epsilon) *)
    let (ctxt21, _, _) =
      declare ctxt20 []
        (read ctxt20 "epsilon: ('a -> bool) -> 'a")
    in
    let (ctxt22, _) =
      axiom ctxt21 "epsilon_ax"
        (read ctxt21 "!P: (?x: P x) => (P(epsilon P))")
    in
    (** Conditional *)
    let (ctxt23, _) =
      define ctxt22 []
      (read_defn ctxt22
         "IF b t f = (epsilon (%z: (b => (z = t)) and ((not b) => (z = f))))")
    in
    (** Any value *)
    let (ctxt24, _) =
      define ctxt23 []
        (read_defn ctxt23 "any = epsilon (%a: true)")
    in
    (** Unique existence *)
    let (ctxt25, _) =
      define ctxt24 []
        (read_defn ctxt24
           ("EXISTS_UNIQUE p "
            ^"= (? x: (p x)) and (! x y : ((p x) and (p y)) => (x = y))"))
    in
    let ctxt26 = end_theory ctxt25 save
    in
    ctxt26

  end
