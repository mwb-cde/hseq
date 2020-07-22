(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

begin_theory "base" [];;

(** Types *)

(**
   Grammar:
   type ::=
           bool
         | ind
         | ('a ->'b)      (100, infixr, "FUN")

   Default precedence=10, assoc = non_assoc, fixity=non_fix
*)

let _ = typedef [] <:def<: ('a, 'b)FUN >> ~pp:(100, infixr, Some("->"));;
let _ = typedef [] <:def<: bool >> ;;
let _ = typedef [] <:def<: ind >>;;

(** Terms *)

(**
   Grammar:
   term ::=
    ~x                 (205, prefix, Lterm.negid) {Boollib.PP}
   | not x             (205, prefix, Lterm.negid) {Boollib.PP}
   | x = y             (200, infixl, Lterm.equalsid)
   | x => y            (195, infixr, Lterm.impliesid)
   | x | y             (190, infixr, Lterm.impliesid)
   | x or y            (190, infixr, Lterm.impliesid)
   | x & y             (185, infixr, Lterm.impliesid)
   | x and y           (185, infixr, Lterm.impliesid)
   | x iff y           (180, infixr, Lterm.impliesid)

   | f x               (90, nonfix) {Printer}  (Function application)
   | % x: P            (60, nonfix) {Printer, Parser} (Lambda abstraction)
   | ! x: P            (55, nonfix) {Printer, Parser} (Universal quantifier)
   | ? x: P            (55, nonfix) {Printer, Parser} (Existential quantifier)

   | x, y              (10, infixr) {pariLib}  (Pair constructor)
   | ?! x: P           (10, nonfix) {BoolPP} (Unique existance)
   | @ x: P            (10, nonfix) {BoolPP}  (Choice quantifier)

   Default precedence=10, assoc = non_assoc, fixity=non_fix
*)

(**
   Negation:
   Negation has its own printer [BoolLib.PP.negation_printer]
*)
let _ =
  let prec = BoolPP.negation_pprec.Printkit.prec
  and fixity = BoolPP.negation_pprec.Printkit.fixity
  in
  declare
    (Commands.read_unchecked
       ((Ident.name_of Lterm.notid)^": bool -> bool"))
    ~pp:(prec, fixity, Some "~");;

let _ =
  let prec = BoolPP.negation_pprec.Printkit.prec
  and fixity = BoolPP.negation_pprec.Printkit.fixity
  in
  add_term_pp "not" prec fixity (Some "not");;

(** Equality *)
let _ =
declare
(Commands.read_unchecked
   ((Ident.name_of Lterm.equalsid)^": 'a -> 'a -> bool"))
  ~pp:(200, infixl, (Some "="));;

(** Conjunction *)
let _ =
declare
(Commands.read_unchecked ((Ident.name_of Lterm.andid)^":bool->bool->bool"))
  ~pp:(185, infixr, Some "and");;

let _ = add_term_pp "and" 185 infixr (Some "&");;

(** Disjunction *)
let or_def =
define
(Commands.read_defn ((Ident.name_of Lterm.orid)
            ^" x y = (not ((not x) and (not y)))"))
  ~pp:(190, infixr, Some "or");;

let _ = add_term_pp "or" 190 infixr (Some "|");;

(** Implication *)
let implies_def =
define
  (hdefn ((Ident.name_of Lterm.impliesid)^" x y = (not x) or y"))
  ~pp:(195, infixr, Some "=>");;

(** Equivalance *)

let iff_def =
define
  (hdefn
     ((Ident.name_of Lterm.iffid)
      ^" x y = (x => y) and (y => x)"))
  ~pp:(180, infixn, Some "iff");;


(** Axioms *)

(** False definition *)
let false_def = axiom "false_def" (!# " false = (not true) ");;

(** Boolean cases *)
let bool_cases_ax =
  axiom "bool_cases" (!# " !x: (x=true) or (x=false) ");;

(** Equality *)

let eq_refl_ax =
  axiom "eq_refl" (!# " !x: x=x ");;

let one_one_def =
  define
    (?<# "one_one f = !x1 x2: ((f x1) = (f x2)) => (x1=x2) ");;

let onto_def =
  define (?<# "onto f = !y: ?x: y=(f x) ");;

let infinity_ax =
  axiom "infinity_ax" (!# " ?(f: ind -> ind): (one_one f) and (onto f) ");;

let extensionality =
  axiom "extensionality"  (!# " !f g: (!x: (f x) = (g x)) => (f = g) ");;

(** Specification operator (epsilon) *)
let _ = declare (!# " epsilon: ('a -> bool) -> 'a ");;

let epsilon_ax =
  axiom "epsilon_ax" (!# " !P: (?x: P x) => (P(epsilon P)) ");;

(** Conditional *)
let cond_def =
  define
    (?<# "IF b t f = (epsilon (%z: (b => (z=t)) and ((not b) => (z=f)))) ");;

(** Any value *)
let any_def = define (?<# "any = epsilon (%a: true) ");;

(** Unique existence *)
let exists_unique_def =
  define
    (?<#
       "EXISTS_UNIQUE p =
        (? x: (p x)) and (! x y : ((p x) and (p y)) => (x = y))");;

end_theory();;

let _ = Display.print_theory (theory "");;
