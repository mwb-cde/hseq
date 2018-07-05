(*----
  Name: display.ml
  Copyright Matthew Wahab 2005-2018
  Author: Matthew Wahab <mwb.cde@gmail.com>

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

open Term
open Format
open Report

let cfun_string c =
  match c with
    | "not" -> Format.printf "@[not@ @]"
    | "and" -> Format.printf "@[and@ @]"
    | "or" -> Format.printf "@[or@ @]"
    | "implies" -> Format.printf "@[=>@ @]"
    | "iff" -> Format.printf "@[<=>@ @]"
    | "equals" -> Format.printf "@[=@ @]"
    | x -> Format.printf "@[%s@ @]" x

let print_fnident x = Printkit.print_ident x

let print_term ppinf x =
  open_box 0;
  Printers.print_term ppinf x;
  close_box()

let print_formula ppinf x =
  open_box 0;
  Printers.print_term ppinf (Formula.term_of x);
  close_box()

let rec print_type ppinf x =
  Format.printf "@[";
  Printers.print_type ppinf x;
  Format.printf "@]"

let print_sqnt ppinf x =
  Logic.print_sqnt ppinf x
let print_node ppinf x =
  Logic.print_node ppinf x
let print_branch ppinf x =
  Logic.print_branch ppinf x

let print_thm ppinf t = Logic.print_thm ppinf t

let print_prf ppinf p = Goals.Proof.print ppinf p
let print_prfstk ppinf p = Goals.ProofStack.print ppinf p

let print_termdefn ppinf def =
  let n, ty, th = Logic.Defns.dest_termdef def
  in
  Format.printf "@[";
  Format.printf "@[";
  print_fnident (Ident.mk_long Ident.null_thy (Ident.name_of n));
  Format.printf ":@ ";
  print_type ppinf ty;
  Format.printf "@],@ ";
  print_thm ppinf th;
  Format.printf "@]"

let print_termdecln ppinf def =
  let n, ty = Logic.Defns.dest_termdecln def
  in
  Format.printf "@[";
  print_fnident (Ident.mk_long Ident.null_thy (Ident.name_of n));
  Format.printf ":@ ";
  print_type ppinf ty;
  Format.printf "@]"

let print_defn ppinf def =
  Logic.Defns.print_cdefn ppinf def

let print_subst tenv f=
  Format.printf "@[<2>";
  Hashtbl.iter
    (fun x y -> Format.printf "@[(%s =@ %s):@]@ " (f x) (f y))
    tenv;
  Format.printf "@]"

let fprint_error fmt ppinf r = r.Report.printer fmt ppinf
let print_error = fprint_error Format.std_formatter

let print_type_error ppinf err =
  Printers.print_type_error Format.std_formatter ppinf err

let print_report ppinf depth err =
  Report.print_error ppinf depth err

let print_theory ppinf x =
  Theory.print ppinf x
