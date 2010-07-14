(*----
  Name: display.ml
  Copyright M Wahab 2005-2010
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

let print_fnident x = Printer.print_ident x

let print_term x = 
  open_box 0;
  Term.print (Global.PP.info()) x;
  close_box()

let print_formula x = 
  open_box 0;
  Term.print (Global.PP.info()) (Formula.term_of x);
  close_box()

let rec print_type x = 
  Format.printf "@[";
  Gtypes.print (Global.PP.info())  x; 
  Format.printf "@]"

let print_sqnt x = 
  Logic.print_sqnt (Global.PP.info()) x
let print_node x = 
  Logic.print_node (Global.PP.info()) x
let print_branch x = 
  Logic.print_branch (Global.PP.info()) x
    
let print_thm t = Logic.print_thm (Global.PP.info()) t

let print_prf p = Goals.Proof.print (Global.PP.info()) p
let print_prfstk p = Goals.ProofStack.print (Global.PP.info()) p

let print_termdefn def = 
  let n, ty, th = Logic.Defns.dest_termdef def
  in 
  Format.printf "@[";
  Format.printf "@[";
  print_fnident (Ident.mk_long Ident.null_thy (Ident.name_of n));
  Format.printf ":@ ";
  print_type ty;
  Format.printf "@],@ ";
  print_thm th;
  Format.printf "@]"

let print_termdecln def = 
  let n, ty = Logic.Defns.dest_termdecln def
  in 
  Format.printf "@[";
  print_fnident (Ident.mk_long Ident.null_thy (Ident.name_of n));
  Format.printf ":@ ";
  print_type ty;
  Format.printf "@]"


let print_defn def =
  Logic.Defns.print_cdefn (Global.PP.info()) def
    
let print_subst tenv f= 
  Format.printf "@[<2>";
  Hashtbl.iter 
    (fun x y -> Format.printf "@[(%s =@ %s):@]@ " (f x) (f y))
    tenv;
  Format.printf "@]"
    
let print_error r = (r#print) (Global.PP.info())

let print_theory x = 
  Theory.print (Global.PP.info()) x
