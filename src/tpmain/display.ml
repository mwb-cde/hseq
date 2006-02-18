(*-----
 Name: display.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)


open Term
open Format
open Result

let cfun_string c =
  match c with 
    "not" -> Format.printf "@[not@ @]"
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

let print_prf p = 
  let g = Goals.Proof.top p
  in 
  let subgls = Logic.get_subgoals g
  in 
  Format.printf "@[<v>Goal ";
  Format.printf "@[";
  print_term 
    (Formula.term_of (Logic.get_goal g));
  Format.printf "@]@,";
  (match subgls with
    [] -> Format.printf "@[No subgoals@]@,"
  | (x::_) -> 
      let num_gls = (List.length subgls)
      in 
      Format.printf "@[%i %s@]@," 
	num_gls (if num_gls>1 then "subgoals" else "subgoal");
       print_sqnt x);
  Format.printf "@]"
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
(*
  if(Logic.Defns.is_termdef def)
  then print_termdefn def
  else 
    if (Logic.Defns.is_termdecln def)
    then print_termdecln def
    else ()
*)
  
let print_subst tenv f= 
  Format.printf "@[<2>";
  (Hashtbl.iter 
     (fun x y -> Format.printf "@[(%s =@ %s):@]@ " (f x) (f y))
     tenv);
  Format.printf "@]"
    
let print_error r = (r#print) (Global.PP.info())

let print_theory x = 
  Theory.print (Global.PP.info()) x
