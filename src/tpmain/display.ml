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
  Term.print (Global.pp_info()) x;
  close_box()

let print_formula x = 
  open_box 0;
  Term.print (Global.pp_info()) (Formula.dest_form x);
  close_box()

let rec print_type x = 
  Format.printf "@[";
  Gtypes.print (Global.pp_info())  x; 
  Format.printf "@]"

let print_sqnt x = 
  Logic.print_sqnt (Global.pp_info()) x
let print_node x = 
  Logic.print_node (Global.pp_info()) x
let print_branch x = 
  Logic.print_branch (Global.pp_info()) x
    
let print_thm t = Logic.print_thm (Global.pp_info()) t

let print_prf p = 
  let g = Goals.curr_goal p
  in 
  let subgls = Logic.num_of_subgoals g
  in 
  Format.printf "@[<v>Goal ";
  Format.printf "@[";
  print_term 
    (Formula.term_of_form (Logic.get_goal g));
  Format.printf "@]@,";
  (match subgls with
    0 -> Format.printf "@[No subgoals@]@,"
  | _ -> 
      (Format.printf "@[%i %s@]@," subgls
	 (if subgls>1 
	 then "subgoals" else "subgoal");
       print_sqnt (Goals.curr_sqnt p)));
  Format.printf "@]"

let print_defn def =
  let n, ty, th = Defn.dest_defn def
  in 
  Format.printf "@[";
  Format.printf "@[";
  print_fnident (Basic.mk_long Basic.null_thy (Basic.name n));
  Format.printf ":@ ";
  print_type ty;
  Format.printf "@],@ ";
  print_thm th;
  Format.printf "@]"
  
let print_subst tenv f= 
  Format.printf "@[<2>";
  (Hashtbl.iter 
     (fun x y -> Format.printf "@[(%s =@ %s):@]@ " (f x) (f y))
     tenv);
  Format.printf "@]"
    
let print_error r = (r#print) (Global.pp_info())

let print_theory x = 
  Theory.print (Global.pp_info()) x
