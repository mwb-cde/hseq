
open Term
open Format
open Result

let cfun_string c =
  match c with 
    "not" -> Format.print_string "not"
  | "and" -> Format.print_string "and"
  | "or" -> Format.print_string "or"
  | "implies" -> Format.print_string " => "
  | "iff" -> Format.print_string "<=>"
  | "equals" -> Format.print_string "="
  | x -> Format.print_string x

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
  open_box 0; 
  Gtypes.print (Global.pp_info())  x; 
  close_box()

let print_sqnt x = 
  Logic.print_sqnt (Global.pp_info()) x
let print_node x = 
  Logic.print_node (Global.pp_info()) x
let print_branch x = 
  Logic.print_branch (Global.pp_info()) x
    

let print_thm t = 
  open_box 3; Format.print_string "|- ";
  print_term (Formula.term_of_form (Logic.dest_thm t));
  close_box()


let print_prf p = 
  let print_subgoals i = 
    Format.print_int i; 
    if i>1 
    then Format.print_string " subgoals"
    else Format.print_string " subgoal"
  in 
  let g = Goals.curr_goal p
  in 
  let subgls = Logic.num_of_subgoals g
  in 
  open_box 3; 
  Format.print_string "Goal: "; 
  print_term 
    (Formula.term_of_form (Logic.get_goal g));
  close_box();
  Format.print_newline();
  (match subgls with
    0 -> (open_box 0; 
	  Format.print_string "No subgoals"; 
	  close_box();
	  print_newline())
  | _ -> 
      (open_box 0;
       print_subgoals subgls;
       close_box ();
       print_newline();
       print_sqnt (Goals.curr_sqnt p)))

let print_defn def =
  let n, ty, th = Defn.dest_defn def
  in 
  Format.open_box 3;
  Format.open_box 3;
  print_fnident (Basic.mk_long Basic.null_thy (Basic.name n));
  Format.print_string ":";
  Format.print_space();
  print_type ty;
  Format.close_box();
  Format.print_cut();
  print_thm th;
  Format.close_box()
  
let print_subst tenv f= 
  open_box 3;
  (Hashtbl.iter 
     (fun x y -> 
       Format.print_string ("("^(f x)^" = "^(f y)^"):");
       Format.print_space())
     tenv);
  close_box();
  Format.print_newline()
    

let print_error r = (r#print) (Global.pp_info())

let print_theory x = 
  Theory.print (Global.pp_info()) x
