
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
  Term.print (Tpenv.pp_info()) x;
  close_box()

let print_formula x = 
  open_box 0;
  Term.print (Tpenv.pp_info()) (Formula.dest_form x);
  close_box()

let rec print_type x = 
  open_box 0; 
  Gtypes.print (Tpenv.pp_info())  x; 
  close_box()


let print_sqnt sq = 
  let nice = Settings.get_nice_sequent()
  in let nice_prefix = 
    if nice then (!Settings.nice_sequent_prefix)
    else "-"
  in 
  let string_of_asm_index i =  (nice_prefix^(string_of_int (-i)))
  in 
  let string_of_concl_index i = string_of_int i
  in 
  let rec print_asm i afl= 
    match afl with 
      [] -> ()
    | (s::als) -> 
	 (open_box 0;
	  Format.print_string ("["^(string_of_asm_index i)^"] ");
	  print_term (Formula.term_of_form s);
	  close_box(); 
	  print_newline(); 
	  print_asm (i-1) als)
  and print_cncl i cfl =
    match cfl with
      [] -> ()
    | (s::cls) -> 
	(open_box 0;
	 Format.print_string ("["^(string_of_concl_index i)^"] ");
	 print_term  (Formula.term_of_form s);
	 close_box(); 
	 print_newline(); 
	 (print_cncl (i+1) cls))
  in 
  (Format.print_newline();
   print_asm (-1) (Drule.asm_forms sq); 
   Format.open_box 0;
   Format.print_string ("----------------------"); 
   Format.close_box();
   print_newline();
   print_cncl 1  (Drule.concl_forms sq);
   print_newline())


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
    

let print_error r = (r#print) (Tpenv.pp_info())

let print_theory x = 
  Theory.print (Tpenv.pp_info()) x
