
open Term
open Format
open Result

let cfun_string c =
  match c with 
    "not" -> print_string "not"
  | "and" -> print_string "and"
  | "or" -> print_string "or"
  | "implies" -> print_string " => "
  | "iff" -> print_string "<=>"
  | "equals" -> print_string "="
  | x -> print_string x

let print_fnident x = Basic.PP.print_ident x

let print_term x = 
  open_box 0;
  Term.print (Tpenv.pp_info()) x;
  close_box()

let rec print_type x = 
  open_box 0; 
  Gtypes.print_type_info (Tpenv.pp_info()) 0 x; 
  close_box()


let print_sqnt sq = 
  let rec print_asm i afl= 
    match afl with 
      [] -> ()
    | (s::als) -> 
	(print_string ("["^(string_of_int i)^"] ");
	 open_box 0;
	 print_term (Formula.term_of_form s);
	 close_box(); 
	 print_newline(); 
	 print_asm (i-1) als)
  and print_cncl i cfl =
    match cfl with
      [] -> ()
    | (s::cls) -> 
	(print_string ("["^(string_of_int i)^"] ");
	 open_box 0;
	 print_term  (Formula.term_of_form s);
	 close_box(); 
	 print_newline(); 
	 (print_cncl (i+1) cls))
  in 
  (open_box 0;
   print_newline();
   print_asm (-1) (Drule.asm_forms sq); 
   print_string ("----------------------"); print_newline();
   print_cncl 1  (Drule.concl_forms sq);
   print_newline();
   close_box())


let print_thm t = 
  open_box 0; print_string "|- ";
  print_term (Formula.term_of_form (Logic.dest_thm t));
  close_box()

let print_prf p = 
  let g = Goals.curr_goal p
  in 
  let subgls = Logic.num_of_subgoals g
  in 
  open_box 0; 
  print_string "Goal: "; print_term 
    (Formula.term_of_form (Logic.get_goal g));
  print_newline();
  close_box();
  (match subgls with
    0 -> (open_box 0; print_string "No subgoals"; close_box())
  | _ -> 
      (open_box 0;
       print_newline();
       print_int subgls; print_string " subgoals";
       force_newline();
       close_box ();
       print_sqnt (Goals.curr_sqnt p)))


let print_defn def =
  let n, ty, th = Defn.dest_defn def
  in 
  Format.open_vbox 0;
  Format.open_box 0;
  print_fnident (Basic.mklong Basic.null_thy (Basic.name n));
  Format.print_string ":";
  Format.print_space();
  print_type ty;
  Format.close_box();
  Format.print_cut();
  print_thm th;
  Format.close_box()
  
let print_subst tenv f= 
  open_box 0;
  (Hashtbl.iter 
     (fun x y -> print_string 
	 ("("^(f x)^" = "^(f y)^"):"))
     tenv);
  close_box()

let print_error r =
  (r#print) (Tpenv.pp_info())

