
  open Term
  open Format
  open Result

  let printer_info_list = ref []
  let printer_info () = !printer_info_list
  let set_printer_info l = printer_info_list := l

  let prec_of f = 
    try fst (List.assoc f (printer_info()))
    with Not_found -> (try Tpenv.prec_of Basic.fn_id f with _ -> -1)

  let is_infix f = 
    try snd (List.assoc f (printer_info()))
    with Not_found -> 
      (try Tpenv.is_infix Basic.fn_id f with _ -> false)

  let type_printer_info_list = ref []
  let type_printer_info () = !printer_info_list      
  let set_type_printer_info l = type_printer_info_list := l

  let add_printer_info st pr inf = 
    try (ignore(List.assoc st (!printer_info_list)));
	 failwith ("Identifier "^(Basic.string_fnid st)^" exists")
    with Not_found -> 
      printer_info_list:=(st, (pr, inf))::!printer_info_list

  let remove_printer_info st =
    let rec remove_aux ls =
      match ls with
	[] -> []
      |	((x, y)::xs) -> 
	  if x=st then remove_aux xs else (x, y)::(remove_aux xs)
    in printer_info_list:=remove_aux (!printer_info_list)


  let add_type_printer_info st pr inf = 
    try (List.assoc st (!type_printer_info_list));
	 failwith ("Identifier "^st^" exists")
    with Not_found -> 
      type_printer_info_list:=(st, (pr, inf))::!type_printer_info_list


  let remove_type_printer_info st =
    let rec remove_aux ls =
      match ls with
	[] -> []
      |	((x, y)::xs) -> if x=st then remove_aux xs else (x, y)::(remove_aux xs)
    in type_printer_info_list:=remove_aux (!type_printer_info_list)


  let rec list_print f sep x =
    match x with 
      [] -> ()
    | (b::[]) -> (f b)
    | (b::bs) -> (f b); sep(); (list_print f sep bs)


let cfun_string c =
  match c with 
    "not" -> print_string "not"
  | "and" -> print_string "and"
  | "or" -> print_string "or"
  | "implies" -> print_string " => "
  | "iff" -> print_string "<=>"
  | "equals" -> print_string "="
  | x -> print_string x


  let rec print_term_aux inf i x =
    Term.print_term_aux (Tpenv.base_pp_state()) i x

  let  print_term x = 
    open_box 0;
    Term.print_term (Tpenv.base_pp_state()) x;
    close_box()

  let rec print_termlist x =
    match x with 
      [] -> print_string ""
    | [p] -> print_term p
    | (p::ps) -> 
	(print_term p; 
	 print_string ", "; 
	 print_termlist ps)

(*
  let rec print_typ x = 
    open_box 0; 
    print_string(Gtypes.string_gtype x); 
    close_box()
*)
  let rec print_typ x = 
    open_box 0; 
    Gtypes.print_type_info (Tpenv.base_pp_state()) 0 x; 
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
(*     print_asm (-1) (Logic.asms sq); *)
     print_asm (-1) (Drule.asm_forms sq); 
     print_string ("----------------------"); print_newline();
     print_cncl 1  (Drule.concl_forms sq);
     print_newline();
     close_box())


  let print_thm t = 
    open_box 0; print_string "|- ";
      print_term (Formula.term_of_form (Logic.dest_thm t));
      close_box()

(*
      (Term.string_inf_term (prec_of, is_infix) 
	 (Formula.term_of_form (Logic.dest_thm t))));
*)      

(*
  let print_prf p = 
    let cur = Goals.curr_indx p
    and g = Goals.curr_goal p
    in 
    let subgls = Logic.get_goal_sqnts g
    in 
    open_box 0; 
    print_string "Goal: "; print_term 
      (Formula.term_of_form (Logic.get_goal g));
    print_newline();
    close_box();
    (match subgls with
      [] -> (open_box 0; print_string "No subgoals"; close_box())
    | _ -> 
	(open_box 0;
	 print_newline();
	 print_int (List.length subgls); print_string " subgoals";
	 print_newline();
	 print_string "Subgoal "; print_int cur; 
	 force_newline();
    	 close_box ();
	 print_sqnt (Goals.curr_sqnt p)))
*)    

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

(*	 print_newline();
	 print_string "Subgoal "; print_int cur; *)


let print_fnident x = print_string (Basic.string_fnid x)

let print_subst tenv f= 
  open_box 0;
  (Hashtbl.iter 
     (fun x y -> print_string 
	 ("("^(f x)^" = "^(f y)^"):"))
     tenv);
  close_box()

let print_error r =
   (r#print) (Tpenv.base_pp_state())
