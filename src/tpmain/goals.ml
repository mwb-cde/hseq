open Logic
open Tactics

let save_hook = ref (fun () -> ())

let set_hook f = (save_hook := f)

type prf = goal

let prflist = ref([]: prf list)

let curr_goal p = p
let top () = List.hd !prflist

let drop()=prflist:=[]

let pop_plist () = 
  let p = List.hd !prflist
  in (prflist:=List.tl !prflist); p

let push_plist p = 
  (prflist:=p::!prflist); p

let curr_sqnt p = 
  match p with 
    g -> (Logic.get_nth_subgoal_sqnt 0 g)

let get_asm i = 
  let (ft, nt)= Logic.Sequent.get_asm i (curr_sqnt (top()))
  in 
  (ft, Formula.term_of_form nt)

let get_concl i = 
  let (ft, nt)= Logic.Sequent.get_cncl i (curr_sqnt (top()))
  in 
  (ft, Formula.term_of_form nt)

let mk_dummy_fntype n=
  let rec mk_dum m j=
    if m = 0 
    then []
    else 
      (Gtypes.mk_typevar j)::mk_dum (m-1) j
  in let i = ref 0
  in Logicterm.mk_fun_ty_from_list (mk_dum n i)
    (Gtypes.mk_typevar i)

let goal trm = 
  let f = Formula.mk_form (Global.scope()) (Global.mk_term (Global.scope()) trm)
  in 
  prflist:= [mk_goal  (Global.scope()) f];
  (!save_hook()); top()

let apply ?report tac goal=
  Logic.Subgoals.apply_to_goal ?report tac goal

let prove_goal scp trm tac =
  mk_thm 
    (apply tac (mk_goal scp (Formula.mk_form (Global.scope()) trm)))

let prove trm tac = prove_goal (Global.scope()) trm tac

let by_list trm tacl =
  let fg=mk_goal (Global.scope()) 
      (Formula.mk_form (Global.scope()) trm)
  in 
  let rec by_aux ts g =
      match ts with 
	[] -> g
      | (x::xs) -> 
	  if Logic.has_subgoals g
	  then by_aux xs (apply x g)
	  else g
  in 
   mk_thm (by_aux tacl fg)


let report node branch = 
  let rec print_subgoals i gs = 
    match gs with 
      [] -> ()
    | (y::ys) -> 
	Format.open_box 0;
	Format.print_string "(Subgoal ";
	Format.print_int i;
	Format.print_string ")";
	Format.close_box();
	Format.print_newline();
	Logic.print_sqnt (Global.pp_info()) y;
	Format.print_newline();
	print_subgoals (i+1) ys
  in 
  let sqnts = Logic.Subgoals.branch_sqnts branch
  in 
  match sqnts with
    [] -> 
      Format.open_box 0;
      Format.print_string "Subgoal solved";
      Format.close_box(); 
      Format.print_newline()
  | _ -> 
      let len=(List.length sqnts)
      in 
      if(len>1)
      then 
	(Format.open_box 0;
	 Format.print_int len;
	 Format.print_string " subgoals";
	 Format.close_box(); 
	 Format.print_newline();
	 print_subgoals 1 sqnts)
      else ()

let by_com tac =
  let p = top()
  in 
  (let g = 
    Logic.Subgoals.apply_to_goal ~report:report tac (curr_goal p)
  in 
  (if (num_of_subgoals g)=0 
  then prflist:= g::(!prflist)
  else 
    (prflist:=g::(!prflist));
   (!save_hook()));
  top())

let postpone() =
  match (!prflist) with
    [] -> raise (Result.error "No goal")
  |	_ -> 
      (let ng = Logic.postpone (pop_plist())
      in 
      push_plist ng)

let undo() =
  match (!prflist) with
    [] -> raise (Result.error "No goal")
  |	[x] -> raise (Result.error "No previous goals")
  |	_ -> (ignore(pop_plist()); top())
	
let result ()= mk_thm (curr_goal (top()))


