open Logic
open Tactics

let save_hook = ref (fun () -> ())

let set_hook f = (save_hook := f)

type prf = goal

let prflist = ref([]: prf list)

let curr_goal p = p
let top () = List.hd !prflist

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
  in Gtypes.mk_fun_from_list (mk_dum n i)
    (Gtypes.mk_typevar i)

let goal trm = 
  let f = Formula.mk_form (Tpenv.scope()) (Tpenv.mk_term (Tpenv.scope()) trm)
  in 
  prflist:= [mk_goal  (Tpenv.scope()) f];
  (!save_hook()); top()

let prove_goal trm tac =
  mk_thm (tac (mk_goal  (Tpenv.scope()) 
		 (Formula.mk_form (Tpenv.scope()) trm)))

let by_list trm tacs =
  let fg=mk_goal (Tpenv.scope()) 
      (Formula.mk_form (Tpenv.scope()) trm)
  in 
  let rec by_aux tcs g= 
    match tcs with
      [] -> g
    | t::ts -> 
	if Logic.goal_has_subgoals g
    	then 
	  by_aux ts (try (t g) with _ -> g)
	else g
  in 
  mk_thm (by_aux tacs fg)


let by_com tac =
  let p = top()
  in 
  (let g = (tac (curr_goal p))
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
      (let ng = Logic.Rules.postpone (pop_plist())
      in 
      push_plist ng)

let undo() =
  match (!prflist) with
    [] -> raise (Result.error "No goal")
  |	[x] -> raise (Result.error "No previous goals")
  |	_ -> (ignore(pop_plist()); top())
	
let result ()= mk_thm (curr_goal (top()))


