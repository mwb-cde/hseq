open Logic
open Tactics

let save_hook = ref (fun () -> ())

let set_hook f = (save_hook := f)

(*    type prf = (int * goal)*)
type prf = goal

let prflist = ref([]: prf list)

(*    let curr_indx p = fst p*)
let curr_goal p = p
let top () = List.hd !prflist

let pop_plist () = 
  let p = List.hd !prflist
  in (prflist:=List.tl !prflist); p

let push_plist p = 
  (prflist:=p::!prflist); p

let curr_sqnt p = 
  match p with 
    g -> (*get_nth (get_goal_sqnts g) i*)
      (Logic.get_nth_subgoal_sqnt 0 g)

let get_asm i = 
  let (ft, nt)= Logic.get_asm i (curr_sqnt (top()))
  in 
  (ft, Formula.term_of_form nt)

let get_concl i = 
  let (ft, nt)= Logic.get_cncl i (curr_sqnt (top()))
  in 
  (ft, Formula.term_of_form nt)
(*
   Formula.term_of_form (Logic.get_cncl i (curr_sqnt (top())))
 *)
let mk_dummy_fntype n=
  let rec mkdum m j=
    if m = 0 
    then []
    else 
      (Gtypes.mk_typevar j)::mkdum (m-1) j
(*
   (j:=(!j)+1; 
   (Gtypes.mk_var
   ("ty"^(string_of_int !j)))::mkdum (m-1) j)
 *)
  in let i = ref 0
  in Gtypes.mkfun_from_list (mkdum n i)
    (Gtypes.mk_typevar i)
(*
   (i:=(!i)+1;(Gtypes.mk_var ("ty"^(string_of_int !i))))
 *)


(*(Tpenv.mk_thyinfo()) *)

let goal trm = 
  let f = Formula.mk_form (Tpenv.scope()) (Tpenv.mkterm (Tpenv.scope()) trm)
  in 
  prflist:= [mk_goal  (Tpenv.scope()) f];
  (!save_hook()); top()

(*
let goal_string st = goal (read st)
*)
(*(Tpenv.mk_thyinfo())*)

let prove_goal trm tac =
  mk_thm (Logic.Rules.goal_apply tac 
	    (mk_goal  (Tpenv.scope()) 
	       (Formula.mk_form (Tpenv.scope()) trm)))
(*
let prove_goal_string st tac= prove_goal (read st) tac
*)
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
	  by_aux ts (try (Logic.Rules.goal_apply t g) with _ -> g)
	else g
  in 
  mk_thm (by_aux tacs fg)

(*
let by_list_string st tacs =by_list (read st) tacs
*)

let by_com tac =
  let p = top()
  in 
  (let g = (Logic.Rules.goal_apply tac (curr_goal p))
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
      (let ng = Logic.Rules.goal_postpone (pop_plist())
      in 
      push_plist ng)

let undo() =
  match (!prflist) with
    [] -> raise (Result.error "No goal")
  |	[x] -> raise (Result.error "No previous goals")
  |	_ -> (ignore(pop_plist()); top())
	
let result ()= mk_thm (curr_goal (top()))


