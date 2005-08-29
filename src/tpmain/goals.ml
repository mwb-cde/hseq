(*-----
 Name: goals.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Logic
open Tactics

let save_hook = ref (fun () -> ())
let set_hook f = (save_hook := f)

(***
* Single interactive proofs
***)
module Proof = 
struct
  type t = Logic.goal list

  let push x p = x::p

  let top p = 
    match p with 
      [] -> raise (Result.error "No goals.")
    | (x::_) -> x

  let pop p = 
    match p with
      [] -> raise (Result.error "No goals.")
    | (_::xs) -> xs

end

(***
* Multiple interactive proofs.
****)
module ProofStack = 
struct 
  type t = Proof.t list

  let push x p = x::p

  let top p = 
    match p with 
      [] -> raise (Result.error "No goals.")
    | (x::_) -> x

  let pop p = 
    match p with
      [] -> raise (Result.error "No goals.")
    | (_::xs) -> xs

  let rotate p = 
    match p with 
      [] -> raise (Result.error "No goals.")
    | (x::xs) -> xs@[x]

  let lift n p =
    match p with 
      [] -> raise (Result.error "No goals.")
    | xs -> 
	let (l, c, r) = Lib.full_split_at_index n xs
	in 
	c::(List.rev_append l r)

  let push_goal g p = 
    match p with 
      [] -> (Proof.push g [])::[]
    | (x::xs) -> (Proof.push g x)::xs

  let top_goal p = 
    match p with 
      [] -> raise (Result.error "No goals.")
    | (x::_) -> Proof.top x

  let pop_goal p = 
    match p with
      [] -> raise (Result.error "No goals.")
    | (x::xs) -> (Proof.pop x)::xs

  let undo_goal p=
    match p with
      [] -> raise (Result.error "No goals.")
    | (x::xs) ->
	match (Proof.pop x) with
	  [] -> raise (Result.error "Can't undo anymore")
	| y -> y::xs
end

let prflist = ref ([]:ProofStack.t)

let proofs() = !prflist

let top () = ProofStack.top (!prflist)
let top_goal () = ProofStack.top_goal (!prflist)

let drop() = prflist:=ProofStack.pop (!prflist)

let goal trm = 
  let f = Formula.make (Global.scope()) (Global.mk_term trm)
  in 
  prflist:= ProofStack.push_goal (mk_goal  (Global.scope()) f) (!prflist);
  !save_hook(); top()

let postpone () =
  prflist := ProofStack.rotate (!prflist);
  top()

let lift n =
  let nlist = 
    try  ProofStack.lift n (!prflist)
    with err -> 
      raise 
	(Result.add_error (Result.error "Failed to focus on proof.") err)
  in 
  prflist := nlist;
  top()


let undo() =
  match (!prflist) with
    [] -> raise (Result.error "No goals")
  | _ -> 
      (prflist := ProofStack.undo_goal (!prflist); top())
	
let result () = mk_thm (top_goal())

let apply ?report tac goal=
  Logic.Subgoals.apply_to_goal ?report tac goal

let prove_goal scp trm tac =
  mk_thm  (apply tac (mk_goal scp (Formula.make (Global.scope()) trm)))

let prove trm tac = prove_goal (Global.scope()) trm tac

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
	Logic.print_sqnt (Global.PP.info()) y;
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
  let p = (top_goal():Logic.goal)
  in 
  let g = Logic.Subgoals.apply_to_goal ~report:report tac p
  in 
  prflist:= (ProofStack.push_goal g) !prflist;
  !save_hook();
  top()

let by_list trm tacl =
  let fg=mk_goal (Global.scope()) (Formula.make (Global.scope()) trm)
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


(***
* Miscellaneous
***)

let curr_sqnt () = 
  (match (Logic.get_subgoals (top_goal())) with
    [] -> raise (Result.error "No subgoals")
  | x::xs -> x)

let get_asm i = 
  let (ft, nt)= Logic.Sequent.get_asm i (curr_sqnt ())
  in 
  (ft, Formula.term_of nt)

let get_concl i = 
  let (ft, nt)= Logic.Sequent.get_cncl i (curr_sqnt ())
  in 
  (ft, Formula.term_of nt)

let goal_scope () = 
  let sq = curr_sqnt()
  in 
  Logic.Sequent.scope_of sq
