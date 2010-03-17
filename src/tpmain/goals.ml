(*----
 Name: goals.ml
 Copyright M Wahab 2005-2010
 Author: M Wahab  <mwb.cde@googlemail.com>

 This file is part of HSeq

 HSeq is free software; you can redistribute it and/or modify it under
 the terms of the Lesser GNU General Public License as published by
 the Free Software Foundation; either version 3, or (at your option)
 any later version.

 HSeq is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
 License for more details.

 You should have received a copy of the Lesser GNU General Public
 License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
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

  let make gl = [gl]

  let empty() = []

  let push x p = x::p

  let top p = 
    match p with 
      [] -> raise (Report.error "No goals.")
    | (x::_) -> x

  let pop p = 
    match p with
      [] -> raise (Report.error "No goals.")
    | (_::xs) -> xs

  (* Printer *)
  let print ppinfo prf = 
    let g = top prf
    in 
    let subgls = Logic.get_subgoals g
    in 
      Format.printf "@[<v>Goal ";
      Format.printf "@[";
      Term.print ppinfo
        (Formula.term_of (Logic.get_goal g));
      Format.printf "@]@,";
      (match subgls with
          [] -> Format.printf "@[No subgoals@]@,"
        | (x::_) -> 
            let num_gls = (List.length subgls)
            in 
              Format.printf "@[%i %s@]@," 
	        num_gls (if num_gls>1 then "subgoals" else "subgoal");
              Logic.print_sqnt ppinfo x);
      Format.printf "@]"
end

(***
* Multiple interactive proofs.
****)
module ProofStack = 
struct 
  type t = Proof.t list

  let is_empty stk = 
    match stk with 
        [] -> true 
      |  _ -> false

  let empty () = []
  let push x p = x::p

  let top p = 
    match p with 
      [] -> raise (Report.error "No proof attempts.")
    | (x::_) -> x

  let pop p = 
    match p with
      [] -> raise (Report.error "No proof attempts.")
    | (_::xs) -> xs

  let rotate p = 
    match p with 
      [] -> raise (Report.error "No proof attempts.")
    | (x::xs) -> xs@[x]

  let lift n p =
    match p with 
      [] -> raise (Report.error "No proof attempts.")
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
      [] -> raise (Report.error "No proof attempts.")
    | (x::_) -> Proof.top x

  let pop_goal p = 
    match p with
      [] -> raise (Report.error "No proof attempts.")
    | (x::xs) -> (Proof.pop x)::xs

  let undo_goal p=
    match p with
      [] -> raise (Report.error "No proof attempts.")
    | (x::xs) ->
	match (Proof.pop x) with
	  [] -> raise (Report.error "Can't undo anymore")
	| y -> y::xs

  (* Printer *)
  let print ppinfo stk = 
    let print_short prf = 
      Format.printf "@[<v>Goal ";
      Format.printf "@[";
      Term.print ppinfo
        (Formula.term_of (Logic.get_goal (top prf)));
      Format.printf "@]@]";
    and num_prfs = (List.length stk) 
    in
    match stk with 
        [] -> Format.printf "@[No goals@]@,"
      | (p::prfs) ->
          let rprfs = List.rev prfs in
            Format.printf "@[<v>";
            Format.printf "@[%i %s@]@," 
	      num_prfs 
              (if num_prfs > 1 then "goals" else "goal");
            Format.printf "@[";
            List.iter print_short rprfs;
            Format.printf "@]@,";
            Proof.print ppinfo p;
            Format.printf "@]"
end

let prflist = ref (ProofStack.empty())
let proofs() = !prflist
let set_proofs(prfs) = prflist := prfs

let top () = ProofStack.top (proofs())
let top_goal () = ProofStack.top_goal (proofs())

let drop() = (set_proofs(ProofStack.pop (proofs())); proofs())

(*
let goal ?info trm = 
  let f = Formula.make (Global.scope()) trm
  in 
    set_proofs(ProofStack.push_goal (mk_goal ?info (Global.scope()) f) (proofs()));
    !save_hook(); 
    top()
*)
let goal ?info trm = 
  let frm = Formula.make (Global.scope()) trm in 
  let gl = mk_goal ?info (Global.scope()) frm in
  let prf = Proof.make gl in
    set_proofs(ProofStack.push prf (proofs()));
    !save_hook(); 
    top()

let postpone () =
  set_proofs(ProofStack.rotate (proofs()));
  top()

let lift n =
  let nlist = 
    try  ProofStack.lift n (proofs())
    with err -> 
      raise 
	(Report.add_error (Report.error "Failed to lift proof.") err)
  in 
    set_proofs(nlist);
    top()


let undo() =
  match (proofs()) with
    [] -> raise (Report.error "No proof attempts")
  | _ -> (set_proofs(ProofStack.undo_goal (proofs())); top())
	
let result () = mk_thm (top_goal())

let apply ?report tac goal=
  Logic.Subgoals.apply_to_goal ?report tac goal

let prove_goal ?info scp trm tac =
  mk_thm  (apply tac (mk_goal ?info scp (Formula.make scp trm)))

let prove ?scp trm tac = 
  let sp = 
    match scp with 
	None -> Global.scope()
      | Some x -> x
  in 
    prove_goal sp trm tac

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
    set_proofs((ProofStack.push_goal g) (proofs()));
    !save_hook();
    top()

let by_list ?info trm tacl =
  let fg=mk_goal ?info (Global.scope()) (Formula.make (Global.scope()) trm)
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
    [] -> raise (Report.error "No subgoals")
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
