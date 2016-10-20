(*----
  Name: goals.ml
  Copyright Matthew Wahab 2005-2016
  Author: Matthew Wahab <mwb.cde@gmail.com>

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

(*** Single interactive proofs ***)
module Proof =
struct
  type t = Logic.goal list

  let empty() = []
  let make gl = [gl]

  let push x p =  x::p

  let is_empty p =
    match p with
      [] -> true
    | _ -> false

  let top p =
    match p with
      | [] -> raise (Report.error "No goals")
      | (x::_) -> x

  let pop p =
    match p with
      | [] -> raise (Report.error "No goals")
      | (_::xs) -> xs

  (* Printer *)
  let print ppinfo prf =
    let print_aux g =
      begin
        let subgls = Logic.get_subgoals g
        in
        Format.printf "@[<v>Goal ";
        Format.printf "@[";
        Term.print ppinfo (Formula.term_of (Logic.get_goal g));
        Format.printf "@]@,";
        begin
          match subgls with
          | [] -> Format.printf "@[No subgoals@]@,"
          | (x::_) ->
            let num_gls = List.length subgls
            in
            Format.printf "@[%i %s@]@,"
              num_gls
              (if num_gls > 1 then "subgoals" else "subgoal");
            Logic.print_sqnt ppinfo x
        end;
        Format.printf "@]"
      end
    in
    if is_empty prf
    then Format.printf "@[No goals@]@,"
    else print_aux (top prf)

end

(*** Multiple interactive proofs ****)
module ProofStack =
struct
  type t =
    {
      stck_f : Proof.t list;
      save_hook_f: unit -> unit
    }

  let is_empty stk =
    match stk.stck_f with
      | [] -> true
      |  _ -> false

  let empty () = { stck_f = []; save_hook_f = (fun () -> ()) }
  let push x p = { p with stck_f = x::(p.stck_f) }

  let top p =
    match p.stck_f with
      | [] -> raise (Report.error "No proof attempts")
      | (x::_) -> x

  let pop p =
    match p.stck_f with
      | [] -> raise (Report.error "No proof attempts")
      | (_::xs) -> { p with stck_f = xs }

  let rotate p =
    match p.stck_f with
      | [] -> raise (Report.error "No proof attempts")
      | (x::xs) -> { p with stck_f = xs @ [x] }

  let lift n p =
    match p.stck_f with
      | [] -> raise (Report.error "No proof attempts")
      | xs ->
        let (l, c, r) = Lib.full_split_at_index n xs
        in
        { p with stck_f = c::(List.rev_append l r) }

  let push_goal (g: Logic.goal) p =
    let nlst =
      match p.stck_f with
      | [] -> (Proof.push g [])::[]
      | (x::xs) -> (Proof.push g x)::xs
    in
    { p with stck_f = nlst }

  let top_goal p =
    match p.stck_f with
      | [] -> raise (Report.error "No proof attempts")
      | (x::_) -> (Proof.top x)

  let pop_goal p =
    match p.stck_f with
      | [] -> raise (Report.error "No proof attempts.")
      | (x::xs) ->
        { p with stck_f = (Proof.pop x)::xs }

  let undo_goal p =
    match p.stck_f with
      | [] -> raise (Report.error "No proof attempts.")
      | (x::xs) ->
        begin
          match Proof.pop x with
            | [] -> raise (Report.error "Can't undo anymore")
            | y -> { p with stck_f = (y::xs) }
        end

  let save_hook p = p.save_hook_f
  let set_hook f p = { p with save_hook_f = f }

  (* Printer *)
  let print ppinfo stk =
    let print_short prf idx =
      Format.printf "@[<v>Goal %i: " idx;
      Format.printf "@[";
      Term.print ppinfo (Formula.term_of (Logic.get_goal (Proof.top prf)));
      Format.printf "@]@]"
    and num_prfs = List.length stk.stck_f
    in
    let rec print_short_list prfs ctr =
      match prfs with
        | [] -> ()
        | p::ps ->
          begin
            print_short p ctr;
            print_short_list ps (ctr + 1)
          end
    in
    match stk.stck_f with
      | [] -> Format.printf "@[No goals@]@,"
      | p::prfs ->
        let rprfs = List.rev_append prfs []
        in
        Format.printf "@[<v>";
        Format.printf "@[%i %s@]@,"
          num_prfs
          (if num_prfs > 1 then "goals" else "goal");
        Format.printf "@[";
        print_short_list rprfs 1;
        Format.printf "@]@,";
        Proof.print ppinfo p;
        Format.printf "@]"
end

let has_proofs pstk = not (ProofStack.is_empty pstk)
let top pstk =
  if has_proofs pstk
  then ProofStack.top pstk
  else Proof.empty()

let top_goal pstk = ProofStack.top_goal pstk

let drop pstk =
  if has_proofs pstk
  then ProofStack.pop pstk
  else pstk

let goal pstk scp trm =
  let frm = Formula.make scp trm in
  let gl = mk_goal scp frm in
  let prf = Proof.make gl in
  ProofStack.push prf pstk

let postpone pstk =
  ProofStack.rotate pstk

let lift pstk n =
  let nlist =
    try ProofStack.lift n pstk
    with err ->
      raise (Report.add_error (Report.error "Failed to lift proof.") err)
  in
  nlist

let undo pstk =
  if ProofStack.is_empty pstk
  then raise (Report.error "No proof attempts")
  else ProofStack.undo_goal pstk

let result ptsk = mk_thm (top_goal ptsk)

let apply ?report ctxt tac goal =
  Logic.apply_to_goal ?report (tac ctxt) goal

let prove_goal ctxt trm tac =
  mk_thm (apply ctxt tac
            (mk_goal (Context.scope_of ctxt)
               (Formula.make (Context.scope_of ctxt) trm)))

let prove scp trm tac =
  prove_goal scp trm tac

let report ppinf node branch =
  let rec print_subgoals i gs =
    match gs with
      | [] -> ()
      | (y::ys) ->
        Format.open_box 0;
        Format.print_string "(Subgoal ";
        Format.print_int i;
        Format.print_string ")";
        Format.close_box();
        Format.print_newline();
        Logic.print_sqnt ppinf y;
        Format.print_newline();
        print_subgoals (i+1) ys
  in
  let sqnts = Logic.Subgoals.branch_sqnts branch
  in
  match sqnts with
    | [] ->
      Format.open_box 0;
      Format.print_string "Subgoal solved";
      Format.close_box();
      Format.print_newline()
    | _ ->
      let len = List.length sqnts
      in
      if len > 1
      then
        (Format.open_box 0;
         Format.print_int len;
         Format.print_string " subgoals";
         Format.close_box();
         Format.print_newline();
         print_subgoals 1 sqnts)
      else ()

let by_com ctxt pstk tac =
  let p = top_goal pstk in
  let g =
    Logic.apply_to_goal
      ~report:(report (Context.ppinfo ctxt)) (tac ctxt) p
  in
  ProofStack.push_goal g pstk

let by_list ctxt trm tacl =
  let goal_form = Formula.make (Context.scope_of ctxt) trm
  in
  let new_goal = mk_goal (Context.scope_of ctxt) goal_form
  in
  let rec by_aux ts g =
    match ts with
      | [] -> g
      | (x::xs) ->
        if Logic.has_subgoals g
        then by_aux xs (apply ctxt x g)
        else g
  in
  mk_thm (by_aux tacl new_goal)


(*** Miscellaneous ***)

let set_hook = ProofStack.set_hook
let save_hook = ProofStack.save_hook

let curr_sqnt pstk =
  match Logic.get_subgoals (top_goal pstk) with
    | [] -> raise (Report.error "No subgoals")
    | x::xs -> x

let get_asm pstk i =
  let (ft, nt) = Logic.Sequent.get_asm i (curr_sqnt pstk )
  in
  (ft, Formula.term_of nt)

let get_concl pstk i =
  let (ft, nt) = Logic.Sequent.get_cncl i (curr_sqnt pstk)
  in
  (ft, Formula.term_of nt)

let goal_scope pstk =
  Logic.Sequent.scope_of (curr_sqnt pstk)

let goal_changes pstk =
  Logic.goal_changes (top_goal pstk)
