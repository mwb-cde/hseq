(*----
  Name: tactics.ml
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

open Lib.Ops
open Logic
open Rewrite

type tactic = Logic.tactic

(*
 * Support functions 
 *)

(*** Error reporting ***)

let error s = Report.error s
let add_error s err = Report.add_error (error s) err

(*** Accessing elements of a list ***)

let get_one ?(msg="Tactics.get_one failed") l =
  Lib.get_one l (Failure msg)

let get_two ?(msg="Tactics.get_two failed") l =
  Lib.get_two l (Failure msg)

(*** Formulas ***)

let drop_tag = Logic.drop_tag
let drop_formula = Logic.form_tag

(*** Labels ***)

let fnum n = Logic.FNum n
let ftag t = Logic.FTag t
let fname s = Logic.FName s

let (!!) = fnum
let (!~) x = fnum (-x)
let (!$) = fname

(*** Sequents ***)

let asms_of = Logic.Sequent.asms
let concls_of = Logic.Sequent.concls
let sqnt_tag  = Logic.Sequent.sqnt_tag

(*** Nodes ***)

let sequent g = Logic.Subgoals.node_sqnt g
let scope_of g = Logic.Sequent.scope_of (sequent g)
let typenv_of n = Logic.Subgoals.node_tyenv n
let node_tag n = Logic.Sequent.sqnt_tag (sequent n)
let changes n = Logic.Tactics.changes n

let get_tagged_asm i g = Logic.get_label_asm i (sequent g)
let get_tagged_concl i g= Logic.get_label_cncl i (sequent g)

let get_asm i g = Logic.drop_tag (get_tagged_asm i g)
let get_concl i g = Logic.drop_tag (get_tagged_concl i g)
let get_form i g = 
  try get_concl i g
  with Not_found -> get_asm i g

(*** Branches ***)

let branch_tyenv b = Logic.Subgoals.branch_tyenv b
let branch_subgoals b = Logic.Subgoals.branch_sqnts b
let has_subgoals b =
  match branch_subgoals b with
    | [] -> false
    | _ -> true
let num_subgoals b = List.length (branch_subgoals b) 

(*** Information records ***)

module Info =
struct
  type t = Changes.t ref

  let make () = ref (Changes.empty())
  let empty info = info := (Changes.empty())
  let subgoals inf = Changes.goals (!inf)
  let aformulas inf = Changes.aforms (!inf)
  let cformulas inf = Changes.cforms (!inf)
  let constants inf = Changes.terms (!inf)

  let form dst sgs afs cfs cnsts =
    begin
      match dst with 
        | None -> ()
        | Some(vr) -> 
          let chngs = Changes.make sgs afs cfs cnsts
          in
          vr := chngs
    end

  let form_changes dst chngs =
    begin
      match dst with 
        | None -> ()
        | Some(vr) -> vr := chngs
    end

  let add dst sgs afs cfs cnsts =
    begin
      match dst with 
        | None -> ()
        | Some(vr) -> 
          let chngs = Changes.add (!vr) sgs afs cfs cnsts
          in
          vr := chngs
    end

  let add_changes dst chngs = 
    begin
      match dst with 
        | None -> ()
        | Some(vr) -> vr := chngs
    end

  let set dst (sgs, afs, cfs, cnsts) = 
    add dst sgs afs cfs cnsts

end
    
module New =
struct
  let changes = Logic.Tactics.changes

  let subgoals = Changes.goals
  let aformulas = Changes.aforms
  let cformulas = Changes.cforms
  let constants = Changes.terms
end

(*** Utility functions ***)

let extract_consts vars env=
  let rec extract_aux qs cnsts =
    match qs with 
      | [] -> cnsts
      | x::xs -> 
	try 
	  let nv = Term.find (Basic.Bound x) env
	  in 
	  extract_aux xs (nv::cnsts)
	with Not_found -> extract_aux xs cnsts
  in 
  List.rev (extract_aux vars [])

let qnt_opt_of kind pred trm =
  let (_, body) = Term.strip_qnt kind trm
  in pred body

let first_asm p sq =
  Lib.first p (asms_of sq)

let first_concl p sq =
  Lib.first p (concls_of sq)

let first_form p sq = 
  try first_asm p sq 
  with Not_found -> first_concl p sq

let first_asm_label a pred sq =
  match a with
    | Some x -> x
    | _ -> 
      let asm_pred f = pred (drop_tag f) in
      let tag = drop_formula (first_asm asm_pred (sequent sq))
      in 
      ftag tag

let first_concl_label c pred sq =
  match c with
    | Some x -> x
    | _ -> 
      let concl_pred f = pred (drop_tag f) in
      let tag = drop_formula (first_concl concl_pred (sequent sq))
      in 
      ftag tag

let node_changes = Logic.Subgoals.node_changes
let branch_changes = Logic.Subgoals.branch_changes

(* 
 * Basic tacticals and tactics
 *)

let foreach = Logic.Subgoals.apply_to_each 

let skip = Logic.Tactics.skip

let fail ?err sq = 
  match err with 
    | None -> raise (error "failed")
    | Some e -> raise e

let data_tac f info g= f info; skip g

(*
 * Tacticals
 *)

let seq rls sq =
  let rec seq_aux fs sqs =
    match fs with 
      | [] -> sqs 
      | r::rs ->
	if has_subgoals sqs
	then seq_aux rs (foreach r sqs)
	else sqs
  in 
  match rls with
    | [] -> raise (error "seq: empty tactic list")
    | x::xs -> seq_aux xs (x sq)

let (++) tac1 tac2 g = seq [tac1; tac2] g

let alt tacl g = 
  let rec alt_aux ts =
    match ts with
      | [] -> raise (error "alt: empty tactic list")
      | x::[] -> x g
      | x::xs ->
	try x g
	with _ -> alt_aux xs
  in alt_aux tacl 

let (//) tac1 tac2 g =
  try tac1 g 
  with  _ -> tac2 g

let thenl tac rls sq = Logic.Subgoals.zip rls (tac sq)
let (--) = thenl

let apply_fold = Logic.Subgoals.apply_fold

let fold data rls sq =
  let rec fold_aux fs d sqs =
    match fs with 
      | [] -> (d, sqs)
      | r::rs ->
	if has_subgoals sqs
	then 
          let (d1, sqs1) = apply_fold r d sqs
          in
          fold_aux rs d1 sqs1
	else 
          (d, sqs)
  in 
  begin
    match rls with
      | [] -> raise (error "seq: empty tactic list")
      | x::xs -> 
        let (d1, sqs1) = fold_aux rls data sq
        in
        (d1, sqs1)
(**        
        let (d1, sqs1) = apply_fold x data sq
        in
        fold_aux xs d1 sqs1
**)
  end

let result_tac tac t f g = 
  try (t, tac g)
  with _ -> (f, skip g)

let rec repeat tac g =
  (tac ++ ((repeat tac) // skip)) g

let cond pred ttac ftac g =
  if pred g 
  then ttac g 
  else ftac g

let (-->) pred tac = cond pred tac skip

let restrict p tac goal =
  let ng = tac goal
  in 
  if p ng 
  then ng 
  else raise (Failure "restrict_tac")

let notify_tac f d tac goal =
  let ng = tac goal
  in 
  f d; ng

(***
let (!!) f d tac goal =
  let ng = tac goal
  in 
  f d; ng
***)

let data_tac f tacl g = tacl (f g) g
let (>>) f tacl g = tacl (f g) g
let query_tac tacl g = tacl (New.changes g) g
let (??) tacl g = tacl (New.changes g) g

let rec map_every tac l goal = 
  let rec every_aux ls g =
    match ls with 
      | [] -> skip g
      | x::xs -> (tac x ++ every_aux xs) g
  in 
  every_aux l goal

let map_first tac l goal = 
  let rec every_aux ls g =
    match ls with 
      | [] -> fail ~err:(error "map_first: no tactic succeeded.") g
      | x::xs -> 
	try tac x g
	with _ -> every_aux xs g
  in 
  every_aux l goal
    
let map_some tac l goal =
  let nofail_tac l = (tac l // skip)
  in 
  let rec some_aux ls g =
    match ls with 
      | [] -> fail ~err:(error "map_some: no tactic succeeded.") g
      | x::xs ->
	try (tac x ++ map_every nofail_tac xs) g
	with _ -> some_aux xs g
  in 
  some_aux l goal

let seq_some tacs goal =
  let nofail_tac tac g = (tac // skip) g
  in 
  let rec some_aux ls g =
    match ls with 
      | [] -> fail ~err:(Report.error "seq_some: no tactic succeeded.") g
      | x::xs ->
	try (x ++ map_every nofail_tac xs) g
	with _ -> some_aux xs g
  in 
  some_aux tacs goal

let seq_any tacl goal =
  let try_tac tac d g = result_tac tac true d g in
  let (succ, goal1) = fold false (List.map try_tac tacl) goal
  in
  if succ 
  then goal1
  else fail ~err:(Report.error "seq_any: no tactic succeeded.") goal


let foreach_asm tac goal =
  let label_tac tf = tac (ftag (drop_formula tf))
  in 
  try map_some label_tac (asms_of (sequent goal)) goal
  with err -> raise (add_error "foreach_asm: no change." err)

let foreach_concl tac goal =
  let label_tac tf = tac (ftag (drop_formula tf))
  in 
  try map_some label_tac (concls_of (sequent goal)) goal
  with err -> raise (add_error "foreach_concl: no change." err)

let foreach_form tac goal =
  seq_any [foreach_asm tac; foreach_concl tac] goal

(*
 * Tactics
 *)

(*** Formula manipulation ***)

let lift_info ?info tac goal = 
  let (result: Logic.branch) = tac goal 
  in
  Info.add_changes info (branch_changes result); result

let rotateA ?info g = 
  lift_info ?info Logic.Tactics.rotate_asms g
  
let rotateC ?info g = 
  lift_info ?info Logic.Tactics.rotate_cncls g

let copyA ?info i g = 
  lift_info ?info (Logic.Tactics.copy_asm i) g
let copyC ?info i g = 
  lift_info ?info (Logic.Tactics.copy_cncl i) g

let liftA ?info l g = 
  lift_info ?info (Logic.Tactics.lift_asm l) g
let liftC ?info l g = 
  lift_info ?info (Logic.Tactics.lift_concl l) g
let lift ?info id g =
  lift_info ?info (Logic.Tactics.lift id) g

let deleteA ?info i = 
  lift_info ?info (Logic.Tactics.deleteA i)
let deleteC ?info i = 
  lift_info ?info (Logic.Tactics.deleteC i)

let delete ?info i g = 
  try  deleteA ?info i g
  with Not_found -> deleteC ?info i g

let deleten ns sq = 
  let rec del_aux l b =
    match l with
      | [] -> b
      | x::xs -> del_aux xs (foreach (delete x) b)
  in
  del_aux ns (skip sq)

(*** Logic Rules **)

let trueC ?info ?c sq =
  let cf = first_concl_label c Formula.is_true sq
  in
  lift_info ?info (Logic.Tactics.trueC cf) sq

let conjC ?info ?c sq =
  let cf = first_concl_label c Formula.is_conj sq
  in
  lift_info ?info (Logic.Tactics.conjC cf) sq

let conjA ?info ?a sq =
  let af = first_asm_label a Formula.is_conj sq
  in
  lift_info ?info (Logic.Tactics.conjA af) sq

let disjC ?info ?c sq =
  let cf = first_concl_label c Formula.is_disj sq
  in
  lift_info ?info (Logic.Tactics.disjC cf) sq

let disjA ?info ?a sq =
  let af = first_asm_label a Formula.is_disj sq
  in
  lift_info ?info (Logic.Tactics.disjA af) sq

let negC ?info ?c sq =
  let cf = first_concl_label c Formula.is_neg sq
  in
  lift_info ?info (Logic.Tactics.negC cf) sq

let negA ?info ?a sq =
  let af = first_asm_label a Formula.is_neg sq
  in
  lift_info ?info (Logic.Tactics.negA af) sq

let implC ?info ?c sq =
  let cf = first_concl_label c Formula.is_implies sq
  in
  lift_info ?info (Logic.Tactics.implC cf) sq

let implA ?info ?a sq =
  let af = first_asm_label a Formula.is_implies sq
  in 
  lift_info ?info (Logic.Tactics.implA af) sq

let existC ?info ?c trm sq =
  let cf = first_concl_label c Formula.is_exists sq
  in
  lift_info ?info (Logic.Tactics.existC trm cf) sq

let existA ?info ?a sq =
  let af = first_asm_label a Formula.is_exists sq
  in
  lift_info ?info (Logic.Tactics.existA af) sq

let allC ?info ?c sq =
  let cf = first_concl_label c Formula.is_all sq
  in
  lift_info ?info (Logic.Tactics.allC cf) sq

let allA ?info ?a trm sq =
  let af = first_asm_label a Formula.is_all sq
  in
  lift_info ?info (Logic.Tactics.allA trm af) sq

let nameC = Logic.Tactics.nameC 
let nameA = Logic.Tactics.nameA

let instA0 ?info l trms goal =
  let info1 = Info.make ()
  and tag1 = Logic.label_to_tag l (sequent goal)
  in 
  let instf infof trm g = 
    let alabel = get_one ~msg:"instA" (Info.aformulas infof)
    in
    Info.empty infof;
    allA ~info:infof ~a:(ftag alabel) trm g
  in 
  Info.form (Some info1) [] [tag1]  [] [];
  let g1 = map_every (instf info1) trms goal
  in 
  Info.add info [] (Info.aformulas info1) [] [];
  g1

let instA ?info ?a trms goal = 
  let af = first_asm_label a Formula.is_all goal
  in 
  instA0 ?info af trms goal

let instC0 ?info l trms goal =
  let info1 = Info.make ()
  and tag1 = Logic.label_to_tag l (sequent goal)
  in 
  let instf infof trm g = 
    let clabel = get_one ~msg:"instC" (Info.cformulas infof)
    in
    Info.empty infof;
    existC ~info:infof ~c:(ftag clabel) trm g
  in 
  Info.form (Some info1) [] [] [tag1] [];
  let g1 = map_every (instf info1) trms goal
  in 
  Info.add info [] [] (Info.cformulas info1) [];
  g1
    
let instC ?info ?c trms goal=
  let cf= first_concl_label c Formula.is_exists goal
  in 
  instC0 ?info cf trms goal

let inst_tac ?info ?f trms goal = 
  try instA ?info:info ?a:f trms goal
  with _ -> instC ?info:info ?c:f trms goal

let cut ?info ?inst th goal = 
  let cut0 trms g = 
    let info1 = Info.make() in 
    let g1 = lift_info ~info:info1 (Logic.Tactics.cut th) g in 
    let atag = get_one ~msg:"cut" (Info.aformulas info1)
    in 
    Info.empty info1;
    foreach (instA ?info:info ~a:(ftag atag) trms) g1
  in 
  match inst with
    | None -> lift_info ?info (Logic.Tactics.cut th) goal
    | Some(trms) -> 
      try cut0 trms goal
      with err -> raise (add_error "cut" err)


let betaA ?info ?a goal =
  let conv_tac (ft, form) g =
    let scp = scope_of g in 
    let thm = 
      try Logic.Conv.beta_conv scp (Formula.term_of form)
      with err -> raise (add_error "betaA" err)
    in 
    let info1 = Info.make()
    and albl = ftag ft
    in 
    seq 
      [
	cut ~info:info1 thm;
	(fun g1 ->
	  let tlbl = 
	    ftag (Lib.get_one (Info.aformulas info1) (error "Tactics.betaA"))
	  in
	  seq
	    [
	      lift_info ?info:info (Logic.Tactics.substA [tlbl] albl);
	      Logic.Tactics.deleteA tlbl
	    ] g1)
      ] g
  in 
  match a with
    | Some(x) -> conv_tac (get_tagged_asm x goal) goal
    | None -> 
      map_some conv_tac (asms_of (sequent goal)) goal

let betaC ?info ?c goal =
  let conv_tac (ft, form) g =
    let scp = scope_of g in 
    let thm = 
      try Logic.Conv.beta_conv scp (Formula.term_of form)
      with err -> raise (add_error "betaC" err)
    in 
    let info1 = Info.make()
    and clbl = ftag ft
    in 
    seq 
      [
	cut ~info:info1 thm;
	(fun g1 ->
	  let tlbl = 
	    ftag (Lib.get_one (Info.aformulas info1) (error "Tactics.betaC"))
	  in
	  seq
	    [
	      lift_info ?info:info (Logic.Tactics.substC [tlbl] clbl);
	      Logic.Tactics.deleteA tlbl
	    ] g1)
      ] g
  in 
  match c with
    | Some(x) -> conv_tac (get_tagged_concl x goal) goal
    | None -> 
      map_some conv_tac (concls_of (sequent goal)) goal

let beta_tac ?info ?f goal = 
  try 
    seq_some
      [ 
	betaC ?info ?c:f;
	betaA ?info ?a:f
      ] goal
  with err -> raise (add_error "beta_tac" err)

let name_tac ?info n lbl goal = 
  let sqnt = sequent goal
  in 
  match Lib.try_app (Logic.get_label_asm lbl) sqnt with
    | Some _ -> lift_info ?info (Logic.Tactics.nameA n lbl) goal
    | None -> lift_info ?info (Logic.Tactics.nameC n lbl) goal

(*** Unification tactics ***)

let find_basic asm concl node = 
  let sqnt = sequent node in 
  let scp = scope_of node
  and node_asms = 
    match asm with 
      | None -> asms_of sqnt
      | Some(x) -> [get_tagged_asm x node]
  and node_concls = 
    match concl with
      | None ->  concls_of sqnt
      | Some(x) -> [get_tagged_concl x node]
  in 
  let find_match c =  
    Lib.try_find 
      (Lib.first (fun x -> Formula.alpha_equals scp (drop_tag x) c)) 
      node_asms
  in 
  let rec find_basic_aux xs =
    match xs with
      | [] -> raise Not_found
      | cform::cs -> 
        begin
	  match find_match (drop_tag cform) with
	    | Some aform -> 
	      (ftag (drop_formula aform), ftag (drop_formula cform))
	    | None -> find_basic_aux cs 
        end
  in 
  find_basic_aux node_concls

let basic ?info ?a ?c goal =
  match (a, c) with 
    | (Some albl, Some clbl) ->
      begin
        try lift_info ?info (Logic.Tactics.basic albl clbl) goal
        with err -> raise (add_error "basic: failed" err)
      end
    | _ -> 
      begin
        match Lib.try_find (find_basic a c) goal with
	  | None -> raise (error "basic: failed")
          | Some(al, cl) -> 
            lift_info ?info (Logic.Tactics.basic al cl) goal
      end


let unify_engine_tac ?info (atg, aform) (ctg, cform) goal =
  let sqnt = sequent goal in 
  let scope = Logic.Sequent.scope_of sqnt in 
  let albl = ftag atg
  and clbl = ftag ctg
  in 
  let asm = Formula.term_of aform
  and concl = Formula.term_of cform
  in 
  let asm_vars, asm_body =  Term.strip_qnt Basic.All asm
  and concl_vars, concl_body = Term.strip_qnt Basic.Ex concl
  in 
  let asm_varp x = Rewrite.is_free_binder asm_vars x
  and concl_varp x = Rewrite.is_free_binder concl_vars x
  in 
  let varp x = (asm_varp x) || (concl_varp x) in 
  let env1 = 
    (* unify asm and concl *)
    try Unify.unify scope varp asm concl
    with _ -> 
      (* unify asm and concl_body with concl_vars *)
      try Unify.unify scope concl_varp asm concl_body
      with _ -> 
        (* unify asm_body and concl with asm_vars *)
	try Unify.unify scope asm_varp asm_body concl
	with _ -> 
          (* unify asm_body and concl_body with all vars *)
	  try Unify.unify scope asm_varp asm_body concl_body
	  with _ -> raise (error "Can't unify formulas")
  in 
  let asm_consts = extract_consts asm_vars env1
  and concl_consts = extract_consts concl_vars env1
  in 
  seq 
    [
      instA ?info:info ~a:albl asm_consts;
      instC ?info:info ~c:clbl concl_consts;
      (basic ?info:info ~a:albl ~c:clbl // skip) 
    ] goal

let unify_tac ?info ?a ?c goal =
  let sqnt = sequent goal
  in 
  let asms = 
    match a with
      | None -> asms_of sqnt
      | Some(l) -> 
        begin
	  try [get_tagged_asm l goal]
	  with Not_found -> 
	    raise (error "unify_tac: No such assumption")
	    | err -> raise (add_error "unify_tac" err)
        end
  and concls = 
    match c with
      | None -> concls_of sqnt
      | Some(l) -> 
	begin
          try [get_tagged_concl l goal]
	  with Not_found -> 
	    raise (error "unify_tac: No such conclusion")
	    | err -> raise (add_error "unify_tac" err)
        end
  in 
  let tac c g = map_first (fun x -> unify_engine_tac x c) asms g
  in 
  try map_first tac concls goal
  with err -> raise (add_error "unify_tac" err)

let substA ?info rs l g = 
  lift_info ?info (Logic.Tactics.substA rs l) g

let substC ?info rs l g = 
  lift_info ?info (Logic.Tactics.substC rs l) g


(*
 * Derived tactics and tacticals
 *)

(** [named_tac tac anames cnames]: apply [tac ~info:inf goal], rename
    each of [aformulas inf] with a name from [anames], rename each of
    [Info.cformulas inf] with a name from [cnames], in order. Set
    [info=inf'] where [inf'] is [inf], with the formula tag produced
    by renaming.  *)
let named_tac ?info tac anames cnames (goal: Logic.node) =
  let inf1 = Info.make()
  and inf2 = Info.make()
  in 
  let rec name_list ns ls g = 
    match (ns, ls) with 
      | ([], _) -> g
      | (_, []) -> g
      | (x::xs, y::ys) -> 
	name_list xs ys (foreach (name_tac ~info:inf2 x y) g)
  in 
  let g1 = tac ~info:inf1 goal in 
  let albls = List.map ftag (Info.aformulas inf1)
  and clbls = List.map ftag (Info.cformulas inf1)
  in 
  let g2 = name_list anames albls g1 in 
  let g3 = name_list cnames clbls g2
  in 
  Info.add info 
    (Info.subgoals inf1) 
    (List.rev (Info.aformulas inf2)) 
    (List.rev (Info.cformulas inf2))
    (Info.constants inf1);
  g3


(*** Pattern matching tacticals ***)

(** [find_match_formulas typenv scp varp t fs]

    Match a list of tagged formulas. Return the tag of the first
    formula in [fs] to unify with term [t] in scope [scp].

    [varp] determines which terms can be bound by unification.
    [typenv] is the goals type environment.

    @raise [Not_found] if no match.
*)
let find_match_formulas typenv scp varp t fs=
  let rec match_aux l = 
    match l with 
      | [] -> raise Not_found
      | tf::tfs ->
        begin
	  try 
	    let tg, f = tf
	    in
	    ignore(Unify.unify ~typenv:typenv scp varp t (Formula.term_of f));
	    tg
	  with _ -> match_aux tfs
        end
  in
  Logic.FTag (match_aux fs)

let find_match_asm typenv t sq=
  let scp = Logic.Sequent.scope_of sq
  and asms = Logic.Sequent.asms sq
  in 
  let t1 = Lterm.set_names scp t in
  let vars = Term.get_free_vars t1 in
  let varp x = 
    try ignore(List.find (Term.equals x) vars); true
    with Not_found -> false
  in 
  find_match_formulas typenv scp varp t1 asms

let find_match_concl typenv t sq=
  let scp = Logic.Sequent.scope_of sq
  and concls = Logic.Sequent.concls sq
  in
  let t1= Lterm.set_names scp t in
  let vars = Term.get_free_vars t1 in
  let varp x = 
    try ignore(List.find (Term.equals x) vars); true
    with Not_found -> false
  in 
  find_match_formulas typenv scp varp t1 concls

let match_asm trm tac g =
  try tac (find_match_asm (typenv_of g) trm (sequent g)) g
  with Not_found -> raise (Term.term_error "No matching assumption" [trm])

let match_concl trm tac g =
  try tac (find_match_concl (typenv_of g) trm (sequent g)) g
  with Not_found -> raise (Term.term_error "No matching conclusion" [trm])

let match_formula trm tac g =
  let sqnt =sequent g
  and tyenv = typenv_of g
  in 
  try tac (find_match_asm tyenv trm sqnt) g
  with Not_found -> 
    try tac (find_match_concl tyenv trm sqnt) g
    with Not_found ->
      raise (Term.term_error "No matching formula in sequent" [trm])

let specA ?info ?a g =
  let inf1 = Info.make() in 
  let add_data (atgs, cs) =
    Info.add info [] [get_one atgs] [] (List.rev cs) 
  in 
  alt
    [
      seq 
        [ 
	  repeat (existA ~info:inf1 ?a);
          notify_tac 
            add_data ((Info.aformulas inf1), (Info.constants inf1))
            skip
        ];
      fail ~err:(error "specA")
    ] g

let specC ?info ?c g =
  let inf1 = Info.make() in 
  let add_data (ctgs, cs) =
    Info.add info [] [] [get_one ctgs] (List.rev cs) 
  in 
  alt
    [
      seq 
        [ 
	  repeat (allC ~info:inf1 ?c);
	  notify_tac
            add_data ((Info.cformulas inf1), (Info.constants inf1)) 
            skip
        ];
      fail ~err:(error "specC")
    ] g
    
let spec_tac ?info ?f g =
  alt
    [
      specC ?info ?c:f;
      specA ?info ?a:f;
      fail ~err:(error "specA")
    ] g

(*
 * Rewriting 
 *)

type ('a)plan = ('a)Rewrite.plan
(** Rewrite plans *)

type rule = Logic.rr_type
(** Rewrite rules *)

let leftright = Rewrite.leftright
let rightleft = Rewrite.rightleft

let rewrite_control ?max ?(strat=Rewrite.topdown) dir =
  Rewrite.control ~max:max ~dir:dir ~strat:strat 

let is_rewrite_formula t =
  let (_, t1) = Term.strip_qnt Basic.All t
  in 
  Lterm.is_equality t1


(** [conv_rule scp conv thm] apply conversion [conv] to theorem [thm]
*)
let conv_rule scp conv thm =
  let info = Info.make() in 
  let term = Logic.term_of thm in 
  let rule = conv scp term in 
  let (qs, lhs, rhs) = 
    let (qs, body) = Term.strip_qnt Basic.All (Logic.term_of rule) in 
    let (l, r) = Lterm.dest_equality body
    in 
    (qs, l, r)
  in 
  let goal_term = 
    match qs with 
      | [] -> rhs
      | _ -> Lterm.close_term rhs
  in 
  let goal = mk_goal scp (Formula.make scp goal_term) in 
  let _ = Info.form_changes (Some info) (goal_changes goal) in
  let tac g =
    let ctag = 
      Lib.get_one (Info.cformulas info) (Failure "pure_rewrite_rule")
    in 
    seq
      [ 
	cut ~info:info thm;
	fun g1 ->
          begin
	    let atag = 
	      Lib.get_one (Info.aformulas info) (Failure "pure_rewrite_rule")
	    in 
	    seq
	      [
	        cut ~info:info rule;
	        (fun g2 ->
	          let rtag = 
		    Lib.get_one (Info.aformulas info)
		      (error "pure_rewrite_rule: cut rule")
	          in 
	          seq
		    [
		      substA ~info:info [ftag rtag] (ftag atag);
		      basic ~a:(ftag atag) ~c:(ftag ctag)
		    ] g2)
	      ] g1
          end
      ] g
  in
  mk_thm (Logic.Subgoals.apply_to_goal tac goal)

(** [pure_rewriteA info p l]: Rewrite assumption [l] with plan [p].
*)
let pure_rewriteA ?info ?term plan lbl goal =
  let inf = Info.make() in 
  let ltag = Logic.label_to_tag lbl (sequent goal) in 
  let trm = 
    match term with
      | None -> Formula.term_of (get_asm lbl goal)
      | Some(x) -> x
  in 
  let tac1 g = 
    lift_info ~info:inf (Logic.Tactics.rewrite_intro plan trm) g
  in 
  let tac2 g =
    let rule_tag = get_one (Info.aformulas inf)
    in 
    seq
      [
        lift_info ?info (Logic.Tactics.substA [ftag (rule_tag)] (ftag ltag));
        deleteA (ftag rule_tag)
      ] g
  in 
  try seq [ tac1; tac2 ] goal 
  with 
    | Not_found -> raise Not_found
    | err -> raise (add_error "Tactics.Rewriter.pure_rewriteA" err)

(** [pure_rewriteC info p l]: Rewrite conclusion [l] with plan [p].
*)
let pure_rewriteC ?info ?term plan lbl goal =
  let inf = Info.make() in 
  let ltag = Logic.label_to_tag lbl (sequent goal) in 
  let trm = 
    match term with
      | None -> Formula.term_of (get_concl lbl goal)
      | Some(x) -> x
  in 
  let tac1 g =
    lift_info ~info:inf (Logic.Tactics.rewrite_intro plan trm) g
  in 
  let tac2 g =
    let rule_tag = get_one (Info.aformulas inf)
    in 
    seq
      [
        lift_info ?info 
          (Logic.Tactics.substC [ftag (rule_tag)] (ftag ltag));
        deleteA (ftag rule_tag)
      ] g
  in 
  try seq [ tac1; tac2 ] goal 
  with 
      Not_found -> raise Not_found
    | err -> raise (add_error "Tactics.Rewriter.pure_rewriteA" err)

(** [pure_rewrite info p l]: Combination of [pure_rewriteC] and
    [pure_rewriteA]. First tries [pure_rewriteC] then tries
    [pure_rewriteA].
*)
let pure_rewrite_tac ?info ?term plan lbl goal =
  try
    begin
      try pure_rewriteC ?info ?term plan lbl goal
      with Not_found -> (pure_rewriteA ?info ?term plan lbl goal)
    end
  with 
    | Not_found -> raise Not_found
    | err -> raise (add_error "Tactics.pure_rewrite_tac" err)

(** [pure_rewrite_conv plan scp trm]: rewrite term [trm] according to
    [plan] in scope [scp]. This is an interface to
    {!Logic.Conv.rewrite_conv}.

    Returns [|- trm = X] where [X] is the result of rewriting [trm]
*)
let pure_rewrite_conv = Logic.Conv.rewrite_conv

(** [pure_rewrite_rule plan scp thm]: rewrite theorem [thm] according
    to [plan] in scope [scp].

    Returns [|- X] where [X] is the result of rewriting [trm]
*)
let pure_rewrite_rule plan scp thm =
  conv_rule scp (pure_rewrite_conv plan) thm

(*
 * Rewrite planner
 *)

let dest_term x p=
  let qs, b = Term.strip_qnt Basic.All x in 
  let lhs, rhs= Lterm.dest_equality b
  in 
  (qs, lhs, rhs, p)

(** [extract_rules (scp, node) rl]: Extract the rewrite rule [rl],
    getting assumptions from [node].
    
    Extracts the assumptions to use as a rule from subgoal
    [sg]. Checks that other rules are in the scope of [sg]. Creates
    unordered or ordered rewrite rules as appropriate.

    Fails if any rule in [rls] is the label of an assumption which
    does not exist in [sg].

    Fails if any rule in [rls] is not in scope.
*)
let extract_rule node src= 
  let (form, p) = 
    match src with
      | Asm(x) ->
	  let sq = 
	    Subgoals.node_sqnt 
	      (Lib.dest_option ~err:(error "extract_rule") node)
	  in
	  let asm =
	    try drop_tag(Sequent.get_tagged_asm (label_to_tag x sq) sq)
	    with Not_found -> 
	      raise (error "extract_rule: can't find tagged assumption")
	  in 
 	  (asm, None)
      | OAsm(x, order) ->
	let sq = 
	  Subgoals.node_sqnt 
            (Lib.dest_option ~err:(error "extract_rule") node)
	in
	let asm=
	  try drop_tag (Sequent.get_tagged_asm (label_to_tag x sq) sq)
	  with Not_found -> 
	    raise (error "extract_rule: can't find tagged assumption")
	in 
	(asm, Some(order))
      | RRThm(x) -> (formula_of x, None)
      | ORRThm(x, order) -> (formula_of x, Some(order))
  in 
  dest_term (Formula.term_of form) p

(** The main rewrite planner *)

module PlannerData =
struct
  type rule = Logic.rr_type
  type data = Logic.node option
  let dest = extract_rule
end

module Planner = Rewrite.Make(PlannerData)

let mk_plan ?(ctrl=Formula.default_rr_control) goal rules term =
  let scp = scope_of goal in 
  let (_, plan) = Planner.make (Some(goal)) scp ctrl rules term
  in 
  plan

(** The theorem rewrite planner (for conversions) *)
let dest_rr_thm src = 
  match src with
    | RRThm(x) -> x
    | ORRThm(x, _) -> x
    | _ -> failwith "Tactics.Rewriter.dest_rr_thm"

let dest_rr_thm src = 
  match src with
    | RRThm(x) -> x
    | ORRThm(x, _) -> x
    | _ -> failwith "Tactics.Rewriter.dest_rr_thm"

let to_thm_plan plan = 
  mapping dest_rr_thm plan

let mk_thm_plan scp ?(ctrl=Formula.default_rr_control) rules term =
  let (_, plan) = Planner.make None scp ctrl rules term
  in 
  to_thm_plan plan

