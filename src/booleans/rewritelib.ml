(*----
  Name: rewritelib.ml
  Copyright M Wahab 2006-2010
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

open Boolutil
open Boolbase
open Commands
open Tactics
open Lib.Ops

(*** Generalised Rewriting ***)

module Rewriter = 
struct

  (** [rewrite_conv scp ctrl rules trm]: rewrite term [trm] with rules
      [rrl] in scope [scp].

      Returns |- trm = X where [X] is the result of rewriting [trm]
  *)
  let rewrite_conv ?ctrl rls scp term = 
    let c = Lib.get_option ctrl Rewrite.default_control in 
    let is_rl = c.Rewrite.rr_dir=rightleft in 
    let mapper f x = 
      match x with
        | Logic.RRThm t -> Logic.RRThm(f t)
        | Logic.ORRThm (t, o) -> Logic.ORRThm(f t, o)
        | _ -> 
	  raise (error "rewrite_conv: Invalid assumption rewrite rule")
    in 
    let rules = 
      if is_rl 
      then List.map (mapper (eq_sym_rule scp)) rls
      else rls
    in
    let plan = Tactics.mk_thm_plan scp ~ctrl:c rules term
    in 
    Tactics.pure_rewrite_conv plan scp term

  (** [rewrite_rule scp ctrl rules thm: rewrite theorem [thm] with rules
      [rrl] in scope [scp].

      Returns |- X where [X] is the result of rewriting [thm]
  *)
  let rewrite_rule scp ?ctrl rls thm = 
    let c = Lib.get_option ctrl Rewrite.default_control in 
    let is_rl = c.Rewrite.rr_dir=rightleft in 
    let mapper f x = 
      match x with
        | Logic.RRThm t -> Logic.RRThm(f t)
        | Logic.ORRThm (t, o) -> Logic.ORRThm(f t, o)
        | _ -> 
	  raise (error "rewrite_conv: Invalid assumption rewrite rule")
    in 
    let rules = 
      if is_rl 
      then List.map (mapper (eq_sym_rule scp)) rls
      else rls
    in
    let plan = Tactics.mk_thm_plan scp ~ctrl:c rules (Logic.term_of thm)
    in 
    Tactics.pure_rewrite_rule plan scp thm

  (** [map_sym_tac ret rules goal]: Apply [eq_sym] to each rule in
      [rules], returning the resulting list in [ret]. The list in
      [ret] will be in reverse order of [rules].  *)
  let map_sym_tac ret rules goal = 
    let scp = scope_of goal in 
    let asm_fn l g = 
      try 
	let g2 = eq_symA l g in 
	let nl = Lib.get_one (New.aformulas (New.branch_changes g2))
	  (error "Rewriter.map_sym_tac: Invalid assumption")
	in 
	(ftag nl, g2)
      with err -> raise (add_error "Rewriter.map_sym_tac" err)
    in 
    let fn_tac r g =
      match r with
	| Logic.RRThm(th) -> 
	  (Logic.RRThm(eq_sym_rule scp th), skip g)
	| Logic.ORRThm(th, o) -> 
	  (Logic.ORRThm(eq_sym_rule scp th, o), skip g)
	| Logic.Asm(l) -> 
	  let (nl, ng) = asm_fn l g
	  in 
	  (Logic.Asm(nl), ng)
	| Logic.OAsm(l, o) -> 
	  let (nl, ng) = asm_fn l g
	  in 
	  (Logic.OAsm(nl, o), ng)
    in 
    let mapping lst rl g = 
      let (nr, g2) = fn_tac rl g
      in 
      lst := nr::(!lst); g2
    in 
    map_every (mapping ret) rules goal

  let rewriteA_tac 
      ?(ctrl=Formula.default_rr_control) 
      rules albl goal =
    let (atag, aform) = get_tagged_asm albl goal in 
    let aterm = Formula.term_of aform in 
    let is_lr = not (ctrl.Rewrite.rr_dir = rightleft) in 
    let urules = ref [] in 
    let tac1 g = 
      if is_lr 
      then update_tac (fun _ -> urules := rules) () g
      else 
	seq 
	  [
	    map_sym_tac urules rules;
	    (fun g1 -> update_tac (fun x -> urules := List.rev !x) urules g1);
	  ] g
    in 
    let tac2 g = 
      let rls = !urules in 
      let plan = Tactics.mk_plan ~ctrl:ctrl g rls aterm
      in 
      Tactics.pure_rewriteA plan (ftag atag) g
    in 
    let tac3 g = 
      if is_lr
      then skip g
      else map_sym_tac (ref []) rules g
    in 
    try seq [ tac1; tac2; 
              (?> fun inf -> tac3 ++ (set_changes_tac inf))
            ] goal
    with err -> raise (add_error "Rewriter.rewriteA_tac" err)

  let rewriteC_tac  ?(ctrl=Formula.default_rr_control) 
      rules clbl goal =
    let (ctag, cform) = get_tagged_concl clbl goal in 
    let cterm = Formula.term_of cform in 
    let is_lr = (ctrl.Rewrite.rr_dir = leftright) in 
    let urules = ref [] in 
    let tac1 g = 
      if is_lr
      then update_tac (fun x -> urules := x) rules g
      else 
	seq 
	  [
	    map_sym_tac urules rules;
	    (fun g1 -> update_tac (fun x -> urules := List.rev !x) urules g1);
	  ] g
    in 
    let tac2 g = 
      let rls = !urules in 
      let plan = Tactics.mk_plan ~ctrl:ctrl g rls cterm
      in 
      Tactics.pure_rewriteC plan (ftag ctag) g
    in 
    let tac3 g = 
      if is_lr
      then skip g
      else map_sym_tac (ref []) rules g
    in 
    try seq [tac1; tac2;
             (?> fun inf -> tac3 ++ (set_changes_tac inf))
            ] goal
    with err -> raise (add_error "Rewriter.rewriteA_tac" err)

  (** [rewrite_tac ?info ctrl rules l sq]: Rewrite formula [l] with
      [rules].
      
      If [l] is in the conclusions then call [rewriteC_tac] otherwise
      call [rewriteA_tac].  *)
  let rewrite_tac ?(ctrl=Formula.default_rr_control) rls f g =
    try
      begin
        try rewriteA_tac ~ctrl:ctrl rls f g
        with Not_found -> rewriteC_tac ~ctrl:ctrl rls f g
      end
    with err -> raise (add_error "Rewriter.rewrite_tac" err)

end

let rewrite_conv ?ctrl rls scp trm = 
  Rewriter.rewrite_conv ?ctrl 
    (List.map (fun x -> Logic.RRThm(x)) rls) scp trm

let rewrite_rule scp ?ctrl rls thm = 
  Rewriter.rewrite_rule ?ctrl scp 
    (List.map (fun x -> Logic.RRThm(x)) rls) thm

let gen_rewrite_tac ?asm ctrl ?f rules goal =
  match f with
    | None -> 
      begin
        match asm with
	  | None -> 
	    seq_some
	      [
	        foreach_asm
	          (fun l -> (?> fun inf1 ->
                      Rewriter.rewriteA_tac ~ctrl:ctrl rules l
                      ++ append_changes_tac inf1));
	        foreach_concl
                  (fun l -> (?> fun inf1 ->
	            Rewriter.rewriteC_tac ~ctrl:ctrl rules l 
                    ++ append_changes_tac inf1));
	      ] goal
          | Some(x) ->
	    if x 
	    then 
              foreach_asm
	        (fun l -> (?> fun inf1 ->
                  Rewriter.rewriteA_tac ~ctrl:ctrl rules l
                    ++ append_changes_tac inf1))
                goal
	    else foreach_concl
	      (fun l -> (?> fun inf1 ->
                Rewriter.rewriteC_tac  ~ctrl:ctrl rules l
                ++ append_changes_tac inf1)) goal
      end
    | Some (x) -> Rewriter.rewrite_tac ~ctrl:ctrl rules x goal
	
let rewrite_tac ?(dir=leftright) ?f ths goal =
  let ctrl = rewrite_control dir in 
  let rules = List.map (fun x -> Logic.RRThm x) ths 
  in 
  gen_rewrite_tac ctrl ?f:f rules goal

let once_rewrite_tac ?(dir=leftright) ?f ths goal =
  let ctrl= rewrite_control ~max:1 dir in 
  let rules = List.map (fun x -> Logic.RRThm x) ths 
  in 
  gen_rewrite_tac ctrl rules ?f:f goal

let rewriteA_tac ?(dir=leftright) ?a ths goal =
  let ctrl = rewrite_control dir in 
  let rules = List.map (fun x -> Logic.RRThm x) ths
  in 
  gen_rewrite_tac ~asm:true ctrl ?f:a rules goal 

let once_rewriteA_tac ?(dir=leftright) ?a ths goal =
  let ctrl= rewrite_control ~max:1 dir in 
  let rules = List.map (fun x -> Logic.RRThm x) ths 
  in 
  gen_rewrite_tac ~asm:true ctrl rules ?f:a goal

let rewriteC_tac ?(dir=leftright) ?c ths goal =
  let ctrl = rewrite_control dir in 
  let rules = List.map (fun x -> Logic.RRThm x) ths 
  in 
  gen_rewrite_tac ~asm:false ctrl ?f:c rules goal 

let once_rewriteC_tac ?(dir=leftright) ?c ths goal =
  let ctrl= rewrite_control ~max:1 dir in 
  let rules = List.map (fun x -> Logic.RRThm x) ths 
  in 
  gen_rewrite_tac ~asm:false ctrl rules ?f:c goal

let gen_replace_tac ?(ctrl=Formula.default_rr_control) ?asms ?f goal =
  let sqnt = sequent goal in
  (*** ttag: The tag of tag of the target (if given) ***)
  let ttag = 
    match f with 
      | None -> None 
      | Some(x) -> Some(Logic.label_to_tag x sqnt)
  in 
  (*** exclude: a predicate to filter the rewriting target ***)
  let exclude tg = 
    match ttag with 
      | None -> false 
      | Some(x) -> Tag.equal tg x
  in 
  (*** find_equality_asms: Find the assumptions which are equalities ***)
  let rec find_equality_asms sqasms rst =
    match sqasms with 
      | [] -> List.rev rst
      | form::xs -> 
	let tg = drop_formula form 
	in 
	if (not (exclude tg))
	  && (qnt_opt_of Basic.All 
		(Lterm.is_equality) (Formula.term_of (drop_tag form)))
	then find_equality_asms xs (tg::rst)
	else find_equality_asms xs rst
  in 
  (*** asm_tags: The assumptions to use for rewriting. ***)
  let asm_tags =
    match asms with
      | None -> find_equality_asms (Logic.Sequent.asms sqnt) []
      | Some xs -> List.map (fun x -> Logic.label_to_tag x sqnt) xs
  in 
  (*** rules: Assumption labels in rewriting form ***)
  let rules = List.map (fun x -> Logic.Asm (ftag x)) asm_tags
  in 
  (*** filter_replace: The replacment tactics, filtering the target to
       avoid trying to rewrite a formula with itself.  ***)
  let filter_replace x =
    if List.exists (Tag.equal (Logic.label_to_tag x sqnt)) asm_tags
    then fail ~err:(error "gen_replace")
    else 
      gen_rewrite_tac ?asm:None ctrl rules ~f:x
  in 
  (*** tac: apply filter_replace to an identified formula or to all
       formulas in the sequent.  ***)
  let tac = 
    match ttag with
      | None -> foreach_form filter_replace
      | Some(x) -> filter_replace (ftag x)
  in 
  alt 
    [
      tac;
      fail ~err:(error "gen_replace")
    ] goal


let replace_tac ?(dir=leftright) ?asms ?f goal =
  let ctrl=rewrite_control dir
  in 
  gen_replace_tac ~ctrl:ctrl ?asms:asms ?f:f goal

let once_replace_tac ?(dir=leftright) ?asms ?f goal =
  let ctrl=rewrite_control ~max:1 dir
  in 
  gen_replace_tac ~ctrl:ctrl ?asms:asms ?f:f goal

let unfold ?f str g = 
  match Lib.try_find defn str with
    | None -> raise (error ("unfold: Can't find definition of "^str))
    | Some th -> rewrite_tac ?f [th] g
