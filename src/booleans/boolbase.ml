(*----
  Name: boolbase.ml
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
open Commands
open Tactics
open Lib.Ops

(*** Basic Tactics ***)

let make_false_def () = thm (Lterm.base_thy ^"."^"false_def")
let false_def_var = Lib.freeze make_false_def
let false_def () = Lib.thaw ~fresh:fresh_thm false_def_var

let falseA ?info ?a goal =
  let af = first_asm_label a Formula.is_false goal in 
  let th =
    try false_def()
    with Not_found -> 
      raise (Report.error 
	       ("Tactics.Rewriter.falseA: "
	        ^"Can't find needed theorem false_def: |- false = not true"))
  in 
  let plan = Rewrite.mk_node [Rewrite.mk_rules [Logic.RRThm(th)]] in 
  let inf = mk_info()
  in 
  seq
    [ 
      pure_rewriteA ~info:inf plan af;
      (fun g ->
	let atag = Lib.get_one (aformulas inf) (Tactics.error "falseA")
	in 
	seq
	  [ 
	    negA ~info:inf ~a:(ftag atag);
	    (fun g1  -> 
	      let ctag = Lib.get_one (cformulas inf) (error "falseA")
	      in 
	      trueC ~c:(ftag ctag) g1)
	  ] g)
    ] goal
    
let trivial ?info ?f g =  
  try (trueC ?info ?c:f // falseA ?info ?a:f) g
  with _ -> raise (error "trivial")

let cut_thm ?info ?inst str = (cut ?info ?inst (thm str))

(*** Basic equality reasoning ***)

let make_eq_refl_thm () = 
  try thm (Ident.string_of (Ident.mk_long Lterm.base_thy "eq_refl"))
  with Not_found ->
    raise (error 
	     ("Tactics.Rewriter.make_eq_refl_thm:"
	      ^"Can't find needed axiom eq_refl: |- !x: (x = x)"))
      
let eq_refl_thm_var = Lib.freeze make_eq_refl_thm
let eq_refl_thm () =  Lib.thaw ~fresh:fresh_thm eq_refl_thm_var

let make_bool_cases_thm () = 
  try thm (Ident.string_of (Ident.mk_long Lterm.base_thy "bool_cases"))
  with Not_found ->
    raise (error 
	     ("Tactics.Rewriter.make_bool_cases_thm:"
	      ^"Can't find needed axiom bool_cases: "
	      ^"|- !x: (x = true) | (x=false)"))

let bool_cases_thm_var = Lib.freeze make_bool_cases_thm
let bool_cases_thm () = Lib.thaw ~fresh:fresh_thm bool_cases_thm_var

let make_eq_sym_thm () = 
  match Lib.try_app thm "Bool.eq_sym" with
    | Some(th) -> th
    | None -> 
      begin
        let eq_l1 =
	  prove << !x y : (x = y) => (y = x) >>
	  ((repeat allC) ++ implC
	   ++ substC [!~1] (!! 1) 
	   ++ cut ~inst:[ << _y >> ] (eq_refl_thm ()) ++ basic)
        in 
        let eq_l2 =
	  prove << !x y : ((x => y) & (y => x)) => (x = y)>>
	  ((repeat allC)
	   ++ cut ~inst:[ << _x >>] (bool_cases_thm()) ++ disjA
	   ++ cut ~inst:[ << _y >>] (bool_cases_thm()) ++ disjA
	   ++ substC [ !~ 1; !~ 2] (!! 1) ++ implC
	   --
	     [
	       cut ~inst:[ << true >> ] (eq_refl_thm()) ++ basic ;
	       conjA ++ implA ++ trivial;
	       conjA ++ implA ++ implA ++ trivial;
	       cut ~inst:[ << false >> ] (eq_refl_thm()) ++ basic
	     ])
        in 
        prove << !x y : (x = y) = (y = x)>>
            ((repeat allC)
	     ++ cut ~inst:[ << _x = _y >> ; << _y = _x >>] eq_l2
	     ++ implA 
	     --
	       [ 
	         conjC
	         -- 
	           [
	             cut ~inst:[ << _x >> ; << _y >> ] eq_l1 ++ basic ;
	             cut ~inst:[ << _y >> ; << _x >> ] eq_l1 ++ basic 
	           ] ;
	         basic
	       ])
      end

let eq_sym_thm_var = Lib.freeze make_eq_sym_thm
let eq_sym_thm () = Lib.thaw ~fresh:fresh_thm eq_sym_thm_var

let eq_sym_rule scp thm= 
  let ctrl = {Formula.default_rr_control with Rewrite.depth = Some 1} in 
  let term = Logic.term_of thm in 
  let plan = 
    Tactics.mk_thm_plan scp ~ctrl:ctrl [ Logic.RRThm (eq_sym_thm()) ] term
  in 
  Tactics.pure_rewrite_rule plan scp thm

let eq_symA ?info a goal =
  let ctrl = {Formula.default_rr_control with Rewrite.depth = Some 1} in 
  let (atag, form) = get_tagged_asm a goal in 
  let term = Formula.term_of form in 
  let plan = 
    Tactics.mk_plan ~ctrl:ctrl goal [ Logic.RRThm (eq_sym_thm()) ] term
  in 
  Tactics.pure_rewriteA ?info plan (ftag atag) goal

let eq_symC ?info c goal =
  let ctrl = {Formula.default_rr_control with Rewrite.depth = Some 1} in 
  let (ctag, form) = (get_tagged_concl c goal) in 
  let term = Formula.term_of form in 
  let plan = 
    Tactics.mk_plan ~ctrl:ctrl goal [ Logic.RRThm (eq_sym_thm()) ] term
  in 
  Tactics.pure_rewriteC ?info plan (ftag ctag) goal
    
let eq_sym_tac ?info f goal = 
  try eq_symA ?info f goal
  with Not_found -> eq_symC ?info f goal

let eq_tac ?info ?c goal = 
  let th = 
    try thm (Lterm.base_thy ^ ".eq_refl")
    with Not_found -> 
      (raise (error ("eq_tac: Can't find required lemma "
		     ^Lterm.base_thy^".eq_refl")))
  in 
  let info1 = Tactics.mk_info() in 
  let cforms = concls_of (sequent goal) in 
  let tac albl (t, f) g = 
    if Formula.is_equality f
    then unify_tac ~a:albl ~c:(ftag t) g
    else fail g
  in 
  seq 
    [
      Logic.Tactics.cut ~info:info1 th; 
      (fun g -> 
	let af = get_one ~msg:"eq_tac" (Tactics.aformulas info1)
	in 
	map_first (tac (ftag af)) cforms g)
    ] goal


(*** Eliminating boolean operators ***)

(** [direct_alt tacs info l]: Directed alt. Like {!Tactics.alt} but
    pass [info] and [l] to each tactic in [tacs].  **)
let direct_alt tacl info l g =
  let rec alt_aux ts =
    match ts with
      | [] -> raise (error "direct_alt: no successful tactic")
      | tac::tacs ->
	try tac info l g
	with _ -> alt_aux tacs
  in alt_aux tacl 


(** [direct_map_some tac lst l]: Directed map_some. Like
    {!Tactics.map_som} but pass [info] and [l] to [tac]. If [tac] fails
    for [l], then [lst := l::!lst].  **)
let direct_map_some tac lst l goal =
  let add_lbl x = lst := x::(!lst) in 
  let nofail_tac lbl = (tac lbl // data_tac add_lbl lbl) in 
  let rec some_aux ls g =
    match ls with 
      | [] -> fail ~err:(error "direct_map_some: no tactic succeeded.") g
      | (x::xs) ->
	try (tac x ++ map_every nofail_tac xs) g
	with _ -> add_lbl x; some_aux xs g
  in 
  some_aux l goal

(** [asm_elim_rules ?info (arules, crules) f goal]: Apply elimination
    rules to assumption [f] and to all resulting assumptions and
    conclusions. Assumptions are eliminated with [arules], conclusions
    with [crules]. Any new tag which can't be eliminated are stored in
    [?info] (in arbitrary order).
*)
let rec asm_elim_rules_tac ?info rules lbl goal =
  let (arules, _) = rules
  and inf = mk_info()
  and alst = ref []
  and clst = ref []
  in 
  let set_info dst (sgs, afs, cfs, cnsts) = 
    Logic.add_info dst sgs afs cfs cnsts
  in 
  seq
    [ 
      (* Try to elminate the operator. *)
      direct_alt arules inf lbl;
      (* Eliminate new assumptions and conclusions. *)
      (fun g -> 
	let albls = List.map ftag (aformulas inf)
	and clbls = List.map ftag (cformulas inf)
	and sqnt = sequent g
	in 
	seq
	  [
	    (* Eliminate assumptions, saving failing labels. *)
	    alt
	      [ 
	        direct_map_some (asm_elim_rules_tac ?info rules) alst albls;
	        skip
	      ];
	    (* Eliminate conclusions, saving failing labels. *)
	    alt
	      [ 
	        direct_map_some (concl_elim_rules_tac ?info rules) clst clbls;
	        skip
	      ];
	    (* Save failing labels and any other information. *)
	    data_tac (set_info info)
	      (subgoals inf, 
	       List.map 
		 (fun x -> Logic.label_to_tag x sqnt)  
                 (List.rev (!alst)), 
	       List.map 
		 (fun x -> Logic.label_to_tag x sqnt)
                 (List.rev (!clst)), 
	       constants inf)
	  ] g)
    ] goal
(** [concl_elim_rules ?info (arules, crules) f goal]: Apply
    elimination rules to conclusion [f] and to all resulting
    assumptions and conclusions. Assumptions are eliminated with
    [arules], conclusions with [crules]. The tag of any new
    formula for which the elimination rules fails is stored in
    [?info] (in arbitrary order).  *)
and concl_elim_rules_tac ?info rules lbl goal =
  let (_, crules) = rules
  and inf = mk_info()
  and alst = ref []
  and clst = ref []
  in 
  let set_info dst (sgs, afs, cfs, cnsts) = 
    Logic.add_info dst sgs afs cfs cnsts
  in 
  seq
    [ 
      (* Try to elminate the operator. *)
      direct_alt crules inf lbl;
      (* Eliminate new assumptions and conclusions. *)
      (fun g -> 
	let albls = List.map ftag (aformulas inf)
	and clbls = List.map ftag (cformulas inf)
	and sqnt = sequent g
	in 
	seq
	  [
	    (* Eliminate conclusions, saving failing labels. *)
	    alt
	      [ 
	        direct_map_some (concl_elim_rules_tac ?info rules) clst clbls;
	        skip
	      ];
	    (* Eliminate assumptions, saving failing labels. *)
	    alt
	      [ 
	        direct_map_some (asm_elim_rules_tac ?info rules) alst albls;
	        skip
	      ];
	    (* Save failing labels and any other information. *)
	    data_tac (set_info info)
	      (subgoals inf, 
	       List.map 
		 (fun x -> Logic.label_to_tag x sqnt)  
                 (List.rev (!alst)), 
	       List.map 
		 (fun x -> Logic.label_to_tag x sqnt)  
                 (List.rev (!clst)), 
	       constants inf)
	  ] g)
    ] goal

(** [elim_rules_tac ?info (arules, crules) albls clbls]: Apply
    elimination rules to all assumptions with a label in [albls] and
    all conclusions with a label in [clbls] and to all resulting
    assumptions and conclusions. The tag of any new formula for which
    the elimination rules fails is stored in [?info] (in arbitrary
    order).
*)
let elim_rules_tac ?info rules albls clbls =
  match albls with 
    | [] -> map_some (concl_elim_rules_tac ?info rules) clbls
    | _ ->
      let chng = ref false in 
      let notify_chng _ = chng := true in
      let tac g =
	seq
	  [
	    alt 
	      [
	        notify_tac notify_chng ()
		  (map_some (asm_elim_rules_tac ?info rules) albls);
	        skip
	      ];
	    alt 
	      [ 
	        notify_tac notify_chng ()
		  (map_some (concl_elim_rules_tac ?info rules) clbls); 
	        skip 
	      ]
	  ] g
      in 
      restrict (fun _ -> !chng) tac

(** [apply_elim_tac tac ?info ?f]: Apply elimination tactic [tac] to
    formula [?f]. If [?f] is not given, use all formulas in the
    sequent. The tag of any new formula for which the elimination rules
    fails is stored in [?info] (in arbitrary order).

    [apply_elim_tac] is a wrapper for [elim_rules_tac].
*)
let apply_elim_tac tac ?info ?f goal =
  let sqnt = sequent goal in 
  let alst, clst = 
    match f with 
      | None -> 
        let get_formula_ftag x = ftag (drop_formula x) in
	(List.map get_formula_ftag (asms_of sqnt), 
	 List.map get_formula_ftag (concls_of sqnt))
      | Some(x) ->
        begin
	  match Lib.try_find (get_asm x) goal with
	    | None -> ([], [x])
	    | _ -> ([x], [])
        end
  in 
  tac ?info alst clst goal
