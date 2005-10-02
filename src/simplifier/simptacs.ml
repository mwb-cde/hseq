(*-----
 Name: simptacs.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(**
   Tactical interface to the simplifier engine. 
*)

open Tactics
open Simplifier

(** Simplification tactics. 

   [make_asm_entries_tac]/[make_concl_entries_tac]: Tactics to prepare
   assumptions/conclusions for use as simp rules.

   [simp_tac]: The standard simplification tactic. Repeatedly
   simplifies formulas until nothing else can be done.

   [once_simp_tac]: Standard simplication applied at most once to each
   formula.
*)

(** [make_asm_entries_tac ret tags except goal]

   Copy, prepare the assumptions in [tags], set ret to [(asm_tags,
   forms)] where [asm_tags] is the list of pairs [(a, e)] where [a] is
   the assumption used to make the rule tagged [e] and [forms] is the
   list of tagged formulas to be used as the simp rules.
*)
let make_asm_entries_tac ret tags except goal=
  let data = ref []
  in 
  (* prepare assumptions *)
  let tac1 g = Simpconvs.prepare_asms data tags except g
  in 
  (* make list of tagged formulas *)
  let tac2 g =
    let sqnt = Tactics.sequent g
    in 
    let asm_forms = Tactics.asms_of sqnt
    and asm_tags = !data
    in 
    let forms = 
      let use (t, f) = 
	List.exists (fun (_, x) -> Tag.equal t x) asm_tags
      in 
      List.filter use asm_forms
    in 
    data_tac (fun x -> ret:=x) (asm_tags, forms) g
  in 
  seq [tac1; tac2] goal

(** [make_concl_entries_tac ret tags except goal]

   Copy, lift, prepare the conclusions in [tags], set ret to
   [(asm_tags, rules)] where [asm_tags] is a list of pairs [(c, a)]
   where [c] is the conclusion used to form assumption [a] (from which
   a simp rule is formed) and [rules] is the list of tagged formulas
   to be used as simp rules.
*)
let make_concl_entries_tac ret tags except goal=
  let data = ref []
  in 
  (* copy, prepare conclusions *)
  let tac1 g=
    Simpconvs.prepare_concls data tags except g
  in 
  (* make list of tagged formulas *)
  let tac2 g =
    let sqnt = Tactics.sequent g
    in 
    let asm_forms = Tactics.asms_of sqnt
    and asm_tags = !data
    in 
    let forms=
      let use (t, f) = 
	List.exists (fun (_, x) -> Tag.equal t x) asm_tags
      in 
      List.filter use asm_forms
    in 
    data_tac (fun x -> ret:=x) (asm_tags, forms) g
  in 
  seq [tac1; tac2] goal


(**
   [simp_engine_tac cntrl asms l goal]:
   The engine for [simp_tac]

   - eliminate toplevel universal quantifiers of [l]
   - if (asms=true),
     put conclusions other than [l] into assumptions and make simp rules
   - if (asms=true), make simp rules from assumptions
   - simplify
   - delete temporary assumptions
*)
let simp_engine_tac (cntrl, ret, except, concl_forms) tag goal=
  let concl_rules = 
    Simpset.make_asm_rules 
      (fun x -> not (Tag.equal tag (fst x))) 
      concl_forms
  in 
  let set=Simpset.simpset_add_rules (Data.get_simpset cntrl) concl_rules
  in 
  let cntrl1=Data.set_simpset cntrl set
  in 
  let tac1 g = simp_prep_tac cntrl1 ret (ftag tag) g
  in 
  let tac2 g =
    let ncntrl = Lib.get_option (!ret) cntrl1
    in 
(*
  let ncntrl1 =
    let exclude_tags =
      try
	[snd(List.find 
	       (fun (t, _) -> Tag.equal tag t) 
	       (Data.get_concl_pairs ncntrl))]
      with Not_found -> 
	try 
	  [snd(List.find 
		 (fun (t, _) -> Tag.equal tag t) 
		 (Data.get_asm_pairs ncntrl))]
	with Not_found -> []
    in Data.set_exclude ncntrl exclude_tags
  in
*)
    ret:=None;
    try 
      basic_simp_tac ncntrl ret tag g
    with e -> 
      (Lib.set_option ret ncntrl; raise e)
  in 
  let trivia_tac g = 
    try Boollib.trivial ~f:(ftag tag) g
    with _ -> skip g
  in 
  ret:=None; 
  seq [repeat (tac1 ++ tac2) ; trivia_tac] goal

(**
   [simp_tac cntrl asms except ?l goal]:
   - eliminate toplevel universal quantifiers of [l]
   - if (asms=true),
     put conclusions other into assumptions and make simp rules
   - if (asms=true), make simp rules from assumptions
   - simplify
   - delete temporary assumptions

   If [l] is not given, repeat for each conclusion.
   Ignore formulas for which [except] is true.
 *)

let rec simp_tac cntrl asms except l goal=
  let sqnt = (Tactics.sequent goal)
  in 
  let asm_forms = Tactics.asms_of sqnt
  and concl_forms = Tactics.concls_of sqnt
  in 
  let asm_tags = List.map (fun (x, _) -> x) asm_forms
  and concl_tags = List.map (fun (x, _) -> x) concl_forms
  in 
  let targets = 
    match l with
      None -> concl_tags
    | Some x -> [Logic.label_to_tag x sqnt]
  in
  let ret=ref (None: Data.t option)
  in 
  let set = Data.get_simpset cntrl
  in 
  let data1 =
    let prover_tac pd pt g= 
      cond_prover_tac pd pt g
    in 
    Data.set_tactic cntrl prover_tac  
  in
  let asm_rules = ref ([], [])
  and concl_rules = ref ([], [])
  and asm_entry_tags = ref []
  and concl_entry_tags = ref []
  in 
  let tac1 g= 
    seq
      [
       (fun _ -> asms) 
	 --> 
	   seq 
	     [make_asm_entries_tac asm_rules asm_tags except;
	      make_concl_entries_tac concl_rules concl_tags except];
	   data_tac 
	     (fun () -> 
	       (* get the information, put it into a useful form *)
	       asm_entry_tags := fst (!asm_rules);
	       concl_entry_tags := fst (!concl_rules);
	       (*
		  update the simp set with the rules for the assumptions,
		  the tags of the visited formulas
		  and the tags of the new formulas.
		*)
	       let rules = 
		 Simpset.make_asm_rules 
		   (fun _ -> false) 
		   (snd(!asm_rules))
	       in 
	       let set1 = Simpset.simpset_add_rules set rules
	       in 
	       let data2=
		 Data.set_asms data1
		   (List.append 
		      (List.map snd (!asm_entry_tags))
		      (Data.get_asms data1))
	       in 
	       let data2a=
		 Data.set_asms data2
		   (List.append 
		      (List.map snd (!concl_entry_tags))
		      (Data.get_asms data2))
	       in 
	       let data3= 
		 Data.set_visited data2a
		   (List.append 
		      (List.map fst (!asm_entry_tags))
		      (List.append  
			 (List.map fst (!concl_entry_tags))
			 (Data.get_asms data2a)))
	       in 
	       let data4= 
		 Data.set_simpset data3 set1
	       in 
	       Lib.set_option ret data4) ()] g
  in 
  let chng = ref false 
  in 
  let tac2 g = 
    let ncntrl = Lib.dest_option ~err:(Failure "simp_tac: 1") (!ret) 
    in 
    Tactics.map_every 
      (fun tg -> 
	alt
	  [seq
	     [simp_engine_tac (ncntrl, ret, except, snd (!concl_rules)) tg;
	      data_tac (fun _ -> chng:=true) ()];
	   skip]) targets g
  in 
  let tac3 g =
    let ncntrl = Lib.dest_option ~err:(Failure "simp_tac: 2") (!ret)
    in 
    clean_up_tac ncntrl g
  in 
  let tac4 g =
    if(!chng) 
    then skip g
    else raise No_change
  in 
  seq [tac1; tac2; tac3; tac4] goal



(**
   [once_simp_tac cntrl set l g]

   Simplify formula [label] with [set], once.

   NOTE: The *only* difference between the code for once_simp_tac and
   the code fore simp_tac is that simp_engine_tac uses [repeat tac2]
   where once_simp_engine_tac has [tac2] (in the last line of the
   tactics).
*)

(**
   [once_simp_engine_tac cntrl asms l goal]:
   The engine for [once_simp_tac]

   - eliminate toplevel universal quantifiers of [l]
   - if (asms=true),
     put conclusions other than [l] into assumptions and make simp rules
   - if (asms=true), make simp rules from assumptions
   - simplify
   - delete temporary assumptions
*)
let once_simp_engine_tac (cntrl, ret, except, concl_forms) tag goal=
  let concl_rules = 
    Simpset.make_asm_rules 
      (fun x -> not (Tag.equal tag (fst x))) 
      concl_forms
  in 
  let set=Simpset.simpset_add_rules (Data.get_simpset cntrl) concl_rules
  in 
  let cntrl1=Data.set_simpset cntrl set
  in 
  let tac1 g = simp_prep_tac cntrl1 ret (ftag tag) g
  in 
  let tac2 g =
    let ncntrl = Lib.get_option (!ret) cntrl1
    in 
    ret:=None;
    try 
      basic_simp_tac ncntrl ret tag g
    with e -> 
      (Lib.set_option ret ncntrl; raise e)
  in 
  let trivial g = 
    try Boollib.trivial ~f:(ftag tag) g
    with _ -> skip g
  in 
  ret:=None; 
  seq [tac1; tac2; trivial] goal

(**
   [once_simp_tac cntrl asms except ?l goal]:
   - eliminate toplevel universal quantifiers of [l]
   - if (asms=true),
     put conclusions other than [l] into assumptions and make simp rules
   - if (asms=true), make simp rules from assumptions
   - simplify
   - delete temporary assumptions

   If [l] is not given, repeat for each conclusion.
   Ignore formulas for which [except] is true.
 *)
let once_simp_tac cntrl asms except l goal=
  let sqnt = (Tactics.sequent goal)
  in 
  let asm_forms = Tactics.asms_of sqnt
  and concl_forms = Tactics.concls_of sqnt
  in 
  let asm_tags = List.map (fun (x, _) -> x) asm_forms
  and concl_tags = List.map (fun (x, _) -> x) concl_forms
  in 
  let targets = 
    match l with
      None -> concl_tags
    | Some x -> [Logic.label_to_tag x sqnt]
  in
  let data1 =
    Data.set_tactic cntrl cond_prover_tac 
  in
  let set = Data.get_simpset data1
  in 
  let ret=ref (None: Data.t option)
  in 
  let asm_rules = ref ([], [])
  and concl_rules = ref ([], [])
  and asm_entry_tags = ref []
  and concl_entry_tags = ref []
  in 
  let tac1 g= 
   seq
      [
       (fun _ -> asms) 
	 --> 
	   seq 
	     [make_asm_entries_tac asm_rules asm_tags except;
	      make_concl_entries_tac concl_rules concl_tags except];
       data_tac 
	 (fun () -> 
       (* get the information, put it into a useful form *)
	   asm_entry_tags := fst (!asm_rules);
	   concl_entry_tags := fst (!concl_rules);
       (*
	  update the simp set with the rules for the assumptions,
	  the tags of the visited formulas
	  and the tags of the new formulas.
	*)
	   let rules = 
	     Simpset.make_asm_rules (fun _ -> false) (snd(!asm_rules))
	   in 
	   let set1 = Simpset.simpset_add_rules set rules
	   in 
	   let data2=
	     Data.set_asms data1
	       (List.append 
		  (List.map snd (!asm_entry_tags))
		  (Data.get_asms data1))
	   in 
	   let data2a=
	     Data.set_asms data1
	       (List.append 
		  (List.map snd (!concl_entry_tags))
		  (Data.get_asms data2))
	   in 
	   let data3= 
	     Data.set_visited data2a
	       (List.append 
		  (List.map fst (!asm_entry_tags))
		  (List.append  
		     (List.map fst (!concl_entry_tags))
		     (Data.get_asms data2a)))
	   in 
	   let data4= 
	     Data.set_simpset data3 set1
	   in 
	    Lib.set_option ret data4) ()] g
  in 
  let chng = ref false 
  in 
  let tac2 g = 
    let ncntrl = Lib.dest_option ~err:(Failure "once_simp_tac: 1") (!ret) 
    in 
    Tactics.map_every
      (fun tg -> 
	alt
	  [seq
	     [once_simp_engine_tac 
		(ncntrl, ret, except, snd (!concl_rules)) tg;
	      data_tac (fun _ -> chng:=true) ()];
	   skip]) targets g
  in 
  let tac3 g =
    let ncntrl = Lib.dest_option ~err:(Failure "once_simp_tac: 2") (!ret)
    in 
    clean_up_tac ncntrl g
  in 
  let tac4 g =
    if(!chng) 
    then skip g
    else raise No_change
  in 
  seq [tac1; tac2; tac3; tac4] goal



(**
   [full_simp_tac cntrl sset tg gl]

   cntrl: control
   sset: simpset to use
   tg: tag formula to simplifier
   gl: goal

   simplifies formula tg in the first subgoal of goal

   raises
   Not_found if no formula tagged tg in subgoal
   No_change if not change is made
 *)
(*
   let full_simp_tac cntrl simpset tg gl=
   let chng=ref false
   in
   (* get the first sequent *)
   let sqnt = 
   try (Tactics.sequent gl)
   with _ -> raise (Result.error "full_simp_tac: No such formula in goal")
   in 
   let cntrl1=Data.set_simpset cntrl simpset
   in 
   (* prepare the subgoal for simplification *)
   let (prepared_cntrl, prepared_goal) = 
   (try 
   let tmp= simp_prep_tac cntrl1 tg gl
   in (chng:=true; tmp)
   with 
   No_change -> (cntrl1, (skip gl))
   | err -> 
   raise (Result.error "simp_tac: stage 1"))
   in 
   (* invoke the simplifier *)
   let simped_goal = 
   (try 
   Tactics.foreach
   (basic_simp_tac prepared_cntrl tg) prepared_goal
   with No_change -> (chng:=false; skip gl))
   in 
   (* clean up afterwards *)
   let ret_goal=simped_goal
   in 
   if(!chng) 
   then ret_goal
   else raise No_change
 *)
