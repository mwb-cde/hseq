(*-----
   Name: simptacs.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

let log str x = ()

(**
   Tactical interface to the simplifier engine. 
 *)

open Tactics
open Simplifier
open Lib.Ops

(***
* Simplification data
***)

(** [default_data]: The default data set. *)
let default_data =
  let d1 = Simplifier.Data.default
  in 
  Data.set_tactic d1 Simplifier.cond_prover_tac

(** 
   [add_rule_data data rules]: Update [data] with assumption
   [rules]. [rules] should be as provided by {!Simpconvs.prepare_asm}.
 *)
let add_rule_data data rules =
  (*** Put rules into a useful form ***)
  let asm_srcs, asm_new_asms, asm_new_rules = 
    Simpconvs.unpack_rule_data rules
  in 
  (** Add the simp rules to the simpset *)
  let data1 = 
    let simp_rules = Simpset.make_asm_rules (fun _ -> false) asm_new_rules
    in 
    (** Add new simp rules to the simpset *)
    let set1 = 
      Simpset.simpset_add_rules (Data.get_simpset data) simp_rules
    in 
    (** Add the new context *)
    let set2 = 
      List.fold_left Simpset.add_context set1
	(List.map Formula.term_of asm_new_asms)
    in 
    Data.set_simpset data set2   
  in
  (** Record the new assumptions *)
  let data2 = 
    let asm_entry_tags = List.map drop_formula asm_new_rules
    in 
    Data.set_asms data1 
      (List.rev_append asm_entry_tags (Data.get_asms data1))
  in 
  data2

(*** Adding assumptions and conclusions ***)

let add_asms_tac data atags goal =
  let add_tac rdata rl g = 
    let d = Lib.dest_option (!rdata)
    in 
    data_tac (fun _ -> Lib.set_option rdata (add_rule_data d (!rl))) () g
  in 
  let tac tg g = 
    let rl = ref [] 
    in 
    seq 
      [ 
	Simpconvs.prepare_asm rl tg;
	(fun g1 -> add_tac data rl g1)
      ] g
  in
  map_some tac atags goal

let add_concls_tac data ctags goal =
  let add_tac rdata rl g = 
    let d = Lib.dest_option (!rdata)
    in 
    data_tac (fun _ -> Lib.set_option rdata (add_rule_data d (!rl))) () g
  in 
  let tac tg g = 
    let rl = ref [] 
    in 
    seq 
      [ 
	Simpconvs.prepare_concl rl tg;
	(fun g1 -> add_tac data rl g1);
	(fun g1 -> 
	  data_tac (log "add_concls_tac 1") 
	    (Lib.dest_option (!data)) g1)
      ] g
  in
  map_some tac ctags goal


(***
* Simplification engines
***)

(**
   [initial_prep_tac ctrl ret lbl goal]:
   Prepare formula [lbl] for simplification. 
   This just tries to apply [allC] or [existA].
 *)
let initial_prep_tac ctrl ret lbl goal = 
  let is_asm =
    try (ignore(get_tagged_asm lbl goal); true)
    with _ -> false
  in 
  if(is_asm)
  then specA goal
  else specC goal

(**
   [simp_engine_tac cntrl ret l goal]:
   The engine for [simp_tac].

   - eliminate toplevel universal quantifiers of [l]
   - simplify [l]
   - solve trivial goals
   - repeat until nothing works
 *)
let simp_engine_tac cntrl ret tag goal =
  (** tac2: Simplify **)
  let tac2 g =
    let ncntrl = Lib.get_option (!ret) cntrl 
    in 
    alt
      [
       seq_some
	 [
	  (** Prepare the goal for simplification. **)
	  initial_prep_tac cntrl ret (ftag tag);
	  (** Clear the return data. **)
	  seq
	    [
	     data_tac (fun () -> ret:=None) ();
	     alt
	       [
		(** Try simplification. **)
		basic_simp_tac ncntrl ret tag;
		(** On fail, set the return value. **)
		seq 
		  [ 
		    data_tac (Lib.set_option ret) ncntrl; 
		    fail ~err:No_change 
		  ]
	      ]
	   ];
	  (** Fail if nothing worked *)
	  fail ~err:No_change 
	]
     ] g
  in 
  (** trivia_tac: Clean up trivial goals. **)
  let trivia_tac g = 
    alt [Boollib.trivial ~f:(ftag tag); skip] g
  in 
  ret:=None; 
  try
    seq [repeat tac2 ; trivia_tac] goal
  with _ -> raise No_change


(**
   [simpA_engine_tac cntrl ret chng l goal]: Simplify assumption [l],
   returning the updated data in [ret]. Set [chng] to true on success.

   Doesn't clean-up.
 *) 
let simpA_engine_tac cntrl ret chng l goal = 
  let (atag, _) = 
    try get_tagged_asm l goal
    with Not_found -> raise No_change
  in 
  let loopdb = Data.get_loopdb cntrl
  in 
  let ngoal = simp_engine_tac cntrl ret atag goal
  in 
  chng:=true;
  Lib.set_option ret 
    (Data.set_loopdb (Lib.dest_option (!ret)) loopdb); 
  ngoal

(**
   [simpC_engine_tac cntrl ret chng l goal]: Simplify conclusion [l],
   returning the updated data in [ret]. Set [chng] to true on success.

   Doesn't clean-up.
 *) 
let simpC_engine_tac cntrl ret chng l goal = 
  let (ctag, _) = 
    try get_tagged_concl l goal
    with Not_found -> raise No_change
  in 
  let loopdb = Data.get_loopdb cntrl
  in 
  let ngoal = 
    simp_engine_tac cntrl ret ctag goal
  in 
  chng:=true;
  Lib.set_option ret 
    (Data.set_loopdb (Lib.dest_option (!ret)) loopdb); 
  ngoal

(***
* Simplifying assumptions
***)

(** 
   [simpA0_tac ret cntrl goal]: Simplify assumptions

   Simplify each assumptions, starting with the last, adding it to the
   simpset rules after it is simplified.

   Doesn't clean-up.
 *)
let simpA0_tac cntrl ret ?a goal = 
  let sqnt = sequent goal
  in 
  let excluded_tags = Data.get_exclude cntrl
  in 
  let except_tag x = List.exists (Tag.equal x) excluded_tags
  in 
  let concls = 
    List.filter (not <+ except_tag) (List.map drop_formula (concls_of sqnt))
  in 
  let (targets, asms) = 
    let except_or y x = ((Tag.equal y x) || except_tag x)
    in 
    let asm_tags = List.map drop_formula (asms_of sqnt)
    in 
    match a with
      None -> (List.filter (not <+ except_tag) asm_tags, [])
    | Some(x) -> 
	let atag = Logic.label_to_tag x sqnt
	in 
	([atag], List.filter (not <+ (except_or atag)) asm_tags)
  in 
  let asms_tac reg g = add_asms_tac ret asms g
  in 
  let concls_tac ret g = add_concls_tac ret concls g
  in 
  let target_tac chng ret tg g =
    let cntrl = Lib.dest_option (!ret)
    in 
    seq
      [
       (** Simplify the target **)
       alt
	 [
	  simpA_engine_tac cntrl ret chng (ftag tg);
	  skip
	];
       (** Add the assumption to the simpset *)
       (fun g1 -> add_asms_tac ret [tg] g1)
     ] g
  in 
  let main_tac chng ret g = 
    seq_some
      [
       (** Add non-target assumptions to the simpset *)
       asms_tac ret; 
       (** Add conclusions to the simpset *)
       concls_tac ret;
       (** Simplify the targets *)
       (fun g1 -> map_some (target_tac chng ret) (List.rev targets) g1)
     ] g
  in
  let chng = ref false
  in 
  try 
    let ngoal = main_tac chng ret goal
    in 
    if (!chng) 
    then ngoal
    else raise No_change
  with _ -> raise No_change

(** 
   [simpA_tac ret cntrl ?a goal]: Simplify conclusion.

   If [l] is given, just simplify conclusion [l]. Otherwise, simplify
   each conclusion, starting with the last, adding it to the
   assumptions after it is simplified.

   Doesn't clean-up.
 *)
let simpA_tac cntrl ?a goal =
  let tac1 ret g= simpA0_tac cntrl ret g
  in 
  let tac2 data g = 
    let ncntrl = Lib.dest_option ~err:(Failure "simpA_tac") (!data)
    in 
    clean_up_tac ncntrl g
  in 
  let main_tac ret g = 
    seq
      [
       tac1 ret; 
       (fun g1 -> tac2 ret g1)
     ] g
  in 
  let ret = ref (Some (cntrl))
  in 
  try (main_tac ret goal)
  with _ -> raise No_change

(***
* Simplifying conclusions
***)

(** 
   [simpC0_tac ret cntrl goal]: Simplify conclusions.

   Simplify each conclusion, starting with the last, adding it to the
   assumptions after it is simplified.

   Doesn't clean-up.
 *)
let simpC0_tac cntrl ret ?c goal = 
  let sqnt = sequent goal
  in 
  let excluded_tags = Data.get_exclude cntrl
  in 
  let except_tag x = List.exists (Tag.equal x) excluded_tags
  in 
  let asms = 
    List.filter (not <+ except_tag) (List.map drop_formula (asms_of sqnt))
  in 
  let (targets, concls) = 
    let except_or y x = ((Tag.equal y x) || except_tag x)
    in 
    let concl_tags = List.map drop_formula (concls_of sqnt)
    in 
    match c with 
      None -> (List.filter (not <+ except_tag) concl_tags, [])
    | Some(x) -> 
	let ctag = Logic.label_to_tag x sqnt
	in 
	([ctag], List.filter (not <+ (except_or ctag)) concl_tags)
  in 
  let asms_tac ret g = add_asms_tac ret asms g
  in 
  let concls_tac ret g = add_concls_tac ret concls g
  in 
  let target_tac chng ret ct g = 
    let cntrl = Lib.dest_option (!ret)
    in 
    seq
      [
       (** Simplify the target *)
       alt
	 [
	  simpC_engine_tac cntrl ret chng (ftag ct);
	  skip
	];
       (** Add it to the assumptions *)
       (fun g1 -> add_concls_tac ret [ct] g1);
     ] g
  in 
  let main_tac chng ret g = 
    seq_some
      [
       (** Add assumptions to the simpset *)
       asms_tac ret;
       (** Add non-target conclusions to the simpset *)
       concls_tac ret;
       (** Simplify the targets (in reverse order) *)
       (fun g1 -> map_some (target_tac chng ret) (List.rev targets) g1);
     ] g
  in
  let chng = ref false
  in 
  try 
    let ngoal = main_tac chng ret goal
    in
    if (!chng) 
    then ngoal
    else raise No_change
  with _ -> raise No_change

(** 
   [simpC_tac ret cntrl ?l goal]: Simplify conclusion.

   If [l] is given, just simplify conclusion [l]. Otherwise, simplify
   each conclusion, starting with the last, adding it to the
   assumptions after it is simplified.
 *)
let simpC_tac cntrl ?c goal =
  let tac1 data g=
    let ncntrl = Lib.dest_option (!data)
    in 
    simpC0_tac ncntrl data ?c g
  in 
  let tac2 data g = 
    clean_up_tac data g
  in 
  let main_tac g = 
    let ret = ref (Some (cntrl))
    in 
    seq 
      [
       tac1 ret; 
       (fun g1 -> data_tac (log "simpC_tac: 2") 
	   (Lib.dest_option (!ret)) g1);
       (fun g1 -> 
	 let data = Lib.dest_option (!ret)
	 in
	 tac2 data g1)
     ] g
  in 
  try (main_tac goal)
  with _ -> raise No_change



(***
* Simplifying subgoals
***)

(** 
   [full_simp0_tac ret cntrl goal]: Simplify subgoal

   {ul
   {- Simplify each assumption, starting with the last, adding it to the
   simpset rules after it is simplified.}
   {- Simplify each conclusion, starting with the last, adding it to the
   simpset rules after it is simplified.}}

   Doesn't clean-up.
 *)
let full_simp0_tac cntrl ret goal = 
  let excluded = Data.get_exclude cntrl
  in 
  let except x = List.exists (Tag.equal x) excluded
  and sqnt = sequent goal
  in 
  let asms = 
    List.filter (not <+ except) (List.map drop_formula (asms_of sqnt))
  and concls = 
    List.filter (not <+ except) (List.map drop_formula (concls_of sqnt))
  in 
  let asm_tac chng ret tg g =
    let cntrl = Lib.dest_option (!ret)
    in 
    seq
      [
       (** Simplify the assumption **)
       alt
	 [
	  simpA_engine_tac cntrl ret chng (ftag tg);
	  skip
	];
       (** Add the assumption to the simpset *)
       (fun g1 -> add_asms_tac ret [tg] g1)
     ] g
  in 
  let concl_tac chng ret tg g = 
    let cntrl = Lib.dest_option (!ret)
    in 
    seq
      [
       (** Simplify the conclusion **)
       alt
	 [ 
	   simpC_engine_tac cntrl ret chng (ftag tg);
	   skip
	 ];
       (** Add the conclusion to the simpset *)
       (fun g1 -> add_concls_tac ret [tg] g1)
     ] g
  in 
  let main_tac chng ret g = 
    seq_some
      [
       (** Simplify the assumptions (in reverse order) *)
       map_some (asm_tac chng ret) (List.rev asms);
       (** Simplify the conclusions (in reverse order) *)
       (fun g1 -> 
	 map_some (concl_tac chng ret) (List.rev concls) g1)
     ] g
  in
  let chng = ref false
  in 
  try 
    let ngoal = main_tac chng ret goal
    in 
    if (!chng)
    then ngoal
    else raise No_change
  with _ -> raise No_change

(** 
   [full_simp0_tac ret cntrl goal]: Simplify subgoal

   {ul
   {- Simplify each assumption, starting with the last, adding it to the
   simpset rules after it is simplified.}
   {- Simplify each conclusion, starting with the last, adding it to the
   simpset rules after it is simplified.}}
 *)
let full_simp_tac cntrl goal =
  let ret = ref (Some (cntrl))
  in 
  let tac1 g= full_simp0_tac cntrl ret g
  in 
  let tac2 g = 
    let ncntrl = Lib.dest_option ~err:(Failure "full_simp_tac") (!ret)
    in 
    clean_up_tac ncntrl g
  in 
  let main_tac g = 
    seq_some [tac1; tac2] g
  in 
  try (main_tac goal)
  with _ -> raise No_change
