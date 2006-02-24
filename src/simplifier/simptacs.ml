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

(*** Rules from assumptions and conclusions ***)

(***
* Rules from assumptions and conclusions.
***)


(***
* Rule-forming tactics 
***)

(**
   Make simp rules from identified assumptions.

   [make_asm_entries_tac ret tags except goal]: Copy, prepare the
   assumptions in [tags] for use as simp rules. Ignore the assumptions
   in for which [except] is true.

   Return [ret = new_rules_data ] where [new_rules_data] is the list
   of rules data formed from the assumptions.
*)
let make_asm_entries_tac ret tags except goal=
  let data = ref []
  in 
  (*** Prepare assumptions for use as simp rules ***)
  let utags = List.filter (not <+ except) tags
  in
  let tac1 g = Simpconvs.prepare_asms data utags g
  in 
  (*** Make the list of tagged assumptions to use as simp-rules ***)
  let tac2 g = data_tac (fun _ -> ret:= !data) () g
  in 
  seq [tac1; tac2] goal

(** 
   Make simp rules from identified conclusions.

   [make_concl_entries_tac ret tags except goal]: Copy, lift, prepare
   the conclusions in [tags] for use as simp rules.  Ignore the
   conclusions for which [except] is true.

   Return [ret = (asm_tags, rules)] where [asm_tags] is a list of
   pairs [(c, a)], with [a] is a new assumption formed from [c] and
   [rules] is the list of tagged formulas to be used as simp rules.
*)
let make_concl_entries_tac ret tags except goal=
  let data = ref []
  in 
  (*** Prepare conclusions for use a simp-rules ***)
  let utags = List.filter (not <+ except) tags
  in 
  let tac1 g= Simpconvs.prepare_concls data utags g
  in 
  (*** Make the list of tagged assumptions to use as simp rules. *)
  let tac2 g =
    data_tac (fun _ -> ret := !data) () g
  in 
  seq [tac1; tac2] goal

(***
* Simplifier tactics 
***)

(** Arguments for the simplifier **)
type simp_args=
      {
       use_asms: bool; 
      (** Whether to use the assumptions (and conclusions) as simp rules. *)
       exclude: (Tag.t -> bool)
      (** Test on whether a formula is to be ignored (not simplified
      or used as a simp rule. *)
     }

let mk_args b f = 
  { use_asms = b; exclude = f }

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
   [simp_tac cntrl asms except ?l goal]:
   - eliminate toplevel universal quantifiers of [l]
   - if (asms=true), put conclusions other then [l] into assumptions
     and make simp rules from them.
   - if (asms=true), make simp rules from assumptions.
   - simplify
   - delete temporary assumptions

   If [l] is not given, repeat for each conclusion.
   Ignore formulas for which [except] is true.
 *)
let rec simp_tac cntrl args l goal=
  let use_asms = args.use_asms
  and except = args.exclude
  in 
  let sqnt = Tactics.sequent goal
  in 
  (** Get the sequent formulas **)
  let asm_forms = Tactics.asms_of sqnt
  and concl_forms = Tactics.concls_of sqnt
  in 
  (** Get the sequent formula tags **)
  let asm_tags = List.map (fun (x, _) -> x) asm_forms
  and concl_tags = List.map (fun (x, _) -> x) concl_forms
  in 
  (** Find the targets **)
  let targets = 
    match l with
      None -> concl_tags
    | Some x -> [Logic.label_to_tag x sqnt]
  in
  (** 
     Make the exclusion test (only for used for filtering simp rules).
     Not passed to simp_engine_tac.
   **)
  let exclude tag = 
    Pervasives.(||)
      (List.exists (Tag.equal tag) targets)
      (except tag)
  in 
  (** Data variables **)
  let ret=ref (None: Data.t option)
  in 
  let asm_rules = ref []
  and concl_rules = ref []
  in 
  (** Set up the initial simp data **)
  let set = Data.get_simpset cntrl
  in 
  (** Set the condition prover **)
  let data1 =
    let prover_tac = cond_prover_tac
    in 
    Data.set_tactic cntrl prover_tac  
  in
  (** 
     tac1: if [use_asms] is true, make simp rules from the assumptions
     and conclusions. Exclude those, listed in [targets], which are to
     be simplified.
  **)
  let tac1 g= 
    let set_data () =
      (*** Get assumption rules, put it into a useful form ***)
      let (asm_srcs, asm_new_asms, asm_new_rules) = 
	Simpconvs.unpack_rule_data (!asm_rules)
      in 
      let asm_entry_forms = asm_new_rules
      and asm_entry_tags = List.map drop_formula asm_new_rules
      in 
      (** Make the simp rules from the assumption formulas **)
      let asm_rules = 
	Simpset.make_asm_rules 
	  (fun _ -> false) asm_entry_forms
      in 
      (** Add the assumption simp rule to the simp set **)
      let set1 = Simpset.simpset_add_rules set asm_rules
      in 
      (** Add the tags of the new assumptions to the simp data **)
      let data2=
	Data.set_asms data1
	  (List.append asm_entry_tags  (Data.get_asms data1))
      in 
      (*** Get conclusion rules, put it into a useful form ***)
      let (concl_srcs, concl_new_asms, concl_new_rules) = 
	Simpconvs.unpack_rule_data (!concl_rules)
      in 
      let concl_entry_forms = concl_new_rules
      and concl_entry_tags = List.map drop_formula concl_new_rules
      in 
      (** Make the simp rules from the conclusions formulas **)
      let concl_rules = 
	Simpset.make_asm_rules 
	  (fun _ -> false) concl_entry_forms
      in 
      (** Add the conclusion simp rule to the simp set **)
      let set2 = Simpset.simpset_add_rules set1 concl_rules
      in 
      (** Add the tags of the new assumptions to the simp data **)
      let data3=
	Data.set_asms data2
	  (List.append concl_entry_tags (Data.get_asms data2))
      in 
      (** Add the new simp set to the simp data **)
      let data4 = 
	(** Add the new assumptions as new context **)
	let set3 =
	  List.fold_left Simpset.add_context set2
	    (List.map Formula.term_of asm_new_asms)
	in 
	let set4 =
	  List.fold_left Simpset.add_context set3
	    (List.map Formula.term_of concl_new_asms)
	in 
	Data.set_simpset data3 set4
      in 
      (** 
	 Record the tags of the the assumptions and conclusions
	 which have been visited. ie. From which simp rules
	 have been made.
       *)
      let data5= 
	Data.set_visited data4
	  (List.append 
	     (List.map drop_formula asm_srcs)
	     (List.append 
		(List.map drop_formula concl_srcs)
		(Data.get_asms data4)))
      in 
      Lib.set_option ret data5
    in 
    if not(use_asms) then skip g 
    else 
      seq
	[
         (** Make the simp rules **)
	 seq 
	   [
	    make_asm_entries_tac asm_rules asm_tags exclude;
	    make_concl_entries_tac concl_rules concl_tags exclude
	  ];
	 (** Store the data in a simp set **)
	 data_tac set_data ()
       ] g
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
	     [simp_engine_tac ncntrl ret tg;
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


(***
* Alternative approach
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

(** 
   [simpC1_tac ret cntrl goal]: Simplify conclusions.

   Simplify each conclusion, starting with the last, adding it to the
   assumptions after it is simplified.

   Doesn't clean-up.
*)
let simpC1_tac cntrl ret ?c goal = 
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
    simpC1_tac ncntrl data ?c g
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
   [simpA1_tac ret cntrl goal]: Simplify assumptions

   Simplify each assumptions, starting with the last, adding it to the
   simpset rules after it is simplified.

   Doesn't clean-up.
*)
let simpA1_tac cntrl ret ?a goal = 
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
  let tac1 ret g= simpA1_tac cntrl ret g
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
