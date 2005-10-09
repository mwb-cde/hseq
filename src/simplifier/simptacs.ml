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
  let tac1 g = Simpconvs.prepare_asms data tags except g
  in 
  (*** Make the list of tagged assumptions to use as simp-rules ***)
  let tac2 g =
    data_tac (fun _ -> ret:= !data) () g
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
  let tac1 g= Simpconvs.prepare_concls data tags except g
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
  { use_asms = b; exclude = f ; }

(**
   [simp_engine_tac cntrl asms l goal]:
   The engine for [simp_tac].

   - eliminate toplevel universal quantifiers of [l]
   - if (asms=true),
     put conclusions other than [l] into assumptions and make simp rules
   - if (asms=true), make simp rules from assumptions
   - simplify
   - delete temporary assumptions
*)
let simp_engine_tac cntrl (ret, except) tag goal=
  (** Get the simp set. **)
  let set= Data.get_simpset cntrl
  in 
  (** Set the rewriting control. **)
  let cntrl1=Data.set_simpset cntrl set
  in 
  (** tac1: Prepare the goal for simplification. **)
  let tac1 g = simp_prep_tac cntrl1 ret (ftag tag) g
  in 
  (** tac2: Simplify **)
  let tac2 g =
    let ncntrl = Lib.get_option (!ret) cntrl1
    in 
    seq
      [
       (** Clear the return data. **)
       data_tac (fun () -> ret:=None) ();
       alt
	 [
	  (** Try simplification. **)
	  basic_simp_tac ncntrl ret tag;
	  (** On fail, set the return value. **)
	  seq [ data_tac (Lib.set_option ret) ncntrl; fail ~err:No_change ]
	]
     ] g
  in 
  (** trivia_tac: Clean up trivial goals. **)
  let trivia_tac g = 
    alt [Boollib.trivial ~f:(ftag tag); skip] g
  in 
  ret:=None; 
  seq 
    [repeat (tac1 ++ tac2) ; trivia_tac] goal

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
	  (List.append 
	     asm_entry_tags
	     (Data.get_asms data1))
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
	  (List.append 
	     concl_entry_tags
	     (Data.get_asms data2))
      in 
      (** Add the new simp set to the simp data **)
      let data4 = 
	Data.set_simpset data3 set2
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
	     [simp_engine_tac ncntrl (ret, except) tg;
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

