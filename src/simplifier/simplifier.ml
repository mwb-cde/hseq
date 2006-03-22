(*-----
   Name: simplifier.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(**
   The simplifier engine.
 *)

(**
   Simplifier actions:

   Simple actions (no conditional rewrites):
   Recurse through the structure of a term, 
   getting a list of the rewrite rules to be applied

   Always:
   recurse through the structure of a term, 
   getting a list of the rewrite rules to be applied

   For each subterm:
   Get a (un)conditional rule which could be applied.
   If conditional, try to prove the condition
   discarding the rule on failure.
   If successfull apply the rule.
 *)

open Basic
open Term
open Logicterm
open Tactics
open Rewritekit
open Rewrite
  
open Simputils
open Simpset

(***
 * Error handling
 ***)

class simpError s ts =
  object (self)
    inherit Result.error s
    val trms = (ts :term list)
    method get() = trms
    method print st = 
      Format.open_box 0; 
      print_string "Simplifier Error: ";
      print_string ((self#msg())^" "); 
      Format.print_newline();
      Format.open_box 0; 
      Printer.print_sep_list ((Term.print st), ",")
	(self#get());
      Format.close_box();
      Format.close_box();
  end

let error s t = Result.mk_error((new simpError s t):>Result.error)
let add_error s t e =
  Result.add_error e (error s t) 

exception No_change

(***
 * Simplifier data.
 ***)

type control = Rewrite.control

module Data =
  struct

    type loopDB = Basic.term Net.net
	  (** Structure used to store terms for looping rewriting detection *)

(**
   [type Data.t]
   Information used by and built up during simplification.
 *)
    type t =
	{
(**
   [simpset]: the simpset being used. Assumptions may be added to this
   during the course of simplification
 *)
	 simpset:Simpset.simpset;

(** [cond_tac]: the tactic used to prove conditions of rewrite rules *)
	 cond_tac: t -> Tag.t -> Tactics.tactic;

(** [control]: rewrite control ([direction] is ignored *)
	   control: Rewrite.control;

(** conds: max. no. of conditions to try and prove at once *)
	   conds: int option;  

(** rr_depth: max. no. of rr rules to apply at one level *)
	   rr_depth: int option;

(** asms: assumptions generated during the course of simplification *)
	   asms: Tag.t list;

(** visited: formulas visited during the course of simplification *)
	   visited: Tag.t list;

(****
(** asm_pairs: 
   tags of original formulas and the new modified formula
   (in (a, b) a is the tag of the original assumption,
   b is the tag of the formula used as a rewrite rule
 *)
   asm_pairs: (Tag.t*Tag.t) list;
(** concl_pairs: 
   tags of original formulas and the new modified formula
   (in (a, b) a is the tag of the original conclusion
   b is the tag of the formula used as a rewrite rule
 *)
   concl_pairs: (Tag.t*Tag.t) list;
 ****)

(** exclude: formulas not to use as a rewrite rule *)
	   exclude: Tag.t list;

(** loopdb: Terms already rewritten. *)
	   loopdb: loopDB
       }

    let make (sset, tac, cntrl, cd, rd, a, vs, ex, rs) = 
      {
       simpset=sset;
       cond_tac=tac;
       conds=cd;
       control=cntrl;
       rr_depth=rd;
       asms=a;
       visited = vs;
       exclude=ex;
(*
   asm_pairs = aps; concl_pairs = cps;
 *)
       loopdb=Net.empty()
     }

    let set_simpset cntrl set=
      {cntrl with simpset = set}

    let set_tactic cntrl tac=
      {cntrl with cond_tac = tac}

    let set_conds cntrl d=
      {cntrl with conds=d}

    let set_conds_val cntrl d=
      {cntrl with conds = (Lib.set_int_option d) }

    let set_control cntrl c=
      {cntrl with control=c}

    let set_rr_depth cntrl d=
      {cntrl with rr_depth=d}

    let set_rr_depth_val cntrl d=
      {cntrl with rr_depth= (Lib.set_int_option d) }

    let set_asms cntrl ds=
      {cntrl with asms=ds}

    let set_visited cntrl ds=
      {cntrl with visited=ds}

(*
   let set_asm_pairs cntrl ds=
   {cntrl with asm_pairs=ds}
   let set_concl_pairs cntrl ds=
   {cntrl with concl_pairs=ds}
 *)

    let set_exclude cntrl ds=
      {cntrl with exclude=ds}

    let set_loopdb cntrl ds=
      {cntrl with loopdb=ds}

    let get_loopdb cntrl= cntrl.loopdb

    let add_loopdb cntrl t =
      let varp x = false
      in 
      set_loopdb cntrl (Net.add varp (get_loopdb cntrl) t t)

    let mem_loopdb scp cntrl t = 
      let opts = 
	try (Net.lookup (get_loopdb cntrl) t)
	with Not_found -> []
      in
      List.exists (Logicterm.alpha_equals scp t) opts

    let get_simpset cntrl=cntrl.simpset
    let get_tactic cntrl=cntrl.cond_tac
    let get_control cntrl=cntrl.control

    let add_asm cntrl a=
      set_asms cntrl (a::(cntrl.asms))

    let get_asms cntrl =cntrl.asms

    let get_visited cntrl =cntrl.visited

(*
   let get_asm_pairs cntrl =cntrl.asm_pairs
   let get_concl_pairs cntrl =cntrl.concl_pairs
 *)

    let get_exclude cntrl =cntrl.exclude

    let dec_cond_depth cntrl=
      set_conds cntrl (Lib.dec_int_option (cntrl.conds))

    let get_cond_depth cntrl= cntrl.conds

    let dec_rr_depth cntrl=
      set_conds cntrl (Lib.dec_int_option (cntrl.rr_depth))

    let get_rr_depth cntrl= cntrl.rr_depth

    let add_simp_rule cntrl rule=
      set_simpset cntrl
	(Simpset.add_rule rule (get_simpset cntrl))

    let default_rr_depth = ref (Some 100)
    let default_cond_depth = ref (Some 100)

(** [default]: The default control information  *)
    let default = 
      make 
	(Simpset.empty_set(),
	 (fun _ _ -> skip),
	 Formula.default_rr_control,
	 (!default_cond_depth), (!default_rr_depth), 
	 [], [], [], [])

  end

(***
 * Utility functions 
 ***)

(**
   [strip_rrs]: prepare for direct rewriting of term. For tests only
 *)
let strip_rrs rrs=
  let strip_rr x = 
    match x with
      Logic.RRThm x -> (Logic.term_of x)
    |	 _ -> failwith "simp_tac"
  in 
  List.map strip_rr rrs

(**
   [is_conditional rl]: True if simp rule [rl] is conditional.
 *)
let is_conditional rl = 
  match Simpset.rule_cond rl with
    None -> false | _ -> true

(** [is_none x]: true if [x=None], false otherwise. *)
let is_none x = match x with None -> true | _ -> false

(**
   [is_excluded excluded sqnt rl]: True if rewrite rule [rl] is an assumption
   in the excluded list.
 *)
let is_excluded  excluded sqnt x = 
  match x with
    Logic.Asm l -> 
      let tag = Logic.label_to_tag l sqnt
      in 
      List.exists (Tag.equal tag) excluded
  | Logic.OAsm(l, _) ->
      let tag = Logic.label_to_tag l sqnt
      in 
      List.exists (Tag.equal tag) excluded
  | _ -> false

(**
   [get_form t n]:  Get formula tagged [t] from node [n].
   First try conclusions, then try assumptions.
   raise [Not_found] if not found.
 *)
let get_form t sqnt = 
  try Logic.Sequent.get_tagged_cncl t sqnt
  with Not_found -> Logic.Sequent.get_tagged_asm t sqnt

(**
   [simp_fail]: A hook to allow failures to be traced.
 *)
let simp_fail ?err g = Tactics.fail ?err g

(** 
   [check_change p]: Test whether plan [p] does anything. Raise
   [No_change] if it does not.
*)
let check_change x = 
  match x with
    Rewritekit.Skip -> raise No_change
  | _ -> ()

(** 
   [check_change2 p1 p2]: Test either plan [p1] or plan [p2] does
   anything. Raise [No_change] if both do nothing.
*)
let check_change2 x y = 
  match (x, y) with
    (Rewritekit.Skip, Rewritekit.Skip) -> raise No_change
  | _ -> ()

(**
   [check_add_loop scp cntrl t]: Test whether term [t] is in the
   loopdb. If it isn't, add it to the loopdb.
 *)
let check_add_loop scp cntrl t =
  if (Data.mem_loopdb scp cntrl t)
  then raise (Failure "check_add_loop")
  else Data.add_loopdb cntrl t


let null_term = Term.mk_free "" (Gtypes.mk_null())

(**
   [get_form t n]: Get formula tagged [t] from node [n].  First try
   conclusions, then try assumptions.  return the formula and a flag
   which is [true] if the formula was in the conclusions and false if
   the formula was in the assumptions.  raise [Not_found] if not
   found.
 *)
let get_form t sqnt = 
  match Lib.try_find (Logic.Sequent.get_tagged_cncl t) sqnt
  with
    None -> 
      (Logic.Sequent.get_tagged_asm t sqnt, false)
  | Some(x) -> (x, true)


(***
 * Utility tactics
 ***)

let cleanup = ref true

(**
   [clean_up_tac ctrl g]:
   Clean up after simplification.
   Delete all assumptions listed in [ctrl.asms].
 *)
let clean_aux_tac tags g =
  map_every (fun x -> deleteA (Logic.FTag x)) tags g

let clean_up_tac ctrl g=
  if (!cleanup) 
  then clean_aux_tac (Data.get_asms ctrl) g
  else (skip g)


(**
   [copyA_inst_tac info vals x]: Copy assumption [x], instantiate the copy
   with [vals]. info: aformulas = [x1], where [x1] is the tag of the
   new assumption.  Fails if there are more terms in [vals] then
   variables in [x].
 *)
let copyA_inst_tac ?info vals x goal =
  let inf1 = mk_info()
  in 
  seq
    [
     copyA ~info:inf1 x;
     (fun g ->
       let x1 = get_one ~msg:"copyA_inst_tac" (aformulas inf1);
       in 
       instA ?info:info ~a:(ftag x1) vals g)
   ] goal
    
    
(** 
   [cut_rr_rule info vals t g]
   Cut rule [t] into goal [g], instantiating with vals.
   If [t] is a theorem, it is cut into the goal.
   If [t] is an assumption, it is copied.

   info: aforms = [[x]] where [x] is the tag of the new assumption.
 *)
let cut_rr_rule ?info vals t g =
  match t with
    Logic.RRThm(th) ->
      cut ?info:info ~inst:vals th g
  | Logic.ORRThm(th, _) ->
      cut ?info:info ~inst:vals th g
  | Logic.Asm(x) ->
      copyA_inst_tac ?info:info vals x g
  | Logic.OAsm(x, _) ->
      copyA_inst_tac ?info:info vals x g


(** 
   [simp_rewrite_tac ?info is_concl plan term lbl]: Local interface to
   the main rewriting tactics. If [is_concl] is true, call
   [Tactics.pure_rewriteC ?info plan ~term:trm lbl goal] otherwise
   call [Tactics.pure_rewriteA ?info plan ~term:trm lbl goal].

*)
let simp_rewrite_tac ?info is_concl plan trm lbl goal =
  if is_concl 
  then 
    pure_rewriteC ?info plan ~term:trm lbl goal
  else
    pure_rewriteA ?info plan ~term:trm lbl goal

(***
* Conditional rule tactics
***)

(** [prep_cond_tac cntrl values thm g]

   Cut [thm] into the sequent, instantiate with [values].  Apply
   [implA] to get two subgoals, tagged [(cgltg, rgltg)] with condition
   in [cgltg] tagged [cftg] and rewrite-rule in [rgltg] tagged [rrftg].
   Add [rrftg] to [cntrl], getting [ncntrl].

   return g3
   ret=(ncntrl, [cgltg; rgltg], [cftg; rrftg])
 *)
let prep_cond_tac cntrl ret values thm goal =
  let info = Tactics.mk_info()
  in
  let add_data rl_ftg inf= 
    let (cnd_gltg, rl_gltg) =  (* condition-goal, rule-goal *)
      get_two ~msg:"prep_cond_tac: goals" (subgoals inf)
    in 
    let cnd_ftg= (* condition- formula-tag *)
      get_one ~msg:"prep_cond_tac: forms" (cformulas inf)
    in 
    let ncntrl= Data.add_asm cntrl rl_ftg
    in 
    Lib.set_option ret (ncntrl, (cnd_gltg, rl_gltg), (cnd_ftg, rl_ftg))
  in 
  let tac g =
    seq
      [
       cut_rr_rule ~info:info values thm;
       (fun g1 -> 
	 let rl_ftg=Lib.get_one (aformulas info) No_change
	 in 
	 seq
	   [
	    Logic.Tactics.implA ~info:info (ftag rl_ftg);
	    data_tac (add_data rl_ftg) info
	  ] g1)
     ] g
  in 
  try tac goal
  with _ -> raise No_change


(** 
   [prove_cond_tac cntrl tac values entry g]: Prepare a
   conditional simp rule [entry] for use in rewriting.
   
   Use [prep_cond_tac] add the rule to the goal to create a subgoal
   for the condition. Use tactic [cntrl.cond_tac] to prove the
   condition, failing if it fails to prove the condition.

   Return [ret=(ncntrl, rl)] where [ncntrl] is the new simp data and
   [rl] the rewrite rule built from the new theorem/assumption.
 *)
let prove_cond_tac cntrl ret values entry goal = 
  (* let qs = Simpset.rule_binders entry *)
  let thm = Simpset.rule_src entry
  and ret1 = ref None
  in 
  let orig_loopdb = Data.get_loopdb cntrl
  in 
  let tac g =
    seq
      [
       (** Add rule to the goal assumptions. *)
       prep_cond_tac cntrl ret1 values thm;
       (** Prove the condition. **)
       (fun g1 -> 
	 let (ncntrl, (cnd_gltg, rl_gltg), (cnd_ftg, rl_ftg))
	     = Lib.dest_option ~err:(Failure "prove_cond_tac: 1") (!ret1)
	 in
	 let prover_tac = Data.get_tactic ncntrl
	 in 
	 seq
	   [
	    ((fun n -> Tag.equal cnd_gltg (node_tag n))
	       --> (** Restrict to the condition sub-goal. **)
		 notify_tac 
		   (fun x -> ret := (Some x)) 
		   (ncntrl, Logic.Asm(ftag rl_ftg))
		   (** Apply the prover **)
		   (prover_tac ncntrl cnd_ftg));
	    (** Add the data to ret. *)
	    (fun g2 ->
	      let form= drop_tag(get_tagged_asm (ftag rl_ftg) g2)
	      in 
	      let rcntrl = Data.set_loopdb ncntrl orig_loopdb
	      in 
	      let rule = 
		Simpset.make_rule 
		  (Logic.Asm (ftag rl_ftg)) (Formula.term_of form)
	      in 
	      data_tac (fun x -> ret := Some x)
		(Data.add_simp_rule rcntrl rule, 
		 Logic.Asm(ftag rl_ftg)) g2)
	  ] g1)
     ] g
  in
  (** Ensure tac left only one subgoal (so condition is solved) *)
  let test_result br = 
    match (branch_subgoals br) with
      [ x ] -> true | _ -> false
  in 
  restrict test_result tac goal


(***
* Simplifier functions 
***)

let log str x = ignore(x)

type data = 
    (Data.t  (** Scope *)
       * Gtypes.substitution (** Type environment *)
       * Term.substitution)   (** Quantifier environment *)

(**
   [match_rewrite scp tyenv qntenv trmenv rule trm]: Try to match lhs
   of [rule] with [trm] in type envivornment [tyenv] and term bindings
   [trmenv]. Return rhs of [rule], instantiated with the binding from
   the match, and the type and term environments that made the match
   successful. Raise [Failure] on failure.
 *)
let match_rewrite scp tyenv qntenv rl trm = 
  let (qs, _, lhs, rhs, order, src) = rl
  in 
  let varp = Rewrite.is_free_binder qs
  in 
  let find_match term1 term2 = 
(*
    Unify.unify_rewrite 
      scp tyenv (Term.empty_subst()) varp term1 term2
*)
    Unify.matches_rewrite 
      scp tyenv (Term.empty_subst()) varp term1 term2
  in 
  try
    (let ntyenv, nenv=find_match lhs trm
    in 
    let nt = Term.subst_closed qntenv nenv rhs
    in 
    match order with
      None -> (src, ntyenv, nenv, nt)
    | Some(p) ->
	if (p nt trm)
	then (src, ntyenv, nenv, nt)
	else raise (Failure "match_rewrite"))
  with x -> (failwith "match_rewrite")


(** 
   [find_basic ret data rl trm g]: Try to match simp rule [rl] with
   term [trm] in goal [g], with [data=(cntrl, tyenv, qntenv)]. If [rl]
   matches but is conditional, try to prove the condition using tactic
   [cntrl.cond_tac], adding the rule to the goal assumptions.

   Returns [ret=(ndata, ntrm, rr)] where [ndata=(ncntrl, ntyenv,
   qntenv)], [ncntrl] is the updated simplifier data, [nytenv] is the
   type environment made by the matching, [ntrm] is the result of
   rewriting [trm] and [rl] the rewrite rule to add to the list being
   compiled.
 *)
let find_basic ret data rl trm goal=
  let (cntrl, tyenv, qntenv) = data
(*
  let (cntrl, _, qntenv) = data
  and tyenv = typenv_of goal 
*)
  in 
  let (qs, c, lhs, rhs, order, thm)= rl
  and scp=scope_of goal
  in 
(** 
   Test whether the rule is a match, throws an exception on failure.
 **)
  let (src, ntyenv, ntenv, nt)=
    match_rewrite scp tyenv qntenv rl trm
  in 
(** Test for a looping rewrite *)
  let cntrl1 = 
    try check_add_loop scp cntrl nt
    with _ -> raise No_change
  in 
  let ret1=ref None
  in 
  let tac g =
    cond
      (fun _ -> is_conditional rl)
      (seq
	 [ 
	   (fun g1 ->
	     let values=extract_consts qs ntenv
	     in 
	     prove_cond_tac cntrl1 ret1 values rl g1);
	   (fun g1 -> 
	     let (ncntrl, rr) = 
	       Lib.dest_option 
		 ~err:(Failure "find_basic: 1") (!ret1)
	     in 
	     data_tac (Lib.set_option ret) (ncntrl, ntyenv, nt, rr) g1)
	 ])
      (fun g1 -> 
	let (ncntrl, rr) = (cntrl, thm)
	in 
	data_tac (Lib.set_option ret) (ncntrl, ntyenv, nt, rr) g1) g
  in 
  try (tac goal) 
  with _ -> raise No_change

(**
   [find_match_tac ret data trm g]: Find a rule in simpset [set] which
   matches term [trm] in goal [g], with [data=(cntrl, tyenv,
   qntenv)]. If found, rewrite [trm] with the rule.

   Returns [ret=(ndata, ntrm, rr)] where [ndata=(ncntrl, ntyenv,
   qntenv)], [ncntrl] is the updated data, [nytenv] is the type
   environment made by the matching, [ntrm] is the result of rewriting
   [trm] with [rl] and [rl] the rewrite rule to add to the list being
   compiled.

   Raise [No_change] and set [ret:=None] if no matches.
 *)
let find_match_tac ret data trm (goal: Logic.node)=
  let (cntrl, tyenv, qntenv) = data 
  in 
  let scp = scope_of goal
  and sqnt = sequent goal 
  and excluded = Data.get_exclude cntrl
  in 
  let rec find_aux rls t g= 
    match rls with
      [] -> ret:=None; raise No_change 
    | (rl::nxt) ->
	let src = Simpset.rule_src rl
	in 
	if(is_excluded excluded sqnt src)
	then find_aux nxt t g
	else 
	  (try find_basic ret data rl t g
	  with _ -> find_aux nxt t g)
  in 
  let lst = 
    try (lookup scp (Data.get_simpset cntrl) trm)
    with _ -> raise No_change
  in 
  find_aux lst trm goal


(** 
   [find_all_matches ret (cntrl, tyenv, qntenv) trm g]: Find all rules
   in simpset [cntrl.set] which can be used to rewrite term [trm] in
   goal [g].

   Returns new simp data, the new type environment and the rewritten
   term. The new simp data is built by adding the rules used to
   rewrite the term, in the order they are applied.
*)
let rec find_all_matches_tac ret data trm goal =
  let (cntrl0, tyenv, qntenv) = data
  in 
  (** Get the original loopdb *)
  let orig_loopdb = Data.get_loopdb cntrl0
  in 
  let cntrl = 
    Data.add_loopdb (Data.set_loopdb cntrl0 (Net.empty())) trm
  in 
  let ret_list = ref None
  and ret_tmp = ref None
  in 
  let rec find_aux l c ty t g= 
    alt
      [
       seq
	 [
	  (** 
	     Try to find a match, first check that rr_depth is not
	     reached.
	   **)
	  (fun g1 ->
	    let ndata = Data.dec_rr_depth c
	    in 
	    cond 
	      (fun _ -> 
		Lib.compare_int_option (Data.get_rr_depth ndata) 0)
	      (simp_fail ~err:No_change)
	      (find_match_tac ret_list (c, ty, qntenv) t) g1);
	  (** Found a match **)
	  (fun g1 -> 
	    let (cntrl1, tyenv1, t1, r1) =
	      Lib.dest_option 
		~err:(Failure "find_all_matches: 1") (!ret_list)
	    in 
	    let rslt = r1::l 
	    in 
	    seq
	      [
	       (** Add the result to ref_tmp **)
	       data_tac (Lib.set_option ret_tmp)
		 (cntrl1, tyenv, t1, rslt);
	       (** Try to go round again **)
	       (find_aux rslt cntrl1 tyenv1 t1 // skip)
	     ] g1)
	];
       (** Failed to find any match **)
       seq
	 [
	  (** Add information to ret_tmp. **)
	  data_tac (Lib.set_option ret_tmp) (c, ty, t, l);
	  (** Raise failure **)
	  fail ~err:(Failure "find_all_matches")
	]
     ] g
  in 
  try 
    seq
      [
       find_aux [] cntrl tyenv trm;
       (fun g ->
	 let (rcntrl0, rtyenv, rtrm, rlist) =
	   Lib.dest_option (!ret_tmp)
	 in 
	 (** Restore original loopdb *)
	 let rcntrl = Data.set_loopdb rcntrl0 orig_loopdb
	 in 
	 data_tac (Lib.set_option ret)
	   (rcntrl, rtyenv, rtrm, List.rev rlist) g)
     ] goal
  with _ -> raise No_change

(**
   [find_subterm_bu_tac ret (ctrl, tyenv, qntenv) trm g]: Make a plan
   to rewrite, bottom-up, the subterms of [trm].

   Return [ret=(ncntrl, ntyenv, ntrm, plan)], where [ncntrl] is the
   new simp data, [ntyenv] the new type-environment, [ntrm] the term
   resulting from simplification and [plan] the constructed rewriting
   plan.

   This is a companion function to {!Simplifier.find_term_bu_tac}.
*)
let rec find_subterm_bu_tac ret data trm goal=
  let (ctrl, tyenv, qntenv) = data 
  in 
  let ret_list = ref None
  and ret_plan = ref None
  in 
  let clear_ret () = ret_list:=None; ret_plan:=None
  in 
  match trm with
    Basic.Qnt(q, b) -> 
      let qntenv1 = Term.bind (Bound q) null_term qntenv
      in 
      seq
	[
	 (** Rewrite quantifier body **)
	 (find_term_bu_tac ret_plan (ctrl, tyenv, qntenv1) b
	    // skip);
	 (fun g1 -> 
	   let (bcntrl, btyenv, btrm, bplan) = 
	     Lib.get_option (!ret_plan) (ctrl, tyenv, b, mk_skip)
	   in 
	   (** Add data *)
	   clear_ret();
	   check_change bplan;
	   let subplan = pack(mk_subnode 0 bplan)
	   in 
	   data_tac 
	     (Lib.set_option ret)
	     (bcntrl, btyenv, Qnt(q, btrm), subplan) g1)
       ] goal
  | Basic.App(f, a)->
      seq 
	[
	 (** Rewrite function term **)
	 (find_term_bu_tac ret_plan (ctrl, tyenv, qntenv) f
	    // skip);
	 (fun g1 ->
	   let (fcntrl, ftyenv, nf, fplan)=
	     Lib.get_option (!ret_plan) (ctrl, tyenv, f, mk_skip)
	   in 
	   clear_ret();
	   seq
	     [
	      (** Rewrite argument term **)
	      (find_term_bu_tac ret_plan (fcntrl, ftyenv, qntenv) a
		 // skip);
	      (fun g2 -> 
		let (acntrl, atyenv, na, aplan) = 
		  Lib.get_option 
		    (!ret_plan) (fcntrl, ftyenv, a, mk_skip)
		in 
		clear_ret();
		(** Add data *)
		check_change2 fplan aplan;
		let subplan = pack(mk_branches [fplan; aplan])
		in 
		data_tac 
		  (Lib.set_option ret)
		  (acntrl, atyenv, App(nf, na), subplan) g2)
	    ] g1)
       ] goal
  | Basic.Typed(tt, ty) -> 
      find_term_bu_tac ret (ctrl, tyenv, qntenv) tt goal
  | _ -> raise No_change
and 
(**
   [find_term_bu_tac ret (ctrl, tyenv, qntenv) trm g]: Traverse term [trm],
   bottom-up, constructing a rewrite plan.

   Return [ret=(ncntrl, ntyenv, ntrm, plan)], where [ncntrl] is the
   new simp data, [ntyenv] the new type-environment, [ntrm] the term
   resulting from simplification and [plan] the constructed rewriting
   plan.
*)
    find_term_bu_tac ret data trm goal =
  let (cntrl, tyenv, qntenv) = data 
  in 
  let ret_list = ref None
  and ret_plan = ref None
  in 
  let clear_ret () = ret_list:=None; ret_plan:=None
  in 
  seq
    [
     (** Rewrite subterms *)
     ((find_subterm_bu_tac ret_plan data trm)
	// skip);
     (fun g1 ->
       let (scntrl, styenv, strm, splan) =
	 Lib.get_option (!ret_plan)
	   (cntrl, tyenv, trm, mk_skip)
       in 
       clear_ret();
       seq
	 [
	  (** Rewrite main term *)
	  (find_all_matches_tac ret_list data strm // skip);
	  (fun g2 ->
	    let (mcntrl, mtyenv, mtrm, rules) =
	      Lib.get_option (!ret_list) (cntrl, tyenv, strm, [])
	    in 
	    clear_ret();
	    let rplan = pack (mk_rules rules)
	    in 
	    check_change2 rplan splan;
	    let plan = pack (mk_node (key_of trm) [splan; rplan])
	    in 
	    data_tac 
	      (Lib.set_option ret)
	      (mcntrl, mtyenv, mtrm, plan) g2)
	] g1)
   ] goal


(**
   [find_subterm_td_tac ret (ctrl, tyenv, qntenv) trm g]: Make a plan
   to rewrite, top-down, the subterms of [trm].

   Return [ret=(ncntrl, ntyenv, ntrm, plan)], where [ncntrl] is the
   new simp data, [ntyenv] the new type-environment, [ntrm] the term
   resulting from simplification and [plan] the constructed rewriting
   plan.

   This is a companion function to {!Simplifier.find_term_bu_tac}.
*)
let rec find_subterm_td_tac ret data trm g=
  let (ctrl, tyenv, qntenv) = data
  in 
  let ret_list = ref None
  and ret_plan = ref None
  in 
  let clear_ret () = ret_list:=None; ret_plan:=None
  in 
  match trm with
    Basic.Qnt(q, b) -> 
      let qntenv2 = Term.bind (Basic.Bound(q)) null_term qntenv
      in 
      seq
	[
	 (** Rewrite quantifier body, top-down **)
	 (find_term_td_tac ret_plan (ctrl, tyenv, qntenv2) b 
	    // skip);
	 (** Add data to ret **)
	 (fun g1 ->
	   let (bcntrl, btyenv, btrm, bplan0) = 
	     Lib.get_option (!ret_plan) (ctrl, tyenv, b, mk_skip)
	   in 
	   check_change bplan0;
	   let bplan = pack (mk_subnode 0 bplan0)
	   in 
	   data_tac
	     (Lib.set_option ret) 
	     (bcntrl, btyenv, Basic.Qnt(q, btrm), bplan) g1)
       ] g
  | Basic.App(f, a)->
      seq
	[
	 (** Rewrite function, top-down **)
	 (find_term_td_tac ret_plan (ctrl, tyenv, qntenv) f
	    // skip);
	 (** Extract function plan *)
	 (fun g1->
	   let (fcntrl, ftyenv, nf, fplan) = 
	     Lib.get_option (!ret_plan) (ctrl, tyenv, f, mk_skip)
	   in 
	   clear_ret();
	   seq
	     [
	      (** Rewrite argument, top-down **)
	      (find_term_td_tac 
		 ret_plan (fcntrl, ftyenv, qntenv) a // skip);
	      (** Extract argument plan *)
	      (fun g2 ->
		let (acntrl, atyenv, na, aplan) = 
		  Lib.get_option (!ret_plan) 
		    (fcntrl, ftyenv, a, mk_skip)
		in 
		(** Add data to ret *)
		check_change2 fplan aplan;
		let subplan = pack (mk_branches [fplan; aplan])
		in 
		data_tac 
		  (Lib.set_option ret)
		  (acntrl, atyenv, App(nf, na), subplan) g2)
	    ] g1)
       ] g
  | Basic.Typed(tt, ty) -> 
      find_term_td_tac ret (ctrl, tyenv, qntenv) tt g
  | _ -> raise No_change
and 
(**
   [find_term_td_tac ret (ctrl, tyenv, qntenv) trm g]: Traverse term
   [trm], top-down, constructing a rewrite plan.

   Return [ret=(ncntrl, ntyenv, ntrm, plan)], where [ncntrl] is the
   new simp data, [ntyenv] the new type-environment, [ntrm] the term
   resulting from simplification and [plan] the constructed rewriting
   plan.
*)
    find_term_td_tac ret data trm goal=
  let (ctrl, tyenv, qntenv) = data 
  in 
  let ret_list = ref None
  and ret_plan = ref None
  in 
  let clear_ret () = ret_list:=None; ret_plan:=None
  in 
  let tac g = 
    seq
      [
       (** Rewrite the current term, ignoring errors **)
       (find_all_matches_tac ret_list data trm // skip);
       (** Descend through the subterms **)
       (fun g1 -> 
	 let (nctrl, ntyenv, ntrm, rules)=
	   Lib.get_option (!ret_list) (ctrl, tyenv, trm, [])
	 in 
	 let tplan = pack (mk_rules rules)
	 in 
	 clear_ret ();
	 seq
	   [
	    (find_subterm_td_tac ret_plan (nctrl, ntyenv, qntenv) ntrm 
	       // skip);
	    (fun g2 ->
	      let (sctrl, styenv, strm, splan) = 
		Lib.get_option (!ret_plan) 
		  (nctrl, ntyenv, ntrm, mk_skip) 
	      in 
	      check_change2 tplan splan;
	      let plan = pack (mk_node (key_of trm) [tplan; splan])
	      in 
	      data_tac (Lib.set_option ret)
		(sctrl, styenv, strm, plan) g2)
	  ] g1)
     ] g
  in 
  tac goal


(**
   [basic_simp_tac data ret tag goal]: Main interface to the basic
   simplifier functions.

   Simplify formula tagged [tag] in [goal]: 
   {ul
   {- Descend top-down or bottom-up into formula, at each level collect
   rewrite rules which can be used to rewrite the term.}
   {- Use collected rules to rewrite the formula.}}

   Doesn't clean up afterwards.

   Returns [ret = ndata] where [ndata] is [data] updated with the
   rules used to rewrite the formula.

   raise [No_change] if no rules can be found.
 *)
let rec basic_simp_tac cntrl ret ft goal=
  let tyenv= typenv_of goal
  and sqnt = sequent goal
  in 
  let rr_cntrl = Data.get_control cntrl
  and rr_depth = Data.get_rr_depth cntrl
  and rr_conds = Data.get_cond_depth cntrl
  in 
  let (trm, is_concl)=
    let (ftrm, flag) = get_form ft sqnt
    in
    (Formula.term_of (Logic.drop_tag ftrm), flag)
  in 
  let ret_plan=ref None
  in
  let trivial f g = Boollib.trivial ~f:f g
  in 
  let cntrl1 = Data.add_loopdb cntrl trm
  in 
  let tac1 g1 = (** Get the rewrites **)
    let data = (cntrl1, tyenv, Term.empty_subst())
    in 
    cond 
      (fun _ -> rr_cntrl.Rewrite.rr_strat = Rewrite.bottomup)
      (find_term_bu_tac ret_plan data trm)
      (find_term_td_tac ret_plan data trm) g1
  in 
  let tac2 g2 = (** Apply the rewrites found by tac1 **)
    let (ncntrl0, ntyenv, ntrm, plan) =  
      Lib.dest_option ~err:(Failure "basic_simp_tac: 1") (!ret_plan)
    in 
    (if (Data.mem_loopdb (scope_of g2) ncntrl0 ntrm)
    then raise No_change
    else ());
    (** Check the rewrite plan **)
    match Lib.try_app check_change plan with
      None -> trivial (ftag ft) g2
    | _ -> 
	(** Reset the control data **)
	let ncntrl =
	  Data.set_rr_depth
	    (Data.set_conds ncntrl0 rr_conds) rr_depth
	in 
	let info = mk_info()
	in 
	(try
	  seq
	    [
	     simp_rewrite_tac ~info:info 
	       is_concl plan trm (ftag ft);
	     data_tac (Lib.set_option ret) ncntrl
	   ] g2
	with _ -> raise No_change)
  in 
  try
    (seq [tac1 ; tac2] goal)
  with _ -> raise No_change


(***
* Derived simplifier functions 
***)

(**
   [simp_prep_tac data ret lbl g]: Prepare goal [g] for simplifying
   formula [lbl].

   Returns [ret = ncontrol] where [ncontrol] is the new control
   recording formulas added/modified by simp_prep_tac

   Currently this does nothing except strip the quantifiers off
   formula [lbl].

   Always succeeds.
 *)

let simp_asm_tac ctrl ret lbl = 
  seq
    [
     data_tac (fun _ -> Lib.set_option ret ctrl) ();
     specA // skip
   ]

let simp_concl_tac ctrl ret lbl = 
  seq
    [
     data_tac (fun _ -> Lib.set_option ret ctrl) ();
     specC // skip
   ]

let simp_prep_tac ctrl ret lbl goal = 
  let is_asm =
    try (ignore(get_tagged_asm lbl goal); true)
    with _ -> false
  in 
  if(is_asm)
  then 
    simp_asm_tac ctrl ret lbl goal
  else
    simp_concl_tac ctrl ret lbl goal


(**
   [cond_prover_tac ctrl tg g]: The tactic used to prove the conditions of
   rewrite rules.

   If ctrl.conds > 0, 
   decrement ctrl.conds,
   apply [simp_prep_tac]
   apply [basic_simp_tac].
   apply [Logic.Tactics.trueR] to solve goal
   reset ctrl.conds to original value.
   
   If not(ctrl.conds > 0) fail.
 *) 
      
let cond_prover_trueC = Logic.Tactics.trueC

let rec cond_prover_worker_tac ctrl1 ret tg0 g2= 
  let ctrl1 = 
    Lib.dest_option ~err:(Failure "cond_prover_worker_tac: 1") (!ret)
  in 
  repeat (basic_simp_tac ctrl1 ret tg0) g2

let cond_prover_tac ctrl tg goal=
  let cond_depth = Data.get_cond_depth ctrl
  in 
  let ret=ref None
  in 
  if (Lib.apply_option (fun i -> i>0) cond_depth true)
  then 
    let data = Data.dec_cond_depth ctrl
    in 
    (alt
       [
	Logic.Tactics.trueC (ftag tg);
	seq
	  [
	   simp_prep_tac data ret (ftag tg);
	   cond_prover_worker_tac data ret tg;
	   cond_prover_trueC (ftag tg)
	 ]
      ]) goal
  else
    fail ~err:No_change goal


(**
   [inital_flatten_tac exclude g]: Prepare goal for simplification.

   Flatten all except formulas with tag in [exclude].  Try to prove
   trivial facts. Put conclusions into assumptions (by negation)
 *)

let simp_asm_elims =
  [
   (fun inf l -> Boollib.falseA ~info:inf ~a:l);
   (fun inf -> Logic.Tactics.negA ~info:inf);
   (fun inf -> Logic.Tactics.conjA ~info:inf);
   (fun inf -> Logic.Tactics.existA ~info:inf)
 ]

let simp_concl_elims =
  [
   (fun inf -> Logic.Tactics.trueC ~info:inf);
   (fun inf -> Logic.Tactics.disjC ~info:inf);
   (fun inf -> Logic.Tactics.allC ~info:inf)
 ]


let simp_flatten_asms_tac ?info lst = 
  Boollib.asm_elim_rules_tac ?info (simp_asm_elims, []) lst

let simp_flatten_concls_tac ?info lst = 
  Boollib.concl_elim_rules_tac ?info ([], simp_concl_elims) lst

let simp_flatten_tac excluded ?f goal =
  let basic_flatter ?info =
    Boollib.elim_rules_tac ?info:info (simp_asm_elims, simp_concl_elims)
  in 
  match f with
    None -> Boollib.apply_elim_tac basic_flatter ?f goal
  | Some(l) -> 
      let tg = Logic.label_to_tag l (sequent goal)
      in 
      if (List.exists (Tag.equal tg) excluded)
      then skip goal
      else Boollib.apply_elim_tac basic_flatter ?f goal

let initial_flatten_tac exclude goal=
  simp_flatten_tac exclude goal 

