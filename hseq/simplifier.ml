(*----
  Name: simplifier.ml
  Copyright Matthew Wahab 2005-2018
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

(** The simplifier engine. *)

(** Simplifier actions:

    Simple actions (no conditional rewrites): Recurse through the
    structure of a term, getting a list of the rewrite rules to be
    applied

    Always: Recurse through the structure of a term, getting a list of
    the rewrite rules to be applied

    For each subterm: Get a (un)conditional rule which could be
    applied. If conditional, try to prove the condition discarding
    the rule on failure.  If successfull apply the rule.
*)

open Basic
open Term
open Lterm
open Tactics
open Rewritekit
open Rewrite

open Simputils
open Simpset

(*** Error handling ***)

let print_simp_error s ts fmt pinfo =
    Format.fprintf fmt "@[simplifier error:@ %s @;@[" s;
    Printer.print_sep_list ((Printers.print_term pinfo), ",") ts;
    Format.fprintf fmt "@]@]@."

let error s ts = Report.mk_error (print_simp_error s ts)
let add_error s t e = Report.add_error e (error s t)

exception No_change

(*** Simplifier data. ***)

type control = Rewrite.control

module Data =
struct

  type loopDB = Basic.term Net.net
  (** Structure used to store terms for looping rewriting detection *)

  (** [type Data.t] Information used by and built up during
      simplification.  *)
  type t =
      {
        (** [simpset]: the simpset being used. Assumptions may be
            added to this during the course of simplification *)
        simpset:Simpset.simpset;
        (** [cond_tac]: the tactic used to prove conditions of rewrite
            rules *)
        cond_tac: t -> Logic.ftag_ty -> Tactics.tactic;
        (** [control]: rewrite control ([direction] is ignored *)
        control: Rewrite.control;
        (** conds: max. no. of conditions to try and prove at once *)
        conds: int option;
        (** rr_depth: max. no. of rr rules to apply at one level *)
        rr_depth: int option;
        (** asms: assumptions generated during the course of
            simplification *)
        asms: Logic.ftag_ty list;
        (** visited: formulas visited during the course of
            simplification *)
        visited: Logic.ftag_ty list;
        (** exclude: formulas not to use as a rewrite rule *)
        exclude: Logic.ftag_ty list;
        (** loopdb: Terms already rewritten. *)
        loopdb: loopDB}

      let make (sset, tac, cntrl, cd, rd, a, vs, ex, rs) =
        {
          simpset = sset;
          cond_tac = tac;
          conds = cd;
          control = cntrl;
          rr_depth = rd;
          asms = a;
          visited = vs;
          exclude = ex;
          loopdb = Net.empty()
        }

      let set_simpset cntrl set =
        {cntrl with simpset = set}

      let set_tactic cntrl tac =
        {cntrl with cond_tac = tac}

      let set_conds cntrl d =
        {cntrl with conds=d}

      let set_conds_val cntrl d =
        {cntrl with conds = Lib.set_int_option d}

      let set_control cntrl c =
        {cntrl with control = c}

      let set_rr_depth cntrl d =
        {cntrl with rr_depth = d}

      let set_rr_depth_val cntrl d =
        {cntrl with rr_depth = Lib.set_int_option d}

      let set_asms cntrl ds =
        {cntrl with asms = ds}

      let set_visited cntrl ds =
        {cntrl with visited = ds}

      let set_exclude cntrl ds =
        {cntrl with exclude = ds}

      let set_loopdb cntrl ds =
        {cntrl with loopdb = ds}

      let get_loopdb cntrl = cntrl.loopdb

      let add_loopdb cntrl t =
        let varp x = false
        in
        set_loopdb cntrl (Net.add varp (get_loopdb cntrl) t t)

      let mem_loopdb scp cntrl t =
        let opts =
          try Net.lookup (get_loopdb cntrl) t
          with Not_found -> []
        in
        List.exists (Lterm.alpha_equals scp t) opts

      let get_simpset cntrl = cntrl.simpset
      let get_tactic cntrl = cntrl.cond_tac
      let get_control cntrl = cntrl.control

      let add_asm cntrl a =
        set_asms cntrl (a::(cntrl.asms))

      let get_asms cntrl = cntrl.asms

      let get_visited cntrl = cntrl.visited

      let get_exclude cntrl = cntrl.exclude

      let dec_cond_depth cntrl =
        set_conds cntrl (Lib.dec_int_option (cntrl.conds))

      let get_cond_depth cntrl = cntrl.conds

      let dec_rr_depth cntrl =
        set_conds cntrl (Lib.dec_int_option (cntrl.rr_depth))

      let get_rr_depth cntrl = cntrl.rr_depth

      let add_simp_rule cntrl rule =
        set_simpset cntrl (Simpset.add_rule rule (get_simpset cntrl))

      let default_rr_depth = ref (Some 1000)
      let default_cond_depth = ref (Some 1000)

      (** [default]: The default control information *)
      let default = make (Simpset.empty_set(),
                          (fun _ __ -> skip),
                          Formula.default_rr_control,
                          (!default_cond_depth),
                          (!default_rr_depth),
                          [], [], [], [])
end

(*** Utility functions ***)

(** [strip_rrs]: prepare for direct rewriting of term. For tests
    only *)
let strip_rrs rrs =
  let strip_rr x =
    match x with
      | Logic.RRThm x -> Logic.term_of x
      | __ -> failwith "simp_tac"
  in
  List.map strip_rr rrs

  (** [is_conditional rl]: True if simp rule [rl] is conditional.
  *)
let is_conditional rl =
  match Simpset.rule_cond rl with
    | None -> false
    | _ -> true

(** [is_none x]: true if [x=None], false otherwise. *)
let is_none x =
 match x with
   | None -> true
   | _ -> false

(** [is_excluded excluded sqnt rl]: True if rewrite rule [rl] is an
    assumption in the excluded list.  *)
let is_excluded  excluded sqnt x =
  match x with
    | Logic.Asm l ->
      let tag = Logic.label_to_tag l sqnt
      in
      List.exists (Tag.equal tag) excluded
    | Logic.OAsm(l, _) ->
      let tag = Logic.label_to_tag l sqnt
      in
      List.exists (Tag.equal tag) excluded
    | _ -> false

(** [get_form t n]: Get formula tagged [t] from node [n].  First try
    conclusions, then try assumptions.  raise [Not_found] if not found.
*)
let get_form t sqnt =
  try Logic.Sequent.get_tagged_cncl t sqnt
  with Not_found -> Logic.Sequent.get_tagged_asm t sqnt

(** [simp_fail]: A hook to allow failures to be traced.
*)
let simp_fail ?err g = Tactics.fail ?err g

(** [check_change p]: Test whether plan [p] does anything. Raise
    [No_change] if it does not.
*)
let check_change x =
  match x with
    | Rewritekit.Skip -> raise No_change
    | _ -> ()

(** [check_change2 p1 p2]: Test either plan [p1] or plan [p2] does
    anything. Raise [No_change] if both do nothing.
*)
let check_change2 x y =
  match (x, y) with
    | (Rewritekit.Skip, Rewritekit.Skip) -> raise No_change
    | _ -> ()

(** [check_add_loop scp cntrl t]: Test whether term [t] is in the
    loopdb. If it isn't, add it to the loopdb.
*)
let check_add_loop scp cntrl t =
  if Data.mem_loopdb scp cntrl t
  then raise (Failure "check_add_loop")
  else Data.add_loopdb cntrl t

let null_term = Term.mk_free "" (Gtypes.mk_null())

(** [get_form t n]: Get formula tagged [t] from node [n].  First try
    conclusions, then try assumptions.  return the formula and a flag
    which is [true] if the formula was in the conclusions and false if
    the formula was in the assumptions.  raise [Not_found] if not
    found.
*)
let get_form t sqnt =
  match Lib.try_find (Logic.Sequent.get_tagged_cncl t) sqnt
  with
    | None -> (Logic.Sequent.get_tagged_asm t sqnt, false)
    | Some(x) -> (x, true)

(*** Utility tactics ***)

let cleanup = ref true

(** [clean_up_tac ctrl g]: Clean up after simplification.  Delete all
    assumptions listed in [ctrl.asms].  *)
let clean_aux_tac tags g =
  map_every (fun x -> deleteA (Logic.FTag x)) tags g

let clean_up_tac ctrl g=
  if !cleanup
  then clean_aux_tac (Data.get_asms ctrl) g
  else skip g

(** [copyA_inst_tac info vals x]: Copy assumption [x], instantiate the
    copy with [vals]. info: aformulas = [x1], where [x1] is the tag of
    the new assumption.  Fails if there are more terms in [vals] then
    variables in [x].
*)
let copyA_inst_tac vals x ctxt goal =
  seq
    [
      copyA x;
      (?> (fun inf1 ctxt1 g1 ->
        let asm_tg = get_one ~msg:"copyA_inst_tac" (Info.aformulas inf1) in
        let asm_form = drop_tag (get_tagged_asm (ftag asm_tg) g1)
        in
        if (Formula.is_all asm_form)
        then instA ~a:(ftag asm_tg) vals ctxt1 g1
        else skip ctxt1 g1))
    ] ctxt goal

(** [cut_rr_rule info vals t g] Cut rule [t] into goal [g],
    instantiating with vals.  If [t] is a theorem, it is cut into the
    goal.  If [t] is an assumption, it is copied.

    info: aforms = [[x]] where [x] is the tag of the new assumption.
*)
let cut_rr_rule vals t g =
  match t with
    | Logic.RRThm(th) ->
      cut ~inst:vals th g
    | Logic.ORRThm(th, _) ->
      cut ~inst:vals th g
    | Logic.Asm(x) ->
      copyA_inst_tac vals x g
    | Logic.OAsm(x, _) ->
      copyA_inst_tac vals x g

(** [simp_rewrite_tac is_concl plan term lbl]: Local interface
    to the main rewriting tactics. If [is_concl] is true, call
    [Tactics.pure_rewriteC plan ~term:trm lbl goal] otherwise
    call [Tactics.pure_rewriteA plan ~term:trm lbl goal].

*)
let simp_rewrite_tac is_concl plan trm lbl goal =
  if is_concl
  then pure_rewriteC plan ~term:trm lbl goal
  else pure_rewriteA plan ~term:trm lbl goal

(*** Conditional rule tactics ***)

type tag_pair = (Logic.ftag_ty * Logic.ftag_ty)

(** [prep_cond_tac cntrl values thm g]

    Cut [thm] into the sequent, instantiate with [values].  Apply
    [implA] to get two subgoals, tagged [(cgltg, rgltg)] with
    condition in [cgltg] tagged [cftg] and rewrite-rule in [rgltg]
    tagged [rrftg].  Add [rrftg] to [cntrl], getting [ncntrl].

    return g3
    ret=(ncntrl, [cgltg; rgltg], [cftg; rrftg])
*)

let prep_cond_tac cntrl values thm ctxt goal =
  let add_data cntrl rl_ftg (sgls, cforms) =
    (* (condition-goal, rule-goal) *)
    let (cnd_gltg, rl_gltg) =
      get_two ~msg:"prep_cond_tac: goals" sgls
    in
    (* condition-formula-tag *)
    let cnd_ftg =
      get_one ~msg:"prep_cond_tac: forms" cforms
    in
    (* new control data *)
    let ncntrl = Data.add_asm cntrl rl_ftg
    in
    (ncntrl, (cnd_gltg, rl_gltg), (cnd_ftg, rl_ftg))
  in
  let main_tac ctxt0 g =
    fold_seq (None, Tag.create())
      [
        (* Add the theorem to the sequent. *)
        (fun fold_data -> (fold_data >+ cut_rr_rule values thm));
        (* Apply implA *)
        (fun (data, _) ctxt1 g1 ->
          let info1 = Info.changes g1 in
          let rl_ftg = Lib.get_one (Info.aformulas info1) No_change
          in
          ((data, rl_ftg) >+ Tactics.implA ~a:(ftag rl_ftg)) ctxt1 g1);
        (* Extract the data from the subgoal *)
        (fun (data, rl_ftg) ctxt1 g1 ->
          let info1 = Info.changes g1 in
          let asm_tg = List.hd (Info.aformulas info1) in
          let ret_data = (add_data cntrl asm_tg
                            (Info.subgoals info1, Info.cformulas info1))
          in
          ((Some(ret_data), asm_tg) >+ skip) ctxt1 g1)
      ] ctxt0 g
  in
  let extractor data =
    match data with
      | (Some(d), _) -> d
      | _ -> failwith "prep_cond_tac: Failed to prepare condition."
  in
  try (main_tac >/ extractor) ctxt goal
  with x -> raise (Report.add_error (Failure "prep_cond_tac") x)


(** [prove_cond_tac cntrl tac values entry g]: Prepare a conditional
    simp rule [entry] for use in rewriting.

    Use [prep_cond_tac] to add the rule to the goal and create a
    subgoal for the condition. Use tactic [cntrl.cond_tac] to prove
    the condition, failing if the condition can't be proved.

    Return [ret = (ncntrl, rl)] where [ncntrl] is the new simp data and
    [rl] the rewrite rule built from the new theorem/assumption.
*)
let prove_cond_tac (cntrl: Data.t) values entry ctxt goal =
  let (thm: Logic.rr_type) = Simpset.rule_src entry
  and orig_loopdb = Data.get_loopdb cntrl
  in
  let main_tac cntrl ctxt0 g =
    fold_seq (None, None)
      [
       (** Add rule to the goal assumptions. *)
        (fun (_, data2) -> (prep_cond_tac cntrl values thm)
          >/ (fun x -> (Some x, data2)));
        (** Prove the condition. **)
        (fun (cond_data, data2) ctxt1 g1 ->
          let (ncntrl, (cnd_gltg, rl_gltg), (cnd_ftg, rl_ftg)) =
            Lib.dest_option ~err:(Failure "prove_cond_tac: 1") cond_data
          in
          let prover_tac = Data.get_tactic ncntrl
          in
          (** Apply the prover to the sub-goal with the condition to
              prove. *)
          if (Tag.equal cnd_gltg (node_tag g1))
          then ((cond_data, data2) >+ prover_tac ncntrl cnd_ftg) ctxt1 g1
          else
            (** Get the rule-data from the other sub-goal.*)
            begin
              let rcntrl = Data.set_loopdb ncntrl orig_loopdb
              and form = drop_tag (get_tagged_asm (ftag rl_ftg) g1) in
              let rule =
                Simpset.make_rule
                  (Logic.Asm (ftag rl_ftg))
                  (Formula.term_of form)
              in
              let rule_data = Some(Data.add_simp_rule rcntrl rule,
                                   Logic.Asm(ftag rl_ftg))
              in
              ((cond_data, rule_data) >+ skip) ctxt1 g1
            end)
      ] ctxt0 g
  in
  let extractor x =
    match x with
    | (_, Some(z)) -> z
    | _ -> (failwith "prove_cond_tac: 2")
  in
  (** Ensure tac left only one subgoal (so condition is solved) *)
  let test_result br =
    match branch_subgoals br with
      | [ x ] -> true
      | _ -> false
  in
  let (det, br) = (main_tac cntrl >/ extractor) ctxt goal
  in
  if test_result br then (det, br)
  else failwith "prove_cond_tac"

(*** Simplifier functions ***)

let log str x = ignore(x)

type match_data =
    {
      (** Simplifier data *)
      cntrl: Data.t;
      (** Type environment *)
      tyenv: Gtypes.substitution;
      (** Quantifier environment *)
      qntenv: Term.substitution;
    }

let mk_match_data acntrl atyenv aqntenv =
  {cntrl = acntrl; tyenv = atyenv; qntenv = aqntenv}

let dest_match_data data =
  (data.cntrl, data.tyenv, data.qntenv)


(** [match_rewrite scp tyenv qntenv trmenv rule trm]: Try to match lhs
    of [rule] with [trm] in type envivornment [tyenv] and term bindings
    [trmenv]. Return rhs of [rule], instantiated with the binding from
    the match, and the type and term environments that made the match
    successful. Raise [Failure] on failure.  *)
let match_rewrite scp tyenv qntenv rl trm =
  let (qs, _, lhs, rhs, order, src) = rl in
  let varp = Rewrite.is_free_binder qs in
  let find_match term1 term2 =
    Unify.matches_rewrite scp tyenv (Term.empty_subst()) varp term1 term2
  in
  try
    begin
      let ntyenv, nenv = find_match lhs trm in
      let nt = Lterm.subst_closed qntenv nenv rhs
      in
      match order with
        | None -> (src, ntyenv, nenv, nt)
        | Some(p) ->
          if p nt trm
          then (src, ntyenv, nenv, nt)
          else raise (Failure "match_rewrite")
    end
  with x -> (failwith "match_rewrite")

(** [find_basic ret data rl trm g]: Try to match simp rule [rl] with
    term [trm] in goal [g], with [data=(cntrl, tyenv, qntenv)]. If
    [rl] matches but is conditional, try to prove the condition using
    tactic [cntrl.cond_tac], adding the rule to the goal assumptions.

    Returns [ret = (ndata, ntrm, rr)] where [ndata = (ncntrl, ntyenv,
    qntenv)], [ncntrl] is the updated simplifier data, [nytenv] is the
    type environment made by the matching, [ntrm] is the result of
    rewriting [trm] and [rl] the rewrite rule to add to the list being
    compiled.
*)
let find_basic data rl trm ctxt (goal: Logic.node) =
  let (cntrl, tyenv, qntenv) = (data.cntrl, data.tyenv, data.qntenv)
  and (qs, c, lhs, rhs, order, thm) = rl
  and scp = scope_of_goal goal in
  (* Test whether the rule is a match, throws an exception on
     failure.  *)
  let (src, ntyenv, ntenv, ntrm) =
    match_rewrite scp tyenv qntenv rl trm
  in
  (* Test for a looping rewrite *)
  let cntrl1 =
    try check_add_loop scp cntrl ntrm
    with _ -> raise No_change
  in
  let tac ctxt0 g =
    if (is_conditional rl)
    then
      let values = extract_consts qs ntenv
      in
      ((prove_cond_tac cntrl1 values rl) >/
          (fun (ncntrl, rr) ->
            ({data with cntrl = ncntrl; tyenv = ntyenv},
             ntrm, rr))) ctxt0 g
    else
      (skip +< ({data with cntrl = cntrl1}, ntrm, thm)) ctxt0 g
  in
  try tac ctxt goal
  with _ -> raise No_change

(** [find_match_tac data trm g]: Find a rule in simpset [set] which
    matches term [trm] in goal [g], with [data=(cntrl, tyenv,
    qntenv)]. If found, rewrite [trm] with the rule.

    Returns [ret = (ndata, ntrm, rr)] where [ndata = (ncntrl, ntyenv,
    qntenv)], [ncntrl] is the updated data, [nytenv] is the type
    environment made by the matching, [ntrm] is the result of rewriting
    [trm] with [rl] and [rl] the rewrite rule to add to the list being
    compiled.

    Raise [No_change] and set [ret:=None] if no matches.
*)
let find_match_tac data trm ctxt goal =
  let (cntrl, tyenv, qntenv) = (data.cntrl, data.tyenv, data.qntenv)
  in
  let sqnt = sequent goal
  and excluded = Data.get_exclude cntrl
  in
  let rec find_aux rls t ctxt0 g =
    match rls with
      | [] -> raise No_change
      | rl::nxt ->
        let src = Simpset.rule_src rl
        in
        if is_excluded excluded sqnt src
        then find_aux nxt t ctxt0 g
        else
          begin
            try find_basic data rl t ctxt0 g
            with _ -> find_aux nxt t ctxt0 g
          end
  in
  let gctxt = goal_context ctxt goal in
  let lst =
    try lookup gctxt (Data.get_simpset cntrl) trm
    with _ -> raise No_change
  in
  find_aux lst trm gctxt goal

(** [find_all_matches ret (cntrl, tyenv, qntenv) trm g]: Find all
    rules in simpset [cntrl.set] which can be used to rewrite term
    [trm] in goal [g].

    Returns new simp data, the new type environment and the rewritten
    term. The new simp data is built by adding the rules used to
    rewrite the term, in the order they are applied.
*)
let rec find_all_matches_tac data trm ctxt goal =
  let rec find_aux data1 trm1 rrlist1 ctxt1 g1 =
    (** Try to find a match, checking that rr_depth is not reached.
    **)
    let (cntrl1, tyenv1, qntenv1) = dest_match_data data in
    let cntrl2 = Data.dec_rr_depth cntrl1
    in
    if Lib.compare_int_option (Data.get_rr_depth cntrl2) 0
    then ((data1, trm1, rrlist1) >+ skip) ctxt1 g1
    else
      begin
        try
          fold_seq (data1, trm1, rrlist1)
            [
              (fun (data2, trm2, rrlist2) ->
                (find_match_tac data2 trm2
                 >/ (fun (data3, trm3, rr3)
                 ->
                   ({data3 with qntenv = qntenv1},
                    trm3, rr3::rrlist2))));
              (fun (data3, trm3, rrlist3) ->
                find_aux data3 trm3 rrlist3)
            ] ctxt1 g1
        with _ -> ((data1, trm1, rrlist1) >+ skip) ctxt1 g1
      end
  in
  (** Get the original loopdb *)
  let (cntrl0, tyenv0, qntenv0) = dest_match_data data in
  let orig_loopdb = Data.get_loopdb cntrl0 in
  let ncntrl = Data.add_loopdb (Data.set_loopdb cntrl0 (Net.empty())) trm
  in
  try
    begin
      (find_aux {data with cntrl = ncntrl} trm []
       >/
         (** Restore original loopdb *)
         (fun (data1, trm1, rlist) ->
           let cntrl2 = Data.set_loopdb cntrl0 orig_loopdb
           in
           ({data1 with cntrl = cntrl2}, trm1, List.rev rlist)))
        ctxt goal
    end
  with _ -> raise No_change

(** [find_subterm_bu_tac ret (ctrl, tyenv, qntenv) trm g]: Make a plan
    to rewrite, bottom-up, the subterms of [trm].

    Return [ret=(ncntrl, ntyenv, ntrm, plan)], where [ncntrl] is the
    new simp data, [ntyenv] the new type-environment, [ntrm] the term
    resulting from simplification and [plan] the constructed rewriting
    plan.

    This is a companion function to {!Simplifier.find_term_bu_tac}.
*)
let rec find_subterm_bu_tac data trm ctxt goal =
  let rw_term_tac ((data: match_data), plan) t ctxt0 g =
    try find_term_bu_tac data t ctxt0 g
    with _ -> ((data, t, plan) >+ skip) ctxt0 g
  in
  let (ctrl, tyenv, qntenv0) = dest_match_data data
  in
  match trm with
    | Basic.Qnt(q, b) ->
      let finalize (data1, btrm, plan1) =
        check_change plan1;
        let subplan = pack (mk_subnode 0 plan1)
        in
        ({data1 with qntenv = qntenv0}, Qnt(q, btrm), subplan)
      in
      let qntenv1 = Term.bind (Bound q) null_term qntenv0 in
      let data1 = {data with qntenv = qntenv1}
      in
      begin
        (** Rewrite quantifier body **)
        (rw_term_tac (data1, mk_skip) b >/ finalize) ctxt goal
      end
    | Basic.App(f, a)->
      let finalize (data, nf, na, fplan, aplan) =
        check_change2 fplan aplan;
        let subplan = pack(mk_branches [fplan; aplan])
        in
        (data, App(nf, na), subplan)
      in
      begin
        ((fold_seq (data, f, a, mk_skip, mk_skip)
            [
             (** Rewrite function term **)
              (fun (data1, f1, a1, fplan1, aplan1) ->
                (rw_term_tac (data1, fplan1) f1 >/
                   (fun (data2, f1, fplan2) ->
                     ({data2 with qntenv = qntenv0},
                      f1, a1, fplan2, aplan1))));

              (** Rewrite argument term **)
              (fun (data1, f1, a1, fplan1, aplan1) ->
                (rw_term_tac (data1, aplan1) a1
                 >/
                   (fun (data2, a1, aplan2) ->
                     ({data2 with qntenv = qntenv0},
                      f1, a1, fplan1, aplan2))))
            ]) >/ finalize) ctxt goal
      end
    | _ -> raise No_change
(** [find_term_bu_tac ret (ctrl, tyenv, qntenv) trm g]: Traverse term
    [trm], bottom-up, constructing a rewrite plan.

    Return [ret = (ncntrl, ntyenv, ntrm, plan)], where [ncntrl] is the
    new simp data, [ntyenv] the new type-environment, [ntrm] the term
    resulting from simplification and [plan] the constructed rewriting
    plan.
*)
and find_term_bu_tac data trm ctxt goal =
  let (cntrl, tyenv, qntenv) = dest_match_data data
  in
  let finalize (data1, trm1, mplan, splan) =
    check_change2 mplan splan;
    let plan = pack (mk_node [mplan; splan])
    in
    ({data1 with qntenv = qntenv}, trm1, plan)
  in
  let rw_term_tac (data1, trm1, mplan1, splan1) ctxt0 g =
    try (find_all_matches_tac data1 trm1
         >/
           (fun (data2, trm2, rrlist) ->
             let mplan2 = pack (mk_rules rrlist)
             in
             ({data2 with qntenv = qntenv},
              trm2, mplan2, splan1))) ctxt0 g
    with _ -> ((data1, trm1, mplan1, splan1) >+ skip) ctxt0 g
  in
  begin
    (fold_seq (data, trm, mk_skip, mk_skip)
       [
         (** Rewrite subterms *)
         (fun fold_arg ->
           alt_data fold_arg
             [
               (fun (data1, trm1, mplan1, splan1) ->
                 (find_subterm_bu_tac data1 trm1
                  >/
                    (fun (data2, trm2, plan2) ->
                      ({data2 with qntenv = qntenv},
                       trm2, mplan1, plan2))));
                 (fun alt_arg -> (alt_arg >+ skip))
             ]);
         (** Rewrite main term *)
         rw_term_tac
       ] >/ finalize) ctxt goal
  end

(** [find_subterm_td_tac ret (ctrl, tyenv, qntenv) trm g]: Make a plan
    to rewrite, top-down, the subterms of [trm].

    Return [ret = (ncntrl, ntyenv, ntrm, plan)], where [ncntrl] is the
    new simp data, [ntyenv] the new type-environment, [ntrm] the term
    resulting from simplification and [plan] the constructed rewriting
    plan.

    This is a companion function to {!Simplifier.find_term_bu_tac}.
*)
let rec find_subterm_td_tac data trm ctxt goal =
  let (_, _, qntenv) = dest_match_data data in
  match trm with
    | Basic.Qnt(q, b) ->
      let qntenv2 = Term.bind (Basic.Bound(q)) null_term qntenv
      in
      begin
        (** Rewrite quantifier body, top-down **)
        (alt_data ({data with qntenv = qntenv2}, b, mk_skip)
          [
            (fun (data1, b1, plan1) -> find_term_td_tac data1 b1);
            (fun alt_arg -> (alt_arg >+ skip))
          ]
         >/
          (** Add data to ret **)
          (fun (data1, trm1, plan1) ->
            check_change plan1;
            let plan = pack (mk_subnode 0 plan1)
            in
            ({data1 with qntenv = qntenv},
             Basic.Qnt(q, trm1), plan))) ctxt goal
      end
    | Basic.App(f, a)->
      let rw_term_tac ((data: match_data), plan) t ctxt1 g =
        try find_term_td_tac data t ctxt1 g
        with _ -> ((data, t, plan) >+ skip) ctxt1 g
      in
      let finalize (data1, nf, na, fplan1, aplan1) =
        check_change2 fplan1 aplan1;
        let subplan = pack (mk_branches [fplan1; aplan1])
        in
        (data1, App(nf, na), subplan)
      in
      begin
        (fold_seq (data, f, a, mk_skip, mk_skip)
           [
             (** Rewrite function, top-down **)
             (fun (data1, f1, a1, fplan1, aplan1) ->
               (rw_term_tac (data1, fplan1) f1
                  >/
                    (fun (data2, f2, fplan2) ->
                      (data2, f2, a1, fplan2, aplan1))));

             (** Rewrite argument, top-down **)
             (fun (data1, f1, a1, fplan1, aplan1) ->
               (rw_term_tac (data1, aplan1) a1
                  >/
                    (fun (data2, a2, aplan2) ->
                      (data2, f1, a2, fplan1, aplan2))))
           ]
         >/ finalize) ctxt goal
      end
    | _ -> raise No_change
(** [find_term_td_tac ret (ctrl, tyenv, qntenv) trm g]: Traverse term
    [trm], top-down, constructing a rewrite plan.

    Return [ret = (ncntrl, ntyenv, ntrm, plan)], where [ncntrl] is
    the new simp data, [ntyenv] the new type-environment, [ntrm]
    the term resulting from simplification and [plan] the
    constructed rewriting plan.  *)
and find_term_td_tac (data: match_data) trm (ctxt: Context.t) goal =
  let (cntrl, tyenv, qntenv) = dest_match_data data
  in
  let rw_term_tac ((data1: match_data), trm1, mplan1, splan1) ctxt0 g =
  let (_, _, qntenv1) = dest_match_data data1
  in
    try
      begin
        (find_all_matches_tac data1 trm1
         >/
           (fun (data2, trm2, rrlist) ->
             let mplan2 = pack (mk_rules rrlist)
             in
             ({data2 with qntenv = qntenv1},
              trm2, mplan2, splan1))) ctxt0 g
      end
    with _ -> ((data1, trm1, mplan1, splan1) >+ skip) ctxt0 g
  in
  let tac ctxt0 g =
    fold_seq (data, trm, mk_rules [], mk_skip)
      [
        (** Rewrite the current term, ignoring errors **)
        rw_term_tac ;

        (** Descend through the subterms **)
        (fun fold_arg ->
          (alt_data fold_arg
             [
               (fun (data1, trm1, mplan1, splan1) ->
                 find_subterm_td_tac data trm1
                 >/
                   (fun (data2, trm2, splan2) ->
                     ({data2 with qntenv = qntenv},
                      trm2, mplan1, splan2)));
               (fun alt_arg -> (alt_arg >+ skip))
             ]))
      ] ctxt0 g
  in
  let finalize (data1, trm1, tplan, splan) =
    check_change2 tplan splan;
    let plan = pack (mk_node [tplan; splan])
    in
    (data1, trm1, plan)
  in
  (tac >/ finalize) ctxt goal

(** [basic_simp_tac data ret tag goal]: Main interface to the basic
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
let basic_simp_tac cntrl ft ctxt goal =
  let rr_depth cntrl = Data.get_rr_depth cntrl
  and rr_conds cntrl = Data.get_cond_depth cntrl
  in
  let sqnt = sequent goal in
  let (trm, is_concl) =
    let (ftrm, flag) = get_form ft sqnt
    in
    (Formula.term_of (Logic.drop_tag ftrm), flag)
  in
  let tac1 ((data1: match_data), trm1, _) ctxt1 g1 =
    (** Get the rewrite plan **)
    let rr_cntrl = Data.get_control (data1.cntrl) in
    if (rr_cntrl.Rewrite.rr_strat = Rewrite.bottomup)
    then find_term_bu_tac data1 trm1 ctxt1 g1
    else find_term_td_tac data1 trm1 ctxt1 g1
  in
  let tac2 (data1, trm1, plan1) ctxt1 g1 =
    (** Apply the rewrite plan found by tac1 **)
    let cntrl1 = data1.cntrl in
    begin
      if Data.mem_loopdb (scope_of_goal g1) cntrl1 trm1
      then raise No_change
      else ()
    end;
    (** Check the rewrite plan **)
    if (Lib.try_app check_change plan1) = None
    then ((data1, trm1, plan1) >+ Boollib.trivial ~f:(ftag ft)) ctxt1 g1
    else
      begin
        (** Reset the control data **)
        let ncntrl =
          Data.set_rr_depth
            (Data.set_conds cntrl1 (rr_conds cntrl1))
            (rr_depth cntrl1)
        in
        let ndata = {data1 with cntrl = ncntrl} in
        try
          ((ndata, trm1, plan1)
           >+ (simp_rewrite_tac is_concl plan1 trm (ftag ft))) ctxt1 g1
        with _ -> raise No_change
      end
  in
  let finalize (data, _, _) = (data.cntrl: Data.t) in
  let cntrl2 = Data.add_loopdb cntrl trm in
  let data = mk_match_data cntrl2 (typenv_of goal) (Term.empty_subst())
  in
  try
    (fold_seq (data, trm, mk_skip)
       [
         tac1;
         tac2
       ] >/ finalize) ctxt goal
  with _ -> raise No_change


(*** Derived simplifier functions ***)

(** [simp_prep_tac data ret lbl g]: Prepare goal [g] for simplifying
    formula [lbl].

    Returns [ret = ncontrol] where [ncontrol] is the new control
    recording formulas added/modified by simp_prep_tac

    Currently this does nothing except strip the quantifiers off
    formula [lbl].

    Always succeeds.
*)

let simp_asm_tac ctrl lbl goal =
  (ctrl >+ (specA ~a:lbl // skip)) goal

let simp_concl_tac ctrl lbl ctxt goal =
  (ctrl >+ (specC ~c:lbl // skip)) ctxt goal

let simp_prep_tac ctrl lbl ctxt goal =
  let is_asm =
    try ignore(get_tagged_asm lbl goal); true
    with _ -> false
  in
  if is_asm
  then simp_asm_tac ctrl lbl ctxt goal
  else simp_concl_tac ctrl lbl ctxt goal

(** [cond_prover_tac ctrl tg g]: The tactic used to prove the
    conditions of rewrite rules.

    If ctrl.conds > 0,
    decrement ctrl.conds,
    apply [simp_prep_tac]
    apply [basic_simp_tac].
    apply [Logic.Tactics.trueR] to solve goal
    reset ctrl.conds to original value.

    If not(ctrl.conds > 0) fail.
*)

 let cond_prover_trueC l = Tactics.trueC ~c:l

 let cond_prover_worker_tac ctrl tg ctxt goal =
   let rec main_tac ctrl1 ctxt1 g1 =
     let ctrl2 = Data.set_loopdb ctrl1 (Net.empty())
     in
     alt_data ctrl2
       [
         (fun ctrl3 ->
           fold_seq ctrl3
             [
               (fun ctrl4 -> basic_simp_tac ctrl4 tg);
               (fun ctrl4 -> main_tac ctrl4)
             ]);
         (fun ctrl3 -> (ctrl3 >+ skip))
       ] ctxt1 (g1: Logic.node)
   in
   let finalize ctrl1 = Data.set_loopdb ctrl1 (Data.get_loopdb ctrl)
   in
   (main_tac ctrl >/ finalize) ctxt goal

 let cond_prover_tac ctrl tg ctxt goal =
   let cond_depth = Data.get_cond_depth ctrl in
   if Lib.apply_option (fun i -> i>0) cond_depth true
   then
     let data = Data.dec_cond_depth ctrl
     in
     alt
       [
         Tactics.trueC ~c:(ftag tg);
         apply_tac
           (fold_seq data
              [
                (fun ctrl1 -> simp_prep_tac ctrl1 (ftag tg));
                (fun ctrl1 -> cond_prover_worker_tac ctrl1 tg)
              ])
           (fun _ -> cond_prover_trueC (ftag tg))
       ] ctxt goal
  else fail ~err:No_change ctxt goal

(** [inital_flatten_tac exclude g]: Prepare goal for simplification.

    Flatten all except formulas with tag in [exclude].  Try to prove
    trivial facts. Put conclusions into assumptions (by negation)
*)

let simp_asm_elims =
  [
    (fun l -> Boollib.falseA ~a:l);
    (fun l -> Tactics.negA ~a:l);
    (fun l -> Tactics.conjA ~a:l);
    (fun l -> Tactics.existA ~a:l)
  ]
let simp_concl_elims =
  [
    (fun l -> Tactics.trueC ~c:l);
    (fun l -> Tactics.disjC ~c:l);
    (fun l -> Tactics.allC ~c:l)
  ]

let simp_flatten_asms_tac lst =
  Boollib.asm_elim_rules_tac (simp_asm_elims, []) lst

let simp_flatten_concls_tac lst =
  Boollib.concl_elim_rules_tac ([], simp_concl_elims) lst

let simp_flatten_tac excluded ?f ctxt goal =
  let basic_flatter ctxt1 g1 =
    Boollib.elim_rules_tac (simp_asm_elims, simp_concl_elims) ctxt1 g1
  in
  match f with
    | None -> Boollib.apply_elim_tac basic_flatter ?f ctxt goal
    | Some(l) ->
      let tg = Logic.label_to_tag l (sequent goal)
      in
      if List.exists (Tag.equal tg) excluded
      then skip ctxt goal
      else Boollib.apply_elim_tac basic_flatter ?f ctxt goal

let initial_flatten_tac exclude ctxt goal =
  simp_flatten_tac exclude ctxt goal
