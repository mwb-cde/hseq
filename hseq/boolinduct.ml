(*----
  Name: boolinduct.ml
  Copyright Matthew Wahab 2006-2016
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

(*** Induction tactics ***)

open Lib.Ops
open Tactics

open Boolutil
open Boolbase
open Rewritelib

(*** Tactics ***)

(** [mini_scatter_tac c goal]: Mini scatter tactic for
    induction.

    Scatter conclusion [c], using [falseA], [conjA], [existA],
    [trueC], [implC] and [allC]
*)
let mini_scatter_tac c ctxt goal =
  let asm_rules = [ (fun l -> falseA ~a:l) ] in
  let concl_rules =
    [
      (fun l -> Tactics.trueC ~c:l);
      (fun l -> Tactics.conjC ~c:l)
    ]
  in
  let main_tac ctxt0 g = elim_rules_tac (asm_rules, concl_rules) ctxt0 g
  in
  apply_elim_tac main_tac ~f:c ctxt goal

(** [mini_mp_tac asm1 asm2 goal]: Apply modus ponens to [asm1 =
    A => C] and [asm2 = A] to get [asm3 = C].  info: aformulas=[asm3];
    subgoals = [goal1] Fails if [asm2] doesn't match the assumption of
    [asm1].
*)
let mini_mp_tac asm1 asm2 ctxt goal =
  let tac =
    seq
      [
        implA ~a:asm1
        --
          [
            (?> (fun tinfo ->
              let a1_tag = get_one (Info.aformulas tinfo) in
              let c_tag = get_one (Info.cformulas tinfo) in
              let (_, g_tag) = get_two (Info.subgoals tinfo)
              in
              seq
                [
                  set_changes_tac
                    (Changes.make [g_tag] [a1_tag] [] []);
                  basic ~a:asm2 ~c:(ftag c_tag);
                  (fun ctxt1 -> fail ~err:(error "mini_mp_tac") ctxt1)
                ]));
            skip
          ]
      ]
  in
  tac ctxt goal

(*** The induction tactic [induct_tac] ***)

(** [induct_tac_bindings tyenv scp aterm cterm]: Extract bindings for
    the induction theorem in [aterm] from conclusion term [cterm] in
    type environment [tyenv] and scope [scp].

    [aterm] is in the form [(vars, asm, concl)], obtained by
    splitting a theorem of the form [! vars : asm => concl].

    Returns an updated type environment and a substitution containing
    bindings for the variables in [vars], with which to instantiate the
    induction theorem.

    This function is specialized for use by [induct_tac].
*)
let induct_tac_bindings typenv scp aterm cterm =
  (** Split the induction theorem *)
  let (thm_vars, thm_asm, thm_concl) = aterm in
  let is_thm_var = Rewrite.is_free_binder thm_vars in
  (** Split the theorem conclusion (the hypothesis) *)
  let (hyp_vars, hyp_asm, hyp_concl) = dest_qnt_implies thm_concl in
  (** Split the property application *)
  let prop_fun, prop_args = Term.get_fun_args hyp_concl in
  (** Split the target conclusion *)
  let (concl_vars, concl_asm, concl_concl) = dest_qnt_implies cterm
  in
  (**
     Unify [hyp_asm] (= [pred x .. y]) and [concl_asm] (= [pred a
     .. b]) to get the bindings for the [hyp_vars]
  *)
  let is_hyp_var x =
    (Rewrite.is_free_binder hyp_vars x) || (is_thm_var x)
  in
  let (typenv1, hyp_var_bindings) =
    try Unify.unify_fullenv scp typenv (Term.empty_subst())
          is_hyp_var hyp_asm concl_asm
    with err ->
      raise (add_error "Can't unify induction theorem with formula" err)
  in
  (** Eta abstract [concl_concl] w.r.t the constants for [hyp_vars] *)
  let hyp_var_constants = Tactics.extract_consts hyp_vars hyp_var_bindings
  in
  let thm_var_constants = Tactics.extract_consts thm_vars hyp_var_bindings
  in
  (** [concl_concl_eta = (% a1 .. an: (! v1 .. vn: C))] and
      [concl_concl_args = [a; .. ; b]] where [v1 .. vn] are the
      variables ([f .. g]) that don't appear in [pred a .. b]. *)
  let (concl_concl_eta, concl_conl_args) =
    (** Get the variables which don't appear in [concl_asm] *)
    let cvar_set =
      let null_term = Term.mk_free "null" (Gtypes.mk_null()) in
      let form_binders e x =
        if Term.is_bound x
        then Term.bind x null_term e
        else e
      in
      let env1 =
        List.fold_left form_binders (Term.empty_subst()) hyp_var_constants
      in
      List.fold_left form_binders env1 thm_var_constants
    in
    let cvars =
      List.filter
        (fun x -> Term.member (Term.mk_bound x) cvar_set)
        concl_vars
    in
    let concl_concl_trm =
      Lterm.eta_conv hyp_var_constants concl_concl
    in
    close_lambda_app cvars concl_concl_trm
  in
  let (ret_typenv, ret_subst) =
    Unify.unify_fullenv scp typenv1 hyp_var_bindings
      is_hyp_var prop_fun concl_concl_eta
  in
  (ret_typenv, ret_subst)

(** [induct_tac_solve_rh_tac a c goal]: solve the right sub-goal of an
    induction tactic([t2]).

    Formula [a] is of the form [ ! a .. b: A => C ]
    Formula [c] is of the form [ ! a .. b x .. y: A => C]
    or of the form [ ! a .. b: A => (! x .. y : C)]

    Specialize [c], instantiate [a],
    implC [c] to get [a1] and [c1]
    mini_mp_tac [a] and [a2] to replace [a] with [a3]
    specialize [c1] again, intantiate [a3]
    basic [c1] and [a3].

    Completely solves the goal or fails.
*)
let induct_tac_solve_rh_tac a_lbl c_lbl ctxt g =
  let (c_tag, c_trm) =
    let (tg, cf) =
      try get_tagged_concl c_lbl g
      with _ -> failwith "induct_tac_solve_rh_tac: can't get (c_tag, c_trm)"
    in
    (tg, Formula.term_of cf)
  in
  let (c_vars, c_lhs, c_rhs) = dest_qnt_implies c_trm in
  let a_trm =
    try Formula.term_of (get_asm a_lbl g)
    with _ -> failwith "induct_tac_solve_rh_tac: can't get a_trm"
  in
  let (a_vars, a_lhs, a_rhs) = dest_qnt_implies a_trm in
  let a_varp = Rewrite.is_free_binder a_vars
  in
  seq
    [
      (specC ~c:c_lbl
       // (set_changes_tac (Changes.make [] [] [c_tag] [])));
      (?> (fun inf1 ctxt0 g0 ->
        let const_list =
          extract_consts a_vars (unify_in_goal a_varp a_lhs c_lhs g0)
        in
        (instA ~a:a_lbl const_list ++
           (?> (fun inf2 ->
             (implC ~c:c_lbl ++
             (?> (fun inf3 ->
               set_changes_tac (Changes.combine inf3 inf2))))))) ctxt0 g0));
      (?> (fun inf1 ->
        let a1_tag, a_tag = get_two (Info.aformulas inf1)
        in
        (mini_mp_tac (ftag a_tag) (ftag a1_tag) ++
           (?> (fun inf2 ->
             let c1_tag = get_one (Info.cformulas inf1)
             and a3_tag = get_one (Info.aformulas inf2)
             in
             ((specC ~c:(ftag c1_tag) // skip) ++
                 (set_changes_tac
                    (Changes.make [] [a3_tag] [c1_tag] []))))))));
      (?> (fun inf1 ctxt1 g1 ->
        let c1_tag = get_one (Info.cformulas inf1)
        and a3_tag = get_one (Info.aformulas inf1) in
        let c1_lbl = ftag c1_tag in
        let c1_trm =
          try Formula.term_of (get_concl c1_lbl g1)
          with _ -> failwith "induct_tac_solve_rh_tac: can't get c1_trm"
        in
        let a3_lbl = ftag a3_tag in
        let a3_trm =
          try Formula.term_of (get_asm a3_lbl g1)
          with _ -> failwith "induct_tac_solve_rh_tac: can't get a3_trm"
        in
        let (a3_vars, a3_body) = Term.strip_qnt Basic.All a3_trm in
        let a3_varp = Rewrite.is_free_binder a3_vars in
        let const_list =
          try
            extract_consts a3_vars (unify_in_goal a3_varp a3_body c1_trm g1)
          with err ->
            raise (add_error
                     "induct_tac_solve_rh_tac: can't get const_list"
                     err)
        in
        seq
          [
            (instA ~a:a3_lbl const_list // skip);
            basic ~a:a3_lbl ~c:c1_lbl;
            (fun g2 -> fail ~err:(error "induct_tac_solve_rh_goal") g2)
          ] ctxt1 g1))
    ] ctxt g

let asm_induct_tac alabel clabel ctxt goal =
  let typenv = typenv_of goal
  and scp = scope_of_goal goal
  in
  (** Get the theorem and conclusion *)
  let (atag, aform) = get_tagged_asm alabel goal
  and (ctag, cform) = get_tagged_concl clabel goal
  in
  (** Get theorem and conclusion as terms *)
  let aterm = Formula.term_of aform
  and cterm = Formula.term_of cform
  in
  (** Split the induction theorem *)
  let (thm_vars, thm_asm, thm_concl) = dest_qnt_implies aterm
  in
  (** Get the bindings for the outer-most theorem variables *)
  let (consts_typenv, consts_subst) =
    induct_tac_bindings typenv scp (thm_vars, thm_asm, thm_concl) cterm
  in
  let consts_list = Tactics.extract_consts thm_vars consts_subst in
  (** tinfo: information built up by the tactics. *)
  (*  let tinfo = info_make() in *)
  (** [inst_split_asm_tac]: Instantiate and split the assumption.
      tinfo: aformulas=[a1]; cformulas=[c1]; subgoals = [t1; t2]

      Goals:
      {L
      [a, asms |- c, concls]  (a = ! .. : a1 => c)
      ---->
      t1: [asms |- c1, c, concls];
      t2: [a1, asms |- c, concls]
      }
  *)
  let inst_split_asm_tac g =
    let albl = ftag atag in
    seq
      [
        instA ~a:albl consts_list;
        (betaA ~a:albl // skip);
        implA ~a:albl
      ] g
  in
  (** [split_lh_tac c]: Split conclusion [c] of the left-hand
      subgoal.  *)
  let split_lh_tac c ctxt0 g = (mini_scatter_tac c // skip) ctxt0 g
  in
  (** the Main tactic *)
  let main_tac ctxt0 g =
    seq
      [
        inst_split_asm_tac
        --
          [
            (** Left-hand sub-goal *)
            (?> (fun tinfo ->
              let c1_tag = get_one ~msg:"asm_induct_tac.main_tac:1"
                (Info.cformulas tinfo)
              in
              seq [
                deleteC (ftag ctag);
                split_lh_tac (ftag c1_tag)
              ]));
            (** Right-hand sub-goal *)
            (?> (fun tinfo ->
              let a1_tag = get_one ~msg:"asm_induct_tac.main_tac:2"
                (Info.aformulas tinfo)
              in
              seq
                [
                  (specC // skip);
                  induct_tac_solve_rh_tac
                    (ftag a1_tag) (ftag ctag)
                ]))
          ]
      ] ctxt0 g
  in
  main_tac ctxt goal

(** [basic_induct_tac c thm]: Apply induction theorem [thm] to
    conclusion [c].

    See {!Induct.induct_tac}.
*)
let basic_induct_tac c thm ctxt goal =
  let main_tac c_lbl ctxt0 g =
    seq
      [
        cut thm;
        (?> (fun tinfo ->
          let a_tag = get_one ~msg:"basic_induct_tac" (Info.aformulas tinfo)
          in
          asm_induct_tac (ftag a_tag) c_lbl))
      ] ctxt0 g
  in
  main_tac c ctxt goal

(** [induct_tac ?c thm]: Apply induction theorem [thm] to conclusion
    [c] (or the first conclusion to succeed).

    Theorem [thm] must be in the form:
    {L ! P a .. b : (thm_asm P a .. b) => (thm_concl P a .. b)}
    where
    {L
    thm_concl P d .. e = (! x .. y : (pred x .. y) => (P d .. e x .. y))
    }
    The order of the outer-most bound variables is not relevant.

    The conclusion must be in the form:
    {L ! a .. b f .. g: (pred a .. b) => (C a .. b f ..g) }

    info:
    cformulas=the new conclusions (in arbitray order).
    subgoals=the new sub-goals (in arbitray order).
*)
let induct_tac ?c thm ctxt goal =
  let one_tac x ctxt0 g =
    try basic_induct_tac x thm ctxt0 g
    with err ->
      raise (add_error "induct_tac: applying basic_induct_tac failed " err)
  in
  let all_tac targets ctxt0 g =
    try
      map_first (fun l -> basic_induct_tac l thm)
        targets ctxt0 g
    with err ->
      raise (error ("induct_tac: failed to apply induction to"
                    ^" any formula in the conclusions."))
  in
  match c with
    | Some(x) -> one_tac x ctxt goal
    | _ ->
      begin
        let targets =
          List.map (ftag <+ drop_formula) (concls_of (sequent goal))
        in
        all_tac targets ctxt goal
      end


(*** The induct-on tactic [induct_on]. ***)

(** [get_binder qnt n trm]: Get the top-most [qnt] binder with name
    [n] in term [trm]. Raises [Not_found] if the no top-most [qnt]
    binder named [n] can be found.
*)
let get_binder qnt n trm =
  let qnt = Basic.All in
  let rec get_aux t =
    match t with
      | Basic.Qnt(b, body) ->
        if (Basic.binder_kind b) = qnt
        then
          if (Basic.binder_name b) = n
          then b
          else get_aux body
        else raise Not_found
      | _ -> raise Not_found
  in
  get_aux trm

(** [induct_thm ?thm scp tyenv trm]: Get the induction theorem for
    [trm].  If [?thm] is given, return [thm].  Othewise, get the
    theorem named "TY_induct" where [TY] is the type of [trm].

    e.g. if [trm] has type [bool], the induction theorem is
    [bool_induct] and if [trm] has type [('a, 'b)PAIR], the induction
    theorem is [PAIR_induct].
*)
let induct_thm ctxt ?thm scp tyenv trm =
  match thm with
    | Some x -> x
    | None ->
      begin
        let ty =
          let sb = Typing.settype scp ~env:tyenv trm
          in
          Gtypes.mgu (Typing.typeof scp ~env:tyenv trm) sb
        in
        let (th, id) = Ident.dest (get_type_name ty) in
        let thm_name = id^"_induct"
        in
        try Commands.thm ctxt (Ident.string_of (Ident.mk_long th thm_name))
        with _ ->
          (try Commands.thm ctxt thm_name
           with _ -> failwith ("Can't find cases theorem "^thm_name))
      end

(** [induct_on_bindings tyenv scp nbind aterm cterm]: Extract bindings
    for the induction theorem in [aterm] from conclusion term [cterm]
    in type environment [tyenv] and scope [scp], to induct on term
    [Bound nbind].

    [aterm] is in the form [(vars, asm, concl)], obtained by splitting
    a theorem of the form [! vars : asm => concl].

    [cterm] is in the form [! xs : body].

    [nbind] must be a universally quantified binder.

    Tries to unify [body] and [concl], returns an updated type
    environment and a substitution containing bindings for the
    variables in [vars], with which to instantiate the induction
    theorem.

    This function is specialized for use by [induct_on].
*)
let induct_on_bindings typenv scp nbind aterm cterm =
  let nterm = Term.mk_bound nbind in
  (** Split the induction theorem *)
  let (thm_vars, thm_asm, thm_concl) = aterm in
  let is_thm_var = Rewrite.is_free_binder thm_vars in
  (** Split the theorem conclusion (the hypothesis) *)
  let (hyp_vars, hyp_body) = Term.strip_qnt Basic.All thm_concl in
  (** Split the property application *)
  let prop_fun, prop_args = Term.get_fun_args hyp_body in
  (** Split the target conclusion *)
  let (concl_vars, concl_body) = Term.strip_qnt Basic.All cterm in
  (** eta abstract the conclusion body, wrt [nbind], close the
      resulting term.  *)
  let (concl_body_eta, concl_concl_args) =
    let concl_concl_trm = Lterm.eta_conv [nterm] concl_body in
    let cvars = [ nbind ]
    in
    close_lambda_app cvars concl_concl_trm
  in
  let (ret_typenv, ret_subst) =
    try Unify.unify_fullenv scp typenv (Term.empty_subst())
          is_thm_var prop_fun concl_body_eta
    with err ->
      raise (add_error "Can't unify induction theorem with formula" err)
  in
  (ret_typenv, ret_subst)

(** [induct_on_solve_rh_tac a c goal]: solve the right sub-goal
    of an induction tactic([t2]).

    Formula [a] is of the form [ ! a .. b: C ].
    Formula [c] is of the form [ ! a .. b x .. y: C].

    Specialize [c], instantiate [a], basic [a] and [c].

    Completely solves the goal or fails.
*)
let induct_on_solve_rh_tac a_lbl c_lbl ctxt goal =
  let (c_tag, c_trm) =
    let (tg, cf) = get_tagged_concl c_lbl goal in
    (tg, Formula.term_of cf)
  in
  let (c_vars, c_body) = Term.strip_qnt Basic.All c_trm
  and a_trm = Formula.term_of (get_asm a_lbl goal) in
  let (a_vars, a_body) = Term.strip_qnt Basic.All a_trm in
  let a_varp = Rewrite.is_free_binder a_vars
  in
  seq
    [
      (specC ~c:c_lbl
       // set_changes_tac (Changes.make [] [] [c_tag] []));
      (?> (fun inf1 ctxt1 g1 ->
        let const_list =
          extract_consts a_vars (unify_in_goal a_varp a_body c_body g1)
        in
        (instA ~a:a_lbl const_list ++
           (?> (fun inf2 ->
             let c_tag = get_one (Info.cformulas inf2)
             and a_tag = get_one (Info.aformulas inf2)
             in
             basic ~a:(ftag a_tag) ~c:(ftag c_tag)))) ctxt1 g1))
    ] ctxt goal

(** [induct_on ?thm ?c n]: Apply induction to the first universally
    quantified variable named [n] in conclusion [c] (or the first
    conclusion to succeed). The induction theorem is [thm], if given or
    the theorem [thm "TY_induct"] where [TY] is the name of the type
    constructor of [n].

    Theorem [thm] must be in the form:
    {L ! P a .. b : (thm_asm P a .. b) => (thm_concl P a .. b)}
    where
    {L
    thm_concl P a .. b= (! x : (P x a .. b))
    }
    The order of the outer-most bound variables is not relevant.

    The conclusion must be in the form:
    {L ! n f .. g: (C n f ..g) }
    [n] does not need to be the outermost quantifier.
*)

let basic_induct_on ?thm name clabel ctxt goal =
  let typenv = typenv_of goal
  and scp = scope_of_goal goal
  in
  (** Get the conclusion *)
  let (ctag, cform) = get_tagged_concl clabel goal in
  (** Get conclusion as a term *)
  let cterm = Formula.term_of cform in
  (** Get the top-most binder named [name] in [cterm] Fail if not
      found.  *)
  let nbinder =
    try get_binder Basic.All name cterm
    with _ ->
      raise (Term.term_error
               ("No quantified variable named "^name^" in term")
               [cterm])
  in
  let nterm = Term.mk_bound nbinder in
  (** Get the theorem *)
  let thm = induct_thm ctxt ?thm scp typenv nterm in
  let thm_term = Logic.term_of thm in
  (** Split the induction theorem *)
  let (thm_vars, thm_asm, thm_concl) = dest_qnt_implies thm_term in
  (** Get the bindings for the outer-most theorem variables *)
  let (consts_typenv, consts_subst) =
    induct_on_bindings typenv scp
      nbinder (thm_vars, thm_asm, thm_concl) cterm
  in
  let consts_list = Tactics.extract_consts thm_vars consts_subst in
  (** tinfo: information built up by the tactics. *)
  (** [inst_split_asm_tac]: Instantiate and split the assumption.
      tinfo: aformulas=[a1]; cformulas=[c1]; subgoals = [t1; t2].

      Goals:
      {L
      [a, asms |- c, concls]  (a = ! .. : a1 => c)
      ---->
      t1: [asms |- c1, c, concls]; t2: [a1, asms |- c, concls]
      }
  *)
  let inst_split_asm_tac atag g =
    let albl = ftag atag in
    seq
      [
        ((instA ~a:albl consts_list)
         ++ (betaA ~a:albl // skip));
        implA ~a:(ftag atag)
      ] g
  in
  (** [split_lh_tac c]: Split conclusion [c] of the left-hand
      subgoal.  *)
  let split_lh_tac c = (mini_scatter_tac c // skip)
  in
  (** the Main tactic *)
  let main_tac ctxt0 g =
    seq
      [
        cut thm;
        (?> (fun inf1 ->
          let atag = get_one (Info.aformulas inf1) in
          (inst_split_asm_tac atag
           --
             [
               (** Left-hand sub-goal *)
               (?> (fun inf2 ->
                 (deleteC (ftag ctag) ++
                 (fun g3 ->
                   let c1_tag = get_one (Info.cformulas inf2) in
                   split_lh_tac (ftag c1_tag) g3))));
               (** Right-hand sub-goal *)
               (?> (fun inf2 ->
                 let a1_tag = get_one (Info.aformulas inf2) in
                 seq
                   [
                     (specC // skip);
                     induct_on_solve_rh_tac
                       (ftag a1_tag) (ftag ctag)
                   ]))
             ])))
      ] ctxt0 g
  in
  main_tac ctxt goal

let induct_on ?thm ?c n ctxt goal =
  match c with
    | Some(x) -> basic_induct_on ?thm n x ctxt goal
    | _ ->
      begin
        let targets =
          List.map (ftag <+ drop_formula) (concls_of (sequent goal))
        in
        let main_tac =
          map_first
            (fun l -> basic_induct_on ?thm n l)
            targets
        in
        try main_tac ctxt goal
        with _ -> raise (error "induct_on: Failed")
      end
