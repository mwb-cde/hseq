(*----
  Name: boollib.ml
  Copyright Matthew Wahab 2006-2019
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

open Boolutil
open Boolbase
open Rewritelib
open Commands
open Tactics
open Lib.Ops

let term = BoolPP.read

module Thms =
struct

  open Booltacs

  (** {5 Theorems}

      Theorems about boolean operators which may be needed by tactics.
  *)

  let false_def = Boolbase.false_def

  let iff_def = Booltacs.iff_def
  (** [make_n_thm()]: prove theorem n [n_thm()]: get theorem n,
      proving it if necessary *)

  (** [equals_iff_thm]: |- !x y: (x iff y) = (x = y) *)
  let make_equals_iff_thm (sctxt: Context.t) =
    let iff_l2 =
      Commands.prove sctxt
        (term "!x y: ((x => y) and (y => x)) => (x = y)")
      (seq [
        allC;
        (?> (fun (info1: Info.t) ->
          seq [
            allC ;
            (?> (fun info2 ->
              let x_term = Lib.get_one (Info.constants info1)
                (Failure "make_equals_iff_thm: x_term")
              and y_term = Lib.get_one (Info.constants info2)
                (Failure "make_equals_iff_thm: y_term")
              in
              (flatten_tac
               ++ (cut_thm [] "bool_cases" ++ allA x_term)
               ++ (cut_thm [] "bool_cases" ++ allA y_term)
               ++ split_tac
               ++
                 alt
                 [(replace_tac ++ (basic // trivial));
                  (basic // trivial);
                  (replace_tac ++ eq_tac)])))
          ]))])
    in
    Commands.prove sctxt (term "!x y: (x iff y) = (x = y)")
      (seq [
        allC;
        (?> (fun info1 ->
          seq [
            allC;
            (?> (fun info2 ->
              let x_term = Lib.get_one (Info.constants info1)
                (Failure "make_equals_iff_thm: x_term")
              and y_term = Lib.get_one (Info.constants info2)
                (Failure "make_equals_iff_thm: y_term")
              in
              ((cut [] iff_l2)
               ++ inst_tac [Lterm.mk_iff x_term y_term;
                            Lterm.mk_equality x_term y_term]
               ++ split_tac
               --
                 [
                   flatten_tac
                   ++ cut [] iff_l2 ++ inst_tac [x_term; y_term]
                   ++ unfold "iff" ~f:(!~2)
                   ++ (implA --  [basic; basic]);
                   flatten_tac
                   ++ replace_tac
                   ++ unfold "iff" ~f:(!! 1)
                   ++ split_tac ++ flatten_tac ++ basic;
                   replace_tac ++ eq_tac])))
          ]))])

(*
  let equals_iff_thm_var = Lib.freeze make_equals_iff_thm
  let equals_iff_thm() = Lib.thaw ~fresh:fresh_tthm equals_iff_thm_var
*)
  let equals_iff_id = Ident.mk_long "Bool" "equals_iff"
  let equals_iff_thm ctxt =
    Context.find_thm ctxt equals_iff_id make_equals_iff_thm

  (** [equals_bool_thm]: |- !x y: (x = y) = (x iff y) *)
  let equals_bool_id = Ident.mk_long "Bool" "equals_bool"
  let make_equals_bool_thm sctxt =
    get_or_prove sctxt (Ident.string_of equals_bool_id)
    (term "!x y: (x = y) = (x iff y)")
      (flatten_tac
       ++ (rewrite_tac [equals_iff_thm sctxt])
       ++ eq_tac)

(*
  let equals_bool_thm_var = Lib.freeze make_equals_bool_thm
  let equals_bool_thm() = Lib.thaw ~fresh:fresh_thm equals_bool_thm_var
*)
  let equals_bool_thm ctxt =
    Context.find_thm ctxt equals_bool_id make_equals_bool_thm

  (** [bool_eq_thm]: |- !x y: x = y = ((x => y) and (y=>x)) *)
  let bool_eq_thm_id = Ident.mk_long "Bool" "bool_eq"
  let make_bool_eq_thm ctxt =
    prove ctxt (term "!x y: (x = y) = ((x => y) and (y => x))")
    (flatten_tac
     ++ rewrite_tac [equals_bool_thm ctxt]
     ++ unfold "iff"
     ++ (split_tac ++ flatten_tac
         ++ split_tac ++ flatten_tac ++ basic))

  let bool_eq_thm ctxt =
    Context.find_thm ctxt bool_eq_thm_id make_bool_eq_thm

  (** [double_not_thm]: |- ! x: x = (not (not x)) *)
  let double_not_id = Ident.mk_long "Bool" "double_not"
  let make_double_not_thm ctxt =
    Commands.prove ctxt (term "!x: x = (not (not x))")
    (flatten_tac ++ rewrite_tac [bool_eq_thm ctxt]
     ++ scatter_tac ++ basic)

  let double_not_thm ctxt =
    Context.find_thm ctxt double_not_id make_double_not_thm

  (** [rule_true_thm]: |- !x: x = (x=true) *)
  let rule_true_id = Ident.mk_long "Bool" "rule_true"
  let make_rule_true_thm ctxt =
    let rule_true_l1 =
      Commands.prove ctxt (term "!x: (x=true) => x")
      (flatten_tac ++ replace_tac ++ trivial)
    in
    let rule_true_l2 =
      Commands.prove ctxt (term "!x: x => (x=true)")
      (allC
       ++ (?> (fun info g ->
         let x_term =
           Lib.get_one (Info.constants info) (Failure "rule_true_l2")
         in
         (flatten_tac
          ++ (cut_thm [] "bool_cases")
          ++ (allA x_term)
          ++ disjA
          --
            [basic;
             rewrite_tac [Commands.thm ctxt "false_def"]
             ++ replace_tac ++ negA ++ trueC]) g)))
    in
    let rule_true_l3 =
      Commands.prove ctxt (term "! x: x iff (x=true)")
        ((flatten_tac ++ unfold "iff" ~f:(!! 1) ++ conjC)
         --
           [cut [] rule_true_l2 ++ unify_at (!~1) (!! 1);
            cut [] rule_true_l1 ++ unify_at (!~1) (!! 1)])
    in
    rewrite_rule ctxt [equals_iff_thm ctxt] rule_true_l3

(*
  let rule_true_thm_var = Lib.freeze make_rule_true_thm
  let rule_true_thm() = Lib.thaw ~fresh:fresh_thm rule_true_thm_var
*)
  let rule_true_thm ctxt =
    Context.find_thm ctxt rule_true_id make_rule_true_thm

  (** rule_false_thm: !x: (not x) = (x=false) *)
  let rule_false_id = Ident.mk_long "Bool" "rule_false"
  let make_rule_false_thm ctxt =
    Commands.prove ctxt (term "! x: (not x) = (x=false)")
        (allC
         ++ (?> (fun info g ->
           let x_term =
             Lib.get_one (Info.constants info)
               (Failure "make_rule_false_thm")
           in
           ((once_rewrite_tac [equals_bool_thm ctxt]
             ++ unfold "iff"
             ++ scatter_tac)
            --
              [
                cut_thm [] "bool_cases" ++ inst_tac [x_term]
                ++
                  (split_tac
                   ++ replace_tac
                   ++ (trivial // eq_tac));
                replace_tac ++ trivial]) g)))

(*
  let rule_false_thm_var = Lib.freeze make_rule_false_thm
  let rule_false_thm() = Lib.thaw ~fresh:fresh_thm rule_false_thm_var
*)
  let rule_false_thm ctxt =
    Context.find_thm ctxt rule_false_id make_rule_false_thm

  let bool_cases_thm = Boolbase.bool_cases_thm
  let cases_thm = Booltacs.cases_thm

  let eq_refl_thm = Boolbase.eq_refl_thm
  let eq_sym_thm = Boolbase.eq_sym_thm

end


(** Conversions on boolean operators.
*)
module Convs=
struct

  open Thms

  (** [neg_all_conv]: |- (not (!x..y: a)) = ?x..y: not a *)
  let neg_all_conv sctxt trm =
    if not (Lterm.is_neg trm)
    then failwith "neg_all_conv: not a negation"
    else
      let (_, trmbody) = Term.dest_unop trm in
      let (aqvars, aqbody) = Term.strip_qnt Term.All trmbody
      in
      begin
        match aqvars with
          | [] ->
            failwith
              "neg_all_conv: body of negation is not universally quantified"
        | _ -> ()
      end;
      let eqvars =
        List.map
          (fun b ->
            let (_, n, ty) = Term.Binder.dest b in
            Term.Binder.make Term.Ex n ty)
          aqvars
      in
      let eqbody =
        let nsubst =
          List.fold_left2
            (fun s l r -> Term.Subst.bind l r s)
            (Term.Subst.empty())
            (List.map Term.mk_bound aqvars)
            (List.map Term.mk_bound eqvars)
        in
        Term.subst nsubst aqbody
      in
      let newterm =
        Term.rename (Term.rebuild_qnt eqvars (Lterm.mk_not eqbody))
      in
      let goal_term = Lterm.mk_equality trm newterm in
      let proof ctxt g =
        let sctxt1 = set_scope ctxt (scope_of_goal g) in
        seq [once_rewrite_tac [bool_eq_thm sctxt1] ~f:(fnum 1);
             Tactics.conjC_at (fnum 1)
             --
               [
                 seq
                   [
                     Tactics.implC_at (fnum 1);
                     (?> (fun info ->
                      let atag =
                        Lib.get_one (Info.aformulas info)
                          (Failure "neg_all_conv: 1")
                      and ctag =
                        Lib.get_one (Info.cformulas info)
                          (Failure "neg_all_conv: 1")
                      in
                      seq
                        [
                          Tactics.negA_at (ftag atag);
                          (?> (fun info ->
                            let ctag2 =
                              Lib.get_one (Info.cformulas info)
                                (Failure "neg_all_conv: 2")
                            in
                            seq
                              [
                                repeat
                                  (?> (fun info ->
                                   (Tactics.allC_at (ftag ctag2)
                                    ++ append_changes_tac info)));
                                (?> (fun info ->
                                  instC_at (List.rev (Info.constants info))
                                     (ftag ctag)));
                                Tactics.negC_at (ftag ctag);
                                (?> (fun info ->
                                  let atag3 =
                                    Lib.get_one (Info.aformulas info)
                                      (Failure "neg_all_conv: 3")
                                  in
                                  Tactics.basic_at
                                    (ftag atag3) (ftag ctag2)))
                              ]))]))];
                 seq
                   [
                     Tactics.implC_at (fnum 1);
                     (?> (fun info ->
                       let atag =
                         Lib.get_one (Info.aformulas info)
                         (Failure "neg_all_conv: 4")
                      and ctag =
                        Lib.get_one (Info.cformulas info)
                          (Failure "neg_all_conv: 4")
                      in
                      seq
                        [
                          Tactics.negC_at (ftag ctag);
                          (?> (fun info ->
                            let atag2 =
                              Lib.get_one (Info.aformulas info)
                                (Failure "neg_all_conv: 2")
                            in
                            seq
                              [
                                repeat
                                  (?> (fun info ->
                                    (Tactics.existA_at (ftag atag)
                                    ++ append_changes_tac info)));
                                (?> (fun info ->
                                  instA_at
                                    (List.rev (Info.constants info))
                                    (ftag atag2)));
                                Tactics.negA_at (ftag atag);
                                (?> (fun info ->
                                 let ctag3 =
                                   Lib.get_one (Info.cformulas info)
                                     (Failure "neg_all_conv: 3")
                                 in
                                 Tactics.basic_at
                                   (ftag atag2) (ftag ctag3)))
                              ]))]))]]
            ] sctxt g
      in
      Commands.prove sctxt goal_term proof

  (** [neg_exists_conv]: |- (not (?x..y: a)) = !x..y: not a *)
  let neg_exists_conv sctxt trm =
    if not (Lterm.is_neg trm)
    then failwith "neg_exists_conv: not a negation"
    else
      let (_, trmbody) = Term.dest_unop trm in
      let (eqvars, eqbody) = Term.strip_qnt Term.Ex trmbody in
      begin
        match eqvars with
          | [] ->
            failwith
              "neg_all_conv: body of negation is not universally quantified"
          | _ -> ()
      end;
      let aqvars =
        List.map
          (fun b ->
            let (_, n, ty) = Term.Binder.dest b
            in Term.Binder.make Term.All n ty)
          eqvars
      in
      let aqbody =
        let nsubst =
          List.fold_left2
            (fun s l r -> Term.Subst.bind l r s)
            (Term.Subst.empty())
            (List.map Term.mk_bound eqvars)
            (List.map Term.mk_bound aqvars)
        in
        Term.subst nsubst eqbody
      in
      let newterm =
        Term.rename (Term.rebuild_qnt aqvars (Lterm.mk_not aqbody))
      in
      let goal_term = Lterm.mk_equality trm newterm in
      let proof ctxt g =
        let sctxt1 = set_scope ctxt (scope_of_goal g) in
        seq [once_rewrite_tac [bool_eq_thm sctxt1] ~f:(fnum 1);
             Tactics.conjC_at (fnum 1)
             --
               [
                 seq
                   [
                     Tactics.implC_at (fnum 1);
                     (?> (fun info ->
                      let atag =
                        Lib.get_one (Info.aformulas info)
                          (Failure "neg_exists_conv: 1")
                      and ctag =
                        Lib.get_one (Info.cformulas info)
                          (Failure "neg_exists_conv: 1")
                      in
                      seq
                        [
                          Tactics.negA_at (ftag atag);
                          (?> (fun info->
                            let ctag2 =
                              Lib.get_one (Info.cformulas info)
                                (Failure "neg_all_conv: 2")
                            in
                            seq
                              [
                                repeat
                                  (?> (fun info ->
                                    (Tactics.allC_at (ftag ctag)
                                     ++ append_changes_tac info)));
                                (?> (fun info g3 ->
                                  instC_at
                                    (List.rev (Info.constants info))
                                    (ftag ctag2) g3));
                                Tactics.negC_at (ftag ctag);
                                (?> (fun info g3 ->
                                  let atag3 =
                                    Lib.get_one (Info.aformulas info)
                                      (Failure "neg_exists_conv: 3")
                                  in
                                  Tactics.basic_at
                                    (ftag atag3) (ftag ctag2) g3))
                              ]))]))];
                 seq
                   [
                     Tactics.implC_at (fnum 1);
                     (?> (fun info g1 ->
                      let atag =
                        Lib.get_one (Info.aformulas info)
                          (Failure "neg_exists_conv: 4")
                      and ctag =
                        Lib.get_one (Info.cformulas info)
                          (Failure "neg_exists_conv: 4")
                      in
                      seq
                        [
                          Tactics.negC_at (ftag ctag);
                          (?> (fun info g2->
                            let atag2 =
                              Lib.get_one (Info.aformulas info)
                                (Failure "neg_exists_conv: 2")
                            in
                            seq
                              [
                                repeat
                                  (?> (fun info ->
                                    (Tactics.existA_at (ftag atag2)
                                     ++ append_changes_tac info)));
                               (?> (fun info g3 ->
                                 instA_at
                                   (List.rev (Info.constants info))
                                   (ftag atag) g3));
                                Tactics.negA_at (ftag atag);
                                (?> (fun info g3 ->
                                  let ctag3 =
                                    Lib.get_one (Info.cformulas info)
                                      (Failure "neg_exists_conv: 3")
                                  in
                                  Tactics.basic_at
                                    (ftag atag2) (ftag ctag3) g3))
                              ] g2))] g1))]]
            ] sctxt g
      in
      Commands.prove sctxt goal_term proof

end

(** Functions to construct theorems from other theorems. *)
module Rules=
struct
  (** [once_rewrite_rule scp rules thm]: rewrite [thm] with [rules]
      once.  *)
  let once_rewrite_rule scp rules thm =
    let ctrl = { Rewrite.default with Rewrite.depth = Some(1) }
    in
    rewrite_rule scp ~ctrl:ctrl rules thm

  (** [conjunctL scp thm] Get the left hand side of conjunct [thm].
      [conjunctL scp << l and r >> = l] *)
  let conjunctL ctxt thm =
    let trm = Logic.term_of thm in
    if not (Lterm.is_conj trm)
    then raise (error "conjunct1: not a conjunction")
    else
      let (_, lhs, rhs) = Term.dest_binop trm in
      let proof l g =
        seq [Tactics.cut [] thm;
             (?> (fun info g1 ->
               let ttag =
                 Lib.get_one (Info.aformulas info)
                   (error "conjunctL")
               in
               Tactics.conjA_at (ftag ttag) g1));
             (?> (fun info g1 ->
               let (ltag, rtag)=
                 Lib.get_two (Info.aformulas info)
                   (error "conjunctL")
               in
               Tactics.basic_at (ftag ltag) l g1))] g
      in
      Commands.prove ctxt lhs (proof (fnum 1))

  (** [conjunctR scp thm] Get the right hand side of conjunct [thm].
      [conjunctL scp << l and r >> = r] *)
  let conjunctR ctxt thm =
    let trm = Logic.term_of thm in
    if not (Lterm.is_conj trm)
    then raise (error "conjunct1: not a conjunction")
    else
      let (_, lhs, rhs) = Term.dest_binop trm in
      let proof l g =
        seq [Tactics.cut [] thm;
             (?> (fun info g1 ->
               let ttag =
                 Lib.get_one (Info.aformulas info)
                   (error "conjunctL")
               in
               Tactics.conjA_at (ftag ttag) g1));
             (?> (fun info g1 ->
               let (ltag, rtag)=
                 Lib.get_two (Info.aformulas info)
                   (error "conjunctL")
               in
               Tactics.basic_at (ftag rtag) l g1))] g
      in
      Commands.prove ctxt rhs (proof (fnum 1))

  (** [conjuncts scp thm] break theorem [thm] into the list of
      conjuncts.  [conjuncts scp << f1 and f2 and .. and fn>> = [f1;
      f2; ..; fn]] *)
  let conjuncts ctxt thm =
    let is_conj_thm thm = Lterm.is_conj (Logic.term_of thm) in
    let rec conjuncts_aux scp thm result =
      if not (is_conj_thm thm)
      then thm::result
      else
        let lhs = conjunctL scp thm
        and rhs = conjunctR scp thm in
        let result1=conjuncts_aux scp rhs result
        in
        conjuncts_aux ctxt lhs result1
    in
    conjuncts_aux ctxt thm []

end

(*** Tactics ***)

let falseA_at  = Boolbase.falseA_at
let falseA  = Boolbase.falseA

let trivial_at = Boolbase.trivial_at
let trivial = Boolbase.trivial
let cut_thm = Boolbase.cut_thm

(*** Basic equality reasoning ***)

let eq_sym_rule = Boolbase.eq_sym_rule
let eq_symA = Boolbase.eq_symA
let eq_symC = Boolbase.eq_symC
let eq_sym_tac = Boolbase.eq_sym_tac
let eq_tac = Boolbase.eq_tac
let eq_at = Boolbase.eq_at

(*** Rewriting ***)

let rewrite_conv = Rewritelib.rewrite_conv
let rewrite_rule = Rewritelib.rewrite_rule
let gen_rewrite_tac = Rewritelib.gen_rewrite_tac
let rewrite_tac = Rewritelib.rewrite_tac
let once_rewrite_tac = Rewritelib.once_rewrite_tac
let rewriteC_tac = Rewritelib.rewriteC_tac
let once_rewriteC_tac = Rewritelib.once_rewriteC_tac
let rewriteA_tac = Rewritelib.rewriteA_tac
let once_rewriteA_tac = Rewritelib.once_rewriteA_tac
let gen_replace_tac = Rewritelib.gen_replace_tac
let replace_tac = Rewritelib.replace_tac
let once_replace_tac = Rewritelib.once_replace_tac
let unfold = Rewritelib.unfold

(*** Eliminating boolean operators ***)

let direct_alt = Boolbase.direct_alt
let direct_map_some = Boolbase.direct_map_some

let asm_elim_rules_tac = Boolbase.asm_elim_rules_tac
let concl_elim_rules_tac = Boolbase.concl_elim_rules_tac
let apply_elim_tac = Boolbase.apply_elim_tac
let elim_rules_tac = Boolbase.elim_rules_tac
let apply_elim_tac = Boolbase.apply_elim_tac

(*** Boolean equivalence ***)

let iffA = Booltacs.iffA
let iffC = Booltacs.iffC
let iffE = Booltacs.iffE
let iffE_at = Booltacs.iffE_at

(*** Splitting formulas ***)

let split_asms_tac = Booltacs.split_asms_tac
let split_concls_tac = Booltacs.split_concls_tac
let split_at = Booltacs.split_at
let split_tac = Booltacs.split_tac

(*** Flattening formulas. ***)

let flatter_asms_tac = Booltacs.flatter_asms_tac
let flatter_concls_tac = Booltacs.flatter_concls_tac
let flatten_at = Booltacs.flatten_at
let flatten_tac = Booltacs.flatten_tac

(*** Scattering formulas ***)

let scatter_at = Booltacs.scatter_at
let scatter_tac = Booltacs.scatter_tac

(*** Scattering, solving formulas ***)

let blast_at = Booltacs.blast_at
let blast_tac = Booltacs.blast_tac

(*** Cases ***)

let cases_tac = Booltacs.cases_tac
let show_tac = Booltacs.show_tac
let show = Booltacs.show
let cases_of = Booltacs.cases_of

(*** Modus Ponens ***)

let mp_tac = Booltacs.mp_tac
let cut_mp_tac = Booltacs.cut_mp_tac

let back_tac = Booltacs.back_tac
let cut_back_tac = Booltacs.cut_back_tac

(*** Equality ***)

let equals_tac ?f ctxt goal =
  let thm =
    try Thms.equals_bool_thm ctxt
    with Not_found ->
      (raise (error "Can't find required lemma Bool.equals_bool"))
  in
  let sctxt = set_scope ctxt (scope_of_goal goal) in
  let act_tac x ctxt0 g = once_rewrite_tac [thm] ~f:x sctxt g in
  let main_tac ctxt0 gl =
    match f with
      | Some x -> act_tac x ctxt0 goal
      | _ ->
        let test_tac (tg, form) ctxt1 g =
          if Formula.is_equality form
          then act_tac (ftag tg) ctxt1 g
          else fail (error "equals_tac") ctxt1 g
        in
        let sqnt = sequent gl
        in
        ((map_first test_tac (concls_of sqnt))
         // map_first test_tac (asms_of sqnt)) ctxt0 gl
  in
  try main_tac sctxt goal
  with err -> raise (add_error "equals_tac: Failed" err)


(** Induction tactics *)

(*** induct_tac ***)

let asm_induct_tac = Boolinduct.asm_induct_tac
let induct_at = Boolinduct.induct_at
let induct_tac = Boolinduct.induct_tac

(*** induct_on ***)
let induct_on = Boolinduct.induct_on
