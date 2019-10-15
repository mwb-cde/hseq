(*----
  Name: RelationScript.ml
  Copyright Matthew Wahab 2005-2019
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

(**
   Relations
 *)

let _ = begin_theory "Relation" ["Bool"]

(** {5 Definitions} *)

let empty =
  define [] (?<% " empty x y = false ");;

let inv_def =
  define [] (?<% " inv R x y = R y x ");;

let refl_def =
  define [] (?<% " refl R = ! x: R x x ");;

let sym_def =
  define [] (?<% " sym R = ! x y: (R x y) => (R y x) ");;

let trans_def =
  define [] (?<% " trans R = ! x y z: ((R x y) & (R y z)) => (R x z) ");;

let antisym_def =
  define [] (?<% " antisym R = ! x y: ((R x y) & (R y x)) => (x = y) ");;

let equiv_rel_def =
  define [] (?<% " equiv_rel R = (refl R) & (sym R) & (trans R) ");;

let partial_order_def =
  define [] (?<% " partial_order R = (refl R) & (trans R) & (antisym R) ");;

let total_order_def =
  define []
    (?<% "total_order R = (partial_order R) & (! x y: (R x y) | (R y x))");;

let refl_closure_def =
  define [] (?<% " RC R x y = ((x = y) | (R x y)) ");;

let refl_trans_closure_def =
  define []
    (?<% "RTC R a b =
          !P: ((! x: (P x x))
          & (! x y z: ((R x y) & (P y z)) => (P x z)))
          => (P a b)");;

let trans_closure_def =
  define []
    (?<% "TC R a b =
          !P:
          ((! x y: (R x y) => (P x y))
          & (! x y z: ((R x y) & (P y z)) => (P x z)))
          => (P a b)");;


let tc_rules =
  theorem
    "tc_rules"
    (!% "(!R x y: (R x y) => (TC R x y))
         &
         (!R x y z: ((R x y) & (TC R y z)) => (TC R x z))")
    [
      scatter_tac
      ++ once_rewriteC_tac [defn "TC"]
      ++ flatten_tac
      --
        [
          back_tac ++ simp [];
          (match_asm (!% " TC R X Y ")
                     (fun l ->
                       once_rewrite_tac [defn "TC"]
                       ++ instA_at [ (!% " _P ") ] l))
          ++ simpA []
          ++ (match_asm (!% " ! x y z: X ") liftA)
          ++ instA [ (!% " _x ") ; (!% " _y "); (!% " _z ") ]
          ++ back_tac
          ++ simp []
        ]
    ];;

let tc_rule1=
  theorem
    "tc_rule1"
    (!% " (!R x y: (R x y) => (TC R x y)) ")
    [
      simp [thm "tc_rules"]
    ];;

let tc_rule2=
  theorem
    "tc_rule2"
    (!% " (!R x y z: ((R x y) & (TC R y z)) => (TC R x z)) ")
    [ cut [] (thm "tc_rules") ++ conjA ++ basic ];;

let tc_induct =
  theorem
    "tc_induct"
    (!% "
         !P R:
         ((! x y: (R x y) => (P x y))
         &
         (! x y z: ((R x y) & (P y z)) => (P x z)))
         =>
         (! x y: (TC R x y) => (P x y))
         ")
    [
      flatten_tac ++ once_rewrite_tac [defn "TC"]
      ++ (match_asm (!% " !P: X => (P _x _y) ") liftA)
      ++ back_tac
      ++ simp_all []
    ];;

let tc_cases0 =
  prove
    (!% "! R x y:
         (TC R x y) => ((R x y) | ? z: (R x z) & (TC R z y))")
    (seq
       [
         allC;
         induct_tac (thm "tc_induct") ++ scatter_tac
         --
           [
             (* 1 *)
             basic;
             (* 2 *)
             seq
               [
                 match_concl
                   (!% " ?z: (_R _x z) & (TC _R z _z) ")
                   (nameC "c");
                 instC_at [ (!% " _y ") ] (!$ "c");
                 simp [tc_rule1]
               ];
             (* 3 *)
             seq
               [
                 instC [ (!% " _y ") ];
                 show
                   (!% " TC _R _y _z ")
                   (cut [ (!% " _R "); (!% " _y ");
                                (!% " _z1 "); (!% " _z ") ]
                        tc_rule2
                    ++ simp []);
                 simp []
               ]
           ]
    ]);;

let tc_cases =
  theorem
    "tc_cases"
    (!% "! R x y:
         (TC R x y) = ((R x y) | ? z: (R x z) & (TC R z y))")
    [
      flatten_tac ++ equals_tac ++ iffC
      --
        [
          (* 1 *)
          simp [tc_cases0];
          (* 2 *)
          scatter_tac
          --
            [
              cut_mp_tac [] (thm "tc_rule1") ++ basic;
              cut [ (!% "_R"); (!% " _x "); (!% " _z "); (!% " _y ") ]
                  (thm "tc_rule2")
              ++ back_tac
              ++ simp []
            ]
        ]
    ];;

let rtc_rules=
  theorem
    "rtc_rules"
    (!% "(!R x: (R x x) => (RTC R x x))
         &
         (!R x y z: ((R x y) & (RTC R y z)) => (RTC R x z))")
    [
      scatter_tac
      ++ once_rewriteC_tac [defn "RTC"]
      ++ flatten_tac
      --
        [
          simp [];
          (match_asm
             (!% " RTC R Y Z ")
             (fun l ->
               once_rewrite_tac [defn "RTC"]
               ++ instA_at [ (!% " _P ") ] l))
          ++ (implA ++ (simp [] // skip))
          ++ (match_asm
                (!% " ! x y z: P ")
                (fun l ->
                  instA_at
                    [ (!% " _x ") ; (!% " _y "); (!% " _z ") ] l))
          ++ (back_tac ++ simp [])
        ]
    ];;

let rtc_rule1=
  theorem
    "rtc_rule1"
    (!% " (!R x x: (R x x) => (RTC R x x)) ")
    [ simp [thm "rtc_rules"] ];;

let rtc_rule2=
  theorem
    "rtc_rule2"
    (!% " (!R x y z: ((R x y) & (RTC R y z)) => (RTC R x z)) ")
    [ cut [] (thm "rtc_rules") ++ conjA ++ basic ];;

let rtc_induct =
  theorem
    "rtc_induct"
    (!% "
         !P R:
         ((! x : P x x)
         &
         (! x y z: ((R x y) & (P y z)) => (P x z)))
         =>
         (! x y: (RTC R x y) => (P x y))
         ")
    [
      flatten_tac ++ once_rewrite_tac [defn "RTC"]
      ++ back_tac ++ simp_all []
    ];;

let _ = end_theory ();;

let _ = Display.print_theory (theory "");;
