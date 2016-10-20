(*----
  Name: RelationScript.ml
  Copyright Matthew Wahab 2005-2016
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
  define <:def< empty x y = false >>;;

let inv_def =
  define <:def< inv R x y = R y x >>;;

let refl_def =
  define <:def< refl R = ! x: R x x >>;;

let sym_def =
  define <:def< sym R = ! x y: (R x y) => (R y x) >>;;

let trans_def =
  define <:def< trans R = ! x y z: ((R x y) & (R y z)) => (R x z) >>;;

let antisym_def =
  define <:def< antisym R = ! x y: ((R x y) & (R y x)) => (x = y) >>;;

let equiv_rel_def =
  define <:def< equiv_rel R = (refl R) & (sym R) & (trans R) >>;;

let partial_order_def =
  define <:def< partial_order R = (refl R) & (trans R) & (antisym R) >>;;

let total_order_def =
  define
    <:def<
  total_order R = (partial_order R) & (! x y: (R x y) | (R y x))
    >>;;

let refl_closure_def =
  define <:def< RC R x y = ((x = y) | (R x y)) >>;;

let refl_trans_closure_def =
  define
    <:def<
  RTC R a b =
  !P:
    ((! x: (P x x))
       & (! x y z: ((R x y) & (P y z)) => (P x z)))
    => (P a b)
    >>;;

let trans_closure_def =
  define
    <:def<
  TC R a b =
  !P:
    ((! x y: (R x y) => (P x y))
       & (! x y z: ((R x y) & (P y z)) => (P x z)))
    => (P a b)
    >>;;


let tc_rules =
  theorem "tc_rules"
    <<
    (!R x y: (R x y) => (TC R x y))
    &
    (!R x y z: ((R x y) & (TC R y z)) => (TC R x z))
    >>
    [
      scatter_tac
        ++ once_rewriteC_tac [defn "TC"]
        ++ flatten_tac
        --
        [
          back_tac ++ simp;
          (match_asm << TC R X Y >>
             (fun l ->
                once_rewrite_tac [defn "TC"]
                ++ instA ~a: l[ << _P >> ]))
          ++ simpA []
          ++ (match_asm << ! x y z: X >> liftA)
          ++ instA [ << _x >> ; << _y >>; << _z >> ]
          ++ back_tac
          ++ simp
        ]
    ];;

let tc_rule1=
  theorem "tc_rule1"
    << (!R x y: (R x y) => (TC R x y)) >>
    [
      simp_tac [thm "tc_rules"]
    ];;

let tc_rule2=
  theorem "tc_rule2"
    << (!R x y z: ((R x y) & (TC R y z)) => (TC R x z)) >>
    [ cut (thm "tc_rules") ++ conjA ++ basic ];;

let tc_induct =
  theorem "tc_induct"
    <<
    !P R:
    ((! x y: (R x y) => (P x y))
     &
     (! x y z: ((R x y) & (P y z)) => (P x z)))
  =>
  (! x y: (TC R x y) => (P x y))
    >>
  [
    flatten_tac ++ once_rewrite_tac [defn "TC"]
    ++ (match_asm << !P: X => (P _x _y) >> liftA)
    ++ back_tac
    ++ simp_all []
  ];;

let tc_cases0 =
  prove
  <<
    ! R x y:
    (TC R x y) => ((R x y) | ? z: (R x z) & (TC R z y))
  >>
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
            match_concl << ?z: (_R _x z) & (TC _R z _z) >> (nameC "c");
            instC ~c:(!$ "c") [ << _y >> ];
            simp_tac [tc_rule1]
          ];
        (* 3 *)
        seq
          [
            instC [ << _y >> ];
            show << TC _R _y _z >>
              (cut ~inst:[ << _R >>; << _y >> ; << _z1 >>; << _z >> ]
                 tc_rule2
                 ++ simp);
            simp
          ]
      ]
  ]);;


let tc_cases =
  theorem "tc_cases"
  <<
    ! R x y:
    (TC R x y) = ((R x y) | ? z: (R x z) & (TC R z y))
  >>
  [
    flatten_tac ++ equals_tac ++ iffC
    --
      [
        (* 1 *)
        simp_tac [tc_cases0];
        (* 2 *)
        scatter_tac
        --
          [
            cut_mp_tac (thm "tc_rule1") ++ basic;
            cut ~inst:[ <<_R>>; << _x >>; << _z >>; << _y >> ]
              (thm "tc_rule2")
            ++ back_tac
            ++ simp
          ]
      ]
  ];;

let rtc_rules=
  theorem "rtc_rules"
    <<
    (!R x: (R x x) => (RTC R x x))
    &
    (!R x y z: ((R x y) & (RTC R y z)) => (RTC R x z))
    >>
    [
      scatter_tac
        ++ once_rewriteC_tac [defn "RTC"]
        ++ flatten_tac
        --
        [
          simp;
          (match_asm << RTC R Y Z >>
             (fun l ->
                once_rewrite_tac [defn "RTC"]
                ++ instA  ~a:l [ << _P >> ]))
          ++ (implA ++ (simp // skip))
          ++ (match_asm << ! x y z: P >>
              (fun l -> instA ~a:l [ << _x >> ; << _y >>; << _z >> ]))
          ++ (back_tac ++ simp)
        ]
    ];;


let rtc_rule1=
  theorem "rtc_rule1"
    << (!R x x: (R x x) => (RTC R x x)) >>
    [ simp_tac [thm "rtc_rules"] ];;

let rtc_rule2=
  theorem "rtc_rule2"
    << (!R x y z: ((R x y) & (RTC R y z)) => (RTC R x z)) >>
    [ cut (thm "rtc_rules") ++ conjA ++ basic ];;


let rtc_induct =
  theorem "rtc_induct"
    <<
    !P R:
    ((! x : P x x)
     &
     (! x y z: ((R x y) & (P y z)) => (P x z)))
  =>
  (! x y: (RTC R x y) => (P x y))
    >>
    [
      flatten_tac ++ once_rewrite_tac [defn "RTC"]
      ++ back_tac ++ simp_all []
  ];;


(**
let rtc_cases =
  theorem "rtc_cases"
    <<
    ! R x y:
    (RTC R x y) = ((R x x) | ? z: (R x z) & (RTC R z y))
    >>
    [
      flatten_tac ++ equals_tac ++ iffC
        --
        [
          simp_tac [rtc_cases0]
          cut (thm "rtc_induct")
          ++ instA [ << % x y: (x = y) | ?z : ((_R x z) & (RTC _R z y)) >>;
                     << _R >> ]
          ++ betaA
          ++ scatter_tac
            --
            [
              simp;
              replace_tac
              ++
              (match_concl << ?z: (_R _x1 x) & X >>
                   liftC)
              ++ instC [ << _y1 >> ]
              ++ back_tac
              ++ simp
              ++ flatten_tac
              ++ instC [ << _y1 >> ]
              ++ simp
              ++ cut rtc_rules
              ++ conjA
              ++ back_tac


              (show << RTC _R _y1 _z >>
                 (cut (thm "rtc_rules") ++ conjA
                  ++ (match_asm << !x y z: P >>
                        (fun l ->
                           instA ~a:l [ << _R >>; << _y1 >> ;
                                        << _z1 >>; << _z>> ]
                        ++ back_tac ~a:l
                        ++ simp))))



              (show << RTC _R _y1 _z >>
                 (cut (thm "rtc_rules") ++ conjA
                  ++ (match_asm << !x y z: P >>
                        (fun l ->
                           instA ~a:l [ << _R >>; << _y1 >> ;
                                        << _z1 >>; << _z>> ]
                        ++ back_tac ~a:l
                        ++ simp))))
              ++ (match_concl << ?z: X & (RTC _R z _z) >>
                   (fun l -> instC ~c:l [ << _y1 >> ]))
              ++ simp;
              mp_tac ++ split_tac ++ simp
            ];
          cut (thm "rtc_rules")
          ++ flatten_tac
          ++ once_rewriteC_tac [defn "RTC"]
          ++ flatten_tac
          ++ (split_tac -- [simp])
          ++ flatten_tac
          ++ (match_asm << !x y z: P >>
                (fun l -> instA ~a:l [ << _x >> ; << _z >> ; << _y >> ]))
          ++ back_tac ++ simp
          ++ (match_asm << RTC R Z Y >>
                (fun l -> once_rewriteA_tac ~f:l [defn "RTC"]))
          ++ back_tac ++ simp
        ]
    ];;
**)

let _ = end_theory ();;
