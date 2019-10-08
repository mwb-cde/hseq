(*----
  Name: SetScript.ml
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
   Sets, set operators and their basic properties.
*)

let _ = begin_theory "Set" ["Bool"];;

let _ = compile [] "setLib.mli";;
let _ = compile [] "setLib.ml";;

let _ = add_symbol "{" "{";;
let _ = add_symbol "}" "}";;

let _ = add_file ~use:true "setLib.cmo";;

(***
* Main Definitions
***)

let set_typedef = typedef (?<: "('a) set = ('a -> bool)");;

let set_def =
  define (?<% " SET (A:('a)set) = (A:('a)set) ");;

let empty_def =
  define
  (?<% " (empty:('a)set) = SET(% x: false) ")
  ~pp:(Printkit.default_term_prec, Printkit.default_term_fixity, Some "{}");;

let in_def =
  define
  (?<% " in x (A: ('a)set) = A x ")
      ~pp:(220, infixn, None)

let univ_def =
  define (?<% " univ = { x: true } ");;

let add_def =
  define (?<% " add x A = {y: (y = x) | (y in A)}");;

let remove_def =
  define (?<% " remove x A = {y: (y in A) & ~(y = x) }");;

let single_def =
  define (?<% " single x  = {y: (y = x)}");;

let union_def =
  define (?<% " union A B = {x: (x in A) | (x in B) } ") ;;

let unions_def =
  define (?<% " Union A = {x: ? B: B in A | x in B } ");;

let inter_def =
  define (?<% " inter A B = {x: (x in A) & (x in B) } ");;

let inters_def =
  define (?<% " Inter A = {x: ? B: B in A & x in B } ");;

let neg_def =
  define (?<% " neg A = { x: ~(x in A) } ");;

let diff_def =
  define
  (?<% " diff A B = {x: x in A & ~x in B } ")
  ~pp:(230, infixr, Some("/"));;

let subset_def =
  define
  (?<% " subset A B = !x: x in A => (x in B) ")
      ~pp:(225, infixr, Some("<=")) ;;

let psubset_def =
  define
  (?<% " psubset A B = ~(A=B) & (A <= B) ")
  ~pp:(225, infixr, Some("<"));;



(***
* Finite sets
***)
(**
   finite empty;
   [ ~x in A; finite A ] --> finite (add x A);
*)

let finite_def=
  define
  (?<% "finite X =
        !P:
        ((P empty)
        & (! x A: (~(x in A) & (P A)) => (P (add x A))))
        => (P X)
        ");;

let finite_induct =
  theorem "finite_induct"
  (!% "
  !P:
    ((P empty)
     & (! x A: (~(x in A) & (P A)) => (P (add x A))))
  =>
  !A : (finite A) => (P A)
  ")
  [flatten_tac
   ++ unfold "finite"
   ++ back_tac
   ++ blast_tac ++ back_tac ++ blast_tac];;


let finite_rules =
  theorem ~simp:false "finite_rules"
  (!% "
    (finite empty)
  & (!x A: (~(x in A) & (finite A)) => (finite (add x A)))
  ")
    [rewrite_tac [defn "finite"]
     ++ blast_tac
     ++ (match_asm (!% " !P: A => (P x) ")
         (instA_at [ (!% " _P ")]))
     ++ (implA -- [split_tac ++ basic])
     ++ back_tac
     ++ blast_tac];;


(***
* Primitive properties
***)

let set_simp=
  prove_thm ~simp:true "set_simp"
  (!% " !x A: ((SET A) x) = (A x) ")
  [
    flatten_tac
    ++ unfold "SET"
    ++ eq_tac
  ];;

let in_simp =
  prove_thm ~simp:true "in_simp"
  (!% " !x A: (x in (SET(A)))  = (A x) ")
  [
    flatten_tac ++ unfold "in" ++ unfold "SET"
    ++ eq_tac
  ];;

let set_equal =
  prove_thm "set_equal"
  (!% " ! A B: (A = B) = (!x: (x in A) = (x in B)) ")
  [
    flatten_tac ++ equals_tac ++ scatter_tac
    --
      [
        simp [];
        cut_back_tac (thm "extensionality")
        ++ simp_all [defn "in"]
      ]
  ];;

let set_cases =
  theorem "SET_cases"
    (!% " ! A : (A = empty) | ? x: x in A ")
     [
       scatter_tac
       ++ once_rewriteC_tac [set_equal]
       ++ flatten_tac ++ equals_tac ++ scatter_tac
       --
         [
           (* 1 *)
           unify_tac;
           (* 2 *)
           simpA [defn "empty"]
         ]
     ];;

let add_thm =
  theorem "add_thm"
    (!% " ! x y A : (y in (add x A)) = ((y = x) | (y in A)) ")
  [ flatten_tac ++ simp [defn "add"] ];;

let remove_thm =
  theorem "remove_thm"
    (!% " ! x y A: (y in (remove x A)) = ((y in A) & ~(y = x)) ")
  [ flatten_tac ++ simp [ defn "remove" ] ];;

let neg_thm =
  theorem ~simp:true "neg_thm"
    (!% " ! x A: (x in (neg A)) = ~(x in A) ")
  [ simp [defn "neg"] ];;

let union_thm =
  theorem "union_thm"
  (!% " !x A B: (x in (union A B)) = ((x in A) | (x in B)) ")
  [ simp [defn "union"] ];;

let inter_thm =
  theorem "inter_thm"
  (!% " !x A B: (x in (inter A B)) = ((x in A) & (x in B)) ")
  [ simp [defn "inter"] ];;

let subset_thm =
  theorem "subset_thm"
    (!% " ! A B: A <= B = (!x: x in A => x in B) ")
   [ simp [defn "subset"] ]

let psubset_thm =
  theorem "psubset_thm"
    (!% " ! A B: A < B = (~(A=B) & (A<=B)) ")
   [ simp [defn "psubset"] ];;


(***
* Membership
***)

let not_in_empty =
  prove_thm ~simp:true "not_in_empty"
  (!% " !x: not (x in {}) ")
    [
      unfold "empty"
      ++ simp []
      ++ flatten_tac
    ];;

let in_univ=
  prove_thm ~simp:true "in_univ"
  (!% " !x: (x in univ) ")
    [ simp [defn "univ"] ];;

let in_single =
  prove_thm ~simp:true "in_single"
  (!% " !x a: (x in (single a)) = (x = a) ")
  [ simp [defn "single" ] ];;

let in_add =
  prove_thm ~simp:true "in_add"
  (!% "
    (!x a S: (x = a) => (x in (add a S)))
    &
    (!x a S: ~(x = a) => (x in (add a S) = (x in S)))
  ")
  [
    split_tac
    --
      [
        (* 1 *)
        flatten_tac ++ simp [defn "add" ];
        (* 2 *)
        seq
          [
            specC ++ implC;
            simp [defn "add" ]
          ]
      ]
  ]

let in_remove=
  theorem ~simp:true "in_remove"
  (!% "
    (!x  A: ~(x in (remove x A)))
  & (!x y A: ~(x=y) => ((x in (remove y A)) = (x in A)))
  ")
  [
    scatter_tac -- [ simp_all [remove_thm] ; simp [remove_thm]]
  ];;


(*** Properties of Add *)

let add_member =
  prove_thm ~simp:true "add_member"
  (!% " !x A: (x in A) => ((add x A) = A) ")
  [
    flatten_tac
    ++ once_rewrite_tac [set_equal]
    ++ simp [add_thm]
    ++ equals_tac ++ scatter_tac ++ simp []
  ];;

let add_remove =
  theorem ~simp:true "add_remove"
    (!% " !x A: (add x (remove x A)) = (add x A) ")
  [
    simp [set_equal]
    ++ simp [add_thm; remove_thm]
    ++ equals_tac
    ++ blast_tac
  ]

(** Properties of Remove *)

let remove_member =
  theorem ~simp:true "remove_member"
    (!% " ! x A: ~(x in A) => ((remove x A) = A) ")
  [
    simp [set_equal] ++ simp [remove_thm]
      ++ flatten_tac
      ++ equals_tac ++ blast_tac
      ++ simp_all []
  ]

let remove_add =
  theorem ~simp:true "remove_add"
    (!% " !x A: (remove x (add x A)) = (remove x A) ")
  [
    simp [set_equal] ++ simp [add_thm; remove_thm]
    ++ equals_tac ++ blast_tac
  ]

(** Properties of neg *)

let neg_univ =
  theorem ~simp:true "neg_univ"
    (!% " (neg univ) = empty ")
  [ once_rewrite_tac [thm "set_equal"] ++ simp [] ];;

let neg_empty =
  theorem ~simp:true "neg_empty"
    (!% " (neg empty) = univ ")
  [ once_rewrite_tac [thm "set_equal"] ++ simp [] ];;

let neg_union =
  theorem ~simp:true "neg_union"
    (!% " ! A B : (neg (union A B)) = (inter (neg A) (neg B)) ")
  [
    once_rewrite_tac [thm "set_equal"]
    ++ simp [defn "union"; defn "inter"]
  ];;

let neg_inter =
  theorem ~simp:true "neg_inter"
    (!% " ! A B : (neg (inter A B)) = (union (neg A) (neg B)) ")
  [
    once_rewrite_tac [thm "set_equal"]
    ++ simp [defn "union"; defn "inter"]
  ];;


(** Union **)

let union_assoc =
    theorem ~simp:true "union_assoc"
      (!% " ! A B C: (union A (union B C)) = (union (union A B) C) ")
  [
    simp [set_equal; union_thm]
    ++ equals_tac ++ blast_tac
  ];;

let union_comm =
  theorem ~simp:true "union_comm"
    (!% " ! A B: (union A B) = (union B A) ")
  [
    simp [set_equal; union_thm]
    ++ equals_tac ++ blast_tac
  ];;

let union_lcomm =
  theorem ~simp:true "union_lcomm"
    (!% " ! A B C: (union A (union B C)) = (union B (union A C)) ")
  [
    simp [set_equal; union_thm]
    ++ equals_tac ++ blast_tac
  ];;

let union_trivial =
  theorem ~simp:true "union_trivial"
    (!% " ! A: (union A A) = A ")
  [
    simp [set_equal; union_thm]
    ++ equals_tac ++ blast_tac
  ];;

let union_absorb =
  theorem ~simp:true "union_absorb"
    (!% "
    (! A: (union A empty) = A)
    & (! A: (union empty A) = A)
    & (! A: (union A univ) = univ)
    & (! A: (union univ A) = univ)
    ")
    [
      split_tac ++ simp [set_equal; union_thm]
      ++ equals_tac ++ blast_tac
    ];;

let union_add_left =
  theorem "union_add_left"
  (!% " ! a S T: (union (add a S) T) = (add a (union S T)) ")
  [
    simp [set_equal]; simp [union_thm; add_thm]
    ++ equals_tac ++ blast_tac
  ];;

let union_add_right =
  theorem "union_add_right"
  (!% " ! a S T: (union S (add a T)) = (add a (union S T)) ")
  [
    simp [set_equal]; simp [union_thm; add_thm]
    ++ equals_tac ++ blast_tac
  ]

(** Intersection *)

let inter_assoc =
  theorem ~simp:true "inter_assoc"
  (!% " ! A B C: (inter A (inter B C)) = (inter (inter A B) C) ")
  [
    simp [set_equal; inter_thm]
    ++ equals_tac ++ blast_tac
  ];;

let inter_comm =
  theorem ~simp:true "inter_comm"
    (!% " ! A B: (inter A B) = (inter B A) ")
  [
    simp [set_equal; inter_thm]
    ++ equals_tac ++ blast_tac
  ];;

let inter_lcomm =
  theorem ~simp:true "inter_lcomm"
    (!% " ! A B C: (inter A (inter B C)) = (inter B (inter A C)) ")
  [
    simp [set_equal; inter_thm]
    ++ equals_tac ++ blast_tac
  ];;

let inter_absorb =
  theorem ~simp:true "inter_absorb"
    (!% "
    (! A: (inter A empty) = empty)
  & (! A: (inter empty A) = empty)
  & (! A: (inter A univ) = A)
  & (! A: (inter univ A) = A)
    ")
    [
      split_tac
      ++ simp [set_equal; inter_thm]
      ++ equals_tac ++ blast_tac
    ];;

let inter_trivial =
  theorem "inter_trivial"
    (!% " ! A: (inter A A) = A ")
  [
    simp [set_equal; inter_thm]
    ++ equals_tac ++ blast_tac
  ];;

(** Subset *)

let subset_cases =
  theorem "subset_cases"
    (!% " ! A B : (A <= B) => ((A=B) | ?x: (x in B) & ~(x in A)) ")
  [
    flatten_tac
    ++ simp_all [subset_thm; set_equal]
    ++ instC [ (!% " _x ") ]
    ++ equals_tac ++ scatter_tac ++ simp []
  ]

let subset_trival =
  theorem ~simp:true "subset_trivial"
  (!% " ! A: (A <= A) = true ")
  [ unfold "subset" ++ flatten_tac ++ equals_tac ++ blast_tac ];;

let subset_absorb =
theorem ~simp:true "subset_absorb"
  (!% "
  (! A: (empty <= A) = true)
  & (! A: (A <= univ) = true)
  ")
  [unfold "subset" ++ split_tac ++ simp [] ++ equals_tac ++ blast_tac];;

let subset_refl =
  theorem ~simp:true "subset_refl"
  (!% " ! A : A <= A ")
    [ unfold "subset" ++ flatten_tac ++ basic]

let subset_trans =
  theorem "subset_trans"
    (!% " !A B C: ((A <= C) & (C <= B)) => (A <= B) ")
    [
      unfold "subset" ++ flatten_tac
      ++ simp []
    ]

let subset_antisym =
  theorem "subset_antisym"
    (!% " ! A B: ((A <= B) & (B<=A)) => (A = B) ")
  [
    simp [subset_thm]
    ++ flatten_tac
    ++ once_rewrite_tac [set_equal]
    ++ flatten_tac ++ equals_tac ++ scatter_tac
    ++ simp []
  ]

let subset_empty =
  theorem ~simp:true "subset_empty"
    (!% " ! A: (A <= {}) = (A = {}) ")
  [
    flatten_tac ++ equals_tac ++ iffC
      --
    [
      (* 1 *)
      once_rewrite_tac [thm "set_equal"]
        ++ flatten_tac
        ++ unfold "subset"
        ++ equals_tac ++ iffC ++ scatter_tac
        -- [simp []; simp_all []];
      (* 2 *)
      flatten_tac ++ simp []
    ]
  ]

let subset_add =
  theorem "subset_add" ~simp:true
  (!% " ! x S T: ~(x in S) => ((S <= (add x T)) = (S <= T)) ")
  [
    flatten_tac
      ++ equals_tac
      ++ scatter_tac
      ++ (unfold "subset")
      ++ flatten_tac
      ++ mp_tac
      ++ (unfold "add")
      ++ simp_all []
      --
      [
        (* 1 *)
        (match_asm (!% " X | Y ") liftA)
        ++ split_tac ++ simp_all [];
        (* 2 *)
        simp []
      ]
  ]

let subset_add_remove =
  theorem "subset_add_remove"
    (!% " ! x A B: (A <= (add x B)) = ((remove x A) <= B) ")
  [
    flatten_tac
    ++ equals_tac ++ scatter_tac
      --
      [
        (* 1 *)
        simp_all [subset_thm]
        ++ flatten_tac
        ++ rewrite_tac [remove_thm]
        ++ flatten_tac
        ++ mp_tac
        ++ rewrite_tac [add_thm]
        ++ blast_tac;
        (* 2 *)
        simp_all [subset_thm]
        ++ flatten_tac
        ++ rewrite_tac [add_thm]
        ++ flatten_tac
        ++ back_tac
        ++ simp_all [remove_thm]
      ]
  ]

let subset_remove =
  theorem "subset_remove" ~simp:true
    (!% " ! x S: (remove x S) <= S ")
    [
      unfold "subset" ++ scatter_tac
        ++ simpA [remove_thm]
    ]

let subset_inter =
  theorem "subset_inter" ~simp:true
    (!% "
    (! A B : (inter A B) <= A)
    & (! A B : (inter A B) <= B)
    ")
    [ simp [subset_thm; inter_thm] ++ blast_tac ]

let subset_psubset =
  theorem "subset_psubset"
    (!% " ! A B: (A<=B) = ((A<B) | (A = B)) ")
  [
    simp_all [psubset_thm]
    ++ equals_tac ++ blast_tac
    ++ simp []
  ]

let subset_member =
  theorem "subset_member"
    (!% " ! x A B: ((x in A) & (A <= B)) => (x in B) ")
  [
    simp [subset_thm]
    ++ flatten_tac
    ++ mp_tac
    ++ simp_all []
  ]

(** Proper subset *)

let psubset_cases =
  theorem "psubset_cases"
    (!% " ! A B : (A < B) => (?x: (x in B) & ~(x in A)) ")
  [
    flatten_tac
    ++ simp_all [psubset_thm; subset_thm; set_equal]
    ++ scatter_tac
    ++ equals_tac ++ scatter_tac
    -- [simp []; instC [ (!% " _x ") ] ++ blast_tac]
  ]

let psubset_empty =
  theorem "psubset_empty" ~simp:true
    (!% " ! A : ~(A < {}) ")
    [
      simp [psubset_thm]
      ++ scatter_tac ++ simp []
    ]

let psubset_subset =
  theorem "psubset_subset"
    (!% " !A B : (A < B) => (A <= B) ")
  [
    simp [defn "subset"; defn "psubset"]
    ++ scatter_tac
    ++ simp []
  ]

let psubset_irrefl =
  theorem "psubset_irrefl" ~simp:true
    (!% " ! A : ~(A < A) ")
    [
      simp [defn "psubset"]
    ]

let psubset_trans =
  theorem "psubset_trans"
  (!% " ! A B C: ((A < C) & (C < B)) => (A < B) ")
  [
    simp [psubset_thm] ++ blast_tac
    --
      [
        (* 1 *)
        replace_tac []
        ++ cut_back_tac subset_antisym
        ++ simp [];
        (* 2 *)
        cut [ (!% " _A "); (!% " _B "); (!% " _C ") ] subset_trans
        ++ blast_tac
      ]
  ]


let psubset_remove =
  theorem "psubset_remove" ~simp:true
    (!% " !x A: (x in A) => ((remove x A) < A) ")
  [
    simp [defn "psubset"]
      ++ implC
      ++ once_rewriteC_tac [set_equal]
      ++ simp [remove_thm]
      ++ scatter_tac
      ++ instA [ (!% " _x ") ]
      ++ once_rewrite_tac [thm "equals_bool"]
      ++ iffA ++ simpA[]
  ]

let psubset_add =
  theorem "psubset_add" ~simp:true
    (!% " !x A: ~(x in A) => (A < (add x A)) ")
  [
    simp [defn "psubset"]
      ++ flatten_tac
      ++ once_rewriteC_tac [set_equal]
      ++ simpC [add_thm]
      ++ scatter_tac
      ++ once_replace_tac []
      ++ flatten_tac
      ++ eq_tac
  ]

let psubset_add_subset =
  theorem "psubset_add_subset"
    (!% " ! x A B : ((~x in A) & (A < (add x B))) => (A <= B) ")
  [
    blast_tac
    ++ simp_all [psubset_thm]
    ++ blast_tac
    ++ once_rewriteC_tac [set_equal]
    ++ flatten_tac
    ++ equals_tac ++ blast_tac
    ++ simpA [subset_thm]  ++ mp_tac ++ basic
  ]

let psubset_member =
  theorem "psubset_member"
    (!% " ! x A B: ((x in A) & (A < B)) => (x in B) ")
  [
    flatten_tac
    ++ (show (!% " _A <= _B ") (simp [psubset_subset]))
    ++ cut [ (!% " _x "); (!% " _A "); (!% " _B ") ] subset_member
    ++ simp []
  ]

(** Finite *)

let finite_empty =
  theorem ~simp:true "finite_empty"
  (!% " (finite {}) ")
    [simp [finite_rules]]

let finite_add =
  theorem ~simp:true "finite_add"
  (!% " !x A: (finite A) => (finite (add x A)) ")
  [
    flatten_tac;
    (cases_tac (!% " _x in _A ")
       --
       [
         (* 1 *)
         seq
           [
             cut [] finite_rules ++ conjA;
             back_tac;
             simp []
           ];
         (* 2 *)
         simp []
       ])
  ];;

let finite_union0 =
  prove
  (!% " ! A B : (finite A) => (finite B) => (finite (union A B)) ")
  (induct_tac (thm "finite_induct") ++ flatten_tac
      --
      [
        (* 1 *)
        simp [];
        (* 2 *)
        seq
          [
            rewrite_tac [union_add_left];
            simp [finite_add]
          ]
      ])

let finite_union =
  theorem ~simp:true "finite_union"
  (!% " ! A B : ((finite A) & (finite B)) => (finite (union A B)) ")
  [
    flatten_tac;
    cut_mp_tac ~inst:[ (!% " _A "); (!% " _B ") ] finite_union0;
    simp []
  ]

let finite_subset =
  theorem "finite_subset"
  (!% " ! A : (finite A) => (!B: (B <= A) => (finite B)) ")
  [
    induct_tac finite_induct
   ++ flatten_tac
   --
   [
     (* 1 *)
     simp_all [];
     (* 2 *)
     simp_all [subset_add_remove]
     ++ mp_tac
     ++ cut_mp_tac ~inst:[ (!% " _x "); (!% " remove _x _B ") ] finite_add
     ++ simpA []
     ++ cases_tac (!% " _x in _B ")
     ++ simpA [remove_member; add_member]
   ]
  ]

let finite_subset_back =
  theorem "finite_subset_back"
  (!% " ! B : (?A: (finite A) & (B <= A)) => (finite B) ")
  [
    flatten_tac
    ++ cut [] finite_subset
    ++ mp_tac ++ mp_tac
    ++ basic
  ]

let finite_psubset =
  theorem "finite_psubset"
  (!% " ! A : (finite A) => (!B: (B < A) => (finite B)) ")
  [
    flatten_tac
    ++ rewriteA_tac [psubset_thm]
    ++ scatter_tac
    ++ cut [] finite_subset
    ++ mp_tac ++ mp_tac
    ++ basic
  ]

let finite_psubset_back =
  theorem "finite_psubset_back"
  (!% " ! B : (?A: (finite A) & (B < A)) => (finite B) ")
  [
    flatten_tac
    ++ cut [] finite_psubset
    ++ mp_tac ++ mp_tac
    ++ basic
  ]

let finite_inter =
  theorem "finite_inter" ~simp:true
    (!% " ! A B: ((finite A) | (finite B)) => (finite (inter A B)) ")
  [
    scatter_tac
    --
      [
        (* 1 *)
        cut_back_tac finite_subset_back
        ++ instC [ (!% " _A ") ];
        (* 2 *)
        cut_back_tac finite_subset_back
        ++ instC [ (!% " _B ") ];
      ]
    ++ cut [] subset_inter
    ++ blast_tac
    ++ unify_tac
  ]


(** Finite set induction properties *)

(**
let finite_induct =
  theorem "finite_induct"
  (!% "
    !P:
    ((P empty)
     & (! x A: (~(x in A) & (finite A) & (P A)) => (P (add x A))))
  =>
  !A : (finite A) => (P A)
  ")
  [
    specC
      ++ cut [ (!% " (%x: (finite x) & (_P x)) ") ] finite_induct
      ++ betaA
      ++ cut finite_rules
      ++ blast_tac ++ ((back_tac ++ blast_tac) // skip)
      ++
      (match_asm (!% " ! X : (finite X) => P ")
          (fun l -> (instA ~a:l [ (!% " _A ") ] ++ blast_tac ~f:l)))
  ]
**)

let _ = end_theory();;

let _ = Display.print_theory (theory "");;
