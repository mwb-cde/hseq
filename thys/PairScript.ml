(*----
  Name: PairScript.ml
  Copyright Matthew Wahab 2005-2017
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
   Product of types.
 *)

let _ = begin_theory "Pair" ["Fun"];;

(* Compile the library and its signature: pairLib.ml *)

let _ = compile [] "pairLib.mli";;
let _ = compile [] "pairLib.ml";;
let _ = add_file ~use:true "pairLib.cmo";;

(** [pair_prec] and [pair_fixity] must agree with the values in pairLib.ml. *)
let pair_prec = 10
let pair_fixity = Printer.infixr

(** {5 Definition and basic properties of pairs} *)

let mk_pair_def =
  define (?<% " mk_pair x y = (%a b: (a=x) and (b=y)) ");;

let is_pair_def =
  define (?<% " is_pair p = (?x y: p = (mk_pair x y)) ");;

let pair_exists =
  prove_thm
    "pair_exists" (!% " ? p: is_pair p ")
    [
      unfold "is_pair";
      inst_tac  [ (!% " mk_pair true true"); (!% "true"); (!% "true") ];
      eq_tac
    ];;

let mk_pair_is_pair =
  prove_thm
    "mk_pair_is_pair" (!% " !x y : is_pair (mk_pair x y) ")
    [
      flatten_tac; unfold "is_pair" ;
      inst_tac [ (!% " _x "); (!% " _y ") ];
      eq_tac
    ];;

let pair_tydef =
  typedef ~pp:(pair_prec, infixr, Some("*"))
          ~thm:pair_exists
          ~rep:"dest_PAIR" ~abs:"make_PAIR"
          (?<: "('a, 'b)PAIR = ('a -> 'b -> bool): is_pair ");;

let pair_def =
  define
    ~pp:(pair_prec, pair_fixity, Some(","))
    (?<% " pair x y = make_PAIR (mk_pair x y) ") ;;

let fst_def =
  define
    (?<% " fst p = (epsilon(% x: ?y: p = (pair x y))) ");;

let snd_def =
  define
    (?<% " snd p = epsilon(% y: ?x: p = (pair x y)) ");;

let mk_pair_eq =
  prove_thm
    "mk_pair_eq"
    (!% "! a b x y:
         ((mk_pair a b) = (mk_pair x y))
         =
         ((a = x) and (b = y))")
    [flatten_tac ++ equals_tac ++ iffE
     --
       [ (match_asm (!% " X = Y ")
                    (fun l -> once_rewrite_tac
                                ?dir:None [thm "function_eq"] ~f:l))
         ++ inst_tac [ (!% " _a ") ]
         ++ (match_asm (!% " L = R ")
                       (fun l -> once_rewrite_tac
                                   ?dir:None [thm "function_eq"] ~f:l))
         ++ inst_tac [ (!% " _b ") ]
         ++ (match_asm (!% " A = B ")
                       (fun l -> once_rewrite_tac [thm "eq_sym"] ~f:l))
         ++ unfold "mk_pair" ++ beta_tac
         ++ replace_tac
         ++ split_tac ++ eq_tac;
         (* 2 *)
         flatten_tac ++ replace_tac ++ eq_tac]
    ];;

let rep_abs_pair=
  prove_thm
    "rep_abs_pair"
    (!% " !x y: (dest_PAIR(make_PAIR (mk_pair x y))) = (mk_pair x y) ")
    [
      flatten_tac
      ++ cut_thm "mk_pair_is_pair"
      ++ inst_tac [ (!% " _x ") ; (!% " _y ") ]
      ++ cut_mp_tac (thm "make_PAIR_inverse")
      ++ basic
    ];;

let pair_thm =
  prove_thm
    "pair_thm"
    (!% " ! x y: (dest_PAIR (pair x y)) = (mk_pair x y) ")
    [
      flatten_tac ++ unfold "pair"
      ++ cut_thm "epsilon_ax"
      ++ rewrite_tac [thm "rep_abs_pair"]
      ++ eq_tac
    ];;


let inj_on_make_PAIR =
  prove_thm
    "inj_on_make_PAIR"
    (!% " inj_on make_PAIR is_pair ")
    [
      cut_thm "inj_on_inverse_intro"
      ++ inst_tac [ (!% "make_PAIR"); (!% "is_pair"); (!% "dest_PAIR") ]
      ++ cut_thm "make_PAIR_inverse"
      ++ split_tac
      -- [ basic; basic ]
    ];;


(** {5 Properties} *)

let basic_pair_eq =
  prove_thm
    "basic_pair_eq"
    (!% "! a b x y: ((pair a b) = (pair x y)) = ((a = x) and (b = y))")
    [
      flatten_tac
      ++ equals_tac
      ++ iffE
      --
        [
          (* 1 *)
          unfold "pair"
          ++ cut_thm "inj_on_make_PAIR"
          ++ unfold "inj_on"
          ++ inst_tac [ (!% " mk_pair _a _b "); (!% " mk_pair _x _y ") ]
          ++ cut_thm "mk_pair_is_pair"
          ++ inst_tac [ (!% " _a ") ; (!% " _b ") ]
          ++ cut_thm "mk_pair_is_pair"
          ++ inst_tac [ (!% " _x ") ; (!% " _y ") ]
          ++ (implA -- [conjC ++ basic] )
          ++ (implA -- [basic])
          ++ rewrite_tac [thm "mk_pair_eq"]
          ++ basic;
          (* 2 *)
          flatten_tac++ replace_tac ++ eq_tac
        ]
    ];;


let fst_thm =
  prove_thm
    "fst_thm" ~simp:true
    (!% " ! x y: (fst (pair x y)) = x ")
    [
      flatten_tac ++ unfold "fst"
      ++ cut_thm "epsilon_ax"
      ++ inst_tac [ (!% " %a: ?b: (pair _x _y) = (pair a b) ") ]
      ++ beta_tac
      ++ split_tac
      --
        [
          inst_tac [ (!% " _x ") ; (!% " _y ") ] ++ eq_tac;
          flatten_tac
          ++ rewrite_tac [basic_pair_eq]
          ++ flatten_tac
          ++ (replace_tac ~dir:rightleft)
          ++ eq_tac
        ]
    ];;

let snd_thm =
  prove_thm
    "snd_thm" ~simp:true
    (!% " ! x y: (snd (pair x y)) = y ")
    [
      flatten_tac ++ unfold "snd"
      ++ cut_thm "epsilon_ax"
      ++ inst_tac [ (!% " %b: ?a: (pair _x _y) = (pair a b) ") ]
      ++ beta_tac
      ++ split_tac
      --
        [
          inst_tac [ (!% " _y "); (!% " _x ") ] ++ eq_tac;
          flatten_tac
          ++ rewrite_tac [basic_pair_eq]
          ++ flatten_tac
          ++ (replace_tac ~dir:rightleft)
          ++ eq_tac
        ]
    ];;

let pair_inj =
  prove_thm
    "pair_inj"
    (!% " ! p: ?x y: p = (pair x y) ")
    [
      flatten_tac ++ unfold "pair"
      ++ cut ~inst:[ (!% " _p ") ] (thm "dest_PAIR_mem")
      ++ cut ~inst:[ (!% " _p ") ] (thm "dest_PAIR_inverse")
      ++ unfold "is_pair"
      ++ flatten_tac
      ++ inst_tac [ (!% " _x ") ; (!% " _y ") ]
      ++ (match_asm (!% " (dest_PAIR x) = Y ")
                    (fun l -> replace_tac ~dir:rightleft ~asms:[l] ?f:None))
      ++ (match_asm (!% " (make_PAIR (dest_PAIR x)) = Y ")
                    (fun l -> replace_tac ?dir:None ~asms:[l] ?f:None))
      ++ eq_tac
    ];;

let pair_cases =
  save_thm "PAIR_cases" pair_inj

let pair_induct =
  theorem "PAIR_induct"
          (!% " !P: (! x y: (P (x, y))) => (!x: P x) ")
          [
            flatten_tac
            ++ cut pair_cases
            ++ instA [ (!% " _x ") ]
            ++ specA
            ++ replace_tac ++ unify_tac
          ]

let surjective_pairing =
  prove_thm
    "surjective_pairing" ~simp:true
    (!% " !p: (pair (fst p) (snd p)) = p ")
    [
      flatten_tac
      ++ cut ~inst:[ (!% " _p ") ] pair_inj
      ++ flatten_tac
      ++ replace_tac
      ++ rewrite_tac [basic_pair_eq; fst_thm; snd_thm]
      ++ (split_tac ++ eq_tac)
    ];;

let pair_eq =
  prove_thm
    "pair_eq"
    (!% "! p q : (p = q) = (((fst p) = (fst q)) and ((snd p) = (snd q)))")
    [
      flatten_tac
      ++ cut ~inst:[ (!% " fst _p "); (!% " snd _p ") ;
                     (!% " fst _q "); (!% "snd _q ") ] basic_pair_eq
      ++ rewrite_tac [surjective_pairing]
      ++ basic
    ];;

let _ = end_theory();;
