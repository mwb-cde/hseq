(*----
  Name: SumExample.ml
  Copyright Matthew Wahab 2017
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
  Example of linking with the HSeq library.

  Compile with
   hseqc -o sum SumExample.ml
  or
   hseqc --native -o sum.opt SumExample.ml

  This requires that hseq has been properly installed (ie. with 'make install').
 *)

(**
   Open HSeq modules.
 *)

open HSeq
open HSeqUser
open Tactics
open Boollib
open Simplib
open Userlib

(** Initialise the system *)

(** Initialize the system but set the file loader to ignore library files linked
    in by theories. *)
let initialize() =
  Userlib.init();
  Userlib.set_load_file_func (fun  _ -> ()); (* Library files are ignored. *)
  Userlib.add_to_path "../thys"

(**
   Sum of types.
 *)

let sum_example () =
  begin_theory "SumExample" [];
  let sum_prec = 35 in

  (** {5 Definition and basic properties of Sum types} *)

  let _ =
    define
      (?<% "mk_suml v = (% sel l r: ((l = v) & sel)) ");
  in
  let _ =
    define
      (?<% "mk_sumr v = (%sel l r: ((r = v) & not sel))");
  in
  let _ =
    define
      (?<% " is_sum f =
            ?l r: (f = (mk_suml l)) | (f = (mk_sumr r))");
  in
  let sum_exists =
    theorem "sum_exists"
            (!% " ? p: is_sum p ")
            [
              unfold "is_sum"
              ++ inst_tac  [ (!% "mk_suml true "); (!% " true ") ;
                             (!% " true ") ]
              ++ flatten_tac ++eq_tac
            ]
  in
  let _ =
    typedef ~pp:(sum_prec, infixr, Some("+"))
            ~thm:sum_exists
            ~rep:"dest_SUM" ~abs:"make_SUM"
            (?<: "('a, 'b)SUM = (bool -> 'a -> 'b -> bool): is_sum")
  in
  let mk_suml_is_sum =
    theorem "mk_suml_is_sum"
            (!% " ! x : is_sum (mk_suml x) ")
            [flatten_tac
             ++ unfold "is_sum"
             ++ inst_tac [ (!% "_x"); (!% " any ") ]
             ++ flatten_tac ++ eq_tac]
  in
  let mk_sumr_is_sum =
    theorem "mk_sumr_is_sum"
            (!% " ! x : is_sum (mk_sumr x) ")
            [flatten_tac
             ++ unfold "is_sum"
             ++ inst_tac [ (!% " any ") ; (!% "_x") ]
             ++ simpC []]
  in

  let mk_suml_eq =
    theorem
      "mk_suml_eq"
      (!% " !x y: ((mk_suml x) = (mk_suml y)) = (x = y) ")
      [
        flatten_tac ++ unfold "mk_suml" ++ equals_tac ++ iffE
        --
          [
            repeat
              (once_rewriteA_tac [thm "function_eq"]
               ++ beta_tac)
            ++ inst_tac [ (!% " true "); (!% " _x ") ]
            ++ simpA [];
            simpC []
          ]
      ]
  in

  let mk_sumr_eq =
    theorem
      "mk_sumr_eq"
      (!% " !x y: ((mk_sumr x) = (mk_sumr y)) = (x = y) ")
      [
        flatten_tac ++ unfold "mk_sumr" ++ equals_tac ++ iffE
        --
          [
            (match_asm
               (!% " X = Y ")
               (fun l ->
                 repeat
                   (once_rewrite_tac ~f:l [thm "function_eq"]
                    ++ beta_tac ~f:l)
                 ++ inst_tac ~f:l [ (!% " false "); (!% " any ") ;
                                    (!% " _y ") ]
                 ++ simp ~f:l));
            simpC []
          ]
      ]
  in

  (**
   {7 Definitions}

   [inl a]: Inject on the left
   [inr a]: Inject on the right
   [isl a]: [a] is constructed from [inl]
   [isr a]: [a] is constructed from [inr]
   [outl a]: Destructor for [inl]
   [outr a]: Destructor for [inr]
   *)

  let _ = define (?<% " inl a = make_SUM (mk_suml a) ")
  in

  let _ = define (?<% " inr b = make_SUM (mk_sumr b) ")
  in

  let _ = define (?<% " isl a = ?x: a = (inl x) ")
  in

  let _ = define (?<% " isr a = ?x: a = (inr x) ")
  in

  let _ = define (?<% " outl x = (@v: x = (inl v)) ")
  in

  let _ = define (?<% " outr x = (@v: x = (inr v)) ")
  in

  let _ =
    prove (!% " !v: (dest_SUM (inl v)) = (mk_suml v) ")
      (flatten_tac
       ++ unfold "inl"
       ++ simpC_tac [mk_suml_is_sum; thm "make_SUM_inverse"])
  in

  let _ =
    prove (!% " !v: (dest_SUM (inr v)) = (mk_sumr v) ")
      (flatten_tac
       ++ unfold "inr"
       ++ simpC_tac [mk_sumr_is_sum; thm "make_SUM_inverse"])
  in


  let _ =
    theorem "rep_abs_suml"
      (!% " !x: (dest_SUM( make_SUM (mk_suml x))) = (mk_suml x) ")
      [
        flatten_tac
        ++ simpC_tac [thm "mk_suml_is_sum"; thm "make_SUM_inverse"]
      ]
  in

  let _ =
    theorem "rep_abs_sumr"
      (!% " !x: (dest_SUM( make_SUM (mk_sumr x))) = (mk_sumr x) ")
      [
        flatten_tac
        ++ simpC_tac [thm "mk_sumr_is_sum"; thm "make_SUM_inverse"]
      ]
  in

  let _ =
    theorem "inl_thm"
     (!% " ! x: (dest_SUM (inl x)) = (mk_suml x) ")
     [
        flatten_tac ++ unfold "inl"
        ++ rewrite_tac [thm "rep_abs_suml"]
        ++ eq_tac
      ]
  in

  let _ =
    theorem "inr_thm"
      (!% " ! x: (dest_SUM (inr x)) = (mk_sumr x) ")
      [
        flatten_tac ++ unfold "inr"
        ++ rewrite_tac [thm "rep_abs_sumr"]
        ++ eq_tac
      ]
  in

  let inj_on_make_SUM =
    theorem "inj_on_make_SUM"
      (!% " inj_on make_SUM is_sum ")
      [
        cut_thm "inj_on_inverse_intro"
        ++ inst_tac [ (!% " make_SUM "); (!% " is_sum ");
                      (!% " dest_SUM ") ]
        ++ cut_thm "make_SUM_inverse"
        ++ split_tac
        -- [ basic; basic ]
      ]
  in

  let inl_eq =
    theorem "inl_eq"
      (!% " !x y: ((inl x) = (inl y)) = (x = y) ")
      [ flatten_tac ++ equals_tac ++ iffE
        --
          [
            unfold "inl"
            ++ cut_thm "inj_on_make_SUM"
            ++ unfold "inj_on"
            ++ inst_tac [ (!% " mk_suml _x ") ; (!% " mk_suml _y ") ]
            ++ blast_tac ++ (simpC_tac [mk_suml_is_sum] // skip)
            ++ rewrite_tac [mk_suml_eq]
            ++ basic;
            simp
          ]
      ]
  in

  let inr_eq =
    theorem "inr_eq"
      (!% " !x y: ((inr x) = (inr y)) = (x = y) ")
      [ flatten_tac ++ equals_tac ++ iffE
        --
          [
            unfold "inr"
            ++ cut_thm "inj_on_make_SUM"
            ++ unfold "inj_on"
            ++ inst_tac [ (!% " mk_sumr _x ") ; (!% " mk_sumr _y ") ]
            ++ blast_tac ++ (simpC_tac [mk_sumr_is_sum] // skip)
            ++ rewrite_tac [mk_sumr_eq]
            ++ basic;
            simp
          ]
      ]
  in

  let inr_not_inl =
    theorem "inr_not_inl"
      (!% " !x y: ~((inr x) = (inl y)) ")
      [
        flatten_tac
        ++ rewrite_tac [defn "inr" ; defn "inl"]
        ++ (show
              (!% "
                   ! x y: ((is_sum x) & (is_sum y))
                   => (((make_SUM x) = (make_SUM y)) => (x = y))
                   ")
              (cut inj_on_make_SUM
               ++ unfold "inj_on"
               ++ basic))
        ++ inst_tac [ (!% " mk_sumr _x "); (!% " mk_suml _y ") ]
        ++ implA
        --
          [
            simpC_tac  [mk_sumr_is_sum; mk_suml_is_sum];
            mp_tac
            ++ unfold "mk_sumr" ++ unfold "mk_suml"
            ++ once_rewrite_tac [thm "function_eq"]
            ++ inst_tac [ (!% " true ") ] ++ beta_tac
            ++ once_rewrite_tac [thm "function_eq"]
            ++ inst_tac [ (!% " _y ") ] ++ beta_tac
            ++ once_rewrite_tac [thm "function_eq"]
            ++ inst_tac [ (!% " _x ") ] ++ beta_tac
            ++ once_rewrite_tac [thm "equals_bool"] ++ iffA
            ++ simpA []
          ]
      ]
  in

  let inl_not_inr =
    theorem "inl_not_inr"
      (!% " !x y: ~((inl x) = (inr y)) ")
      [
        flatten_tac
        ++ cut ~inst:[ (!% " _y "); (!% " _x ") ] inr_not_inl
        ++ simpA []
      ]
  in

  let outl_thm =
    theorem "outl_thm"
      (!% " ! x: ((outl (inl x)) = x) ")
      [
        flatten_tac
        ++ (unfold "outl")
        ++ rewrite_tac [inl_eq]
        ++ rewrite_tac [thm "choice_refl2"]
        ++ eq_tac
      ]
  in

  let outr_thm =
    theorem "outr_thm"
      (!% " (! x: ((outr (inr x)) = x)) ")
      [
        flatten_tac
        ++ (unfold "outr")
        ++ rewrite_tac [inr_eq]
        ++ rewrite_tac [thm "choice_refl2"]
        ++ eq_tac
      ]
  in

  let make_SUM_onto =
    prove (!% "!a: ?f : ((a = (make_SUM f)) & (is_sum f))")
      (flatten_tac
       ++ inst_tac [ (!% " dest_SUM _a ") ]
       ++ split_tac
       --
         [
           rewrite_tac [thm "dest_SUM_inverse"] ++ eq_tac;
           cut (thm "dest_SUM_mem") ++ unify_tac
      ])
  in

  let _ =
    theorem "SUM_cases"
      (!% " !(a: ('a + 'b)): ((?x: a = (inl x)) | (?x: a = (inr x))) ")
      [
        flatten_tac
        ++ cut ~inst:[ (!% " _a ") ] make_SUM_onto
        ++ unfold "is_sum"
        ++ flatten_tac
        ++ rewrite_tac [defn "inl"; defn "inr"]
        ++ inst_tac [ (!% " _l ") ] ++ inst_tac [ (!% " _r ") ]
        ++ split_tac ++ simp
      ]
  in

  let forall_sum =
    theorem "forall_sum"
      (!% "!(P: ('a + 'b) ->bool) :
           (!x: P x) = ((!x: P (inl x)) & (!x: P (inr x)))")
      [
        flatten_tac ++ equals_tac ++ blast_tac
        ++ (unify_tac // skip)
        ++ cases_of  (!% " _x ") ++ simp
      ]
  in

  let _ =
    theorem "SUM_induct"
      (!% "!(P: ('a + 'b) ->bool) :
           ((!x: P (inl x)) & (!x: P (inr x))) => (!x: P x)")
      [
        allC ++ implC
        ++ rewriteC_tac [forall_sum]
        ++ basic
      ]
  in

  let sum_axiom =
    theorem "sum_axiom"
      (!% "! f g :
           ?! h: (!x: (h (inl x)) = (f x)) & (!x: (h (inr x)) = (g x))")
      [
        flatten_tac
        ++ (unfold "EXISTS_UNIQUE")
        ++ betaC
        ++ scatter_tac
        --
          [
            (* 1 *)
            inst_tac
              [ (!% " (%x:
                     if (?v : (x = (inl v)))
                     then (_f (@v: x = (inl v)))
                     else (_g (@v: x = (inr v)))) ") ]
            ++ (show (!% " !x: ?v: (inl x) = (inl v) ")
                     (flatten_tac ++ inst_tac [ (!% " _x ") ]  ++ eq_tac))
            ++ beta_tac
            ++ rewrite_tac [inl_eq]
            ++ split_tac ++ flatten_tac
            --
              [
                simpC_tac [thm "choice_refl2"; inl_eq];
                simpC_tac [thm "choice_refl2"; inl_eq; inr_eq; inr_not_inl]
              ];
            (* 2 *)
            once_rewriteC_tac [thm "function_eq"]
            ++ specC
            ++ cases_of (!% " _x1 ")
            ++ split_tac
            ++ simp
          ]
      ]

  in
  let _ =
    theorem "sum_fn_exists"
      (!% "! f g: ?h: (!x: (h(inl x)) = (f x)) & (!x: (h(inr x)) = (g x))")
      [
        flatten_tac
        ++ cut ~inst:[ (!% " _f "); (!% " _g ") ] sum_axiom
        ++ unfold "EXISTS_UNIQUE"
        ++ betaA
        ++ flatten_tac
        ++ instC [ (!% " _x ") ]
        ++ blast_tac ++ unify_tac
      ]
  in

  let _ =
    theorem "sum_unique"
      (!% "! f g a b :
           (((!x: (a (inl x)) = (f x)) & (!x: (a (inr x)) = (g x)))
           & ((!x: (b (inl x)) = (f x)) & (!x: (b (inr x)) = (g x))))
           =>
           (a = b)")
      [
        flatten_tac
        ++ cut ~inst:[ (!% " _f "); (!% " _g ") ] sum_axiom
        ++ unfold "EXISTS_UNIQUE"
        ++ beta_tac
        ++ flatten_tac
        ++ back_tac
        ++ split_tac ++ basic
      ]
  in

  let isl_thm =
    theorem "isl_thm"
      (!% " (!x: (isl (inl x))) & (!x: ~(isl (inr x))) ")
      [
        unfold "isl"
        ++ blast_tac
        --
          [
            instC [ (!% " _x ") ] ++ eq_tac;
            simpA_tac [inr_not_inl]
          ]
      ]
  in

  let isr_thm =
    theorem "isr_thm"
      (!% " (!x: (isr (inr x))) & (!x: ~(isr (inl x))) ")
      [
        unfold "isr"
        ++ blast_tac
        --
          [
            instC [ (!% " _x ") ] ++ eq_tac;
            cut inl_not_inr
            ++ instA [ (!% " _x "); (!% " _x1 ") ]
            ++ blast_tac
          ]
      ]
  in

  let _ =
    theorem "isl_outl"
      (!% " ! x: (isl x) => ((inl (outl x)) = x) ")
      [
        flatten_tac
        ++ cases_of (!% " _x ") ++ replace_tac
        --
          [
            rewriteC_tac [outl_thm] ++ eq_tac;
            cut isl_thm
            ++ flatten_tac
            ++ inst_tac [ (!% " _x1 ") ]
            ++ inst_tac [ (!% " _x1 ") ]
            ++ blast_tac
          ]
      ]
  in

  let _ =
    theorem "isr_outr"
      (!% " ! x: (isr x) => ((inr (outr x)) = x) ")
      [
        flatten_tac
        ++ cases_of (!% " _x ") ++ replace_tac
        --
          [
            cut isr_thm
            ++ flatten_tac
            ++ inst_tac [ (!% " _x1 ") ]
            ++ inst_tac [ (!% " _x1 ") ]
            ++ blast_tac;
            rewriteC_tac [outr_thm] ++ eq_tac
          ]
      ]
  in

  let _ =
    define (?<% "map f g x
                 =
                 if (isl x) then (inl (f (outl x))) else (inr (g (outr x)))")
  in

  let _ =
    theorem "map_thm"
      (!% "
           (! f g x : ((map f g) (inl x)) = (inl (f x)))
           &
           (! f g x : ((map f g) (inr x)) = (inr (g x)))
           ")
      [
        split_tac ++ flatten_tac ++ unfold "map"
        ++ simpC_tac [isl_thm; outl_thm; outr_thm]
      ]
  in

  end_theory ~save:false ()


(** Printer for errors. *)
let rec error_exn_printer e =
  match e with
  | Report.Error(err) ->
     Format.open_box 0;
     Display.print_error err;
     Format.close_box();
     Format.print_newline()
  | Report.Errors(errs) ->
     List.iter error_exn_printer errs
  | _ ->
     Format.open_box 0;
     Format.print_string (Printexc.to_string e);
     Format.close_box();
     Format.print_newline()

(** Top-level. *)
let _ =
  try
    (* Initalize the HSeq libraries. *)
    initialize();
    (* Build the SumExample theory. *)
    sum_example();
    (* Print the built theory. *)
    Format.open_box 0;
    Display.print_theory (theory "SumExample");
    Format.close_box();
    Format.print_newline()
  with
    exn ->
    begin
      error_exn_printer exn;
      exit (-1)
    end
