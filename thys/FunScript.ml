(*----
 Name: FunScript.ml
 Copyright M Wahab 2005-2013
 Author: M Wahab  <mwb.cde@gmail.com>

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
   Relations and Functions
*)

let _ = begin_theory "Fun" ["Relation"];; 

(** {5 Equality of functions} *)

let function_eq =
  theorem "function_eq"
    << !f g : (f = g) = (!x: (f x) = (g x))>>
  [
   flatten_tac ++ equals_tac ++ scatter_tac
     -- 
     [
      replace_tac ++eq_tac;
      cut_back_tac (thm "extensionality")
	++ basic
    ]
 ];;

(** {5 Composition} *)

let compose_def = 
  define 
    <:def< compose f g = (%x : f (g x)) >>
  ~pp:(275, infixl, Some "++")

let compose_thm = 
theorem ~simp:true "compose_thm" 
<< !f g x: ((f ++ g) x) = (f (g x)) >>
[simp_tac [defn "compose"] ++ eq_tac];;

let compose_assoc = 
theorem ~simp:true "compose_assoc"
<< ! f g h: (f ++ (g ++h)) = ((f ++ g) ++ h) >>
[flatten_tac; cut_back_tac (thm "extensionality"); simp];;

let compose_abs_r =
theorem ~simp:true "compose_abs_r"
<< ! f g : (f ++ (% x : g x)) = (% x : f (g x)) >>
[flatten_tac; cut_back_tac (thm "extensionality"); simp];;

(** 
   {5 Combinators} 

   These are from HOL CombinScript.
*)

(** {7 Definitions} *)

let combinK_def = 
define <:def< cK = (% x y : x) >>;;

let combinS_def = 
define <:def< cS = (% f g x : f x (g x)) >>;;

let combinI_def = 
define <:def< cI = (cS cK cK) >>;;

let combinC_def = 
define <:def< cC = % f x y: f y x >>;;

let combinW_def =
define <:def< cW = % f x : f x x >>;;

let fail_def =
define <:def< fail = % x y: x >>;;

(** {7 Theorems} *)

let combinK_thm =
theorem ~simp:true "combinK_thm"
<< ! x y: (cK x y) = x >>
[simp_tac [defn "cK"]];;

let combinS_thm =
theorem ~simp:true "combinS_thm"
<< ! f g x : (cS f g x) = (f x (g x)) >>
[simp_tac [defn "cS"]];;

let combinS_abs_l =
theorem ~simp:true "combinS_abs_l"
<< ! f g: (cS (%x : f x) g) = (%x: f x ( g x)) >>
[flatten_tac ++ (cut_back_tac (thm "extensionality")) ++ simp];;

let combinS_abs_r =
theorem ~simp:true "combinS_abs_r"
<< ! f g: (cS f (%x : g x)) = (%x: f x ( g x)) >>
[flatten_tac ++ (cut_back_tac (thm "extensionality")) ++ simp];;

let combinC_thm = 
theorem ~simp:true "combinC_thm"
<< ! f x y : (cC f x y) = (f y x) >>
[simp_tac [defn "cC"]];;

let combinC_abs_l =
theorem ~simp:true "combinC_abs_l" 
<< ! f y: (cC (%x: f x) y) = (%x: f x y) >>
[flatten_tac ++ (cut_back_tac (thm "extensionality")) ++ simp];;

let combinW_thm=
theorem ~simp:true "combinW_thm"
<< !f x : (cW f x) = (f x x)>>
[simp_tac [defn "cW"]];;

let combinI_thm=
theorem ~simp:true "combinI_thm"
<< ! x : (cI x) = x>>
[simp_tac [defn "cI"]];;

let combinI_compose_f =
theorem ~simp:true "combinI_compose_f"
<< (!f : ((cI ++ f) = f)) & (!f : (f ++ cI) = f) >>
[
 conjC 
   ++ flatten_tac 
   ++ (cut_back_tac (thm "extensionality")) ++ simp
];;

let combinK_compose_thm =
theorem ~simp:true "combinK_compose_thm"
<< 
  (!f v : ((cK v) ++ f) = (cK v))
    & (!f v: (f ++ (cK v)) = (cK (f v)))
>>
[
 conjC 
   ++ flatten_tac 
   ++ (cut_back_tac (thm "extensionality")) ++ simp
];;

let fail_thm =
theorem ~simp:true "fail_thm"
<< ! x y: (fail x y) = x >>
[simp_tac [defn "fail"]] ;;

(** {5 Functions} *)

(** {7 Definitions} *)

let id_def =
  define <:def< id = % x : x >>;;

let domain_def = 
  define <:def< domain f = %x: ?y: y = (f x) >>;;

let range_def = 
  define <:def< range f = %x: ?y: x = (f y) >>;;

(** [surj f]: [f] is surjective *)
let surj_def = 
  define <:def< surj f = !y: ?x: y = (f x) >>;;

(** [inj f]: [f] is surjective *)
let inj_def = 
  define <:def< inj f = ! x y: ((f x) = (f y)) => (x = y) >>;;

(** 
   [inj_on f A]: Function [f] is injective on values [x] for which 
   [A x] is true.

   (Note that [inj_on f A] is equivalent to [one_one f (%x: true)].)
*)
let inj_on = 
  define 
    <:def<
  inj_on f A 
    = !x y: ((A x) and (A y)) => (((f x) = (f y)) => (x=y)) 
    >>;;

(** [bij f]: [f] is bijective *)
let bij_def = 
  define <:def< bij f = (inj f) & (surj f) >>;;

(** [invf f g]: [g] is an inverse of function [f]. *)
let invf_def =
  define <:def< invf f g = ! x: ((g (f x)) = x) >>;;

(** {7 Theorems} *)

let id_thm =
theorem ~simp:true "id_thm"
<< ! x : (id x) = x >>
[simp_tac [defn "id"]] ;;

let surj_intro =
  theorem "surj_intro"
    << ! f g: (! x: (f (g x)) = x) => (surj f) >>
  [
   flatten_tac
     ++ unfold "surj" ++ specC
     ++ instC [ << _g _y >> ] 
     ++ instA [ << _y >> ]
     ++ simp
 ];;

let surj_compose = 
  theorem "surj_compose" 
    << ! f g: ((surj f) & (surj g)) => (surj (f ++ g)) >>
  [
   flatten_tac
     ++ simp_all_tac [defn "surj"]
     ++ instA [ << _y >> ] ++ specA
     ++ instA [ << _x  >> ] ++ specA
     ++ instC [ << _x1 >> ]
     ++ simp_all []
 ];;

let inj_compose =
  theorem "inj_compose"
    << ! f g: ((inj f) & (inj g)) => (inj f ++ g) >>
  [ 
    unfold "inj"
    ++ flatten_tac 
    ++ rewrite_tac [compose_thm]
    ++ mp_tac
    ++ mp_tac
    ++ basic
  ];;

(** 
   [inj_on_inverse_intro]: For functions [f, g] and predicate [A], if
   [g] is an inverse for [f] on values for which [A] is true then [f]
   is injective on on [A].
*)
let inj_on_inverse_intro =
  theorem "inj_on_inverse_intro"
    << ! f A g: ((!x: (A x) => ((g (f x)) = x)) => (inj_on f A)) >>
  [
   unfold "inj_on"
     ++ flatten_tac
     ++ (match_asm << !x: P >> copyA)
     ++ inst_tac [ << _x >> ]
     ++ inst_tac [ << _y >> ]
     ++ simp_all []
 ];;

let invf_compose =
  theorem "inv_compose"
    << (! f g: (invf f g) => (g ++ f = id)) >>
  [ 
    flatten_tac 
      ++ once_rewrite_tac [thm "function_eq"]
      ++ simp_all_tac [ defn "invf"]
  ];;

let _ = end_theory ();;
