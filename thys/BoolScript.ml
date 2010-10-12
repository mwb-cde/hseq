(*----
 Name: BoolScript.ml
 Copyright M Wahab 2005-2010
 Author: M Wahab  <mwb.cde@googlemail.com>

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
   Boolean operators and their basic properties.
*)

begin_theory "Bool" ["base"];; 

(** Cases/excluded middle *)

let cases_thm=
theorem "cases_thm" << !P: (not P) or P >>
[flatten_tac++ basic];;

let excluded_middle=
theorem "excluded_middle" << ! x: (not x) or x>>
[flatten_tac ++ basic];;

(** Induction *)
let bool_induct = 
  theorem "bool_induct"
  << !P: ((P true) & (P false)) => (!(x:bool): P x) >>
  [
    flatten_tac
    ++ cut (thm "bool_cases")
    ++ instA [ << _x >> ] 
    ++ disjA ++ replace_tac ++ basic
  ]

(** Equivalence of boolean equality (=) and equivalence (iff) *)

let true_l1 = 
theorem "true_l1" << !x: (x=true) => x >> 
[flatten_tac; replace_tac; trivial];; 

let true_l2 = 
theorem "true_l2" << !x: x => (x=true) >>
[flatten_tac ++ (cut_thm "bool_cases") ++ (allA << _x >>) ++ disjA ;
basic;
rewrite_tac [thm "false_def"]++replace_tac ++ scatter_tac];;

let iff_l1 = theorem "iff_l1" << !x y: (x = y ) => (x => y)>>
[flatten_tac ++ replace_tac ++ basic];;

let _ = stop();;

let iff_l2 = theorem "iff_l2"
<< !x y: ((x => y) and (y => x)) => (x=y) >>
[
  (flatten_tac ++ (cut_thm ~inst:[ << _x >> ] "bool_cases") ++ disjA);
  cut ~inst:[ << _x >> ] true_l1
    ++ (match_asm << (A = B) => C >>
    (fun a -> 
      (match_asm << (A = B) >> 
       (fun f -> mp_tac ?info:None ~a:a ~h:f))))
++ mp_tac 
++ (cut true_l2) ++ (allA <<_y>>) ++ mp_tac
++ replace_tac ++ eq_tac;
(cut_thm "bool_cases") ++ (allA << _y >>) ++ disjA;
(cut true_l1) ++ (allA << _y >>) ++ mp_tac
++ (match_asm << (_y => C) >>
    (fun a -> 
      (match_asm << _y >> 
       (fun f -> mp_tac ?info:None ~a:a ~h:f))))
++ (cut true_l2) ++ (allA <<_x>>) ++ mp_tac
++ replace_tac ++ eq_tac;
replace_tac ++ eq_tac];;

let iff_equals = 
theorem "iff_equals" << !x y: (x iff y) iff (x = y)>>
([flatten_tac ++ (rewrite_tac [(defn "iff")]) ++ conjC;
flatten_tac ++ (cut iff_l2) ++ (inst_tac [ <<_x>>; << _y >> ])
++ implA;
conjC ++ flatten_tac;
mp_tac ++ basic;
(match_asm << _y => _x >> 
 (fun a -> match_asm << _y >> 
   (fun f -> mp_tac ?info:None ~a:a ~h:f)))
++basic;
basic;
flatten_tac ++ replace_tac
++ split_tac ++ (flatten_tac ++ basic)]);;

let equals_iff=
theorem "equals_iff" << !x y: (x iff y) = (x = y)>>
([
flatten_tac 
++ (cut iff_l2) ++ (inst_tac [ << _x iff _y >>; <<_x = _y>> ])
++ split_tac;
flatten_tac ++ (unfold "iff")
++ (cut_thm "iff_l2") ++ (inst_tac [ << _x >>;  << _y >> ])
++ mp_tac ++ basic;
flatten_tac ++ replace_tac
++ (rewrite_tac [defn "iff"])
++ split_tac ++ (flatten_tac ++ basic);
basic]);;

let equals_bool=
theorem "equals_bool" << ! x y: (x = y) = (x iff y)>>
[ flatten_tac ++ (rewrite_tac [(thm "equals_iff")])  ++ eq_tac];;

let iff_def=
theorem "iff_def" << !x y: (x iff y) = ((x=>y) and (y=> x))>>
[flatten_tac ++  (rewrite_tac [(defn "iff")]) ++ eq_tac];;

(** Truth and falsity of a property *)

let true_prop =
theorem "true_prop" << !x : (x=true) = x >>
[flatten_tac ++ equals_tac 
++ cut ~inst:[ << _x >> ] true_l1
++ cut ~inst:[ << _x >> ] true_l2
++ blast_tac ];;

let false_prop =
theorem "false_prop" << !x : (x=false) = ~x >>
  [
   flatten_tac 
     ++ cut ~inst:[ << _x >> ] (thm "bool_cases")
     ++ rewrite_tac [thm "false_def"] 
     ++ equals_tac ++ blast_tac 
     ++ (replace_tac ++ scatter_tac)
 ];;

let true_not_false = 
theorem "true_not_false" ~simp:true
    << (true = false) = false >>
[ rewrite_tac [false_prop] ++ scatter_tac ];;

let false_not_true = 
theorem "false_not_true" ~simp:true
    << (false = true) = false >>
[ rewrite_tac [true_prop] ++ eq_tac ];;


(** Negation *)

let not_not = 
theorem ~simp:true "not_not"
<< !x: (not not x) = x >>
[ flatten_tac ++ equals_tac ++ blast_tac];;

let not_absorb=
theorem ~simp:true "not_absorb"
<< ((not true) = false) and ((not false) = true) >>
[split_tac ++ equals_tac ++ blast_tac];;

let not_conj = 
theorem ~simp:true "not_conj"
<< !x y: (not (x and y)) = (not x or not y) >>
[flatten_tac ++ equals_tac ++ blast_tac];;

let not_disj = 
theorem ~simp:true "not_disj"
<< !x y: (not (x or y)) = (not x and not y) >>
[flatten_tac ++ equals_tac ++ blast_tac];;

let not_forall=
theorem ~simp:true "not_forall" 
    << !P: (not (!x: (P x))) = (?x: not (P x)) >>
[flatten_tac ++ equals_tac ++ scatter_tac 
++ inst_tac [ << _x >> ] ++ blast_tac];;

let not_exists=
theorem ~simp:true "not_exists"
  << !P: (not (?x: P x)) = (! x: not (P x)) >>
[flatten_tac ++ equals_tac ++ scatter_tac 
++ inst_tac [ << _x >> ] ++ blast_tac];;

(** Conjunction *)

(*
let conj_assoc=
theorem ~simp:true "conj_assoc" 
<< !x y z: (x and (y and z))=((x and y) and z)>>
[ flatten_tac ++ equals_tac ++ blast_tac];;
*)

let conj_assoc=
theorem ~simp:true "conj_assoc" 
<< !x y z: ((x and y) and z)=(x and (y and z))>>
[ flatten_tac ++ equals_tac ++ blast_tac];;

let conj_comm=
theorem ~simp:false "conj_comm"
<< !x y: (x and y) = (y and x) >>
[ flatten_tac ++ equals_tac ++ blast_tac] ;;

let conj_lcomm=
theorem ~simp:true "conj_lcomm"
<< !x y z: (x and (y and z)) = (y and (x and z)) >>
[ flatten_tac ++ equals_tac ++ blast_tac] ;;

let conj_absorb =
theorem ~simp:true "conj_absorb"
<< 
  (! x: (true and x) = x) 
  and (! x: (x and true) = x) 
  and (! x: (false and x) = false) 
  and (! x: (x and false) = false) 

>>
[ split_tac ++ flatten_tac ++ equals_tac ++ blast_tac];;

let conj_trivial =
theorem ~simp:true "conj_trivial"
<< !x : (x & x) = x >>
[flatten_tac ++equals_tac ++ blast_tac];;

let conj_disj_distrib=
theorem ~simp:false "conj_disj_distrib"
<< !x y z: (x and (y or z)) = ((x and y) or (x and z)) >>
[ flatten_tac  ++ equals_tac ++ blast_tac];;

(** Disjunction *)

(*
let disj_assoc =
theorem ~simp:true "disj_assoc"
<< !x y z: (x or (y or z)) = ((x or y) or z) >>
[ flatten_tac ++ equals_tac ++ blast_tac];;
*)

let disj_assoc =
theorem ~simp:true "disj_assoc"
<< !x y z: ((x or y) or z) = (x or (y or z)) >>
[ flatten_tac ++ equals_tac ++ blast_tac];;

let disj_comm=
theorem ~simp:false "disj_comm"
<< !x y: (x or y) = (y or x) >>
[ flatten_tac ++ equals_tac ++ blast_tac ];;

let disj_lcomm=
theorem ~simp:true "disj_lcomm"
<< !x y z: (x or (y or z)) = (y or (x or z)) >>
[ flatten_tac ++ equals_tac ++ blast_tac ];;

let disj_absorb =
theorem ~simp:true "disj_absorb"
<< 
(! x: (true or x) = true) 
and (! x: (x or true) = true) 
and (! x: (false or x) = x) 
and (! x: (x or false) = x) 
>>
[ split_tac ++ flatten_tac ++ equals_tac ++ blast_tac];;

let disj_trivial =
theorem ~simp:true "disj_trivial"
<< !x : (x | x) = x >>
[flatten_tac ++equals_tac ++ blast_tac];;

let disj_conj_distrib=
theorem ~simp:false "disj_conj_distrib"
<< !x y z: (x or (y and z)) = ((x or y) and (x or z)) >>
[ flatten_tac  ++ equals_tac ++ blast_tac ];;

(** Implication *)

let implies_trivial =
theorem ~simp:true "implies_trivial"
<< !x : (x => x) = true >>
[flatten_tac ++equals_tac ++ blast_tac];;

let implies_absorb =
theorem ~simp:true "implies_absorb"
<<
(! x: (true => x) = x)
and (!x: (x => true) = true)
and (!x: (false => x) = true)
and (!x: (x => false) = not x)
>>
[split_tac ++ flatten_tac ++ equals_tac ++ blast_tac];;

(** Iff *)

let iff_trivial = 
theorem ~simp:true "iff_trivial"
<< !x : (x iff x) = true >>
[flatten_tac ++equals_tac ++ blast_tac ];;

(** Quantifiers *)

(** Exististenial *)

let exists_or=
theorem "exists_or" 
    << !P Q: ((?x: (P x) or (Q x)) = ((?x: (P x)) or (?x: (Q x))))>>
[flatten_tac ++ (rewrite_tac [equals_bool]) ++ (unfold "iff")
++ split_tac ++ flatten_tac;
(inst_tac [ <<_x>> ]) ++ (inst_tac [ <<_x>> ])
++ split_tac ++  basic;
split_tac ++ flatten_tac
++ inst_tac [ <<_x>> ] ++ flatten_tac ++ basic];;

let exists_implies = 
theorem "exists_implies" 
  << !P Q: (?x: (P x) => (Q x)) = ((?x: not (P x)) or (?x: Q x)) >> 
[
  flatten_tac ++ rewrite_tac [equals_bool] ++ unfold "iff"
  ++ scatter_tac
  -- 
    [
      match_concl << ?x: not (_P1 x) >>
	(fun l -> inst_tac [ << _x >> ] ~f:l)
      ++ flatten_tac ++ basic; 
      match_concl << ?x: (_Q1 x) >>
	(fun l -> inst_tac [ << _x >> ] ~f:l) ++ basic;
      match_concl << ?x:  (_P1 x) => X>>
	(fun l -> inst_tac [ << _x >> ] ~f:l)++ flatten_tac ++ basic; 
      match_concl << ?x: X => (_Q1 x) >>
	(fun l -> inst_tac [ << _x >> ] ~f:l) ++ flatten_tac ++ basic
    ]
];;


let exists_and = 
theorem "exists_and"
 << !P Q: (?x: (P x) and (Q x)) => ((?x: P x)  and (?x: Q x)) >>
[flatten_tac ++ split_tac ++ inst_tac [ << _x>> ] ++ basic];;

let exists_absorb = 
theorem ~simp:true "exists_absorb"
<< ((? x: true) = true) and ((? x: false) = false) >>
[ split_tac ++ equals_tac ++ scatter_tac 
++ inst_tac [ << any >> ] ++ trivial];;

let exists_simp =
theorem ~simp:false "exists_simp"
<< !P: (? x: P) = P >>
[ flatten_tac ++ equals_tac ++ blast_tac
    ++ inst_tac [ << any >> ] ++ basic ];;

(** Universal *)

let forall_and=
theorem "forall_and" 
  << !P Q: (!x: (P x) and (Q x))= ((!x: P x) and (!x: Q x)) >>
[flatten_tac ++ equals_tac ++ scatter_tac 
++ inst_tac [ << _x>> ] 
++ (blast_tac // inst_tac [ << _x>> ] ++ basic)]

let forall_or=
theorem "forall_or"
<< ! P Q: ((! x: P x) or (!x: Q x)) => (!x: (P x) or (Q x)) >>
[flatten_tac ++ split_tac ++ inst_tac [ << _x >> ] ++ basic];;

let forall_absorb = 
theorem ~simp:true "forall_absorb"
<< ((! x: true) = true) and ((! x: false) = false) >>
[ split_tac ++ equals_tac ++ blast_tac 
++ inst_tac [ << any >> ] ++ trivial];;


(** Equality *)

let eq_trans =
theorem "eq_trans" << !x y z: ((x = y) and (y = z)) => (x = z) >>
[flatten_tac ++ replace_tac ++ eq_tac];;

let eq_sym =
theorem "eq_sym" << !x y : (x = y) = (y = x) >>
[flatten_tac ++ once_rewrite_tac [equals_bool]
   ++ scatter_tac ++ replace_tac ++ eq_tac];;

let eq_fact = 
theorem "eq_fact" ~simp:true 
<< ! x : (x = x) = true >>
[ flatten_tac++ cut_back_tac true_l2 ++ eq_tac];;

(***
   The simplifier now has enough information to be useful for boolean
   reasoning.

   Theorems below this point may use the simplifier tactics and should
   not be moved to an earlier position.
***)

(** Conditional *)

let if_true=
theorem ~simp:true "if_true" 
  << ! t f: (if true then t else f) = t>>
[
flatten_tac ++ (unfold "IF")++ (cut_thm "epsilon_ax")
++ (allA << (%(z:'a): ((true => (z=_t)) and ((not  true) => (z=_f)))) >>)
++ implA;
existC <<_t>> ++ beta_tac 
++ scatter_tac ++ eq_tac;
beta_tac ++ flatten_tac ++ implA -- [trivial; basic]
];;

let if_false=
theorem ~simp:true "if_false" << ! t f: (if false then t else f) = f>>
([
flatten_tac ++ (unfold "IF")++ (cut_thm "epsilon_ax")
++ (allA << (%(z:'a): ((false => (z=_t)) and ((not false) => (z=_f)))) >>)
++ implA;
(existC <<_f>>) ++ beta_tac 
++ split_tac 
-- [flatten_tac ; flatten_tac ++ eq_tac];
beta_tac ++ flatten_tac ++
  (match_asm << (not false) => C >> 
   (fun l -> (implA ?info:None ~a:l))) 
   -- [flatten_tac; basic]
]);;

let if_true1=
theorem ~simp:true "if_true1" 
<< !x a b: x => ((if x then a else b) =a)>>
  [
   flatten_tac 
     ++ 
     show << _x = true >>
       (cut ~inst:[ << _x >> ] true_prop 
	  ++ replace_tac 
	  ++ basic)
     ++ replace_tac
     ++ rewrite_tac [if_true] ++ eq_tac
 ];;

let if_false1=
theorem ~simp:true "if_false1" 
<< !x a b: (not x)=> ((if x then a else b)=b) >>
  [
   scatter_tac 
     ++ (show << _x = false>>
	 (cut ~inst:[ << _x >> ] false_prop 
	    ++ replace_tac ++ flatten_tac
	    ++ basic))
     ++ replace_tac
     ++ rewrite_tac [if_false] ++ eq_tac
 ];;

let if_expand=
theorem ~simp:false "if_expand" 
<< ! x a b : (if x then a else b) = ((x and a) or (not x and b)) >>
[
 flatten_tac
   ++ cut ~inst:[ << _x >> ] (thm "bool_cases")
   ++ split_tac ++ replace_tac ++ rewrite_tac [if_true; if_false]
   ++ simp
];;

let if_id =
theorem ~simp:true "if_id"
<< !x a: (if x then a else a) = a >>
[ 
  flatten_tac 
    ++ cut ~inst:[ << _x >> ] (thm "bool_cases")
    ++ split_tac
    ++ simp
];;

let if_rand = 
theorem ~simp:false "if_rand"
<< !f x a b: (f (if x then a else b)) = (if x then f a else f b) >>
[ 
  flatten_tac 
    ++ cut ~inst:[ << _x >> ] (thm "bool_cases")
    ++ split_tac
    ++ simp
];;

let if_rator = 
theorem ~simp:false "if_rator"
<< !v x a b : ((if x then a else b) v) = (if x then a v else b v) >>
[ 
  flatten_tac 
    ++ cut ~inst:[ << _x >> ] (thm "bool_cases")
    ++ split_tac
    ++ simp
];;



(** Exists Unique *)

let exists_unique_thm = 
  theorem "exists_unique_thm"
    << 
  !P: (?! x: (P x)) 
    = ((?x: P x) and (! x y: ((P x) and (P y)) => x = y)) 
    >>
[
 flatten_tac ++ unfold "EXISTS_UNIQUE" 
   ++ (show << ! P a: ((% x: _P x) a) = (_P a) >> simp)
   ++ replace_tac
   ++ eq_tac 
];;

let exists_unique_refl =
  theorem "exists_unique_refl"
    << !a : (?! x: (x = a)) >>
  [
   flatten_tac
     ++ unfold "EXISTS_UNIQUE" 
     ++ simp
     ++ scatter_tac
     -- 
     [
       inst_tac [ << _a >> ]++ simp;
       simp
     ]
 ];;

let exists_unique_or = 
  theorem "exists_unique_or"
    << ! P Q: (?! x : (P x) | (Q x)) => ((?! x: P x) | (?! x: Q x)) >>
  [
   flatten_tac
     ++ cut ~inst:[ << % x: (_P x) | (_Q x) >> ] exists_unique_thm
     ++ cut ~inst:[ << % x: (_P x) >> ] exists_unique_thm
     ++ cut ~inst:[ << % x: (_Q x) >> ] exists_unique_thm
     ++ beta_tac 
     ++ replace_tac
     ++ scatter_tac ++ (unify_tac // skip)
     ++ back_tac ++ simp
 ];;

let exists_unique_simp = 
  theorem "exists_unique_simp"
    << ! P : (?! x : P) = (P & (! x y: x = y)) >>
  [
   flatten_tac
     ++ cut ~inst:[ << % x: _P >> ] exists_unique_thm
     ++ beta_tac ++ replace_tac
     ++ simp_tac [exists_simp]
     ++ equals_tac ++ blast_tac 
    ++ simp
 ];;

(** Epsilon (choice) *)

let choice_elim_thm = 
  theorem "choice_elim_thm"
  << !P: (?x: P x) => (P (@x: P x))>>
[
 flatten_tac
   ++ cut ~inst:[ << (%x: _P x) >> ] (thm "epsilon_ax")
   ++ beta_tac ++ blast_tac
   ++ inst_tac [ << _x >> ]
   ++ basic
];;

let choice_refl =
theorem "choice_refl"
    << !x: (@v: v=x) = x >>
[
 flatten_tac
   ++ cut ~inst:[ << (%y: y = _x) >> ] choice_elim_thm
   ++ split_tac
   -- 
   [
    inst_tac [ << _x >> ] ++ beta_tac ++ eq_tac;
    beta_tac ++ basic
  ]
];;

let choice_refl2 =
theorem "choice_refl2"
    << !x: (@v: x=v) = x >>
[
 flatten_tac
   ++ cut ~inst:[ << (%y: _x = y) >> ] choice_elim_thm
   ++ split_tac
   -- 
   [
    inst_tac [ << _x >> ] ++ beta_tac ++ eq_tac;
    beta_tac 
      ++ 
      (match_concl << X >>
       (fun l -> once_rewrite_tac ~f:l [eq_sym]))
      ++ basic
  ]
];;

let choice_thm = 
  theorem "choice_thm"
    << !P : (P (@x: P x)) =  ?x: P x >>
[
 flatten_tac
   ++ equals_tac ++ iffE
       -- 
       [ 
	 inst_tac [ << @x: _P x >> ] ++ basic;
	 cut_back_tac choice_elim_thm
	   ++ basic
       ]
];;

let choice_unique=
  theorem "choice_unique"
    << !P x: (!y: (P y) = (y = x)) => ((@y: P y) = x) >>
  [
   flatten_tac
     ++ replace_tac
     ++ cut choice_refl ++ unify_tac
 ];;


(** Functions *)

let congruence =
theorem "congruence"
<< !f g a b: ((f = g) and (a=b)) =>((f a)=(g b)) >>
[flatten_tac ++ replace_tac ++ eq_tac];;

let eta=
theorem "eta" << !f: (% x: f x) = f >>
[flatten_tac ++ cut_thm "extensionality" 
++ inst_tac [ << % x: _f x >> ; << _f>> ] 
++ split_tac 
-- 
[flatten_tac ++ beta_tac ++ eq_tac;
basic]];;

(*
let false_l1=
theorem "false_l1" << !x: (not x) = (x=false) >>
[flatten_tac ++ equals_tac  ++ blast_tac
--
   [
    equals_tac ++ blast_tac;
    replace_tac ++ trivial
  ]
];;

*)

end_theory();;  (* end of theory *)

