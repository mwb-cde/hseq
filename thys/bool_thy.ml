new_theory "boolean";;       (* begin theory boolean *)
parents ["base"];;

let true_l1 =  prove_theorem "true_l1" "!x: (x=true) => x" 
([flatten_tac;(replace (-1) 1);trivial]);; 

let true_l2 = prove_theorem "true_l2" "!x: x => (x=true)"
([flatten_tac; cut_thm "bool_cases"; 
allE "x_1"; disjI; replace (-1) 1; eq_tac;
replace (-1) (-2); rewrite_thm "false_def" (-2);
flatten_tac]);;

let iff_l1 = prove_theorem "iff_l1" "!x y: (x = y ) => (x => y)"
([flatten_tac;replace (-2) (-1); basic]);;

let iff_l2 = prove_theorem "iff_l2" 
"!x y: ((x => y) and (y => x)) => (x=y)"
([
flatten_tac; cut_thm "bool_cases"; allE "x_1";
disjI; cut true_l1; allE "x_1"; 
implE; replace (-1) 1; eq_tac;
mp_tac;  flatten_tac; cut true_l2;  allE "y_1";
mp_tac; replace (-1) 1; replace (-3) 1; eq_tac;
cut_thm "bool_cases"; allE "y_1"; disjI;
replace (-1) (-4); rotateA; rotateA; rotateA; implE; trivial;
cut true_l2; allE "x_1"; mp_tac; replace_rl (-3) (-1);
unify_tac (-1) 1; replace (-1) 1; replace (-2) 1;eq_tac]);;

let iff_l3 = 
prove_theorem "iff_equals" "!x y: (x iff y) iff (x = y)"
([
flatten_tac; unfold "iff" 1; 
conjI ; flatten_tac; cut iff_l2; allE "x_1"; allE "y_1"; 
implE; conjI; flatten_tac;  
basic; flatten_tac;
basic; flatten_tac; basic; 
replace (-1) 1; eq_tac; 
flatten_tac;  replace (-1) 1; conjI;
repeat (flatten_tac++basic)]);;


prove_theorem "equals_iff" "!x y: (x iff y) = (x = y)"
([flatten_tac; cut_thm "iff_l2"; allE "x_1 iff y_1"; allE "x_1 = y_1";
split_tac; basic; split_tac; flatten_tac;
replace (-1) 1; 
unfold "iff" 1; conjI;
flatten_tac; basic; flatten_tac; basic;
flatten_tac;
cut_thm "iff_l2"; allE "x_1"; allE "y_1"; 
unfold "iff" (-2); mp_tac; basic
]);;

prove_theorem "if_true" "! t f: (if true t f) = t"
[flatten_tac; unfold "if" 1; cut_thm "epsilon_ax";
allE "(%z: (true => (z=t_1)) and ((not  true) => (z=f_1)))";
implE; 
existE "t_1"; beta_tac; split_tac; 
flatten_tac;flatten_tac; eq_tac; 
beta_tac; flatten_tac; delete (-2); implE; trivial; basic
];;

prove_theorem "if_false" "! t f: (if false t f) = f"
[flatten_tac; unfold "if" 1; cut_thm "epsilon_ax";
allE "(%z: (false => (z=t_1)) and ((not false) => (z=f_1)))";
beta_tac; implE;  existE "f_1"; split_tac;
flatten_tac; eq_tac; 
implI; false_tac; 
flatten_tac; delete (-1); implE; flatten_tac;
basic];;

close_theory();;  (* Postpone completion *)

open_theory "boolean";;  (* continue with theory boolean *)

(* some of the following properties taken from the PVS prelude library *)

(* cases *)

prove_theorem "cases_thm" "!P: (not P) or P"
[flatten_tac; basic];;

(* properties of boolean functions *)

prove_theorem "equals_bool" "! x y: (x = y) = (x iff y)"
[ flatten_tac ; rewrite_thm "equals_iff" 1; eq_tac];;

prove_theorem "excluded_middle" "! x: x or (not x)"
[flatten_tac; basic];;

prove_theorem "iff_def" "!x y: (x iff y) = ((x=>y) and (y=> x))"
[flatten_tac; rewrite_thm "iff" 1;  eq_tac];;

(* conjunction *)

prove_theorem "conj_assoc" "!x y z: (x and (y and z))=((x and y) and z)"
[flatten_tac; rewrite_thm "equals_bool" 1; unfold "iff" 1; conjI;
flatten_tac; repeat (Logic.apply_list[conjI; basic]);
flatten_tac;  repeat (Logic.apply_list[conjI; basic])];;

(* quantifiers *)

prove_theorem "not_exists" "!P: (?x: P x) = (not (!x: not (P x)))"
[rewrite_thm "equals_bool" 1; flatten_tac; unfold "iff" 1; conjI;
flatten_tac;  inst_tac ["x_1"] (-1); flatten_tac; basic;
flatten_tac; flatten_tac; unify_tac (-1) 1];;

prove_theorem "exists_not" "!P: (?x: not (P x)) = (not (!x: P x))"
[rewrite_thm "equals_bool" 1; flatten_tac; unfold "iff" 1; conjI;
flatten_tac;  unify_tac (-1) 1; 
flatten_tac; flatten_tac; inst_tac ["x_1"] 2; flatten_tac; basic];;

prove_theorem "exists_or" 
    "!P Q: ((?x: (P x) or (Q x)) = ((?x: (P x))or (?x: (Q x))))"
[flatten_tac; rewrite_thm "equals_bool" 1;
split_tac; flatten_tac; 
split_tac; 
(flatten_tac++(inst_tac ["x_1"] 1)++ flatten_tac++basic);
(flatten_tac++(inst_tac ["x_1"] 1)++ flatten_tac++basic);
flatten_tac; inst_tac ["x_1"] 1; inst_tac ["x_1"] 2; split_tac;
basic; basic];;

prove_theorem "exists_implies" 
"!P Q: (?x: (P x) => (Q x)) = ((?x: not (P x)) or (?x: Q x))" 
[flatten_tac; rewrite_thm "equals_bool" 1;
split_tac; split_tac; flatten_tac; inst_tac ["x_1"] 1; 
flatten_tac; basic; flatten_tac; inst_tac ["x_1"] 2;
flatten_tac; basic; flatten_tac; implE; implE; inst_tac ["x_1"] 2;
flatten_tac; basic; inst_tac["x_1"] 2; basic];;

prove_theorem "exists_and"
 "!P Q: (?x: (P x) and (Q x)) => ((?x: P x)  and (?x: Q x))"
[flatten_tac; split_tac; repeat((inst_tac ["x_1"] 1)++basic)];;

prove_theorem "not_forall" 
  "!P: ((!x: P x)= (not(? x: not (P x))))"
[flatten_tac; rewrite_thm "equals_bool" 1; split_tac;
flatten_tac; inst_tac ["x_1"] 1; flatten_tac; basic;
flatten_tac; inst_tac ["x_1"] (-1); flatten_tac; basic];;

prove_theorem "forall_not"
  "!P:((!x: not (P x))=(not (? x: P x)))"
[flatten_tac; rewrite_thm "equals_bool" 1; split_tac;
flatten_tac; inst_tac ["x_1"] 1; basic;
flatten_tac; inst_tac ["x_1"] (-2); flatten_tac; basic];;

prove_theorem "forall_and" 
  "!P Q: (!x: (P x) and (Q x))= ((!x: P x) and (!x: Q x))"
[flatten_tac; equals_tac 1; split_tac; flatten_tac;
inst_tac ["x_1"] (-1); inst_tac ["x_1"] (-2); split_tac; basic; basic;
flatten_tac; inst_tac ["x_1"] (-1); flatten_tac; basic;
flatten_tac; inst_tac ["x_1"] (-1); flatten_tac; basic
];;

prove_theorem "forall_or"
"! P Q: ((! x: P x) or (!x: Q x)) => (!x: (P x) or (Q x))"
[flatten_tac; split_tac; (repeat ((inst_tac ["x_1"] (-1))++basic))
];;

(* functions *)

prove_theorem "congruence"
"!f g a b: ((f = g) and (a=b)) =>((f a)=(g b))"
[flatten_tac; replace (-1) 1; replace (-2) 1; eq_tac];;

prove_theorem "eta" "!f: (% x: f x) = f"
[flatten_tac; cut_thm "extensionality";
inst_tac ["% x: f_1 x"; "f_1"] (-1); split_tac; 
basic; flatten_tac; beta_tac; eq_tac];;

prove_theorem "false_l1" "!x: (not x) = (x=false)"
[flatten_tac; equals_tac 1; split_tac; flatten_tac; basic;
flatten_tac; bool_tac; flatten_tac;
split_tac; basic; basic; bool_tac];;

prove_theorem "if_true1" "!x a b: x => ((if x a b) =a)"
[flatten_tac; cut_thm  "if_true"; cut_thm "true_l2"; 
inst_asm ["x_1"]; mp_tac; replace (-1) 1; replace (-2) 1; 
eq_tac];;

prove_theorem "if_false1" "!x a b: (not x)=> ((if x a b)=b)"
[rewrite_thm "false_l1" 1; flatten_tac; replace (-1) 1; 
rewrite_thm "if_false" 1; eq_tac];;

end_theory();;  (* end of theory *)

