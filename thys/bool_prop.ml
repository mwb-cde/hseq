open_theory "boolean";;

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
[ rewrite_thm "iff" 1; flatten_tac; eq_tac];;


(* conjunction *)

prove_theorem "conj_assoc" "!x y z: (x and (y and z))=((x and y) and z)"
[flatten_tac; rewrite_thm "equals_bool" 1; unfold "iff" 1; conjI;
flatten_tac; repeat conjI; repeat basic; 
flatten_tac;  conjI; basic; conjI; basic;  basic];;

(* quantifiers *)

prove_theorem "not_exists" "!P: (?x: P x) = (not (!x: not (P x)))"
[rewrite_thm "equals_bool" 1; flatten_tac; unfold "iff" 1; conjI;
flatten_tac;  inst_tac (-1) ["x_1"]; flatten_tac; basic;
flatten_tac; flatten_tac; unify_tac (-1) 1];;

prove_theorem "exists_not" "!P: (?x: not (P x)) = (not (!x: P x))"
[rewrite_thm "equals_bool" 1; flatten_tac; unfold "iff" 1; conjI;
flatten_tac;  unify_tac (-1) 1; 
flatten_tac; flatten_tac; inst_tac 2["x_1"]; flatten_tac; basic];;

prove_theorem "exists_or"
  "!P Q: ((?x: (P x) or (Q x)) = ((?x: (P x)) or (?x: (Q x))))"
[flatten_tac; rewrite_thm "equals_bool" 1; split_tac;
flatten_tac; inst_tac 1 ["x_1"]; inst_tac 2 ["x_1"];
split_tac; (repeat basic);
split_tac; 
repeat (flatten_tac++(inst_tac 1 ["x_1"])++ flatten_tac++(repeat basic))
];;

prove_theorem "exists_implies"
  "!P Q: (?x: (P x) => (Q x)) = ((?x: not (P x)) or (?x: Q x))"
[flatten_tac; rewrite_thm "equals_bool" 1; split_tac;
flatten_tac; inst_tac 1 ["x_1"]; inst_tac 2 ["x_1"]; flatten_tac; basic;
flatten_tac; split_tac;
flatten_tac; inst_tac 1 ["x_1"]; flatten_tac; basic;
flatten_tac; inst_tac 2 ["x_1"]; flatten_tac; basic];;

prove_theorem "exists_and"
 "!P Q: (?x: (P x) and (Q x)) => ((?x: P x)  and (?x: Q x))"
[flatten_tac; split_tac; repeat((inst_tac 1 ["x_1"])++basic)];;

prove_theorem "not_forall" 
  "!P: ((!x: P x)= (not(? x: not (P x))))"
[flatten_tac; rewrite_thm "equals_bool" 1; split_tac;
flatten_tac; inst_tac (-1) ["x_1"]; basic;
flatten_tac; inst_tac 1 ["x_1"]; flatten_tac; basic];;

prove_theorem "forall_not"
  "!P:((!x: not (P x))=(not (? x: P x)))"
[flatten_tac; rewrite_thm "equals_bool" 1; split_tac++flatten_tac;
inst_tac (-2) ["x_1"]; flatten_tac; basic;
flatten_tac; inst_tac 1 ["x_1"]; basic];;

prove_theorem "forall_and" 
  "!P Q: (!x: (P x) and (Q x))= ((!x: P x) and (!x: Q x))"
[flatten_tac; equals_tac 1; split_tac;
flatten_tac; inst_tac (-1) ["x_1"]; flatten_tac; basic;
flatten_tac; inst_tac (-1) ["x_1"]; flatten_tac; basic;
flatten_tac; inst_tac (-1) ["x_1"]; inst_tac (-2) ["x_1"];
split_tac; repeat basic];;

prove_theorem "forall_or"
"! P Q: ((! x: P x) or (!x: Q x)) => (!x: (P x) or (Q x))"
[flatten_tac; split_tac; (repeat ((inst_tac (-1) ["x_1"])++basic))
];;

(* functions *)

prove_theorem "congruence"
"!f g a b: ((f = g) and (a=b)) =>((f a)=(g b))"
[flatten_tac; replace (-1) 1; replace (-2) 1; eq_tac];;

prove_theorem "eta" "!f: (% x: f x) = f"
[flatten_tac; cut_thm "extensionality";
inst_tac (-1) ["% x: f_1 x"]; inst_tac (-1) ["f_1"];
split_tac; basic; flatten_tac; beta 1; eq_tac];;

end_theory();;
