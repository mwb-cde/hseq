new_theory "pair";;

parents ["boolean"];;

define "mk_pair x y = (%a b: (a=x) and (b=y))";;
define "is_pair p = (?x y: p = (mk_pair x y))";;

prove_theorem "is_pair_mk_pair" "!x y : is_pair (mk_pair x y)"
[flatten_tac; unfold "is_pair" 1; inst_tac 1 ["x_1"; "y_1"]; eq_tac];;

prove_theorem "pair_exists" "? p: is_pair p"
[unfold "is_pair" 1; 
inst_tac 1 ["mk_pair true true"; "true"; "true"];
eq_tac];;

close_theory();;
