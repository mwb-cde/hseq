new_theory "nums";;

parents ["base"];;

new_full_decln "plus" "num -> num -> num" true 10 "+";;
new_full_decln "mult" "num -> num -> num" true 11 "*";;
new_full_decln "negate" "num -> num" true 11 "~";;

new_full_decln "minus" "num -> num -> num" true 10 "--";;
declare "max: num -> num -> num";;
declare "min: num -> num -> num";;

new_full_decln "greater" "num -> num -> bool" true 15 ">";;
new_full_decln "geq" "num -> num -> bool" true 15 ">=";;
new_full_decln "less" "num -> num -> bool" true 15 "<";;
new_full_decln "leq" "num -> num -> bool" true 15 "=<";;


new_axiom "nat_induction" 
"!P: 
((P 0) and 
(!a: ((a>=0) and (P a)) => (P (a+1))))
=>
(!x: (x>=0) => (P x))";;

new_axiom "strong_nat_induction" 
"!P: 
(!a : (a>=0) 
=> (!b: (b>=0) => (b<a) => (P b))
=> (P a))
=>
(!x: x>=0 => (P x))";;

close_theory();;

