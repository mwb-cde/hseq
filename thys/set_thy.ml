new_theory "sets";;
parents["boolean"];;
new_type "set('a)=('a -> bool)";;

(* if lemmas *)

define "empty= (%x: false):set('a)";;
define "mem (a:'a) (b:set('a)) = (b a)";;
define "add (a:'a) (b:set('a)) = (%x: (x=a) or (mem x b)):set('a)";;
define 
  "remove (a: 'a) (b: set('a))=(%x: if (x=a) false (mem a b))";; 

define "subset (a: set('a)) (b:set('a))=
  !x: (mem x a) => (mem x b)";;
define "psubset (a: set('a)) (b:set('a))= (subset a b) and (not (a = b))";;

prove_theorem "mem_empty" "!x: not (mem x empty)"
[ unfold "mem" 1; unfold "empty" 1; beta 1;flatten_tac; bool_tac];;

prove_theorem "extension" "!a b: (a=b) iff (! x: (mem x a) =(mem x b))"
[flatten_tac; split_tac; replace (-1) 1; flatten_tac; eq_tac;
cut_thm "extensionality"; inst_tac ["a_1"; "b_1"] (-1);
split_tac; basic; flatten_tac; unfold "mem" (-1); 
unify_tac (-1) 1];;

define "finite (a: set('a)) =
(!(P:set('a) -> bool): 
   ((P empty) 
and (! (b: set('a)): ((finite b) and (P b))
       => (!(x:'a): (not (mem x b)) => (P (add x b)))))
   => (P a))";;

prove_theorem "mem_add"  "!x a: mem x (add x a)"
[flatten_tac; unfold "mem" 1; unfold "add" 1; beta 1;
flatten_tac; eq_tac];;

prove_theorem "mem_add1" "!x a: (mem x a) => ((add x a) =a )"
[flatten_tac; cut_thm "extension"; inst_tac ["add x_1 a_1"; "a_1"] (-1);
rewrite_thm "equals_iff" (-1); replace (-1) 1;flatten_tac;
equals_tac 1; delete (-1); split_tac; basic; 
unfold "mem" 1; unfold "add" 1; beta 1; flatten_tac; basic];;

(* induction on finite sets *)


prove_theorem "empty_finite"
  "finite empty"
[unfold "finite" 1; flatten_tac; basic];;

prove_theorem "add_finite" "!x a: (finite a) => (finite (add x a))"
[
flatten_tac; unfold "finite" 1; flatten_tac; 
copy_asm (-3);  unfold "finite" (-3); 
inst_tac ["P_1"] (-3);split_tac; postpone; 
split_tac; flatten_tac; inst_asm ["b_1"]; split_tac;
inst_asm ["x_1"]; split_tac; basic; flatten_tac; basic;
split_tac; basic; basic; basic; inst_asm ["a_1"]; 
split_tac;  inst_asm ["x_1"];  split_tac;  basic; 
flatten_tac; cut_thm "mem_add1"; inst_asm ["x_1"; "a_1"]; 
mp_tac; replace (-1) 1; basic; split_tac; basic; basic
];;

prove_theorem "simple_finite_induce"
"!P : ((P empty) and (!a x: (P a) => (P (add x a))))
      => (!a: (finite a) => (P a))"
[
flatten_tac; unfold "finite" (-3); inst_tac ["P_1"] (-3); split_tac;
basic; split_tac; 
flatten_tac; inst_tac ["b_1"; "x_1"] (-4); flatten_tac;
basic; basic] ;;


close_theory();;

