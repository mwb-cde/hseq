(** Inductive definitions how-to *)

(**
   Required theorems
*)

let outl_thm = thm "outl_thm"
let outr_thm = thm "outr_thm"

let isl_thm1 = 
  prove << !x: (isl (inl x)) = true >>
  (cut (thm "isl_thm") 
   ++ conjA
   ++ rewriteC_tac [thm "true_prop"; thm "false_prop"]
   ++ basic);;

let isl_thm2 = 
  prove << !x: (isl (inr x)) = false >>
  (cut (thm "isl_thm") 
   ++ conjA
   ++ rewriteC_tac [thm "true_prop"; thm "false_prop"]
   ++ basic);;

let isr_thm1 = 
  prove << !x: (isr (inr x)) = true >>
  (cut (thm "isr_thm") 
   ++ conjA
   ++ rewriteC_tac [thm "true_prop"; thm "false_prop"]
   ++ basic);;

let isr_thm2 = 
  prove << !x: (isr (inl x)) = false >>
  (cut (thm "isr_thm") 
   ++ conjA
   ++ rewriteC_tac [thm "true_prop"; thm "false_prop"]
   ++ basic);;

let implies_l1=
  prove << !x: (true => x) = x >>
  (flatten_tac ++ equals_tac ++ blast_tac);;

let implies_l2=
  prove << !x: (false => x) = true >>
  (flatten_tac ++ equals_tac ++ blast_tac);;

let conj_l1 = 
prove << !x: (true & x) = x >>
  (flatten_tac ++ equals_tac ++ blast_tac);;

let conj_l2 = 
prove << !x: (false & x) = false >>
  (flatten_tac ++ equals_tac ++ blast_tac);;

let conj_l3 = 
prove << !x: (x & true) = x >>
  (flatten_tac ++ equals_tac ++ blast_tac);;

let conj_l4 = 
prove << !x: (x & x) = x >>
  (flatten_tac ++ equals_tac ++ blast_tac);;

(** Start of example theory *)

let _ = begin_theory "indefeg" [];;


(** Single induction *)

let empty = declare << empty: 'a >>;; 
let add= declare << add : 'a -> 'b -> 'b >>;;
let in_def = declare << in: 'a -> 'b -> bool >>;;

(**
   finite empty;
   [ ~x in A; finite A ] --> finite (add x A);
*)

let finite_def=
  define 
  <:def<
    finite X =
  !P:
    ((P empty)
     & (! x A: (~(x in A) & (P A)) => (P (add x A))))
  => (P X)
  >>;;

let finite_induct =
  prove 
  <<
  !P:
    ((P empty)
     & (! x A: (~(x in A) & (P A)) => (P (add x A))))
  => 
  !A : (finite A) => (P A)
  >>
  (flatten_tac 
   ++ unfold "finite"
   ++ back_tac
   ++ blast_tac ++ back_tac ++ blast_tac);;  


let finite_rules = 
  prove 
  << 
    (finite empty)
  & (!x A: (~(x in A) & (finite A)) => (finite (add x A)))
  >>
    (rewrite_tac [defn "finite"]
     ++ blast_tac
     ++ (match_asm << !P: A => (P x) >>
	 (fun l -> instA ~a:l [ << _P >> ]))
     ++ (implA -- [split_tac ++ basic])
     ++ back_tac 
     ++ blast_tac);;

let finite_cases =
  prove 
  <<
    !X:
    (finite X) =
  ((X = empty) | (? x A: (~(x in A) & (finite A)) & (X = (add x A))))
  >>
    (specC ++ equals_tac ++ iffC 
     --
     [
       (* => *)
       cut ~inst:
	 [<< %X: 
	     (X = empty)
	 | (?x A: (~(in x A) & (finite A)) & X = (add x A))
	   >>]
	 finite_induct
       ++ betaA
       ++ implA -- [match_concl << (finite A) => B >> deleteC; 
		    instA [<<_X>>] ++ basic]
       ++ conjC
       --
	 [
	   (* 1 *)
	   (repeat disjC) ++ eq_tac;
	   (* 2 *)
	   flatten_tac ++ disjA
	   --
	     [
	       (* 1 *)
	       instC [ << _x >>; <<_A>>]
	       ++ simp_tac [finite_rules]
	       ++ blast_tac;
	       (* 2 *)
	       flatten_tac
	       ++ negA ++ negA
	       ++ (match_concl << ? X: Y >> liftC)
	       ++ instC [<< _x >> ; << add _x1 _A1 >>]
	       ++ simp_tac [finite_rules]
	     ]
	 ];
       (* <= *)
       cut finite_rules
	 ++ scatter_tac
	 ++ simp
	 ++ back_tac
	 ++ simp
     ]);;

(** Mutual induction *)

let zero = declare << zero: 'a >>;; 
let succ= declare << succ : 'a -> 'a >>;;

(**
   even zero;
   [ even x ] --> odd (succ x);
   [ odd x ] --> even (succ x)
*)

let odd_even_def =
  define
  <:def< 
    odd_even x =
  !P: 
    ((P (inl zero))
    & (! x: ((isr x) & (P x)) => (P (inl (succ (outr x)))))
    & (! x: ((isl x) & (P x)) => (P (inr (succ (outl x))))))
    => 
  (P x)
    >>;;

let even_def = 
  define <:def< even x = (odd_even (inl x)) >>;;
let odd_def = 
  define <:def< odd x = (odd_even (inr x)) >>;;

let odd_even_ind =
  prove 
  <<
  !P: 
    ((P (inl zero))
    & (! x: ((isr x) & (P x)) => (P (inl (succ (outr x)))))
    & (! x: ((isl x) & (P x)) => (P (inr (succ (outl x))))))
    => 
  (!x: (odd_even x) => (P x))
  >>
  (flatten_tac 
   ++ unfold "odd_even"
   ++ back_tac
   ++ blast_tac ++ back_tac ++ blast_tac);;


let odd_even_induct =
  prove 
  <<
    !Peven Podd: 
    ((Peven zero)
     & (! x: (Podd x) => (Peven (succ x)))
     & (! x: (Peven x) => (Podd (succ x))))
  => 
  ((!x: (odd x) => (Podd x)) 
   & (!x: (even x) => (Peven x)))
  >>
  (flatten_tac ++ split_tac ++ flatten_tac
   ++ rewriteA_tac [defn "even"; defn "odd"]
   ++ 
   (match_asm << odd_even X >>
      (fun l ->
	 (unfold "odd_even"
	  ++ instA ~a:l
	  [ << 
	      (%x: ((isr x) => (_Podd (outr x))) 
	       & ((isl x) => (_Peven (outl x))))
	    >> ]
	  ++ (betaA ~a:l))))
   ++ repeat (rewriteA_tac 
		[isl_thm1; isl_thm2; isr_thm1; isr_thm2;
		 outl_thm; outr_thm;
		 implies_l1; implies_l2; 
		 conj_l1; conj_l2; conj_l3; conj_l4])
   ++ back_tac
   ++ repeat (conjC ++ (basic //skip))
   ++ specC ++ implC ++ repeat conjA 
   ++ simp
  );;

let odd_even_rules = 
  prove
  << 
    (even zero) 
  & (!x: (odd x) => (even (succ x)))
  & (!x: (even x) => (odd (succ x))) 
    >>
    (rewrite_tac [defn "even"; defn "odd"]
       ++ rewrite_tac [defn "odd_even"]
       ++ repeat conjC ++ flatten_tac
       --
       [
	 (* 1 *)
	 basic;
	 (* 2 *)
	 (match_asm << !P: A => (P x) >>
	     (fun l -> instA ~a:l [ << _P >> ]))
	 ++ (implA -- [split_tac ++ basic])
	 ++ repeat (instA [ << inr _x >>])	
	 ++ repeat
	   (rewriteA_tac 
	      [isl_thm1; isl_thm2; isr_thm1; isr_thm2;
	       outl_thm; outr_thm;
	       implies_l1; implies_l2; 
	       conj_l1; conj_l2; conj_l3; conj_l4])
	 ++ back_tac ++ basic;
	 (* 2 *)
	 (match_asm << !P: A => (P x) >>
	     (fun l -> instA ~a:l [ << _P >> ]))
	 ++ (implA -- [split_tac ++ basic])
	 ++ repeat (instA [ << inl _x >>])
	 ++ repeat
	   (rewriteA_tac 
	      [isl_thm1; isl_thm2; isr_thm1; isr_thm2;
	       outl_thm; outr_thm;
	       implies_l1; implies_l2; 
	       conj_l1; conj_l2; conj_l3; conj_l4])
	 ++ back_tac ++ basic
       ]);;

(**
let even_cases = 
  prove
  << 
    !x: (even x) = 
  ((x = zero) | ?n: (odd n) & x=(succ n))
  >>;;
**)
