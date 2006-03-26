(*-----
 Name: SetScript.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** 
   Sets, set operators and their basic properties.
*)

let _ = begin_theory "Set" ["Bool"];;

let _ = compile [] "setLib.mli";;
let _ = compile [] "setLib.ml";;

let _ = add_file ~use:true "setLib.cmo";;

let set_typedef = typedef <:def<: ('a) set = ('a -> bool) >>;;

let set_def = 
  define <:def< SET (A:('a)set) = (A:('a)set) >>;;

let empty_def = 
  define 
  <:def< (empty:('a)set) = SET(% x: false) >>
  ~pp:(Printer.default_term_prec, Printer.default_term_fixity, Some "{}");;


let in_def = 
  define 
  <:def< in x (A: ('a)set) = A x >> 
      ~pp:(220, infixn, None)

let univ_def = 
  define <:def< univ = { x: true } >>;;

let add_def = 
  define <:def< add x A = {y: (y = x) | (y in A)}>>;;

let single_def = 
  define <:def< single x  = {y: (y = x)}>>;;

let union_def = 
  define <:def< union A B = {x: (x in A) | (x in B) } >> ;;

let unions_def = 
  define <:def< Union A = {x: ? B: B in A | x in B } >>;;

let inter_def = 
  define <:def< inter A B = {x: (x in A) & (x in B) } >>;;

let inters_def = 
  define <:def< Inter A = {x: ? B: B in A & x in B } >>;;

let neg_def = 
  define <:def< neg A = { x: ~(x in A) } >>;;

let diff_def = 
  define 
  <:def< diff A B = {x: x in A & ~x in B } >>
  ~pp:(230, infixr, Some("/"));;

let subseteq_def = 
  define
  <:def< subseteq A B = !x: x in A => (x in B) >>
      ~pp:(225, infixr, Some("<=")) ;;

let subset_def = 
  define
  <:def< subset A B = ~(A=B) & (A <= B) >>
  ~pp:(225, infixr, Some("<"));;

let set_simp=
  prove_thm ~simp:true "set_simp"
  << !x A: ((SET A) x) = (A x) >> 
  [
    flatten_tac
    ++ unfold "SET"
    ++ eq_tac
  ];;

let in_simp = 
  prove_thm ~simp:true "in_simp"
  << !x A: (x in (SET(A)))  = (A x) >>
  [
    flatten_tac ++ unfold "in" ++ unfold "SET"
    ++ eq_tac
  ];;

let in_add=
  prove_thm ~simp:true "in_add"
  <<
    !x y A: ((x = y) | (x in A)) => (x in (add y A))
  >>
  [
    flatten_tac
    ++ unfold "add"
    ++ simp
  ];;

let not_in_empty =
  prove_thm ~simp:true "not_in_empty"
  << !x: not (x in {}) >>
    [
      unfold "empty"
      ++ simp 
      ++ flatten_tac
    ];;

let in_univ=
  prove_thm ~simp:true "in_univ"
  << !x: (x in univ) >>
    [ simp_tac [defn "univ"] ];;

let in_single = 
  prove_thm ~simp:true "in_single"
  << !x a: (x in (single a)) = (x = a) >>
  [ simp_tac [defn "single" ] ];;

let set_equal =
  prove_thm "set_equal"
  << ! A B: (A = B) = (!x: (x in A) = (x in B)) >>
  [
    flatten_tac ++ equals_tac ++ scatter_tac
    --
      [ 
	simp;
	cut_back_tac (thm "extensionality")
	++ simp_all_tac [defn "in"]
      ]
  ];;

let add_member = 
  prove_thm ~simp:true "add_member"
  << !x A: (x in A) => ((add x A) = A) >>
  [
    flatten_tac
    ++ rewrite_tac [set_equal]
    ++ simp_tac [defn "add"]
    ++ (equals_tac ++ scatter_tac ++ simp)
  ];;

let finite_def=
  define 
  <:def<
    finite (X:('a)set) =
  !(P: ('a) set -> bool):
    ((P { })
     & (! x (A:('a)set): (~(x in A) & (P A)) => (P (add x A))))
  => (P X)
  >>;;

let empty_finite = 
  prove_thm ~simp:true "empty_finite"
  << finite {} >>
    [
      unfold "finite"
      ++ flatten_tac 
      ++ basic
    ];;

let add_finite = 
  prove_thm ~simp:true "add_finite"
  << !x A: (finite A) => (finite (add x A)) >>
  [
    flatten_tac 
    ++ unfold "finite"
    ++ flatten_tac
    ++ (match_asm << !p: ((p { }) & X) => Y >>
	(fun l -> inst_tac ~f:l [<< _P >> ]))
    ++ simp_all
    ++ inst_tac [<< _x >> ; << _A >> ]
    ++ scatter_tac ++ simp_all
  ];;


(** Properties of neg *)

let in_neg =
  theorem ~simp:true "in_neg"
    << ! x A: (x in (neg A)) = ~(x in A) >>
  [ simp_tac [defn "neg"] ];;

let neg_univ = 
  theorem ~simp:true "neg_univ"
    << (neg univ) = empty >>
  [ once_rewrite_tac [thm "set_equal"] ++ simp ];;

let neg_empty = 
  theorem ~simp:true "neg_empty"
    << (neg empty) = univ >>
  [ once_rewrite_tac [thm "set_equal"] ++ simp ];;

let neg_union =
  theorem ~simp:true "neg_union"
    << ! A B : (neg (union A B)) = (inter (neg A) (neg B)) >>
  [ 
    once_rewrite_tac [thm "set_equal"] 
    ++ simp_tac [defn "union"; defn "inter"]
  ];;

let neg_inter =
  theorem ~simp:true "neg_inter"
    << ! A B : (neg (inter A B)) = (union (neg A) (neg B)) >>
  [ 
    once_rewrite_tac [thm "set_equal"] 
    ++ simp_tac [defn "union"; defn "inter"]
  ];;


(** Union **)

let in_union =
  theorem ~simp:true "in_union"
  << !x A B: (x in (union A B)) = ((x in A) | (x in B)) >>
  [ simp_tac [defn "union"] ];;

let union_assoc =
    theorem "union_assoc"
      << ! A B C: (union A (union B C)) = (union (union A B) C) >>
  [
    once_rewrite_tac [thm "set_equal"] 
    ++ simp
    ++ equals_tac ++ blast_tac
  ];;

let union_comm =
  theorem "union_comm"
    << ! A B: (union A B) = (union B A) >>
  [
    once_rewrite_tac [thm "set_equal"] 
    ++ simp
    ++ equals_tac ++ blast_tac
  ];;
  
let union_lcomm = 
  theorem "union_lcomm"
    << ! A B C: (union A (union B C)) = (union B (union A C)) >>
  [
    once_rewrite_tac [thm "set_equal"] 
    ++ simp
    ++ equals_tac ++ blast_tac
  ];;
  
let union_trivial =
  theorem "union_trivial"
    << ! A: (union A A) = A >>
  [ 
    once_rewrite_tac [thm "set_equal"] 
    ++ simp
    ++ equals_tac ++ blast_tac
  ];;
  
let union_absorb =
  theorem ~simp:true "union_absorb"
    <<
    (! A: (union A empty) = A)
    & (! A: (union empty A) = A)
    & (! A: (union A univ) = univ)
    & (! A: (union univ A) = univ)
    >>
    [
      split_tac ++ once_rewriteC_tac [thm "set_equal"]
      ++ simp ++ equals_tac ++ blast_tac
    ];;

(** Intersection *)

let in_inter =
  theorem ~simp:true "in_inter"
  << !x A B: (x in (inter A B)) = ((x in A) & (x in B)) >>
  [ simp_tac [defn "inter"] ];;

let inter_assoc =
  theorem "inter_assoc"
  << ! A B C: (inter A (inter B C)) = (inter (inter A B) C) >>
  [
    once_rewrite_tac [thm "set_equal"] 
    ++ simp
    ++ equals_tac ++ blast_tac
  ];;

let inter_comm =
  theorem "inter_comm"
    << ! A B: (inter A B) = (inter B A) >>
  [
    once_rewrite_tac [thm "set_equal"] 
    ++ simp
    ++ equals_tac ++ blast_tac
  ];;
  
let inter_lcomm = 
  theorem "inter_lcomm"
    << ! A B C: (inter A (inter B C)) = (inter B (inter A C)) >>
  [
    once_rewrite_tac [thm "set_equal"] 
    ++ simp
    ++ equals_tac ++ blast_tac
  ];;
  
let inter_absorb =
  theorem ~simp:true "inter_absorb"
    <<
    (! A: (inter A empty) = empty)
  & (! A: (inter empty A) = empty)
  & (! A: (inter A univ) = A)
  & (! A: (inter univ A) = A)
    >>
    [
      split_tac ++ once_rewriteC_tac [thm "set_equal"]
      ++ simp ++ equals_tac ++ blast_tac
    ];;

let inter_trivial =
  theorem "inter_trivial"
    << ! A: (inter A A) = A >>
  [ 
    once_rewrite_tac [thm "set_equal"] 
    ++ simp
    ++ equals_tac ++ blast_tac
  ];;

(** Subseteq *)

let subseteq_trival =
  theorem ~simp:true "subseteq_trivial"
  << ! A: (A <= A) = true >>
  [ unfold "subseteq" ++ flatten_tac ++ equals_tac ++ blast_tac ];;

let subseteq_absorb =
theorem ~simp:true "subseteq_absorb"
  <<
  (! A: (empty <= A) = true)
  & (! A: (A <= univ) = true)
  >>
  [unfold "subseteq" ++ split_tac ++ simp ++ equals_tac ++ blast_tac];;


let _ = end_theory();;

