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

(***
* Main Definitions
***)

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

let remove_def = 
  define <:def< remove x A = {y: (y in A) & ~(y = x) }>>;;

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

let subset_def = 
  define
  <:def< subset A B = !x: x in A => (x in B) >>
      ~pp:(225, infixr, Some("<=")) ;;

let psubset_def = 
  define
  <:def< psubset A B = ~(A=B) & (A <= B) >>
  ~pp:(225, infixr, Some("<"));;


(***
* Finite sets
***)
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
  theorem "finite_induct"
  <<
  !P:
    ((P empty)
     & (! x A: (~(x in A) & (P A)) => (P (add x A))))
  => 
  !A : (finite A) => (P A)
  >>
  [flatten_tac 
   ++ unfold "finite"
   ++ back_tac
   ++ blast_tac ++ back_tac ++ blast_tac];;  


let finite_rules = 
  theorem ~simp:false "finite_rules"
  << 
    (finite empty)
  & (!x A: (~(x in A) & (finite A)) => (finite (add x A)))
  >>
    [rewrite_tac [defn "finite"]
     ++ blast_tac
     ++ (match_asm << !P: A => (P x) >>
	 (fun l -> instA ~a:l [ << _P >> ]))
     ++ (implA -- [split_tac ++ basic])
     ++ back_tac 
     ++ blast_tac];;


(***
* Primitive properties
***)

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

let add_thm = 
  theorem "add_thm"
    << ! x y A : (y in (add x A)) = ((y = x) | (y in A)) >> 
  [ flatten_tac ++ simp_tac [ defn "add" ] ]

let remove_thm = 
  theorem "remove_thm"
    << ! x y A: (y in (remove x A)) = ((y in A) & ~(y = x)) >>
  [ flatten_tac ++ simp_tac [ defn "remove" ] ]

let neg_thm =
  theorem ~simp:true "neg_thm"
    << ! x A: (x in (neg A)) = ~(x in A) >>
  [ simp_tac [defn "neg"] ];;

let union_thm =
  theorem "union_thm"
  << !x A B: (x in (union A B)) = ((x in A) | (x in B)) >>
  [ simp_tac [defn "union"] ];;

let inter_thm =
  theorem "inter_thm"
  << !x A B: (x in (inter A B)) = ((x in A) & (x in B)) >>
  [ simp_tac [defn "inter"] ];;


(***
* Membership
***)

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

let in_add = 
  prove_thm ~simp:true "in_add"
  << 
    (!x a S: (x = a) => (x in (add a S)))
    & 
    (!x a S: ~(x = a) => (x in (add a S) = (x in S)))
  >>
  [
    split_tac
    --
      [ 
	(* 1 *)
	flatten_tac ++ simp_tac [defn "add" ];
	(* 2 *)
	seq
	  [
	    specC ++ implC;
	    simp_tac [defn "add" ]
	  ]
      ]
  ]

let in_remove=
  theorem ~simp:true "in_remove"
  <<
    (!x  A: ~(x in (remove x A)))
  & (!x y A: ~(x=y) => ((x in (remove y A)) = (x in A)))
  >>
  [
    scatter_tac
    ++ simp_all_tac [remove_thm]
    ++ equals_tac ++ scatter_tac ++ basic
  ]


(*** Properties of Add *)

let add_member = 
  prove_thm ~simp:true "add_member"
  << !x A: (x in A) => ((add x A) = A) >>
  [
    flatten_tac
    ++ rewrite_tac [set_equal]
    ++ simp_tac [add_thm]
    ++ equals_tac ++ scatter_tac ++ simp
  ];;

let add_remove =
  theorem ~simp:true "add_remove"
    << !x A: (add x (remove x A)) = (add x A) >>
  [
    simp_tac [set_equal; add_thm; remove_thm]
    ++ equals_tac ++ scatter_tac ++ basic
  ]

(** Properties of Remove *)

let remove_member = 
  theorem ~simp:true "remove_member"
    << ! x A: ~(x in A) => ((remove x A) = A) >>
  [
    simp_tac [set_equal; remove_thm] ++ flatten_tac
    ++ equals_tac ++ blast_tac
    ++ simp_all
  ]

let remove_add = 
  theorem ~simp:true "remove_add"
    << !x A: (remove x (add x A)) = (remove x A) >>
  [
    simp_tac [set_equal; add_thm; remove_thm]
    ++ equals_tac ++ blast_tac
  ]

(** Properties of neg *)

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

let union_assoc =
    theorem ~simp:true "union_assoc"
      << ! A B C: (union A (union B C)) = (union (union A B) C) >>
  [
    simp_tac [set_equal; union_thm] 
    ++ equals_tac ++ blast_tac
  ];;

let union_comm =
  theorem ~simp:true "union_comm"
    << ! A B: (union A B) = (union B A) >>
  [
    simp_tac [set_equal; union_thm]
    ++ equals_tac ++ blast_tac
  ];;
  
let union_lcomm = 
  theorem ~simp:true "union_lcomm"
    << ! A B C: (union A (union B C)) = (union B (union A C)) >>
  [
    simp_tac [set_equal; union_thm] 
    ++ equals_tac ++ blast_tac
  ];;
  
let union_trivial =
  theorem ~simp:true "union_trivial"
    << ! A: (union A A) = A >>
  [ 
    simp_tac [set_equal; union_thm] 
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
      split_tac ++ simp_tac [set_equal; union_thm]
      ++ equals_tac ++ blast_tac
    ];;

let union_add_left = 
  theorem "union_add_left"
  << ! a S T: (union (add a S) T) = (add a (union S T)) >>
  [
    simp_tac [set_equal; union_thm; add_thm]
    ++ equals_tac ++ blast_tac
  ];;

let union_add_right = 
  theorem "union_add_right"
  << ! a S T: (union S (add a T)) = (add a (union S T)) >>
  [
    simp_tac [set_equal; union_thm; add_thm]
    ++ equals_tac ++ blast_tac
  ]

(** Intersection *)

let inter_assoc =
  theorem ~simp:true "inter_assoc"
  << ! A B C: (inter A (inter B C)) = (inter (inter A B) C) >>
  [
    simp_tac [set_equal; inter_thm] 
    ++ equals_tac ++ blast_tac
  ];;

let inter_comm =
  theorem ~simp:true "inter_comm"
    << ! A B: (inter A B) = (inter B A) >>
  [
    simp_tac [set_equal; inter_thm] 
    ++ equals_tac ++ blast_tac
  ];;
  
let inter_lcomm = 
  theorem ~simp:true "inter_lcomm"
    << ! A B C: (inter A (inter B C)) = (inter B (inter A C)) >>
  [
    simp_tac [set_equal; inter_thm] 
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
      split_tac 
      ++ simp_tac [set_equal; inter_thm] 
      ++ equals_tac ++ blast_tac
    ];;

let inter_trivial =
  theorem "inter_trivial"
    << ! A: (inter A A) = A >>
  [ 
    simp_tac [set_equal; inter_thm]
    ++ equals_tac ++ blast_tac
  ];;

(** Subset *)

let subset_trival =
  theorem ~simp:true "subset_trivial"
  << ! A: (A <= A) = true >>
  [ unfold "subset" ++ flatten_tac ++ equals_tac ++ blast_tac ];;

let subset_absorb =
theorem ~simp:true "subset_absorb"
  <<
  (! A: (empty <= A) = true)
  & (! A: (A <= univ) = true)
  >>
  [unfold "subset" ++ split_tac ++ simp ++ equals_tac ++ blast_tac];;


let subset_refl =
  theorem ~simp:true "subset_refl"
  << ! A : A <= A >>
    [ unfold "subset" ++ flatten_tac ++ basic]

let subset_trans =
  theorem "subset_trans"
    << !A B C: ((A <= B) & (B <= C)) => (A <= C) >>
    [ 
      unfold "subset" ++ flatten_tac 
      ++ simp
    ]
  

let subset_empty = 
  theorem ~simp:true "subset_empty"
    << ! A: (A <= {}) = (A = {}) >>
  [
    flatten_tac ++ equals_tac ++ iffC
      --
    [
      (* 1 *)
      once_rewrite_tac [thm "set_equal"]
	++ flatten_tac
	++ unfold "subset"
	++ equals_tac ++ iffC ++ scatter_tac
	-- [simp; simp_all];
      (* 2 *)
      flatten_tac ++ simp
    ]
  ]

let subset_add = 
  theorem "subset_add" ~simp:true
  << ! x S T: ~(x in S) => ((S <= (add x T)) = (S <= T)) >>
  [
    flatten_tac
      ++ equals_tac 
      ++ scatter_tac
      ++ (unfold "subset")
      ++ flatten_tac
      ++ mp_tac
      ++ (unfold "add")
      ++ simp_all
      --
      [
	(* 1 *)
	(match_asm << X | Y >> liftA)
	++ split_tac ++ simp_all;
        (* 2 *)
	simp
      ]
  ]



(** Finite *)

let empty_finite = 
  theorem ~simp:true "empty_finite"
  << (finite {}) >>
    [simp_tac [finite_rules]]

let add_finite = 
  theorem ~simp:true "add_finite"
  << !x A: (finite A) => (finite (add x A)) >>
  [
    flatten_tac;
    (cases_tac << _x in _A >>
       --
       [
	 (* 1 *) 
	 seq
	   [ 
	     cut finite_rules ++ conjA;
	     back_tac;
	     simp
	   ];
	 (* 2 *)
	 simp
       ])
  ]

let union_finite0 = 
  prove
  << ! A B : (finite A) => (finite B) => (finite (union A B)) >>
  (induct_tac (thm "finite_induct") ++ flatten_tac
      --
      [
	(* 1 *)
	simp;
	(* 2 *)
	seq
	  [
	    rewrite_tac [union_add_left];
	    simp_tac [add_finite]
	  ]
      ])

let union_finite = 
  theorem ~simp:true "union_finite"
  << ! A B : ((finite A) & (finite B)) => (finite (union A B)) >>
  [
    flatten_tac;
    cut_mp_tac ~inst:[<< _A >>; << _B >>] union_finite0;
    simp
  ]


let _ = end_theory();;

