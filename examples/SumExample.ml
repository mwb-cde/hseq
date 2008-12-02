(*-----
Name: SumExample.ml
Author: M Wahab <mwahab@users.sourceforge.net>
Copyright M Wahab 2006
----*)

(**
  Example of linking with the HSeq library.

  Compile with 
   hseqc -o sum SumExample.ml
  or 
   hseqc --native -o sum SumExample.ml
*)


(**
   Open HSeq modules.
*)

open HSeq
open Tactics
open Boollib
open Simplib
open Userlib

(** Initialise the system *)

let _ = 
  Global.Hooks.load_file := (fun _ -> ());
  Global.Hooks.use_file := (fun ?silent _ -> ());
  Global.init()

(** 
   Sum of types.

   (Mainly from HOL sumScript.)
 *)

let _ = begin_theory "SumExample" [];;

let sum_prec = 35;;
let sum_fixity = infixr;;

(** {5 Definition and basic properties of Sum types} *)

let mk_suml_def = 
  define 
    <:def< 
  mk_suml v = (% sel l r: ((l = v) & sel))
    >>;;

let mk_sumr_def = 
  define 
    <:def< 
  mk_sumr v = (%sel l r: ((r = v) & not sel))
    >>;;

let is_sum_def = 
  define 
    <:def<
  is_sum f = 
  ?l r: (f = (mk_suml l)) | (f = (mk_sumr r))
    >> ;;

let sum_exists = 
  theorem "sum_exists" 
    << ? p: is_sum p >>
  [
   unfold "is_sum"
     ++ inst_tac  [ <<mk_suml true >>; << true >> ; << true >> ]
     ++ flatten_tac ++eq_tac
 ];;

let sum_tydef = 
  typedef ~pp:(sum_prec, infixr, Some("+"))
    ~thm:sum_exists
    ~rep:"dest_SUM" ~abs:"make_SUM"
    <:def<: ('a, 'b)SUM = (bool -> 'a -> 'b -> bool): is_sum>>;;

let mk_suml_is_sum = 
  theorem "mk_suml_is_sum" 
    << ! x : is_sum (mk_suml x) >>
  [flatten_tac
     ++ unfold "is_sum" 
     ++ inst_tac [ <<_x>>; << any >> ]
     ++ flatten_tac ++ eq_tac];;

let mk_sumr_is_sum = 
  theorem "mk_sumr_is_sum" 
    << ! x : is_sum (mk_sumr x) >>
  [flatten_tac
     ++ unfold "is_sum" 
     ++ inst_tac [ << any >> ; <<_x>> ]
     ++ simpC];;

let mk_suml_eq = 
  theorem "mk_suml_eq"
    << !x y: ((mk_suml x) = (mk_suml y)) = (x = y) >>
  [
   flatten_tac ++ unfold "mk_suml" ++ equals_tac ++ iffE
       --
       [
	repeat
	  (once_rewriteA_tac [thm "function_eq"] 
	     ++ beta_tac)
	  ++ inst_tac [ << true >>; << _x >> ]
	  ++ simpA;
	simpC
      ]
 ];;

let mk_sumr_eq = 
  theorem "mk_sumr_eq"
    << !x y: ((mk_sumr x) = (mk_sumr y)) = (x = y) >>
  [
   flatten_tac ++ unfold "mk_sumr" ++ equals_tac ++ iffE
       --
       [
	(match_asm << X = Y >> 
	 (fun l -> 
	   repeat
	     (once_rewrite_tac ~f:l [thm "function_eq"] 
		++ beta_tac ~f:l)
	     ++ inst_tac ~f:l [ << false >>; << any >> ; << _y >> ]
	     ++ simp ~f:l));
	simpC
      ]
 ];;


(** 
   {7 Definitions}

   [inl a]: Inject on the left

   [inr a]: Inject on the right

   [isl a]: [a] is constructed from [inl]

   [isr a]: [a] is constructed from [inr]

   [outl a]: Destructor for [inl]

   [outr a]: Destructor for [inr]
 *)

let inl_def = 
  define
    <:def< inl a = make_SUM (mk_suml a) >>;;

let inr_def = 
  define
    <:def< inr b = make_SUM (mk_sumr b) >>;;

let isl_def =
  define 
    <:def< isl a = ?x: a = (inl x) >>;;

let isr_def =
  define 
    <:def< isr a = ?x: a = (inr x) >>;;

let outl_def = 
  define <:def< outl x = (@v: x = (inl v)) >>;;

let outr_def = 
  define <:def< outr x = (@v: x = (inr v)) >>;;

let dest_inl = 
  prove
    << !v: (dest_SUM (inl v)) = (mk_suml v) >>
  (flatten_tac
     ++ unfold "inl"
     ++ simpC_tac [mk_suml_is_sum; thm "make_SUM_inverse"]);;

let dest_inr = 
  prove
    << !v: (dest_SUM (inr v)) = (mk_sumr v) >>
  (flatten_tac
     ++ unfold "inr"
     ++ simpC_tac [mk_sumr_is_sum; thm "make_SUM_inverse"]);;


let rep_abs_suml =
  theorem "rep_abs_suml"
    << !x: (dest_SUM( make_SUM (mk_suml x))) = (mk_suml x) >>
  [
   flatten_tac
     ++ simpC_tac [thm "mk_suml_is_sum"; thm "make_SUM_inverse"]
 ];;

let rep_abs_sumr =
  theorem "rep_abs_sumr"
    << !x: (dest_SUM( make_SUM (mk_sumr x))) = (mk_sumr x) >>
  [
   flatten_tac
     ++ simpC_tac [thm "mk_sumr_is_sum"; thm "make_SUM_inverse"]
 ];;

let inl_thm = 
  theorem "inl_thm" 
    << ! x: (dest_SUM (inl x)) = (mk_suml x) >>
  [
   flatten_tac ++ unfold "inl"
     ++ rewrite_tac [thm "rep_abs_suml"]
     ++ eq_tac
 ];;

let inr_thm = 
  theorem "inr_thm" 
    << ! x: (dest_SUM (inr x)) = (mk_sumr x) >>
  [
   flatten_tac ++ unfold "inr"
     ++ rewrite_tac [thm "rep_abs_sumr"]
     ++ eq_tac
 ];;

let inj_on_make_SUM =
  theorem "inj_on_make_SUM"
    << inj_on make_SUM is_sum >>
  [
   cut_thm "inj_on_inverse_intro"
     ++ inst_tac [ << make_SUM >>; << is_sum >>; << dest_SUM >> ]
     ++ cut_thm "make_SUM_inverse" 
     ++ split_tac 
     -- [ basic; basic ]
 ];;

let inl_eq = 
  theorem "inl_eq"
    << !x y: ((inl x) = (inl y)) = (x = y) >>
  [ flatten_tac ++ equals_tac ++ iffE
      --
      [
       unfold "inl"
	 ++ cut_thm "inj_on_make_SUM"
	 ++ unfold "inj_on"
	 ++ inst_tac [ << mk_suml _x >> ; << mk_suml _y >> ]
	 ++ blast_tac ++ (simpC_tac [mk_suml_is_sum] // skip)
	 ++ rewrite_tac [mk_suml_eq]
	 ++ basic;
       simp
     ]
  ];;

let inr_eq = 
  theorem "inr_eq"
    << !x y: ((inr x) = (inr y)) = (x = y) >>
  [ flatten_tac ++ equals_tac ++ iffE
      --
      [
       unfold "inr"
	 ++ cut_thm "inj_on_make_SUM"
	 ++ unfold "inj_on"
	 ++ inst_tac [ << mk_sumr _x >> ; << mk_sumr _y >> ]
	 ++ blast_tac ++ (simpC_tac [mk_sumr_is_sum] // skip)
	 ++ rewrite_tac [mk_sumr_eq]
	 ++ basic;
       simp
     ]
  ];;

let inr_not_inl =
  theorem "inr_not_inl" 
    << !x y: ~((inr x) = (inl y)) >>
  [
   flatten_tac
     ++ rewrite_tac [defn "inr" ; defn "inl"]
     ++ (show 
	   << 
	 ! x y: ((is_sum x) & (is_sum y)) 
	   => (((make_SUM x) = (make_SUM y)) => (x = y))
	   >> 
	 (cut inj_on_make_SUM 
	    ++ unfold "inj_on"
	    ++ basic))
     ++ inst_tac [ << mk_sumr _x >>; << mk_suml _y >> ]
     ++ implA
     --
     [
      simpC_tac  [mk_sumr_is_sum; mk_suml_is_sum];
(*
      split_tac  
	-- 
	[ 
	  cut mk_sumr_is_sum ++ unify_tac; 
	  cut mk_suml_is_sum ++ unify_tac
	];
*)
      mp_tac
	++ unfold "mk_sumr" ++ unfold "mk_suml"
	++ once_rewrite_tac [thm "function_eq"]
	++ inst_tac [ << true >> ] ++ beta_tac
	++ once_rewrite_tac [thm "function_eq"]
	++ inst_tac [ << _y >> ] ++ beta_tac
	++ once_rewrite_tac [thm "function_eq"]
	++ inst_tac [ << _x >> ] ++ beta_tac
	++ once_rewrite_tac [thm "equals_bool"] ++ iffA
	    ++ blast_tac 
	    ++ eq_tac
    ]
 ];;

let inl_not_inr =
  theorem "inl_not_inr" 
    << !x y: ~((inl x) = (inr y)) >>
  [
   flatten_tac 
     ++ cut ~inst:[ << _y >>; << _x >> ] inr_not_inl
     ++ simpA
 ];;

let outl_thm = 
  theorem "outl_thm" 
    << ! x: ((outl (inl x)) = x) >>
  [ 
    flatten_tac
      ++ (unfold "outl")
      ++ rewrite_tac [inl_eq]
      ++ rewrite_tac [thm "choice_refl2"]
      ++ eq_tac
  ];;

let outr_thm = 
  theorem "outr_thm" 
    << (! x: ((outr (inr x)) = x)) >>
  [ 
    flatten_tac
      ++ (unfold "outr")
      ++ rewrite_tac [inr_eq]
      ++ rewrite_tac [thm "choice_refl2"]
      ++ eq_tac
  ];;

let make_SUM_onto = 
  prove 
    << 
  !a: ?f : ((a = (make_SUM f)) & (is_sum f))
    >>
  (flatten_tac
     ++ inst_tac [ << dest_SUM _a >> ]
     ++ split_tac
     --
     [
      rewrite_tac [thm "dest_SUM_inverse"] ++ eq_tac;
      cut (thm "dest_SUM_mem") ++ unify_tac
    ]);;
       
let sum_cases = 
  theorem "SUM_cases"
    << !(a: ('a + 'b)): ((?x: a = (inl x)) | (?x: a = (inr x))) >>
  [
   flatten_tac
     ++ cut ~inst:[ << _a >> ] make_SUM_onto
     ++ unfold "is_sum"
     ++ flatten_tac
     ++ rewrite_tac [defn "inl"; defn "inr"]
     ++ inst_tac [ << _l >> ] ++ inst_tac [ << _r >> ]
     ++ split_tac ++ simp
 ];;

let forall_sum = 
  theorem "forall_sum"
    << 
  !(P: ('a + 'b) ->bool) : 
    (!x: P x) = ((!x: P (inl x)) & (!x: P (inr x)))
    >>
  [
   flatten_tac ++ equals_tac ++ blast_tac
       ++ (unify_tac // skip)
     ++ cases_of  << _x >> 
     ++ simp
 ];;
    
let sum_induct=
  theorem "SUM_induct"
    << 
  !(P: ('a + 'b) ->bool) : 
    ((!x: P (inl x)) & (!x: P (inr x))) => (!x: P x)
    >>
  [
   allC ++ implC
     ++ rewriteC_tac [forall_sum]
     ++ basic
 ];;

let sum_axiom = 
  theorem "sum_axiom"
  << 
    ! f g : 
    ?! h: (!x: (h (inl x)) = (f x)) & (!x: (h (inr x)) = (g x))
  >> 
    [
      flatten_tac
     ++ (unfold "EXISTS_UNIQUE") 
     ++ betaC
     ++ scatter_tac
     -- 
     [
       (* 1 *)
       inst_tac 
	 [ << (%x: 
		 if (?v : (x = (inl v)))
		 then (_f (@v: x = (inl v)))
		 else (_g (@v: x = (inr v)))) >> ]
       ++ (show << !x: ?v: (inl x) = (inl v) >> 
	 (flatten_tac ++ inst_tac [ << _x >> ]  ++ eq_tac))
       ++ beta_tac
       ++ rewrite_tac [inl_eq]
       ++ split_tac ++ flatten_tac
       -- 
	 [
	   simpC_tac [thm "choice_refl2"; inl_eq];
	   simpC_tac [thm "choice_refl2"; inl_eq; inr_eq; inr_not_inl]
	 ];
       (* 2 *)
       once_rewriteC_tac [thm "function_eq"]
       ++ specC
       ++ cases_of << _x1 >> ++ replace_tac ++ simp
     ]
    ]

(***
let sum_axiom_alt = 
  prove
    << 
  ! f g : ?! h: ((h ++ inl) = f) & ((h ++ inr) = g)
    >> 
   (flatten_tac
     ++ (cut ~inst:[ << _f >>; << _g >> ] sum_axiom)
     ++ unfold "EXISTS_UNIQUE"
     ++ beta_tac
     ++ scatter_tac
     --
     [
       (* 1 *)
       instC [ << _x >> ]
       ++ split_tac
       ++ once_rewriteC_tac [thm "function_eq"]
       ++ simp;
       (* 2 *)
       back_tac
       ++ (repeat (match_asm << (A ++ B) = C >> eq_sym_tac))
       ++ replace_tac
       ++ scatter_tac
       --
	 [
	 (* 1 *)
	   replace_tac ~dir:rightleft
	   ++ 
	 (* 2 *)
	 (* 3 *)
	 (* 4 *)
	 ]
     ])
;;
***)

let sum_fn_exists = 
  theorem "sum_fn_exists"
  << 
  ! f g: ?h: (!x: (h(inl x)) = (f x)) & (!x: (h(inr x)) = (g x))
  >>
  [
   flatten_tac
     ++ cut ~inst:[ << _f >>; << _g >> ] sum_axiom
     ++ unfold "EXISTS_UNIQUE"
     ++ betaA
     ++ flatten_tac
     ++ instC [ << _x >> ]
     ++ blast_tac ++ unify_tac
 ];;

let sum_unique =
  theorem "sum_unique"
    <<
  ! f g a b :
    (((!x: (a (inl x)) = (f x)) & (!x: (a (inr x)) = (g x)))
    & ((!x: (b (inl x)) = (f x)) & (!x: (b (inr x)) = (g x))))
    =>
  (a = b)
    >>
[
 flatten_tac
   ++ cut ~inst:[ << _f >>; << _g >> ] sum_axiom
   ++ unfold "EXISTS_UNIQUE"
   ++ beta_tac
   ++ flatten_tac
   ++ back_tac
   ++ split_tac ++ basic
];;

let isl_thm =
  theorem "isl_thm"
    << (!x: (isl (inl x))) & (!x: ~(isl (inr x))) >>
  [
   unfold "isl"
     ++ blast_tac
     -- 
     [
       instC [ << _x >> ] ++ eq_tac; 
       simpA_tac [inr_not_inl]
    ]
 ];;

let isr_thm =
  theorem "isr_thm"
    << (!x: (isr (inr x))) & (!x: ~(isr (inl x))) >>
  [
   unfold "isr"
     ++ blast_tac
     -- 
     [
      instC [ << _x >> ] ++ eq_tac; 
      cut inl_not_inr
	++ instA [ << _x >>; << _x1 >> ] 
	++ blast_tac
    ]
 ];;

let isl_outl =
  theorem "isl_outl"
    << ! x: (isl x) => ((inl (outl x)) = x) >>
  [
   flatten_tac
     ++ cases_of << _x >> ++ replace_tac
     --
     [
      rewriteC_tac [outl_thm] ++ eq_tac;
      cut isl_thm 
	++ flatten_tac
	++ inst_tac [ << _x1 >> ]
	++ inst_tac [ << _x1 >> ]
	++ blast_tac
    ]
 ];;

let isr_outr =
  theorem "isr_outr"
    << ! x: (isr x) => ((inr (outr x)) = x) >>
  [
   flatten_tac
     ++ cases_of << _x >> ++ replace_tac
     --
     [
      cut isr_thm 
	++ flatten_tac
	++ inst_tac [ << _x1 >> ]
	++ inst_tac [ << _x1 >> ]
	++ blast_tac;
      rewriteC_tac [outr_thm] ++ eq_tac
    ]
 ];;

let sum_map =
  define 
    <:def< 
  map f g x = 
  if (isl x) then (inl (f (outl x))) else (inr (g (outr x)))
    >>;;

let map_thm = 
  theorem "map_thm"
    << 
  (! f g x : ((map f g) (inl x)) = (inl (f x)))
    & 
  (! f g x : ((map f g) (inr x)) = (inr (g x)))
    >>
[
 split_tac ++ flatten_tac ++ unfold "map" 
   ++ simpC_tac [isl_thm; outl_thm; outr_thm]
];;


let _ = end_theory();;


