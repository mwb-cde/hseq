(* 
   Function to prepare theorems and assumptions being added to a simpset.
 *)
(*
   #load "simputils.cmo";;
   #load "simpset.cmo";;
   #load "simplifier.cmo";;
   let _ = Boollib.BaseTheory.init(); Tpenv.init()
 *)
(*
module Simpconvs =
  struct
*)
    open Simputils

    open Boollib
    open Tactics




(* 
   [make_n_ax()]: prove theorem n
   [get_n_ax()]: get theorem n, proving it if necessary

   [iff_equals_ax]:  |- !x y: (x iff y) = (x = y)
 *)
    let make_iff_equals_ax ()=
      let iff_l1= 
	Goals.prove << !x y: (x = y ) => (x => y) >>
	(flatten_tac ++ replace_tac ++ basic)
      in 
      let iff_l2 = Goals.prove
	  <<!x y: ((x => y) and (y => x)) => (x=y)>>
	(flatten_tac
	   ++ (cut_thm "bool_cases" ++ allA <<x_1>>)
	   ++ (cut_thm "bool_cases" ++ allA <<y_1>>)
	   ++ split_tac 
	   ++ 
	   orl 
	   [(replace_tac ++ (basic || trivial));
            (basic || trivial);
	    (replace_tac ++ eq_tac)])
      in 
      let iff_l3 = 
	Goals.prove << !x y: (x iff y) iff (x = y) >>
	  ((flatten_tac ++ unfold "iff" ~f:(!!1) 
	      ++ conjC ++ flatten_tac)
	     --
	     [cut iff_l2 ++ inst_tac [<<x_1>>; <<y_1>>]
		 ++ split_tac ++ orl [ basic; flatten_tac ++ basic ];
	      split_tac ++ flatten_tac ++ replace_tac ++ basic])
      in 
      Goals.prove <<!x y: (x iff y) = (x = y)>>
      ((flatten_tac ++ cut iff_l2
	  ++ inst_tac [<<x_1 iff y_1>>; <<x_1 = y_1>>]
	  ++ split_tac)
	 --
	 [flatten_tac
	    ++ cut iff_l2 ++ inst_tac [<<x_1>>; <<y_1>>]
		++ unfold "iff" ~f:(!~2)
		++ (implA --  [basic; basic]);
	  flatten_tac
	    ++ replace_tac
	    ++ unfold "iff" ~f:(!! 1)
	    ++ split_tac ++ flatten_tac ++ basic;
	  replace_tac
	    ++ eq_tac])

    let iff_equals_ax = ref None
    let get_iff_equals_ax ()=
      match !iff_equals_ax with
	None -> 
	  let nthm = make_iff_equals_ax()
	  in 
	  iff_equals_ax := Some(nthm);
	  nthm
      | Some(x) -> x


(*
   [equals_iff_ax]:  |- !x y: (x = y) = (x iff y)
 *)
    let make_equals_iff_ax ()=
      Goals.prove << !x y: (x = y) = (x iff y) >>
      (flatten_tac 
	 ++ (rewrite_tac [get_iff_equals_ax()])
	 ++ eq_tac)

    let equals_iff_ax = ref None
    let get_equals_iff_ax ()=
      match !equals_iff_ax with
	None -> 
	  let nthm = make_equals_iff_ax()
	  in 
	  equals_iff_ax := Some(nthm);
	  nthm
      | Some(x) -> x


(**
   [bool_eq_ax]: |- !x y: x iff y = ((x => y) and (y=>x))
 *)
    let make_bool_eq_ax () = 
      Goals.prove << !x y: (x=y) = ((x => y) and (y => x)) >>
      (flatten_tac 
	 ++ rewrite_tac [get_equals_iff_ax()]
	 ++ unfold "iff"
	 ++ (split_tac ++ flatten_tac ++ split_tac ++ flatten_tac ++ basic))

    let bool_eq_ax = ref None
    let get_bool_eq_ax ()=
      match !bool_eq_ax with
	None -> 
	  let nthm = make_bool_eq_ax()
	  in 
	  bool_eq_ax := Some(nthm);
	  nthm
      | Some(x) -> x
	    

(**
   [double_not_ax]: |- ! x: x = (not (not x))
 *)
    let make_double_not_ax () = 
      Goals.prove << !x: x=(not (not x)) >> 
      (flatten_tac ++ rewrite_tac [get_bool_eq_ax()]
	 ++ split_tac ++ flatten_tac ++ basic)

    let double_not_ax = ref None
    let get_double_not_ax ()=
      match !double_not_ax with
	None -> 
	  let nthm = make_double_not_ax()
	  in 
	  double_not_ax := Some(nthm);
	  nthm
      | Some(x) -> x

(* 
   [rule_true_ax]:  |- !x: x = (x=true) 
 *)
    let make_rule_true_ax ()= 
      let rule_true_l1 =  
	Goals.prove <<!x: (x=true) => x>> 
	(flatten_tac ++ replace_tac ++ trivial)
      in
      let rule_true_l2 = 
	Goals.prove <<!x: x => (x=true)>>
	((flatten_tac ++ (cut_thm "bool_cases") ++ (allA << x_1 >>) ++ disjA)
	   -- 
	   [basic;
	    rewrite_tac [Commands.lemma "false_def"]++replace_tac ++ flatten_tac])
      in
      let rule_true_l3 = 
	Goals.prove <<! x: x iff (x=true)>>
	  ((flatten_tac ++ unfold "iff" ~f:(!! 1) ++ conjC)
	     --
	     [cut rule_true_l2 ++ unify_tac ~a:(!~1) ~c:(!! 1); 
	      cut rule_true_l1 ++ unify_tac ~a:(!~1) ~c:(!! 1)])
      in 
      Logic.ThmRules.rewrite_conv (Tpenv.scope()) 
	[get_iff_equals_ax()] rule_true_l3

    let rule_true_ax = ref None

    let get_rule_true_ax ()= 
      match !rule_true_ax with
	None -> 
	  let nthm =make_rule_true_ax()
	  in 
	  rule_true_ax:=Some(nthm);
	  nthm
      | Some(t) -> t

(*
   rule_false_ax: !x: (not x) = (x=false)
 *)
    let make_rule_false_ax ()= 
      Goals.prove <<! x : (not x)=(x=false)>>
      ((flatten_tac 
	  ++ once_rewrite_tac [get_equals_iff_ax()]
	  ++ unfold "iff"
	  ++ split_tac ++ flatten_tac)
	 -- 
	 [
	  cut_thm "bool_cases" ++ inst_tac [<<x_1>>]
	    ++
	    (split_tac 
	       ++ replace_tac 
	       ++ (trivial || eq_tac));
	  replace_tac ++ trivial])

    let rule_false_ax = ref None
    let get_rule_false_ax ()= 
      match !rule_false_ax with
	None -> 
	  let nthm =make_rule_false_ax()
	  in 
	  rule_false_ax:=Some(nthm);
	  nthm
      | Some(t) -> t

(*
   [cond_rule_true_ax] : |- !x y: (x=>y) = (x => (y=true))
 *)
    let make_cond_rule_true_ax()=
      Goals.prove << !x y: (x=>y) = (x => (y=true)) >>
      (flatten_tac
	 ++ cut (get_rule_true_ax()) ++ inst_tac [<< y_1 >>]
	 ++ once_replace_tac
	 ++ eq_tac)

    let cond_rule_true_ax = ref None
    let get_cond_rule_true_ax ()= 
      match !cond_rule_true_ax with
	None -> 
	  let nthm =make_cond_rule_true_ax()
	  in 
	  cond_rule_true_ax:=Some(nthm);
	  nthm
      | Some(t) -> t


(*
   [cond_rule_false_ax]: |- !x y: (x=>~y) = (x => (y=false))
 *)
    let make_cond_rule_false_ax()=
      Goals.prove << !x y: (x=>(not y)) = (x => (y=false)) >>
      (flatten_tac
	 ++ cut (get_rule_false_ax()) ++ inst_tac [<< y_1 >>]
	 ++ once_replace_tac
	 ++ eq_tac)

    let cond_rule_false_ax = ref None
    let get_cond_rule_false_ax ()= 
      match !cond_rule_false_ax with
	None -> 
	  let nthm =make_cond_rule_false_ax()
	  in 
	  cond_rule_false_ax:=Some(nthm);
	  nthm
      | Some(t) -> t


(** {c6} Rewriting applied inside topmost universal quantifiers *)

(** [once_rewrite_rule scp rules thm]: 
   rewrite [thm] with [rules] once.
 *)
    let once_rewrite_rule scp rules thm =
      let ctrl = {Formula.default_rr_control with Rewrite.depth=Some(1)}
      in 
      Logic.ThmRules.rewrite_conv ~ctrl:ctrl scp rules thm


(** [conv_rule scp conv thm]
   apply conversion [conv] to theorem [thm]
 *)
    let conv_rule scp conv thm =
      let rule = conv scp (Formula.dest_form (Logic.dest_thm thm))
      in 
      once_rewrite_rule scp [rule] thm

(**
   [simple_rewrite_conv scp rule trm]

   Form an equality from term [trm=!x .. y: body] and [rule=(l=r)] by
   descending through topmost universal quantifiers of [trm] and
   applying rewrite once only to the body of [trm]. Return the theorem
   stating the equality [(!x..y: body)=(!x.. y: (body=r))].

   e.g 
   [simple_rewrite_conv |- ! a: a = true,  |- !x y z: (f x y z) ]
   ->
   [ |- (!x y z: f x y z) = (! x y z: (f x y z = true))  ]
 *)
    let simple_rewrite_conv scp rule trm=
      let rule_trm =(Formula.dest_form (Logic.dest_thm rule))
      in 
      let rvars, rbody = Term.strip_qnt Basic.All rule_trm
      in 
      let rlhs, rrhs = Logicterm.dest_equality rbody
      in 
      let trmvars, trmbody = Term.strip_qnt Basic.All trm
      in 
      let info = Drule.mk_info()
      in 
      let env=Unify.unify scp (Rewrite.is_free_binder rvars) rlhs trmbody
      in 
      let new_goal =
	Term.rename
	  (Logicterm.mk_equality 
	     (Drule.rebuild_qnt Basic.All trmvars trmbody)
	     (Drule.rebuild_qnt Basic.All trmvars (Term.subst env rrhs)))
      in 
      let proof g= 
	seq
	  [
	   once_rewrite_tac [get_bool_eq_ax()] ~f:(fnum 1);
	   Logic.Rules.conjC None (fnum 1)
	     --
	     [
	      seq
		[Logic.Rules.implC (Some info) (fnum 1);
		 (fun g1-> 
		   let atag, ctag = 
		     Lib.get_two (Drule.formulas info) 
		       (Failure "simple_rewrite_conv: 1")
		   in 
		   seq 
		     [repeat (Logic.Rules.allC (Some info) (ftag ctag));
		      (fun g2 -> 
			let trms = List.rev (Drule.constants info)
			in 
			seq
			  [inst_asm ~a:(ftag atag) trms;
			   once_rewrite_tac ~f:(ftag atag) [rule];
			   basic] g2)] g1)];
	      seq 
		[(add_info_tac (fun () -> ignore(Drule.empty_info info)) ());
		 Logic.Rules.implC (Some info) (fnum 1);
		 (fun g1 -> 
		   let atag, ctag = Lib.get_two (Drule.formulas info) 
		       (Failure "simple_rewrite_conv: 1")

		   in 
		   seq 
		     [repeat (Logic.Rules.allC (Some info) (ftag ctag));
		      (fun g2 -> 
			let trms = List.rev (Drule.constants info)
			in 
			seq
			  [inst_asm ~a:(ftag atag) trms;
			   once_rewrite_tac ~f:(ftag ctag) [rule];
			   basic] g2)] g1)]]] g
      in 
      Goals.prove_goal scp new_goal proof;;


(**
   [simple_rewrite_rule scp rule thm]

   Apply [simple_rewrite_conv] to theorem [thm].
 *)
let simple_rewrite_rule scp rule thm=
  conv_rule scp (fun s -> simple_rewrite_conv s rule) thm

(**
   [simple_asm_rewrite_tac rule asm]

   Rewrite assumption [asm] with [rule] by descending through topmost
   universal quantifiers and applying rewrite once only to the body of
   [asm].  i.e.
   
   rule=|- lhs = rhs
   asm:lhs, A |- C 
   -->
   asm:rhs, A |- C
 *)
let simple_asm_rewrite_tac rule asm node=
  let sqnt=Drule.sequent node
  in 
  let (_, f)=Logic.get_label_asm asm sqnt
  and scp = Drule.scope_of node
  in 
  let thm=simple_rewrite_conv scp rule (Formula.dest_form f)
  in 
  once_rewrite_tac [thm] ~f:asm node


(** 
   [asm_rewrite info thm tg g]:

   Rewrite assumption [tg] with rule [thm] = |- a=b

   tg:a, asms |- concl
   -->
   tg:b, asms |- concl

   info: [] [tg] []
 *)
let asm_rewrite thm tg g=
  simple_asm_rewrite_tac thm (Drule.ftag tg) g

(** 
   {c6} Functions manipulating theorems, needed to
   convert theorems to rewriting rules.
 *)

(** [many_conj_conv thm]:
   Break conjunctions in theorem [thm] to a list of theorems

   |- a and b and c and 
   -->
   |- a ; |- b ; |- c ; ..
 *)
let many_conj_conv thm=
  let rec many_aux t ths=
    if(Formula.is_conj (Logic.dest_thm t))
    then 
      match (Logic.ThmRules.conjE_conv t) with 
	[a; b] ->
	  many_aux a (many_aux b ths)
      | _ -> raise 
	    (Logic.logicError "many_conj_conv: unknown error" 
	       [Logic.dest_thm t])
    else t::ths
  in many_aux thm []


(** [negate_concl info t g]:
   Negate conclusion [t], making it assumption tagged [t'].

   asms|- t:c, cncl
   -->
   t':~c, asms |- cncl
   info [] [t'] []
 *)
let negate_concl info c goal=
  let inf= Drule.mk_info()
  in 
  let add_fn x = 
    Logic.add_info info [] (Drule.formulas x) []
  in 
  seq [ once_rewrite_tac [get_double_not_ax()] ~f:c;
	Logic.Rules.negC (Some inf) c;
	add_info_tac add_fn inf] goal


(** Tests on theorems *)

(** [is_many_conj thm]:
   test [thm] is of the form |- a and b and ... and z 
 *)
let is_many_conj thm=
  let thmtrm=Formula.dest_form (Logic.dest_thm thm)
  in 
  Logicterm.is_conj thmtrm

(** [is_iffterm (vars, cnd, main)]: 
   true if [main] is of the for [a iff b] 
 *)
let is_iffterm (vars, cnd, main) =
  (try(fst(Term.dest_fun main) = Logicterm.iffid) with _ -> false)

(** [is_negation (var, cnd, main):  
   true if [main] is of the form [not a]
 *)
let is_negation (vars, cnd, main)=
  Logicterm.is_neg main

(** [is_equality (var, cnd, main): 
   true if [main] is of the form a=b 
 *)
let is_equality (vars, cnd, main)=
  Logicterm.is_equality main


(*
   Two types of rule: theorems and assumptions 
   for both: a formula f is transformed to T(f) as follows:

   T(x and y) = T(x), T(y)
   T(x iff y) = T(x=y), if this results in a rewrite rule
   T(not x) = x=false
   T(x) = x=true

   conditional formulas: 
   T(c=>x) transformed as above 
   except 
   T(c=>(x and y)) = c=>(x and y)

   rewrite rules:
   T(x=y) = x=y, if all variables in y also occur in x 
   = (x=y)=true

   T(c=>x=y) 
   = c=>(x=y), if all variables in y and c occur in x
   = c=>((x=y)=true), if variables in y don't occur in x 
   and all variables in c occur in x
   = (c=>x=y)=true, otherwise
 *)


(* Utility functions *)

(** [find_variables is_var vars trm]
   find all subterms [t] of [trm] s.t. [(is_var t)] is true.
   add [t] to [vars]
   return [vars]
 *)
let find_variables is_var vars trm=
  let rec find_aux env t=
    match t with
      Basic.Qnt(_, _, b) ->
	find_aux env b
    | Basic.Bound(q) ->
	if(is_var q)
	then 
	  (try ignore(Term.find t env); env
	  with 
	    Not_found ->
	      (Term.bind t t env))
	else env
    | Basic.Typed(tr, _) -> find_aux env tr
    | Basic.App(f, a) -> 
	let nv=find_aux env f
	in find_aux nv a
    | _ -> env
  in find_aux vars trm

(** [check_variables is_var vars trm]
   check that all subterms [t] of [trm] s.t. [is_var t] 
   is in [vars]
 *)   
let check_variables is_var vars trm=
  let rec check_aux t=
    match t with
      Basic.Qnt(_, _, b) ->
	check_aux b
    | Basic.Bound(q) ->
	if(is_var q)
	then 
	  ignore(Term.find t vars)
	else ()
    | Basic.Typed(tr, _) -> check_aux tr
    | Basic.App(f, a) -> check_aux f; check_aux a
    | _ -> ()
  in check_aux trm

(**  [is_rr_rule qs c l r]: 

   Check that [c=>(l=r)] is a rewrite rule.

   All variables (in [qs]) occuring in [c] or [r] must also occur in [l]
   return: [(cnd, rhs)]
   where 
   [cnd]= [Some(true)] iff all variables in [c] occur in [l]
   = [None] if no condition
   [rhs]= [Some(true)] iff all variables in [r] occur in [l]
   = [None] if no [rhs]
 *)
let is_rr_rule qs c l r=
  let is_var b=List.mem b qs
  and rret=ref (Some true)
  and cret=ref (Some true)
  in
  let vars=find_variables is_var (Term.empty_subst()) l
  in 
  (* check rhs *)
  (try
    match r with 
      None -> rret:=None 
    | Some (rhs) -> check_variables is_var vars rhs;
  with Not_found -> rret:=(Some false));
  (* check cond (if any) *)
  (try
    (match c with 
      None -> cret:=None 
    | Some(cnd) -> check_variables is_var vars cnd)
  with Not_found -> cret:=(Some false));
  (!cret, !rret)

(* [dest_rr_rule trm]: 
   Split rule [trm] into binders, condition, lhs, rhs 
   rules are of the form:
   [c=>(l=r)] or [l=r]
 *)


(** [strip_qnt_cond trm]
   split rule [trm] into variable binders, condition, equality
   rules are of the form:
   a=>c
   c
 *)
let strip_qnt_cond t =
  let (qs, t1)=Term.strip_qnt (Basic.All) t  (* get leading quantifiers *)
  in 
  if (Logicterm.is_implies t1)  (* deal with conditional equalities *)
  then 
    (let (_, a, c)=Term.dest_binop t1
    in 
    (qs, Some a, c))
  else
    (qs, None, t1)


(* conversions for manipulating rules *)

(** Functions to make simp rules from theorems *)


(**
   [thm_to_rule scp thm]: convert theorem [thm] to a list of theorems suitable 
   for rewriting.

   Conversion:
   |- l=r   ->  no change, if all variables in [r] also occur in [l])
   -> |- (l=r)=true, otherwise

   |- c => l = r -> no change, if all variables in [r] and [c] 
   also occur in [l]
   -> |- (c=> l = r)=true, otherwise

   |- a -> |- a=true
   |- c=> a -> |- c => a=true
   |- not a ->  |- a=false
   |- c=> not a -> |- c => a = false
   |- a and b -> |- a; |- b
   |- false -> not true
 *)

(** 
   [check_for_constant t]
   check whether [t] is a boolean constant (true/false)
   or in the form [l=r] where [r] is a boolean constant.
 *)
let check_for_constant t=
  let r = 
    if(Logicterm.is_equality t)
    then 
      let (_, _, x) = Term.dest_binop t in x
    else t
  in 
  (List.exists (Term.equals r) [Term.mk_bool true; Term.mk_bool false])

(** 
   [apply_by_test lst x]

   Apply each function in [lst], return the result of the first to 
   succeed.

   Fail if all functions in [lst] fail.
 *)
let rec apply_by_test lst x=
  match lst with
    [] -> raise (Failure "apply_by_test")
  | f::ts -> 
      try (f x) with _ -> apply_by_test ts x

(** Functions to test and convert a specific type of theorem 
   [accept_all_thms]: convert |- a to |- a=true 

   [do_rr_equality]: accept |- l=r or |= c=> l=r 

   [do_fact_rule]: 
   convert |- a to |- a=true 
   and  |- c=> a to |- c => a=true

   [do_neg_rule]: convert |- not a to |- a=false 
   and |- c=> not a to |- c=> a=false

   [do_conj_rule]: convert  |- a and b to |- a and |- b.

   [single_thm_to_rules scp thm]:
   convert a theorem stating a single fact to a rewrite-rule.

   [multi_thm_to_rules scp thm]:
   convert a theorem stating many facts to a list of rewrite-rules.

   [thm_to_rules scp thm]: Toplevel conversion function.
   Convert theorem [thm] to a list of rules
 *)

let rec accept_all_thms (scp, thm, (qs, c, a))= 
  if(check_for_constant a)
  then thm
  else once_rewrite_rule scp [get_rule_true_ax()] thm 

let do_rr_equality (scp, thm, (qs, c, a)) =
  if(Logicterm.is_equality a)
  then 
    let (_, lhs, rhs)= Term.dest_binop a
    in 
    match (is_rr_rule qs c lhs (Some rhs)) with
      (Some(true), Some(true)) -> thm
    | (None, Some(true)) -> thm
    | _ -> failwith "is_rr_equality"
  else failwith "is_rr_equality"

let do_fact_rule (scp, thm, (qs, c, a)) = 
  if(not (Logicterm.is_equality a))
  then 
    match (is_rr_rule qs c a None) with
      (None, _) -> 
	if(check_for_constant a)
	then thm
	else simple_rewrite_rule scp (get_rule_true_ax()) thm
    | (Some(true), _) -> 
	if(check_for_constant a)
	then thm
	else simple_rewrite_rule scp (get_cond_rule_true_ax()) thm
    | _ -> failwith "do_fact_rule"
  else failwith "do_fact_rule"

let do_neg_rule (scp, thm, (qs, c, a)) = 
  if(not (Logicterm.is_equality a))
  then 
    match (is_rr_rule qs c a None) with
      (None, _) -> 
	simple_rewrite_rule scp (get_rule_false_ax()) thm
    | (Some(true), _) -> 
	simple_rewrite_rule scp (get_cond_rule_false_ax()) thm
    | _ -> failwith "do_neg_rule"
  else failwith "do_neg_rule"

let single_thm_to_rules scp thm = 
  let (qs, c, a) = strip_qnt_cond (Formula.dest_form (Logic.dest_thm thm))
  in 
  apply_by_test 
    [
     do_rr_equality;
     do_neg_rule;
     do_fact_rule;
     accept_all_thms
   ] (scp, thm, (qs, c, a))


let do_conj_rule scp thm=
  if(is_many_conj thm)
  then many_conj_conv thm
  else failwith "do_conj_rule: not a conjunction"

let rec apply_get_list f ls result = 
  match ls with 
    [] -> result
  | (x::xs) -> 
      try 
	apply_get_list f xs (apply_get_list f (f x) result)
      with _ -> apply_get_list f xs (x::result)

let rec app_first fs x =
  match fs with
    [] -> failwith "app_first"
  | f::ffs -> try f x with _ -> app_first ffs x

let rec multi_thm_to_rules scp thm = 
  let fs x= app_first [do_conj_rule scp] x
  in 
  List.rev(apply_get_list fs [thm] [])

let thm_to_rules scp thm = 
  let thmlst=multi_thm_to_rules scp thm
  in 
  List.map (single_thm_to_rules scp) thmlst


(** [make_thm_rule thm]:
   make rule from theorem [thm]
 *)
let make_thm_rule thm=
  let qs, c, l, r=
    Simpset.dest_rr_rule (Formula.dest_form (Logic.dest_thm thm))
  in 
  (qs, c, l, r, Logic.RRThm thm)

(** [add_simp_rule scp set rls]
   add (properly formed rules) [rls] to set [set]
   in scope [scp]
 *)
    
(*
   let add_simp_rule sset src =
   let nset=ref sset
   in 
   let rec add_aux thms =
   match thms with
   [] -> !nset
   | rl::ts -> 
   nset:=Simpset.add_rule rl (!nset);
   add_aux ts
   in 
   add_aux src
 *)

(**
   [thm_to_entries scp thm]: convert a theorem to a list of 
   simpset entries.
 *)   
let thm_to_entries scp thm=
  let rules = thm_to_rules scp thm
  in 
  List.map make_thm_rule rules

(**
   [simpset_add_thm scp sset thm]: add rewrites from [thm] to
   simpset [sset].
 *)   
let add_simp_rule sset entries=
  List.fold_left (fun s e-> Simpset.add_rule e s) sset entries

let simpset_add_thm scp sset thm=
  let entries = thm_to_entries scp thm
  in 
  add_simp_rule sset entries

(** {6c} Converting assumptions to rewrite rules *)

(** [test_apply_tac info ttacs tg goal]

   [ttacs] is a list of test-tactic pairs.
   Each test is applied to asm tagged [tg] in first subgoal of [goal].
   The tactic [tac] of the first test to suceed is applied to [goal].
   [info] is passed to [tac].
   raises Not_found if no tactic matches.
 *)
let test_apply_tac info ttac tg goal=
  let rec app_aux ts=
    match ts with 
      [] -> raise Not_found
    | (tst, tac)::tts ->
	if(tst 
	     (Formula.dest_form
		(Logic.drop_tag 
		   (Logic.Sequent.get_tagged_asm tg 
		      (Drule.sequent goal)))))
	then 
	  tac info tg goal
	else 
	  app_aux tts 
  in 
  app_aux ttac 

(** [prep_asm_rule info tg goal]:

   Prepare assumption [tg] to be used as a rewrite rule.
 *)

(** [is_cond_fact t]
   [t] is of the form [a=>b] where [b] isn't an equality
 *)
let is_cond_true_fact trm=
  let (_, t)=Term.strip_qnt Basic.All trm 
  in 
  if(Logicterm.is_implies t)
  then 
    (let (_, args)=Term.dest_fun t
    in 
    let (asm, cncl)=Lib.get_two args (Not_found)
    in 
    not((Logicterm.is_equality cncl) or (Logicterm.is_neg cncl)))
  else false

let is_cond_false_fact trm=
  let (_, t)=Term.strip_qnt Basic.All trm 
  in 
  if(Logicterm.is_implies t)
  then 
    (let (_, args)=Term.dest_fun t
    in 
    try 
      let (asm, cncl)=Lib.get_two args (Not_found)
      in 
      (Logicterm.is_neg cncl)
    with _ -> false)
  else false

let is_false_fact trm= 
  let (_, t)=Term.strip_qnt Basic.All trm 
  in 
  Logicterm.is_neg t

let is_true_fact t= true

let cond_true_asm_to_rule info tg g=
  asm_rewrite (get_cond_rule_true_ax()) tg g

let cond_false_asm_to_rule info tg g=
  asm_rewrite (get_cond_rule_false_ax()) tg g

let false_asm_to_rule info tg g=
  asm_rewrite (get_rule_false_ax()) tg g

let true_asm_to_rule info tg g=
  asm_rewrite (get_rule_true_ax()) tg g


let asm_rule_makers=
  [
   (is_cond_false_fact, cond_false_asm_to_rule);
   (is_cond_true_fact, cond_true_asm_to_rule);
   (is_false_fact, false_asm_to_rule);
   (is_true_fact, true_asm_to_rule)
 ]

let prep_asm_rule info tg g=
  test_apply_tac info asm_rule_makers tg g


(** [term_cond_rewrite scp rl fm]:
   for rewrite rule [rl]=|-l=r,
   rewrite conditional term [fm]= c=>a=l to c=>a=r
   in scope [scp]
 *)
let term_cond_rewrite scp rl fm =
  let qs, cnd, qb=strip_qnt_cond fm
  in 
  let rrtrm = Formula.dest_form (Logic.dest_thm rl)
  in 
  let cond = dest_option cnd
  in 
  Drule.rebuild_qnt Basic.All qs 
    (Logicterm.mk_implies cond
       (Rewrite.rewrite scp
	  (Rewrite.control 
	     ~dir:Rewrite.rightleft
	     ~strat:Rewrite.BottomUp
	     ~max:None) [rrtrm] qb))
    
(** [form_cond_rewrite scp rl fm]:
   for rewrite rule [rl]=|-l=r,
   rewrite conditional formula [fm]= c=>a=l to c=>a=r
   in scope [scp]
 *)
let form_cond_rewrite scp rl fm =
  Formula.mk_form scp 
    (term_cond_rewrite scp rl (Formula.dest_form fm))

(*
end
*)
(* Previously in simpset.ml *)
(*

   open Simpset;;


   let scope () = Tpenv.scope();;

   let saxiom str = 
   Logic.mk_axiom 
   (Formula.form_of_term (scope()) (Tpenv.read str));;

   let get_tags asms g=
   let sqnt=Logic.get_sqnt g
   in 
   List.map (fun i -> Logic.Sequent.index_to_tag i sqnt) asms;;

(*
   let mk_entry asm g=
   make_asm_entry (get_tags asm g) [] g;;
 *)   
(*
   let mk_goal str= 
   Logic.mk_goal (scope()) 
   (Formula.mk_form (scope()) (read str));;

   goal <<(!a b: a=>b) => true>>;;
   by  implI;;
   let gl=curr_goal(top());;
   let sqnt=curr_sqnt gl;;
   let thm=saxiom "!a: a= (a=true)";;
   let scp=Logic.scope_of sqnt;;
   let info=ref (Logic.Rules.make_tag_record [][][]);;
   let ftg=Logic.index_to_tag (-1) sqnt;;

   let g= implI (mk_goal "(!a b: a iff b) => true");;
 *)

   let scp=scope();;

   let rl=saxiom "!a: a= (a=true)";;
   let thm=saxiom"(!a b: a=>b)";;


 *)
(*
   let scope = Tpenv.scope()

   let naxiom t= Logic.mk_axiom (Formula.mk_form (Tpenv.scope()) t)


   let axioms = List.map naxiom
   [
   << !x: (true and x) = x>>;
   <<!x: (x and true) = x>>;

   <<!x: (false and x) = false>>;
   <<!x: (x and false) = false>>;

   <<!x: (true or x) = true>>;
   <<!x: (x or true) = true>>;
   <<!x: (false or x) = x>>;
   <<!x: (x or false) = x>>;

   <<(not false) = true>>;
   <<(not true) = false>>;

   <<!x: (not (not x))=x>>;
   << !x y: (x => y) = ((not x) or y)>>;

   << (!x: true) = true >>;
   << (!x: false) = false>>;

   << ! x: (x=x)=true>>;
   ];;
 *)
