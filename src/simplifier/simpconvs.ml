(*-----
 Name: simpconvs.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

let log_message = ref None
let log x = log_message:=Some x

let log_tac x g = log x; Tactics.skip g

(* 
   Function to prepare theorems and assumptions being added to a simpset.
*)

open Simputils

open Boollib
open Boollib.Props
open Boollib.Convs
open Boollib.Rules

open Tactics

(**
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


(**
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
  let rule_trm =(Logic.term_of rule)
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
	    [(data_tac (fun () -> ignore(Drule.empty_info info)) ());
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
  let thm=simple_rewrite_conv scp rule (Formula.term_of f)
  in 
  once_rewrite_tac [thm] ~f:asm node


(** 
   {c6} Functions manipulating theorems, needed to
   convert theorems to rewriting rules.
 *)


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
	data_tac add_fn inf] goal


(** Tests on theorems *)

(** [is_many_conj thm]:
   test [thm] is of the form |- a and b and ... and z 
 *)
let is_many_conj thm=
  let thmtrm=Logic.term_of thm
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


(** {6c Utility functions} *)

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

(** {6c Functions to make simp rules from theorems} *)

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
   [apply_first lst x]

   Apply each function in [lst], return the result of the first to 
   succeed.

   Fail if all functions in [lst] fail.
 *)
let rec apply_first lst x=
  match lst with
    [] -> raise (Failure "apply_first")
  | f::ts -> 
      try (f x) with _ -> apply_first ts x

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


(* The tests on terms *)

let is_constant (qs, c, t)=
  let r = 
    if(Logicterm.is_equality t)
    then 
      let (_, _, x) = Term.dest_binop t in x
    else t
  in 
  (List.exists (Term.equals r) [Term.mk_bool true; Term.mk_bool false])

let is_neg_all (qs, c, t) = 
  if (Logicterm.is_neg t)
  then 
    let (_, not_body)=Term.dest_unop t
    in
    Logicterm.is_all not_body
  else false

let is_neg_exists (qs, c, t) = 
  if (Logicterm.is_neg t)
  then 
    let (_, not_body)=Term.dest_unop t
    in
    Logicterm.is_exists not_body
  else false

let is_rr_equality (qs, c, a)=
  if(Logicterm.is_equality a)
  then 
    let (_, lhs, rhs)= Term.dest_binop a
    in 
    match (is_rr_rule qs c lhs (Some rhs)) with
      (Some(true), Some(true)) -> true
    | (None, Some(true)) -> true
    | _ -> false
  else false

(* The conversion functions *)

let rec accept_all_thms (scp, thm, (qs, c, a))= 
  if(is_constant (qs, c, a))
  then thm
  else once_rewrite_rule scp [get_rule_true_ax()] thm 

and do_rr_equality (scp, thm, (qs, c, a)) =
  if(is_rr_equality (qs, c, a))
  then thm
  else failwith "is_rr_equality: not a rewrite rule"

and do_fact_rule (scp, thm, (qs, c, a)) = 
  if(not (Logicterm.is_equality a))
  then 
    match (is_rr_rule qs c a None) with
      (None, _) -> 
	if(is_constant (qs, c, a))
	then thm
	else simple_rewrite_rule scp (get_rule_true_ax()) thm
    | (Some(true), _) -> 
	if(is_constant (qs, c, a))
	then thm
	else simple_rewrite_rule scp (get_cond_rule_true_ax()) thm
    | _ -> failwith "do_fact_rule"
  else failwith "do_fact_rule"

and do_neg_rule (scp, thm, (qs, c, a)) = 
  if(not (Logicterm.is_equality a))
  then 
    match (is_rr_rule qs c a None) with
      (None, _) -> 
	simple_rewrite_rule scp (get_rule_false_ax()) thm
    | (Some(true), _) -> 
	simple_rewrite_rule scp (get_cond_rule_false_ax()) thm
    | _ -> failwith "do_neg_rule"
  else failwith "do_neg_rule"

and do_neg_all_rule (scp, thm, (qs, c, a)) = 
  match c with 
    None -> 
      if (is_neg_all (qs, c, a))
      then 
	let thm1= once_rewrite_rule scp [neg_all_conv scp a] thm
	in 
	single_thm_to_rules (scp: Scope.t) (thm1: Logic.thm)
      else failwith "do_neg_all_rule: Not a negated universal quantifier"
  | Some _ ->  
      failwith 
	"do_neg_all_rule: Not an unconditional negated universal quantifier"

and do_neg_exists_rule (scp, thm, (qs, c, a)) = 
  match c with 
    None -> 
      if (is_neg_exists (qs, c, a))
      then 
	let thm1=
	  once_rewrite_rule scp [neg_exists_conv scp a] thm
	in 
	single_thm_to_rules scp thm1
      else failwith "do_neg_exists_rule: Not a negated existential quantifier"
  | Some _ ->  
      failwith 
	"do_neg_exists_rule: Not an unconditional negated existential quantifier"

and single_thm_to_rules scp thm = 
  let (qs, c, a) = strip_qnt_cond (Logic.term_of thm)
  in 
  apply_first 
    [
     do_neg_all_rule;
     do_neg_exists_rule;
     do_rr_equality;
     do_neg_rule;
     do_fact_rule;
     accept_all_thms
   ] (scp, thm, (qs, c, a))


and do_conj_rule scp thm=
  if(is_many_conj thm)
  then Boollib.Rules.conjuncts scp thm
  else failwith "do_conj_rule: not a conjunction"


and multi_thm_to_rules scp thm = 
  let fs x= app_first [do_conj_rule scp] x
  in 
  List.rev(apply_get_list fs [thm] [])

let thm_to_rules scp thm = 
  let thmlst=multi_thm_to_rules scp thm
  in 
  List.map (single_thm_to_rules scp) thmlst

(** {6c Converting assumptions to rewrite rules} *)

(** 
   [asm_rewrite thm tg g]:

   Rewrite assumption [tg] with rule [thm] = |- a=b

   tg:a, asms |- concl
   -->
   tg:b, asms |- concl
 *)
let asm_rewrite thm tg g=
  once_rewrite_tac [thm] ~f:(Drule.ftag tg) g


(** 

   Functions to convert an assumption

   [accept_asm]: convert |- a to |- a=true 

   [rr_equality_asm]: accept |- l=r or |= c=> l=r 

   [fact_rule_asm]: 
   convert |- a to |- a=true 
   and  |- c=> a to |- c => a=true

   [neg_rule_asm]: convert |- not a to |- a=false 
   and |- c=> not a to |- c=> a=false

   [neg_all_rule_asm]: convert |- not (!a: b) to |- ?a: not b
   then convert the new theorem.

   [neg_exists_rule_asm]: convert |- not (?a: b) to |- !a: not b
   then convert the new theorem.

   [conj_rule_asm]: convert  |- a and b to |- a and |- b.

   [single_asm_to_rules l g]:
   convert an assumption stating a single fact to a rewrite-rule.

   [asm_to_rules tg ret g]: Toplevel conversion function.  Convert
   assumption [tg] of goal [g] to one or more rules.  The tag of each
   rule (including [tg]) generated from [tg] is stored in [ret].
 *)

let rec accept_asm (tg, (qs, c, a)) g =
  if(is_constant (qs, c, a))
  then skip g
  else asm_rewrite (get_rule_true_ax()) tg g

and rr_equality_asm (tg, (qs, c, a)) g =
  if(is_rr_equality (qs, c, a))
  then skip g
  else failwith "rr_equality_asm: not a rewrite rule"

and fact_rule_asm (tg, (qs, c, a)) g= 
  if(not (Logicterm.is_equality a))
  then 
    match (is_rr_rule qs c a None) with
      (None, _) -> 
	if(is_constant (qs, c, a))
	then skip g
	else asm_rewrite (get_rule_true_ax()) tg g
    | (Some(true), _) -> 
	if(is_constant (qs, c, a))
	then skip g
	else asm_rewrite (get_cond_rule_true_ax()) tg g
    | _ -> failwith "do_fact_asm"
  else failwith "do_fact_asm"

and neg_rule_asm (tg, (qs, c, a)) g = 
  if(not (Logicterm.is_equality a))
  then 
    match (is_rr_rule qs c a None) with
      (None, _) -> 
	asm_rewrite (get_rule_false_ax()) tg g
    | (Some(true), _) -> 
	asm_rewrite (get_cond_rule_false_ax()) tg g
    | _ -> failwith "neg_rule_asm"
  else failwith "neg_rule_asm"

and neg_all_rule_asm (tg, (qs, c, a)) g= 
  let scp = Drule.scope_of g
  in 
  match c with 
    None -> 
      if (is_neg_all (qs, c, a))
      then 
	(asm_rewrite (neg_all_conv scp a) tg 
	   ++ single_asm_to_rule tg) g
      else failwith "neg_all_rule_asm: Not a negated universal quantifier"
  | Some _ ->  
      failwith 
	"neg_all_rule_asm: Not an unconditional negated universal quantifier"

and neg_exists_rule_asm (tg, (qs, c, a)) g= 
  let scp = Drule.scope_of g
  in 
  match c with 
    None -> 
      if (is_neg_exists (qs, c, a))
      then 
	(asm_rewrite (neg_exists_conv scp a) tg 
	   ++ single_asm_to_rule tg) g
      else failwith "neg_exists_rule_asm: Not a negated existential quantifier"
  | Some _ ->  
      failwith 
	"neg_exists_rule_asm: Not an unconditional negated existential quantifier"

and single_asm_to_rule tg goal = 
  let trm = 
    Formula.term_of
      (Logic.drop_tag 
	 (Logic.Sequent.get_tagged_asm tg 
	    (Drule.sequent goal)))
  in 
  let (qs, c, a) = strip_qnt_cond trm
  in 
  apply_first 
    [
     neg_all_rule_asm (tg, (qs, c, a));
     neg_exists_rule_asm (tg, (qs, c, a));
     rr_equality_asm (tg, (qs, c, a));
     neg_rule_asm (tg, (qs, c, a));
     fact_rule_asm (tg, (qs, c, a));
     accept_asm (tg, (qs, c, a))
   ]  goal

let asm_to_rules tg ret goal = 
  ret := [tg];
  single_asm_to_rule tg goal

(* 
   [prepare_concls data cs except g]

   Copy and lift the conclusions labelled with a tag in [cs] into the
   assumptions (by negation) then prepare the assumption for use as a
   simp rule.  Ignore conclusions for which [except] is true. Set
   [!data] to the list of pairs [(c, a)] where [c] is the tag of the
   original conclusion and [a] is the tag of the new assumption (which
   is to be used as a simp rule).
 *)
let prepare_concl data except c goal =
  if(except c)
  then skip goal
  else 
    let info=Drule.mk_info()
    and new_asm_tags = ref []
    in 
    let data_fn () = 
      let l = List.map (fun t -> (c, t)) (!new_asm_tags)
      in 
      new_asm_tags:=[];
      data:=List.append l (!data)
    in 
    seq [Logic.Rules.copy_cncl (Some info) (Drule.ftag c); 
	 (fun g -> 
	   let c1 = 
	     Lib.get_one (Drule.formulas info) 
	       (Failure "Simplib.prepare_concl")
	   in 
	   ignore(Drule.empty_info info);
	   negate_concl (Some info) (Drule.ftag c1) g); 
	 (fun g ->
	   let a=
	     Lib.get_one (Drule.formulas info) 
	       (Failure "Simplib.prepare_concl")
	   in 
	   seq 
	     [asm_to_rules a new_asm_tags;
	      data_tac data_fn ()] g)
       ] goal


let prepare_concls data cs except goal=
  let d=ref []
  in 
  seq
    [Tactics.each cs (prepare_concl d except);
     data_tac (fun () -> data:=(!d)) ()] goal


(* 
   [prepare_asms data cs except g]

   Copy the assumptions labelled with a tag in [cs] then prepare the
   copy for use as a rewrite rule.  Ignore assumptions for which
   [except] is true. Set [!data] to the list of pairs [(a, na)] where
   [a] is the tag of the original assumption and [na] is the tag of the
   new assumption generated from [a].
 *)

let prepare_asm data except a goal =
  if(except a)
  then skip goal
  else 
    let info=Drule.mk_info()
    and new_asm_tags = ref []
    in 
    let data_fn () = 
      let l = List.map (fun t -> (a, t)) (!new_asm_tags)
      in 
      new_asm_tags:=[];
      data:=List.append l (!data)
    in 
    seq [Logic.Rules.copy_asm (Some info) (Drule.ftag a); 
	 (fun g ->
	   let a1=
	     Lib.get_one (Drule.formulas info) 
	       (Failure "Simplib.prepare_asm")
	   in 
	   seq 
	     [asm_to_rules a1 new_asm_tags;
	      data_tac data_fn ()] g)
       ] goal

let prepare_asms data ams except goal=
  let d=ref []
  in 
  seq
    [
     Tactics.each ams (prepare_asm d except);
     data_tac (fun () -> data:=!d) ()
   ] goal

