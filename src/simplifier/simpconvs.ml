(*-----
 Name: simpconvs.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

let log_message = ref None
let log x = log_message:=Some x

let log_tac x g = log x; Tactics.skip g

(**
 Functions to prepare theorems and assumptions being added to a simpset.
*)

open Simputils

open Boollib
open Boollib.Thms
open Boollib.Convs
open Boollib.Rules

open Tactics

(***
* Theorems and conversions used by the simplifier.
***)

(***
  Theorems
***)

(**
   [cond_rule_true_thm] : |- !x y: (x=>y) = (x => (y=true))
*)
let make_cond_rule_true_thm()=
  let info = Tactics.mk_info () 
  in 
  Commands.prove << !x y: (x=>y) = (x => (y=true)) >>
  (allC ~info:info ++ allC ~info:info
     ++
     (fun g-> 
       let y_term, x_term = 
	 Lib.get_two (Tactics.constants info) 
	   (Failure "make_cond_rule_true_thm")
       in 
       (cut (rule_true_thm()) ++ inst_tac [ y_term ]
	  ++ once_replace_tac
	  ++ eq_tac) g))

let cond_rule_true_var = lazy (make_cond_rule_true_thm())
let cond_rule_true_thm () = Lazy.force cond_rule_true_var

(**
   [cond_rule_false_thm]: |- !x y: (x=>~y) = (x => (y=false))
 *)
let make_cond_rule_false_thm()=
  let info = Tactics.mk_info()
  in 
  Commands.prove << !x y: (x=>(not y)) = (x => (y=false)) >>
  (allC ~info:info ++ allC ~info:info
     ++ 
     (fun g -> 
       let y_term, x_term = 
	 Lib.get_two (Tactics.constants info) 
	   (Failure "make_cond_rule_false_thm")
       in 
       (cut (rule_false_thm()) ++ inst_tac [y_term]
	  ++ once_replace_tac
	  ++ eq_tac) g))

let cond_rule_false_var = lazy (make_cond_rule_false_thm())

let cond_rule_false_thm () =
  Lazy.force cond_rule_false_var

(** Rewriting applied inside topmost universal quantifiers *)

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
  let info = Tactics.mk_info()
  in 
  let env=Unify.unify scp (Rewrite.is_free_binder rvars) rlhs trmbody
  in 
  let new_goal =
    Term.rename
      (Logicterm.mk_equality 
	 (Term.rebuild_qnt trmvars trmbody)
	 (Term.rebuild_qnt trmvars (Term.subst env rrhs)))
  in 
  let proof g= 
    seq
      [
       once_rewrite_tac [bool_eq_thm()] ~f:(fnum 1);
       Logic.Tactics.conjC None (fnum 1)
	 --
	 [
	  seq
	    [Logic.Tactics.implC (Some info) (fnum 1);
	     (fun g1-> 
	       let atag = 
		 Lib.get_one (aformulas info) 
		   (Failure "simple_rewrite_conv: 1")
	       and ctag = 
		 Lib.get_one (cformulas info) 
		   (Failure "simple_rewrite_conv: 1")
	       in 
	       seq 
		 [
		  repeat (Logic.Tactics.allC (Some info) (ftag ctag));
		  (fun g2 -> 
		    let trms = List.rev (constants info)
		    in 
		    seq
		      [
		       instA ~a:(ftag atag) trms;
		       once_rewrite_tac ~f:(ftag atag) [rule];
		       basic] g2)
		] g1)
	   ];
	  seq 
	    [
	     data_tac (fun () -> empty_info info) ();
	     Logic.Tactics.implC (Some info) (fnum 1);
	     (fun g1 -> 
	       let atag = 
		 Lib.get_one (aformulas info) 
		   (Failure "simple_rewrite_conv: 1")
	       and ctag = 
		 Lib.get_one (cformulas info) 
		   (Failure "simple_rewrite_conv: 1")
	       in 
	       seq 
		 [
		  repeat (Logic.Tactics.allC (Some info) (ftag ctag));
		  (fun g2 -> 
		    let trms = List.rev (constants info)
		    in 
		    seq
		      [
		       instA ~a:(ftag atag) trms;
		       once_rewrite_tac ~f:(ftag ctag) [rule];
		       basic
		     ] g2)
		] g1)
	   ]
	]
     ] g
  in 
  Commands.prove ~scp:scp new_goal proof;;


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
let simple_asm_rewrite_tac ?info rule asm node=
  let sqnt=sequent node
  in 
  let (_, f)=Logic.get_label_asm asm sqnt
  and scp = scope_of node
  in 
  let thm=simple_rewrite_conv scp rule (Formula.term_of f)
  in 
  once_rewrite_tac ?info:info [thm] ~f:asm node

(** 
   [asm_rewrite_tac thm tg g]:

   Rewrite assumption [tg] with rule [thm] = |- a=b

   tg:a, asms |- concl
   -->
   tg:b, asms |- concl
 *)
let asm_rewrite_tac ?info thm tg g=
  once_rewrite_tac ?info:info [thm] ~f:(ftag tg) g

(** 
   Functions manipulating theorems, needed to
   convert theorems to rewriting rules.
 *)


(** 
   [negate_concl_tac info t g]: Negate conclusion [t], making it
   assumption tagged [t'].
 *)
let negate_concl_tac ?info c goal=
  let inf= mk_info()
  in 
  let add_fn x = 
    Logic.add_info info [] (aformulas x) [] []
  in 
  seq [ once_rewrite_tac [double_not_thm()] ~f:c;
	Logic.Tactics.negC (Some inf) c;
	data_tac add_fn inf] goal


(***
* Preparing simplifier rules.
***)

(** Tests on theorems *)

(** [is_many_conj thm]:
   test [thm] is of the form |- a and b and ... and z 
 *)
let is_many_conj thm=
  Logicterm.is_conj (Logic.term_of thm)

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

let is_constant (qs, c, t)=
  let r = 
    if(Logicterm.is_equality t)
    then 
      let (_, _, x) = Term.dest_binop t in x
    else t
  in 
  (List.exists (Term.equals r) [Logicterm.mk_true; Logicterm.mk_false])

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

(**  
   [is_rr_rule qs c l r]: Check that [c=>(l=r)] is a rewrite rule.

   All variables (in [qs]) occuring in [c] or [r] must also occur in [l]

   Returns [(cnd, rhs)]
   where 
   [cnd = Some(true)] iff all variables in [c] occur in [l].
   [cnd = None] if no condition.
   [rhs = Some(true)] iff all variables in [r] occur in [l].
   [rhs = None] if no [rhs].
 *)
let is_rr_rule (qs, c, l, r) =
  let is_var b=List.mem b qs
  in 
  let vars = Simputils.find_variables is_var (Term.empty_subst()) l
  in 
  let rret =
    match r with 
      None -> None 
    | Some (rhs) -> 
	try (check_variables is_var vars rhs; Some true)
	with Not_found -> Some false
  in 
  let cret =
    match c with 
      None -> None 
    | Some(cnd) -> 
	try (check_variables is_var vars cnd; Some true)
	with Not_found -> Some false
  in 
  (cret, rret)


let is_rr_equality (qs, c, a)=
  if(Logicterm.is_equality a)
  then 
    let (_, lhs, rhs)= Term.dest_binop a
    in 
    match is_rr_rule (qs, c, lhs, (Some rhs)) with
      (Some(true), Some(true)) -> true
    | (None, Some(true)) -> true
    | _ -> false
  else false

(**
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


(*** Functions to make simp rules from theorems **)

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


(* The conversion functions *)

let rec accept_all_thms (scp, thm, (qs, c, a))= 
  if(is_constant (qs, c, a))
  then thm
  else once_rewrite_rule scp [rule_true_thm()] thm 

and do_rr_equality (scp, thm, (qs, c, a)) =
  if(is_rr_equality (qs, c, a))
  then thm
  else failwith "is_rr_equality: not a rewrite rule"

and do_fact_rule (scp, thm, (qs, c, a)) = 
  if(not (Logicterm.is_equality a))
  then 
    match is_rr_rule (qs, c, a, None) with
      (None, _) -> 
	if(is_constant (qs, c, a))
	then thm
	else simple_rewrite_rule scp (rule_true_thm()) thm
    | (Some(true), _) -> 
	if(is_constant (qs, c, a))
	then thm
	else simple_rewrite_rule scp (cond_rule_true_thm()) thm
    | _ -> failwith "do_fact_rule"
  else failwith "do_fact_rule"

and do_neg_rule (scp, thm, (qs, c, a)) = 
  if(not (Logicterm.is_equality a))
  then 
    match is_rr_rule (qs, c, a, None) with
      (None, _) -> 
	simple_rewrite_rule scp (rule_false_thm()) thm
    | (Some(true), _) -> 
	simple_rewrite_rule scp (cond_rule_false_thm()) thm
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
  Lib.apply_first 
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
  let fs x= Lib.apply_first [do_conj_rule scp] x
  in 
  List.rev(apply_merge_list fs [thm])

let thm_to_rules scp thm = 
  let thmlst=multi_thm_to_rules scp thm
  in 
  List.map (single_thm_to_rules scp) thmlst

(*** Converting assumptions to rewrite rules **)


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
  else asm_rewrite_tac (rule_true_thm()) tg g

and rr_equality_asm (tg, (qs, c, a)) g =
  if(is_rr_equality (qs, c, a))
  then skip g
  else failwith "rr_equality_asm: not a rewrite rule"

and fact_rule_asm (tg, (qs, c, a)) g= 
  if(not (Logicterm.is_equality a))
  then 
    match is_rr_rule (qs, c, a, None) with
      (None, _) -> 
	if(is_constant (qs, c, a))
	then skip g
	else asm_rewrite_tac (rule_true_thm()) tg g
    | (Some(true), _) -> 
	if(is_constant (qs, c, a))
	then skip g
	else asm_rewrite_tac (cond_rule_true_thm()) tg g
    | _ -> failwith "do_fact_asm"
  else failwith "do_fact_asm"

and neg_rule_asm (tg, (qs, c, a)) g = 
  if(not (Logicterm.is_equality a))
  then 
    match is_rr_rule (qs, c, a, None) with
      (None, _) -> 
	asm_rewrite_tac (rule_false_thm()) tg g
    | (Some(true), _) -> 
	asm_rewrite_tac (cond_rule_false_thm()) tg g
    | _ -> failwith "neg_rule_asm"
  else failwith "neg_rule_asm"

and neg_all_rule_asm (tg, (qs, c, a)) g= 
  let scp = scope_of g
  in 
  match c with 
    None -> 
      if (is_neg_all (qs, c, a))
      then 
	(asm_rewrite_tac (neg_all_conv scp a) tg 
	   ++ single_asm_to_rule tg) g
      else failwith "neg_all_rule_asm: Not a negated universal quantifier"
  | Some _ ->  
      failwith 
	"neg_all_rule_asm: Not an unconditional negated universal quantifier"

and neg_exists_rule_asm (tg, (qs, c, a)) g= 
  let scp = scope_of g
  in 
  match c with 
    None -> 
      if (is_neg_exists (qs, c, a))
      then 
	(asm_rewrite_tac (neg_exists_conv scp a) tg 
	   ++ single_asm_to_rule tg) g
      else failwith "neg_exists_rule_asm: Not a negated existential quantifier"
  | Some _ ->  
      failwith 
	"neg_exists_rule_asm: Not an unconditional negated existential quantifier"

and single_asm_to_rule tg goal = 
  let tform = 
    Logic.Sequent.get_tagged_asm tg (sequent goal)
  in 
  let trm = Formula.term_of (drop_tag tform)
  in 
  let (qs, c, a) = strip_qnt_cond trm
  in 
  Lib.apply_first 
    [
     neg_all_rule_asm (tg, (qs, c, a));
     neg_exists_rule_asm (tg, (qs, c, a));
     rr_equality_asm (tg, (qs, c, a));
     neg_rule_asm (tg, (qs, c, a));
     fact_rule_asm (tg, (qs, c, a));
     accept_asm (tg, (qs, c, a))
   ]  goal

let asm_to_rules tg ret goal = 
    seq
      [
	single_asm_to_rule tg;
	(fun g ->
	   let rule = get_tagged_asm (ftag tg) g
	   in 
	     data_tac (fun _ -> ret:=[rule]) () g)
      ] goal

(***
* Rules from assumptions and conclusions.
***)


(** Information about rules created from sequent formulas. **)
type rule_data = 
    { 
      src: Logic.tagged_form; (** The source of the rules. **)
      new_asm: Formula.form;
      (**
	  The new assumption (e.g. when a conclusion is lifted into
	  the assumptions) to add to the simp set.
      **)
      new_rules: (Logic.tagged_form) list 
	(** The rules formed fro the source. *)
    }

let mk_rule_data s a rs = 
  { src = s; new_asm = a; new_rules = rs }

(**
    [unpack_rule_data rd]: Unpack a list of rule data in a list of
    sources, a list of new assumptions and a list of new rules.
*)
let unpack_rule_data rds = 
  let srcs, asms = 
    Lib.apply_split (fun rd -> (rd.src, rd.new_asm)) rds
  in 
  let rules = 
    Lib.apply_flatten (fun rd -> rd.new_rules) rds
  in 
    (srcs, asms, rules)

(**
   [prepare_asm data except a goal]: Prepare assumption labelled [a]
   for use as a simp rule. 

   Does nothing if [except a] is true. Solves the goal if [a] is
   [false]. Otherwise, returns the list of new assumptions formed from
   [a] which are to be used as simp rules.

   {ul
   {- Copy the assumption to get new assumption [a1].}
   {- Call [asm_to_rules] on [a1] to get [rules].}
   {- For each new assumption [r], store the result as the pair
   [{ src = a; new_asm = a1; new_rules = rules }].
   {- Return the result in [data]. }
   }
*)
let prepare_asm data except a goal =
  if (except a)
  then skip goal
  else 
    let info=mk_info()
    and new_asm_tags = ref []
    and asm_form = get_tagged_asm (ftag a) goal
    in 
    let data_fn (new_asm, rules) = 
      data := 
	(mk_rule_data asm_form new_asm rules)::(!data)
    in 
    let false_test g = 
      let (_, asm) = Logic.Sequent.get_tagged_asm a (sequent g)
      in 
      Logicterm.is_false (Formula.term_of asm)
    in 
    seq 
      [
       (false_test --> Boollib.falseA ~a:(ftag a));
       copyA ~info:info (ftag a);
       (fun g ->
	 let a1=
	   get_one ~msg:"Simplib.prepare_asm" (aformulas info)
	 in 
	 let a1form = drop_tag (get_tagged_asm (ftag a1) g)
	 in 
	 seq 
	   [
	    asm_to_rules a1 new_asm_tags;
	     (fun g1 -> 
		let rules = (!new_asm_tags)
		in 
		  data_tac data_fn (a1form, rules) g1)
	   ] g)
       ] goal

(**
   [prepare_asms data asm except g]: Apply [prepare_asm] to each
   assumption in the list [asms]. Return the cumulative results.
 *)
let prepare_asms data ams except goal=
  let d=ref []
  in 
  seq
    [
     map_every (prepare_asm d except) ams;
     data_tac (fun () -> data:=!d) ()
   ] goal

(**
   [prepare_concl data except c goal]: Prepare conclusion labelled [a]
   for use as a simp rule. 

   Does nothing if [except c] is true. Solves the goal if [c] is
   [true]. Otherwise, returns the list of new assumptions formed from
   [c] which are to be used as simp rules.

   {ul 

   {- Copy the conclusion and lift it into the assumptions (by
   negation) to get new assumption [a].}

   {- Call [asm_to_rules] on [a] to get [rules].}

   {- For each new assumption [r], store the result as the pair
   [{ src = c; new_asm = a; new_rules = rules }].
   {- Return the result in [data]. }
   }
*)
let prepare_concl data except c goal =
  if(except c)
  then skip goal
  else 
    let info=mk_info()
    and new_asm_tags = ref []
    and concl_form = get_tagged_concl (ftag c) goal
    in 
    let data_fn (new_asm, rules) = 
      data:= (mk_rule_data concl_form new_asm rules)::(!data)
    in 
    let true_test g = 
      let (_, concl) = Logic.Sequent.get_tagged_cncl c (sequent g)
      in 
      Logicterm.is_true (Formula.term_of concl)
    in 
    seq 
      [
       (true_test --> Logic.Tactics.trueC None (ftag c));
       copyC ~info:info (ftag c);
       (fun g -> 
	 let c1 = 
	   get_one ~msg:"Simplib.prepare_concl" (cformulas info) 
	 in 
	 empty_info info;
	 negate_concl_tac ~info:info (ftag c1) g); 
       (fun g ->
	 let a=
	   get_one ~msg:"Simplib.prepare_concl" (aformulas info) 
	 in 
	 let aform = drop_tag (get_tagged_asm (ftag a) g)
	 in 
	 seq 
	   [
	    asm_to_rules a new_asm_tags;
	     (fun g1 ->
		let rules = (!new_asm_tags)
		in 
		  data_tac data_fn (aform, rules) g1)
	  ] g)
     ] goal

(**
   [prepare_concls data concls except g]: Apply [prepare_concl] to each
   assumption in the list [concls]. Return the cumulative results.
 *)
let prepare_concls data cs except goal=
  let d=ref []
  in 
  seq
    [map_every (prepare_concl d except) cs;
     data_tac (fun () -> data:=(!d)) ()] goal

