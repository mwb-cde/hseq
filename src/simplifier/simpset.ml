module Simpset =
  struct

    open Basic
    open Term
    open Logicterm
    open Simpconvs
    open Simputils


(* simpsets *)

(** [rule] :
   variables, optional condition, lhs , rhs , source of rule
 *)
    type rule = 
	(binders list 
	   * term option * term * term 
	   * Logic.rr_type)

    let dest_rule (vs, cnd, lhs, rhs, rr)=(vs, cnd, lhs, rhs, rr)


(** 
   [simp_set]:
   set of simplifier rules 
 *)
    type simp_set = 
	{ 
	  basic: rule Termnet.net;          (* global rules *)
	  next: simp_set option             (* next simp_set *)
	} 

    let empty_set() = 
      { basic=Termnet.empty(); 
	next=None
      }

(** [is_variable qnts x]:
   test for variables (universal quantifiers) in an entry 
 *)
    let is_variable qnts x= Rewrite.is_free_binder qnts x

(** [lt x y]:
   less than ordering of terms
 *)
    let lt (_, _, _, x, _) (_, _, _, y, _)=term_gt y x

(** [add_rule rl s]:

   add rule [rl] to set [s]
 *)
    let add_rule rl s=
      let (vs, _, l, _, _)=rl
      in 
      { 
	basic=Termnet.insert lt (is_variable vs) (s.basic) l rl;
	next=s.next
      }

(** [lookup trm s]:

   find list of possible matches for term [trm] in set [s].
 *)
    let rec lookup set trm =
      try
	Termnet.lookup set.basic trm 
      with Not_found ->
	(match set.next with 
	  None -> raise Not_found
	| Some(s) -> lookup s trm)


(** [join s t]
   Join sets s and t together.
   In the set [join s t], the set [s] will be searched before set [t]
 *)
    let rec join s1 s2 = 
      match s1.next with 
	None ->
	  { basic=s1.basic;
	    next=Some(s2)}
      | Some x -> 
	  {basic=s1.basic;
	   next=Some(join x s2)}

(** [split s]
   Split simp_set [s] into two parts.
   fails if [s] is not joined to another set.
 *)
    let split s1=
      match s1.next with 
	None ->
	  raise (Failure "Simpset.split")
      | Some x -> 
	  ({basic=s1.basic;
	    next=None}, x)
	    
(* needed properties *)

(*
   let iff_equals_ax = saxiom "! a b: (a iff b)=(a=b)"
   let rule_false_ax = saxiom "! a : (not a)=(a=false)"
   let rule_true_ax = saxiom "! a : a=(a=true)"
 *)

(* prepare simplifier rules *)

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


(* utility functions *)


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
	  None -> rret:=None | 
	  Some (rhs) -> check_variables is_var vars rhs;
      with Not_found -> rret:=(Some false));
      (* check cond (if any) *)
      (try
	(match c with 
	  None -> rret:=None 
	| Some(cnd) -> check_variables is_var vars cnd)
      with Not_found -> cret:=(Some false));
      (!cret, !rret)

(* [dest_rr_rule trm]: 
   Split rule [trm] into binders, condition, lhs, rhs 
   rules are of the form:
   [c=>(l=r)] or [l=r]
 *)
    let dest_rr_rule trm =
      (* Get leading quantifiers *)
      let (qs, t1)=strip_qnt (Basic.All) trm 
      in 
      (* test for conditional equalities *)
      let (cnd, rl)=
	if (is_implies t1)         
	then (* is conditional *)
	  (let (asm, cncl)=dest_implies t1
	  in 
	  (Some(asm), cncl))
	else  (* is not conditional *)
	  (None, t1)
      in 
      (* break the equality *)
      if (is_equal rl)
      then 
	let (lhs, rhs)=dest_equal rl
	in (qs, cnd, lhs, rhs)
      else 
	  raise (Failure 
		   ("Not an equality or a conditional equality\n"))

(** [strip_qnt_cond trm]
   split rule [trm] into variable binders, condition, equality
   rules are of the form:
   a=>c
   c
 *)
    let strip_qnt_cond t =
      let (qs, t1)=strip_qnt (Basic.All) t  (* get leading quantifiers *)
      in 
      if (is_implies t1)         (* deal with conditional equalities *)
      then 
	(let (_, args)=dest_fun t1
	in 
	match args with
	  [a; c] -> (qs, Some a, c)
	| _  -> 
	    raise 
	      (termError "strip_qnt_cond: not a valid implication" [t]))
      else
	(qs, None, t1)


(* conversions for manipulating rules *)


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
      rebuild_qnt Basic.All qs 
	(Logicterm.mkimplies (dest_option cnd)
	   (Rewrite.rewrite_univs scp ~dir:true ~simple:true 
	      [rrtrm] qb))
	
(** [form_cond_rewrite scp rl fm]:
   for rewrite rule [rl]=|-l=r,
   rewrite conditional formula [fm]= c=>a=l to c=>a=r
   in scope [scp]
 *)
    let form_cond_rewrite scp rl fm =
      Formula.mk_form scp 
	(term_cond_rewrite scp rl (Formula.dest_form fm))

(* functions to make simp rules from assumptions *)


    let monitor = ref None

(* make_entry t:
   y -> y=true
   x=y -> x=y        (if a rr rule)
   -> (x=y)=true (otherwise)
   c=>x=y -> c=>x=y        (if a rr rule)
   -> (c=>x=y)=true (otherwise)
 *)   


(* addition of rules *)

(** [is_valid_rule rl]:
   true if [th] is a valid rule
*)

(** [make_rule src]:
   make rule from theorem or assumption [src] in scope [scp]
*)
    let make_rule rl=
      match rl with
	Logic.RRThm(th) -> 
	  let qs, c, l, r=
	    dest_rr_rule (Formula.dest_form (Logic.dest_thm th))
	  in 
	  (qs, c, l, r, rl)
      | _ -> failwith "make_rule: can only handle theorems"


(** [add_simp_rule scp set rls]
   add (properly formed rules) [rls] to set [set]
   in scope [scp]
*)
    let add_simp_rule scp sset src =
      let nset=ref sset
      in 
      let rec add_aux thms =
	match thms with
	  [] -> !nset
	| rl::ts -> 
	    nset:=add_rule (make_rule rl) (!nset);
	    add_aux ts
      in 
      add_aux src

(*
    let add_sqnt_asms sset asms sqnt =
      let nset=ref sset
      in 
      let rec get_asms nasms bl= 
	match nasms with 
	  [] -> List.rev bl
	| ((_, (_, _, _, _, Logic.Tagged(t))) ::na) 
	  -> get_asms nasms (t::bl)
	| _ -> failwith "add_sqnt_asms.get_asms"
      in 
      let rec add_aux asml =
	match asml with
	  [] -> ()
	| rl::ts -> 
	    nset:=add_rule rl (!nset);
	    add_aux ts
      in 
      let (ents, ng)=make_asm_entry asms [] sqnt
      in 
      add_aux ents;
      (!nset, get_asms ents, ng)
*)
  end

   open Simpset;;


   let scope () = Tpenv.scope();;

   let saxiom str = 
   Logic.mk_axiom 
   (Formula.form_of_term (scope()) (Tpenv.read str));;

   let get_tags asms g=
   let sqnt=Logic.get_sqnt g
   in 
   List.map (fun i -> Logic.index_to_tag i sqnt) asms;;

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


