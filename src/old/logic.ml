  open Basic
  open Formula

module Tags=
struct
  type tag=string ref

  let named x = ref x
  let name x = !x

  let null=ref ""
  let new_tag() = ref ""

  let equal x y = x==y
end

  type thm = 
      Axiom of form
    | Theorem of form

  type saved_thm = 
      Saxiom of saved_form
    | Stheorem of saved_form

  type tagged_form = (Tags.tag* form)

  type skolem_cnst = (Basic.fnident * (int * Gtypes.gtype))
  type skolem_type = skolem_cnst list

  type sqnt_env = {sklms: skolem_type; sqenv : Gtypes.scope}
  type sqnt = (Tags.tag* sqnt_env * form list * form list)

  type goal =  Goal of (sqnt list * form)
  type rule = goal -> goal
  type conv = thm list -> thm list 

(* cdefn:
   Checked Definitions: 
   checking of type and term definitions and declarations
*)

type cdefn =
    TypeDef of Basic.fnident * string list * Gtypes.gtype option
  | TermDef of 
      Basic.fnident * Gtypes.gtype
	* (string*Gtypes.gtype) list * thm option

  let mk_axiom t = Axiom t
  let mk_theorem t = Theorem t


(*
   rr_type: where to get rewrite rule from
   Asm : numbered assumption
   Tagged: tagged assumption
   RRThm: given theorem
*)

type rr_type = 
   Asm of int 
   | Tagged of Tags.tag
   | RRThm of thm


let is_axiom t = match t with (Axiom _) -> true | _ -> false
let is_thm t = match t with (Theorem _) -> true | _ -> false

  let dest_thm x = 
    match x with 
      Axiom f -> f
    | Theorem f -> f

  let to_save t = 
    match t with 
      Axiom f -> Saxiom (Formula.to_save f)
    | Theorem f -> Stheorem (Formula.to_save f)

  let from_save t = 
    match t with 
      Saxiom f -> Axiom (Formula.from_save f)
    | Stheorem f -> Theorem (Formula.from_save f)

  let string_thm x = string_form (dest_thm x)

(* Error handling *)

  let mklogicError s t = 
      ((new Term.termError s 
          (List.map Formula.term_of_form t)):>Result.error)
  let logicError s t = 
      Result.mkError((new Term.termError s 
           (List.map Formula.term_of_form t)):>Result.error)
  let addlogicError s t es = 
          raise (Result.addError ((mklogicError s t):>Result.error) es)


(* Sequent manipulation *)

  let get_sklm_name (x, (_, _)) = x
  let get_sklm_indx (_, (i, _)) = i
  let get_sklm_type (_, (_, t)) = t

  let get_old_sklm n sklms = 
    (n, List.assoc  n sklms)

  let is_sklm n sklms = 
    try ignore(get_old_sklm n sklms); true
    with Not_found -> false

  let get_new_sklm n t sklms = 
    try 
    (let oldsk = get_old_sklm n sklms
    in let nindx = ((get_sklm_indx oldsk)+1)
    in let nnam = 
      mklong (thy_of_id n) ((name n)^"_"^(string_of_int nindx))
    in ((Term.mk_typed_var nnam t),
	((nnam,(nindx, t))::sklms)))
    with Not_found -> 
      let nn =  (mklong (thy_of_id n) ((name n)^"_"^(string_of_int 1)))
      in 
      ((Term.mk_typed_var nn t), (nn, (1, t))::sklms)


  let asms (_, _, asl, _) = asl
  let concls (_, _, _, cnl) = cnl
  let env (_, e, _, _) = e
  let sklm_cnsts (_, e, _, _) = e.sklms
  let scope_of (_, e, _, _) = e.sqenv
  let sqnt_tag(t, _, _, _) = t

(* mk_sqnt x: |- x  (with x to be proved)*) 

  let mk_sqnt tenv x = (Tags.new_tag(), {sklms=[]; sqenv = tenv}, [], [x])
  let sqnt_scope sq = scope_of sq

  let get_asm i sq = rename (List.nth (asms sq) ((-i)-1))
  let delete_asm i asms = (Lib.delete_nth (-i) asms)
  let replace_asm i asms a = (Lib.replace_nth (-i) asms a)

  let get_cncl i sq = rename (List.nth (concls sq) (i-1))
  let delete_cncl  = Lib.delete_nth 
  let replace_cncl = Lib.replace_nth

  let sqntError s = 
      Result.mkError(new Result.error s)
  let addsqntError s es = 
          raise (Result.addError (new Result.error s) es)

  let thy_of_sq sq = (scope_of sq).Gtypes.curr_thy

(* Subgoals *)
  exception No_subgoals

let has_subgoals g = 
  match g with
    Goal([], _) -> false
  | _ -> true

(* get_sqnt: take first subgoal of goal *)

let get_sqnt g=
  match g with
  Goal(s::_, f) -> s
  | _ -> Result.raiseError ("get_sqnt: No subgoals")

let combine_subgoals a b = a@b

let get_goal (Goal(_, f)) = f

let get_subgoal_tags (Goal(sqs, _)) = List.map sqnt_tag sqs

let goal_focus t (Goal(sqnts, f)) =
  let rec focus sqs rslt=
    match sqs with
      [] -> raise Not_found
    | (x::xs) -> 
	if Tags.equal t (sqnt_tag x) 
	then (x::((List.rev rslt)@xs))
	else focus xs (x::rslt)
  in Goal(focus sqnts [], f)
      

  let apply_nth r i g = 
    match g with 
      Goal(sgs, f) ->
 	let nsgs = r (List.nth sgs i)
      	in Goal(Lib.splice_nth i sgs [nsgs], f)


let num_of_subgoals x = 
  match x with
    (Goal(sqs, _)) -> List.length sqs

let get_subgoals (Goal(sq, _)) = sq

let get_nth_subgoal_sqnt i (Goal(sq, _)) =
  try (List.nth sq i)
  with _ -> raise (sqntError "get_nth_subgoal: invalid argument")

let goal_has_subgoals g =
  match g with 
    (Goal([], _)) -> false
  | (Goal(_, _)) -> true

  let mk_goal tenv f = 
    let nf= Formula.typecheck tenv f (Gtypes.mk_bool)
    in Goal([mk_sqnt tenv nf], nf)

  let mk_thm g = 
    match g with 
      Goal([], f) -> Theorem f
    | _ -> raise (logicError "Not a theorem" [])


module Rules=
struct

let sqnt_apply r g =
match g with 
  Goal([], f) -> raise No_subgoals
| Goal(x::xs, f) -> 
    try 
      Goal((r x)@xs, f)
    with No_subgoals -> Goal(xs, f)

let mk_subgoal sq = [sq]

  let goal_apply r g = 
    match g with 
      Goal([], _) -> raise (sqntError "No subgoals")
    |(Goal(sqs, f)) -> r g	

  let postpone g = 
    match g with
      Goal (sq::[], _) -> raise (sqntError "postpone: No other subgoals")
    | Goal (sg::sgs, f) -> Goal (List.concat [sgs;[sg]], f)
    | _ -> raise (sqntError "postpone: No subgoals")

  let goal_postpone g = 
    match g with
      (Goal(sqs, f)) -> postpone g

(* composition of rules *)

(* foreach r sqs:
   applies r to each subgoal in g
*)

  let foreach r g =
    let rec each_aux f gs rslts =
      match gs with 
	[] -> rslts
      |	(x::xs) ->
	  each_aux f xs (f x)
    in 
    match g with 
      Goal([], f) -> raise No_subgoals
    | Goal(sqs, f) -> 
	Goal(List.flatten(List.rev(each_aux r sqs [])), f)

(* fail sq:
   do nothing and fail
*)
  let fail sq = raise (logicError "failed" [])

(* thenl rules sq:
   applies rules, in order, to each subgoal resulting from
   application of first rule to sq 
   fails if any rule fails ( sequential composition )
*)

  let thenl rls sq =
    let rec thenl_aux fs sqs =
      match fs with 
	[] -> sqs 
      |	r::rs ->
          let nsq=(r sqs)
	  in 
	  (thenl_aux rs (r sqs))
    in 
    thenl_aux rls sq 

(* apply_list rules sq:
   applies rules, in order, to each subgoal resulting from
   application of first rule to sq 
   fails if no rule succeeds 
*)

  let apply_list fs sq =
    let chng = ref false
    in 
    let rec appl_aux fs sqs =
      match fs with 
	[] -> sqs 
      |	r::rs-> 
        (try 
        (let nsq=(r sqs) 
        in chng:=true; appl_aux rs nsq)
        with No_subgoals -> raise No_subgoals
        | _ -> appl_aux rs sqs)
    in let nsqs = appl_aux fs sq
    in if !chng then nsqs else (fail sq)


let rec repeat r sqnt =
  let nsqnt = r sqnt 
  in 
  try 
    repeat r nsqnt
  with No_subgoals -> raise No_subgoals
  | _ -> nsqnt

(* skip sq:
   do nothing and return [sq]
*)

  let skip sq = sq

(* orl fs sq:
   apply first of fs to to sq, 
   if unsucesseful try next f
*)

  let rec orl fs sq =
    match fs with
      [] -> fail sq
    | r::rs -> 
	(try (r sq)
	with No_subgoals -> raise No_subgoals
              | _ -> orl rs sq)

  let copy_asm0 i sq = 
    let na=Formula.rename (get_asm i sq) in
     mk_subgoal (sqnt_tag sq, env sq,
     Lib.splice_nth (-i) (asms sq) [na; na],
     concls sq)

  let copy_asm i g = sqnt_apply (copy_asm0 i) g

  let rotate_asms0 sq = 
    let hs = asms sq in
    match hs with 
      [] -> mk_subgoal(sqnt_tag sq, env sq, hs, concls sq)
    | h::hys -> mk_subgoal(sqnt_tag sq, env sq, hys@[h], concls sq)

  let rotate_asms sqnt = sqnt_apply rotate_asms0 sqnt

  let copy_cncl0 i sq=
    let nc=Formula.rename (get_cncl i sq)
    in 
    mk_subgoal(sqnt_tag sq, env sq,
     asms sq,
     Lib.splice_nth (i) (concls sq) [nc; nc])

let copy_cncl i sqnt = sqnt_apply (copy_cncl0 i) sqnt

  let rotate_cncls0 sq =
    let cs = concls sq in
    match cs with 
      [] -> mk_subgoal(sqnt_tag sq, env sq, asms sq, cs)
    | c::cns -> mk_subgoal(sqnt_tag sq, env sq, asms sq, cns@[c])

let rotate_cncls sqnt = sqnt_apply rotate_cncls0 sqnt

(* get_pair: auxiliary function *)

let get_pair x = 
  match x with
    [t1; t2] -> (t1, t2)
  | _ -> raise (logicError "get_pair" x)

let get_one x = 
  match x with
    [t1] -> t1
  | _ -> raise (logicError "get_one" x)

(* cut x sq: adds theorem x to assumptions of sq 

   asm |- cncl      --> x, asm |- cncl
*)

  let cut0 x sq=
    let nt = Formula.rename (dest_thm x)
    and scp = scope_of sq
    in 
    try 
      ignore(Formula.in_thy_scope scp (scp.Gtypes.curr_thy) nt);
      mk_subgoal(sqnt_tag sq, env sq, nt::(asms sq), concls sq)
    with 
      x -> (addlogicError "Not in scope of sequent" [nt] x)

let cut x sqnt = sqnt_apply (cut0 x) sqnt

(* delete x sq: delete assumption (x<0) or conclusion (x>0) from sq*)

  let delete0 x sq=
    if x>0 then 
     mk_subgoal (sqnt_tag sq, env sq, asms sq, delete_cncl x (concls sq))
    else 
     mk_subgoal (sqnt_tag sq, env sq, delete_asm x (asms sq), concls sq)

let delete x sqnt = sqnt_apply (delete0 x) sqnt

(* basic i j sq: compares asm i with cncl j of sq, 
   if equal then result is the theorem concl j 

   asm, a_{i}, asm' |- concl, c_{j}, concl' 
   -->
   true if a_{i}=c_{j}

  let basic i j sq = 
    if Logicterm.alpha_convp (scope_of sq) (get_asm i sq) (get_cncl j sq) 
    then []
    else raise (logicError "Not basic" [get_asm i sq; get_asm j sq])
*)

(* conjI i sq: 
   asm |- a /\ b, concl   
   -->
   asm |- a  and asm |- b *)

  let conjI0 i sq=
    let t=(get_cncl i sq) 
    in 
    if (Formula.is_conj t) 
    then 
      (let (t1, t2) = get_pair (Formula.dest_conj t)
      in 
      let cnl1=(replace_cncl i (concls sq) t1)
      and cnl2=(replace_cncl i (concls sq) t2)
      and tagl=Tags.new_tag()
      and tagr=Tags.new_tag()
      in 
      [(tagl, env sq, asms sq, cnl1); 
       (tagr, env sq, asms sq, cnl2)])
    else raise (logicError "Not a conjunct" [t])

let conjI i sqnt = sqnt_apply (conjI0 i) sqnt

(* conjE i sq: 
   a/\ b, asm |- concl   
   -->
   a, b, asm |- concl *)

  let conjE0 i sq=
    let t=(get_asm i sq) 
    in 
    if (Formula.is_conj t) 
    then 
      (let (t1,  t2) = get_pair(Formula.dest_conj t)
      in 
      mk_subgoal(
      sqnt_tag sq, env sq, t1::t2::(delete_asm i (asms sq)), concls sq))
    else raise (logicError "Not a conjunction" [t])

let conjE i sqnt = sqnt_apply (conjE0 i) sqnt

(* disjI i sq: 
   a\/b, asm |-  concl   
   -->
   a, asm |- concl  and b, asm |- concl *)

  let disjI0 i sq=
    let t=(get_asm i sq) 
    in 
    if (Formula.is_disj t) 
    then 
      (let (t1, t2) = get_pair (Formula.dest_disj t)
      in 
      let asm1=replace_asm i (asms sq) t1
      and asm2=replace_asm i (asms sq) t2
      and tagl=Tags.new_tag()
      and tagr=Tags.new_tag()
      in [(tagl, env sq, asm1, concls sq); 
	  (tagr, env sq, asm2, concls sq)])
    else raise (logicError "Not a disjunction" [t])

let disjI i sqnt = sqnt_apply (disjI0 i) sqnt

(* disjE i sq: 
   asm |- a\/b, concl   
   -->
   asm |- a, b, concl *)

  let disjE0 i sq =
    let t=(get_cncl i sq) 
    in 
    if (Formula.is_disj t) 
    then 
      (let (t1, t2) = get_pair (Formula.dest_disj t)
      in 
      mk_subgoal 
	(sqnt_tag sq, env sq, asms sq, t1::t2::(delete_cncl i (concls sq))))
    else raise (logicError "Not a disjunction" [t])

let disjE i sqnt = sqnt_apply (disjE0 i) sqnt

(* negA i sq:
   ~a, asms |- concl
   -->
   asms |- a, concl
*)

  let negA0 i sq =
    let t=(get_asm i sq) 
    in 
    if (Formula.is_neg t) 
    then 
      (let t1 = get_one(Formula.dest_neg t)
      in 
      mk_subgoal
	(sqnt_tag sq, env sq, delete_asm i (asms sq), t1::(concls sq)))
    else raise (logicError "Not a negation"[t])

let negA i sqnt = sqnt_apply (negA0 i) sqnt

(* negC i sq:
   asms |- ~c, concl
   -->
   c, asms |- concl
*)

  let negC0 i sq =
    let t=(get_cncl i sq) 
    in 
    if (Formula.is_neg t) 
    then 
      (let t1 = get_one (Formula.dest_neg t)
      in 
      mk_subgoal
	(sqnt_tag sq, env sq, t1::(asms sq), delete_cncl i (concls sq)))
    else raise (logicError "Not a negation"[t])

let negC i sqnt = sqnt_apply (negC0 i) sqnt

(* implI i sq
   asms |- a-> b, cncl 
   -->
   a, asms |- b, cncl
*)

  let implI0 i sq=
    let t=(get_cncl i sq) 
    in 
    if (Formula.is_implies t) 
    then 
      (let  (t1, t2) = get_pair (Formula.dest_implies t)
      in 
      mk_subgoal
	(sqnt_tag sq, env sq, t1::(asms sq), 
	(replace_cncl i (concls sq) t2)))
    else raise (logicError "Not an implication" [t])

let implI i sqnt = sqnt_apply (implI0 i) sqnt

(* implE i sq
   a-> b,asms |-cncl 
   -->
   asms |- a, cncl  and    b, asms |- cncl
*)

  let implE0 i sq =
    let t=(get_asm i sq) 
    in 
    if (Formula.is_implies t) 
    then 
      (let (t1, t2) = get_pair (Formula.dest_implies t)
      in 
      let asm2=replace_asm i (asms sq) t2
      and cncl1=t1::(concls sq)
      and asm1 = delete_asm i (asms sq)
      and tagl=Tags.new_tag()
      and tagr=Tags.new_tag()
      in [(tagl, env sq, asm1, cncl1); 
	  (tagr, env sq, asm2, concls sq)])
    else raise (logicError "Not an implication" [t])

let implE i sqnt = sqnt_apply (implE0 i) sqnt

(* allI i sq
   asm |- !x. P(c), concl
   -->
   asm |- P(c'), concl   where c' is a new identifier
*)

  let add_sklms_to_scope sklms scp =
    Gtypes.extend_scope scp 
      (fun x -> 
	let y =
	  (if (thy_of_id x)=null_thy
	  then mklong scp.Gtypes.curr_thy (name x)
	  else x)
	in get_sklm_type (get_old_sklm y sklms))

  let allI0 i sq =
    let t=(get_cncl i sq)
    in 
    if (Formula.is_all t)
    then 
      (let (nv, nty)=(Formula.get_binder_name t, Formula.get_binder_type t)
      in 
      let (sv, nsklms) = 
	get_new_sklm (mklong (thy_of_sq sq) nv) nty (sklm_cnsts sq)
      in 
      let nscp = add_sklms_to_scope nsklms (scope_of sq)
      in 
      mk_subgoal(sqnt_tag sq, {sklms=nsklms; sqenv=nscp},
		 asms sq, 
		 (replace_cncl i (concls sq) 
		    (Formula.inst nscp t sv))))
    else raise (logicError "Not a universal quantifier" [t])

let allI i sqnt = sqnt_apply (allI0 i) sqnt

(* existI i sq
   ?x. P(c), asm |- concl
   -->
   P(c'), asm |- concl   where c' is a new identifier
*)

  let existI0 i sq =
    let t=(get_asm i sq)
    in 
    if (Formula.is_exists t)
    then 
      (let (nv, nty) = (Formula.get_binder_name t, Formula.get_binder_type t)
      in 
      let (sv, nsklms) = 
	get_new_sklm (mklong (thy_of_sq sq) nv) nty (sklm_cnsts sq)
      in let nscp = add_sklms_to_scope nsklms (scope_of sq)
      in mk_subgoal(sqnt_tag sq, {sklms=nsklms; sqenv = nscp},
           (replace_asm i (asms sq) 
	      (Formula.inst nscp t sv)), 
	   concls sq))
    else raise (logicError "Not an existential quantifier" [t])

let existI i sqnt = sqnt_apply (existI0 i) sqnt

(* trueR i sq
   asm |- true, concl
   --> true
*)

  let trueR0 i sq=
    let t=(get_cncl i sq)
    in 
    ((if (Formula.is_true t) 
    then raise No_subgoals (* mk_branch[] *) 
    else raise (logicError "Not trivial" [t])))

let trueR i sqnt = sqnt_apply (trueR0 i) sqnt

(* beta i sq:  (beta reduction of asm (i<0) or concl (i>0) in sq) *)

  let beta0 i sq = 
    let t = if i>0 then get_cncl i sq else get_asm i sq
    in let nt = 
      (try Formula.beta_reduce (scope_of sq) t
      with x -> raise 
	  (Result.catchError(mklogicError "Beta reduction" [t]) x))
    in 
    if i> 0 
    then mk_subgoal
	(sqnt_tag sq, env sq, (asms sq), replace_cncl i (concls sq) nt)
    else mk_subgoal
	(sqnt_tag sq, env sq, replace_asm i (asms sq) nt, concls sq)

  let beta i sqnt = sqnt_apply (beta0 i) sqnt

(* name_rule: introduce a new name in the sqnt as a synonym for a term  *)
(* name id trm:
  Asm|-Cncl -> id=trm, Asm|-Cncl

the long name thy.id must be unique (where thy is the current theory name)

*)

let name_rule0 id trm sq =
  let scp = scope_of sq
  in 
  let long_id = Basic.mklong scp.Gtypes.curr_thy id
  in 
  try 
    ignore(scp.Gtypes.typeof_fn long_id);
    raise (logicError "Name already exists in scope" [])
  with 
    Not_found ->
     let nty = Gtypes.mk_var "name_typ"
     and tenv=Gtypes.empty_subst()
     in 
     let rty= ignore(Typing.typecheck_env scp tenv trm nty);
              (Gtypes.mgu nty tenv)
     in 
     let nscp= Gtypes.extend_scope scp 
	 (fun x-> if x=long_id then rty else scp.Gtypes.typeof_fn x)
     in
     let ntrm = Formula.form_of_term nscp
	 (Logicterm.mkequal (Term.mkvar long_id) (trm))
     in 
     mk_subgoal(sqnt_tag sq, {sklms=(sklm_cnsts sq); sqenv=nscp},
       ntrm::(asms sq), concls sq)

let name_rule id trm sqnt = sqnt_apply (name_rule0 id trm) sqnt


(* instantiation terms *)  

  let is_inst_term sq trm expty =
    Typing.typecheck_env (scope_of sq) (Gtypes.empty_subst()) trm expty

  let inst_term sq t trm =
    let scp = scope_of sq
    and sklm_scp = add_sklms_to_scope (sklm_cnsts sq) (Gtypes.empty_scope())
    in 
    let ntrm0 = Term.set_names scp trm
    in 
    let ntrm= Typing.set_exact_types sklm_scp ntrm0
    in 
    (Formula.typecheck scp 
       (Formula.inst scp t ntrm) 
       (Gtypes.mk_var "inst_ty"))

let set_term_sklm_types sq t = 
  Typing.set_exact_types 
    (add_sklms_to_scope (sklm_cnsts sq) (Gtypes.empty_scope())) t

(* unify i j sq: if asm i unifies  with cncl j of sq, 
   then result is the theorem concl j 

   asm, a_{i}, asm' |- concl, c_{j}, concl' 
   -->
   true if a_{i}=c_{j}
*)

  let unify0 i j sq = 
    let tyenv = scope_of sq
    in 
    try
      ((ignore(Formula.unify tyenv (get_asm i sq) (get_cncl j sq)); 
       raise No_subgoals))  (*mk_branch[])*)
    with x -> 
     raise (Result.catchError
	      (mklogicError "Can't unify assumption with conclusion"
		 [get_asm i sq; get_cncl j sq]) x)
			   
  let unify i j sqnt = sqnt_apply (unify0 i j) sqnt

(* existE i sq
   asm |- ?x. P(c), concl
   -->
   asm |- P(c'), concl   where c' is a given term
*)

  let existE0 trm i sq =
    let t=(get_cncl i sq)
    in 
    if (Formula.is_exists t) 
    then 
      try 
      	(let tenv = is_inst_term sq trm (Formula.get_binder_type t)
      	in 
      	let nt = inst_term sq t trm 
      	in 
      	mk_subgoal
	  (sqnt_tag sq, env sq, asms sq, (replace_cncl i (concls sq) nt)))
      with x -> raise (Result.catchError
			 (mklogicError "existE:" [t]) x)
    else 
      raise (logicError "Not an existential quantifier" [t])

let existE trm i sqnt = sqnt_apply (existE0 trm i) sqnt

(* allE i sq
  !x. P(c), asm |-  concl
   -->
   P(c'), asm |- concl   where c' is a given term
*)

  let allE0 trm i sq =
    let t=(get_asm i sq)
    in 
    if (Formula.is_all t) 
    then 
      try 
      (let tenv = is_inst_term sq trm (Formula.get_binder_type t)
      in 
      let nt = inst_term  sq t trm 
      in 
      mk_subgoal
	(sqnt_tag sq, env sq,  (replace_asm i (asms sq) nt), concls sq)) 
     with x -> 
       (raise (Result.catchError
		 (mklogicError "allE: " [t]) x))
    else 
	raise (logicError "Not a universal quantifier" [t])

  let allE trm i sqnt = sqnt_apply (allE0 trm i) sqnt


(* rewrite_any dir thms j sq:
   list of theorems or assumptions containing x=y
   asm |- P(x), concl
   -->
   asm |- P(y), concl
  where dir is =true for right-left and false for left-right
  theorems must be in scope.
  silently discards theorems not in scope and assumptions which don't exist
*)



let filter_rules scp rls j sq= 
  let memo = Lib.empty_env() 
  and thyname= scp.Gtypes.curr_thy
  in 
  let rec ft srcs rslt =
    match srcs with 
      [] -> List.rev rslt
    | ((Asm(x))::xs) -> 
(*print_string "filter_rules: Asm\n";*)
	if x=j then raise (logicError "Rewrite" [get_asm x sq])
	else 
	  ft xs ((get_asm x sq)::rslt)
    | (Tagged(x)::xs) -> print_string "filter_rules: Tagged\n"; ft xs rslt
    | ((RRThm(x))::xs) -> 
(*print_string "filter_rules: RRthm\n";*)
	if (in_thy_scope_memo memo scp thyname (dest_thm x))
	then ft xs ((dest_thm x)::rslt)
	else ft xs rslt
  in ft rls []

let rewrite_any0 dir rls j sq=
    let tyenv = scope_of sq
    in 
    let r=filter_rules tyenv rls j sq
    and t=if j>=0 then (get_cncl j sq)  else (get_asm j sq)
    in 
    try
      (let nt = (Formula.rewrite ~dir:dir tyenv r t)
      in 
      if j>=0 then
	mk_subgoal
	  (sqnt_tag sq, env sq, asms sq, replace_cncl j (concls sq) nt)
      else 
	mk_subgoal
	  (sqnt_tag sq, env sq, replace_asm j (asms sq) nt, concls sq))
    with x -> raise 
	(Result.catchError (mklogicError"rewriting" (t::r)) x)

let rewrite_any ?(dir=true) rls j sqnt
    = sqnt_apply (rewrite_any0 dir rls j) sqnt


(* rewrite dir i j sq
   x=y, asm |- P(x), concl
   -->
   x=y, asm |- P(y), concl
where dir is =true for right-left and false for left-right
*)

let get_asms xs j sq = 
  List.map 
    (fun x -> 
    if x=j then raise (logicError "Rewrite" [get_asm x sq])
    else get_asm x sq) xs

let rewrite0 dir i j sq=
    let r=List.map Formula.rename (get_asms i j sq)
    and t=if j>=0 then (get_cncl j sq)  else (get_asm j sq)
    and tyenv = scope_of sq
    in 
    try
      (let nt = (Formula.rewrite ~dir:dir tyenv r t)
      in 
      if j>=0 then
      mk_subgoal
	  (sqnt_tag sq, env sq, asms sq, replace_cncl j (concls sq) nt)
      else 
      mk_subgoal
	  (sqnt_tag sq, env sq, replace_asm j (asms sq) nt, concls sq))
    with x -> raise 
	(Result.catchError (mklogicError"rewriting" (t::r)) x)


(*
let rewrite ?(dir=true) i j  sqnt= sqnt_apply (rewrite0 dir i j) sqnt
*)

let rewrite ?(dir=true) i j  sqnt
    = rewrite_any ~dir:dir (List.map (fun x-> Asm(x)) i) j sqnt

(* rewrite_thms dir thms j sq:
   list of theorems containing x=y
   asm |- P(x), concl
   -->
   asm |- P(y), concl
  where dir is =true for right-left and false for left-right
  theorems must be in scope.
   discards theorems not in scope
*)

let filter_thms scp thms= 
  let memo = Lib.empty_env() 
  and thyname= scp.Gtypes.curr_thy
  in 
  let rec ft ths rslt =
    match ths with 
      [] -> List.rev rslt
    | (x::xs) -> 
	if (in_thy_scope_memo memo scp thyname (dest_thm x))
	then ft xs ((dest_thm x)::rslt)
	else ft xs rslt
  in ft thms []

let rewrite_thms0 dir thms j sq=
    let tyenv = scope_of sq
    in 
    let r=filter_thms tyenv thms
    and t=if j>=0 then (get_cncl j sq)  else (get_asm j sq)
    in 
    try
      (let nt = (Formula.rewrite ~dir:dir tyenv r t)
      in 
      if j>=0 then
      mk_subgoal
	  (sqnt_tag sq, env sq, asms sq, replace_cncl j (concls sq) nt)
      else 
      mk_subgoal
	  (sqnt_tag sq, env sq, replace_asm j (asms sq) nt, concls sq))
    with x -> raise 
	(Result.catchError (mklogicError"rewriting" (t::r)) x)

(*
let rewrite_thms ?(dir=true) thms j sqnt
    = sqnt_apply (rewrite_thms0 dir thms j) sqnt
*)

let rewrite_thms ?(dir=true) thms j sqnt
    = rewrite_any ~dir:dir (List.map (fun x-> RRThm(x)) thms) j sqnt

end

open Rules
module ThmRules=
struct

(* conversions for theorems *)

(* conjE_conv "|- A and B"  -> ["|- A"; "|- B"] *)

let mk_same_thm t f = 
  if (is_axiom t) then mk_axiom f else mk_theorem f

let get_one_thm t = 
  match t with
    [f] -> f
  | _ -> raise (logicError "Expected one theorem only" (List.map dest_thm t))

let conjE_conv thm=
  let t = get_one_thm thm
  in 
  let f = dest_thm t
  in 
  (try 
    (let fs = Formula.dest_conj f
    in (List.map (mk_same_thm t) fs))
  with x -> raise 
      (Result.catchError 
	 (mklogicError "Not a conjunction" [f]) x))

let allI_conv scp trm ts=
  let t = get_one_thm ts
  in 
  let f = dest_thm t
  in 
  if Formula.is_all f
  then 
    try 
      (let tenv = Typing.typecheck_env scp 
	  (Gtypes.empty_subst()) trm (Gtypes.mk_null())
      in 
      let nf = Formula.inst scp f trm
      in 
      [mk_same_thm t nf])
    with x -> 
      raise (Result.catchError
	       (mklogicError "allE_conv:" [f]) x)
  else raise (logicError "allE_conv:" [f])

let beta_conv scp ts =
  let t = get_one_thm ts
  in 
  try 
  let f = dest_thm t
  in let nt = (Formula.beta_conv scp f)
  in [mk_same_thm t nt]
  with x -> raise 
      (Result.catchError (mklogicError "beta_conv" []) x)

let eta_conv scp x ts =
  let t = get_one_thm ts
  in 
  try 
  let f = (dest_thm t)
  in let nt = (Formula.eta_conv scp x 
		 (Typing.typeof scp (Formula.term_of_form x)) f)
  in [mk_same_thm t nt]
  with e -> 
    raise (Result.catchError (mklogicError "eta_conv" []) e)

let rewrite_conv scp ?(dir=true) rrl thms =
  let conv_aux t = 
    try 
      let f= dest_thm t
      and rs=List.map (fun x -> Formula.rename (dest_thm x)) rrl
      in 
      let nt = (Formula.rewrite ~dir:dir scp rs f)
      in mk_same_thm t nt
    with x -> raise 
	(Result.catchError(mklogicError "rewrite_conv" [dest_thm t]) x)
  in 
  List.map conv_aux thms

      
(* rewriting with nets *)

(*
type rulesDB = (Basic.thy_id * Rewrite.rewrite_rules Net.net)

let empty_db scp = (scp.Gtypes.curr_thy, Net.empty())

let thy_of_db (t, _) = t
let net_of_db (_, n) = n
*)
(* dir = true for right-to-left, false for left-to-right rewriting *)
(*
let dest_rr dir f =
  let (qs, t) = Term.strip_qnt (Basic.All)  f
  in 
  let (a, b) = Logicterm.dest_equal t
  in 
  if dir then (qs, a, b) else (qs, b, a)

let is_free_binder qs t= 
  (match t with
    Term.Bound(q) -> List.exists (fun x ->  Term.binder_equality x q) qs
  |_ -> false)

let add scp dir fs net =
  let nfs = 
    let memo = Lib.empty_env()
    and th = thy_of_db net
    in 
    Lib.filter 
      (fun x -> 
    	not (Formula.in_thy_scope_memo memo scp th (dest_thm x))) fs
  in 
  let rs=List.map 
      (fun x-> (dest_rr dir) (Term.rename (term_of_form (dest_thm x)))) nfs
  in 
  let nnet =   
    (List.fold_left 
       (fun n (qs, a, b) -> 
      	 Net.enter (is_free_binder qs) (a, (qs, a, b)) n) (net_of_db net) rs)
  in (thy_of_db net, nnet)

let rescope_db scp th net = 
  if scp.Gtypes.thy_in_scope (thy_of_db net) th 
  then (th, net_of_db net)
  else raise (logicError "rescope_net: not in scope" [])

let rewrite_net_conv scp rnet t =
  try 
  let f= dest_thm t
  in 
  let nt = (Formula.rewrite_net scp (net_of_db rnet) f)
  in mk_same_thm t nt
  with _ -> raise (logicError "rewrite_conv" [])

let rewrite_net0 rnet j sq=
    let t=if j>=0 then (get_cncl j sq)  else (get_asm j sq)
    and tyenv = scope_of sq
    in 
    try
      (let nt = (Formula.rewrite_net tyenv (net_of_db rnet) t)
      in 
      if j>=0 then
      mk_subgoal
	  (sqnt_tag sq, env sq, asms sq, replace_cncl j (concls sq) nt)
      else 
      mk_subgoal
	  (sqnt_tag sq, env sq, replace_asm j (asms sq) nt, concls sq))
    with x -> raise x

let rewrite_net rnet j sqnt=sqnt_apply (rewrite_net0 rnet j) sqnt
*)
end


(* Defns: functions for checking definitions and declarations *)
module Defns =
struct

let is_typedef x = 
  match x with 
    TypeDef _ -> true
  | _ -> false

let is_termdef x = 
  match x with 
    TermDef _ -> true
  | _ -> false

let dest_typedef x =
  match x with 
    TypeDef (id, args, def) -> (id, args, def)
  | _ -> failwith "Not a term definition"

let dest_termdef x =
  match x with 
    TermDef (id, ty, args, thm) -> (id, ty, args, thm)
  | _ -> failwith "Not a term definition"


(* mk_typedef scp n args d:
   - check n doesn't exist already
   - check all arguments in args are unique
   if defining n as d
     - check d is well defined 
       (all constructors exist and variables are in the list of arguments)
*)

let check_args_unique ags=
  let rec check_aux ys=
  match ys with
    [] -> ()
  | (x::xs) -> 
      if (List.exists (fun a -> a=x) xs) 
      then Result.raiseError 
	  ("Identifier "^x^" appears twice in argument list")
      else check_aux xs 
  in 
  check_aux ags

  let check_type_defn scp arguments ty =
    try 
      Gtypes.well_defined scp ~args:arguments ty
    with
      _ ->
	raise (Gtypes.typeError "Badly formed type" [ty])

let mk_typedef scp n ags d =
  let th = scp.Gtypes.curr_thy
  in 
  let args = check_args_unique ags
  in 
  let dfn = 
    match d with
      None -> None
    | Some(a) -> 
	(try check_type_defn scp ags a; Some(a)
	with 
	  _ -> raise (Gtypes.typeError "Badly formed definition" [a]))
  in 
  (TypeDef((Basic.mklong th n), ags, dfn))


(* mk_termdef scp n ty args d:
   - check n doesn't exist already
   - check all arguments in args are unique
*)

  let mk_termdef scp n ty args d = failwith "mk_termdef is undefined"

end
