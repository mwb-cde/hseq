open Basic
open Formula

module Tag=
  struct
    type t=string ref

    let named x = ref x
    let name x = !x

    let null=ref ""
    let create() = ref ""

    let equal x y = x==y
  end

type thm = 
    Axiom of form
  | Theorem of form

type saved_thm = 
    Saxiom of saved_form
  | Stheorem of saved_form

type tagged_form = (Tag.t* form)

type skolem_cnst = (Basic.fnident * (int * Gtypes.gtype))
type skolem_type = skolem_cnst list

type sqnt_env = {sklms: skolem_type; sqenv : Gtypes.scope}

(*
   type sqnt = (Tag.t* sqnt_env * form list * form list)
 *)

type sqnt = (Tag.t* sqnt_env * tagged_form list * tagged_form list)

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
  | Tagged of Tag.t
  | RRThm of thm

(* fident: sequent formula identifiers *)

type fident = 
    FNum of int
  | FTag of Tag.t

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

let form_tag (t, _) = t
let sqnt_form (_, f) = f

(* mk_sqnt x: |- x  (with x to be proved)*) 

let mk_sqnt_form f = (Tag.create(), f)

let mk_sqnt tenv x = 
  (Tag.create(), {sklms=[]; sqenv = tenv}, [], [mk_sqnt_form x])
let sqnt_scope sq = scope_of sq

let get_asm i sq = 
  let (t, f) = (List.nth (asms sq) ((-i)-1))
  in (t, rename f)
let delete_asm i asms = (Lib.delete_nth (-i) asms)
let replace_asm i asms a = (Lib.replace_nth (-i) asms a)

let get_cncl i sq = 
  let (t, f) = (List.nth (concls sq) (i-1))
  in (t, rename f)
let delete_cncl i cncls = Lib.delete_nth i cncls
let replace_cncl i cncls c= Lib.replace_nth i cncls c

let tag_of_form (t, _) = t
let drop_tag (_, f) = f

let get_tagged_asm t sq = 
  let rec get_aux ams = 
    match ams with
      [] -> raise Not_found
    | x::xs -> 
	if Tag.equal (tag_of_form x) t then x
	else get_aux xs
  in 
  get_aux (asms sq)

let get_tagged_cncl t sq = 
  let rec get_aux ccs = 
    match ccs with
      [] -> raise Not_found
    | x::xs -> 
	if Tag.equal (tag_of_form x) t then x
	else get_aux xs
  in 
  get_aux (concls sq)

let get_tagged_form t sq =
  try 
    get_tagged_asm t sq
  with Not_found -> get_tagged_cncl t sq

let tag_to_index t sq =
  let rec index_aux fs i = 
    match fs with
      [] -> raise Not_found
    | x::xs ->
	if Tag.equal (tag_of_form x) t then i
	else index_aux xs (i+1)
  in 
  try 
    -(index_aux (asms sq) 1)
  with 
    Not_found -> index_aux (concls sq) 1

let index_to_tag i sq =
  let rec index_aux fs i=
    match fs with
      [] -> raise Not_found
    | x::xs ->
	if i=1 then (tag_of_form x)
	else index_aux xs (i-1)
  in 
  if(i<0) 
  then index_aux (asms sq)  (-i)
  else index_aux (concls sq) i


let dest_fident f sq=
  match f with
    FNum(x) -> x
  | FTag(x) -> tag_to_index x sq

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
	if Tag.equal t (sqnt_tag x) 
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

let get_all_goal_tags (Goal(sqs, _))=
  List.map sqnt_tag sqs

let get_goal_tag g =
  match g with
    (Goal(x::_, _))-> sqnt_tag x
  | _ -> raise Not_found

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

(* tag information for rules *)
(* goals: new goals produced by rule *)
(* forms: new forms produced by rule *)
    type tag_record = { goals:Tag.t list; forms : Tag.t list;}
    type tag_info = tag_record ref

    let make_tag_record gs fs = {goals=gs; forms=fs}
    let do_tag_info info gs fs =
      match info with
	None -> ()
      | Some(v) -> v:=make_tag_record gs fs

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

(* copy_asm i: 
   .., t:Ai, ..|- C
   ->
   .., t':Ai, t:Ai, .. |- C
   info: [] [t']
 *)

    let copy_asm0 info i sq = 
      let na=get_asm i sq 
      and nt = Tag.create()
      in
      let nb = (nt, sqnt_form na)
      in 
      do_tag_info info [] [nt];
      mk_subgoal (sqnt_tag sq, env sq,
		  Lib.splice_nth (-i) (asms sq) [nb; na],
		  concls sq)

    let copy_asm i g = sqnt_apply (copy_asm0 None i) g
    let copy_asm_info info i g = sqnt_apply (copy_asm0 (Some info) i) g

    let copy_asm_full info i g = 
      let sq=get_sqnt g
      in 
      sqnt_apply (copy_asm0 info (dest_fident i sq)) g

(* copy_cncl i: 
   A|- .., t:Ci, ..
   ->
   A|- .., t':Ci, t:Ci, ..
   info: [] [t']
 *)

    let copy_cncl0 info i sq=
      let nc=get_cncl i sq
      and nt=Tag.create()
      in 
      let nb = (nt, sqnt_form nc)
      in 
      do_tag_info info [] [nt];
      mk_subgoal(sqnt_tag sq, env sq,
		 asms sq,
		 Lib.splice_nth (i) (concls sq) [nb; nc])

    let copy_cncl i sqnt = sqnt_apply (copy_cncl0 None i) sqnt
    let copy_cncl_info inf i sqnt = sqnt_apply (copy_cncl0 (Some inf) i) sqnt

    let copy_cncl_full inf i g = 
      let sq=get_sqnt g
      in 
      sqnt_apply (copy_cncl0 inf (dest_fident i sq)) g


(* rotate asms/cncls:
   info: [] []
 *)

    let rotate_asms0 info sq = 
      let hs = asms sq 
      in
      do_tag_info info [] [];
      match hs with 
	[] -> mk_subgoal(sqnt_tag sq, env sq, hs, concls sq)
      | h::hys -> mk_subgoal(sqnt_tag sq, env sq, hys@[h], concls sq)

    let rotate_asms sqnt = sqnt_apply (rotate_asms0 None) sqnt
    let rotate_asms_info info sqnt = sqnt_apply (rotate_asms0 (Some info)) sqnt

    let rotate_cncls0 inf sq =
      let cs = concls sq in
      do_tag_info inf [] [];
      match cs with 
	[] -> mk_subgoal(sqnt_tag sq, env sq, asms sq, cs)
      | c::cns -> mk_subgoal(sqnt_tag sq, env sq, asms sq, cns@[c])

    let rotate_cncls sqnt = sqnt_apply (rotate_cncls0 None) sqnt
    let rotate_cncls_info inf sqnt = sqnt_apply (rotate_cncls0 (Some inf)) sqnt

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

   asm |- cncl      --> t:x, asm |- cncl
   info: [] [t]
 *)

    let cut0 info x sq=
      let nt = dest_thm x
      and scp = scope_of sq
      and ftag = Tag.create()
      in 
      let nasm = (ftag, nt)
      in 
      try 
	ignore(Formula.in_thy_scope scp (scp.Gtypes.curr_thy) nt);
	do_tag_info info [] [ftag];
	mk_subgoal(sqnt_tag sq, env sq, nasm::(asms sq), concls sq)
      with 
	x -> (addlogicError "Not in scope of sequent" [nt] x)

    let cut x sqnt = sqnt_apply (cut0 None x) sqnt
    let cut_info info x sqnt = sqnt_apply (cut0 (Some info) x) sqnt
    let cut_full info x sqnt = sqnt_apply (cut0 info x) sqnt

(* case x sq: adds formula x to assumptions of sq, 
   creates new subgoal in which to prove x

   g|asm |- cncl      --> g| t:x, asm |- cncl , g'|asm |- t:x, cncl

   info: [g, g'] [t]
 *)

    let case0 info nt sq=
      let scp = scope_of sq
      and ftag = Tag.create()
      and tagl=sqnt_tag sq
      and tagr=Tag.create()
      in 
      let ntrm=(ftag, nt)
      in 
      try 
	ignore(Formula.in_thy_scope scp (scp.Gtypes.curr_thy) nt);
	do_tag_info info [tagl; tagr] [ftag];
	[(tagl, env sq, ntrm::(asms sq), concls sq);
	 (tagr, env sq, (asms sq), ntrm::concls sq)]
      with 
	x -> (addlogicError "Not in scope of sequent" [nt] x)

    let case x sqnt = sqnt_apply (case0 None x) sqnt
    let case_info info x sqnt = sqnt_apply (case0 (Some info) x) sqnt
    let case_full info x sqnt = sqnt_apply (case0 info x) sqnt

(* delete x sq: delete assumption (x<0) or conclusion (x>0) from sq
   info: [] []
 *)

    let delete0 inf x sq=
      let ng=
	if x>0 then 
	  mk_subgoal (sqnt_tag sq, env sq, asms sq, delete_cncl x (concls sq))
	else 
	  mk_subgoal (sqnt_tag sq, env sq, delete_asm x (asms sq), concls sq)
      in 
      do_tag_info inf [] [];
      ng

    let delete x sqnt = sqnt_apply (delete0 None x) sqnt
    let delete_info inf x sqnt = sqnt_apply (delete0 (Some inf) x) sqnt

    let delete_full inf x g = 
      sqnt_apply (delete0 inf (dest_fident x (get_sqnt g))) g

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
   g| asm |- t:(a /\ b), concl   
   -->
   g| asm |- t:a  and g'| asm |- t:b 

   (where t:a means formula has tag t)
   info: [g;g'] [t]
 *)

    let conjI0 inf i sq=
      let (ft1, t)=(get_cncl i sq) 
      in 
      if (Formula.is_conj t) 
      then 
	(let (t1, t2) = get_pair (Formula.dest_conj t)
	in 
	let cnl1=(replace_cncl i (concls sq) (ft1, t1))
	and cnl2=(replace_cncl i (concls sq) (ft1, t2))
	and tagl=sqnt_tag sq
	and tagr=Tag.create()
	in 
	do_tag_info inf [tagl; tagr] [ft1];
	[(tagl, env sq, asms sq, cnl1); 
	 (tagr, env sq, asms sq, cnl2)])
      else raise (logicError "Not a conjunct" [t])

    let conjI i sqnt = sqnt_apply (conjI0 None i) sqnt
    let conjI_info inf i sqnt = sqnt_apply (conjI0 (Some inf) i) sqnt

    let conjI_full inf i g = 
      sqnt_apply (conjI0 inf (dest_fident i (get_sqnt g))) g

(* conjE i sq: 
   t:a/\ b, asm |- concl   
   -->
   t:a, t':b, asm |- concl 
   info: [] [t; t']
 *)

    let conjE0 inf i sq=
      let (ft1, t)=(get_asm i sq) 
      in 
      if (Formula.is_conj t) 
      then 
	(let (t1,  t2) = get_pair(Formula.dest_conj t)
	and ft2=Tag.create()
	in 
	let asm1=(ft1, t1)
	and asm2=(ft2, t2)
	in 
	do_tag_info inf [] [ft1; ft2];
	mk_subgoal(
	sqnt_tag sq, env sq, asm1::asm2::(delete_asm i (asms sq)), concls sq))
      else raise (logicError "Not a conjunction" [t])

    let conjE i sqnt = sqnt_apply (conjE0 None i) sqnt
    let conjE_info inf i sqnt = sqnt_apply (conjE0 (Some inf) i) sqnt

    let conjE_full inf i g = 
      sqnt_apply (conjE0 inf (dest_fident i (get_sqnt g))) g

(* disjI i sq: 
   g| t:a\/b, asm |-  concl   
   -->
   g| t:a, asm |- concl  and g'| t:b, asm |- concl
   info: [g;g'] [t]
 *)

    let disjI0 inf i sq=
      let (ft, t)=(get_asm i sq) 
      in 
      if (Formula.is_disj t) 
      then 
	(let (t1, t2) = get_pair (Formula.dest_disj t)
	in 
	let asm1=replace_asm i (asms sq) (ft, t1)
	and asm2=replace_asm i (asms sq) (ft, t2)
	and tagl=sqnt_tag sq
	and tagr=Tag.create()
	in 
	do_tag_info inf [tagl; tagr] [ft];
	[(tagl, env sq, asm1, concls sq); 
	 (tagr, env sq, asm2, concls sq)])
      else raise (logicError "Not a disjunction" [t])

    let disjI i sqnt = sqnt_apply (disjI0 None i) sqnt
    let disjI_info inf i sqnt = sqnt_apply (disjI0 (Some inf) i) sqnt
    let disjI_full inf i g = 
      sqnt_apply (disjI0 inf (dest_fident i (get_sqnt g))) g

(* disjE i sq: 
   asm |- t:a\/b, concl   
   -->
   asm |- t:a, t':b, concl 
   info: [] [t;t']
 *)

    let disjE0 inf i sq =
      let (ft1, t)=(get_cncl i sq) 
      in 
      if (Formula.is_disj t) 
      then 
	(let (t1, t2) = get_pair (Formula.dest_disj t)
	and ft2=Tag.create()
	in 
	let cncl1=(ft1, t1)
	and cncl2=(ft2, t2)
	in 
	do_tag_info inf [] [ft1; ft2];
	mk_subgoal 
	  (sqnt_tag sq, env sq, asms sq, 
	   cncl1::cncl2::(delete_cncl i (concls sq))))
      else raise (logicError "Not a disjunction" [t])

    let disjE i sqnt = sqnt_apply (disjE0 None i) sqnt
    let disjE_info inf i sqnt = sqnt_apply (disjE0 (Some inf) i) sqnt
    let disjE_full inf i g = 
      sqnt_apply (disjE0 inf (dest_fident i (get_sqnt g))) g

(* negA i sq:
   t:~a, asms |- concl
   -->
   asms |- t:a, concl
   info: [] [t]
 *)

    let negA0 inf i sq =
      let (ft, t)=(get_asm i sq) 
      in 
      if (Formula.is_neg t) 
      then 
	(let t1 = get_one(Formula.dest_neg t)
	in 
	let cncl1=(ft, t1)
	in 
	do_tag_info inf [] [ft];
	mk_subgoal
	  (sqnt_tag sq, env sq, delete_asm i (asms sq), cncl1::(concls sq)))
      else raise (logicError "Not a negation"[t])

    let negA i sqnt = sqnt_apply (negA0 None i) sqnt
    let negA_info inf i sqnt = sqnt_apply (negA0 (Some inf) i) sqnt
    let negA_full inf i g = 
      sqnt_apply (negA0 inf (dest_fident i (get_sqnt g))) g

(* negC i sq:
   asms |- t:~c, concl
   -->
   t:c, asms |- concl
   info: [] [t]
 *)

    let negC0 inf i sq =
      let (ft, t)=(get_cncl i sq) 
      in 
      if (Formula.is_neg t) 
      then 
	(let t1 = get_one (Formula.dest_neg t)
	in 
	let asm1=(ft, t1)
	in 
	do_tag_info inf [] [ft];
	mk_subgoal
	  (sqnt_tag sq, env sq, asm1::(asms sq), delete_cncl i (concls sq)))
      else raise (logicError "Not a negation"[t])

    let negC i sqnt = sqnt_apply (negC0 None i) sqnt
    let negC_info inf i sqnt = sqnt_apply (negC0 (Some inf) i) sqnt
    let negC_full inf i g = 
      sqnt_apply (negC0  inf (dest_fident i (get_sqnt g))) g

(* implI i sq
   asms |- t:a-> b, cncl 
   -->
   t':a, asms |- t:b, cncl
   info: [] [t'; t]
 *)

    let implI0 inf i sq=
      let (ft1, t)=(get_cncl i sq) 
      in 
      if (Formula.is_implies t) 
      then 
	(let  (t1, t2) = get_pair (Formula.dest_implies t)
	and ft2=Tag.create()
	in 
	let asm =(ft2, t1)
	and cncl = (ft1, t2)
	in 
	do_tag_info inf [] [ft2; ft1];
	mk_subgoal
	  (sqnt_tag sq, env sq, asm::(asms sq), 
	   (replace_cncl i (concls sq) cncl)))
      else raise (logicError "Not an implication" [t])

    let implI i sqnt = sqnt_apply (implI0 None i) sqnt
    let implI_info inf i sqnt = sqnt_apply (implI0 (Some inf) i) sqnt
    let implI_full inf i g = 
      sqnt_apply (implI0  inf (dest_fident i (get_sqnt g))) g

(* implE i sq
   g| t:a-> b,asms |-cncl 
   -->
   g'| asms |- t:a, cncl  and  g| t:b, asms |- cncl

   info: [g'; g]  [t]

   where g| asms |- concl 
   means g is the tag for the sequent
 *)

    let implE0 info i sq =
      let (ft, t)=(get_asm i sq) 
      in 
      if (Formula.is_implies t) 
      then 
	(let (t1, t2) = get_pair (Formula.dest_implies t)
	in 
	let asm2=replace_asm i (asms sq) (ft, t2)
	and cncl1=(ft, t1)::(concls sq)
	and asm1 = delete_asm i (asms sq)
	and tagl=Tag.create()
	and tagr=sqnt_tag sq
	in 
	do_tag_info info [tagl; tagr] [ft];
	[(tagl, env sq, asm1, cncl1); 
	 (tagr, env sq, asm2, concls sq)])
      else raise (logicError "Not an implication" [t])

    let implE i sqnt = sqnt_apply (implE0 None i) sqnt
    let implE_info info i sqnt = sqnt_apply (implE0 (Some info) i) sqnt
    let implE_full info i g = 
      sqnt_apply (implE0 info (dest_fident i (get_sqnt g))) g

(* allI i sq
   asm |- t:!x. P(c), concl
   -->
   asm |- t:P(c'), concl   where c' is a new identifier
 *)

    let add_sklms_to_scope sklms scp =
      Gtypes.extend_scope scp 
	(fun x -> 
	  let y =
	    (if (thy_of_id x)=null_thy
	    then mklong scp.Gtypes.curr_thy (name x)
	    else x)
	  in get_sklm_type (get_old_sklm y sklms))

    let allI0 inf i sq =
      let (ft, t)=(get_cncl i sq)
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
	let ncncl = (ft, Formula.inst nscp t sv)
	in 
	do_tag_info inf [] [ft];
	mk_subgoal(sqnt_tag sq, {sklms=nsklms; sqenv=nscp},
		   asms sq, 
		   replace_cncl i (concls sq) ncncl))
      else raise (logicError "Not a universal quantifier" [t])

    let allI i sqnt = sqnt_apply (allI0 None i) sqnt
    let allI_info inf i sqnt = sqnt_apply (allI0 (Some inf) i) sqnt
    let allI_full inf i g = 
      sqnt_apply (allI0 inf (dest_fident i (get_sqnt g))) g

(* existI i sq
   t:?x. P(c), asm |- concl
   -->
   t:P(c'), asm |- concl   where c' is a new identifier
   info: [] [t]
 *)

    let existI0 inf i sq =
      let (ft, t)=(get_asm i sq)
      in 
      if (Formula.is_exists t)
      then 
	(let (nv, nty) = (Formula.get_binder_name t, Formula.get_binder_type t)
	in 
	let (sv, nsklms) = 
	  get_new_sklm (mklong (thy_of_sq sq) nv) nty (sklm_cnsts sq)
	in 
	let nscp = add_sklms_to_scope nsklms (scope_of sq)
	in 
	let nasm=(ft, Formula.inst nscp t sv)
	in 
	do_tag_info inf [] [ft];
	mk_subgoal
	  (sqnt_tag sq, {sklms=nsklms; sqenv = nscp},
           replace_asm i (asms sq) nasm, 
	   concls sq))
      else raise (logicError "Not an existential quantifier" [t])

    let existI i sqnt = sqnt_apply (existI0 None i) sqnt
    let existI_info inf i sqnt = sqnt_apply (existI0 (Some inf) i) sqnt
    let existI_full inf i g = 
      sqnt_apply (existI0 inf (dest_fident i (get_sqnt g))) g

(* trueR i sq
   t:asm |- true, concl
   --> true
   info : [] []
 *)

    let trueR0 inf i sq=
      let (_, t)=(get_cncl i sq)
      in 
      if (Formula.is_true t)
      then 
	(do_tag_info inf [] [];
	 raise No_subgoals)
      else 
	raise (logicError "Not trivial" [t])

    let trueR i sqnt = sqnt_apply (trueR0 None i) sqnt
    let trueR_info inf i sqnt = sqnt_apply (trueR0 (Some inf) i) sqnt
    let trueR_full inf i g = 
      sqnt_apply (trueR0 inf (dest_fident i (get_sqnt g))) g

(* beta i sq:  (beta reduction of asm (i<0) or concl (i>0) in sq)
   t:(%x.P(x))(c), asm |- concl
   -->
   t:P(c), asm |- concl
   info: [] [t] 
 *)

    let beta0 inf i sq = 
      let (ft, t) = if i>0 then get_cncl i sq else get_asm i sq
      in 
      let nt = 
	(ft,
	 try 
	   Formula.beta_reduce (scope_of sq) t
	 with x -> raise 
	     (Result.catchError(mklogicError "Beta reduction" [t]) x))
      in 
      do_tag_info inf [] [ft];
      if i> 0 
      then mk_subgoal
	  (sqnt_tag sq, env sq, (asms sq), replace_cncl i (concls sq) nt)
      else mk_subgoal
	  (sqnt_tag sq, env sq, replace_asm i (asms sq) nt, concls sq)

    let beta i sqnt = sqnt_apply (beta0 None i) sqnt
    let beta_info inf i sqnt = sqnt_apply (beta0 (Some inf) i) sqnt

(* name_rule: introduce a new name in the sqnt as a synonym for a term  *)
(* name id trm:
   Asm|-Cncl -> t:id=trm, Asm|-Cncl

   the long name thy.id must be unique (where thy is the current theory name)
   info: [] [t]
 *)

    let name_rule0 inf id trm sq =
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
	  and ft=Tag.create()
	  in
	  let ntrm = 
	    (ft, Formula.form_of_term nscp
	       (Logicterm.mkequal (Term.mkvar long_id) trm))
	  in 
	  do_tag_info inf [] [ft];
	  mk_subgoal(sqnt_tag sq, {sklms=(sklm_cnsts sq); sqenv=nscp},
		     ntrm::(asms sq), concls sq)

    let name_rule id trm sqnt = sqnt_apply (name_rule0 None id trm) sqnt
    let name_rule_info inf id trm sqnt = 
      sqnt_apply (name_rule0 (Some inf) id trm) sqnt

    let name_rule_full inf id trm sqnt = 
      sqnt_apply (name_rule0 inf id trm) sqnt


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
   info: [] []
 *)

    let unify0 inf i j sq = 
      let tyenv = scope_of sq
      and (_, asm) = get_asm i sq
      and (_, cncl) = get_cncl j sq
      in 
      try
	((ignore(Formula.unify tyenv asm cncl); 
	  do_tag_info inf [] [];
	  raise No_subgoals)) 
      with x -> 
	try
	  ((ignore(Formula.alpha_convp tyenv asm cncl); 
	    do_tag_info inf [] [];
	    raise No_subgoals)) 
	with x->
	  raise (Result.catchError
		   (mklogicError "Can't unify assumption with conclusion"
		      [sqnt_form (get_asm i sq); 
		       sqnt_form (get_cncl j sq)]) x)
	    
    let unify i j sqnt = sqnt_apply (unify0 None i j) sqnt
    let unify_info inf i j sqnt = 
      sqnt_apply (unify0 (Some inf) i j) sqnt

    let unify_full inf i j g = 
      let sq=get_sqnt g
      in 
      sqnt_apply (unify0 inf (dest_fident i sq) (dest_fident j sq)) g

(* existE i sq
   asm |- t:?x. P(c), concl
   -->
   asm |- t:P(c'), concl   where c' is a given term
   info: [] [t]
 *)

    let existE0 inf trm i sq =
      let (ft, t)=(get_cncl i sq)
      in 
      if (Formula.is_exists t) 
      then 
	try 
      	  (let tenv = is_inst_term sq trm (Formula.get_binder_type t)
      	  in 
      	  let nt = (ft, inst_term sq t trm)
      	  in 
	  do_tag_info inf [] [ft];
      	  mk_subgoal
	    (sqnt_tag sq, env sq, asms sq, (replace_cncl i (concls sq) nt)))
	with x -> raise (Result.catchError
			   (mklogicError "existE:" [t]) x)
      else 
	raise (logicError "Not an existential quantifier" [t])

    let existE trm i sqnt = sqnt_apply (existE0 None trm i) sqnt
    let existE_info inf trm i sqnt = 
      sqnt_apply (existE0 (Some inf) trm i) sqnt

    let existE_full inf trm i g = 
      sqnt_apply (existE0 inf trm (dest_fident i (get_sqnt g))) g

(* allE i sq
   t:!x. P(c), asm |-  concl
   -->
   t:P(c'), asm |- concl   where c' is a given term
   info: [] [t]
 *)

    let allE0 inf trm i sq =
      let (ft, t)=(get_asm i sq)
      in 
      if (Formula.is_all t) 
      then 
	try 
	  (let tenv = is_inst_term sq trm (Formula.get_binder_type t)
	  in 
	  let nt = (ft, inst_term  sq t trm)
	  in 
	  do_tag_info inf [] [ft];
	  mk_subgoal
	    (sqnt_tag sq, env sq,  (replace_asm i (asms sq) nt), concls sq)) 
	with x -> 
	  (raise (Result.catchError
		    (mklogicError "allE: " [t]) x))
      else 
	raise (logicError "Not a universal quantifier" [t])

    let allE trm i sqnt = sqnt_apply (allE0 None trm i) sqnt
    let allE_info inf trm i sqnt = sqnt_apply (allE0 (Some inf) trm i) sqnt

    let allE_full inf trm i g = 
      sqnt_apply (allE0 inf trm (dest_fident i (get_sqnt g))) g


(* rewrite_any dir simple thms j sq:
   list of theorems or assumptions containing x=y
   asm |- t:P(x), concl
   -->
   asm |- t:P(y), concl
   where dir is =true for right-left and false for left-right
   theorems must be in scope.
   silently discards theorems not in scope and assumptions which don't exist
   info: [] [t]
 *)

    let filter_rules scp rls j sq= 
      let memo = Lib.empty_env() 
      and thyname= scp.Gtypes.curr_thy
      in 
      let rec ft srcs rslt =
	match srcs with 
	  [] -> List.rev rslt
	| ((Asm(x))::xs) -> 
	    if x=j then 
	      raise (logicError "Rewrite" [sqnt_form (get_asm x sq)])
	    else 
	      ft xs ((sqnt_form (get_asm x sq))::rslt)
	| (Tagged(x)::xs) -> 
	    let tgdasm=
	      (try 
		get_tagged_asm x sq
	      with 
		Not_found -> 
		  raise 
		    (logicError "Rewrite: can't find tagged assumption" []))
	    in 
	    ft xs ((sqnt_form tgdasm)::rslt)
	| ((RRThm(x))::xs) -> 
	    if (in_thy_scope_memo memo scp thyname (dest_thm x))
	    then ft xs ((dest_thm x)::rslt)
	    else ft xs rslt
      in ft rls []

    let rewrite_any0 inf dir simple rls j sq=
      let tyenv = scope_of sq
      in 
      let r=filter_rules tyenv rls j sq
      and (ft, t)=if j>=0 then (get_cncl j sq)  else (get_asm j sq)
      in 
      try
	(let nt = 
	  if simple 
	  then 
	    (ft, Formula.rewrite_simple ~dir:dir tyenv r t)
	  else 
	    (ft, Formula.rewrite ~dir:dir tyenv r t)
	in 
	do_tag_info inf [] [ft];
	if j>=0 then
	  mk_subgoal
	    (sqnt_tag sq, env sq, asms sq, replace_cncl j (concls sq) nt)
	else 
	  mk_subgoal
	    (sqnt_tag sq, env sq, replace_asm j (asms sq) nt, concls sq))
      with x -> raise 
	  (Result.catchError (mklogicError"rewriting" (t::r)) x)

    let rewrite_any ?(dir=true) ?(simple=false) rls j sqnt
	= sqnt_apply (rewrite_any0 None dir simple rls j) sqnt

    let rewrite_any_info inf ?(dir=true) ?(simple=false) rls j sqnt
	= sqnt_apply (rewrite_any0 (Some inf) dir simple rls j) sqnt

    let rewrite_any_full inf ?(dir=true) ?(simple=false) rls j g=
      sqnt_apply 
	(rewrite_any0 inf dir simple rls 
	   (dest_fident j (get_sqnt g))) g

(* rewrite dir i j sq
   x=y, asm |- t:P(x), concl
   -->
   x=y, asm |- t:P(y), concl
   where dir is =true for right-left and false for left-right
 *)

    let get_asms xs j sq = 
      List.map 
	(fun x -> 
	  if x=j then raise (logicError "Rewrite" [sqnt_form (get_asm x sq)])
	  else (sqnt_form (get_asm x sq))) xs

    let rewrite0 inf dir i j sq=
      let r=get_asms i j sq
      and (ft, t)=if j>=0 then (get_cncl j sq)  else (get_asm j sq)
      and tyenv = scope_of sq
      in 
      try
	(let nt = (ft, Formula.rewrite ~dir:dir tyenv r t)
	in 
	do_tag_info inf [] [ft];
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

    let rewrite_info inf ?(dir=true) i j  sqnt
	= rewrite_any_info inf ~dir:dir (List.map (fun x-> Asm(x)) i) j sqnt

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

    let rewrite_thms0 inf dir thms j sq=
      let tyenv = scope_of sq
      in 
      let r=filter_thms tyenv thms
      and (ft, t)=if j>=0 then (get_cncl j sq)  else (get_asm j sq)
      in 
      try
	(let nt = (ft, Formula.rewrite ~dir:dir tyenv r t)
	in 
	do_tag_info inf [] [ft];
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

    let rewrite_thms_info inf ?(dir=true) thms j sqnt
	= rewrite_any_info inf ~dir:dir (List.map (fun x-> RRThm(x)) thms) j sqnt

  end

open Rules
module ThmRules=
  struct

(* conversions for theorems *)

(* conjE_conv "|- A and B"  -> "|- A" "|- B" *)

    let mk_same_thm t f = 
      if (is_axiom t) then mk_axiom f else mk_theorem f

    let get_one_thm t = 
      match t with
	[f] -> f
      | _ -> raise 
	    (logicError "Expected one theorem only" (List.map dest_thm t))

    let conjE_conv thm=
      let t = thm
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

    let rewrite_conv scp ?(dir=true) ?(simple=false) rrl thm =
      let conv_aux t = 
	try 
	  let f= dest_thm t
	  and rs=List.map (fun x -> Formula.rename (dest_thm x)) rrl
	  in 
	  let nt = 
	    if simple
	    then
	      (Formula.rewrite_simple ~dir:dir scp rs f)
	    else 
	      (Formula.rewrite ~dir:dir scp rs f)
	  in mk_same_thm t nt
	with x -> raise 
	    (Result.catchError(mklogicError "rewrite_conv" [dest_thm t]) x)
      in 
      conv_aux thm

	
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
