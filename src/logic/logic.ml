open Basic
open Formula


type thm = 
    Axiom of form
  | Theorem of form

type saved_thm = 
    Saxiom of saved_form
  | Stheorem of saved_form

type tagged_form = (Tag.t* form)

type skolem_cnst = (Basic.ident * (int * Basic.gtype))
type skolem_type = skolem_cnst list

(* 
   A sequent is made up of
   a unique tag
   information about skolem constants (the sqnt_env)
   a list of tagged formulas: the assumptions
   a list of tagged formulas: the conclusions
 *)
(* 
   A sqnt_env is made up of
   the shared type variables (Gtypes.WeakVar) that may be used in the sequent
   information for constructing names of weak types
   the skolem constants (Term.Meta) that may be used in the sequent
   the scope of the sequent
 *)
type sqnt_env = 
    {
     sklms: skolem_type; 
     sqscp : Gtypes.scope;
     tyvars: Basic.gtype list;
     tynames: (string * int) list;
   }
type sqnt = (Tag.t* sqnt_env * tagged_form list * tagged_form list)

(* 
   A goal is made up of
   a list of sequents: the sub-goals still to be proved
   a type environment: the bindings of the shared type variables 
   which occur in the goals sequents.
   a formula: the theorem which is to be proved
 *)
type goal =  Goal of (sqnt list * Gtypes.substitution * form)

type rule = goal -> goal
type conv = thm list -> thm list 

(* 
   cdefn:
   Checked Definitions: 
   checking of type and term definitions and declarations
 *)

type cdefn =
    TypeDef of Basic.ident * string list * Basic.gtype option
  | TermDef of 
      Basic.ident * Basic.gtype
	* (string*Basic.gtype) list * thm option

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


(* theorem recognisers/destructors *)

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
  Result.mk_error((new Term.termError s 
		    (List.map Formula.term_of_form t)):>Result.error)
let addlogicError s t es = 
  raise (Result.add_error (logicError s t) es)


(* Skolem constants *)

let get_sklm_name (x, (_, _)) = x
let get_sklm_indx (_, (i, _)) = i
let get_sklm_type (_, (_, t)) = t

let get_old_sklm n sklms =  (n, List.assoc n sklms)

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

let add_sklms_to_scope sklms scp =
  Gtypes.extend_scope scp 
    (fun x -> 
      let y =
	(if (thy_of_id x)=null_thy
	then mklong scp.Gtypes.curr_thy (name x)
	else x)
      in 
      get_sklm_type (get_old_sklm y sklms))

(* 
   Skolem Constants: new version 
 *)

(* mk_skolem scp n ty:
   make a new skolem constant with name n and type ty
   scope scp is needed for unification
   return the new term and its type 
 *)

(*
   let mk_skolem scp n ty = 
   (* tyname: if ty is a variable then use its name for the
      weak variable otherwise use the empty string
    *)
   let tyname=
   if Gtypes.varp ty 
   then !(Gtypes.dest_var ty) 
   else ""
   in
   (* make the weak type *)
   let nty=Gtypes.mk_weak tyname
   in 
   (* unify the weak type with the given type *)
   let rty=Gtypes.mgu nty (Gtypes.unify scp nty ty)
   in 
   (Term.mkmeta n rty, rty)
 *)

(* 
   [new_weak_type n names]

   Make a new weak type with a name derived from [n] and [names].
   Return this type and the updated names.
 *)

let new_weak_type n names=
  (* get the index of the name (if any) *)
  let nm_int=
    try List.assoc n names
    with Not_found -> 0
  in 
  let nnames = Lib.replace n (nm_int+1) names
  in 
  let nm_s = (n^(Lib.int_to_name nm_int))
  in (nm_s, nnames)

(* [mk_new_skolem scp n ty]

   make a new skolem constant with name [n] and type [ty]
   scope [scp] is needed for unification
   return the new identifier, its type and the updated 
   information for skolem building
 *)

type skolem_info=
    {
     name: Basic.ident;
     ty: Basic.gtype;
     tyenv: Gtypes.substitution;
     scope: Gtypes.scope;
     skolems: skolem_type;
     tylist: (string*int) list
   }

let mk_new_skolem info=
  (* tyname: if ty is a variable then use its name for the
     weak variable otherwise use the empty string
   *)
  let tyname x=
    if Gtypes.is_var info.ty 
    then new_weak_type (Gtypes.get_var info.ty) info.tylist
    else new_weak_type (x^"_ty") info.tylist
  in
  (* make the weak type *)
  let mk_nty x=
    let ty_name, nnames=tyname x
    in 
    let tty=Gtypes.mk_weak ty_name
    in 
    (* unify the weak type with the given type *)
    let ntyenv=Gtypes.unify_env info.scope tty info.ty info.tyenv
    in 
    (Gtypes.mgu tty ntyenv, ntyenv, nnames)
  in 
  try 
    (* see if name is already associated with a skolem *)
    let oldsk = get_old_sklm info.name info.skolems
    in 
    (* get new index for skolem named n *)
    let nindx = (get_sklm_indx oldsk)+1
    in 
    (* make the new identifier *)
    let nnam = 
      mklong 
	(thy_of_id info.name) 
	((name info.name)^"_"^(string_of_int nindx))
    in 
    let nty, ntyenv, new_names=mk_nty (name nnam)
    in 
    (Term.mk_typed_var nnam nty, nty, (nnam, (nindx, nty))::info.skolems, 
     ntyenv, new_names)
  with Not_found -> 
    let nnam=
      mklong 
	(thy_of_id info.name) ((name info.name)^"_"^(string_of_int 1))
    in 
    let nty, ntyenv, new_names=mk_nty (name nnam)
    in 
    (Term.mk_typed_var nnam nty, nty, (nnam, (1, nty))::info.skolems, 
     ntyenv, new_names)


(* Sequent manipulation *)

let asms (_, _, asl, _) = asl
let concls (_, _, _, cnl) = cnl
let sqnt_env (_, e, _, _) = e
let sklm_cnsts (_, e, _, _) = e.sklms
let scope_of (_, e, _, _) = e.sqscp
let sqnt_tyvars (_, e, _, _)=e.tyvars
let sqnt_tynames (_, e, _, _)=e.tynames
(*let sqnt_consts (_, e, _, _)=e.consts*)
let sqnt_tag(t, _, _, _) = t

let form_tag (t, _) = t
let sqnt_form (_, f) = f

(* mk_sqnt x: |- x  (with x to be proved)*) 

let mk_sqnt_form f = (Tag.create(), f)

let mk_sqnt_env sks scp tyvs names=
  {sklms=sks; sqscp=scp; tyvars=tyvs; tynames=names}

let mk_sqnt tg env ps cs= (tg, env, ps, cs)
let dest_sqnt (tg, env, ps, cs) = (tg, env, ps, cs)

let new_sqnt scp x = 
  let env=mk_sqnt_env [] scp [] []
  in 
  mk_sqnt (Tag.create()) env [] [mk_sqnt_form x]

let sqnt_scope sq = scope_of sq

let get_asm i sq = 
  let (t, f) = try (List.nth (asms sq) ((-i)-1)) with _ -> raise Not_found
  in (t, rename f)

let delete_asm i asms = (Lib.delete_nth (-i) asms)
let replace_asm i asms a = (Lib.replace_nth (-i) asms a)

let get_cncl i sq = 
  let (t, f) = try (List.nth (concls sq) (i-1)) with _ -> raise Not_found
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

let fident_to_tag f sq=
  match f with
    FNum(x) -> index_to_tag x sq
  | FTag(x) -> x


let sqntError s = 
  Result.mk_error(new Result.error s)
let addsqntError s es = 
  raise (Result.add_error (sqntError s) es)

let thy_of_sq sq = (scope_of sq).Gtypes.curr_thy

(* Subgoals *)
exception No_subgoals

(* 
   Solved_subgoal tyenv:
   solved a subgoal, creating new goal type environment tyenv
 *)
exception Solved_subgoal of Gtypes.substitution

let has_subgoals g = 
  match g with
    Goal([], _, _) -> false
  | _ -> true

(* get_sqnt: take first subgoal of goal *)

let get_sqnt g=
  match g with
    Goal(s::_, _, f) -> s
  | _ -> raise (Result.error "get_sqnt: No subgoals")

let goal_tyenv (Goal(_, e, _)) = e

let combine_subgoals a b = a@b

let get_goal (Goal(_, _, f)) = f

let get_subgoal_tags (Goal(sqs, _, _)) = List.map sqnt_tag sqs

let goal_focus t (Goal(sqnts, tyenv, f)) =
  let rec focus sqs rslt=
    match sqs with
      [] -> raise Not_found
    | (x::xs) -> 
	if Tag.equal t (sqnt_tag x) 
	then (x::((List.rev rslt)@xs))
	else focus xs (x::rslt)
  in Goal(focus sqnts [], tyenv, f)
    

let apply_nth r i g = 
  match g with 
    Goal(sgs, tyenv, f) ->
      let nsgs = r (try (List.nth sgs i) with _ -> raise Not_found)
      in Goal(Lib.splice_nth i sgs [nsgs], tyenv, f)

let get_tyenv (Goal(_, tyenv, _)) = tyenv

let num_of_subgoals x = 
  match x with
    (Goal(sqs, _, _)) -> List.length sqs

let get_subgoals (Goal(sq, tyenv, _)) = sq

let get_nth_subgoal_sqnt i (Goal(sq, _, _)) =
  try (List.nth sq i)
  with _ -> 
    raise Not_found (*(sqntError "get_nth_subgoal: invalid argument")*)

let goal_has_subgoals g =
  match g with 
    (Goal([], _, _)) -> false
  | (Goal(_, _, _)) -> true

let get_all_goal_tags (Goal(sqs, _, _))=
  List.map sqnt_tag sqs

let get_goal_tag g =
  match g with
    (Goal(x::_, _, _))-> sqnt_tag x
  | _ -> raise Not_found

let mk_goal scp f = 
  let nf= Formula.typecheck scp f (Gtypes.mk_bool)
  in 
  Goal([new_sqnt scp nf], Gtypes.empty_subst(), nf)

let mk_thm g = 
  match g with 
    Goal([], _, f) -> Theorem f
  | _ -> raise (logicError "Not a theorem" [])


let print_thm pp t = 
  Format.open_box 3; 
  print_string "|- ";
  Term.print pp (Formula.term_of_form (dest_thm t));
  Format.close_box();

module Rules=
  struct

(* 
   Rules:
   The implementation of the rules of the sequent calculus

   Information:
   Each tactic implementing a basic rule produces information about 
   the tags of the formulas affected by applying the rule.
   e.g. applying conjunction introduction to [a and b], produces 
   the tags for the two new formulas [a] and [b]
 *)

(* sqnt_apply f g:
   apply function (f: Gtype.substitution->sqnt
   ->(sqnt list *Gtypes.substitution))
   to the first subgoal of g
 *)

(*
   let sqnt_apply r g =
   match g with 
   Goal([], _, f) -> raise No_subgoals
   | Goal(x::xs, tyenv, f) -> 
   (try 
   let ng, ntyenv=r tyenv x
   in 
   Goal(ng@xs, ntyenv, f)
   with No_subgoals -> Goal(xs, tyenv, f)
   | Solved_subgoal ntyenv -> Goal(xs, ntyenv, f))
 *)
    let sqnt_apply r g =
      match g with 
	Goal([], _, f) -> raise No_subgoals
      | Goal(x::xs, tyenv, f) -> 
	  (try 
	    let ng, ntyenv=r tyenv x
	    in 
	    Goal(ng@xs, ntyenv, f)
	  with No_subgoals -> Goal(xs, tyenv, f)
	  | Solved_subgoal ntyenv -> Goal(xs, ntyenv, f))

(* simple_sqnt_apply f g:
   apply function (f: sqnt -> sqnt list) to the first subgoal of g
   Like sqnt_apply but does not change [tyenv] of [g].
   Used for rules which do not alter the type environment.
 *)

(*
   let simple_sqnt_apply r g =
   match g with 
   Goal([], _, f) -> raise No_subgoals
   | Goal(x::xs, tyenv, f) -> 
   try 
   let ng=r x
   in 
   Goal(ng@xs, tyenv, f)
   with No_subgoals -> Goal(xs, tyenv, f)
   | Solved_subgoal ntyenv -> Goal(xs, ntyenv, f)
 *)
    let simple_sqnt_apply r g =
      match g with 
	Goal([], _, f) -> raise No_subgoals
      | Goal(x::xs, tyenv, f) -> 
	  try 
	    let ng=r x
	    in 
	    Goal(ng@xs, tyenv, f)
	  with No_subgoals -> Goal(xs, tyenv, f)
	  | Solved_subgoal ntyenv -> Goal(xs, ntyenv, f)


    let mk_subgoal sq = [sq]

    let goal_apply r g = 
      match g with 
	Goal([], _, _) -> raise (sqntError "No subgoals")
      |(Goal(sqs, tyenv, f)) -> r g	

    let postpone g = 
      match g with
	Goal (sq::[], _, _) -> raise (sqntError "postpone: No other subgoals")
      | Goal (sg::sgs, tyenv, f) -> 
	  Goal (List.concat [sgs;[sg]], tyenv, f)
      | _ -> raise (sqntError "postpone: No subgoals")

    let goal_postpone g = 
      match g with
	(Goal(sqs, tyenv, f)) -> postpone g

(* tag information for rules *)
(* goals: new goals produced by rule *)
(* forms: new forms produced by rule *)
(* terms: new constants produced by rule *)
    type tag_record = 
	{ 
	  goals:Tag.t list; 
	  forms : Tag.t list;
	  terms: Basic.term list
	}
    type tag_info = tag_record ref

    let make_tag_record gs fs ts = {goals=gs; forms=fs; terms=ts}
    let do_tag_info info gs fs ts=
      match info with
	None -> ()
      | Some(v) -> v:=make_tag_record gs fs ts

(* composition of rules *)

(* lift assumptions/conclusions *)


(* foreach r sqs:
   applies r to each subgoal in g
 *)
(*
    let foreach r g =
      let rec each_aux tyenv gs rslts =
	match gs with 
	  [] -> rslts, tyenv
	| (x::xs) ->
	    let nx, xtyenv=r tyenv x
	    in 
	    each_aux xtyenv xs nx
      in 
      match g with 
	Goal([], _, f) -> raise No_subgoals
      | Goal(sqs, tyenv, f) -> 
	  let nsqs, ntyenv=each_aux tyenv sqs []
	  in 
	  Goal(List.flatten(List.rev nsqs), ntyenv, f)

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
	| r::rs-> 
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

*)

(* Lifting assumption/conclusion formulas *)
(* 
   extract_tagged_asm/concl: 
   find asm with given tag, remove it from list of tagged formulas
   return it and the new list of tagged formulas
 *)

    let extract_tagged_formula t fs = 
      let rec extract_aux ams rs= 
	match ams with
	  [] -> raise Not_found
	| x::xs -> 
	    if Tag.equal (tag_of_form x) t 
	    then (x, List.rev_append xs rs)
	    else extract_aux xs (x::rs)
      in 
      let nf, nfs=extract_aux fs []
      in 
      (nf, List.rev nfs)

    let lift_tagged id fs =
      let nf, nfs = extract_tagged_formula id fs
      in 
      nf::nfs

    let lift_asm_sq f sq = 
      let id = fident_to_tag f sq
      in 
      [mk_sqnt 
	 (sqnt_tag sq) (sqnt_env sq) 
	 (lift_tagged id (asms sq)) (concls sq)]

    let lift_asm f g =
      simple_sqnt_apply (lift_asm_sq f) g

    let lift_concl_sq f sq = 
      let id = fident_to_tag f sq
      in 
      [mk_sqnt 
	 (sqnt_tag sq) (sqnt_env sq) (asms sq)  (lift_tagged id (concls sq))]

    let lift_concl f g =
      simple_sqnt_apply (lift_concl_sq f) g

	
    let lift f g =
      try 
	lift_asm f g
      with Not_found -> lift_concl f g


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
      do_tag_info info [] [nt] [];
      mk_subgoal (sqnt_tag sq, sqnt_env sq,
		  Lib.splice_nth (-i) (asms sq) [nb; na],
		  concls sq)

    let copy_asm i g = simple_sqnt_apply (copy_asm0 None i) g
    let copy_asm_info info i g = simple_sqnt_apply (copy_asm0 (Some info) i) g

    let copy_asm_full info i g = 
      let sq=get_sqnt g
      in 
      simple_sqnt_apply (copy_asm0 info (dest_fident i sq)) g

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
      do_tag_info info [] [nt] [];
      mk_subgoal(sqnt_tag sq, sqnt_env sq,
		 asms sq,
		 Lib.splice_nth (i) (concls sq) [nb; nc])

    let copy_cncl i sqnt = simple_sqnt_apply (copy_cncl0 None i) sqnt
    let copy_cncl_info inf i sqnt = 
      simple_sqnt_apply (copy_cncl0 (Some inf) i) sqnt

    let copy_cncl_full inf i g = 
      let sq=get_sqnt g
      in 
      simple_sqnt_apply (copy_cncl0 inf (dest_fident i sq)) g


(* rotate asms/cncls:
   info: [] []
 *)

    let rotate_asms0 info sq = 
      let hs = asms sq 
      in
      do_tag_info info [] [] [];
      match hs with 
	[] -> mk_subgoal(sqnt_tag sq, sqnt_env sq, hs, concls sq)
      | h::hys -> mk_subgoal(sqnt_tag sq, sqnt_env sq, hys@[h], concls sq)

    let rotate_asms sqnt = simple_sqnt_apply (rotate_asms0 None) sqnt
    let rotate_asms_info info sqnt = 
      simple_sqnt_apply (rotate_asms0 (Some info)) sqnt

    let rotate_cncls0 inf sq =
      let cs = concls sq in
      do_tag_info inf [] [] [];
      match cs with 
	[] -> mk_subgoal(sqnt_tag sq, sqnt_env sq, asms sq, cs)
      | c::cns -> mk_subgoal(sqnt_tag sq, sqnt_env sq, asms sq, cns@[c])

    let rotate_cncls sqnt = simple_sqnt_apply (rotate_cncls0 None) sqnt
    let rotate_cncls_info inf sqnt = 
      simple_sqnt_apply (rotate_cncls0 (Some inf)) sqnt

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
	do_tag_info info [] [ftag] [];
	mk_subgoal(sqnt_tag sq, sqnt_env sq, nasm::(asms sq), concls sq)
      with 
	x -> (addlogicError "Not in scope of sequent" [nt] x)

    let cut x sqnt = simple_sqnt_apply (cut0 None x) sqnt
    let cut_info info x sqnt = simple_sqnt_apply (cut0 (Some info) x) sqnt
    let cut_full info x sqnt = simple_sqnt_apply (cut0 info x) sqnt

(* 
   delete x sq: delete assumption (x<0) or conclusion (x>0) from sq
   info: [] []
 *)

    let delete0 inf x sq=
      let ng=
	if x>0 then 
	  mk_subgoal (sqnt_tag sq, sqnt_env sq, asms sq, delete_cncl x (concls sq))
	else 
	  mk_subgoal (sqnt_tag sq, sqnt_env sq, delete_asm x (asms sq), concls sq)
      in 
      do_tag_info inf [] [] [];
      ng

    let delete x sqnt = simple_sqnt_apply (delete0 None x) sqnt
    let delete_info inf x sqnt = 
      simple_sqnt_apply (delete0 (Some inf) x) sqnt

    let delete_full inf x g = 
      simple_sqnt_apply (delete0 inf (dest_fident x (get_sqnt g))) g

(* 
   basic i j sq: compares asm i with cncl j of sq, 
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
	do_tag_info inf [tagl; tagr] [ft1] [];
	[mk_sqnt tagl (sqnt_env sq) (asms sq) cnl1;
	 mk_sqnt tagr (sqnt_env sq) (asms sq) cnl2])
(*
   [(tagl, sqnt_env sq, asms sq, cnl1); 
   (tagr, sqnt_env sq, asms sq, cnl2)])
 *)
      else raise (logicError "Not a conjunct" [t])

    let conjI i sqnt = simple_sqnt_apply (conjI0 None i) sqnt
    let conjI_info inf i sqnt = 
      simple_sqnt_apply (conjI0 (Some inf) i) sqnt

    let conjI_full inf i g = 
      simple_sqnt_apply (conjI0 inf (dest_fident i (get_sqnt g))) g

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
	do_tag_info inf [] [ft1; ft2] [];
	mk_subgoal(
	sqnt_tag sq, sqnt_env sq, asm1::asm2::(delete_asm i (asms sq)), concls sq))
      else raise (logicError "Not a conjunction" [t])

    let conjE i sqnt = simple_sqnt_apply (conjE0 None i) sqnt
    let conjE_info inf i sqnt = simple_sqnt_apply (conjE0 (Some inf) i) sqnt

    let conjE_full inf i g = 
      simple_sqnt_apply (conjE0 inf (dest_fident i (get_sqnt g))) g

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
	do_tag_info inf [tagl; tagr] [ft] [];
	[mk_sqnt tagl (sqnt_env sq) asm1 (concls sq);
	 mk_sqnt tagr (sqnt_env sq) asm2 (concls sq)])
(*
   [(tagl, sqnt_env sq, asm1, concls sq); 
   (tagr, sqnt_env sq, asm2, concls sq)])
 *)
      else raise (logicError "Not a disjunction" [t])

    let disjI i sqnt = simple_sqnt_apply (disjI0 None i) sqnt
    let disjI_info inf i sqnt = 
      simple_sqnt_apply (disjI0 (Some inf) i) sqnt
    let disjI_full inf i g = 
      simple_sqnt_apply (disjI0 inf (dest_fident i (get_sqnt g))) g

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
	do_tag_info inf [] [ft1; ft2] [];
	mk_subgoal 
	  (sqnt_tag sq, sqnt_env sq, asms sq, 
	   cncl1::cncl2::(delete_cncl i (concls sq))))
      else raise (logicError "Not a disjunction" [t])

    let disjE i sqnt = simple_sqnt_apply (disjE0 None i) sqnt
    let disjE_info inf i sqnt = simple_sqnt_apply (disjE0 (Some inf) i) sqnt
    let disjE_full inf i g = 
      simple_sqnt_apply (disjE0 inf (dest_fident i (get_sqnt g))) g

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
	do_tag_info inf [] [ft] [];
	mk_subgoal
	  (sqnt_tag sq, sqnt_env sq, delete_asm i (asms sq), cncl1::(concls sq)))
      else raise (logicError "Not a negation"[t])

    let negA i sqnt = simple_sqnt_apply (negA0 None i) sqnt
    let negA_info inf i sqnt = simple_sqnt_apply (negA0 (Some inf) i) sqnt
    let negA_full inf i g = 
      simple_sqnt_apply (negA0 inf (dest_fident i (get_sqnt g))) g

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
	do_tag_info inf [] [ft] [];
	mk_subgoal
	  (sqnt_tag sq, sqnt_env sq, asm1::(asms sq), delete_cncl i (concls sq)))
      else raise (logicError "Not a negation"[t])

    let negC i sqnt = simple_sqnt_apply (negC0 None i) sqnt
    let negC_info inf i sqnt = simple_sqnt_apply (negC0 (Some inf) i) sqnt
    let negC_full inf i g = 
      simple_sqnt_apply (negC0  inf (dest_fident i (get_sqnt g))) g

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
	do_tag_info inf [] [ft2; ft1] [];
	mk_subgoal
	  (sqnt_tag sq, sqnt_env sq, asm::(asms sq), 
	   (replace_cncl i (concls sq) cncl)))
      else raise (logicError "Not an implication" [t])

    let implI i sqnt = simple_sqnt_apply (implI0 None i) sqnt
    let implI_info inf i sqnt = simple_sqnt_apply (implI0 (Some inf) i) sqnt
    let implI_full inf i g = 
      simple_sqnt_apply (implI0  inf (dest_fident i (get_sqnt g))) g

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
	do_tag_info info [tagl; tagr] [ft] [];
	[mk_sqnt tagl (sqnt_env sq) asm1 cncl1;
	 mk_sqnt tagr (sqnt_env sq) asm2 (concls sq)])
(*
   [(tagl, sqnt_env sq, asm1, cncl1); 
   (tagr, sqnt_env sq, asm2, concls sq)])
 *)
      else raise (logicError "Not an implication" [t])

    let implE i sqnt = simple_sqnt_apply (implE0 None i) sqnt
    let implE_info info i sqnt = simple_sqnt_apply (implE0 (Some info) i) sqnt
    let implE_full info i g = 
      simple_sqnt_apply (implE0 info (dest_fident i (get_sqnt g))) g

(* allI i sq
   asm |- t:!x. P(c), concl
   -->
   asm |- t:P(c'), concl   where c' is a new identifier

   info: [] [t]
 *)


    let allI0 inf i tyenv sq =
      (* get the conclusion and its tag *)
      let (ft, t)=(get_cncl i sq)
      in 
      (* check that it is a universal quantification *)
      if (Formula.is_all t)
      then 
	(* make the skolem constant from the binder name and type *)
	(let (nv, nty)=(Formula.get_binder_name t, Formula.get_binder_type t)
	in 
	let sv, sty, nsklms, styenv, ntynms=
	  mk_new_skolem 
	    {
	     name=(mklong (thy_of_sq sq) nv);
	     ty=nty;
	     tyenv=tyenv;
	     scope=scope_of sq;
	     skolems=sklm_cnsts sq;
	     tylist=sqnt_tynames sq
	   }
	in 
	let nscp = add_sklms_to_scope nsklms (scope_of sq)
	in 
	(* add skolem constant and type variable to sequent list *)
	let nsqtys=
	  if (Gtypes.is_weak sty)
	  then sty::(sqnt_tyvars sq)
	  else (sqnt_tyvars sq)
	in 
	let ncncl, ntyenv = 
	  Formula.inst_env nscp [] styenv t sv
	in 
	(* update the goals' type environment *)
	let gtyenv=Gtypes.extract_bindings nsqtys ntyenv tyenv
	in 
	(* build the subgoal and return information *)
	do_tag_info inf [] [ft] [];
	(mk_subgoal(sqnt_tag sq, 
		    mk_sqnt_env nsklms nscp nsqtys ntynms,
		    asms sq, 
		    replace_cncl i (concls sq) (ft, ncncl)), gtyenv))
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

    let existI0 inf i tyenv sq =
      (* get the assumption and its tag *)
      let (ft, t)=(get_asm i sq)
      in 
      (* check that it is existential quantification *)
      if (Formula.is_exists t)
      then 
	(* make the skolem constant from the binder name and type *)
	(let (nv, nty) = (Formula.get_binder_name t, Formula.get_binder_type t)
	in 
	let sv, sty, nsklms, styenv, ntynms=
	  mk_new_skolem
	    {
	     name=(mklong (thy_of_sq sq) nv);
	     ty=nty;
	     tyenv=tyenv;
	     scope=scope_of sq;
	     skolems=sklm_cnsts sq;
	     tylist=sqnt_tynames sq
	   }
	in 
	let nscp = add_sklms_to_scope nsklms (scope_of sq)
	in 
	(* add skolem constant and type variable to sequent list *)
	let nsqtys=
	  if (Gtypes.is_weak sty)
	  then sty::(sqnt_tyvars sq)
	  else (sqnt_tyvars sq)
	in 
	let nasm, ntyenv= 
	  Formula.inst_env nscp [] styenv t sv
	in 
	(* update the goals' type environment *)
	let gtyenv=Gtypes.extract_bindings nsqtys ntyenv tyenv
	in 
	do_tag_info inf [] [ft] [];
	(mk_subgoal
	   (sqnt_tag sq, 
	    mk_sqnt_env nsklms nscp nsqtys ntynms,
            replace_asm i (asms sq) (ft, nasm), 
	    concls sq)), gtyenv)
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
	(do_tag_info inf [] [] [];
	 raise No_subgoals)
      else 
	raise (logicError "Not trivial" [t])

    let trueR i sqnt = simple_sqnt_apply (trueR0 None i) sqnt
    let trueR_info inf i sqnt = simple_sqnt_apply (trueR0 (Some inf) i) sqnt
    let trueR_full inf i g = 
      simple_sqnt_apply (trueR0 inf (dest_fident i (get_sqnt g))) g

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
	     (Result.add_error(logicError "Beta reduction" [t]) x))
      in 
      do_tag_info inf [] [ft] [];
      if i> 0 
      then mk_subgoal
	  (sqnt_tag sq, sqnt_env sq, (asms sq), replace_cncl i (concls sq) nt)
      else mk_subgoal
	  (sqnt_tag sq, sqnt_env sq, replace_asm i (asms sq) nt, concls sq)

    let beta i sqnt = simple_sqnt_apply (beta0 None i) sqnt
    let beta_info inf i sqnt = simple_sqnt_apply (beta0 (Some inf) i) sqnt

(* name_rule: introduce a new name in the sqnt as a synonym for a term  *)
(* name id trm:
   Asm|-Cncl -> t:id=trm, Asm|-Cncl

   the long name thy.id must be unique (where thy is the current theory name)
   info: [] [t]
 *)

    let name_rule0 inf id trm tyenv sq =
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
	  in 
	  let ntyenv= Typing.typecheck_env scp tyenv trm nty
	  in 
	  let rty=Gtypes.mgu nty ntyenv
	  in 
	  let nscp= Gtypes.extend_scope scp 
	      (fun x-> if x=long_id then rty else scp.Gtypes.typeof_fn x)
	  and ft=Tag.create()
	  in
	  let ntrm = 
	    (ft, Formula.form_of_term nscp
	       (Logicterm.mkequal (Term.mkvar long_id) trm))
	  in 
	  let gtyenv = Gtypes.extract_bindings (sqnt_tyvars sq) ntyenv tyenv
	  in 
	  do_tag_info inf [] [ft] [];
	  (mk_subgoal(sqnt_tag sq, sqnt_env sq,
(*
   {sklms=(sklm_cnsts sq); sqscp=nscp},
 *)
		      ntrm::(asms sq), concls sq), gtyenv)

    let name_rule id trm sqnt = sqnt_apply (name_rule0 None id trm) sqnt
    let name_rule_info inf id trm sqnt = 
      sqnt_apply (name_rule0 (Some inf) id trm) sqnt

    let name_rule_full inf id trm sqnt = 
      sqnt_apply (name_rule0 inf id trm) sqnt


(* instantiation terms *)  

    let is_inst_term sq tyenv trm expty =
      Typing.typecheck_env (scope_of sq) tyenv trm expty

(*
   let inst_term sq tyenv t trm =
   let scp = scope_of sq
   and sklm_scp = add_sklms_to_scope (sklm_cnsts sq) (Gtypes.empty_scope())
   in 
   let ntrm0 = Term.set_names scp trm
   in 
   let ntrm= Typing.set_exact_types sklm_scp ntrm0
   in 
   (Formula.typecheck_env scp 
   (Formula.inst scp t ntrm) 
   (Gtypes.mk_var "inst_ty"))
 *)
    let inst_term sq tyenv t trm =
      let scp = scope_of sq
      and sklm_scp = add_sklms_to_scope (sklm_cnsts sq) (scope_of sq)
      in 
      let ntrm0 = Term.set_names scp trm
      in 
      let ntrm1= Typing.set_exact_types sklm_scp ntrm0
      in 
      let ntrm2, ntyenv2=Formula.inst_env scp [] tyenv t ntrm1
      in 
      let ntyenv3=Formula.typecheck_env scp ntyenv2 ntrm2 
	  (Gtypes.mk_var "inst_ty")
      in ntrm2, ntyenv3

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


    let unify0 inf i j tyenv sq = 
      let scp = scope_of sq
      and (_, asm) = get_asm i sq
      and (_, cncl) = get_cncl j sq
      in 
      let (unify_rslt, gtyenv)=
	(try 
	  let (ntyenv, _) = Formula.unify_env scp tyenv asm cncl
	  in 
	  (true,  Gtypes.extract_bindings (sqnt_tyvars sq) ntyenv tyenv )
	with _ -> (false, tyenv))
      in 
      if(unify_rslt)
      then (do_tag_info inf [] [] []; raise (Solved_subgoal gtyenv))
      else 
	if(Formula.alpha_convp scp asm cncl)
	then 
	  (do_tag_info inf [] [] []; raise (Solved_subgoal tyenv))
	else 
	  (raise (logicError "Can't unify assumption with conclusion"
		    [sqnt_form (get_asm i sq); 
		     sqnt_form (get_cncl j sq)]))

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

    let existE0 inf trm i tyenv sq =
      let (ft, t)=(get_cncl i sq)
      in 
      if (Formula.is_exists t) 
      then 
	try 
      	  (let tyenv1 = is_inst_term sq tyenv trm (Formula.get_binder_type t)
      	  in 
      	  let nt, ntyenv = inst_term sq tyenv1 t trm
      	  in 
	  let gtyenv=Gtypes.extract_bindings (sqnt_tyvars sq) ntyenv tyenv
	  in 
	  do_tag_info inf [] [ft] [];
      	  (mk_subgoal
	     (sqnt_tag sq, sqnt_env sq, asms sq, 
	      (replace_cncl i (concls sq) (ft, nt))),
	   gtyenv))
	with x -> raise (Result.add_error
			   (logicError "existE:" [t]) x)
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

    let allE0 inf trm i tyenv sq =
      let (ft, t)=(get_asm i sq)
      in 
      if (Formula.is_all t) 
      then 
	try 
	  (let tenv = is_inst_term sq tyenv trm (Formula.get_binder_type t)
	  in 
	  let nt, ntyenv = inst_term sq tenv t trm
	  in 
	  let gtyenv=Gtypes.extract_bindings (sqnt_tyvars sq) ntyenv tyenv
	  in 
	  do_tag_info inf [] [ft] [];
	  (mk_subgoal
	     (sqnt_tag sq, sqnt_env sq, 
	      (replace_asm i (asms sq) (ft, nt)), concls sq)),
	  gtyenv)
	with x -> 
	  (raise (Result.add_error
		    (logicError "allE: " [t]) x))
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

(*
   let rewrite_any0 inf dir simple rls j tyenv sq=
   let scp = scope_of sq
   in 
   let r=filter_rules scp rls j sq
   and (ft, t)=if j>=0 then (get_cncl j sq)  else (get_asm j sq)
   in 
   try
   (let nt = 
   if simple 
   then 
   (ft, Formula.rewrite_simple ~dir:dir scp r t)
   else 
   (ft, Formula.rewrite ~dir:dir scp r t)
   in 
   do_tag_info inf [] [ft] [];
   if j>=0 then
   mk_subgoal
   (sqnt_tag sq, env sq, asms sq, replace_cncl j (concls sq) nt)
   else 
   mk_subgoal
   (sqnt_tag sq, env sq, replace_asm j (asms sq) nt, concls sq))
   with x -> raise 
   (Result.add_error (mklogicError"rewriting" (t::r)) x)
 *)
    let rewrite_any0 inf dir simple rls j tyenv sq=
      let scp = scope_of sq
      in 
      let r=filter_rules scp rls j sq
      and (ft, t)=if j>=0 then (get_cncl j sq)  else (get_asm j sq)
      in 
      try
	(let nt, ntyenv = 
	  if simple 
	  then 
	    Formula.rewrite_simple_env scp ~dir:dir tyenv r t
	  else 
	    Formula.rewrite_env scp ~dir:dir tyenv r t
	in 
	let gtyenv = Gtypes.extract_bindings (sqnt_tyvars sq) ntyenv tyenv
	in 
	do_tag_info inf [] [ft] [];
	if j>=0 then
	  (mk_subgoal
	     (sqnt_tag sq, sqnt_env sq, asms sq, 
	      replace_cncl j (concls sq) (ft, nt)), gtyenv)
	else 
	  (mk_subgoal
	     (sqnt_tag sq, sqnt_env sq, 
	      replace_asm j (asms sq) (ft, nt), concls sq), gtyenv))
      with x -> raise 
	  (Result.add_error (logicError"rewriting" (t::r)) x)

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
      and scp = scope_of sq
      in 
      try
	(let nt = (ft, Formula.rewrite ~dir:dir scp r t)
	in 
	do_tag_info inf [] [ft] [];
	if j>=0 then
	  mk_subgoal
	    (sqnt_tag sq, sqnt_env sq, asms sq, replace_cncl j (concls sq) nt)
	else 
	  mk_subgoal
	    (sqnt_tag sq, sqnt_env sq, replace_asm j (asms sq) nt, concls sq))
      with x -> raise 
	  (Result.add_error (logicError"rewriting" (t::r)) x)


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
      let scp = scope_of sq
      in 
      let r=filter_thms scp thms
      and (ft, t)=if j>=0 then (get_cncl j sq)  else (get_asm j sq)
      in 
      try
	(let nt = (ft, Formula.rewrite ~dir:dir scp r t)
	in 
	do_tag_info inf [] [ft] [];
	if j>=0 then
	  mk_subgoal
	    (sqnt_tag sq, sqnt_env sq, asms sq, replace_cncl j (concls sq) nt)
	else 
	  mk_subgoal
	    (sqnt_tag sq, sqnt_env sq, replace_asm j (asms sq) nt, concls sq))
      with x -> raise 
	  (Result.add_error (logicError"rewriting" (t::r)) x)

(*
   let rewrite_thms ?(dir=true) thms j sqnt
   = sqnt_apply (rewrite_thms0 dir thms j) sqnt
 *)

    let rewrite_thms ?(dir=true) thms j sqnt
	= rewrite_any ~dir:dir (List.map (fun x-> RRThm(x)) thms) j sqnt

    let rewrite_thms_info inf ?(dir=true) thms j sqnt=
      rewrite_any_info inf ~dir:dir 
	(List.map (fun x-> RRThm(x)) thms) j sqnt

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
	  (Result.add_error 
	     (logicError "Not a conjunction" [f]) x))

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
	  let nf = Formula.inst scp [] f trm
	  in 
	  [mk_same_thm t nf])
	with x -> 
	  raise (Result.add_error
		   (logicError "allE_conv:" [f]) x)
      else raise (logicError "allE_conv:" [f])

    let beta_conv scp ts =
      let t = get_one_thm ts
      in 
      try 
	let f = dest_thm t
	in let nt = (Formula.beta_conv scp f)
	in [mk_same_thm t nt]
      with x -> raise 
	  (Result.add_error (logicError "beta_conv" []) x)

    let eta_conv scp x ts =
      let t = get_one_thm ts
      in 
      try 
	let f = (dest_thm t)
	in let nt = (Formula.eta_conv scp x 
		       (Typing.typeof scp (Formula.term_of_form x)) f)
	in [mk_same_thm t nt]
      with e -> 
	raise (Result.add_error (logicError "eta_conv" []) e)

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
	    (Result.add_error(logicError "rewrite_conv" [dest_thm t]) x)
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
   and scp = scope_of sq
   in 
   try
   (let nt = (Formula.rewrite_net scp (net_of_db rnet) t)
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
	    then raise 
		(Result.error 
		("Identifier "^x^" appears twice in argument list"))
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
