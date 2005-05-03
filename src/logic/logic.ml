(*-----
Name: logic.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

open Basic
open Formula

type thm = 
    Axiom of form
  | Theorem of form

type saved_thm = 
    Saxiom of saved_form
  | Stheorem of saved_form

let mk_axiom t = Axiom t
let is_axiom t = match t with (Axiom _) -> true | _ -> false

let mk_theorem t = Theorem t
let is_thm t = match t with (Theorem _) -> true | _ -> false
let dest_thm x = 
  match x with 
    Axiom f -> f
  | Theorem f -> f

let formula_of = dest_thm

let term_of x = 
  match x with 
    Axiom f -> Formula.term_of f
  | Theorem f -> Formula.term_of f


let to_save t = 
  match t with 
    Axiom f -> Saxiom (Formula.to_save f)
  | Theorem f -> Stheorem (Formula.to_save f)

let from_save t = 
  match t with 
    Saxiom f -> Axiom (Formula.from_save f)
  | Stheorem f -> Theorem (Formula.from_save f)

let string_thm x = string_form (formula_of x)

(* Error handling *)

(*
   let mk_logic_error s t = 
   (Term.term_error s (List.map Formula.term_of t))
 *)
let logic_error s t = Term.term_error s (List.map Formula.term_of t)
let add_logic_error s t es = 
  raise (Result.add_error (logic_error s t) es)

(* Skolem constants *)
module Skolem = 
  struct

    type skolem_cnst = (Basic.ident * (int * Basic.gtype))
(*
   type skolem_cnst = ((Basic.ident * Basic.gtype) * int)
 *)
    type skolem_type = skolem_cnst list

    let make_sklm x ty i = (x, (i, ty))

    let get_sklm_name (x, (_, _)) = x
    let get_sklm_indx (_, (i, _)) = i
    let get_sklm_type (_, (_, t)) = t

    let decln_of_sklm x= (get_sklm_name x, get_sklm_type x)
(*
   let get_sklm_name ((x, _), _) = x
   let get_sklm_indx ((_, _),i) = i
   let get_sklm_type ((_, t), _) = t
 *)

    let get_old_sklm n sklms =  (n, List.assoc n sklms)

    let is_sklm n sklms = 
      try ignore(get_old_sklm n sklms); true
      with Not_found -> false

    let make_skolem_name id indx = 
      mk_long (thy_of_id id) ((name id)^"_"^(string_of_int indx))

    let get_new_sklm n t sklms = 
      try 
	(let oldsk = get_old_sklm n sklms
	in let nindx = ((get_sklm_indx oldsk)+1)
	in let nnam = make_skolem_name n nindx
(*
	  mk_long (thy_of_id n) ((name n)^"_"^(string_of_int nindx))
*)
	in (Term.mk_typed_var nnam t, (make_sklm nnam t nindx)::sklms))
      with Not_found -> 
	let nn =  (mk_long (thy_of_id n) ((name n)^"_"^(string_of_int 1)))
	in 
	((Term.mk_typed_var nn t), (nn, (1, t))::sklms)

    let add_skolems_to_scope sklms scp =
      let declns = List.map decln_of_sklm sklms
      in 
      Scope.extend_with_terms scp declns

(*

   let add_skolems_to_scope sklms scp =
   { scp with
   Gtypes.typeof_fn = 
   (fun x ->
   (let y =
   (if (thy_of_id x)=null_thy
   then mk_long scp.Gtypes.curr_thy (name x)
   else x)
   in 
   try get_sklm_type (get_old_sklm y sklms)
   with Not_found -> scp.Gtypes.typeof_fn x));
   Gtypes.thy_of=
   (fun sel x-> 
   if(sel = Basic.fn_id)
   then 
   try 
   ignore(List.assoc (Basic.mk_long scp.Gtypes.curr_thy x) sklms);
   scp.Gtypes.curr_thy
   with Not_found -> scp.Gtypes.thy_of sel x
   else scp.Gtypes.thy_of sel x)
   }
 *)	
    let add_skolem_to_scope sv sty scp =
      Scope.extend_with_terms scp [(Term.get_var_id sv, sty)] 
(*
   let add_skolem_to_scope sv sty scp =
   let svname=Basic.name (Term.get_var_id sv)
   in 
   { scp with
   Gtypes.typeof_fn = 
   (fun x ->
   let id_thy = thy_of_id x
   in 
   if (id_thy=null_thy) or (id_thy=scp.Gtypes.curr_thy)
   then 
   (if (name x) = svname
   then sty
   else scp.Gtypes.typeof_fn x)
   else scp.Gtypes.typeof_fn x);
   Gtypes.thy_of=
   (fun sel x-> 
   if(sel = Basic.fn_id) & x = svname
   then scp.Gtypes.curr_thy
   else scp.Gtypes.thy_of sel x)
   }
 *)

(** [mk_new_skolem scp n ty]

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
	 scope: Scope.t;
	 skolems: skolem_type;
	 tylist: (string*int) list
       }

(**
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
(*
   let nm_s = (n^(Lib.int_to_name nm_int))
 *)
      let nm_s = (Lib.int_to_name nm_int)
      in 
      (nm_s, nnames)

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
	  make_skolem_name info.name nindx
(*
	  mk_long 
	    (thy_of_id info.name) 
	    ((name info.name)^"_"^(string_of_int nindx))
*)
	in 
	let nty, ntyenv, new_names=mk_nty (name nnam)
	in 
	(Term.mk_typed_var nnam nty, nty, (nnam, (nindx, nty))::info.skolems, 
	 ntyenv, new_names)
      with Not_found -> 
	let nnam=
	  make_skolem_name info.name 1
(*
	  mk_long 
	    (thy_of_id info.name) ((name info.name)^"_"^(string_of_int 1))
*)
	in 
	let nty, ntyenv, new_names=mk_nty (name nnam)
	in 
	(Term.mk_typed_var nnam nty, nty, (nnam, (1, nty))::info.skolems, 
	 ntyenv, new_names)
  end


type tagged_form = (Tag.t* form)

let form_tag (t, _) = t
let drop_tag (_, f) = f

let sqntError s = 
  Result.mk_error(new Result.error s)
let addsqntError s es = 
  raise (Result.add_error (sqntError s) es)

(* label: sequent formula identifiers *)

type label = 
    FNum of int
  | FTag of Tag.t

(*
   rr_type: where to get rewrite rule from
   Asm : labelled assumption
   RRThm: given theorem
   OAsm : labelled assumption, with ordering
   ORRThm: given theorem, with ordering
 *)
type rr_type = 
    Asm of label
  | RRThm of thm
  | OAsm of label * Rewrite.order
  | ORRThm of thm * Rewrite.order

module Sequent=
  struct
(* 
   A sequent is made up of
   a unique tag
   information about skolem constants (the sqnt_env)
   a list of tagged formulas: the assumptions
   a list of tagged formulas: the conclusions
 *)

(* mk_sqnt x: |- x  (with x to be proved)*) 
    let mk_sqnt_form f = (Tag.create(), f)

(* 
   A sqnt_env is made up of
   the shared type variables (Gtypes.WeakVar) that may be used in the sequent
   information for constructing names of weak types
   the skolem constants that may be used in the sequent
   the scope of the sequent
 *)
    type sqnt_env = 
	{
	 sklms: Skolem.skolem_type; 
	 sqscp : Scope.t;
	 tyvars: Basic.gtype list;
	 tynames: (string * int) list;
       }

    type t = (Tag.t * sqnt_env * tagged_form list * tagged_form list)

(* Sequent manipulation *)

    let asms (_, _, asl, _) = asl
    let concls (_, _, _, cnl) = cnl
    let sqnt_env (_, e, _, _) = e
    let sklm_cnsts (_, e, _, _) = e.sklms
    let scope_of (_, e, _, _) = e.sqscp
    let sqnt_tyvars (_, e, _, _)=e.tyvars
    let sqnt_tynames (_, e, _, _)=e.tynames
    let sqnt_tag(t, _, _, _) = t

    let mk_sqnt tg env ps cs= (tg, env, ps, cs)
    let dest_sqnt (tg, env, ps, cs) = (tg, env, ps, cs)

    let mk_sqnt_env sks scp tyvs names=
      {sklms=sks; sqscp=scp; tyvars=tyvs; tynames=names}

    let new_sqnt scp x = 
      let env=mk_sqnt_env [] scp [] []
      in 
      mk_sqnt (Tag.create()) env [] [mk_sqnt_form x]

    let sqnt_scope sq = scope_of sq
    let thy_of_sqnt sq = Scope.thy_of (scope_of sq)


(* Accessing and manipulating formulas in a sequent *)

(* 
   New method: 
   split a list x into (l, c, r) so that x = (rev_append l (c::r)).
   element c is identified by either an integer or a predicate (for tags).
 *)

(**
   [split_at_index i x]: Split [x] into [(l, c, r)] so that
   [x=List.revappend x (c::r)] and [c] is the [i]th element of [x]
   (counting from [0]).

   @raise Not_found if [i] >= [length x].
 *)
    let split_at_index i x=
      let rec split_aux ctr l rst =
	match l with 
	  [] -> raise Not_found
	| (y::ys) -> 
	    if (ctr=0) then (rst, y, ys)
	    else split_aux (ctr-1) ys (y::rst)
      in 
      split_aux (abs i) x []

(**
   [split_at p x]:
   Split [x] into [(l, c, r)] so that [x=List.revappend x (c::r)]
   and [c] is the first element of [x] such that [p x] is true.

   @raise Not_found if [p] is false for all elements of x.
 *)
    let split_at p x=
      let rec split_aux l rst =
	match l with 
	  [] -> raise Not_found
	| (y::ys) -> 
	    if (p y) then (rst, y, ys)
	    else split_aux ys (y::rst)
      in split_aux  x []
	
(**
   [split_at_tag t x]:
   Split [x] into [(l, c, r)] so that [x=List.rev_append x (c::r)]
   and [c] is the formula in [x] identified by tag [t].
 *)
    let split_at_tag t x= 
      let test (l, _) = Tag.equal t l
      in split_at test x

(**
   [split_at_label lbl x]:
   Split [x] into [(l, c, r)] so that [x=List.rev_append x (c::r)]
   and [c] is the formula in [x] identified by label [lbl].

   if [lbl=FNum i], then split at index [(abs i)-1].
   (to deal with assumptions and the index offset).
 *)
    let split_at_label lbl x= 
      match lbl with
	FNum i -> split_at_index ((abs i)-1) x
      | FTag tg -> split_at_tag tg x

(*
   [split_at_asm lbl x]:
   [split_at_concl lbl x]:

   Split [x] into [(l, c, r)] so that [x=List.rev_append x (c::r)]
   and [c] is the formula in [x] identified by label [lbl].

   [split_at_asm lbl x]:
   raise Not_found if [lbl=FNum i] and i>=0

   [split_at_concl lbl x]:
   raise Not_found if [lbl=FNum i] and i<0
 *)
    let split_at_asm l fs = 
      match l with
	FNum i -> 
	  if i>=0 then raise Not_found 
	  else split_at_label l fs
      | _ -> split_at_label l fs

    let split_at_concl l fs = 
      match l with
	FNum i -> 
	  if i<0 then raise Not_found 
	  else split_at_label l fs
      | _ -> split_at_label l fs

(* Old method (still in use): search through lists of formulas *)
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

    let get_tagged_asm t sq = 
      let rec get_aux ams = 
	match ams with
	  [] -> raise Not_found
	| (xt, xf)::xs -> 
	    if Tag.equal xt t then (xt, rename xf)
	    else get_aux xs
      in 
      get_aux (asms sq)

    let get_tagged_cncl t sq = 
      let rec get_aux ccs = 
	match ccs with
	  [] -> raise Not_found
	| (xt, xf)::xs -> 
	    if Tag.equal xt t then (xt, rename xf)
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
	    if Tag.equal (form_tag x) t then i
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
	    if i=1 then (form_tag x)
	    else index_aux xs (i-1)
      in 
      if(i<0) 
      then index_aux (asms sq)  (-i)
      else index_aux (concls sq) i


  end


(* 
   A goal is made up of a list of sequents: 

   - The sub-goals still to be proved 
   - A type environment: the bindings of the shared type
   variables which occur in the goals sequents (all of these are weak
   type variables). 
   - A formula: the theorem which is to be proved
 *)
type goal =  Goal of (Sequent.t list * Gtypes.substitution * form)
(*
   type conv = thm list -> thm list 
 *)

let get_label_asm t sq = 
  match t with
    FTag x -> Sequent.get_tagged_asm x sq
  | FNum x -> Sequent.get_asm x sq

let get_label_cncl t sq = 
  match t with
    FTag x -> Sequent.get_tagged_cncl x sq
  | FNum x -> Sequent.get_cncl x sq

let get_label_form t sq=
  try 
    get_label_asm t sq
  with Not_found -> get_label_cncl t sq

let dest_label f sq=
  match f with
    FNum(x) -> x
  | FTag(x) -> Sequent.tag_to_index x sq

let label_to_tag f sq=
  match f with
    FNum(x) -> Sequent.index_to_tag x sq
  | FTag(x) -> x

let label_to_index f sq=
  match f with
    FNum(x) -> x
  | FTag(x) -> Sequent.tag_to_index x sq


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
let get_goal (Goal(_, _, f)) = f
let get_subgoals (Goal(sq, tyenv, _)) = sq
let get_subgoal_tags (Goal(sqs, _, _)) = List.map Sequent.sqnt_tag sqs

let goal_has_subgoals g =
  match g with 
    (Goal([], _, _)) -> false
  | (Goal(_, _, _)) -> true

let num_of_subgoals x = 
  match x with
    (Goal(sqs, _, _)) -> List.length sqs

let get_nth_subgoal_sqnt i (Goal(sq, _, _)) =
  try (List.nth sq i)
  with _ -> 
    raise Not_found (*(sqntError "get_nth_subgoal: invalid argument")*)

let get_goal_tag g =
  match g with
    (Goal(x::_, _, _))-> Sequent.sqnt_tag x
  | _ -> raise Not_found

let mk_goal scp f = 
  let nf= Formula.typecheck scp f (Gtypes.mk_bool)
  in 
  Goal([Sequent.new_sqnt scp nf], Gtypes.empty_subst(), nf)

let goal_focus t (Goal(sqnts, tyenv, f)) =
  let rec focus sqs rslt=
    match sqs with
      [] -> raise Not_found
    | (x::xs) -> 
	if Tag.equal t (Sequent.sqnt_tag x) 
	then (x::((List.rev rslt)@xs))
	else focus xs (x::rslt)
  in Goal(focus sqnts [], tyenv, f)

let rotate_subgoals_left n (Goal(sqnts, tyenv, f)) =
  if goal_has_subgoals (Goal(sqnts, tyenv, f)) 
  then 
    Goal(Lib.rotate_left n sqnts, tyenv, f)
  else raise No_subgoals

let rotate_subgoals_right n (Goal(sqnts, tyenv, f)) =
  if goal_has_subgoals (Goal(sqnts, tyenv, f)) 
  then 
    Goal(Lib.rotate_right n sqnts, tyenv, f)
  else raise No_subgoals

let apply_nth r i g = 
  match g with 
    Goal(sgs, tyenv, f) ->
      let nsgs = r (try (List.nth sgs i) with _ -> raise Not_found)
      in Goal(Lib.splice_nth i sgs [nsgs], tyenv, f)

let mk_thm g = 
  match g with 
    Goal([], _, f) -> Theorem f
  | _ -> raise (logic_error "Not a theorem" [])

let print_thm pp t = 
  Format.printf "@[<3>|- ";
  Term.print pp (term_of t);
  Format.printf "@]"

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
type info = tag_record ref

let make_tag_record gs fs ts = {goals=gs; forms=fs; terms=ts}
let add_to_record r gs fs ts =
  make_tag_record (gs@r.goals) (fs@r.forms) (ts@r.terms)

let do_info info gs fs ts=
  match info with
    None -> ()
  | Some(v) -> v:=make_tag_record gs fs ts

let add_info info gs fs ts=
  match info with
    None -> ()
  | Some(v) -> v:=add_to_record (!v) gs fs ts


module Subgoals=
  struct
(*
   Subgoals:
   managing sequents used as subgoals
 *)
    type node = Node of (Tag.t * Gtypes.substitution * Sequent.t)
    let node_tag (Node(tg, _, _)) = tg
    let node_tyenv (Node(_, ty, _)) = ty
    let node_sqnt (Node(_, _, s)) = s
    let mk_node tg ty s = Node(tg, ty, s)

    type branch= Branch of (Tag.t * Gtypes.substitution * Sequent.t list)
    let branch_tag (Branch(tg, _, _)) = tg
    let branch_tyenv (Branch(_, ty, _)) = ty
    let branch_sqnts (Branch(_, _, s)) = s
    let mk_branch tg ty gs = Branch(tg, ty, gs)

    let replace_branch_tag b tg =
      mk_branch tg (branch_tyenv b) (branch_sqnts b) 

(* 
   [branch_node node]
   make a branch from [node]
 *)
    let branch_node (Node(tg, tyenv, sqnt))=
      Branch(tg, tyenv, [sqnt])

(*
   [merge env1 env2]: merge type environments.

   Create a [env3] which has the binding of each weak variable in
   [env1 + env2].

   raise [Failure] if a variable ends up bound to itself.
 *)
    let merge_tyenvs env1 env2 = 
      let env3 = ref env1
      in 
      let assign x z =
	try
	  (let y = Gtypes.lookup_var x env2
	  in 
	  if (Gtypes.equals x y)
	  then raise (Failure "Can't merge type environments")
	  else env3:=Gtypes.bind x y (!env3))
	with Not_found -> ()
      in 
      Gtypes.subst_iter assign env1; !env3


(* [merge_tac_tyenvs n b]:
   Merge the type environment of [b], resulting from applying a tactic
   to [n], with the type environment of [n].  Make a new branch with
   the components of [b] but with the new type environment.
 *)
    let merge_tac_tyenvs n b =
      let ntyenv = node_tyenv n
      in 
      let btag = branch_tag b
      and btyenv = branch_tyenv b
      and bsqnts = branch_sqnts b
      in 
      let nbtyenv = 
	try merge_tyenvs btyenv ntyenv
	with _ -> 
	  raise (logic_error "Subgoal.apply: Invalid result from tactic" [])
      in 
      mk_branch btag nbtyenv bsqnts


(*
   [apply tac node]: Apply tactic [tac] to [node].

   This is the work-horse for applying tactics. All other functions
   should call this to apply a tactic.

   Approach:
   1. Create a new tag [ticket].
   3. Apply tac to [node] getting branch [b'].
   4. If the tag of [b'] is not [ticket] then fail.

   5. Merge the type environment of [b'] with [n']. (This may be
   unnecessary.)
   (Almost certainly unnecessary so not done.)

   6. Return the branch formed from [b'] with the tag of [node].

   raise logic_error on failure.
 *)
    let apply tac node = 
      let ticket = Tag.create()
      in 
      let n1 = mk_node ticket (node_tyenv node) (node_sqnt node)
      in 
      let b1 = tac n1
      in 
      if not (Tag.equal ticket (branch_tag b1))
      then raise (logic_error "Subgoal.apply: Invalid result from tactic" [])
      else 
	replace_branch_tag b1 (node_tag n1)
(*
   let b2 = merge_tac_tyenvs n1 b1
   in 
   replace_branch_tag b2 (node_tag n1)
 *)
	  
(*
   [apply_to_node tac (Node(tyenv, sqnt))]

   Apply tactic [tac] to node, getting [result].
   If tag of result is the same as the tag of sqnt,
   then return result.
   Otherwise raise logic_error.
 *)
    let apply_report report node branch=
      match report with
	None -> ()
      | (Some f) -> f node branch

    let apply_to_node ?report tac node = 
      let result = apply tac node
      in 
      apply_report report node result;
      result


(*
   [apply_to_first tac (Branch(tg, tyenv, sqnts))]
   Apply tactic [tac] to firsg sequent of [sqnts] 
   using [apply_to_node].
   replace original sequent with resulting branches.
   return branch with tag [tg].

   raise No_subgoals if [sqnts] is empty.
 *)
    let apply_to_first ?report tac (Branch(tg, tyenv, sqnts))=
      match sqnts with 
	[] -> raise No_subgoals
      | (x::xs) ->
	  let branch1=
	    apply_to_node ?report:report tac 
	      (mk_node (Sequent.sqnt_tag x) tyenv x) 
	  in 
	  mk_branch tg 
	    (branch_tyenv branch1)
	    ((branch_sqnts branch1)@xs)

(*
   [apply_to_each tac (Branch(tg, tyenv, sqnts))]
   Apply tactic [tac] to each sequent in [sqnts] 
   using [apply_to_node].
   replace original sequents with resulting branches.
   return branch with tag [tg].

   raise No_subgoals if [sqnts] is empty.
 *)
    let apply_to_each tac (Branch(tg, tyenv, sqnts))=
      let rec app_aux ty gs lst=
	match gs with
	  [] -> mk_branch tg ty (List.rev lst)
	| (x::xs) ->
	    let branch1=apply_to_node tac 
		(mk_node (Sequent.sqnt_tag x) ty x)
	    in
	    app_aux (branch_tyenv branch1) xs 
	      (List.rev_append (branch_sqnts branch1) lst)
      in 
      match sqnts with
	[] -> raise No_subgoals
      | _ -> app_aux tyenv sqnts []

(*
   [apply_to_goal tac goal]
   Apply tactic [tac] to firat subgoal of [goal] using [apply_to_first].
   Replace original list of subgoals with resulting subgoals.

   raise logic_error "Invalid Tactic" 
   if tag of result doesn't match tag originaly assigned to it.

   raises [No_subgoals] if goal is solved.
 *)
    let apply_to_goal ?report tac (Goal(sqnts, tyenv, f))=
      let g_tag = Tag.create()
      in
      let branch=mk_branch g_tag tyenv sqnts
      in 
      let result=apply_to_first ?report:report tac branch 
      in 
      if Tag.equal g_tag (branch_tag result)
      then 
	let ntyenv = (branch_tyenv result)
	in 
	Goal(branch_sqnts result, ntyenv, f)
      else raise 
	  (logic_error "Subgoal.apply_to_goal: Invalid tactic" [])

(* 
   [zip tacl branch]
   apply each of the tactics in [tacl] to the corresponding 
   subgoal in branch.
   e.g. [zip [t1;t2;..;tn] (Branch [g1;g2; ..; gm])
   is Branch([t1 g1; t2 g2; .. ;tn gn]) 
   (with [t1 g1] first and [tn gn] last)
   if n<m then untreated subgoals are attached to the end of the new
   branch.
   if m<n then unused tactic are silently discarded.
   typenv of new branch is that produced by the last tactic 
   ([tn gn] in the example).
   tag of the branch is the tag of the original branch.
 *)
    let zip tacl (Branch(tg, tyenv, sqnts))=
      let rec zip_aux ty tacs subgs lst=
	match (tacs, subgs) with
	  (_, []) -> mk_branch tg ty (List.rev lst)
	| ([], gs) -> mk_branch tg ty (List.rev_append lst gs)
	| ((tac::ts), (g::gs)) ->
	    let branch1=apply_to_node tac 
		(mk_node (Sequent.sqnt_tag g) ty g)
	    in
	    zip_aux (branch_tyenv branch1) ts gs 
	      (List.rev_append (branch_sqnts branch1) lst)
      in 
      match sqnts with
	[] -> raise No_subgoals
      | _ -> zip_aux tyenv tacl sqnts []

(*
   [seq tac1 tac2 node]
   apply tactic [tac1] to [node] then [tac2] 
   to each of the resulting subgoals.

   if [tac1] solves the goal (no subgoals), then [tac2] is not used.
 *)
    let seq tac1 tac2 node=
      let branch = apply_to_node tac1 node
      in 
      match (branch_sqnts branch) with
	[] -> branch
      | _ -> apply_to_each tac2 branch

(*
   [rule_apply f g]
   Apply function [f] to sequent [sg] and type environment of node [g]
   to get [(ng, tyenv)]. 
   [ng] is the list of sequents produced by [f] from [sg].
   [tyenv] is the new type environment for the goal.   

   [f] must have type 
   [Gtypes.substitution -> Sequent.t 
   -> (Gtypes.substitution * Sequent.t list)]

   Resulting branch has the same tag as [sg].

   THIS FUNCTION MUST REMAIN PRIVATE TO MODULE LOGIC
 *)
    let rule_apply r (Node(ntag, tyenv, sqnt)) =
      try
	(let rg, rtyenv = r tyenv sqnt
	in 
	Branch(ntag, rtyenv, rg))
      with 
	No_subgoals -> Branch(ntag, tyenv, [])
      | Solved_subgoal ntyenv -> Branch(ntag, ntyenv, [])


(* [simple_rule_apply f g]:
   apply function (f: sqnt -> sqnt list) to the first subgoal of g
   Like sqnt_apply but does not change [tyenv] of [g].
   Used for rules which do not alter the type environment.
 *)
    let simple_rule_apply r node =
      rule_apply (fun tyenv sq -> (r sq, tyenv)) node
  end

type node = Subgoals.node
type branch = Subgoals.branch
type rule = Subgoals.node -> Subgoals.branch
type conv = Scope.t -> Basic.term -> thm

let postpone g = 
  match g with
    Goal (sq::[], _, _) -> raise (sqntError "postpone: No other subgoals")
  | Goal (sg::sgs, tyenv, f) -> 
      Goal (List.concat [sgs;[sg]], tyenv, f)
  | _ -> raise (sqntError "postpone: No subgoals")

let foreach rule branch=
  Subgoals.apply_to_each rule branch

let first_only rule branch=
  Subgoals.apply_to_first rule branch

module Tactics = 
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

(* Utility functions *)

    let get_sqnt = Subgoals.node_sqnt

(*
   [sqnt_apply f g]:
   apply function f to the first subgoal of g

   [simple_sqnt_apply f g]:
   apply function (f: sqnt -> sqnt list) to the first subgoal of g
   Like sqnt_apply but does not change [tyenv] of [g].
   Used for rules which do not alter the type environment.
 *)
    let sqnt_apply r g = Subgoals.rule_apply r g
    let simple_sqnt_apply r g = Subgoals.simple_rule_apply r g

    let mk_subgoal sq = [sq]

(* Lifting assumption/conclusion formulas *)

    let split_at_label l fs = Sequent.split_at_label l fs
    let split_at_asm l fs = Sequent.split_at_asm l fs
    let split_at_concl l fs = Sequent.split_at_concl l fs
    let join_up l r = List.rev_append l r

    let lift_tagged id fs =
      let (lhs, c, rhs) = split_at_label id fs
      in 
      let (t, f) = c
      in
      (t, c::join_up lhs rhs)

    let lift_asm_sq info l sq = 
      let (t, nasms) = lift_tagged l (Sequent.asms sq)
      in 
      add_info info [] [t] [];
      [Sequent.mk_sqnt 
	 (Sequent.sqnt_tag sq) (Sequent.sqnt_env sq) 
	 nasms (Sequent.concls sq)]
	

    let lift_asm info f g =
      simple_sqnt_apply (lift_asm_sq info f) g

    let lift_concl_sq info f sq = 
      let (t, nconcls) = lift_tagged f (Sequent.concls sq)
      in 
      add_info info [] [t] [];
      [Sequent.mk_sqnt (Sequent.sqnt_tag sq) 
	 (Sequent.sqnt_env sq) (Sequent.asms sq)  
	 nconcls]

    let lift_concl info f g =
      simple_sqnt_apply (lift_concl_sq info f) g

    let lift info f g =
      try 
	lift_asm info f g
      with Not_found -> lift_concl info f g

(* copy_asm i: 
   .., t:Ai, ..|- C
   ->
   .., t':Ai, t:Ai, .. |- C
   info: [] [t']
 *)
    let copy_asm0 info l sq = 
      let (lasms, na, rasms) = split_at_asm l (Sequent.asms sq)
      and nt = Tag.create()
      in
      let nb = (nt, drop_tag na)
      in 
      add_info info [] [nt] [];
      mk_subgoal (Sequent.sqnt_tag sq, Sequent.sqnt_env sq,
		  join_up lasms (nb::na::rasms),
		  Sequent.concls sq)

    let copy_asm info i g = 
      let sq=get_sqnt g
      in 
      simple_sqnt_apply (copy_asm0 info i) g

(* copy_cncl i: 
   A|- .., t:Ci, ..
   ->
   A|- .., t':Ci, t:Ci, ..
   info: [] [t']
 *)
    let copy_cncl0 info l sq=
      let (lcncls, nc, rcncls) = split_at_concl l (Sequent.concls sq)
      and nt=Tag.create()
      in 
      let nb = (nt, drop_tag nc)
      in 
      add_info info [] [nt] [];
      mk_subgoal(Sequent.sqnt_tag sq, Sequent.sqnt_env sq,
		 Sequent.asms sq,
		 join_up lcncls (nb::nc::rcncls))

    let copy_cncl inf i g = 
      let sq=get_sqnt g
      in 
      simple_sqnt_apply (copy_cncl0 inf i) g


(* rotate asms/cncls:
   info: [] []
 *)
    let rotate_asms0 info sq = 
      let hs = Sequent.asms sq 
      in
      add_info info [] [] [];
      match hs with 
	[] -> mk_subgoal(Sequent.sqnt_tag sq, Sequent.sqnt_env sq, 
			 hs, Sequent.concls sq)
      | h::hys -> mk_subgoal(Sequent.sqnt_tag sq, 
			     Sequent.sqnt_env sq, hys@[h], 
			     Sequent.concls sq)

    let rotate_asms info sqnt = 
      simple_sqnt_apply (rotate_asms0 info) sqnt

    let rotate_cncls0 inf sq =
      let cs = Sequent.concls sq in
      add_info inf [] [] [];
      match cs with 
	[] -> mk_subgoal(Sequent.sqnt_tag sq, 
			 Sequent.sqnt_env sq, Sequent.asms sq, cs)
      | c::cns -> mk_subgoal(Sequent.sqnt_tag sq, 
			     Sequent.sqnt_env sq, Sequent.asms sq, cns@[c])

    let rotate_cncls inf sqnt = 
      simple_sqnt_apply (rotate_cncls0 inf) sqnt

(* get_pair: auxiliary function *)

    let get_pair x = 
      match x with
	[t1; t2] -> (t1, t2)
      | _ -> raise (logic_error "get_pair" x)

    let get_one x = 
      match x with
	[t1] -> t1
      | _ -> raise (logic_error "get_one" x)

(* [set_names scp trm]
   Try to match free variables in term [trm] with identifiers 
   defined in the scope of sequent [sq]
   Return the term with the resolved names.
 *)
(*
   let set_names scp trm = Term.set_names scp trm
 *)

(*
   [check_term sq trm]
   Check that term [trm] is in the scope [scope].
 *)
    let check_term_memo memo scp frm=
      (if (Formula.in_scope_memo memo scp (Scope.thy_of scp) frm)
      then ()
      else (raise (logic_error "Badly formed formula" [frm])))

    let check_term scp frm=
      check_term_memo (Lib.empty_env()) scp frm

(*
   [skip]
   The do nothing tactic
   Useful for turning a node into a branch (e.g. for recursive
   functions)
 *)
    let skip info node = Subgoals.branch_node node

(* cut x sq: adds theorem x to assumptions of sq 

   asm |- cncl      --> t:x, asm |- cncl
   info: [] [t]
 *)
    let cut0 info x sq=
      let scp = Sequent.scope_of sq
      and ftag = Tag.create()
      in 
      let nf = (formula_of x)
      in 
      let nt = Formula.term_of nf
      in 
      let nasm = (ftag, (Formula.make scp nt))
      in 
      try 
	add_info info [] [ftag] [];
	mk_subgoal(Sequent.sqnt_tag sq, 
		   Sequent.sqnt_env sq, 
		   nasm::(Sequent.asms sq), 
		   Sequent.concls sq)
      with 
	x -> (add_logic_error "Not in scope of sequent" [nf] x)

    let cut info x sqnt = simple_sqnt_apply (cut0 info x) sqnt

(* 
   delete x sq: delete assumption (x<0) or conclusion (x>0) from sq
   info: [] []
 *)
    let delete0 inf x sq=
      let delete_asm () =
	let (lasms, _, rasms) = split_at_asm x (Sequent.asms sq)
	in 
	mk_subgoal (Sequent.sqnt_tag sq, 
		    Sequent.sqnt_env sq, 
		    join_up lasms rasms,
		    Sequent.concls sq)
      and delete_concl () =
	let (lcncls, _, rcncls) = split_at_concl x (Sequent.concls sq)
	in 
	mk_subgoal (Sequent.sqnt_tag sq, 
		    Sequent.sqnt_env sq, 
		    Sequent.asms sq, 
		    join_up lcncls rcncls)
      in 
      let ng = 
	try (delete_asm())
	with Not_found -> delete_concl()
      in 
      add_info inf [] [] [];
      ng

    let delete inf x g = 
      simple_sqnt_apply (delete0 inf x) g

(* conjC i sq: 
   g| asm |- t:(a /\ b), concl   
   -->
   g| asm |- t:a  and g'| asm |- t:b 

   (where t:a means formula has tag t)
   info: [g;g'] [t]
 *)

    let conjC0 inf i sq=
      let (lcncls, cncl, rcncls) = split_at_concl i (Sequent.concls sq)
      in 
      let (ft1, t)=cncl
      in 
      if (Formula.is_conj t) 
      then 
	(let (t1, t2) = get_pair (Formula.dest_conj t)
	in 
	let concll = join_up lcncls ((ft1, t1)::rcncls)
	and conclr = join_up lcncls ((ft1, t2)::rcncls)
	and tagl=Sequent.sqnt_tag sq
	and tagr=Tag.create()
	and asms = Sequent.asms sq
	in 
	add_info inf [tagl; tagr] [ft1] [];
	[Sequent.mk_sqnt tagl 
	   (Sequent.sqnt_env sq) asms concll;
	 Sequent.mk_sqnt tagr 
	   (Sequent.sqnt_env sq) asms conclr])
      else raise (logic_error "Not a conjunct" [t])

    let conjC inf i g = 
      simple_sqnt_apply (conjC0 inf i) g

(* conjA i sq: 
   t:a/\ b, asm |- concl   
   -->
   t:a, t':b, asm |- concl 
   info: [] [t; t']
 *)
    let conjA0 inf i sq=
      let lasms, asm, rasms = split_at_asm i (Sequent.asms sq)
      in 
      let (ft1, t) = asm
      in 
      if (Formula.is_conj t) 
      then 
	(let (t1, t2) = get_pair(Formula.dest_conj t)
	and ft2=Tag.create()
	in 
	let asm1=(ft1, t1)
	and asm2=(ft2, t2)
	in 
	add_info inf [] [ft1; ft2] [];
	mk_subgoal (Sequent.sqnt_tag sq, Sequent.sqnt_env sq, 
		    asm1::asm2::(join_up lasms rasms),
		    Sequent.concls sq))
      else raise (logic_error "Not a conjunction" [t])

    let conjA inf i g = 
      simple_sqnt_apply (conjA0 inf i) g

(* disjA i sq: 
   g| t:a\/b, asm |-  concl   
   -->
   g| t:a, asm |- concl  and g'| t:b, asm |- concl
   info: [g;g'] [t]
 *)
    let disjA0 inf i sq=
      let lasms, asm, rasms = split_at_asm i (Sequent.asms sq)
      in 
      let (ft, t) = asm
      in 
      if (Formula.is_disj t) 
      then 
	(let (t1, t2) = get_pair (Formula.dest_disj t)
	in 
	let asmsl= join_up lasms ((ft, t1)::rasms)
	and asmsr = join_up lasms ((ft, t2)::rasms)
	and tagl=Sequent.sqnt_tag sq
	and tagr=Tag.create()
	in 
	add_info inf [tagl; tagr] [ft] [];
	[Sequent.mk_sqnt tagl 
	   (Sequent.sqnt_env sq) asmsl (Sequent.concls sq);
	 Sequent.mk_sqnt tagr 
	   (Sequent.sqnt_env sq) asmsr (Sequent.concls sq)])
      else raise (logic_error "Not a disjunction" [t])

    let disjA inf i g = 
      simple_sqnt_apply (disjA0 inf i) g

(* disjC i sq: 
   asm |- t:a\/b, concl   
   -->
   asm |- t:a, t':b, concl 
   info: [] [t;t']
 *)

    let disjC0 inf i sq =
      let (lconcls, concl, rconcls) = split_at_concl i (Sequent.concls sq)
      in 
      let (ft1, t)=concl
      in 
      if (Formula.is_disj t) 
      then 
	(let (t1, t2) = get_pair (Formula.dest_disj t)
	and ft2=Tag.create()
	in 
	let cncl1=(ft1, t1)
	and cncl2=(ft2, t2)
	in 
	add_info inf [] [ft1; ft2] [];
	mk_subgoal 
	  (Sequent.sqnt_tag sq, 
	   Sequent.sqnt_env sq, 
	   Sequent.asms sq, 
	   cncl1::cncl2::(join_up lconcls rconcls)))
      else raise (logic_error "Not a disjunction" [t])

    let disjC inf i g = 
      simple_sqnt_apply (disjC0 inf i) g

(* negA i sq:
   t:~a, asms |- concl
   -->
   asms |- t:a, concl
   info: [] [t]
 *)
    let negA0 inf i sq =
      let lasms, asm, rasms = split_at_asm i (Sequent.asms sq)
      in 
      let (ft, t)= asm
      in 
      if (Formula.is_neg t) 
      then 
	(let t1 = get_one(Formula.dest_neg t)
	in 
	let cncl1=(ft, t1)
	in 
	add_info inf [] [ft] [];
	mk_subgoal (Sequent.sqnt_tag sq, 
		    Sequent.sqnt_env sq, 
		    join_up lasms rasms,
		    cncl1::(Sequent.concls sq)))
      else raise (logic_error "Not a negation"[t])

    let negA inf i g = 
      simple_sqnt_apply (negA0 inf i) g

(* negC i sq:
   asms |- t:~c, concl
   -->
   t:c, asms |- concl
   info: [] [t]
 *)
    let negC0 inf i sq =
      let lconcls, concl, rconcls = split_at_concl i (Sequent.concls sq)
      in 
      let (ft, t)=concl
      in 
      if (Formula.is_neg t) 
      then 
	(let t1 = get_one (Formula.dest_neg t)
	in 
	let asm1=(ft, t1)
	in 
	add_info inf [] [ft] [];
	mk_subgoal (Sequent.sqnt_tag sq, 
		    Sequent.sqnt_env sq,
		    asm1::(Sequent.asms sq), 
		    join_up lconcls rconcls))
      else raise (logic_error "Not a negation"[t])

    let negC inf i g = 
      simple_sqnt_apply (negC0 inf i) g

(* implC i sq
   asms |- t:a-> b, cncl 
   -->
   t':a, asms |- t:b, cncl
   info: [] [t'; t]
 *)
    let implC0 inf i sq=
      let lconcls, concl, rconcls = split_at_concl i (Sequent.concls sq)
      in 
      let (ft1, t)=concl
      in 
      if (Formula.is_implies t) 
      then 
	(let  (t1, t2) = get_pair (Formula.dest_implies t)
	and ft2=Tag.create()
	in 
	let asm =(ft2, t1)
	and cncl = (ft1, t2)
	in 
	add_info inf [] [ft2; ft1] [];
	mk_subgoal
	  (Sequent.sqnt_tag sq, 
	   Sequent.sqnt_env sq, 
	   asm::(Sequent.asms sq), 
	   join_up lconcls (cncl::rconcls)))
      else raise (logic_error "Not an implication" [t])

    let implC inf i g = 
      simple_sqnt_apply (implC0 inf i) g

(* implA i sq
   g| t:a => b,asms |-cncl 
   -->
   g'| asms |- t:a, cncl  
   and  
   g| t:b, asms |- cncl

   info: [g'; g]  [t]

   where g| asms |- concl 
   means g is the tag for the sequent
 *)
    let implA0 info i sq =
      let lasms, asm, rasms = split_at_asm i (Sequent.asms sq)
      in 
      let (ft, t)=asm
      in 
      if (Formula.is_implies t) 
      then 
	(let (t1, t2) = get_pair (Formula.dest_implies t)
	in 
	let asm2=join_up lasms ((ft, t2)::rasms)
	and asm1 = join_up lasms rasms
	and cncl1=(ft, t1)::(Sequent.concls sq)
	and tagl=Tag.create()
	and tagr=Sequent.sqnt_tag sq
	in 
	add_info info [tagl; tagr] [ft] [];
	[Sequent.mk_sqnt tagl 
	   (Sequent.sqnt_env sq) asm1 cncl1;
	 Sequent.mk_sqnt tagr 
	   (Sequent.sqnt_env sq) asm2 (Sequent.concls sq)])
      else raise (logic_error "Not an implication" [t])

    let implA info i g = 
      simple_sqnt_apply (implA0 info i) g

(* allC i sq
   asm |- t:!x. P(x), concl
   -->
   asm |- t:P(c), concl   where c is a new identifier

   info: [] [t] [c]
 *)
    let allC0 inf i tyenv sq =
      (* get the conclusion and its tag *)
      let lconcls, concl, rconcls = split_at_concl i (Sequent.concls sq)
      in 
      let (ft, t)=concl
      in 
      (* check that it is a universal quantification *)
      if (Formula.is_all t)
      then 
	(* make the skolem constant from the binder name and type *)
	(let (nv, nty)=(Formula.get_binder_name t, Formula.get_binder_type t)
	in 
	let sv, sty, nsklms, styenv, ntynms=
	  Skolem.mk_new_skolem 
	    {
	     Skolem.name=(mk_long (Sequent.thy_of_sqnt sq) nv);
	     Skolem.ty=nty;
	     Skolem.tyenv=tyenv;
	     Skolem.scope=Sequent.scope_of sq;
	     Skolem.skolems=Sequent.sklm_cnsts sq;
	     Skolem.tylist=Sequent.sqnt_tynames sq
	   }
	in 
(*
   let nscp = add_sklms_to_scope nsklms (scope_of sq)
 *)
	let nscp = Skolem.add_skolem_to_scope sv sty (Sequent.scope_of sq)
	in 
	(* add skolem constant and type variable to sequent list *)
	let nsqtys=
	  if (Gtypes.is_weak sty)
	  then sty::(Sequent.sqnt_tyvars sq)
	  else (Sequent.sqnt_tyvars sq)
	in 
	let ncncl, ntyenv = 
	  Formula.inst_env nscp [] styenv t sv
	in 
	(* update the goals' type environment *)
	let gtyenv=Gtypes.extract_bindings nsqtys ntyenv tyenv
	in 
	(* build the subgoal and return information *)
	add_info inf [] [ft] [sv];
	(mk_subgoal(Sequent.sqnt_tag sq, 
		    Sequent.mk_sqnt_env nsklms nscp nsqtys ntynms,
		    Sequent.asms sq, 
		    join_up lconcls ((ft, ncncl)::rconcls)),
	 gtyenv))
      else raise (logic_error "Not a universal quantifier" [t])

    let allC inf i g = 
      sqnt_apply (allC0 inf i) g

(* existA i sq
   t:?x. P(x), asm |- concl
   -->
   t:P(c), asm |- concl   where c is a new identifier
   info: [] [t] [c]
 *)
    let existA0 inf i tyenv sq =
      (* get the assumption and its tag *)
      let lasms, asm, rasms = split_at_asm i (Sequent.asms sq)
      in 
      let (ft, t)=asm
      in 
      (* check that it is existential quantification *)
      if (Formula.is_exists t)
      then 
	(* make the skolem constant from the binder name and type *)
	(let (nv, nty) = (Formula.get_binder_name t, Formula.get_binder_type t)
	in 
	let sv, sty, nsklms, styenv, ntynms=
	  Skolem.mk_new_skolem
	    {
	     Skolem.name=(mk_long (Sequent.thy_of_sqnt sq) nv);
	     Skolem.ty=nty;
	     Skolem.tyenv=tyenv;
	     Skolem.scope=Sequent.scope_of sq;
	     Skolem.skolems=Sequent.sklm_cnsts sq;
	     Skolem.tylist=Sequent.sqnt_tynames sq
	   }
	in 
	let nscp = Skolem.add_skolem_to_scope sv sty (Sequent.scope_of sq)
(*
   let nscp = add_sklms_to_scope nsklms (scope_of sq)
 *)	in 
	(* add skolem constant and type variable to sequent list *)
	let nsqtys=
	  if (Gtypes.is_weak sty)
	  then sty::(Sequent.sqnt_tyvars sq)
	  else (Sequent.sqnt_tyvars sq)
	in 
	let nasm, ntyenv= 
	  Formula.inst_env nscp [] styenv t sv
	in 
	(* update the goals' type environment *)
	let gtyenv=Gtypes.extract_bindings nsqtys ntyenv tyenv
	in 
	add_info inf [] [ft] [sv];
	(mk_subgoal
	   (Sequent.sqnt_tag sq, 
	    Sequent.mk_sqnt_env nsklms nscp nsqtys ntynms,
	    join_up lasms ((ft, nasm)::rasms),
	    Sequent.concls sq)), 
	gtyenv)
      else raise (logic_error "Not an existential quantifier" [t])

    let existA inf i g = 
      sqnt_apply (existA0 inf i) g

(* trueR i sq
   t:asm |- true, concl
   --> true
   info : [] []
 *)
    let trueR0 inf i sq=
      let lconcls, concl, rconcls = split_at_concl i (Sequent.concls sq)
      in 
      let (_, t)=concl
      in 
      if (Formula.is_true t)
      then 
	(add_info inf [] [] [];
	 raise No_subgoals)
      else 
	raise (logic_error "Not trivial" [t])

    let trueR inf i g = 
      simple_sqnt_apply (trueR0 inf i) g

(*
   betaA i sq: beta reduction of assumption i
   t:(%x.P(x))(c), asm |- concl
   -->
   t:P(c), asm |- concl
   info: [] [t] 

   betaC i sq: beta reduction of conclusion i
   asm |- t:(%x.P(x))(c), concl
   -->
   asm |- t:P(c), concl
   info: [] [t] 

   raise Not_found if assumption/conclusion not found.

   beta i sq:  beta reduction of concl or asm i in sq
   (conclusions searched first)

   t:(%x.P(x))(c), asm |- concl
   -->
   t:P(c), asm |- concl
   info: [] [t] 
 *)
    let betaA0 inf i sq = 
      let lasms, asm, rasms = split_at_asm i (Sequent.asms sq)
      in 
      let (ft, t) = asm
      in
      let nt = 
	(ft,
	 try 
	   Formula.beta_reduce (Sequent.scope_of sq) t
	 with x -> raise 
	     (Result.add_error(logic_error "Beta reduction" [t]) x))
      in 
      add_info inf [] [ft] [];
      mk_subgoal
	(Sequent.sqnt_tag sq, 
	 Sequent.sqnt_env sq, 
	 join_up lasms (nt::rasms),
	 Sequent.concls sq)

    let betaA info i g = 
      simple_sqnt_apply (betaA0 info i) g

    let betaC0 inf i sq = 
      let lconcls, concl, rconcls = split_at_concl i (Sequent.concls sq)
      in 
      let (ft, t) = concl
      in 
      let nt = 
	(ft,
	 try 
	   Formula.beta_reduce (Sequent.scope_of sq) t
	 with x -> raise 
	     (Result.add_error(logic_error "Beta reduction" [t]) x))
      in 
      add_info inf [] [ft] [];
      mk_subgoal
	(Sequent.sqnt_tag sq, 
	 Sequent.sqnt_env sq, 
	 Sequent.asms sq, 
	 join_up lconcls (nt::rconcls))

    let betaC info i g = 
      simple_sqnt_apply (betaC0 info i) g

    let beta info i g=
      try betaC info i g
      with Not_found -> betaA info i g

(* instantiation terms *)  

    let inst_term sq tyenv t trm =
      let scp = Sequent.scope_of sq
      in 
      let mtyenv = ref tyenv
      in 
      let fm1 = Formula.make ~env:mtyenv scp trm
      in 
      let ntrm1= Formula.term_of fm1
      in 
      let ntrm2, ntyenv2=Formula.inst_env scp [] tyenv t ntrm1
      in 
      let ntyenv3=Formula.typecheck_env scp ntyenv2 ntrm2 
	  (Gtypes.mk_var "inst_ty")
      in ntrm2, ntyenv3

(* basic i j sq: asm i is alpha-equal to cncl j of sq, 

   asm, a_{i}, asm' |- concl, c_{j}, concl' 
   -->
   true if a_{i}=c_{j}
   info: [] []
 *)
    let basic0 inf i j tyenv sq = 
      let scp = Sequent.scope_of sq
      and (lasms, asm, rasms) = split_at_asm i (Sequent.asms sq)
      and (lconcls, concl, rconcls) = split_at_concl j (Sequent.concls sq)
      in 
      let tyenv1 = 
	try
	  Formula.alpha_equals_match 
	    scp tyenv (drop_tag asm) (drop_tag concl)
	with _ -> 
	  (raise (logic_error "Assumption not equal to conclusion"
		    [drop_tag asm; drop_tag concl]))
      in 
      let tyenv2=
	try 
	  Gtypes.extract_bindings (Sequent.sqnt_tyvars sq) tyenv1 tyenv
	with _ -> 
	  (raise (logic_error "basic: Inconsistent types"
		    [drop_tag asm; drop_tag concl]))
      in 
      (add_info inf [] [] []; raise (Solved_subgoal tyenv2))

    let basic inf i j g = 
      sqnt_apply (basic0 inf i j) g


(* existC i sq
   asm |- t:?x. P(c), concl
   -->
   asm |- t:P(c), concl where c is a given term
   info: [] [t] []
 *)

    let existC0 inf trm i tyenv sq =
      let lconcls, concl, rconcls = split_at_concl i (Sequent.concls sq)
      in
      let (ft, t)=concl
      in 
      if (Formula.is_exists t) 
      then 
	try 
	  let trm2, tyenv2 = inst_term sq tyenv t trm
	  in 
	  let gtyenv=
	    Gtypes.extract_bindings (Sequent.sqnt_tyvars sq) tyenv2 tyenv
	  in 
	  add_info inf [] [ft] [];
      	  (mk_subgoal
	     (Sequent.sqnt_tag sq, 
	      Sequent.sqnt_env sq, 
	      Sequent.asms sq, 
	      join_up lconcls ((ft, trm2)::rconcls)),
	   gtyenv)
	with x -> raise (Result.add_error
			   (logic_error "existC:" [t]) x)
      else 
	raise (logic_error "Not an existential quantifier" [t])

    let existC inf trm i g = 
      sqnt_apply (existC0 inf trm i) g

(* allA i sq
   t:!x. P(c), asm |-  concl
   -->
   t:P(c'), asm |- concl   where c' is a given term
   info: [] [t] []
 *)
    let allA0 inf trm i tyenv sq =
      let lasms, asm, rasms = split_at_asm i (Sequent.asms sq)
      in 
      let (ft, t)=asm
      in 
      if (Formula.is_all t) 
      then 
	try 
	  (let ntrm, tyenv2 = inst_term sq tyenv t trm
	  in 
	  let gtyenv=
	    Gtypes.extract_bindings (Sequent.sqnt_tyvars sq) tyenv2 tyenv
	  in 
	  add_info inf [] [ft] [];
	  (mk_subgoal
	     (Sequent.sqnt_tag sq, 
	      Sequent.sqnt_env sq, 
	      join_up lasms ((ft, ntrm)::rasms),
	      Sequent.concls sq)),
	  gtyenv)
	with x -> 
	  (raise (Result.add_error
		    (logic_error "allA: " [t]) x))
      else 
	raise (logic_error "Not a universal quantifier" [t])


    let allA inf trm i g = 
      sqnt_apply (allA0 inf trm i) g

(*
   rewrite ctrl simple thms j sq:
   list of theorems or assumptions containing x=y
   asm |- t:P(x), concl
   -->
   asm |- t:P(y), concl
   where ctrl is the rewrite control
   theorems must be in scope.
   silently discards theorems not in scope and assumptions which don't exist
   info: [] [t] []

   rewriteA ctrl simple thms j sq: rewrite assumption j
   rewriteC ctrl simple thms j sq: rewrite conclusion j
 *)
    let filter_rules scp rls j sq= 
      let memo = Lib.empty_env() 
      and thyname= Scope.thy_of scp
      in 
      let rec ft srcs rslt =
	match srcs with 
	  [] -> List.rev rslt
	|  (Asm(x)::xs) ->
	    let asm=
	      (try 
		drop_tag(Sequent.get_tagged_asm (label_to_tag x sq) sq)
	      with 
		Not_found -> 
		  raise 
		    (logic_error "Rewrite: can't find tagged assumption" []))
	    in 
	    ft xs ((Rewrite.rule (Formula.term_of asm))::rslt)
	|  (OAsm(x, order)::xs) ->
	    let asm=
	      (try 
		drop_tag (Sequent.get_tagged_asm (label_to_tag x sq) sq)
	      with 
		Not_found -> 
		  raise 
		    (logic_error "Rewrite: can't find tagged assumption" []))
	    in 
	    ft xs ((Rewrite.orule (Formula.term_of asm) order)::rslt)
	| ((RRThm(x))::xs) -> 
	    (try 
	      check_term_memo memo scp (formula_of x);
	      ft xs ((Rewrite.rule (term_of x))::rslt)
	    with 
	      _ -> ft xs rslt)
	| ((ORRThm(x, order))::xs) -> 
	    (try 
	      (check_term_memo memo scp (formula_of x));
	      ft xs 
		((Rewrite.orule (term_of x) order)
		 ::rslt)
	    with 
	      _ -> ft xs rslt)
      in ft rls []

    let rewriteA0 inf ctrl rls j tyenv sq=
      let scp = Sequent.scope_of sq
      and lasms, asm, rasms = split_at_asm j (Sequent.asms sq)
      in 
      let r=filter_rules scp rls j sq
      and (ft, t)= asm
      in 
      try
	(let nt, ntyenv = 
	  Formula.rewrite_env scp ~ctrl:ctrl tyenv r t
	in 
	let gtyenv = 
	  Gtypes.extract_bindings (Sequent.sqnt_tyvars sq) ntyenv tyenv
	in 
	add_info inf [] [ft] [];
	(mk_subgoal
	   (Sequent.sqnt_tag sq, 
	    Sequent.sqnt_env sq, 
	    join_up lasms ((ft, nt)::rasms),
	    Sequent.concls sq), 
	 gtyenv))
      with x -> raise 
	  (Result.add_error (logic_error "rewriting" [t]) x)

    let rewriteA inf ?ctrl rls j g=
      let rrc = Lib.get_option ctrl Formula.default_rr_control
      in 
      sqnt_apply (rewriteA0 inf rrc rls j) g

    let rewriteC0 inf ctrl rls j tyenv sq=
      let scp = Sequent.scope_of sq
      and lconcls, concl, rconcls = split_at_concl j (Sequent.concls sq)
      in 
      let r=filter_rules scp rls j sq
      and (ft, t)= concl
      in 
      try
	(let nt, ntyenv = 
	  Formula.rewrite_env scp ~ctrl:ctrl tyenv r t
	in 
	let gtyenv = 
	  Gtypes.extract_bindings (Sequent.sqnt_tyvars sq) ntyenv tyenv
	in 
	add_info inf [] [ft] [];
	(mk_subgoal
	   (Sequent.sqnt_tag sq, 
	    Sequent.sqnt_env sq, 
	    Sequent.asms sq, 
	    join_up lconcls ((ft, nt)::rconcls)),
	 gtyenv))
      with x -> raise 
	  (Result.add_error (logic_error"rewriting" [t]) x)

    let rewriteC inf ?ctrl rls j g=
      let rrc = Lib.get_option ctrl Formula.default_rr_control
      in 
      sqnt_apply (rewriteC0 inf rrc rls j) g

    let rewrite inf ?ctrl rls j g=
      let rrc = Lib.get_option ctrl Formula.default_rr_control
      in 
      try 
	sqnt_apply (rewriteC0 inf rrc rls j) g
      with Not_found -> sqnt_apply (rewriteA0 inf rrc rls j) g

(*
   [rewrite_rule scp ctrl rrl thm]
   rewrite theorem [thm] with rules [rrl] in scope [scp].
 *)
    let rewrite_rule scp ?(ctrl=Formula.default_rr_control) rrl thm =
      let conv_aux t = 
	try 
	  let f= formula_of t
	  in 
	  let nt = Formula.rewrite ~ctrl:ctrl scp 
	      (List.map 
		 (fun x -> 
		   Rewrite.rule (term_of x)) rrl) f
	  in mk_theorem nt
	with x -> raise 
	    (Result.add_error(logic_error "rewrite_conv" [formula_of t]) x)
      in 
      conv_aux thm

  end


(* 
   cdefn:  Checked Definitions: 
   checking of type and term definitions and declarations

   scdefn: version of cdefn suitable for disk storage.
 *)
type cdefn =
    TypeAlias of Basic.ident * string list * Basic.gtype option
  | TypeDef of ctypedef
  | TermDecln of Basic.ident * Basic.gtype
  | TermDef of 
      Basic.ident * Basic.gtype	* thm 
and ctypedef =
    {
     type_name : Basic.ident;  (* name of new type *)
     type_args : string list;  (* arguments of new type *)
     type_base: Basic.gtype;   (* the base type *)
     type_rep: cdefn;          (* representation function *)
     type_abs: cdefn;          (* abstraction function *)
     type_set: Formula.form;      (* defining set *)
     rep_type: thm;
     rep_type_inverse: thm;
     abs_type_inverse: thm
   }

type saved_cdefn =
    STypeAlias of Basic.ident * string list * Gtypes.stype option
  | STypeDef of saved_ctypedef
  | STermDecln of Basic.ident * Gtypes.stype
  | STermDef of Basic.ident * Gtypes.stype * saved_thm 
and saved_ctypedef =
    {
     stype_name : Basic.ident;  (* name of new type *)
     stype_args : string list;  (* arguments of new type *)
     stype_base: Gtypes.stype; 
     stype_rep: saved_cdefn;          (* representation function *)
     stype_abs: saved_cdefn;          (* abstraction function *)
     stype_set: Formula.saved_form;      (* defining set *)
     srep_type: saved_thm;
     srep_type_inverse: saved_thm;
     sabs_type_inverse: saved_thm
   }


module Conv=
  struct
    
    (** 
       [beta_conv scp term]: Apply a single beta conversion to [term].

       Fails if [term] is not of the form [(%x: F)y]
       or the resulting formula is not in scope.
     *)

    let beta_conv scp term =
      let rhs ()= Logicterm.beta_conv term
      in 
      let eq_term t = 
	Formula.make scp (Logicterm.mk_equality term t)
      in 
      try
	mk_theorem (eq_term (rhs()))
      with err -> 
	raise(Result.add_error
		(logic_error "beta_conv" [])
		(Result.add_error 
		   (Term.term_error "beta_conv term: " [term]) err))

  end 

(* Defns: functions for checking definitions and declarations *)
module Defns =
  struct

    let defn_error s t = Term.term_error s (List.map Formula.term_of t)
    let add_defn_error s t es = 
      raise (Result.add_error (defn_error s t) es)


    let rec to_saved_cdefn td = 
      match td with
	TypeAlias (id, sl, ty) ->
	  (match ty with 
	    None -> STypeAlias (id, sl, None)
	  | Some t -> STypeAlias (id, sl, Some(Gtypes.to_save t)))
      | TermDecln (id, ty) -> 
	  STermDecln (id, Gtypes.to_save ty)
      | TermDef (id, ty, thm) ->
	  STermDef (id, Gtypes.to_save ty, to_save thm)
      | TypeDef ctdef ->
	  STypeDef (to_saved_ctypedef ctdef)
    and
	to_saved_ctypedef x = 
      {
       stype_name = x.type_name;
       stype_args = x.type_args;
       stype_base = Gtypes.to_save x.type_base;
       stype_rep = to_saved_cdefn x.type_rep;
       stype_abs = to_saved_cdefn x.type_abs;
       stype_set = Formula.to_save x.type_set;
       srep_type = to_save x.rep_type;
       srep_type_inverse = to_save x.rep_type_inverse;
       sabs_type_inverse = to_save x.abs_type_inverse
     }

    let rec from_saved_cdefn td = 
      match td with
	STypeAlias (id, sl, ty) ->
	  (match ty with 
	    None -> TypeAlias (id, sl, None)
	  | Some t -> TypeAlias (id, sl, Some(Gtypes.from_save t)))
      | STermDecln (id, ty) -> 
	  TermDecln (id, Gtypes.from_save ty)
      | STermDef (id, ty, thm) ->
	  TermDef (id, Gtypes.from_save ty, from_save thm)
      | STypeDef ctdef ->
	  TypeDef (from_saved_ctypedef ctdef)
    and
	from_saved_ctypedef x = 
      {
       type_name = x.stype_name;
       type_args = x.stype_args;
       type_base = Gtypes.from_save x.stype_base;
       type_rep = from_saved_cdefn x.stype_rep;
       type_abs = from_saved_cdefn x.stype_abs;
       type_set = Formula.from_save x.stype_set;
       rep_type = from_save x.srep_type;
       rep_type_inverse = from_save x.srep_type_inverse;
       abs_type_inverse = from_save x.sabs_type_inverse
     }



(* mk_termdef scp n ty args d:
   - check n doesn't exist already
   - check all arguments in args are unique
 *)
    let is_termdef x = 
      match x with 
	TermDef _ -> true
      | _ -> false

    let dest_termdef x =
      match x with 
	TermDef (id, ty, thm) -> (id, ty, thm)
      | _ -> raise (defn_error "Not a term definition" [])

    let mk_termdef scp n args d = 
      let (id, ty, frm) = Defn.mk_defn scp n args d
      in 
      let thm = mk_axiom frm
      in 
      TermDef(id, ty, thm)

(* 
   [mk_termdecln scp name ty]: Declare identifier [name] of type [ty] in
   scope [scp].
   Fails if identifier [name] is already defined in [scp]
   or if [ty] is not well defined.
 *)
    let is_termdecln x = 
      match x with 
	TermDecln _ -> true
      | _ -> false

    let dest_termdecln x =
      match x with 
	TermDecln (id, ty) -> (id, ty)
      | _ -> raise (defn_error "Not a term declaration" [])

    let mk_termdecln scp n ty =
      let name = Basic.mk_long (Scope.thy_of scp) n
      in 
      let (id, typ) = Defn.mk_decln scp name ty
      in 
      TermDecln(id, typ)

(*
   Type definitions
 *)

(* Type aliasing *)

(* mk_typealias scp n args d:
   - check n doesn't exist already
   - check all arguments in args are unique
   if defining n as d
   - check d is well defined 
   (all constructors exist and variables are in the list of arguments)
 *)
    let is_typealias x = 
      match x with 
	TypeAlias _ -> true
      | _ -> false

    let dest_typealias x =
      match x with 
	TypeAlias (id, args, def) -> (id, args, def)
      | _ -> raise (defn_error "Not a term definition" [])


(*
   mk_typealias scp n args d:
   - check n doesn't exist already
   - check all arguments in args are unique
   if defining n as d
   - check d is well defined 
   (all constructors exist and variables are in the list of arguments)
 *)
    let mk_typealias scp n ags d =
      let th = Scope.thy_of scp
      in 
      let args = Defn.check_args_unique ags
      in 
      let dfn = 
	match d with
	  None -> None
	| Some(a) -> 
	    (try 
	      Gtypes.well_defined scp ags a;
	      Some(a)
	    with err -> 
	      raise (Gtypes.add_type_error "Badly formed definition" [a] err))
      in 
      TypeAlias((Basic.mk_long th n), ags, dfn)

(* Type definition: subtype *)


(*
   mk_subtype scp name args d setP rep:
   - check name doesn't exist already
   - check all arguments in args are unique
   - check def is well defined 
   (all constructors exist and variables are in the list of arguments)
   - ensure setP has type (d -> bool)
   - declare rep as a function of type (d -> n)
   - make subtype property from setp and rep.
 *)

    let is_subtype x = 
      match x with 
	TypeDef _ -> true
      | _ -> false

    let dest_subtype x =
      match x with 
	TypeDef ctd -> ctd
      | _ -> raise (defn_error "Not a term definition" [])

    let mk_subtype_thm scp prop =
      mk_axiom (Formula.make scp prop)

(*
   [prove_subtype_exists scp setp thm]
   Use [thm] to prove the goal << ?x. setp x >> (built by mk_subtype_exists).

   [thm] should be of the form << ?x. setp x >> otherwise 
   the proof will fail.
 *)
    let prove_subtype_exists scp setp thm=
      let goal_form = Formula.make scp (Defn.mk_subtype_exists setp)
      in 
      let gl = mk_goal scp goal_form
      in 
      let info = ref (make_tag_record [] [] [])
      in 
      let tac1 =
	(fun g -> Tactics.cut (Some info) thm g)
      in 
      let tac2 =
	(fun g ->
	  let a = FTag (List.hd ((!info).forms))
	  and c = FNum 1
	  in 
	  Tactics.basic None a c g)
      in 
      let gl1=Subgoals.apply_to_goal tac1 gl
      in 
      let gl2=Subgoals.apply_to_goal tac2 gl1
      in 
      mk_thm gl2

    let mk_subtype scp name args dtype setp rep_name abs_name exist_thm =
      (* run checks and get the subtype property *)
      let dtype1 = Gtypes.set_name scp dtype
      in
      let subtype_def =
	Defn.mk_subtype scp name args dtype1 setp rep_name abs_name
      in 
      let new_setp = subtype_def.Defn.set
      and type_id = subtype_def.Defn.id
      and (rep_id, rep_ty) = subtype_def.Defn.rep
      and (abs_id, abs_ty) = subtype_def.Defn.abs
      in 
      (* try to prove the subtype exists *)
      let does_exist_thm = prove_subtype_exists scp new_setp exist_thm
      in 
      (* temporarily extend the scope with the new type and rep identifier *)
      (* nscp0: scope with new type *)
      let nscp0 = Scope.extend_with_typedeclns scp [(type_id, args)]
      in 
      (* declare the the rep function *)
      let rep_decln = mk_termdecln nscp0 rep_name rep_ty
      and abs_decln = mk_termdecln nscp0 abs_name abs_ty
      in 
      (* nscp1: scope with new type and rep identifier *)
      let nscp1 = 
	let (rid, rty) = dest_termdecln rep_decln
	and (aid, aty) = dest_termdecln abs_decln
	in
	Scope.extend_with_terms nscp0 [(rid, rty); (aid, aty)]
      in 
      TypeDef
	{
	 type_name = subtype_def.Defn.id;
	 type_args = subtype_def.Defn.args; 
	 type_base = dtype1;
	 type_rep = rep_decln;
	 type_abs = abs_decln;
	 type_set = Formula.make scp new_setp;
	 rep_type = mk_subtype_thm nscp1 subtype_def.Defn.rep_T;
	 rep_type_inverse = 
	 mk_subtype_thm nscp1 subtype_def.Defn.rep_T_inverse;
	 abs_type_inverse = 
	 mk_subtype_thm nscp1 subtype_def.Defn.abs_T_inverse;
       }

  end

let print_sqnt ppinfo sq = 
  let nice = Settings.get_nice_sequent()
  in let nice_prefix = 
    if nice then (!Settings.nice_sequent_prefix)
    else "-"
  in 
  let string_of_asm_index i =  (nice_prefix^(string_of_int (-i)))
  in 
  let string_of_concl_index i = string_of_int i
  in 
  let rec print_asm i afl= 
    Format.printf "@[<v>";
    (match afl with 
      [] -> ()
    | (s::als) -> 
	(Format.printf "@[[%s] " (string_of_asm_index i);
	 Term.print ppinfo (Formula.term_of s);
	 Format.printf "@]@,";
	 print_asm (i-1) als));
    Format.printf "@]"
  and print_cncl i cfl =
    Format.printf "@[<v>";
    (match cfl with
      [] -> ()
    | (s::cls) -> 
	(Format.printf "@[[%s] " (string_of_concl_index i);
	 Term.print ppinfo (Formula.term_of s);
	 Format.printf "@]@,";
	 (print_cncl (i+1) cls)));
    Format.printf "@]"
  in 
  let sq_asms = List.map drop_tag (Sequent.asms sq)
  and sq_concls = List.map drop_tag (Sequent.concls sq)
  in 
  Format.printf "@[<v>";
  (match sq_asms with
    [] -> ()
  | _ -> print_asm (-1) sq_asms);
  Format.printf ("@[----------------------@]@,"); 
  print_cncl 1 sq_concls;
  Format.printf "@]"

let print_node ppstate n = print_sqnt ppstate (Subgoals.node_sqnt n)

let print_branch ppstate branch=
  let rec print_subgoals i gs = 
    match gs with 
      [] -> ()
    | (y::ys) -> 
	Format.printf "@[<v>";
	Format.printf "@[(Subgoal %i)@]@," i;
	print_sqnt ppstate y;
	Format.printf "@]@,";
	print_subgoals (i+1) ys
  in 
  let sqnts = Subgoals.branch_sqnts branch
  in 
  Format.printf "@[<v>";
  (match sqnts with
    [] -> 
      Format.printf "@[No subgoals@]@,"
  | _ -> 
      let len=(List.length sqnts)
      in 
      Format.printf "@[%i %s@]@," 
	len
	(if(len>1) then "subgoal" else "subgoals");
      print_subgoals 1 sqnts);
  Format.printf "@]"


let print_termdefn ppinfo (n, ty, th) = 
  Format.printf "@[";
  Format.printf "@[";
  Printer.print_ident (Basic.mk_long Basic.null_thy (Basic.name n));
  Format.printf ":@ ";
  Gtypes.print ppinfo ty;
  Format.printf "@],@ ";
  print_thm ppinfo th;
  Format.printf "@]"

let print_termdecln ppinfo (n, ty) = 
  Format.printf "@[";
  Printer.print_ident (Basic.mk_long Basic.null_thy (Basic.name n));
  Format.printf ":@ ";
  Gtypes.print ppinfo ty;
  Format.printf "@]"

let print_typealias ppinfo (n, args, ty) = 
  let named_ty = 
    Gtypes.mk_constr 
      (Basic.Defined n)
      (List.map (fun x -> Gtypes.mk_var x) args)
  in 
  Format.printf "@[";
  Gtypes.print ppinfo named_ty;
  (match ty with
    None -> ()
  | (Some t) -> 
      Format.printf "=@,";
      Gtypes.print ppinfo t);
  Format.printf "@]"

let rec print_subtype ppinfo x =  
  let named_ty = 
    Gtypes.mk_constr 
      (Basic.Defined x.type_name)
      (List.map (fun x -> Gtypes.mk_var x) x.type_args)
  in 
  Format.printf "@[<v>";
  Format.printf "@[";
  Gtypes.print ppinfo named_ty;
  Format.printf "@ =@ ";
  Gtypes.print ppinfo x.type_base;
  Format.printf "@,:@ ";
  Formula.print ppinfo x.type_set;
  Format.printf "@]@,";
  print_cdefn ppinfo x.type_rep;
  Format.printf "@,";
  print_cdefn ppinfo x.type_abs;
  Format.printf "@,";
  print_thm ppinfo x.rep_type;
  Format.printf "@,";
  print_thm ppinfo x.rep_type_inverse;
  Format.printf "@,";
  print_thm ppinfo x.abs_type_inverse;
  Format.printf "@]"
and
    print_cdefn ppinfo x = 
  Format.printf "@[";
  (match x with
    TypeAlias (n, args, ty) -> 
      print_typealias ppinfo (n, args, ty)
  | TypeDef y -> print_subtype ppinfo y
  | TermDecln (n, ty) -> print_termdecln ppinfo (n, ty)
  | TermDef (n, ty, th) -> print_termdefn ppinfo (n, ty, th));
  Format.printf "@]"

