open Basic
open Term
open Result

type direction = LeftRight | RightLeft 

let leftright=LeftRight
let rightleft=RightLeft

type strategy = TopDown | BottomUp 

let topdown = TopDown
let bottomup = BottomUp

let is_topdown t = 
  match t with TopDown -> true  | _ -> false

let is_bottonup t = 
  match t with BottomUp -> true  | _ -> false

type control =
    { 
      scope: Gtypes.scope;
      depth: int option; (** (Some i): maximum number of times to rewrite is i,
			    None : unlimited rewriting (default) *)
      rr_dir: direction;
      rr_strat: strategy
    }

let control ?max ?(dir=LeftRight) ?(strat=TopDown) scp = 
  { scope = scp; depth=max; rr_dir=dir; rr_strat=strat }

let limit_reached d = 
  match d with Some 0 -> true | _ -> false

let varp, funp, constp, is_app, is_typed, qntp=
  Term.is_bound, Term.is_fun, Term.is_const, 
  Term.is_app, Term.is_typed, Term.is_qnt 

let eqqnt tyenv s t = 
  let _, qnt1, _, qty1, _=dest_qnt s
  and _, qnt2, _, qty2, _=dest_qnt t
  in 
  (Gtypes.matches tyenv qty1 qty2) & (qnt1=qnt2)

let eqqnt_env scp s t tyenv = 
  let _, qnt1, _, qty1, _=dest_qnt s
  and _, qnt2, _, qty2, _=dest_qnt t
  in 
  if qnt1=qnt2 then 
    (try 
      (true, Gtypes.unify_env scp qty1 qty2 tyenv)
    with _ -> (false, tyenv))
  else (false, tyenv)

let is_free_binder qs t= 
  (match t with
    Bound(q) -> List.exists (fun x ->  x == q) qs
  |	_ -> false)

(* make_rewrites: convert list of equalities to db of rewrites *)

let make_rewrites xs = 
  let rec make_rewrites_aux xs net=
    match xs with
      [] -> net
    | ((vs, key, rep)::rst) -> make_rewrites_aux rst
	  (Net.add (is_free_binder vs) net key (vs, key, rep))
  in 
  make_rewrites_aux (List.rev xs) (Net.empty())

type rewrite_rules = (Basic.binders list * Basic.term * Basic.term)
type rewriteDB = 
    Net_rr of rewrite_rules Net.net 
  | List_rr of rewrite_rules list 

let find_match ctrl tyenv varp term1 term2 env=
  try 
    Unify.unify_fullenv_rewrite ctrl.scope tyenv env varp term1 term2 
  with x -> 
    raise 
      (add_error (termError ("Can't match terms") [term1; term2]) x)

let match_rewrite ctrl tyenv varp lhs rhs trm = 
  let env = Term.empty_subst ()
  in 
  try
    (let tyenv1, env1=find_match ctrl tyenv varp lhs trm env; 
    in 
    Term.subst env1 rhs, tyenv1)
  with x -> 
    (Term.addtermError "match_rewrite: failed" [lhs; trm] x)

(* 
   rewriting with a choice of rewrites 
   rewrite_list_aux qs (t, rs) t2 
   replaces t with some r in rs in trm if matching in binders qs 
*)

let rec match_rr_list ctrl tyenv chng rs trm = 
  match rs with
    [] -> (trm, tyenv)
  | (qs, lhs, rhs)::nxt ->
      let (ntrm, ntyenv), fl = 
	(try 
	  match_rewrite ctrl tyenv (is_free_binder qs) lhs rhs trm, true
	with _ -> (trm, tyenv), false)
      in 
      if fl 
      then (chng:=true; (ntrm, ntyenv))
      else 
	match_rr_list ctrl ntyenv chng nxt trm

let rec match_rewrite_list ctrl tyenv chng net trm =
  let cn=ref false 
  in 
  let rs=
    match net with
      (Net_rr n) -> Net.lookup n trm
    | (List_rr r) -> r 
  in 
  let ntrm, ntyenv=match_rr_list ctrl tyenv cn rs trm
  in 
  if (!cn) 
  then 
    (chng:=true; 
     match_rewrite_list ctrl ntyenv chng net ntrm)
  else
    ntrm, ntyenv

let rewrite_list_topdown ctrl tyenv chng net trm = 
  let rec rewrite_subterm env t=
    match t with
      Basic.Qnt(qnt, q, b) -> 
	let nb, benv = rewrite_aux env b
	in 
	(Basic.Qnt(qnt, q, nb), benv)
    |	Basic.App(f, a)->
	let nf, fenv = (rewrite_aux env f)
	in
	let na, aenv = (rewrite_aux fenv a)
	in 
	(Basic.App(nf, na), aenv)
    | Basic.Typed(tt, ty) -> rewrite_aux env tt
    | _ -> (t, env)
  and rewrite_aux env t = 
    let nt, nenv= match_rewrite_list ctrl env chng net t
    in 
    rewrite_subterm nenv nt
  in 
  rewrite_aux tyenv trm



let rewrite_list_bottomup ctrl tyenv chng net trm = 
  let rec rewrite_aux env t=
    match t with
      Basic.Qnt(qnt, q, b) -> 
	(let nb, benv = rewrite_aux env b
	in 
	match_rewrite_list ctrl benv chng net (Qnt(qnt, q, nb)))
    |	Basic.App(f, a)->
	  (let nf, fenv = 
	    (rewrite_aux env f)
	  in
	  let na, aenv = 
	    (rewrite_aux fenv a)
	  in 
	  match_rewrite_list ctrl aenv chng net (Basic.App(nf, na)))
    | Basic.Typed(tt, ty) -> 
	rewrite_aux env tt
    | _ -> 
	match_rewrite_list ctrl env chng net t
  in 
  rewrite_aux tyenv trm

(* using term nets of rewrites *)

let rewrite_list ctrl chng tyenv rs trm = 
  let nt=Net_rr(make_rewrites rs)
  in 
  if(is_topdown (ctrl.rr_strat))
  then 
    rewrite_list_topdown ctrl tyenv chng nt trm
  else 
    rewrite_list_bottomup ctrl tyenv chng nt trm


(*
   rewrite with equality eqtrm: "l=r" -> (l, r) 
   left-right if dir=true, right-left otherwise
*)

let rewrite_eqs ctrl tyenv rrl trm =
  let chng = ref false
  in 
  let r =
    if ctrl.rr_dir=LeftRight
    then rewrite_list ctrl chng tyenv
	(List.map (fun (qs, b)-> 
	  let (lhs, rhs) = Logicterm.dest_equal b 
	  in (qs, lhs, rhs)) rrl) trm
    else 
      rewrite_list ctrl chng tyenv
	(List.map (fun (qs, b) -> 
	  let (lhs, rhs)= Logicterm.dest_equal b 
	  in (qs, rhs, lhs)) rrl) trm
  in 
  if !chng 
  then r
  else raise (termError "Matching" [trm])
      

(*
   rewrite with equality term: "!x_1, ..., x_n. l=r" 
   -> [x_1, ..., x_n](l, r) 
   left-right if dir=true, right-left otherwise
 *)

let rewrite_env ctrl tyenv rrl trm=
  let rs = List.map (strip_qnt Basic.All) rrl
  in 
  rewrite_eqs ctrl tyenv rs trm

let rewrite ctrl rrl trm =
  let (ret, _) = rewrite_env ctrl (Gtypes.empty_subst()) rrl trm
  in ret

(* rewrite_net *)

let rewrite_net_env ctrl tyenv rn trm = 
  let chng = ref false
  in 
  let nt, nenv = 
    if(is_topdown ctrl.rr_strat)
    then rewrite_list_topdown ctrl tyenv chng rn trm
    else rewrite_list_bottomup ctrl tyenv chng rn trm
  in 
  if !chng then (nt, nenv)
  else raise (termError "rewrite_net: no change" [trm])

let rewrite_net ctrl rn trm = 
  let ret, _= rewrite_net_env ctrl (Gtypes.empty_subst()) rn trm
  in ret
