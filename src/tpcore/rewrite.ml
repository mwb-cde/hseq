open Basic
open Term
open Result

type order = (Basic.term -> Basic.term -> bool)

type rule = 
    Rule of term
  | Ordered of (term * order)

(* Rule constructors *)
let rule t = Rule t
let orule t r = Ordered(t, r)

(* Rule destructors *)
let term_of r =
  match r with
    Rule t -> t
  | Ordered (t, _) -> t

let order_of r =
  match r with
    Rule _ -> raise (Failure "Not an ordered rule")
  | Ordered (_, p) -> p

(* Rewrite control *)

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
      depth: int option; (** (Some i): maximum number of times to rewrite is i,
			    None : unlimited rewriting (default) *)
      rr_dir: direction;
      rr_strat: strategy
    }

let control ~dir ~strat ~max = 
  { depth=max; rr_dir=dir; rr_strat=strat }

let default_control= 
  control 
    ~strat:TopDown
    ~dir:leftright
    ~max:None

let limit_reached d = 
  match d with Some 0 -> true | _ -> false

let decr_depth ctrl = 
  match ctrl.depth with 
    None -> ctrl 
  | Some x -> 
      {ctrl with depth=Lib.set_int_option(x-1)}

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
    | ((vs, key, rep, order)::rst) -> make_rewrites_aux rst
	  (Net.add (is_free_binder vs) net key (vs, key, rep, order))
  in 
  make_rewrites_aux (List.rev xs) (Net.empty())

(*
type rewrite_rules = (Basic.binders list * Basic.term * Basic.term)
*)
type rewrite_rules = 
    (Basic.binders list * Basic.term * Basic.term * order option)

type rewriteDB = 
    Net_rr of rewrite_rules Net.net 
  | List_rr of rewrite_rules list 

let find_match scope ctrl tyenv varp term1 term2 env=
  try 
    Unify.unify_fullenv_rewrite scope tyenv env varp term1 term2 
  with x -> 
    raise 
      (add_error (termError ("Can't match terms") [term1; term2]) x)

let match_rewrite scope ctrl tyenv varp lhs rhs order trm = 
  let env = Term.empty_subst ()
  in 
  try
    (let tyenv1, env1=find_match scope ctrl tyenv varp lhs trm env; 
    in 
    let nt = Term.subst env1 rhs
    in 
    match order with
      None -> (nt, tyenv1)
    | Some(p) ->  
	if (p nt trm)   (* if nt < trm *)
	then (nt, tyenv1) (* accept nt *) 
	else raise (Failure "No match")) (* reject nt *)
  with x -> 
    (Term.addtermError "match_rewrite: failed" [lhs; trm] x)

(* 
   rewriting with a choice of rewrites 
   rewrite_list_aux qs (t, rs) t2 
   replaces t with some r in rs in trm if matching in binders qs 
 *)
let rec match_rr_list scope ctrl tyenv chng rs trm = 
  if (limit_reached (ctrl.depth))
  then (trm, tyenv, ctrl)
  else 
    (match rs with
      [] -> (trm, tyenv, ctrl)
    | (qs, lhs, rhs, order)::nxt ->
	let (ntrm, ntyenv), fl = 
	  (try 
	    (match_rewrite scope ctrl tyenv 
	       (is_free_binder qs) lhs rhs order trm, 
	     true)
	  with _ -> (trm, tyenv), false)
	in 
	if fl 
	then (chng:=true; (ntrm, ntyenv, decr_depth ctrl))
	else 
	  match_rr_list scope ctrl ntyenv chng nxt trm)

let rec match_rewrite_list scope ctrl tyenv chng net trm =
  if(limit_reached ctrl.depth)
  then (trm, tyenv, ctrl)
  else 
    (let cn=ref false 
    in 
    let rs=
      match net with
	(Net_rr n) -> Net.lookup n trm
      | (List_rr r) -> r 
    in 
    let ntrm, ntyenv, nctrl =
      match_rr_list scope ctrl tyenv cn rs trm
    in 
    if (!cn) 
    then 
      (chng:=true; 
       match_rewrite_list scope nctrl ntyenv chng net ntrm)
    else
      (ntrm, ntyenv, nctrl))

let rewrite_list_topdown scope ctrl tyenv chng net trm = 
  let rec rewrite_subterm ctrl env t=
    if(limit_reached ctrl.depth)
    then (t, env, ctrl)
    else 
      (match t with
	Basic.Qnt(qnt, q, b) -> 
	  let nb, benv, bctrl = rewrite_aux ctrl env b
	  in 
	  (Basic.Qnt(qnt, q, nb), benv, bctrl)
      |	Basic.App(f, a)->
	  let nf, fenv, fctrl = (rewrite_aux ctrl env f)
	  in
	  let na, aenv, actrl = (rewrite_aux fctrl fenv a)
	  in 
	  (Basic.App(nf, na), aenv, actrl)
      | Basic.Typed(tt, ty) -> rewrite_aux ctrl env tt
      | _ -> (t, env, ctrl))
  and rewrite_aux ctrl env t = 
    if(limit_reached ctrl.depth)
    then (t, env, ctrl)
    else 
      (let nt, nenv, nctrl= match_rewrite_list scope ctrl env chng net t
      in 
      rewrite_subterm nctrl nenv nt)
  in 
  rewrite_aux ctrl tyenv trm

let rewrite_list_bottomup scope ctrl tyenv chng net trm = 
  let rec rewrite_aux ctrl env t=
    if(limit_reached (ctrl.depth))
    then (t, env, ctrl)
    else 
      (match t with
	Basic.Qnt(qnt, q, b) -> 
	  (let nb, benv, nctrl = rewrite_aux ctrl env b
	  in 
	  match_rewrite_list scope nctrl benv chng net (Qnt(qnt, q, nb)))
      |	Basic.App(f, a)->
	  (let nf, fenv, fctrl = 
	    (rewrite_aux ctrl env f)
	  in
	  let na, aenv, actrl= 
	    (rewrite_aux fctrl fenv a)
	  in 
	  match_rewrite_list scope actrl aenv chng net (Basic.App(nf, na)))
      | Basic.Typed(tt, ty) -> 
	  rewrite_aux ctrl env tt
      | _ -> 
	  match_rewrite_list scope ctrl env chng net t)
  in 
  rewrite_aux ctrl tyenv trm

(* using term nets of rewrites *)

let rewrite_list scope ctrl chng tyenv rs trm = 
  let nt=Net_rr(make_rewrites rs)
  in 
  let (nt, ntyenv, _) = 
    if(is_topdown (ctrl.rr_strat))
    then 
      rewrite_list_topdown scope ctrl tyenv chng nt trm
    else 
      rewrite_list_bottomup scope ctrl tyenv chng nt trm
  in (nt, ntyenv)

(*
   rewrite with equality eqtrm: "l=r" -> (l, r) 
   left-right if dir=true, right-left otherwise
 *)

let rewrite_eqs scope ctrl tyenv rrl trm =
  let chng = ref false
  in 
  let r = rewrite_list scope ctrl chng tyenv rrl trm;
  in 
  if !chng 
  then r
  else raise (termError "Matching" [trm])

(*
let rewrite_eqs scope ctrl tyenv rrl trm =
  let chng = ref false
  in 
  let r =
    if ctrl.rr_dir=LeftRight
    then rewrite_list scope ctrl chng tyenv
	(List.map (fun (qs, b)-> 
	  let (lhs, rhs) = Logicterm.dest_equality b 
	  in (qs, lhs, rhs)) rrl) trm
    else 
      rewrite_list scope ctrl chng tyenv
	(List.map (fun (qs, b) -> 
	  let (lhs, rhs)= Logicterm.dest_equality b 
	  in (qs, rhs, lhs)) rrl) trm
  in 
  if !chng 
  then r
  else raise (termError "Matching" [trm])
*)      

(*
   rewrite with equality term: "!x_1, ..., x_n. l=r" 
   -> [x_1, ..., x_n](l, r) 
   left-right if dir=true, right-left otherwise
 *)

(*
   [dest_lr_rule r]: Destruct for left to right rewriting.

   Break rule [t= !x1..xn: lhs = rhs] 
   into quantifiers [x1..xn], [lhs] and [rhs].

   return ([x1..xn], [lhs], [rhs], [p]).
   where [p] is [None] if [r=Rule t] and [Some x if r= Order(t, x)]
*)
let dest_lr_rule  r= 
  let dest_term x p=
    let qs, b = strip_qnt Basic.All x
    in 
    let lhs, rhs= Logicterm.dest_equality b
    in 
    (qs, lhs, rhs, p)
  in
  match r with
    Rule(t) -> dest_term t None
  | Ordered(t, x) -> dest_term t (Some x)

(*
   [dest_rl_term t]: Destruct for right to left rewriting.
   Break term [t= !x1..xn: lhs = rhs] 
   into quantifiers [x1..xn], [lhs] and [rhs].
   return ([x1..xn], [rhs], [lhs]).
*)
let dest_rl_rule r = 
  let dest_term x p=
    let qs, b = strip_qnt Basic.All x
    in 
    let lhs, rhs= Logicterm.dest_equality b
    in 
    (qs, rhs, lhs, p)
  in 
  match r with
    Rule(t) -> dest_term t None
  | Ordered(t, x) -> dest_term t (Some x)

let rewrite_env scope ctrl tyenv rrl trm=
  let rs = 
    if ctrl.rr_dir=LeftRight
    then 
      List.map dest_lr_rule rrl
    else 
      List.map dest_lr_rule rrl
  in 
  rewrite_eqs scope ctrl tyenv rs trm

let rewrite scope ctrl rrl trm =
  let (ret, _) = rewrite_env scope ctrl (Gtypes.empty_subst()) rrl trm
  in ret

(* rewrite_net *)

(*
let rewrite_net_env scope ctrl tyenv rn trm = 
  let chng = ref false
  in 
  let nt, nenv, _ = 
    if(is_topdown ctrl.rr_strat)
    then rewrite_list_topdown scope ctrl tyenv chng rn trm
    else rewrite_list_bottomup scope ctrl tyenv chng rn trm
  in 
  if !chng then (nt, nenv)
  else raise (termError "rewrite_net: no change" [trm])

let rewrite_net scope ctrl rn trm = 
  let ret, _= rewrite_net_env scope ctrl (Gtypes.empty_subst()) rn trm
  in ret
*)
