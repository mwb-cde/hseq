open Basic
open Term
open Result


let varp, funp, constp, is_app, is_typed=
  Term.is_bound, Term.is_fun, Term.is_const, Term.is_app, Term.is_typed
let qntp t = Term.is_qnt t

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

let find_match_aux scp tyenv varp term1 term2 env=
  try 
    Unify.unify_fullenv_rewrite scp tyenv env varp term1 term2 
  with x -> 
    raise 
      (add_error (mktermError ("Can't match terms") [term1; term2]) x)

let find_match scp tyenv varp term1 term2 env=
  let tyenv1, env1= find_match_aux scp tyenv varp  term1 term2 env
  in 
  let s= Term.chase varp term1 env1
  and t=term2
  in 
  Gtypes.unify scp (Typing.typeof scp s) (Typing.typeof scp t)

(*
   let match_rewrite scp tyenv varp lhs rhs trm = 
   let tenv = Term.empty_subst ()
   in 
   try
   (ignore(find_match_aux scp tyenv varp lhs trm tenv); 
   Term.subst_env tenv rhs)
   with x -> 
   (Term.addtermError "match_rewrite: failed" [lhs; trm] x)
 *)      

let match_rewrite scp tyenv varp lhs rhs trm = 
  let env = Term.empty_subst ()
  in 
  try
    (let tyenv1, env1=find_match_aux scp tyenv varp lhs trm env; 
    in 
    Term.subst env1 rhs, tyenv1)
  with x -> 
    (Term.addtermError "match_rewrite: failed" [lhs; trm] x)

(* 
   rewriting with a choice of rewrites 
   rewrite_list_aux qs (t, rs) t2 
   replaces t with some r in rs in trm if matching in binders qs 
 *)

let is_free_binder qs t= 
  (match t with
    Bound(q) -> List.exists (fun x ->  x == q) qs
  |	_ -> false)

(* convert list of equalities to db of rewrites *)

let rec make_rewrites_aux xs net=
  match xs with
    [] -> net
  | ((vs, key, rep)::rst) -> make_rewrites_aux rst
	(Net.add (is_free_binder vs) net key (vs, key, rep))
(*
	(Net.enter (is_free_binder vs) (key, (vs, key, rep)) net)
*)

let make_rewrites xs = make_rewrites_aux (List.rev xs) (Net.empty())

(* find list of possible rewrites in list *)

(*
   let find_rewrite_opts t rs =  rs

   let rec find_first bs =
   match bs with 
   [] -> []
   | ((q, b, c)::nxt) -> if (similar b t) then bs else  find_first nxt
   and drop_last bs =
   match bs with 
   [] -> []
   | ((q, b, c)::nxt) -> 
   if (similar b t) then (q, b, c)::(drop_last nxt) else []
   in drop_last (find_first rs)
 *)	


type rewrite_rules = (Basic.binders list * Basic.term * Basic.term)
type rewriteDB = 
    Net_rr of rewrite_rules Net.net 
  | List_rr of rewrite_rules list


let rec match_rr_list scp tyenv chng rs trm = 
  match rs with
    [] -> (trm, tyenv)
  | (qs, lhs, rhs)::nxt ->
      let (ntrm, ntyenv), fl = 
	(try 
	  match_rewrite scp tyenv (is_free_binder qs) lhs rhs trm, true
	with _ -> (trm, tyenv), false)
      in 
      if fl 
      then (chng:=true; (ntrm, ntyenv))
      else 
	match_rr_list scp ntyenv chng nxt trm

let rec match_rewrite_list scp tyenv chng net trm =
  let cn=ref false 
  in 
  let rs=
    match net with
      (Net_rr n) -> Net.lookup n trm
    | (List_rr r) -> r
  in 
  let ntrm, ntyenv=match_rr_list scp tyenv cn rs trm
  in 
  if (!cn) 
  then 
    (chng:=true; 
     match_rewrite_list scp ntyenv chng net ntrm)
  else
    ntrm, ntyenv

let rewrite_list_aux scp tyenv chng net trm = 
  let rec rewrite_aux env t=
    match t with
      Basic.Qnt(qnt, q, b) -> 
	(let nb, benv = rewrite_aux env b
	in 
	match_rewrite_list scp benv chng net (Qnt(qnt, q, nb)))
    | Basic.Typed(tt, ty) -> 
	rewrite_aux env tt
    |	Basic.App(f, a)->
	(let nf, fenv = 
	  (rewrite_aux env f)
	in
	let na, aenv = 
	  (rewrite_aux fenv a)
	in 
	match_rewrite_list scp aenv chng net (Basic.App(nf, na)))
    | _ -> 
	match_rewrite_list scp env chng net t
  in 
  rewrite_aux tyenv trm

(*  try
   rewrite_aux trm
   with x->
   (Term.addtermError "rewrite failed for term" [trm] x)
 *)
(* using term nets of rewrites *)

let rewrite_list scp chng tyenv rs trm = 
  let nt=Net_rr(make_rewrites rs)
  in 
  rewrite_list_aux scp tyenv chng nt trm

(*
   let rec rewrite_list_aux  scp tyenv chng net trm = 
   try
   match trm with
   Basic.Qnt(q, b) -> 
   (let nb = rewrite_list_aux scp tyenv chng net b
   in 
   match_rewrite_list scp tyenv chng net (Qnt(q, nb)))
   | Basic.Typed(tt, ty) -> 
   rewrite_list_aux scp tyenv chng net tt
   |	Basic.App(f, a)->
   let nf = 
   (rewrite_list_aux scp tyenv chng net f)
   and na = 
   (rewrite_list_aux scp tyenv chng net a)
   in 
   (match_rewrite_list scp tyenv chng net (Basic.App(nf, na)))
   | _ -> (match_rewrite_list scp tyenv chng net trm)
   with x -> 
   (Term.addtermError "rewrite failed for term" [trm] x)

 *)

(* (using lists of rewrite)
   let rewrite_list scp chng rs trm = 
   rewrite_list_aux scp (Gtypes.empty_subst()) chng (List_rr rs) trm
 *)

let rewrite_net scp rn trm = 
  let chng = ref false
  in 
  let nt, _ = rewrite_list_aux scp (Gtypes.empty_subst()) chng rn trm
  in 
  if !chng then nt 
  else raise (termError "rewrite_net: no change" [trm])

let rewrite_net_env scp tyenv rn trm = 
  let chng = ref false
  in 
  let nt, nenv = rewrite_list_aux scp tyenv chng rn trm
  in 
  if !chng then (nt, nenv)
  else raise (termError "rewrite_net: no change" [trm])


(*
   let rewrite_bound chng scp qs t r trm = 
   rewrite_bound_aux (chng, Term.empty_subst()) scp qs t r trm
 *)

(* rewrite with equality eqtrm: "l=r" -> (l, r) 
   left-right if dir=true, right-left otherwise
 *)

let rewrite_eqs scp dir tyenv rrl trm =
  let chng = ref false
  in 
  let r =
    if dir
    then rewrite_list scp chng tyenv
	(List.map (fun (qs, b)-> 
	  let (lhs, rhs) =  
	    Logicterm.dest_equal b in (qs, lhs, rhs)) rrl) trm
    else rewrite_list scp chng tyenv
	(List.map 
	   (fun (qs, b) -> 
	     let (lhs, rhs)=
	       Logicterm.dest_equal b in (qs, rhs, lhs)) rrl) trm
  in 
  if !chng 
  then r
  else raise (termError "Matching" [trm])
      

(* simple rewriting: rewrite topdown once only  *)

(*
let simple_rewrite_list scp chng rs trm = 
  let tyenv = Gtypes.empty_subst()
  in 
  let rec rewrite_aux tr = 
    match tr with
      Basic.Typed(tt, ty) -> 
	rewrite_aux tt
    | Basic.Qnt(q, b) ->
	let ntrm = match_rr_list scp tyenv chng rs tr
	in 
	if (!chng) 
	then 
	  ntrm
	else 
	  (let nb= rewrite_aux b
	  in 
	  if(!chng)
	  then Basic.Qnt(q, nb)
	  else 
	    raise (Term.termError "No match for rewrite" [tr]))
    | _ -> match_rr_list scp tyenv chng rs tr
  in 
  try
    rewrite_aux trm
  with x -> (Term.addtermError "rewrite failed for term" [trm] x)
*)

let simple_rewrite_list scp chng tyenv rs trm = 
  let rec rewrite_aux env tr = 
    match tr with
      Basic.Typed(tt, ty) -> 
	rewrite_aux env tt
    | Basic.Qnt(qnt, q, b) ->
	let ntrm, nenv = match_rr_list scp env chng rs tr
	in 
	if (!chng) 
	then (ntrm, nenv)
	else 
	  (let nb, benv= rewrite_aux nenv b
	  in 
	  if(!chng)
	  then (Basic.Qnt(qnt, q, nb), benv)
	  else 
	    raise (Term.termError "No match for rewrite" [tr]))
    | _ -> match_rr_list scp env chng rs tr 
  in 
  try
    rewrite_aux tyenv trm
  with x -> (Term.addtermError "rewrite failed for term" [trm] x)

let simple_rewrite_eqs scp dir tyenv rrl trm =
  let chng = ref false
  in 
  let r, ntyenv =
    if dir
    then simple_rewrite_list scp chng tyenv
	(List.map (fun (qs, b)-> 
	  let (a, b) =  Logicterm.dest_equal b in (qs, a, b)) rrl) 
	trm
    else simple_rewrite_list scp chng tyenv
	(List.map 
	   (fun (qs, b) -> 
	     let (a, b)=Logicterm.dest_equal b in (qs, b, a)) rrl)
	trm
  in 
  if !chng 
  then r, ntyenv
  else raise (termError "Matching" [trm])


(* rewrite with equality term: "!x_1, ..., x_n. l=r" 
   -> [x_1, ..., x_n](l, r) 
   left-right if dir=true, right-left otherwise
 *)

let rewrite_univs scp ?(dir=true) ?(simple=false) rrl trm=
  let rs = List.map (strip_qnt Basic.All) rrl
  and tyenv = Gtypes.empty_subst()
  in 
  let nt, ntyenv= 
    if simple 
    then simple_rewrite_eqs scp dir tyenv rs trm
    else rewrite_eqs scp dir tyenv rs trm
  in nt

let rewrite_univ scp ?(dir=true) ?(simple=false) rr trm = 
  rewrite_univs scp ~dir:dir ~simple:simple[rr] trm

let rewrite_univs_env scp ?(dir=true) ?(simple=false) tyenv rrl trm=
  let rs = List.map (strip_qnt Basic.All) rrl
  in 
  let nt, ntyenv=
    if simple 
    then simple_rewrite_eqs scp dir tyenv rs trm
    else rewrite_eqs scp dir tyenv rs trm
  in (nt, ntyenv)

let rewrite_univ_env scp ?(dir=true) ?(simple=false) tyenv rr trm = 
  rewrite_univs_env scp ~dir:dir ~simple:simple tyenv [rr] trm


