open Basic
exception Error

type form =  Term.term
type saved_form =  Dbterm.dbterm

exception TermCheck of Term.term

let term_of_form x = x
let string_form = Term.string_term

let to_save x =  Dbterm.of_term x
let from_save x = Dbterm.to_term x

let rec check_term p t =
  if (p t) then 
    (match t with
      Term.Qnt(q, b) -> check_term p b
    | Term.App(f, a) -> check_term p f; check_term p a
    | Term.Typed(trm, ty) -> check_term p trm
    | _ -> ())
  else raise (Term.termError "Term check failed" [t])

let dest_form  x =  x

(* Substitution primitives *)

type substitution = Term.substitution
let empty_subst () = Term.empty_subst()

let find x subst = Term.find x subst
let remove x subst = Term.remove x subst
let chase = Term.chase
let fullchase = Term.fullchase
let replace x subst = Term.replace x subst

(* closed terms and conversion to formula *)

let rec is_closed_scope env t =
  match t with
    Term.App(l, r) -> is_closed_scope env l; is_closed_scope env r
  | Term.Typed(a, _) -> is_closed_scope env a
  | Term.Qnt(q, b) -> 
      Term.table_add (Term.Bound(q)) (Term.mkbool true) env;
      is_closed_scope env b;
      Term.table_remove (Term.Bound(q)) env
  | Term.Bound(q) -> 
      (try ignore(Term.table_find t env)
      with Not_found -> 
	raise (Term.termError  "Not closed"  [t]))
  | _ -> ()

let is_closed vs t = 
  let tbl=Term.empty_table()
  in 
  (* add bound terms of [vs] to tbl *)
  List.iter 
    (fun x -> 
      if(Term.is_bound x) 
      then ignore(Term.table_add x (Term.mkbool true) tbl)
      else ()) vs;
  try is_closed_scope tbl t; true
  with _ -> false

(* is_form scp t: check that [t] is a closed formula in scope [scp] *)
(* term is in scope if names and types are valid in [scp]*)

let is_form scp t =
  let memo = Lib.empty_env()
  and tyidmemo = Lib.empty_env()
  and tymemo = Gtypes.empty_subst()
  and qnt_env = Term.empty_table()
  in 
  let lookup_id x =
    (try ignore(Lib.find x memo)
    with Not_found ->
      (ignore(scp.Gtypes.typeof_fn x); ignore(Lib.add x true memo)))
  and lookup_ty x =
    (try ignore(Gtypes.lookup x tymemo)
    with Not_found ->
      (Gtypes.quick_well_defined scp tyidmemo x))
  in 
  let rec in_scope x=
    match x with
      Term.Var(id, ty) -> 
	let th, n = Basic.dest_fnid id
	in 
	ignore(lookup_id id); ignore(lookup_ty ty)
    | Term.Qnt(q, b) -> 
	lookup_ty (Term.get_binder_type x);
	ignore(Term.table_add (Term.Bound(q)) (Term.mkbool true) qnt_env);
	in_scope b;
	Term.table_remove (Term.Bound(q)) qnt_env
    | Term.Bound(q) -> 
	ignore(Term.table_find t qnt_env)
    | Term.App(l, r) -> in_scope l; in_scope r
    | Term.Typed(tt, ty) ->
	lookup_ty ty; in_scope tt
    | _ -> ()
  in 
  try 
    in_scope t
  with Not_found -> raise (Term.termError "Badly formed formula" [t])


let in_thy_scope_memo memo scp th f =
  Term.in_thy_scope memo scp th (term_of_form f)

let in_thy_scope scp th f =
  Term.in_thy_scope (Lib.empty_env()) scp th (term_of_form f)

let retype tenv x = Typing.retype tenv x

let mk_form scp t= 
  if is_closed [] t 
  then 
    let nt=Term.set_names scp t
    in 
    (try
      let env = Typing.settype scp nt
      in Typing.retype_pretty env nt
    with x -> Term.addtermError "mk_form: incorrect types" [nt] x)
  else raise (Term.termError "mk_form: Not a closed term" [t])


let form_of_term scp t0 = 
  let t=Term.set_names scp t0
  in 
  let tenv = 
    Typing.typecheck_env scp (Gtypes.empty_subst()) t (Gtypes.mk_null())
  in 
  Typing.retype_pretty tenv t

(* Formula recognisers, constructors and destructors *)

let is_fun= Term.is_fun
let mk_fun = Term.mkfun
let dest_fun = Term.dest_fun

let is_var  = Term.is_var
let mk_typed_var = Term.mk_typed_var
let mkvar n = mk_typed_var n (Gtypes.mk_null())
let dest_var  =  Term.dest_var 
let get_var_id vt= fst (dest_var vt)
let get_var_type vt= snd (dest_var vt)

let is_app = Term.is_app 
let mk_app f a= (Term.App(f, a))
let dest_app t = 
  match t with 
    (Term.App(f, a)) -> (f, a)
  | _ -> raise (Term.termError "Not an application" [t])

let rec mk_comb x y = 
  match y with 
    [] -> x
  | t::ts -> mk_comb (mk_app x t) ts

let is_const = Term.is_const 
let mk_const = Term.mkconst
let dest_const =  Term.dest_const

let mk_num = Term.mknum
let dest_num = Term.destnum 

let mk_bool = Term.mkbool
let dest_bool = Term.destbool 

let is_true x= try dest_bool x with _ -> false

let is_false x = try not (dest_bool x) with _ -> false

let is_fun_name s t =
  try (Term.is_fun t) & (fst(Term.dest_fun t) = s)
  with _ -> false

let is_neg = Logicterm.is_neg 
let mk_neg  = Logicterm.mknot
let dest_neg f = 
  if is_neg f
  then match dest_fun f with (_, x) -> x
  else raise (Term.termError "dest_neg" [f])
      
let is_conj = Logicterm.is_conj 
let mk_conj = Logicterm.mkand
let dest_conj f = 
  if is_conj f
  then match dest_fun f with (_, x) -> x
  else raise (Term.termError "dest_conj" [f])

let is_disj = Logicterm.is_disj 
let mk_disj = Logicterm.mkor
let dest_disj f = 
  if is_disj f
  then match dest_fun f with (_, x) -> x
  else raise (Term.termError "dest_disj" [f])

let is_implies = Logicterm.is_implies 
let mk_implies = Logicterm.mkimplies
let dest_implies f = 
  if is_implies f
  then match dest_fun f with (_, x) -> x
  else raise (Term.termError "dest_implies" [f])

let is_equals = Logicterm.is_equal 
let mk_equals  = Logicterm.mkequal
let dest_equals f =  
  if is_equals f
  then match dest_fun f with 
    (_, [a; b]) -> (a, b)
  |	_ -> raise (Term.termError "dest_equals" [f])
  else raise (Term.termError "dest_equals" [f])

(*
   if is_equals f
   then match dest_fun f with (_, x) -> x
   else raise (Failure "dest_equals")
 *)
let get_binder_name = Term.get_binder_name 
let get_binder_type = Term.get_binder_type 

let dest_qnt f = 
  let (q, _, _, _, b) = Term.dest_qnt f
  in (q, b)

let is_all = Logicterm.is_all 
let mk_all scp x f = 
  form_of_term scp (Logicterm.mkall scp x  f)
let mk_typed_all scp x ty f= 
  form_of_term scp (Logicterm.mkall_ty scp x ty f)

let is_exists = Logicterm.is_exists 
let mk_exists = Logicterm.mkex
let mk_typed_exists = Logicterm.mkex_ty 

let is_qnt t = (is_all t) or (is_exists t)

let is_lambda = Logicterm.is_lambda
let mk_lambda = Logicterm.mklam
let mk_typed_lambda = Logicterm.mklam_ty

(*
   let is_neg t = is_fun_name "not" t
   let mk_neg t = mk_fun "not" [t]
   let dest_neg f = 
   if is_neg f
   then match dest_fun f with (_, x) -> x
   else raise (Failure "dest_neg")
   
   let is_conj t = is_fun_name "and" t
   let mk_conj a b = mk_fun "and" [a; b]
   let dest_conj f = 
   if is_conj f
   then match dest_fun f with (_, x) -> x
   else raise (Failure "dest_conj")

   let is_disj t = is_fun_name "or" t
   let mk_disj a b = mk_fun "or" [a; b]
   let dest_disj f = 
   if is_disj f
   then match dest_fun f with (_, x) -> x
   else raise (Failure "dest_disj")

   let is_implies t = is_fun_name "implies" t
   let mk_implies a b = mk_fun "implies" [a; b]
   let dest_implies f = 
   if is_implies f
   then match dest_fun f with (_, x) -> x
   else raise (Failure "dest_implies")

   let is_equals t = is_fun_name "equals" t
   let mk_equals a b = mk_fun "equals" [a; b]
   let dest_equals f = 
   if is_equals f
   then match dest_fun f with (_, x) -> x
   else raise (Failure "dest_equals")

   let get_binder_name = Term.get_binder_name
   let get_binder_type = Term.get_binder_type


   let is_all = Term.is_all
   let mk_all = Term.mkall
   let mk_typed_all = Term.mkall_ty 

   let is_exists = Term.is_exists
   let mk_exists = Term.mkex
   let mk_typed_exists = Term.mkex_ty 

   let is_qnt t = (is_all t) or (is_exists t)


   let is_lambda = Term.is_lambda
   let mk_lambda = Term.mklam
   let mk_typed_lambda = Term.mklam_ty

 *)
(* Typecheck and reset types of formula *)

let typecheck_env scp tenv f expty = 
  let t = term_of_form f
  in 
  Typing.typecheck_env scp (Gtypes.empty_subst()) t expty

let typecheck scp f expty= 
  let tyenv = typecheck_env scp (Gtypes.empty_subst()) f expty
  in 
  Typing.retype_pretty tyenv f 

let simple_typecheck scp f expty= 
  ignore(typecheck_env scp (Gtypes.empty_subst()) f expty)

let mkiff = Logicterm.mkiff

let unify scp asmf conclf =
  let asm = term_of_form asmf
  and concl = term_of_form conclf
  in 
  let (avars, abody)=Term.strip_qnt Basic.All asm
  and (cvars, cbody)= Term.strip_qnt Basic.Ex concl
  in let varp x = 
    (match x with 
      (Term.Bound q) -> (List.memq q avars) or (List.memq q cvars)
    | _ -> false)
  in (Unify.unify scp varp abody cbody)

let unify_env scp tyenv asmf conclf =
  let asm = term_of_form asmf
  and concl = term_of_form conclf
  in 
  let (avars, abody)=Term.strip_qnt Basic.All asm
  and (cvars, cbody)= Term.strip_qnt Basic.Ex concl
  in let varp x = 
    (match x with 
      (Term.Bound q) -> (List.memq q avars) or (List.memq q cvars)
    | _ -> false)
  in 
  Unify.unify_fullenv scp tyenv (Term.empty_subst()) varp abody cbody


(* manipulation *)

(* substitution: term substitution followed by check for closed term *)

let subst scp env t = 
  let nt = Term.subst env (term_of_form t)
  in 
  form_of_term scp nt

let rename t = Term.rename t


let inst_env scp vs env t r =
  if (Term.is_qnt t) 
  then 
    if (is_closed vs r)
    then 
      (let (q, qnt, n, ty, b) = Term.dest_qnt (term_of_form t)
      in 
      let nr0 = Typing.assign_types scp r
      in 
      let nenv = Typing.simple_typecheck_env scp env nr0 ty
      in 
      let nr= Term.subst_quick (Term.Bound(q)) nr0 b
      in 
      let f =Typing.retype nenv nr
      in 
      (f, nenv))
    else raise (Term.termError "inst: replacement not closed " [r])
  else raise (Term.termError "inst: not a quantified formula" [t])

let inst scp vs t r =
  let f, _ = inst_env scp vs (Gtypes.empty_subst()) t r
  in f

(*
let inst scp vs t r =
  if (Term.is_qnt t) 
  then 
    if (is_closed vs r)
    then 
      (let (q, qnt, n, ty, b) = Term.dest_qnt t
      in 
      let nr0 = Typing.assign_types scp r
      in let nr = 
	Typing.retype 
	  (Typing.simple_typecheck_env scp (Gtypes.empty_subst()) nr0 ty) nr0
      in 
      let f= (Term.subst_quick (Term.Bound(q)) nr b)
      in 
      f)
    else raise (Term.termError "inst: replacement not closed " [r])
  else raise (Term.termError "inst: not a quantified formula" [t])
*)


let equals = Term.equals

let alpha_convp = Logicterm.alpha_convp 
let alpha_equals = Logicterm.alpha_convp 

let beta_convp = Logicterm.beta_convp
let beta_conv scp x =  form_of_term scp (Logicterm.beta_conv x)
let beta_reduce scp x = form_of_term scp (Logicterm.beta_reduce x)

let eta_conv scp f ty x = 
  form_of_term scp (Logicterm.eta_conv f ty x)

(* Rewriting: normal rewrite followed by check for close_term *)

let rewrite scp ?(dir=true) rrs t = 
  let nt = Rewrite.rewrite_univs ~dir:dir scp rrs t
  in 
  form_of_term scp nt

let rewrite_simple scp ?(dir=true) rrs t = 
  let nt = Rewrite.rewrite_univs ~dir:dir ~simple:true scp rrs t
  in 
  form_of_term scp nt

let rewrite_env scp ?(dir=true) tyenv rrs t = 
  let nt, ntyenv = 
    Rewrite.rewrite_univs_env ~dir:dir scp tyenv rrs t
  in 
  (form_of_term scp nt, ntyenv)

let rewrite_simple_env scp ?(dir=true) tyenv rrs t = 
  let nt, ntyenv = 
    Rewrite.rewrite_univs_env ~dir:dir ~simple:true scp tyenv rrs t
  in 
  (form_of_term scp nt, ntyenv)

(* Rewriting with nets *)

type rulesDB = Rewrite.rewriteDB

let empty_db th = Net.empty()

(* dir = true for right-to-left, false for left-to-right rewriting *)

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

let add scp dir fs (t, net) =
  let rs=List.map (dest_rr dir) fs
  in 
  let nnet =   
    (List.fold_left 
       (fun n (qs, a, b) -> 
	 Net.add (is_free_binder qs) n a (qs, a, b)) net rs)
      	 (* Net.enter (is_free_binder qs) (a, (qs, a, b)) n) net rs*)

  in nnet

let rewrite_net scp  rrnet f =
  let nt = Rewrite.rewrite_net scp (Rewrite.Net_rr rrnet) (term_of_form f)
  in form_of_term scp nt

let rewrite_net_env scp tyenv rrnet f =
  let nt, ntyenv = 
    Rewrite.rewrite_net_env scp tyenv 
      (Rewrite.Net_rr rrnet) (term_of_form f)
  in (form_of_term scp nt, ntyenv)


(*    let print_formlist = Term.print_termlist*)
let print inf x = Term.print inf (term_of_form x)


