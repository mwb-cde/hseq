(*-----
 Name: formula.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Basic
exception Error


(** The type for formulas *)
type t =  { thy : Scope.marker; term : Basic.term }

let term_of x = x.term
(** Convert a formula to a term *)

let thy_of x = x.thy
(** Get the theory marker of a formula. *)


(***
* Error handling 
***)

class formError s ts =
  object (self)
    inherit Result.error s
    val forms = (ts : t list)
    method get() = forms
    method print st = 
      Format.printf "@[%s@ @[" (self#msg()); 
      Printer.print_sep_list 
	((fun f-> Term.print st (term_of f)), ",") (self#get());
      Format.printf "@]@]"
  end

let error s t = Result.mk_error((new formError s t):>Result.error)
let add_error s t es = raise (Result.add_error (error s t) es)


(***
* Conversion from a term
***)

(** 
   [mk_scoped_formula scp f]: make [f] a formula of [scp]. 
   Currently does nothing. Must not be exposed for general use.
*)
let mk_scoped_formula scp f = { thy = Scope.marker_of scp; term = f }

(** Convert a term to a formula *)

let resolve_term ?(strict=false) scp trm =
  Term.resolve_closed_term scp trm

let prepare ?(strict=false) scp tyenv trm =
  let t1, lst = Term.resolve_closed_term scp trm
  in 
    if (strict && not (lst=[])) 
    then
      raise
	(Term.term_error 
	   "Formula.make: Can't make formula, not a closed term" [trm])
    else 
      let tyenv1 = 
	try
	 Typing.typecheck_top scp tyenv t1 (Gtypes.mk_null())
	with x -> 
	  raise (Result.add_error x 
		   (Term.term_error "Formula.make: incorrect types" [t1]))
      in 
	(lst, t1, tyenv1)

let make_full ?(strict=false) scp tyenv t= 
  let (lst, t1, tyenv1) = prepare ~strict:strict scp tyenv t
  in
  let t2 = 
      List.fold_left 
	(fun b (q, _) -> Term.mk_qnt (Term.dest_bound q) b)
	t1 lst
  in 
    (mk_scoped_formula scp (Term.retype tyenv1 t2), tyenv1)

(*
  let t1=
    try Term.resolve_closed_term scp t
    with x -> raise
	(Result.add_error x
	   (Term.term_error 
	      "Formula.make: Can't make formula, not a closed term" [t]))
  in 
  try
    let tyenv1 = 
      Typing.typecheck_top scp tyenv t1 (Gtypes.mk_null())
    in 
    (mk_scoped_formula scp (Term.retype tyenv1 t1),
     tyenv1)
  with x -> 
    raise (Result.add_error x 
	     (Term.term_error "Formula.make: incorrect types" [t1]))
*)

let make scp ?tyenv t= 
  let env = 
    match tyenv with
      None -> Gtypes.empty_subst()
    | Some(x) -> x
  in 
  let (form, _) = make_full scp env t
  in 
  form

(*** Fast conversion to formulas for internal use ***)


(** 
   [mk_subterm_unsafe f t]: Make [t] a formula with the same theory
   marker as [f].  This is only safe if [t] is a subterm of [f] and
   [f] is not a quantifier.
*)
let mk_subterm_unsafe f t = {thy = thy_of f; term = t}

(**
   [mk_subterm f t]: Make [t] a formula with the same theory marker as
   [f]. Checks that [t] is a closed subterm of [f], fails otherwise. 
*)
let mk_subterm f t = 
  if (Term.is_subterm t (term_of f)) && (Term.is_closed [] t)
  then 
    {thy = thy_of f; term = t}
  else 
    raise (add_error "Can't make a formula as a subterm of formula" [f]
	     (Term.term_error "term isn't a subterm" [t]))

(**
   [formula_in_scope scp f]: true if formula [f] is in scope [scp].
*)   
let formula_in_scope scp f = Scope.in_scope_marker scp (thy_of f)

(**
   [valid_forms scp fs]: Return true if all formulas in [fs] are in
   scope [scp]. Return false otherwise. Used to test whether the
   formulas can be used e.g. with conjunction to make a new formula
   without the expense of going through [make].
*)
let valid_forms scp fs =
  let test() =
    List.iter 
      (fun f -> 
	if (formula_in_scope scp f) 
	then () else raise (error "invalid formula" [f])) fs
  in 
  try test(); true with _ -> false

(** 
   [fast_make scp fs t]: make a formula without any checks, if
   possible. This function must not be exposed for general use. It is
   only for use by the constructors.

   If [fs] are valid formulas then make [t] a formula of [scp] without
   doing any checks. Otherwise make [t] a formula using [make].
*)
let fast_make ?env scp fs t = 
  if (valid_forms scp fs) 
  then (mk_scoped_formula scp  t)
  else (make scp t)

(***
* Representation for permanent storage
***)

type saved_form =  Dbterm.dbterm
let to_save x =  Dbterm.of_term (term_of x)
let from_save scp x = make scp (Dbterm.to_term x)

(***
* Operations on formulas 
***)

let equals x y = Term.equals (term_of x) (term_of y)

(*** General tests ***)

let in_scope_memo memo scp f =
  if ((formula_in_scope scp f) || (Term.in_scope memo scp (term_of f)))
  then true
  else raise (Term.term_error "Badly formed formula" [term_of f])

let in_scope scp f =
  if ((formula_in_scope scp f) 
    || (Term.in_scope (Lib.empty_env()) scp (term_of f)))
  then true
  else raise (Term.term_error "Badly formed formula" [term_of f])

let is_fresh scp f = formula_in_scope scp f

(*** Recognisers ***)

let is_qnt x = Term.is_qnt (term_of x)
let is_app x= Term.is_app (term_of x)
let is_bound x= Term.is_bound (term_of x)
let is_free x = Term.is_free (term_of x)
let is_var x = Term.is_var (term_of x)
let is_typed x = Term.is_typed (term_of x)
let is_const x = Term.is_const (term_of x)
let is_fun x= Term.is_fun (term_of x)

let is_true x = Logicterm.is_true (term_of x)
let is_false x = Logicterm.is_false (term_of x)
let is_neg x = Logicterm.is_neg (term_of x)
let is_conj x = Logicterm.is_conj (term_of x)
let is_disj x = Logicterm.is_disj (term_of x)
let is_implies x = Logicterm.is_implies (term_of x)
let is_equality x = Logicterm.is_equality (term_of x)

let is_all x = Logicterm.is_all (term_of x)
let is_exists x = Logicterm.is_exists (term_of x)
let is_lambda x = Logicterm.is_lambda (term_of x)

(*** Destructors ***)

let dest_num x = Term.destnum (term_of x)
let dest_neg f = 
  if is_neg f
  then 
    match Term.dest_unop (term_of f) with 
    (_, x) -> mk_subterm_unsafe f x
  else raise (error "dest_neg" [f])

let dest_conj f = 
  if is_conj f
  then 
    match Term.dest_binop (term_of f) with 
    (_, a, b) -> (mk_subterm_unsafe f a, mk_subterm_unsafe f b)
  else raise (error "dest_conj" [f])

let dest_disj f = 
  if is_disj f
  then 
    match Term.dest_binop (term_of f) with 
      (_, a, b) -> (mk_subterm_unsafe f a, mk_subterm_unsafe f b)
  else raise (error "dest_disj" [f])

let dest_implies f = 
  if is_implies f
  then 
    match Term.dest_binop (term_of f)
    with (_, a, b) -> (mk_subterm_unsafe f a, mk_subterm_unsafe f b)
  else raise (error "dest_implies" [f])

let dest_equality f =  
  if is_equality f
  then match Term.dest_binop (term_of f) with 
    (_, a, b) -> (mk_subterm_unsafe f a, mk_subterm_unsafe f b)
  else raise (error "dest_equality" [f])

let get_binder_name x = Term.get_binder_name (term_of x)
let get_binder_type x = Term.get_binder_type (term_of x)

(*** Constructors ***)

let mk_true scp = make scp Logicterm.mk_true
let mk_false scp = make scp Logicterm.mk_false
let mk_bool scp b = if b then mk_true scp else mk_false scp

let mk_not scp f = 
  fast_make scp [f] (Logicterm.mk_not (term_of f))
let mk_and scp a b = 
  fast_make scp [a; b] (Logicterm.mk_and (term_of a) (term_of b))
let mk_or scp a b = 
  fast_make scp [a; b] (Logicterm.mk_or (term_of a) (term_of b))
let mk_implies scp a b = 
  fast_make scp [a; b] (Logicterm.mk_implies (term_of a) (term_of b))
let mk_iff scp a b = 
  fast_make scp [a; b] (Logicterm.mk_iff (term_of a) (term_of b))
let mk_equality scp a b = 
  fast_make scp [a; b] (Logicterm.mk_equality (term_of a) (term_of b))


(***
* Typechecking
***)

let typecheck_env scp tenv f expty = 
  let t = term_of f
  in 
  Typing.typecheck_top scp (Gtypes.empty_subst()) t expty

let typecheck scp f expty= 
  let t = term_of f
  in 
  let tyenv = typecheck_env scp (Gtypes.empty_subst()) f expty
  in 
  make scp (Term.retype_pretty tyenv t)

let retype scp tenv x = make scp (Term.retype tenv (term_of x))

let retype_with_check scp tenv f = 
  let nf = 
    try
      Term.retype_with_check scp tenv (term_of f)
    with err -> 
      raise (add_error "Formula.retype_with_check" [f] err)
  in 
  fast_make scp [f] nf

let typecheck_retype scp tyenv f expty=
  let tyenv1 = typecheck_env scp tyenv f expty
  in 
  try
    (retype_with_check scp tyenv1 f, tyenv1)
  with 
    err -> (add_error "Formula.typecheck_retype" [f] err)


(*** General Operations ***)

let rec is_closed scp env t =
  match t with
    Basic.App(l, r) -> 
      (is_closed scp env l && is_closed scp env r)
  | Basic.Typed(a, _) -> 
      is_closed scp env a
  | Basic.Qnt(q, b) -> 
      let env1 = 
	Term.bind 
	  (Basic.Bound(q)) 
	  (Term.mk_free "" (Gtypes.mk_null())) env
      in 
      is_closed scp env1 b
  | Basic.Meta (q) -> Scope.is_meta scp q
  | Basic.Bound(q) -> Term.member t env
  | Basic.Free(_) -> 
      Term.member t env
  | _ -> true

let rec subst_closed scp qntenv sb trm =
  try 
    let nt = Term.replace sb trm 
    in 
    if (is_closed scp qntenv nt)
    then subst_closed scp qntenv sb nt
    else raise (Failure "subst_closed: Not closed")
  with Not_found ->
    (match trm with
      Basic.Qnt(q, b) -> 
	let qntenv1 = 
	  Term.bind (Bound q) (Term.mk_free "" (Gtypes.mk_null())) qntenv
	in 
	Basic.Qnt(q, subst_closed scp qntenv1 sb b)
    | Basic.App(f, a) -> 
	Basic.App(subst_closed scp qntenv sb f, subst_closed scp qntenv sb a)
    | Basic.Typed(t, ty) -> Typed(subst_closed scp qntenv sb t, ty)
    | _ -> trm)

let subst scp form lst=
  let env = 
    List.fold_left 
      (fun e (t, r) -> Term.bind (term_of t) (term_of r) e) 
      (Term.empty_subst()) lst
  in 
  let nt = Term.subst env (term_of form)
  in 
  fast_make scp (List.map snd lst) nt

let subst_equiv scp form lst =
  let repl_list = 
    List.map (fun (t, r) -> ((term_of t), (term_of r))) lst
  in 
  let nt = Logicterm.subst_equiv scp (term_of form) repl_list
  in 
  fast_make scp (List.map snd lst) nt

let rename t = mk_subterm_unsafe t (Term.rename (term_of t))

let inst_env scp env f r =
  let t = term_of f
  and r1 = term_of r
  in 
  if (Term.is_qnt t) 
  then 
    try
      (let (q, b) = Term.dest_qnt t
      in 
      let t1= 
	  Term.subst
	    (Term.bind (Basic.Bound(q)) r1 (Term.empty_subst())) b
      in 
      let t2 = fast_make scp [f; r] t1
      in 
	typecheck_retype scp env t2 (Gtypes.mk_var "inst_ty"))
    with err -> raise (add_error "inst: " [r] err)
  else raise (error "inst: not a quantified formula" [f])

let inst scp t r =
  let f, _ = inst_env scp (Gtypes.empty_subst()) t r
  in f


(***
* Unification functions
***)

let unify scp asmf conclf =
  let asm = term_of asmf
  and concl = term_of conclf
  in 
  let (avars, abody)=Term.strip_qnt Basic.All asm
  and (cvars, cbody)= Term.strip_qnt Basic.Ex concl
  in let varp x = 
    (match x with 
      (Basic.Bound q) -> (List.memq q avars) or (List.memq q cvars)
    | _ -> false)
  in (Unify.unify scp varp abody cbody)

let unify_env scp tyenv asmf conclf =
  let asm = term_of asmf
  and concl = term_of conclf
  in 
  let (avars, abody)=Term.strip_qnt Basic.All asm
  and (cvars, cbody)= Term.strip_qnt Basic.Ex concl
  in let varp x = 
    (match x with 
      (Basic.Bound q) -> (List.memq q avars) or (List.memq q cvars)
    | _ -> false)
  in 
  Unify.unify_fullenv scp tyenv (Term.empty_subst()) varp abody cbody


(***
* Logic operations
***)

(*** Alpha conversion ***)

let alpha_equals scp x y = Logicterm.alpha_equals scp (term_of x) (term_of y)

let alpha_equals_match scp tyenv asmf conclf= 
  let asm = term_of asmf
  and concl = term_of conclf
  and varp x = false
  in 
  let ret, _ = 
    Unify.unify_fullenv scp tyenv (Term.empty_subst()) varp asm concl
  in ret

(*** Beta conversion ***)

let beta_convp x = Logicterm.beta_convp (term_of x)
let beta_conv scp x =  make scp (Logicterm.beta_conv (term_of x))
let beta_reduce scp x = make scp (Logicterm.beta_reduce (term_of x))

(** 
   [mk_beta_reduce_eq scp tyenv trm]: 
    Make an equality expressing the result of beta-reducing trm.
*)
let mk_beta_reduce_eq scp tyenv trm = 
  let (lhst, lst) = resolve_term scp trm
  in 
  let rhst = Logicterm.beta_reduce lhst
  in 
  let eqtrm = Logicterm.mk_equality lhst rhst
  in 
  let rtrm = 
      List.fold_left 
	(fun b (q, _) -> Term.mk_qnt (Term.dest_bound q) b)
	eqtrm lst
  in make_full scp tyenv rtrm

(*
let mk_beta_reduce_eq scp tyenv trm = 
  let (lhsf, tyenv1) = make_full scp tyenv trm
  in 
  let rhsf = fast_make scp [lhsf] (Logicterm.beta_reduce trm)
  in 
    (mk_equality scp lhsf rhsf, tyenv1)
*)


(*** Eta conversion ***)

let eta_conv scp f ty x = 
  make scp (Logicterm.eta_conv (term_of f) ty (term_of x))


(***
* Rewriting
***)

let default_rr_control= Rewrite.default_control

(*** Rewriting functions ***)

let rec extract_check_rules scp dir pl = 
  let get_test x = 
    if (formula_in_scope scp x)
    then 
      let t = term_of x
      in 
      let qs, b = Term.strip_qnt Basic.All t
      in 
      let lhs, rhs = Logicterm.dest_equality b
      in 
      if dir = Rewrite.leftright 
      then (qs, lhs, rhs)
      else (qs, rhs, lhs)
    else
      raise (error "Rewrite rule not in scope" [x])
  in 
  Rewrite.mapping get_test pl

let rewrite_env scp ?(dir=Rewrite.leftright) tyenv plan f = 
  let plan1 = extract_check_rules scp dir plan
  in 
  let data = (scp, Term.empty_subst(), tyenv)
  in 
  let (data1, nt) = 
    try (Rewrite.rewrite data plan1 (term_of f))
    with 
      Rewritekit.Quit err -> raise err
    | Rewritekit.Stop err -> raise err
    | err -> raise err
  in 
  let (scp1, qntenv1, tyenv1) = data1
  in 
  (fast_make scp [f] nt, tyenv1)


let rewrite scp ?(dir=Rewrite.leftright) plan f = 
  let (nt, ntyenv) = 
    rewrite_env scp ~dir:dir (Gtypes.empty_subst()) plan f
  in 
  nt

(** 
   [mk_rewrite_eq scp tyenv plan trm]: Make an equality by rewriting a
   term w.r.t a type context.  Returns [(trm=t, ntyenv)] where [t] is
   the result of rewriting [trm] with [plan] and [ntyenv] is the type
   environment generated during rewriting.
*)
let mk_rewrite_eq scp tyenv plan trm = 
  let plan1 = extract_check_rules scp Rewrite.leftright plan
  in 
  let (lhst, lst) = resolve_term scp trm
  in 
  let data = (scp, Term.empty_subst(), tyenv)
  in 
  let (data1, nt) = 
    try (Rewrite.rewrite data plan1 trm)
    with 
      Rewritekit.Quit err -> raise err
    | Rewritekit.Stop err -> raise err
    | err -> raise err
  in 
  let (scp1, _, tyenv2) = data1
  in 
  let eqtrm =  Logicterm.mk_equality lhst nt
  in 
  let rtrm =
    List.fold_left 
      (fun b (q, _) -> Term.mk_qnt (Term.dest_bound q) b)
      eqtrm lst
  in 
    make_full scp tyenv2 rtrm 

(*
let mk_rewrite_eq scp tyenv plan trm = 
  let plan1 = extract_check_rules scp Rewrite.leftright plan
  in 
  let (lhsf, tyenv1) = make_full scp tyenv trm
  in 
  let data = (scp, Term.empty_subst(), tyenv1)
  in 
  let (data1, nt) = 
    try (Rewrite.rewrite data plan1 trm)
    with 
      Rewritekit.Quit err -> raise err
    | Rewritekit.Stop err -> raise err
    | err -> raise err
  in 
  let (scp1, _, tyenv2) = data1
  in 
  let rhsf = fast_make scp1 [lhsf] nt
  in 
  (mk_equality scp lhsf rhsf, tyenv2)
*)


(***
* Pretty printing
***)

let print inf x = Term.print inf (term_of x)

let string_form x = Term.string_term (term_of x)

(***
* Miscellaneous
***)
let rec check_term p t =
  if (p t) then 
    (match t with
      Basic.Qnt(q, b) -> check_term p b
    | Basic.App(f, a) -> check_term p f; check_term p a
    | Basic.Typed(trm, ty) -> check_term p trm
    | _ -> ())
  else raise (Term.term_error "Term check failed" [t])
