(*-----
 Name: formula.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Basic
exception Error


(** The type for formulas *)
type form =  Basic.term

(***
* Conversion to and from a term
***)

(** 
   [mk_scoped_formula scp f]: make [f] a formula of [scp]. 
   Currently does nothing. Must not be exposed for general use.
*)
let mk_scoped_formula scp f = f

(** Convert a term to a formula *)
let make ?env scp t= 
  let t1=
    try Term.resolve_closed_term scp t
    with x -> raise
	(Result.add_error x
	   (Term.term_error 
	      "Formula.make: Can't make formula, not a closed term" [t]))
  in 
  try
    let tyenv = Lib.apply_option (fun x -> !x) env (Gtypes.empty_subst())
    in 
    let tyenv1 = 
      Typing.typecheck_top scp tyenv t1 (Gtypes.mk_null())
    in 
    mk_scoped_formula scp (Term.retype tyenv1 t1)
  with x -> 
    raise (Result.add_error x 
	     (Term.term_error "Formula.make: incorrect types" [t1]))

(** Convert a formula to a term *)
let term_of x = x

(*** Fast conversion to formulas for internal use ***)

(**
   [formula_in_scope scp f]: true if formula [f] is in scope [scp].

   In the current implementation, this always returns [false], forcing
   Future implementations will hopefully do something useful here.
*)   
let formula_in_scope scp f = false

(**
   [valid_forms scp fs]: Return true if all formulas in [fs] are in
   scope [scp]. Return false otherwise. Used to test whether the
   formulas can be used e.g. with conjunction to make a new formula
   without the expense of going through [make].

let valid_forms scp fs =
  List.fold_left (fun b f -> b && formula_in_scope scp f) true fs
*)
let valid_forms scp fs = false 

(** 
   [fast_make scp fs t]: make a formula without any checks, if
   possible. This function must not be exposed for general use. It is
   only for use by the constructors.

   If [fs] are valid formulas then make [t] a formula of [scp] without
   doing any checks. Otherwise make [t] a formula using [make].
*)
let fast_make ?env scp fs t = 
  if (valid_forms scp fs) 
  then mk_scoped_formula scp t 
  else make scp t

(***
* Representation for permanent storage
***)

type saved_form =  Dbterm.dbterm
let to_save x =  Dbterm.of_term (term_of x)
let from_save x = Dbterm.to_term (term_of x)

(***
* Operations on formulas 
***)

let equals x y = Term.equals (term_of x) (term_of y)

(*** General tests ***)

let in_scope_memo memo scp f =
  if (Term.in_scope memo scp (term_of f))
  then true
  else raise (Term.term_error "Badly formed formula" [term_of f])

let in_scope scp f =
  if (Term.in_scope (Lib.empty_env()) scp (term_of f))
  then true
  else raise (Term.term_error "Badly formed formula" [term_of f])

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
  then match Term.dest_unop f with (_, x) -> x
  else raise (Term.term_error "dest_neg" [f])

let dest_conj f = 
  if is_conj f
  then match Term.dest_binop f with (_, a, b) -> (a, b)
  else raise (Term.term_error "dest_conj" [f])

let dest_disj f = 
  if is_disj f
  then match Term.dest_binop f with (_, a, b) -> (a, b)
  else raise (Term.term_error "dest_disj" [f])

let dest_implies f = 
  if is_implies f
  then match Term.dest_binop f with (_, a, b) -> (a, b)
  else raise (Term.term_error "dest_implies" [f])

let dest_equality f =  
  if is_equality f
  then match Term.dest_binop f with (_, a, b) -> (a, b)
  else raise (Term.term_error "dest_equality" [f])

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


(*** General Operations ***)

let inst_env scp env t r =
  if (Term.is_qnt t) 
  then 
    try
      (let (q, b) = Term.dest_qnt (term_of t)
      in 
      let nr0, nenv=
	let penv = ref env
	in 
	let r1=make ~env:penv scp r
	in 
	(r1, !penv)
      in 
      let nr= Term.subst_quick (Basic.Bound(q)) nr0 b
      in 
      let f =Term.retype nenv nr
      in 
      (f, nenv))
    with err -> 
      raise
	(Term.add_term_error "inst: replacement not closed " [r] err)
  else raise (Term.term_error "inst: not a quantified formula" [t])

let inst scp t r =
  let f, _ = inst_env scp (Gtypes.empty_subst()) t r
  in f

let subst scp lst form = 
  let env = 
    List.fold_left 
      (fun e (t, r) -> Term.bind (term_of t) (term_of r) e) 
      (Term.empty_subst()) lst
  in 
  let nt = Term.subst env (term_of form)
  in 
  fast_make scp (List.map snd lst) nt

let rename t = Term.rename (term_of t)


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
* Typechecking
***)

let typecheck_env scp tenv f expty = 
  let t = term_of f
  in 
  Typing.typecheck_top scp (Gtypes.empty_subst()) t expty

let typecheck scp f expty= 
  let t = term_of f
  in 
  let tyenv = typecheck_env scp (Gtypes.empty_subst()) t expty
  in 
  make scp (Term.retype_pretty tyenv t)

let retype scp tenv x = make scp (Term.retype tenv (term_of x))

let retype_with_check scp tenv f = 
  let nf = 
    try
      Term.retype_with_check scp tenv (term_of f)
    with err -> 
      raise (Term.add_term_error "Formula.retype_with_check" [term_of f] err)
  in 
  fast_make scp [f] nf

let typecheck_retype scp tyenv f expty=
  let tyenv1 = typecheck_env scp tyenv f expty
  in 
  try
    (retype_with_check scp tyenv1 f, tyenv1)
  with 
    err -> (Term.add_term_error "Formula.typecheck_retype" [term_of f] err)

(***
* Logic operations
***)

(*** Alpha conversion ***)

let alpha_equals x = Logicterm.alpha_equals (term_of x)

let alpha_equals_match scp tyenv asmf conclf= 
  let asm = term_of asmf
  and concl = term_of conclf
  and varp x = false
  in 
  let ret, _ = 
    Unify.unify_fullenv scp tyenv (Term.empty_subst()) varp asm concl
  in ret

(*** Beta conversion ***)

let beta_convp = Logicterm.beta_convp
let beta_conv scp x =  make scp (Logicterm.beta_conv x)
let beta_reduce scp x = make scp (Logicterm.beta_reduce x)

(*** Eta conversion ***)

let eta_conv scp f ty x = 
  make scp (Logicterm.eta_conv f ty x)


(***
* Rewriting
***)

type rule = 
    Rule of form
  | Ordered of (form * Rewrite.order)


(* Rule constructors *)
let rule t = Rule t
let orule t r = Ordered(t, r)

(* Conversions *)
let rule_to_form r = 
  match r with
    Rule f -> f
  | Ordered (f, p) -> f

let to_rewrite_rule r = 
  match r with
    Rule f -> Rewrite.rule (term_of f)
  | Ordered (f, p) -> Rewrite.orule (term_of f) p

let default_rr_control= Rewrite.default_control

(*** Rewriting functions ***)

(** 
   Split a list [rs] of rules into a list [fs] of formulas and a list
   [rrs] of Rewrite.rule. The list [rrs] is in the same order as
   [rrs]. The list [fs] is in arbitrary order.
   In particular [rrs= List.map to_rewrite_rules rs].
*)
let rec split_rules rs (fs, rrs) = 
  match rs with
    [] -> (fs, List.rev rrs)
  | x::xs -> split_rules xs ((rule_to_form x)::fs, (to_rewrite_rule x)::rrs)

let rewrite scp ?ctrl rules t = 
  let c = Lib.get_option ctrl default_rr_control
  in 
  let (fs, rrs) = split_rules rules ([], [])
  in 
  let nt = Rewrite.rewrite scp c rrs (term_of t)
  in 
  fast_make scp fs nt

let rewrite_env scp ?ctrl tyenv rules t = 
  let c = Lib.get_option ctrl default_rr_control
  in 
  let (fs, rrs) = split_rules rules ([], [])
  in 
  let nt, ntyenv = 
    Rewrite.rewrite_env scp c tyenv rrs (term_of t)
  in 
  (fast_make scp fs nt, ntyenv)


(***
* Pretty printing
***)

let print inf x = Term.print inf (term_of x)

let string_form = Term.string_term

(***
* Miscelaneous
***)
let rec check_term p t =
  if (p t) then 
    (match t with
      Basic.Qnt(q, b) -> check_term p b
    | Basic.App(f, a) -> check_term p f; check_term p a
    | Basic.Typed(trm, ty) -> check_term p trm
    | _ -> ())
  else raise (Term.term_error "Term check failed" [t])
