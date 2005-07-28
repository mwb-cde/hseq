(*-----
 Name: formula.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Basic
exception Error


(** The type for formulas *)
type form =  { thy : Scope.marker; term : Basic.term }

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
    val forms = (ts :form list)
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


(*** Fast conversion to formulas for internal use ***)


(** 
   [mk_subterm f t]: Make [t] a formula with the same theory marker as [f].
   This is only safe if [t] is a subterm of [f] and [f] is not a quantifier.
*)
let mk_subterm f t = {thy = thy_of f; term = t}

(**
   [formula_in_scope scp f]: true if formula [f] is in scope [scp].

   In the current implementation, this always returns [false].
   Future implementations will hopefully do something useful here.

let formula_in_scope scp f = false
*)   
let formula_in_scope scp f = Scope.in_scope_marker scp (thy_of f)

(**
   [valid_forms scp fs]: Return true if all formulas in [fs] are in
   scope [scp]. Return false otherwise. Used to test whether the
   formulas can be used e.g. with conjunction to make a new formula
   without the expense of going through [make].

let valid_forms scp fs = false 
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
  then mk_scoped_formula scp t 
  else make scp t

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
    (_, x) -> mk_subterm f x
  else raise (error "dest_neg" [f])

let dest_conj f = 
  if is_conj f
  then 
    match Term.dest_binop (term_of f) with 
    (_, a, b) -> (mk_subterm f a, mk_subterm f b)
  else raise (error "dest_conj" [f])

let dest_disj f = 
  if is_disj f
  then 
    match Term.dest_binop (term_of f) with 
      (_, a, b) -> (mk_subterm f a, mk_subterm f b)
  else raise (error "dest_disj" [f])

let dest_implies f = 
  if is_implies f
  then 
    match Term.dest_binop (term_of f)
    with (_, a, b) -> (mk_subterm f a, mk_subterm f b)
  else raise (error "dest_implies" [f])

let dest_equality f =  
  if is_equality f
  then match Term.dest_binop (term_of f) with 
    (_, a, b) -> (mk_subterm f a, mk_subterm f b)
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


(*** General Operations ***)

(*
let inst_env scp env f r =
  let t = term_of f
  in 
  if (Term.is_qnt t) 
  then 
    try
      (let (q, b) = Term.dest_qnt (term_of t)
      in 
      let r1, nenv=
	let penv = ref env
	in 
	let r1=make ~env:penv scp r
	in 
	(r1, !penv)
      in 
      let t1= Term.subst_quick (Basic.Bound(q)) (term_of r1) b
      in 
      let t2 = Term.retype nenv t1
      in 
      (fast_make scp [f] t2, nenv))
    with err -> 
      raise
	(add_error "inst: replacement not closed " [r] err)
  else raise (error "inst: not a quantified formula" [t])
*)

let inst_env scp env f r =
  let t = term_of f
  and r1 = term_of r
  in 
  if (Term.is_qnt t) 
  then 
    try
      (let (q, b) = Term.dest_qnt t
      in 
      let t1= Term.subst_quick (Basic.Bound(q)) r1 b
      in 
      let t2 = Term.retype env t1
      in 
      (fast_make scp [f; r] t2, env))
    with err -> 
      raise
	(add_error "inst: replacement not closed " [r] err)
  else raise (error "inst: not a quantified formula" [f])

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

let rename t = mk_subterm t (Term.rename (term_of t))


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

(*** Eta conversion ***)

let eta_conv scp f ty x = 
  make scp (Logicterm.eta_conv (term_of f) ty (term_of x))


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

let string_form x = Term.string_term (term_of x)

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
