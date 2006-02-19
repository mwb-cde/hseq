(*-----
 Name: nums.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(**
   Conversion of a term involving boolean and number expressions only
   to a boolexpr.
*)

let error s l = Term.term_error s l
let add_error s l err = Term.add_term_error s l err


(* Term identifiers *)
let num_thy = Logicterm.nums_thy

let bool_type = Logicterm.mk_bool_ty()
let num_type = Logicterm.mk_num_ty()

let plusid = Ident.mk_long num_thy "plus"
let minusid = Ident.mk_long num_thy "minus"
let multid = Ident.mk_long num_thy "mult"
let negid = Ident.mk_long num_thy "negate"
let maxid = Ident.mk_long num_thy "max"
let minid = Ident.mk_long num_thy "min"

let gtid = Ident.mk_long num_thy "greater"
let geqid = Ident.mk_long num_thy "geq"
let ltid = Ident.mk_long num_thy "less"
let leqid = Ident.mk_long num_thy "leq"

let rec strip_typed trm = 
  match trm with 
    Basic.Typed(t, _) -> strip_typed t
  | _ -> trm


(* Variable environment *)

type varenv = (int * (int ref *  Basic.term)list)
let new_env () = (0, [])
let var_ctr (n, _) = n
let var_env (_, e) = e
let get_var (n, e) t =
  fst(List.find (fun (_, x) -> Term.equals t x) e)
let get_index (n, e) i =
    try snd(List.nth e (i-1))
    with _ -> raise Not_found
let add_var (n, e) t = 
  let t1 = strip_typed t
  in 
  try (get_var (n, e) t1, (n, e))
  with Not_found -> (n+1, (n+1, (n+1, t)::e))


(*
   [is_equals scp ty f a b]: test whether [f] is the equality function
   for arguments [a] and [b] of type [ty].
*)
let typing_typecheck scp x ty =
    ignore (Typing.typecheck_top scp (Gtypes.empty_subst()) x ty)

let is_equals scp ty f a b = 
  if f=Logicterm.equalsid
  then 
    try 
      (typing_typecheck scp a ty;
       typing_typecheck scp b ty; 
       true)
    with _ -> false
  else false

let is_num_equals scp f a b = 
  is_equals scp num_type f a b

let is_bool_equals scp f a b = 
  is_equals scp bool_type f a b

(**
   [has_num_type scp trm]: term [trm] has type [num_type].

   [has_bool_type scp trm]: term [trm] has type [bool_type].
*)
let has_num_type scp trm = 
  try (typing_typecheck scp trm num_type; true)
  with _ -> false

let has_bool_type scp trm = 
  try (typing_typecheck scp trm bool_type; true)
  with _ -> false

(**
   [strip_univs scp bvar_env var_env t]:
   Strip universal quantifiers from term [t].

   Return 
   [qnts]: the bound variables
   [body]: the body of the term
   [bqnts]: the quantified variables of boolean type.
   [nqnts]: the quantified variables of num type.
*)
let strip_univs scp benv nenv t =
  let (rqnts, b) = Term.strip_qnt Basic.All t
  in 
  let add_qntvar (benv1, nenv1) x =
    let nx=Basic.Bound(x)
    in 
    (try 
      (typing_typecheck scp nx (num_type);
       (benv1, snd(add_var nenv1 nx)))
    with _ ->
      (try
	(typing_typecheck scp nx (bool_type);
	 (snd(add_var benv1 nx), nenv1))
      with _ -> 
	raise (error "Badly formed expression" [t])))
  in
  let benv2, nenv2= List.fold_left add_qntvar (benv, nenv) rqnts;
  in 
  (rqnts, b, benv2, nenv2)


(* Constructors for integer expressions *)

let mk_plus x = Exprs.Plus x
let mk_max x = Exprs.Max x
let mk_min x = Exprs.Min x

let mk_mult x = 
  match x with
    [a] -> Exprs.Mult (Num.num_of_int 1, a)
  | [Exprs.Val(a); b] -> Exprs.Mult(a, b)
  | [a; Exprs.Val(b)] -> Exprs.Mult(b, a)
  | _ -> raise (error "mult: Badly formed integer expression" [])

let mk_minus x = 
  match x with 
    [a] -> Exprs.Mult(Num.minus_num Exprs.one_num, a)
  | [a; b] -> mk_plus [a; Exprs.Mult(Num.minus_num Exprs.one_num, b)]
  | _ -> raise (error "minus: Badly formed integer expression" [])

let mk_negate x = 
  match x with
    [a] -> Exprs.Mult (Num.num_of_int (-1), a)
  | _ -> raise (error "negate: Badly formed integer expression" [])

(* Identifiers constructing integer functions *)
let num_fns =
  [ plusid, mk_plus;  
    minusid, mk_minus; 
    multid, mk_mult; 
    negid, mk_negate; 
    maxid, mk_max; 
    minid, mk_min]
    
let is_num_fn x = List.mem_assoc x num_fns
let get_num_fn x = List.assoc x num_fns  

(* Conversion from an integer term to an expression *)
let numterm_to_expr var_env scp t =
  let rec conv_aux env x =
    match x with
      Basic.Id(f, ty) -> 
	  (typing_typecheck scp x (num_type);
	   let c, env1=add_var env x
	   in 
	   (Exprs.Var(c), env1))
    | Basic.Free(n, ty) -> 
	  (typing_typecheck scp x (num_type);
	   let c, env1=add_var env x
	   in 
	   (Exprs.Var(c), env1))
    | Basic.Typed(y, ty) -> conv_aux env y
    | Basic.Const(Basic.Cnum(i)) -> (Exprs.Val(i), env)
    | Basic.Const(_) -> 
	raise (error "Badly formed expression: not a number" [x])
    | Basic.Qnt(_) -> raise (error "Badly formed expression: quantifier" [x])
    | Basic.Bound(_) ->
	  (typing_typecheck scp x (num_type);
	   let c, env1=add_var env x
	   in 
	   (Exprs.Var(c), env1))
(*
	(try (Exprs.Var(get_var env x), env)
	with Not_found -> 
	  raise (error "Badly formed expression: unknown bound variable" [x]))
*)
    | Basic.App(_, _) ->
	let f, args = Term.dest_fun x
	in 
	let cnstr = 
	  try get_num_fn f
	  with Not_found -> 
	    raise (error "Badly formed expression: Unknown function" [x])
	in 
	let args1, env1= conv_args env args []
	in 
	try
	  (let rst = cnstr args1
	  in 
	  (rst, env1))
	with _ -> 
	  let c, env2 = add_var env x
	  in 
	  (Exprs.Var(c), env2)
  and 
      conv_args env args rst =
    match args with
      [] -> (List.rev rst, env)
    | (x::xs) -> 
	let nx = 
	  try(Some (conv_aux env x)) 
	  with _ -> None
	in 
	match nx with
	  None -> 
	    let c, env1 = add_var env x
	    in 
	    conv_args env1 xs (Exprs.Var(c)::rst)
	| Some(t, env1) -> 
	    conv_args env1 xs (t::rst)
  in 
  let (nume, var_env1) = conv_aux var_env t
  in 
  (nume, var_env1)

(* expr_to_numterm : conversion from expressions to number terms *)
let expr_to_numterm env t =
  let rec conv_aux e =
    match e with
      Exprs.Val(v) -> Term.mk_num(Num.integer_num v)
    | Exprs.Var(i) -> get_index env i
    | Exprs.Plus(args) ->
	Term.mk_fun plusid (List.map conv_aux args)
    | Exprs.Mult(v, arg) ->
	Term.mk_fun multid 
	  [(Term.mk_num(Num.integer_num v)); conv_aux arg]
    | Exprs.Max(args) ->
	Term.mk_fun maxid (List.map conv_aux args)
    | Exprs.Min(args) ->
	Term.mk_fun minid (List.map conv_aux args)
    | _ -> raise (error "Invalid expression" [])
  in conv_aux t


(* Constructors for comparison expressions *)

let mk_num_equals a b = Prop.mk_bexpr(Supinf.Equals, a, b)
let mk_gt a b = Prop.mk_bexpr(Supinf.Gt, a, b)
let mk_geq a b = Prop.mk_bexpr(Supinf.Geq, a, b)
let mk_lt a b = Prop.mk_bexpr(Supinf.Lt, a, b)
let mk_leq a b = Prop.mk_bexpr(Supinf.Leq, a, b)

(* Identifiers constructing integer functions *)
let comp_fns =
  [ gtid, mk_gt; 
    geqid, mk_geq;
    ltid, mk_lt;
    leqid, mk_leq]
    
let is_comp_fn x = List.mem_assoc x comp_fns
let get_comp_fn scp f a b = 
  try 
    List.assoc f comp_fns  
  with Not_found ->
    (if is_equals scp num_type f a b
    then mk_num_equals
    else raise Not_found)

(* Convert from a comparison term to a comparsn *)
let compterm_to_comprsn env scp fnid args=
  match args with
    [a; b] -> 
      (try 
	(let cnstr = get_comp_fn scp fnid a b
	in 
      	let (na, env1) = numterm_to_expr env scp a
	in 
	let (nb, nenv) = numterm_to_expr env1 scp b
	in (cnstr na nb, nenv))
      with Not_found -> 
	raise (error "Unknown comparison function" [Term.mk_fun fnid args]))
  | _ -> 
      raise (error "Badly formed expression" [Term.mk_fun fnid args])


(* comprsn_to_term: conversion expression to comparison *)
let comprsn_to_term env cfn a b =
  match cfn with
    Supinf.Equals -> 
      Term.mk_fun 
	Logicterm.equalsid 
	[(expr_to_numterm env a); (expr_to_numterm env b)]
  | Supinf.Leq -> 
      Term.mk_fun leqid 
	[(expr_to_numterm env a); (expr_to_numterm env b)]
  | Supinf.Lt -> 
      Term.mk_fun ltid
	[(expr_to_numterm env a); (expr_to_numterm env b)]
  | Supinf.Gt -> 
      Term.mk_fun gtid
	[(expr_to_numterm env a); (expr_to_numterm env b)]
  | Supinf.Geq -> 
      Term.mk_fun geqid
	[(expr_to_numterm env a); (expr_to_numterm env b)]


(* Constructors for propositions *)
let mk_not x =
  match x with
    [a] -> Prop.mk_not a
  | _ -> raise (error "Badly formed expression" [])

let mk_and x =
  match x with
    [a; b] -> Prop.mk_and a b
  | _ -> raise (error "Badly formed expression" [])

let mk_or x =
  match x with
    [a; b] -> Prop.mk_or a b
  | _ -> raise (error "Badly formed expression" [])

let mk_implies x =
  match x with
    [a; b] -> Prop.mk_implies a b
  | _ -> raise (error "Badly formed expression" [])

let mk_iff x =
  match x with
    [a; b] -> Prop.mk_iff a b
  | _ -> raise (error "Badly formed expression" [])

let mk_bool_equals x =
  match x with
    [a; b] -> Prop.mk_equals a b
  | _ -> raise (error "Badly formed expression" [])

(* Identifiers constructing propositions *)
let bool_fns =
  [ Logicterm.notid, mk_not;
    Logicterm.andid, mk_and;
    Logicterm.orid, mk_or;
    Logicterm.iffid, mk_iff;
    Logicterm.impliesid, mk_implies]
    
let is_bool_fn x = List.mem_assoc x bool_fns
let get_bool_fn scp f args = 
  try 
    List.assoc f bool_fns  
  with Not_found ->
    (match args with
      [a;b] ->
    	if is_equals scp bool_type f a b
    	then mk_bool_equals
    	else raise Not_found
    | _ -> raise Not_found)


(*
  [ bterm_to_prop scp bvar_env nvar_env t]
   benv : boolean variables
   nenv: integer variables
   scp : scope
   t: term 
*)
let bterm_to_prop scp bvar_env nvar_env t =
  let rec conv_aux x benv nenv=
    match x with
      Basic.Id(f, ty) -> 
	(typing_typecheck scp x (bool_type);
	 let c, benv1=add_var benv x
	 in 
	 (Prop.Var(c), benv1, nenv))
    | Basic.Free(n, ty) -> 
	(typing_typecheck scp x (bool_type);
	 let c, benv1=add_var benv x
	 in 
	 (Prop.Var(c), benv1, nenv))
    | Basic.Typed(y, ty) -> conv_aux y benv nenv
    | Basic.Qnt(_) -> raise (error "Badly formed expression" [x])
    | Basic.Const(Basic.Cbool(b)) -> 
	if b 
	then (Prop.mk_true(), benv, nenv) 
	else (Prop.mk_false(), benv, nenv) 
    | Basic.Const(_) -> 
	raise (error "Badly formed expression (Const)" [x])
    | Basic.App(_, args) ->
	let f, args = Term.dest_fun x
	in 
	let ret = 
	  try 
	    let ne, nenv1 = (compterm_to_comprsn nenv scp f args)
	    in 
	    Some(ne, benv, nenv1)
	  with _ -> None
	in 
	(match ret with
	  Some x -> x
	| None -> 
	    let mk_rel_constr = 
	      try Some(get_bool_fn scp f args)
	      with Not_found -> None
	    in 
	    (match mk_rel_constr with
	      Some(mk_er) -> 
		let mk_er= get_bool_fn scp f args
		in 
		let cargs, benv1, nenv1 = conv_args args benv nenv []
		in 
		mk_er cargs, benv1, nenv1
	    | None -> 
		raise (error "Badly formed expression:" [x])))
    | Basic.Bound(b) ->
	(typing_typecheck scp x (bool_type);
	 let c, benv1=add_var benv x
	 in 
	 (Prop.Var(c), benv1, nenv))

(*
	let (q, _, qty) = Basic.dest_binding b
	in 
	(match q with
	    Basic.All -> 
		(typing_typecheck scp x (bool_type);
		 let c, benv1=add_var benv x
		 in 
		 (Prop.Var(c), benv1, nenv))
	  | _ -> raise (error "Badly formed expression (Bound)" [x]))
*)
  and 
      conv_args ls benv nenv rst =
    match ls with 
      [] -> (List.rev rst, benv, nenv)
    | (x::xs) -> 
	let x1, benv1, nenv1 = conv_aux x benv nenv
	in 
	conv_args xs benv1 nenv1 (x1::rst)
  in 
  (conv_aux t bvar_env nvar_env)

(* [prop_to_bterm benv nenv e]: conversion from proposition to a term *)
let prop_to_bterm benv nenv e =
  let rec conv_aux t =
    match t with
      Prop.Bool(b) -> 
	if b then Logicterm.mk_true else Logicterm.mk_false
    | Prop.Not(a) -> Logicterm.mk_not (conv_aux a)
    | Prop.And(a, b) -> 
	Logicterm.mk_and (conv_aux a) (conv_aux b)
    | Prop.Or(a, b) -> 
	Logicterm.mk_or (conv_aux a) (conv_aux b)
    | Prop.Implies(a, b) -> 
	Logicterm.mk_implies (conv_aux a) (conv_aux b)
    | Prop.Iff(a, b) -> 
	Logicterm.mk_iff (conv_aux a) (conv_aux b)
    | Prop.Equals(a, b) -> 
	Logicterm.mk_equality (conv_aux a) (conv_aux b)
    | Prop.Bexpr(c, a, b) -> 
	comprsn_to_term nenv c a b
    | Prop.Var(i) -> get_index benv i
  in conv_aux e


(**
   [term_to_prop scp t]: toplevel conversion from term [t] to a
   proposition.  Strip universal quantifiers from [t], create
   proposition in resulting variable environment.
*)
let term_to_prop scp t =
  let (rqnts, nb, benv, nenv) =
    strip_univs scp (new_env()) (new_env()) t
  in 
  let (e, nbenv, nnenv) = bterm_to_prop scp benv nenv nb
  in 
  (e, nbenv, nnenv, rqnts)


(**
   [prop_to_term scp benv nenv rqnts e]: Convert proposition [e] to a
   term using environements [benv] and [nenv] to substitute terms for
   variables.
*)
(*
let prop_to_term scp benv nenv rqnts e =
  let rec rebuild x ls=
    match ls with
      [] -> x
    | q::qs -> rebuild (Basic.Qnt(q, x)) qs
  in 
  rebuild (prop_to_bterm benv nenv e) rqnts
*)
let prop_to_term scp benv nenv rqnts e =
  Term.rebuild_qnt rqnts (prop_to_bterm benv nenv e)

(* simp_term_basic: simplify a term involving only integer functions *)

let simp_term_basic scp t=
  let (e, env) = numterm_to_expr (new_env()) scp t
  in 
  let ne=Exprs.Poly.simp e
  in 
  expr_to_numterm env ne

	
(**
   simp_rewrite: 
   build a rewrite rule for simplifying the given formula
   return the rule as an axiom
*)
let reduce_conv scp trm =
  let nt = simp_term_basic scp trm
  in 
  Logic.mk_axiom 
    (Formula.make scp 
       (Logicterm.gen_term [] (Logicterm.mk_equality trm nt)))

(*
let reduce_conv scp trm =
  let nt = simp_term_basic scp trm
  in 
  Logic.mk_axiom 
    (Formula.make scp (Logicterm.close_term (Logicterm.mk_equality trm nt)))
*)

(*
let simp_rewrite scp f =
  let t =(Formula.term_of f)
  in 
  let nt = simp_term_basic scp t
  in 
  Logic.mk_axiom 
    (Formula.make scp (Logicterm.close_term (Logicterm.mk_equality t nt)))
let simp_conv scp trm = 
  let nt = simp_term_basic scp trm
  in 
  Logic.mk_axiom (Formula.make scp nt)
*)

(**
   [decide_term_basic]:
   Decide whether a term is true, false or undecidable 
*)
let decide_term_basic scp t =
  let (e, _, _, _) = term_to_prop scp t
  in 
  try
    Supinf.decide e
  with 
    Supinf.Unknown -> 
      raise (error "Numbers: Undecidable term" [t])
  | Invalid_argument x -> 
      raise (add_error "Invalid term" [t] (Invalid_argument x))

(*
let decide_term_basic scp t =
  let (e, _, _, _) = term_to_prop scp t
  in 
  try
    Supinf.decide e
  with 
    Supinf.Possible_solution _ -> 
      raise (error "Numbers: Undecidable term" [t])
  | Invalid_argument x -> 
      raise (add_error "Invalid number term" [t] (Invalid_argument x))
*)

(**
   [decide_conv scp t]: 
   decide whether term [t] is true, false or undecidable 
   return [t=true], [t=false] or raise an exception
*)
let decide_conv scp trm = 
  let result = 
    if(decide_term_basic scp trm)
    then Logicterm.mk_true
    else Logicterm.mk_false
  in 
  let ntrm=
    Logicterm.gen_term [] (Logicterm.mk_equality trm result)
  in 
  Logic.mk_axiom (Formula.make scp ntrm)

(*
let decide_term scp t =
  Logicterm.close_term 
    (Logicterm.mk_equality t (Logicterm.mk_bool (decide_term_basic scp t)))
*)
(**
   [decide_rewrite scp f]: Decide whether formula [f] is true, false
   or undecidable return the rule as an axiom or raise an exception
*)
(*
let decide_rewrite scp f =
  let nt = Formula.term_of f
  in 
  Logic.mk_axiom (Formula.make scp (decide_term scp nt))
*)


(* Recognisers *)

(** 
   [is_num_app scp f args]: term [f args] is a number term.

   [is_comp_app scp f args]: term [f args] is a comparison term.

   [is_bool_app scp f args]: term [f args] is a boolean term.
*)
let is_num_app scp f args = 
  (is_num_fn f)

let is_comp_app scp f args = 
  ((is_comp_fn f) ||
  (match args with
    [a; b] -> is_num_equals scp f a b
  | _ -> false))

let is_bool_app scp f args = 
  ((is_bool_fn f) ||
  (match args with
    [a; b] -> is_bool_equals scp f a b
  | _ -> false))

(**
   [is_numterm scp t]: [true] if term [t] is a number, an integer variable
   or the application of an integer function (for which [is_num_fn] is [true])
   to numterms.

   Used to see whether term [t] is suitable for simplifiying (with
   Exprs.simp).

   [is_numterm => not (is_compterm)]
   [is_numterm => not (is_boolterm)]
   [is_numterm => is_presburger]
*)
let rec is_numterm scp trm = 
  match trm with
    Basic.Typed(t, _) -> is_numterm scp t
  | Basic.Const(Basic.Cnum _) -> true
  | Basic.Const _ -> false
  | Basic.Bound _ -> has_num_type scp trm
  | Basic.Free _ -> has_num_type scp trm
  | Basic.Id _ -> has_num_type scp trm
  | Basic.App _ -> 
      let f, args = Term.dest_fun trm 
      in 
      if (is_num_app scp f args)
      then 
	List.fold_left 
	  (fun a t -> a && (is_numterm scp t)) true args
      else false
  | Basic.Qnt _ -> false

(**
   [is_compterm scp t]: [true] if term [t] is a comparison between
   number terms.

   Used to see whether term [t] is suitable for simplifiying (with
   Exprs.simp).
*)
let rec is_compterm scp trm = 
  match trm with
    Basic.Typed(t, _) -> is_compterm scp t
  | Basic.Bound _ -> false
  | Basic.Free _ -> false
  | Basic.Id _ -> false
  | Basic.Const _ -> false
  | Basic.App _ -> 
      let f, args = Term.dest_fun trm 
      in 
      if(is_comp_app scp f args)
      then 
	List.fold_left 
	  (fun a t -> a && (is_numterm scp t)) true args
      else false
  | Basic.Qnt _ -> false

(**
   [is_boolterm scp t]: [true] if term [t] is a quantifier-free
   boolean term in which subterms satisfy either [is_boolterm] or
   [is_compterm] and at least one subterm satisfies [is_compterm].

   Used to see whether term [t] is suitable for the decision procedure
   Supinf.decide.
*)
let rec is_boolterm scp trm = 
  match trm with
    Basic.Typed(t, _) -> is_boolterm scp t
  | Basic.Const _ -> has_bool_type scp trm
  | Basic.Bound _ -> has_bool_type scp trm
  | Basic.Free _ -> has_bool_type scp trm
  | Basic.Id _ -> has_bool_type scp trm
  | Basic.App _ -> 
      let f, args = Term.dest_fun trm 
      in 
      if is_bool_app scp f args
      then 
	List.fold_left 
	  (fun a t -> a && (is_boolterm scp t)) true args
      else 
	is_compterm scp trm
  | Basic.Qnt _ -> false

(**
   [is_presburger scp t]: [true] if term [t] is a number term
   satisfying [is_numterm] or a (possibly universally quantified)
   boolean term in which subterms are quantifier-free boolean terms
   satisfying [is_boolterm]

   Used to see whether term [t] is suitable for the decision procedure.
*)
let is_presburger scp trm = 
  let qnts, body = Term.strip_qnt Basic.All trm
  in 
  ((not (Logicterm.is_true trm))
    && (not (Logicterm.is_false trm))
    && (is_boolterm scp body))

(** 
   [arith_conv scp trm]: Use the decision procedure to simplify term [trm].
   
   Fail, if [trm] doesn't satisfy [is_presburger].
*)
let arith_conv scp trm = 
  let do_error t err= 
    add_error "Nums.arith_conv: Invalid term" [t] err
  in 
  if(is_numterm scp trm)
  then 
    try
      reduce_conv scp trm
    with err -> raise (do_error trm err)
  else 
    try
      decide_conv scp trm
    with err -> raise (do_error trm err)

(**
   Conversions for use with the simplifier.

   [simp_reduce_conv scp asms trm]: call [reduce_conv] with [trm], 

   [simp_decide_conv scp asms trm]: call [decide_conv] with [asms => trm], 
   pulling topmost quantifiers of term to the outside.

   [simp_arith_conv scp asms trm]: call [arith_conv] with [asms => trm], 
   pulling topmost quantifiers of term to the outside.
*)
let simp_reduce_conv scp asms trm=
  reduce_conv scp trm

let simp_decide_conv scp asms trm=
  let asms1 = List.filter (is_presburger scp) asms
  in 
  let ntrm = 
    match asms1 with
      [] -> trm
    | (x::xs) ->
	  let qnts, body = Term.strip_qnt Basic.All trm
	  in 
	  let asm = List.fold_left Logicterm.mk_and x xs
	  in 
	  Term.rebuild_qnt qnts
	    (Logicterm.mk_implies asm body)
  in 
  if(is_presburger scp trm)
  then 
    try(decide_conv scp ntrm)
    with err -> raise (add_error "simp_decide_conv: failed" [ntrm] err)
  else 
    raise (error "simp_decide_conv: Invalid term" [ntrm])

let simp_arith_conv scp asms trm = 
  if(is_numterm scp trm)
  then 
    try
      simp_reduce_conv scp asms trm
    with err -> 
      raise (add_error "simp_arith_convs: failed" [trm] err)
  else 
    try
      simp_decide_conv scp [] trm
    with err -> 
      raise (add_error "simp_arith_convs: failed" [trm] err)
