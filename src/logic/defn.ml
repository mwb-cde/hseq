(*-----
   Name: defn.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

open Basic

  
(*
   Term definitions
 *)
(*
type decln = (Basic.ident* Basic.gtype)
type defn = Defn of (Basic.ident* Basic.gtype * Logic.thm)

let dest_decln (id, ty) = id, ty
let dest_defn (Defn x) = x
*)

let rec mk_all_from_list scp b qnts =
  match qnts with 
    [] -> b
  | (Basic.Free(n, ty)::qs) ->
      mk_all_from_list scp (Logicterm.mk_all_ty scp n ty b) qs
  | (Basic.Id(n, ty)::qs) ->
      raise (Term.term_error "mk_all_from_list, got a Basic.id" qnts)
  | _ -> raise (Term.term_error "Invalid argument, mk_all_from_list" qnts)

let rec mk_var_ty_list ls =
  match ls with
    [] -> []
  | (Basic.Id(n, ty)::ts) -> ((n, ty)::(mk_var_ty_list ts))
  | _ -> raise (Term.term_error "Non-variables not allowed" ls)

let get_lhs t = 
  match t with 
    Basic.Id(n, _) -> (n, [])
  | Basic.App(_, _) -> 
      let f=Term.get_fun t
      in 
      let n=
	(if (Term.is_fun f) 
	then fst(Term.dest_fun f)
	else 
	  (if (Term.is_var f) 
	  then fst(Term.dest_var f)
	  else raise (Term.term_error "Defn: name must be a name" [t])))
      in 
      (n, mk_var_ty_list (Term.get_args t))
  | _ -> 
      raise (Term.term_error "Defn: parameters must be free variables" [])


let mk_decln scp name ty =
  let check_exists () = 
      try 
	ignore(scp.Gtypes.typeof_fn name);
	raise (Term.term_error "Name exists in scope" 
		 [Term.mk_typed_var name ty])
      with Not_found -> ()
  in
  let new_ty () = Gtypes.set_name scp ty
  in 
  let check_type typ = Gtypes.check_decl_type scp typ; typ
  in 
  let ret_ty = 
  try
    (check_exists();
     check_type (new_ty()))
  with err -> 
    raise (Term.add_term_error 
	     "Invalid declaration " [(Term.mk_typed_var name ty)] err)
  in 
  (name, ret_ty)

(*

  with Not_found -> 
    Gtypes.check_decl_type scp ty; (name, ty)
    let vars = Gtypes.get_var_names ty
    in Gtypes.well_defined scp vars ty; (name, ty)
*)

let mk_defn_type env atys rty rfrs = 
  match atys with
    [] -> rty
  |	ts -> 
      (Logicterm.mk_fun_ty_from_list
	 (List.map
	    (fun (x, ty) -> 
	      Gtypes.mgu 
		(try 
		  (List.assoc x rfrs)
		with Not_found -> ty) env)
	    ts) rty)

let rec check_free_vars tyenv name ls = 
  match ls with
    [] -> []
  | (n, ty) :: fvs -> 
      if n = name 
      then check_free_vars tyenv name fvs
      else (ignore(tyenv.Gtypes.typeof_fn n); 
	    check_free_vars tyenv name fvs)
	  
let mk_defn scp name args rhs = 
  let ps = 
    List.map 
      (fun (x, y) -> Term.mk_free x y) args 
  in let rhs1=Term.set_names scp rhs
  in let rty = Typing.typeof scp rhs1
  in let nty = Gtypes.mk_var ("_"^(Basic.name name)^"_typ")
  in let lhs = Term.mk_comb (Term.mk_typed_var name nty) ps
  in let ndn = 
    mk_all_from_list scp (Logicterm.mk_equality lhs rhs1) (List.rev ps) 
  in
  let rfrees = 
    match Term.get_free_vars ndn with
      [] -> ()
    | _ ->
	raise (Term.term_error 
		 "Free variables not allowed in definition" [ndn])
  in 
  let nscp = (Gtypes.add_to_scope scp [name, nty])
  in 
  let tenv=Typing.settype nscp ndn
  in 
  let tenv1=Typing.typecheck_env nscp tenv ndn Gtypes.mk_bool
  in 
  (name, Gtypes.mgu_rename (ref 0) tenv1 (Gtypes.empty_subst()) nty, 
       (Formula.make nscp (Term.retype tenv ndn)))

(*
  Defn(name, Gtypes.mgu_rename (ref 0) tenv1 (Gtypes.empty_subst()) nty, 
       Logic.mk_axiom (Formula.make nscp (Term.retype tenv ndn)))
*)

(* Types *)

(* Type definition: aliasing *)

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


(* Type definition: subtypes *)


(*
   [mk_subtype_property setp rep]:
   make the term 
   << (!x1 x2: (((rep x1) = (rep x2)) => (x1 = x2)))
   and 
   (!x: (P x) = (?x1: x=(rep x1)))>>
   to be used as the subtype theorem.
 *)
(*
let mk_subtype_prop (setP: Basic.term) (rep: Basic.ident)=
  let typedef_term = 
    Term.mk_var (Basic.mk_long "base" "Type_Def") 
  and rep_term = Term.mk_typed_var rep (Gtypes.mk_var "rep_ty1")
  in 
  Term.mk_app (Term.mk_app typedef_term setP) rep_term
*)

let mk_subtype_prop (setP: Basic.term) (rep: Basic.ident)=
  let mk_subtype_1 (rep: Basic.ident)=
    let x1_b=Basic.mk_binding Basic.All "x1" (Gtypes.mk_var "x1_ty")
    and x2_b=Basic.mk_binding Basic.All "x2" (Gtypes.mk_var "x2_ty")
    in 
    let x1= Term.mk_bound x1_b
    and x2= Term.mk_bound x2_b
    and rep_term = Term.mk_typed_var rep (Gtypes.mk_var "rep_ty1")
    in 
    let lhs =
      Logicterm.mk_equality (Term.mk_app rep_term x1) 
	(Term.mk_app rep_term x2)
    and rhs = Logicterm.mk_equality x1 x2
    in 
    let body = Logicterm.mk_implies lhs rhs
    in 
    Basic.Qnt(Basic.All, x1_b, Basic.Qnt(Basic.All, x2_b, body))
  and 
      mk_subtype_2 (setP:Basic.term) (rep: Basic.ident)=
    let y_b=(Basic.mk_binding Basic.All "y" (Gtypes.mk_var "y_ty"))
    and y1_b=(Basic.mk_binding Basic.Ex "y1" (Gtypes.mk_var "y1_ty"))
    in 
    let y= Term.mk_bound y_b
    and y1= Term.mk_bound y1_b
    and rep_term = Term.mk_typed_var rep (Gtypes.mk_var "rep_ty2")
    in 
    let lhs = Term.mk_app setP y
    and rhs =
      Basic.Qnt(Basic.Ex, y1_b, 
		(Logicterm.mk_equality y (Term.mk_app rep_term y1)))
    in 
    Basic.Qnt(Basic.All, y_b, 
	      Logicterm.mk_equality lhs rhs)
  in 
  Logicterm.mk_and (mk_subtype_1 rep) (mk_subtype_2 setP rep)

(*
   [mk_subtype_exists setp]
   make the term << ?x. setp x >> to be used to show that a subtype exists.
 *)
let mk_subtype_exists setp=
  let x_b=(Basic.mk_binding Basic.Ex "x" (Gtypes.mk_var "x_ty"))
  in 
  let x= Term.mk_bound x_b
  in 
  Basic.Qnt(Basic.Ex, x_b, Term.mk_app setp x)


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
let check_type_name scp n = 
  try
    (ignore(scp.Gtypes.typ_defn n);
     raise (Gtypes.type_error "Type already exists" 
	      [Gtypes.mk_constr (Basic.Defined n) []]))
  with Not_found -> ()
       


(*
let rec well_defined scp args t =
  let lookup x= List.find (fun y -> x=y) args
  in 
  let rec well_def t = 
    match t with 
      Constr(Defined n, nargs) ->
	(try 
          (let recrd=scp.Gtypes.typ_defn n
          in 
	  List.iter well_def nargs)
	with Not_found -> 
	  raise (Gtypes.type_error "well_defined: " [t]))
    | Var(v) -> ignore(lookup (!v))
    | WeakVar(v) -> raise (Gtypes.type_error "well_defined " [t])
    | _ -> ()
  in 
  well_def t
*)

let check_well_defined scp args ty= 
    (try 
      Gtypes.well_defined scp args ty;()
    with err ->
      raise (Gtypes.add_type_error "Badly formed type" [ty] err))

let make_witness_type scp dtype setP =
  let fty = Typing.typeof scp setP
  in 
  if not (Logicterm.is_fun_ty fty)
  then 
    raise 
      (Term.add_term_error "Expected a function" [setP]
	 (Gtypes.type_error "Not a function type" [fty]))
  else 
    let tty = 
      Logicterm.mk_fun_ty dtype (Gtypes.mk_base (Basic.Bool))
    in 
    try 
      let sbs=Gtypes.unify scp fty tty
      in 
      Term.retype sbs setP
    with err -> 
      raise
      (Term.add_term_error "Badly typed term" [setP]
	(Gtypes.add_type_error "Invalid type" [tty] err))

let extend_scope_typedef scp id args=
  let record = 
    {Gtypes.name = Basic.name id; Gtypes.args = args; 
     Gtypes.alias = None; Gtypes.characteristics = []}
  in 
  {scp with 
   Gtypes.typ_defn = 
   (fun x -> if(x=id) then record else scp.Gtypes.typ_defn x);
   Gtypes.thy_of =
   (fun sel n ->
     if ((sel = Basic.type_id) & (n=Basic.name id ))
     then thy_of_id id
     else scp.Gtypes.thy_of sel n)     
 }

let extend_scope_identifier scp id typ =
  let thy_of sel n =
    if (sel = fn_id) then 
      if n = (Basic.name id) 
      then Basic.thy_of_id id
      else scp.Gtypes.thy_of sel n
    else scp.Gtypes.thy_of sel n
  and 
      typeof_fn n = 
    if n=id 
    then (Gtypes.copy_type typ)
    else scp.Gtypes.typeof_fn n
  and 
      prec_of sel n = 
    if (sel = fn_id) 
    then 
      if(n = id) then raise Not_found
      else scp.Gtypes.prec_of sel n
    else scp.Gtypes.prec_of sel n
  in 
  {scp with 
   Gtypes.typeof_fn = typeof_fn;
   Gtypes.thy_of = thy_of;
   Gtypes.prec_of = prec_of}

    
let mk_subtype scp name args dtype setP rep=
  let th = scp.Gtypes.curr_thy
  in 
  let id = Basic.mk_long th name
  in 
  let ntype = 
    Gtypes.mk_constr 
      (Basic.Defined id) (List.map Gtypes.mk_var args)
  in
  check_type_name scp id;
  check_args_unique args;
  check_well_defined scp args dtype;
  let new_setp = make_witness_type scp dtype setP
  in 
  let rep_ty = Gtypes.normalize_vars (Logicterm.mk_fun_ty ntype dtype)
  in 
  let nscp = extend_scope_typedef scp id args
  in 
  let subtype_prop = mk_subtype_prop setP rep
  in 
  (rep_ty, new_setp, subtype_prop)
