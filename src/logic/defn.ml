
open Basic
  
type decln = (Basic.ident* Basic.gtype)
type defn = Defn of (Basic.ident* Basic.gtype * Logic.thm)

let dest_decln (id, ty) = id, ty
let dest_defn (Defn x) = x

let rec mk_all_from_list scp b qnts =
  match qnts with 
    (Basic.Id(n, ty)::qs) ->
      mk_all_from_list scp (Logicterm.mkall_ty scp (name n) ty b) qs
  |	[] -> b
  |	_ -> raise (Term.termError "Invalid argument, mk_all_from_list" qnts)


let rec mk_var_ty_list ls =
  match ls with
    [] -> []
  |	(Basic.Id(n, ty)::ts) -> ((n, ty)::(mk_var_ty_list ts))
  |	_ -> raise (Term.termError "Non-variables not allowed" ls)


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
	  else raise (Term.termError "Defn: name must be a name" [t])))
      in 
      (n, mk_var_ty_list (Term.get_args t))
  | _ -> 
      raise (Term.termError "Defn: parameters must be free variables" [])


let mkdecln scp name ty =
  try
    let t=scp.Gtypes.typeof_fn name
    in raise (Term.termError "Name exists in scope" 
		[Term.mk_typed_var name ty])
  with Not_found -> Gtypes.well_defined scp ty; (name, ty)

let mk_defn_type env atys rty rfrs = 
  match atys with
    [] -> rty
  |	ts -> 
      (Gtypes.mkfun_from_list
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
  |	(n, ty) :: fvs -> 
      if n = name then check_free_vars tyenv name fvs
      else (ignore(tyenv.Gtypes.typeof_fn n); 
	    check_free_vars tyenv name fvs)
	  
let mkdefn scp name args rhs = 
  let ps = 
    List.map 
      (fun (x, y) -> 
	Term.mk_typed_var (Basic.mkname x) y) args 
  in let rty = Typing.typeof scp rhs
  in let nty = Gtypes.mk_var ("_"^(Basic.name name)^"_typ")
  in let lhs = Term.mkcomb (Term.mk_typed_var name nty) ps
  in let ndn = 
    mk_all_from_list scp (Logicterm.mkequal lhs rhs) (List.rev ps) 
  in
  let rfrees = 
    try Term.get_free_vars ndn
    with Not_found -> 
      raise (Term.termError 
	       "Free variables not allowed in definition" [ndn])
  in 
  let nscp =  (Gtypes.add_to_scope scp [name, nty])
  in 
  let tenv=Typing.settype nscp ndn
  in 
  let tenv1=Typing.typecheck_env nscp tenv ndn Gtypes.mk_bool
  in 
  Defn(name, Gtypes.mgu_rename (ref 0) tenv1 (Gtypes.empty_subst()) nty, 
       Logic.mk_axiom (Formula.mk_form nscp (Term.retype tenv ndn)))

