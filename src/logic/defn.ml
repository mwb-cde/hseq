(*-----
 Name: defn.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)


open Basic
  
type decln = (Basic.ident* Basic.gtype)
type defn = Defn of (Basic.ident* Basic.gtype * Logic.thm)

let dest_decln (id, ty) = id, ty
let dest_defn (Defn x) = x

let rec mk_all_from_list scp b qnts =
  match qnts with 
    [] -> b
  | (Basic.Free(n, ty)::qs) ->
      mk_all_from_list scp (Logicterm.mk_all_ty scp n ty b) qs
  | (Basic.Id(n, ty)::qs) ->
      raise (Term.termError "mk_all_from_list, got a Basic.id" qnts)
  | _ -> raise (Term.termError "Invalid argument, mk_all_from_list" qnts)


let rec mk_var_ty_list ls =
  match ls with
    [] -> []
  | (Basic.Id(n, ty)::ts) -> ((n, ty)::(mk_var_ty_list ts))
  | _ -> raise (Term.termError "Non-variables not allowed" ls)


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


let mk_decln scp name ty =
  try
    let t=scp.Gtypes.typeof_fn name
    in raise (Term.termError "Name exists in scope" 
		[Term.mk_typed_var name ty])
  with Not_found -> Gtypes.well_defined scp ty; (name, ty)

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
	  raise (Term.termError 
		   "Free variables not allowed in definition" [ndn])
  in 
  let nscp = (Gtypes.add_to_scope scp [name, nty])
  in 
  let tenv=Typing.settype nscp ndn
  in 
  let tenv1=Typing.typecheck_env nscp tenv ndn Gtypes.mk_bool
  in 
  Defn(name, Gtypes.mgu_rename (ref 0) tenv1 (Gtypes.empty_subst()) nty, 
       Logic.mk_axiom (Formula.make nscp (Term.retype tenv ndn)))

