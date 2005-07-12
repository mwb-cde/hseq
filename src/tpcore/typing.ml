(*-----
   Name: typing.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

open Basic
open Term

(*** Type Errors ***)

class typingError s t ty expty=
  object (self)
    inherit Result.error s
    val trm = (t :term)
    val typs = ((expty, ty): Basic.gtype * Basic.gtype)
    method get_term() = trm
    method get_types () = typs
    method print st = 
      Format.printf "@[";
      Format.printf "%s@ " (self#msg());
      Format.printf "expected type@ ";
      Gtypes.print st (fst typs);
      Format.printf "@ got type@ ";
      Gtypes.print st (snd typs);
      Format.printf "@ in term@ ";
      Term.print st trm;
      Format.printf "@]"
  end

let typing_error s tr expty ty= 
  Result.mk_error((new typingError s tr expty ty):>Result.error)
let add_typing_error s tr expty ty es =
  Result.add_error (typing_error s tr expty ty) es

(***
* Typing a term
***)

let typeof_env scp typenv inf trm =
  let rec typeof_aux t env =
    match t with
      Id(n, ty) -> (Gtypes.mgu ty env, env)
    | Free(n, ty) -> (Gtypes.mgu ty env, env)
    | Bound(q) -> (Gtypes.mgu (get_binder_type t) env, env)
    | Const(c) -> (Logicterm.typeof_cnst c, env)
    | Qnt(q, b) -> 
	(match Basic.binder_kind q with
	  Basic.Lambda -> 
	    (let btyp, benv=typeof_aux b env
	    in 
	    (Logicterm.mk_fun_ty (Term.get_binder_type t) btyp, benv))
	| _ -> Logicterm.mk_bool_ty, env)
    | App(f, a) -> 
	let fty, fenv= typeof_aux f env
	in let aty, aenv= typeof_aux a fenv
	and retty = Gtypes.mk_typevar inf
	in
	let nty = Logicterm.mk_fun_ty aty retty
	in 
	let renv = Gtypes.unify_env scp fty nty aenv
	in 
	(Gtypes.mgu retty renv, renv) 
    | Typed(trm, expty) ->
	let tty, tenv= typeof_aux trm env
	in 
	let renv = Gtypes.unify_env scp tty expty tenv
	in 
	(Gtypes.mgu expty renv, renv)
  in typeof_aux trm typenv

let typeof scp t = 
  let ty, _ = typeof_env scp (Gtypes.empty_subst()) (ref 0) t
  in ty
    
(***
* Type-checking 
***)
    
(** Two flavours: 

   [settype] looks up the type of each identifier in the scope before
   type checking.

   [typecheck] assumes that identifiers have already been assigned
   their correct types.
*)

(*** Settype based type checking ****)

let settype_top scp (inf, cache) f typenv exty et =
  let rec settype_aux expty t env =
    match t with
      Id(n, ty) -> 
	(try                         (* get the identifier's type *)
	  let nt = (Scope.type_of scp n) 
	  in 
          (* check given type *) 
	  Gtypes.quick_well_defined scp cache ty; 
          (* unify with identifier type *)
	  let env1= 
	    (try 
	      Gtypes.unify_env scp ty nt env
	    with err -> 
	      raise (add_typing_error "Typechecking: " t 
		       (Gtypes.mgu expty env) (Gtypes.mgu ty env) err))
	  in 
          (* unify with expected type *)
	  Gtypes.unify_env scp nt expty env1
	with Not_found -> (f inf env expty t)) (* error handler *)
    | Free(n, ty) ->
	(Gtypes.quick_well_defined scp cache ty;
	 (try 
	   Gtypes.unify_env scp ty expty env
	 with err -> 
	   raise (add_typing_error "Typechecking: " t 
		    (Gtypes.mgu expty env) (Gtypes.mgu ty env) err)))
    | Bound(q) -> 
	(let ty = get_binder_type t
	in
	Gtypes.quick_well_defined scp cache ty;
	(try
	  Gtypes.unify_env scp ty expty env
	with err -> 
	  raise (add_typing_error "Typechecking: "t
		   (Gtypes.mgu expty env) (Gtypes.mgu ty env) err)))
    | Const(c) -> 
	(let ty = Logicterm.typeof_cnst c
	in
	Gtypes.quick_well_defined scp cache ty;
	(try 
	  Gtypes.unify_env scp ty expty env
	with err -> 
	  raise (add_typing_error "Typechecking: " t
		   (Gtypes.mgu expty env) (Gtypes.mgu ty env) err)))
    | Typed(trm, ty) -> 
	(Gtypes.quick_well_defined scp cache ty;
	 let env1=
	   (try
	     Gtypes.unify_env scp ty expty env
	   with err -> 
	     raise (add_typing_error "Typechecking: " t
	      (Gtypes.mgu expty env) (Gtypes.mgu ty env) err))
	 in 
	 (try
	   settype_aux ty trm env1
	 with err -> 
	   raise (Term.add_term_error "Typechecking failure " [t] err)))
    | App(f, a) -> 
	let aty = Gtypes.mk_typevar inf       (* make an argument type *)
	in
	let fty = Logicterm.mk_fun_ty aty expty  (* expect a function type *)
	in 
	let fenv=
	  (try
	    settype_aux fty f env   (* check function type *)
	  with err -> 
	    raise (Term.add_term_error "Typechecking failure " [t] err))
	in 
	(try
	  settype_aux aty a fenv  (* check argument type *)
	with err -> 
	  raise (Term.add_term_error "Typechecking: " [t] err))
    | Qnt(q, b) ->
	(match Basic.binder_kind q with
	  Basic.Lambda -> 
	    (let rty = Gtypes.mk_typevar inf     (* range type *)
	    and fty = Term.get_binder_type t  (* domain *)
	    in
	    let bty = Logicterm.mk_fun_ty fty rty (* type of term *)
	    in 
            Gtypes.quick_well_defined scp cache fty; (* check domain *)
	    let env1= 
	      (try
		Gtypes.unify_env scp bty expty env
	      with err -> 
		raise (add_typing_error "Typechecking: " t
			 (Gtypes.mgu expty env) (Gtypes.mgu bty env) err))
	    in 
	    (try
	      settype_aux rty b env1
	    with err -> 
	      raise (Term.add_term_error "Typechecking: " [t] err)))
	| _ -> 
	    let env1=
	      (try 
		settype_aux Logicterm.mk_bool_ty b env
	      with err -> 
		raise (Term.add_term_error "Typechecking: " [t] err))
	    in 
	    (try
	      Gtypes.unify_env scp expty Logicterm.mk_bool_ty env1
	    with err -> 
	      raise (add_typing_error "Typechecking: " t
		       (Gtypes.mgu expty env) (Logicterm.mk_bool_ty) err)))
  in settype_aux exty et typenv

let settype scp t=
  let inf = (ref 0)
  and cache =  Lib.empty_env()
  and f inf env expty trm = 
    (match trm with
      Id(n, ty) -> Gtypes.unify_env scp ty expty env
    | _ -> env)
  in 
  settype_top scp (inf, cache) f 
    (Gtypes.empty_subst()) (Gtypes.mk_typevar inf) t




let typecheck_env scp env t expty =
  let inf = (ref 0, Lib.empty_env())
  and f = (fun _ _ _ t -> 
    match t with 
      (Basic.Id (n, ty)) ->
	raise 
	  (Term.term_error
	     ("Typecheck: unknown identifier ")
	     [t])
    | _ -> 
	raise
	  (Term.term_error "Typecheck: unknown error" [t] ))
  in 
  settype_top scp inf f env expty t

let typecheck scp t expty =
  ignore(typecheck_env scp (Gtypes.empty_subst()) t expty)

(*** typecheck based type checking ****)

let typecheck_aux scp (inf, cache) typenv exty et =
  let rec type_aux expty t env=
    match t with
      Id(n, ty) -> 
	Gtypes.quick_well_defined scp cache ty; (* check given type *) 
	(try 
	  Gtypes.unify_env scp ty expty env     (* unify with expected type *)
	with err -> 
	  raise (add_typing_error "Typechecking: " t 
	   (Gtypes.mgu expty env) (Gtypes.mgu ty env) err))
    | Free(n, ty) -> 
	Gtypes.quick_well_defined scp cache ty; (* check given type *) 
	(try 
	  Gtypes.unify_env scp ty expty env     (* unify with expected type *)
	with err -> 
	  raise (add_typing_error "Typechecking: " t 
	   (Gtypes.mgu expty env) (Gtypes.mgu ty env) err))
    | Bound(q) -> 
	(let ty = get_binder_type t
	in
	Gtypes.quick_well_defined scp cache ty;
	(try 
	  Gtypes.unify_env scp ty expty env
	with err -> 
	  raise (add_typing_error "Typechecking: " t 
	   (Gtypes.mgu expty env) (Gtypes.mgu ty env) err)))
    | Const(c) -> 
	(let ty = Logicterm.typeof_cnst c
	in
	Gtypes.quick_well_defined scp cache ty;
	(try 
	  Gtypes.unify_env scp ty expty env
	with err -> 
	  raise (add_typing_error "Typechecking: " t 
	   (Gtypes.mgu expty env) (Gtypes.mgu ty env) err)))
    | Typed(trm, ty) -> 
	(Gtypes.quick_well_defined scp cache ty;
	 let env1=
	   (try
	     Gtypes.unify_env scp ty expty env
	   with err -> 
	     raise (add_typing_error "Typechecking: " t 
	      (Gtypes.mgu expty env) (Gtypes.mgu ty env) err))
	 in 
	 type_aux ty trm env1)
    | App(f, a) -> 
	let aty = Gtypes.mk_typevar inf       (* make an argument type *)
	in
	let fty = Logicterm.mk_fun_ty aty expty  (* expect a function type *)
	in 
	let fenv=
	  (try type_aux fty f env       (* check function type *)
	  with err ->
	    Term.add_term_error "Typechecking:" [t] err)
	in 
	(try
	  type_aux aty a fenv            (* check argument type *)
	with err -> Term.add_term_error "Typechecking: " [t] err)
    | Qnt(q, b) ->
	(match Basic.binder_kind q with
	  Basic.Lambda -> 
	    let rty = Gtypes.mk_typevar inf     (* range type *)
	    and fty = Term.get_binder_type t  (* domain *)
	    in
	    let bty = Logicterm.mk_fun_ty fty rty (* type of term *)
	    in 
	    Gtypes.quick_well_defined scp cache fty; (* check domain *)
	    let env1= 
	      (try
		Gtypes.unify_env scp bty expty env
	      with err -> 
		raise (add_typing_error "Typechecking: " t 
		 (Gtypes.mgu expty env) (Gtypes.mgu bty env) err))
	    in 
	    (try
	      type_aux rty b env1
	    with err ->
	      Term.add_term_error "Typecheck: " [t] err)
	| _ -> 
	    let env1=type_aux Logicterm.mk_bool_ty b env
	    in 
	    (try
	      Gtypes.unify_env scp expty Logicterm.mk_bool_ty env1
	    with err -> 
	      raise (add_typing_error "Typechecking: " t 
		       (Gtypes.mgu expty env) 
		       (Gtypes.mgu Logicterm.mk_bool_ty env) err)))
  in 
  try 
    type_aux exty et typenv
  with err -> 
    raise
      (Term.add_term_error "Typecheck: badly typed" [et] err)


let typecheck_top scp env t expty = 
  let inf = (ref 0, Lib.empty_env())
  in 
  typecheck_aux scp inf env expty t

(*** 
* Well-definedness of types.
***)

let rec check_types scp t =
  match t with
    Id(_, ty) -> Gtypes.well_defined scp [] ty
  | App(f, a) -> check_types scp f; check_types scp a
  | Qnt(q, b) ->
      Gtypes.well_defined scp [] (Term.get_binder_type t);
      check_types scp b
  | Typed(t, ty) -> Gtypes.well_defined scp [] ty; check_types scp t
  | x -> ()

