(*-----
   Name: typing.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

open Basic
open Term

(*
   class typingError s ts tys=
   object (self)
   inherit Result.error s
   val trms = (ts :term list)
   val typs = (tys: Basic.gtype list)
   method get_terms() = trms
   method get_types () = typs
   method print st = 
   Format.printf "@[<v>";
   Format.printf "@[%s@]@," (self#msg());
   (match (self#get_terms()) with 
   [] -> ()
   | ts -> 
   ( Format.printf "@[%s" "Terms: ";
   Printer.print_sep_list 
   (Term.print_term st 0, ",") ts;
   Format.printf "@]@,"));
   (match (self#get_types()) with 
   [] -> ()
   | ts -> 
   (Format.printf "@[%s" "Gtypes: ";
   Printer.print_sep_list
   (Gtypes.print_type st 0, ",") ts;
   Format.printf "@]@,"));
   Format.printf "@]"
   end
   let typingError s tr ty= 
   Result.mk_error((new typingError s tr ty):>Result.error)
   let addtypingError s tr ty es =
   raise (Result.add_error (typingError s tr ty) es)
 *)

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


let typeof_env scp typenv inf trm =
  let rec typeof_aux t env =
    match t with
      Id(n, ty) -> (Gtypes.mgu ty env, env)
    | Free(n, ty) -> (Gtypes.mgu ty env, env)
    | Bound(q) -> (Gtypes.mgu (get_binder_type t) env, env)
    | Const(c) -> (Logicterm.typeof_cnst c, env)
    | Qnt(Basic.Lambda, q, b) -> 
	let btyp, benv=typeof_aux b env
	in 
	(Logicterm.mk_fun_ty (Term.get_qnt_type t) btyp, benv)
    | Qnt(_, q, b) -> Gtypes.mk_bool, env
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
    | Qnt(Basic.Lambda, q, b) ->
	let rty = Gtypes.mk_typevar inf     (* range type *)
	and fty = Term.get_qnt_type t  (* domain *)
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
	  raise (Term.add_term_error "Typechecking: " [t] err))
    | Qnt(_, q, b) ->
	let env1=
	  (try 
	    settype_aux Gtypes.mk_bool b env
	  with err -> 
	    raise (Term.add_term_error "Typechecking: " [t] err))
	in 
	(try
	  Gtypes.unify_env scp expty Gtypes.mk_bool env1
	with err -> 
	  raise (add_typing_error "Typechecking: " t
		   (Gtypes.mgu expty env) (Gtypes.mk_bool) err))

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


(* typecheck: assumes identifiers are assigned their types *)


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
    | Qnt(Lambda, q, b) ->
	let rty = Gtypes.mk_typevar inf     (* range type *)
	and fty = Term.get_qnt_type t  (* domain *)
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
    | Qnt(_, q, b) ->
	let env1=type_aux Gtypes.mk_bool b env
	in 
	(try
	  Gtypes.unify_env scp expty Gtypes.mk_bool env1
	with err -> 
	  raise (add_typing_error "Typechecking: " t 
		   (Gtypes.mgu expty env) 
		   (Gtypes.mgu Gtypes.mk_bool env) err))
  in 
  try 
    type_aux exty et typenv
  with err -> 
    raise
      (Term.add_term_error "Typecheck: badly typed" [et] err)


let simple_typecheck_env scp env t expty = 
  let inf = (ref 0, Lib.empty_env())
  in 
  typecheck_aux scp inf env expty t

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

(*
   let rec check_types scp t =
   match t with
   Id(_, ty) -> Gtypes.check_decl_type scp ty
   | App(f, a) -> check_types scp f; check_types scp a
   | Qnt(_, q, b) ->
   Gtypes.check_decl_type scp (Term.get_qnt_type t);
   check_types scp b
   | Typed(t, ty) -> Gtypes.check_decl_type scp ty; check_types scp t
   | x -> ()
 *)

let rec check_types scp t =
  match t with
    Id(_, ty) -> Gtypes.well_defined scp [] ty
  | App(f, a) -> check_types scp f; check_types scp a
  | Qnt(_, q, b) ->
      Gtypes.well_defined scp [] (Term.get_qnt_type t);
      check_types scp b
  | Typed(t, ty) -> Gtypes.well_defined scp [] ty; check_types scp t
  | x -> ()

(* infers type of terms, returns infered type *)


let rec infer_aux (inf, cache) scp env t =
  match t with
    Id(n, ty) -> 
      (try                         (* get the identifier's type *)
	let nt = Scope.type_of scp n
	in 
	Gtypes.quick_well_defined scp cache ty; (* check type *) 
	let env1=Gtypes.unify_env scp ty nt env (* unify with given type *)
	in (ty, env1)
      with Not_found -> 
	raise (Term.term_error "Typecheck: unknown identifier" [t]))
  | Free(n, ty) -> 
      Gtypes.quick_well_defined scp cache ty;
      (ty, env)
  | Bound(q) -> 
      let ty = get_binder_type t
      in
      Gtypes.quick_well_defined scp cache ty;
      (ty, env)
  | Const(c) ->
      let ty = Logicterm.typeof_cnst c
      in
      Gtypes.quick_well_defined scp cache ty;
      (ty, env)
  | Typed(trm, ty) -> 
      Gtypes.quick_well_defined scp cache ty;
      let nty, env1 = infer_aux (inf, cache) scp env trm
      in 
      let env2=Gtypes.unify_env scp ty nty env1
      in (ty, env2)
  | App(lf, a) -> 
      (* get argument type *)
      let aty, aenv = infer_aux (inf, cache) scp env a
      in  (* get function type *)
      let fty, fenv = infer_aux (inf, cache) scp aenv lf
      in 
      let rty = Gtypes.mk_typevar inf
      in 
      let nty = Logicterm.mk_fun_ty aty rty  (* make a dummy type *)
      in                               (* unify with actual f-type *)
      let env1=Gtypes.unify_env scp nty fty fenv
      in (rty, env1)
  | Qnt(Lambda, q, b) ->
      let rty, renv = infer_aux (inf, cache) scp env b
      and aty = Term.get_qnt_type t  (* domain *)
      in
      let nty = Logicterm.mk_fun_ty aty rty (* type of term *)
      in 
      Gtypes.quick_well_defined scp cache nty; (* check domain *)
      (nty, renv)
  | Qnt(_, q, b) -> 
      let nty, nenv=infer_aux (inf, cache) scp env b
      in 
      let env1=Gtypes.unify_env scp nty Gtypes.mk_bool nenv
      in (nty, env1)

let infer_types_env scp env trm =
  let inf = (ref 0, Lib.empty_env())
  in 
  infer_aux inf scp env trm

let infer_types scp trm =
  let inf = (ref 0, Lib.empty_env())
  in 
  let rty, _ = infer_aux inf scp (Gtypes.empty_subst()) trm
  in rty

let set_exact_types scp trm =
  let rec set_aux t =
    match t with
      Id(id, _) -> 
	(try
	  (let ty = Scope.type_of scp id
	  in (Id(id, ty)))
	with Not_found -> t)
    | Qnt(k, q, b) -> Qnt(k, q, set_aux b)
    | Typed(tt, tty) -> Typed(set_aux tt, tty)
    | App(f, a) -> App(set_aux f, set_aux a)
    | _ -> t
  in set_aux trm


let assign_types scp trm =
  let rec set_aux t =
    match t with
      Id(id, _) -> 
	(try
	  (let ty = Scope.type_of scp id
	  in (Id(id, ty)))
	with Not_found -> 
	  raise (Term.term_error "assign_types: Unknown identifier" [t]))
    | Qnt(k, q, b) -> Qnt(k, q, set_aux b)
    | Typed(tt, tty) -> Typed(set_aux tt, tty)
    | App(f, a) -> App(set_aux f, set_aux a)
    | _ -> t
  in set_aux trm


(*
   let typecheck_env scp env t expty =
   let inf = ref 0
   in 
   let rec typchck_aux expty t =
   match t with
   Var(n, ty) ->    (* unify with expected type *)
   Gtypes.unify_env scp ty expty env  
   | Bound(q) -> 
   (let ty = get_binder_type t
   in
   Gtypes.unify_env scp ty expty env)
   | Const(c) -> 
   (let ty = Logicterm.typeof_cnst c
   in
   Gtypes.unify_env scp ty expty env)
   | Typed(trm, ty) -> 
   Gtypes.unify_env scp ty expty env;
   typchck_aux ty trm
   | App(lf, a) -> 
   let aty = Gtypes.mk_typevar inf       (* make an argument type *)
   in
   let fty = Logicterm.mk_fun_ty aty expty  (* expect a function type *)
   in 
   typchck_aux fty lf;            (* check function type *)
   typchck_aux aty a              (* check argument type *)
   | Qnt(q, b) ->
   if Logicterm.is_lambda t
   then 
   (let rty = Gtypes.mk_typevar inf     (* range type *)
   and fty = Term.get_qnt_type t  (* domain *)
   in
   let bty = Logicterm.mk_fun_ty fty rty (* type of term *)
   in 
   Gtypes.unify_env scp bty expty env;
   typchck_aux rty b)
   else 
   (typchck_aux Gtypes.mk_bool b;
   Gtypes.unify_env scp expty Gtypes.mk_bool env)
   in typchck_aux expty t

   let typecheck scp t expty =
   typecheck_env scp (Gtypes.empty_subst()) t expty; ()
 *)

