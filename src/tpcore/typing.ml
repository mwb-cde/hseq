open Term

class typingError s ts tys=
  object (self)
    inherit Result.error s
    val trms = (ts :term list)
    val typs = (tys: Gtypes.gtype list)
    method get_terms() = trms
    method get_types () = typs
    method print st = 
      Format.open_box 0; 
      Format.open_box 0; 
      print_string (self#msg());
      Format.print_break 1 2;
      Format.print_newline();
      Format.open_box 0; 
      print_string "Terms: ";
      Format.print_break 1 2;
      Format.open_box 0; 
      Printer.print_sep_list 
	(Term.print_term st 0, ",")
	(self#get_terms());
      Format.close_box();
      Format.open_box 0;
      Format.print_break 1 2;
      print_string "Gtypes: ";
      Format.open_box 0; 
      Printer.print_sep_list
	(Gtypes.print_type st 0, ",")
	(self#get_types());
      Format.close_box();
      Format.close_box();
  end
let typingError s tr ty= 
  Result.mkError((new typingError s tr ty):>Result.error)
let addtypingError s tr ty es =
  raise (Result.addError ((new typingError s tr ty):>Result.error) es)

let typeof_env scp typenv inf trm =
  let rec typeof_aux t env =
    match t with
      Var(n, ty) -> (Gtypes.mgu ty env, env)
    |	Bound(q) -> (Gtypes.mgu (get_binder_type t) env, env)
    |	Const(c) -> (Gtypes.typeof_cnst c, env)
    |	Qnt(q, b) -> 
	if Logicterm.is_lambda t 
	then
	  let btyp, benv=typeof_aux b env
	  in 
	  (Gtypes.mk_fun (Term.get_qnt_type t) btyp, benv)
	else 
	  (Gtypes.mk_bool, env)
    |	App(f, a) -> 
	let fty, fenv= typeof_aux f env
	in let aty, aenv= typeof_aux a fenv
	and retty = Gtypes.mk_typevar inf
	in
	let nty = Gtypes.mk_fun aty retty
	in 
	let renv = Gtypes.unify_env scp fty nty aenv
	in 
	(Gtypes.mgu retty renv, renv)
    |	Typed(trm, expty) ->
	let tty, tenv= typeof_aux trm env
	in 
	let renv = Gtypes.unify_env scp tty expty tenv
	in 
	(Gtypes.mgu expty renv, renv)
  in typeof_aux trm typenv

let typeof scp t = 
  let ty, _ = typeof_env scp (Gtypes.empty_subst()) (ref 0) t
  in ty

(*
let retype tyenv t=
  let qenv=Term.empty_table()
  in 
  let rec retype_aux t =
    match t with
      Var(n, ty) -> Var(n, Gtypes.mgu ty tyenv)
    | Bound(q) -> 
	(try Term.table_find t qenv
	with Not_found -> t)
    | Const(c) -> t
    | Typed(trm, ty) -> retype_aux trm
    | App(f, a) -> 
	App(retype_aux f, retype_aux a)
    | Qnt(q, b) ->
	(let (_, oqnt, oqnm, oqty, _) = Term.dest_qnt t
	in 
	let nty = Gtypes.mgu oqty tyenv
	in 
	let nq = Term.mk_binding oqnt oqnm nty
	in 
	Term.table_add (Bound(q)) (Bound(nq)) qenv;
	let rt= Qnt(nq, retype_aux b)
	in 
	Term.table_remove (Bound(q)) qenv; rt)
  in 
  retype_aux t

(* retype_pretty: 
   as for retype, make substitution for type variables
   but also replace other type variables with new, prettier names 
 *)

let retype_pretty_env typenv trm=
  let inf=ref 0
  in 
  let qenv=Term.empty_table()
  in 
  let rec retype_aux t name_env =
    match t with
      Var(n, ty) -> 
	let nt, nenv1=Gtypes.mgu_rename_env inf typenv name_env ty
	in 
	(Var(n, nt), nenv1)
    | Bound(q) -> (Term.table_find t qenv, name_env)
    | Const(c) -> (t, name_env)
    | Typed(trm, ty) -> retype_aux trm name_env
    | App(f, a) -> 
	let nf, nenv1=retype_aux f name_env
	in let na, nenv2=retype_aux a nenv1
	in 
	(App(nf, na), nenv2)
    | Qnt(q, b) ->
	(let (_, oqnt, oqnm, oqty, _) = Term.dest_qnt t
	in 
	let nty, nenv1 =Gtypes.mgu_rename_env inf typenv name_env oqty
	in 
	let nq = Term.mk_binding oqnt oqnm nty
	in 
	Term.table_add (Bound(q)) (Bound(nq)) qenv;
	let nb, nenv2=retype_aux b nenv1
	in let rt= Qnt(nq, nb)
	in 
	Term.table_remove (Bound(q)) qenv; (rt, nenv2))
  in 
  retype_aux trm (Gtypes.empty_subst())

(*
   let retype_pretty typenv trm =
   let nt, _ = retype_pretty_env typenv trm
   in nt
 *)

let retype_pretty typenv trm = 
  let nt, _ = retype_pretty_env typenv trm
  in nt
*)
    
let settype_top scp (inf, cache) f typenv exty et =
  let rec settype_aux expty t env =
    match t with
      Var(n, ty) -> 
	(try                         (* get the identifier's type *)
	  let nt = (scp.Gtypes.typeof_fn n) 
	  in 
          (* check given type *) 
	  Gtypes.quick_well_defined scp cache ty; 
          (* unify with identifier type *)
	  let env1= Gtypes.unify_env scp ty nt env
	  in 
          (* unify with expected type *)
	  Gtypes.unify_env scp nt expty env1
	with Not_found -> (f inf env expty t))  (* error handler *)
    | Bound(q) -> 
	(let ty = get_binder_type t
	in
	Gtypes.quick_well_defined scp cache ty;
	Gtypes.unify_env scp ty expty env)
    | Const(c) -> 
	(let ty = Gtypes.typeof_cnst c
	in
	Gtypes.quick_well_defined scp cache ty;
	Gtypes.unify_env scp ty expty env)
    |	Typed(trm, ty) -> 
	(Gtypes.quick_well_defined scp cache ty;
	 let env1=Gtypes.unify_env scp ty expty env
	 in 
	 settype_aux ty trm env1)
    |	App(f, a) -> 
	let aty = Gtypes.mk_typevar inf       (* make an argument type *)
	in
	let fty = Gtypes.mk_fun aty expty  (* expect a function type *)
	in 
	let fenv=settype_aux fty f env   (* check function type *)
	in 
	settype_aux aty a fenv  (* check argument type *)
    |	Qnt(q, b) ->
	if Logicterm.is_lambda t
	then 
	  (let rty = Gtypes.mk_typevar inf     (* range type *)
	  and fty = Term.get_qnt_type t  (* domain *)
	  in
	  let bty = Gtypes.mk_fun fty rty (* type of term *)
	  in 
          Gtypes.quick_well_defined scp cache fty; (* check domain *)
	  let env1= Gtypes.unify_env scp bty expty env
	  in 
	  settype_aux rty b env1)
	else 
	  let env1=settype_aux Gtypes.mk_bool b env
	  in 
	  Gtypes.unify_env scp expty Gtypes.mk_bool env1
  in settype_aux exty et typenv
    
let settype scp t=
  let inf = (ref 0)
  and cache =  Lib.empty_env()
  and f inf env expty trm = 
    (match trm with
      Var(n, ty) -> Gtypes.unify_env scp ty expty env
    | _ -> env)
  in 
  settype_top scp (inf, cache) f 
    (Gtypes.empty_subst()) (Gtypes.mk_typevar inf) t


(* typecheck: assumes identifiers are assigned their types *)

let typecheck_aux scp (inf, cache) typenv exty et =
  let rec type_aux expty t env=
    match t with
      Var(n, ty) -> 
	Gtypes.quick_well_defined scp cache ty; (* check given type *) 
	Gtypes.unify_env scp ty expty env     (* unify with expected type *)
    | Bound(q) -> 
	(let ty = get_binder_type t
	in
	Gtypes.quick_well_defined scp cache ty;
	Gtypes.unify_env scp ty expty env)
    | Const(c) -> 
	(let ty = Gtypes.typeof_cnst c
	in
	Gtypes.quick_well_defined scp cache ty;
	Gtypes.unify_env scp ty expty env)
    |	Typed(trm, ty) -> 
	(Gtypes.quick_well_defined scp cache ty;
	 let env1=Gtypes.unify_env scp ty expty env
	 in 
	 type_aux ty trm env1)
    |	App(f, a) -> 
	let aty = Gtypes.mk_typevar inf       (* make an argument type *)
	in
	let fty = Gtypes.mk_fun aty expty  (* expect a function type *)
	in 
	let fenv=type_aux fty f env       (* check function type *)
	in type_aux aty a fenv            (* check argument type *)
    |	Qnt(q, b) ->
	if Logicterm.is_lambda t
	then 
	  (let rty = Gtypes.mk_typevar inf     (* range type *)
	  and fty = Term.get_qnt_type t  (* domain *)
	  in
	  let bty = Gtypes.mk_fun fty rty (* type of term *)
	  in 
          Gtypes.quick_well_defined scp cache fty; (* check domain *)
	  let env1= Gtypes.unify_env scp bty expty env
	  in 
	  type_aux rty b env1)
	else 
	  (let env1=type_aux Gtypes.mk_bool b env
	  in 
	  Gtypes.unify_env scp expty Gtypes.mk_bool env1)
  in 
  try 
    type_aux exty et typenv
  with _ -> raise
      (typingError "Typecheck: badly typed" [et] [exty])

let simple_typecheck_env scp env t expty = 
  let inf = (ref 0, Lib.empty_env())
  in 
  typecheck_aux scp inf env expty t

let typecheck_env scp env t expty =
  let inf = (ref 0, Lib.empty_env())
  and f = (fun _ _ _ t -> 
    match t with 
      (Term.Var (n, ty)) ->
	raise 
	  (typingError
	     ("Typecheck: unknown identifier "^(Basic.string_fnid n))
	     [t] [expty])
    | _ -> raise
	  (typingError "Typecheck: unknown error" [t] [expty]))
  in 
  settype_top scp inf f env expty t

let typecheck scp t expty =
  ignore(typecheck_env scp (Gtypes.empty_subst()) t expty)

let rec check_types scp t =
  match t with
    Var(_, ty) -> Gtypes.well_defined scp ty
  |	App(f, a) -> check_types scp f; check_types scp a
  |	Qnt(q, b) ->
      Gtypes.well_defined scp (Term.get_qnt_type t);
      check_types scp b
  |	Typed(t, ty) -> Gtypes.well_defined scp ty; check_types scp t
  |	x -> ()

(* infers type of terms, returns infered type *)

let rec infer_aux (inf, cache) scp env t =
  match t with
    Var(n, ty) -> 
      (try                         (* get the identifier's type *)
	let nt =  (scp.Gtypes.typeof_fn n) 
	in 
	Gtypes.quick_well_defined scp cache ty; (* check type *) 
	let env1=Gtypes.unify_env scp ty nt env (* unify with given type *)
	in (ty, env1)
      with Not_found -> 
	raise (typingError "Typecheck: unknown identifier" [t] []))
  | Bound(q) -> 
      let ty = get_binder_type t
      in
      Gtypes.quick_well_defined scp cache ty;
      (ty, env)
  | Const(c) ->
      let ty = Gtypes.typeof_cnst c
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
      let nty = Gtypes.mk_fun aty rty  (* make a dummy type *)
      in                               (* unify with actual f-type *)
      let env1=Gtypes.unify_env scp nty fty fenv
      in (rty, env1)
  | Qnt(q, b) ->
      if Logicterm.is_lambda t
      then 
	let rty, renv = infer_aux (inf, cache) scp env b
	and aty = Term.get_qnt_type t  (* domain *)
	in
	let nty = Gtypes.mk_fun aty rty (* type of term *)
	in 
        Gtypes.quick_well_defined scp cache nty; (* check domain *)
	(nty, renv)
      else 
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
      Var(id, _) -> 
	(try
	  (let ty = scp.Gtypes.typeof_fn id
	  in (Var(id, ty)))
	with Not_found -> t)
    | Qnt(q, b) -> Qnt(q, set_aux b)
    | Typed(tt, tty) -> Typed(set_aux tt, tty)
    | App(f, a) -> App(set_aux f, set_aux a)
    | _ -> t
  in set_aux trm


let assign_types scp trm =
  let rec set_aux t =
    match t with
      Var(id, _) -> 
	(try
	  (let ty = scp.Gtypes.typeof_fn id
	  in (Var(id, ty)))
	with Not_found -> 
	  raise (typingError "assign_types: Unknown identifier" [t] []))
    | Qnt(q, b) -> Qnt(q, set_aux b)
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
   (let ty = Gtypes.typeof_cnst c
   in
   Gtypes.unify_env scp ty expty env)
   | Typed(trm, ty) -> 
   Gtypes.unify_env scp ty expty env;
   typchck_aux ty trm
   | App(lf, a) -> 
   let aty = Gtypes.mk_typevar inf       (* make an argument type *)
   in
   let fty = Gtypes.mk_fun aty expty  (* expect a function type *)
   in 
   typchck_aux fty lf;            (* check function type *)
   typchck_aux aty a              (* check argument type *)
   | Qnt(q, b) ->
   if Logicterm.is_lambda t
   then 
   (let rty = Gtypes.mk_typevar inf     (* range type *)
   and fty = Term.get_qnt_type t  (* domain *)
   in
   let bty = Gtypes.mk_fun fty rty (* type of term *)
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

