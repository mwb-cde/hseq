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
      Corepp.list_print (Term.print_term st) 
	(fun _ -> Format.print_string ","; 
	  Format.print_break 1 2; 
	  Format.close_box(); Format.open_box 0)
	(self#get_terms());
      Format.close_box();
      Format.open_box 0;
      Format.print_break 1 2;
      print_string "Gtypes: ";
      Format.open_box 0; 
      Corepp.list_print (Gtypes.print_type st) 
	(fun _ -> Format.print_string ","; 
	  Format.print_break 1 2; 
	  Format.close_box(); Format.open_box 0)
	(self#get_types());
      Format.close_box();
      Format.close_box();
  end
  let typingError s tr ty= 
    Result.mkError((new typingError s tr ty):>Result.error)
  let addtypingError s tr ty es =
    raise (Result.addError ((new typingError s tr ty):>Result.error) es)


    let typeof_env scp env inf trm =
    let rec typeof_aux t =
      match t with
	Var(n, ty) -> Gtypes.mgu ty env
      |	Bound(q) -> Gtypes.mgu (get_binder_type t) env
      |	Const(c) -> Gtypes.typeof_cnst c
      |	Qnt(q, b) -> 
	  if Logicterm.is_lambda t 
	  then
	    (Gtypes.mk_fun (Term.get_qnt_type t) 
		(typeof_aux  b))
	  else Gtypes.mk_bool
      |	App(f, a) -> 
	  let fty = typeof_aux  f
	  and aty = typeof_aux  a
	  and retty = Gtypes.mk_typevar inf
	  in
	  let nty = Gtypes.mk_fun aty retty
	  in 
	  ignore(Gtypes.unify_env scp fty nty env);
	  (Gtypes.mgu retty env)
      |	Typed(trm, expty) ->
	  let tty = typeof_aux   trm
	  in 
	  ignore(Gtypes.unify_env scp tty expty env);
	  Gtypes.mgu expty env
    in typeof_aux trm

let typeof scp t = typeof_env scp (Gtypes.empty_subst()) (ref 0) t

let retype env t=
  let qenv=Term.empty_subst()
  in 
  let rec retype_aux  t =
    match t with
      Var(n, ty) -> Var(n, Gtypes.mgu ty env)
    | Bound(q) -> Term.find t qenv
    | Const(c) -> t
    | Typed(trm, ty) -> retype_aux trm
    | App(f, a) -> 
	App(retype_aux f, retype_aux a)
    | Qnt(q, b) ->
	(let (_, oqnt, oqnm, oqty, _) = Term.dest_qnt t
	in 
	let nty = Gtypes.mgu oqty env
	in let nq = Term.mk_binding oqnt oqnm nty
	in ignore(Term.add (Bound(q)) (Bound(nq)) qenv);
	let rt= Qnt(nq, retype_aux b)
	in Term.remove (Bound(q)) qenv; rt)
  in 
  retype_aux t

(* retype_pretty: 
   as for retype, make substitution for type variables
   but also replace other type variables with new, prettier names 
*)

let retype_pretty env trm=
  let inf=ref 0
  in 
  let name_env = Gtypes.empty_subst()
  in 
  let qenv=Term.empty_subst()
  in 
  let rec retype_aux t =
    match t with
      Var(n, ty) -> Var(n, Gtypes.mgu_rename inf env name_env ty)
    | Bound(q) -> Term.find t qenv
    | Const(c) -> t
    | Typed(trm, ty) -> retype_aux trm
    | App(f, a) -> 
	App(retype_aux f, retype_aux a)
    | Qnt(q, b) ->
	(let (_, oqnt, oqnm, oqty, _) = Term.dest_qnt t
	in 
	let nty =Gtypes.mgu_rename inf env name_env oqty
	in let nq = Term.mk_binding oqnt oqnm nty
	in ignore(Term.add (Bound(q)) (Bound(nq)) qenv);
	let rt= Qnt(nq, retype_aux b)
	in Term.remove (Bound(q)) qenv; rt)
  in 
  retype_aux trm
    
let settype_top scp (inf, cache) f env exty et =
  let rec settype_aux expty t =
    match t with
      Var(n, ty) -> 
	(try                         (* get the identifier's type *)
	  let nt = (scp.Gtypes.typeof_fn n) 
	  in 
	  Gtypes.quick_well_defined scp cache ty; (* check given type *) 
	  ignore(Gtypes.unify_env scp ty nt env); 
                                             (* unify with identifier type *)
	  Gtypes.unify_env scp nt expty env     (* unify with expected type *)
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
	 ignore(Gtypes.unify_env scp ty expty env);
	 settype_aux ty trm)
    |	App(lf, a) -> 
	let aty = Gtypes.mk_typevar inf       (* make an argument type *)
	in
	let fty = Gtypes.mk_fun aty expty  (* expect a function type *)
	in 
	ignore(settype_aux fty lf);            (* check function type *)
	settype_aux aty a              (* check argument type *)
    |	Qnt(q, b) ->
	if Logicterm.is_lambda t
	then 
	  (let rty = Gtypes.mk_typevar inf     (* range type *)
	  and fty = Term.get_qnt_type t  (* domain *)
	  in
	  let bty = Gtypes.mk_fun fty rty (* type of term *)
	  in 
          Gtypes.quick_well_defined scp cache fty; (* check domain *)
	  ignore(Gtypes.unify_env scp bty expty env);
	  settype_aux rty b)
	else 
	  (ignore(settype_aux Gtypes.mk_bool b);
	   Gtypes.unify_env scp expty Gtypes.mk_bool env)
  in settype_aux exty et
    
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

let typecheck_aux scp (inf, cache) env exty et =
  let rec type_aux expty t =
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
	  ignore(Gtypes.unify_env scp ty expty env);
	  type_aux ty trm)
      |	App(lf, a) -> 
	  let aty = Gtypes.mk_typevar inf       (* make an argument type *)
	  in
	  let fty = Gtypes.mk_fun aty expty  (* expect a function type *)
	  in 
	  ignore(type_aux fty lf);            (* check function type *)
	  type_aux aty a              (* check argument type *)
      |	Qnt(q, b) ->
	  if Logicterm.is_lambda t
	  then 
	    (let rty = Gtypes.mk_typevar inf     (* range type *)
	    and fty = Term.get_qnt_type t  (* domain *)
	    in
	    let bty = Gtypes.mk_fun fty rty (* type of term *)
	    in 
            Gtypes.quick_well_defined scp cache fty; (* check domain *)
	    ignore(Gtypes.unify_env scp bty expty env);
	    type_aux rty b)
	  else 
	    (ignore(type_aux Gtypes.mk_bool b);
	     Gtypes.unify_env scp expty Gtypes.mk_bool env)
      in 
  try 
    type_aux exty et
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
	  ignore(Gtypes.unify_env scp ty nt env); (* unify with given type *)
	  ty
	with Not_found -> 
	  raise (typingError "Typecheck: unknown identifier" [t] []))
    | Bound(q) -> 
	(let ty = get_binder_type t
	in
	Gtypes.quick_well_defined scp cache ty;
	ty)
    | Const(c) -> 
	  (let ty = Gtypes.typeof_cnst c
	  in
	  Gtypes.quick_well_defined scp cache ty;
	  ty)
    | Typed(trm, ty) -> 
	(Gtypes.quick_well_defined scp cache ty;
	 let nty = infer_aux (inf, cache) scp env trm
	 in 
	 ignore(Gtypes.unify_env scp ty nty env);
	 ty)
    | App(lf, a) -> 
	let aty = infer_aux (inf, cache) scp env a  (* get argument type *)
	and fty = infer_aux (inf, cache) scp env lf (* get function type *)
	in 
	let rty = Gtypes.mk_typevar inf
	in 
	let nty = Gtypes.mk_fun aty rty  (* make a dummy type *)
	in                               (* unify with actual f-type *)
	ignore(Gtypes.unify_env scp nty fty env);
	rty
      |	Qnt(q, b) ->
	  if Logicterm.is_lambda t
	  then 
	    (let rty = infer_aux (inf, cache) scp env b
	    and aty = Term.get_qnt_type t  (* domain *)
	    in
	    let nty = Gtypes.mk_fun aty rty (* type of term *)
	    in 
            Gtypes.quick_well_defined scp cache nty; (* check domain *)
	    nty)
	  else 
	    (let nty=infer_aux (inf, cache) scp env b
	    in 
	    ignore(Gtypes.unify_env scp nty Gtypes.mk_bool env);
	    nty)


let infer_types_env scp env trm =
  let inf = (ref 0, Lib.empty_env())
  in 
  infer_aux inf scp env trm

let infer_types scp trm =
  let inf = (ref 0, Lib.empty_env())
  in 
  infer_aux inf scp (Gtypes.empty_subst()) trm


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

