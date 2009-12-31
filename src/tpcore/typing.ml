(*----
 Name: typing.ml
 Copyright M Wahab 2005-2009
 Author: M Wahab  <mwb.cde@googlemail.com>

 This file is part of HSeq

 HSeq is free software; you can redistribute it and/or modify it under
 the terms of the Lesser GNU General Public License as published by
 the Free Software Foundation; either version 3, or (at your option)
 any later version.

 HSeq is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
 License for more details.

 You should have received a copy of the Lesser GNU General Public
 License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
----*)

open Basic
open Term

(*** Type Errors ***)

class typingError s t ty expty=
  object (self)
    inherit Report.error s
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
  Report.mk_error((new typingError s tr expty ty):>Report.error)
let add_typing_error s tr expty ty es =
  Report.add_error (typing_error s tr expty ty) es

(***
* Typing a term
***)

let typeof_env scp typenv inf trm =
  let rec typeof_aux t env =
    match t with
      Id(n, ty) -> (Gtypes.mgu ty env, env)
    | Free(n, ty) -> (Gtypes.mgu ty env, env)
    | Meta(q) -> (Gtypes.mgu (get_binder_type t) env, env)
    | Bound(q) -> (Gtypes.mgu (get_binder_type t) env, env)
    | Const(c) -> (Lterm.typeof_cnst c, env)
    | Qnt(q, b) -> 
	(match Basic.binder_kind q with
	  Basic.Lambda -> 
	    (let btyp, benv=typeof_aux b env
	    in 
	    (Lterm.mk_fun_ty (Term.get_binder_type t) btyp, benv))
	| _ -> Lterm.mk_bool_ty(), env)
    | App(f, a) -> 
	let fty, fenv= typeof_aux f env
	in let aty, aenv= typeof_aux a fenv
	and retty = Gtypes.mk_typevar inf
	in
	let nty = Lterm.mk_fun_ty aty retty
	in 
	let renv = Gtypes.unify_env scp fty nty aenv
	in 
	(Gtypes.mgu retty renv, renv) 
  in typeof_aux trm typenv

let typeof scp ?env t = 
  let tenv = 
    match env with
      None -> Gtypes.empty_subst()
    | Some x -> x
  in 
  let ty, _ = typeof_env scp tenv (ref 0) t
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
    | Meta(q) -> 
	(let ty = get_binder_type t
	in
	Gtypes.quick_well_defined scp cache ty;
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
	  raise (add_typing_error "Typechecking: " t 
	   (Gtypes.mgu expty env) (Gtypes.mgu ty env) err)))
    | Const(c) -> 
	(let ty = Lterm.typeof_cnst c
	in
	Gtypes.quick_well_defined scp cache ty;
	(try 
	  Gtypes.unify_env scp ty expty env
	with err -> 
	  raise (add_typing_error "Typechecking: " t 
	   (Gtypes.mgu expty env) (Gtypes.mgu ty env) err)))
    | App(f, a) -> 
	let aty = Gtypes.mk_typevar inf       (* make an argument type *)
	in
	let fty = Lterm.mk_fun_ty aty expty  (* expect a function type *)
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
	    let bty = Lterm.mk_fun_ty fty rty (* type of term *)
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
	    let env1=type_aux (Lterm.mk_bool_ty()) b env
	    in 
	    (try
	      Gtypes.unify_env scp expty (Lterm.mk_bool_ty()) env1
	    with err -> 
	      raise (add_typing_error "Typechecking: " t 
		       (Gtypes.mgu expty env) 
		       (Gtypes.mgu (Lterm.mk_bool_ty()) env) err)))
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

let typecheck scp t expty =
  ignore(typecheck_top scp (Gtypes.empty_subst()) t expty)


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
    | Meta(q) -> 
	(let ty = get_binder_type t
	in
	Gtypes.quick_well_defined scp cache ty;
	(try Gtypes.unify_env scp ty expty env
	with err -> 
	  raise (add_typing_error "Typechecking: "t
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
	(let ty = Lterm.typeof_cnst c
	in
	Gtypes.quick_well_defined scp cache ty;
	(try 
	  Gtypes.unify_env scp ty expty env
	with err -> 
	  raise (add_typing_error "Typechecking: " t
		   (Gtypes.mgu expty env) (Gtypes.mgu ty env) err)))
    | App(f, a) -> 
	let aty = Gtypes.mk_typevar inf       (* make an argument type *)
	in
	let fty = Lterm.mk_fun_ty aty expty  (* expect a function type *)
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
	    let bty = Lterm.mk_fun_ty fty rty (* type of term *)
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
		settype_aux (Lterm.mk_bool_ty()) b env
	      with err -> 
		raise (Term.add_term_error "Typechecking: " [t] err))
	    in 
	    (try
	      Gtypes.unify_env scp expty (Lterm.mk_bool_ty()) env1
	    with err -> 
	      raise (add_typing_error "Typechecking: " t
		       (Gtypes.mgu expty env) 
		       (Lterm.mk_bool_ty()) err)))
  in settype_aux exty et typenv

let settype scp ?env t=
  let inf = (ref 0)
  and cache =  Lib.empty_env()
  and f inf env expty trm = 
    (match trm with
      Id(n, ty) -> Gtypes.unify_env scp ty expty env
    | _ -> env)
  in 
  let tyenv =
    match env with 
      None -> (Gtypes.empty_subst()) 
    | Some(x) -> x
  in 
  settype_top scp (inf, cache) f 
    tyenv (Gtypes.mk_typevar inf) t

(** 
   [typecheck_env tyenv scp t ty]: Check, w.r.t type context [tyenv],
   that term [t] has type [ty] in scope [scp]. Type variables in [t]
   take their assigned value from [tyenv], if they have one.

   The type of an identifier [Id(n, ty)] is looked for in scope [scp].
   The given type [ty] is discarded.
*) 
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
  | x -> ()

