(*-----
   Name: defn.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

open Basic
  
(** Term and subtype definition *)

(***
* Term definition and declaration
***)

(*** Term declaration ***)

let mk_decln scp name ty =
  let check_exists () = 
    try 
      ignore(Scope.type_of scp name);
      raise (Term.term_error "Name exists in scope" 
	       [Term.mk_typed_ident name ty])
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
	       "Invalid declaration " [(Term.mk_typed_ident name ty)] err)
  in 
  (name, ret_ty)


(*** Term definition ***)

let rec mk_var_ty_list ls =
  match ls with
    [] -> []
  | (Basic.Id(n, ty)::ts) -> ((n, ty)::(mk_var_ty_list ts))
  | _ -> raise (Term.term_error "Non-variables not allowed" ls)

(*
let rec mk_all_from_list scp b qnts =
  match qnts with 
    [] -> b
  | (Basic.Free(n, ty)::qs) ->
      mk_all_from_list scp (Logicterm.mk_all_ty scp n ty b) qs
  | (Basic.Id(n, ty)::qs) ->
      raise (Term.term_error "mk_all_from_list, got a Basic.Id" qnts)
  | _ -> raise (Term.term_error "Invalid argument, mk_all_from_list" qnts)
*)

let rec mk_all_from_list scp b qnts =
  match qnts with 
    [] -> b
  | (Basic.Bound(q)::qs) ->
      if (Basic.binder_kind q) = Basic.All
      then mk_all_from_list scp (Term.mk_qnt q b) qs
      else 
	raise 
	  (Term.term_error "Invalid argument: wrong quantifier in argument"
	     qnts)
  | (Basic.Id(n, ty)::qs) ->
      raise (Term.term_error "mk_all_from_list, got a Basic.Id" qnts)
  | _ -> raise (Term.term_error "Invalid argument, mk_all_from_list" qnts)

let mk_defn scp (name, namety) args rhs = 
(*
  let nty = Gtypes.mk_var ("_"^(Basic.name name)^"_typ")
  in 
*)
  let nty = Gtypes.set_name scp namety
  in 
  let lhs = Term.mk_comb (Term.mk_typed_ident name nty) args
  in 
  let ndn0 = 
    mk_all_from_list scp 
      (Lterm.mk_equality lhs rhs)  
      (List.rev args) 
  in
  let ndn = Lterm.set_names scp ndn0
  in 
  let nscp = Scope.extend_with_terms scp [(name, nty)]
  in 
  let tenv= Typing.settype nscp ndn
  in 
  let tenv1=Typing.typecheck_top nscp tenv ndn (Lterm.mk_bool_ty())
  in 
  let nty1 = Gtypes.mgu_rename (ref 0) tenv1 (Gtypes.empty_subst()) nty
  in 
  (name, nty1, (Formula.make nscp (Term.retype tenv ndn)))

(*
let mk_defn scp name args rhs = 
  let args1=
    let memo = Lib.empty_env()
    in 
    List.map
      (fun (n, ty) -> 
	(n, Gtypes.set_name ~memo:memo scp ty)) args
  in 
  let ps = 
    List.map 
      (fun (x, y) -> Term.mk_free x y) args1
  in let rhs1=Term.set_names scp rhs
(*
  in let rty = Typing.typeof scp rhs1
*)
  in let nty = Gtypes.mk_var ("_"^(Basic.name name)^"_typ")
  in let lhs = Term.mk_comb (Term.mk_typed_ident name nty) ps
  in let ndn = 
    mk_all_from_list scp 
      (Lterm.mk_equality lhs rhs1) 
      (List.rev ps) 
  in
(*
  let rfrees = 
    match Term.get_free_vars ndn with
      [] -> ()
    | _ ->
	raise (Term.term_error 
		 "Free variables not allowed in definition" [ndn])
  in 
*)
  let nscp = Scope.extend_with_terms scp [(name, nty)]
  in 
  let tenv=Typing.settype nscp ndn
  in 
  let tenv1=Typing.typecheck_top nscp tenv ndn Lterm.mk_bool_ty
  in 
  (name, Gtypes.mgu_rename (ref 0) tenv1 (Gtypes.empty_subst()) nty, 
   (Formula.make nscp (Term.retype tenv ndn)))
*)

(***
* Type definition
***)

(*** Support functions ***)

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

(**
   [check_type_name scp n]: make sure that there is no type named [n]
   in scope [scp].
*)
let check_type_name scp n = 
  try
    (ignore(Scope.defn_of scp n);
     raise (Gtypes.type_error "Type already exists" 
	      [Gtypes.mk_constr n []]))
  with Not_found -> ()

let check_well_defined scp args ty= 
  (try 
    Gtypes.well_defined scp args ty;()
  with err ->
    raise (Gtypes.add_type_error "Badly formed type" [ty] err))


(*** Type definition: aliasing ***)


(** Subtype definition 

   Define the subtype of a type constructing
   [A, args, T, set:(args)T->bool, rep, abs]
   where 
   {- [A] is the name of the subtype.}
   {- [args] are the names of the types' parameters.}
   {- [T] is the representation type.}
   {- [rep] is the name of the representation function
   destructing type [A]}
   {- [abs] is the name of the abstraction function constructing
   type [A].}

   The types of the constructed functions are
   {- representation function:  [rep:(args)T -> A]}
   {- abstraction function: [abs:A-> (args)T]}
 
   The axioms specifying the abstraction and representation functions are:
   {- rep_T: |- !x: set (rep x)}
   {- rep_T_inverse: |- !x: abs (rep x) = x}
   {- abs_T_inverse: |- !x: (set x) => rep (abs x) = x}

   The names of the type, abstraction and representation functions are
   all parameters to the subtype definition.
*)

(*
   [subtype_defn]: The result of constructing a subtype. 

   The result of constructing a subtype:
   {- [id]: The name of the new type.}
   {- [args]: The names of the parameters to the type.}
   {- [rep]: The declaration of the representation function.}
   {- [abs]: The declaration of the abstraction function.}
   {- [set]: The term providing the defining predicate of the subtype.}
   {- [rep_T]: A term of the form |- !x: set (rep x)}
   {- [rep_T_inverse]: A term of the form |- !x: abs (rep x) = x}
   {- [abs_T_inverse]: A term of the form |- !x: (set x) => rep
   (abs x) = x}
*)
type subtype_defn = 
    {
     id: Ident.t;
     args : string list;
     rep : (Ident.t* Basic.gtype);
     abs: (Ident.t* Basic.gtype);
     set: Basic.term;
     rep_T: Basic.term;
     rep_T_inverse: Basic.term;
     abs_T_inverse: Basic.term
   }


(**
   [mk_subtype_exists setp]
   make the term << ?x. setp x >> to be used to show that a subtype exists.
*)
let mk_subtype_exists setp=
  let x_b=(Basic.mk_binding Basic.Ex "x" (Gtypes.mk_var "x_ty"))
  in 
  let x= Term.mk_bound x_b
  in 
  Basic.Qnt(x_b, Term.mk_app setp x)


(**
   [make_witness_type scp dtype setP]: 
   Construct the actual type of the defining set.
 *)
let make_witness_type scp dtype setP =
  let fty = Typing.typeof scp setP
  in 
  if not (Lterm.is_fun_ty fty)
  then 
    raise 
      (Term.add_term_error "Expected a function" [setP]
	 (Gtypes.type_error "Not a function type" [fty]))
  else 
    let tty = 
      Lterm.mk_fun_ty dtype (Lterm.mk_bool_ty())
    in 
    try 
      let sbs=Gtypes.unify scp fty tty
      in 
      Term.retype sbs setP
    with err -> 
      raise
	(Term.add_term_error "Badly typed term" [setP]
	   (Gtypes.add_type_error "Invalid type" [tty] err))


(**
   [mk_rep_T set rep]: 
   build 
   |- !x: set (rep x) 
*)
let mk_rep_T set rep =
  let x_b=Basic.mk_binding Basic.All "x" (Gtypes.mk_var "x_ty")
  in 
  let x = Term.mk_bound x_b
  in 
  let body = Term.mk_app set (Term.mk_app rep x)
  in 
  Basic.Qnt(x_b, body)

(**
   [mk_rep_T_inv rep abs]: 
   build 
   |- !x: (abs (rep x)) = x
*)
let mk_rep_T_inv rep abs=
  let x_b=Basic.mk_binding Basic.All "x" (Gtypes.mk_var "x_ty")
  in 
  let x = Term.mk_bound x_b
  in 
  let body = 
    Lterm.mk_equality 
      (Term.mk_app abs (Term.mk_app rep x)) x
  in 
  Basic.Qnt(x_b, body)

(**
   [mk_abs_T_inv set rep abs]:
   build 
   |- !x: (set x)=> (rep (abs x)) = x
*)
let mk_abs_T_inv set rep abs=
  let x_b=Basic.mk_binding Basic.All "x" (Gtypes.mk_var "x_ty")
  in 
  let x = Term.mk_bound x_b
  in 
  let lhs = Term.mk_app set x
  and rhs = 
    Lterm.mk_equality 
      (Term.mk_app rep (Term.mk_app abs x)) x
  in 
  let body = Lterm.mk_implies lhs rhs
  in 
  Basic.Qnt(x_b, body)
      

(**
   [mk_subtype scp name args d setP rep]: Make a subtype.

   - check name doesn't exist already
   - check all arguments in args are unique
   - check def is well defined 
   (all constructors exist and variables are in the list of arguments)
   - ensure setP has type (d -> bool)
   - declare rep as a function of type (d -> n)
   - make subtype property from setp and rep.
 *)
let mk_subtype scp name args dtype setP rep_name abs_name=
  let th = Scope.thy_of scp
  in 
  let id = Ident.mk_long th name
  and rep_id = Ident.mk_long th rep_name
  and abs_id = Ident.mk_long th abs_name
  in 
  let ntype = 
    Gtypes.mk_constr id (List.map Gtypes.mk_var args)
  in
  check_type_name scp id;
  check_args_unique args;
  check_well_defined scp args dtype;
  let setp0=Formula.term_of (Formula.make scp setP)
  in 
  let new_setp = make_witness_type scp dtype setp0
  in 
  let rep_ty = Gtypes.normalize_vars (Lterm.mk_fun_ty ntype dtype)
  and abs_ty = 
    Gtypes.rename_type_vars
      (Gtypes.normalize_vars (Lterm.mk_fun_ty dtype ntype))
  in 
  let abs_term = Term.mk_typed_ident abs_id (Gtypes.mk_var "abs_ty2")
  and rep_term = Term.mk_typed_ident rep_id (Gtypes.mk_var "rep_ty2")
  in 
  let rep_T_thm = mk_rep_T setP rep_term
  and rep_T_inv_thm = mk_rep_T_inv rep_term abs_term
  and abs_T_inv_thm = mk_abs_T_inv setP rep_term abs_term
  in 
  { 
    id=id;
    args = args;
    rep = (rep_id, rep_ty); abs=(abs_id, abs_ty);
    set = new_setp;
    rep_T=rep_T_thm;
    rep_T_inverse= rep_T_inv_thm; abs_T_inverse=abs_T_inv_thm
  }


module HolLike =
  struct

(*
 * HOL-like type definition. 
 *  A, args, T, set:(args)T->bool
 * 
 *  make declaration
 *   representation function rep = name:(args)T -> A
 *   and theorem
 *   |- ((!x1 x2: (((rep x1) = (rep x2)) => (x1 = x2)))
 *       and (!x: (P x) = (?x1: x=(rep x1))))
 *
 * Everything needed to use subtyping is derived making this approach
 * the more intellectually rigorous. But this takes a lot of work,
 * so use the easy way out.
 * 
 *)

(*
   [mk_subtype_property setp rep]:
   make the term 
   << (!x1 x2: (((rep x1) = (rep x2)) => (x1 = x2)))
   and 
   (!x: (P x) = (?x1: x=(rep x1)))>>
   to be used as the subtype theorem.
 *)
(*
   let mk_subtype_prop (setP: Basic.term) (rep: Ident.t)=
   let typedef_term = 
   Term.mk_ident (Ident.mk_long Lterm.base_thy "Type_Def") 
   and rep_term = Term.mk_typed_ident rep (Gtypes.mk_var "rep_ty1")
   in 
   Term.mk_app (Term.mk_app typedef_term setP) rep_term
 *)

    let mk_subtype_prop (setP: Basic.term) (rep: Ident.t)=
      let mk_subtype_1 (rep: Ident.t)=
	let x1_b=Basic.mk_binding Basic.All "x1" (Gtypes.mk_var "x1_ty")
	and x2_b=Basic.mk_binding Basic.All "x2" (Gtypes.mk_var "x2_ty")
	in 
	let x1= Term.mk_bound x1_b
	and x2= Term.mk_bound x2_b
	and rep_term = Term.mk_typed_ident rep (Gtypes.mk_var "rep_ty1")
	in 
	let lhs =
	  Lterm.mk_equality (Term.mk_app rep_term x1) 
	    (Term.mk_app rep_term x2)
	and rhs = Lterm.mk_equality x1 x2
	in 
	let body = Lterm.mk_implies lhs rhs
	in 
	Basic.Qnt(x1_b, Basic.Qnt(x2_b, body))
      and 
	  mk_subtype_2 (setP:Basic.term) (rep: Ident.t)=
	let y_b=(Basic.mk_binding Basic.All "y" (Gtypes.mk_var "y_ty"))
	and y1_b=(Basic.mk_binding Basic.Ex "y1" (Gtypes.mk_var "y1_ty"))
	in 
	let y= Term.mk_bound y_b
	and y1= Term.mk_bound y1_b
	and rep_term = Term.mk_typed_ident rep (Gtypes.mk_var "rep_ty2")
	in 
	let lhs = Term.mk_app setP y
	and rhs =
	  Basic.Qnt(y1_b, 
		    (Lterm.mk_equality y (Term.mk_app rep_term y1)))
	in 
	Basic.Qnt(y_b, 
		  Lterm.mk_equality lhs rhs)
      in 
      Lterm.mk_and (mk_subtype_1 rep) (mk_subtype_2 setP rep)


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
    let mk_subtype scp name args dtype setP rep=
      let th = Scope.thy_of scp
      in 
      let id = Ident.mk_long th name
      in 
      let ntype = 
	Gtypes.mk_constr id (List.map Gtypes.mk_var args)
      in
      check_type_name scp id;
      check_args_unique args;
      check_well_defined scp args dtype;
      let new_setp = make_witness_type scp dtype setP
      in 
      let rep_ty = Gtypes.normalize_vars (Lterm.mk_fun_ty ntype dtype)
      in 
(*
      let nscp = Scope.extend_with_typedeclns scp [(id, args)]
      in 
*)
      let subtype_prop = mk_subtype_prop setP rep
      in 
      (rep_ty, new_setp, subtype_prop)

  end


(***
* Retired code
***)

(* decln: type of declarations. *)
(* type decln *)
(*
 defn: the ocaml type of a definition 
   This is used only for prettyprinting.
   (The Logic.thm describing the definition is an abstracted type)
*)
(*
type defn = Defn of (Ident.t * Basic.gtype * Logic.thm)
*)

(* destructors *)
(*
val dest_decln : decln -> Ident.t * Basic.gtype
val dest_defn : defn -> (Ident.t * Basic.gtype * Logic.thm)
*)

(*
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
*)

(*
let mk_defn_type env atys rty rfrs = 
  match atys with
    [] -> rty
  | ts -> 
      (Lterm.mk_fun_ty_from_list
	 (List.map
	    (fun (x, ty) -> 
 	      Gtypes.mgu  
		(try 
		  (List.assoc x rfrs)
		with Not_found -> ty) env)
	    ts) rty)
(** [mk_defn_type tyenv arg_tys rty frees]
   Construct the type of a defined term.  Forms the type [a1 -> a2 ->
   .. -> rty] where [arg_tys = [a1; a2; ..]].  

   [frees] is an association list of names and types, making type
   variables with the same name be the same type.
 *)
*)

(*
let rec check_free_vars tyenv name ls = 
  match ls with
    [] -> []
  | (n, ty) :: fvs -> 
      if n = name 
      then check_free_vars tyenv name fvs
      else (ignore(Scope.type_of tyenv n); 
	    check_free_vars tyenv name fvs)
*)	  
