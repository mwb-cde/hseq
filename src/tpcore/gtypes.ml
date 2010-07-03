(*----
  Name: gtypes.ml
  Copyright M Wahab 2005-2009, 2010
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
open Lib
open Report

(*
 * Basic Operations
 *)

(* Equality *)
let rec equals a b = 
  let struct_equals p = 
    match p with
      | (Var(v1), Var(v2)) -> v1 == v2
      | (Constr(f1, args1), Constr(f2, args2)) ->
        let test_args () = 
	  List.iter2 
	    (fun a b -> 
	      if (equals a b) 
	      then () 
	      else raise (Failure ""))
	    args1 args2;
          true
        in
        (f1 = f2) & 
          (try test_args () with _ -> false)
      | (WeakVar(v1), WeakVar(v2)) -> v1 == v2
      | (x, y) -> x = y
  in 
  (a == b) || (struct_equals (a, b))

(* Recognisers *)

let is_var t = 
  match t with
    | Var _ -> true
    | _ -> false

let is_constr t = 
  match t with
    | Constr _ -> true
    | _ -> false

let is_weak t = 
  match t with
    | WeakVar _ -> true
    | _ -> false

(* Constructors *)

let mk_var n = Var(ref n)
let mk_weak n = WeakVar(ref n)
let mk_constr f l = Constr(f, l)

(* Destructors *)

let dest_var t =
  match t with 
    | Var(n) -> n
    | _ -> raise (Failure "Not a variable")

let get_var_name t = 
  match t with 
    | Var(n) -> !n
    | _ -> raise (Failure "Not a variable")

let dest_weak t =
  match t with 
    | WeakVar(n) -> n
    | _ -> raise (Failure "Not a weak variable")

let get_weak_name t = 
  match t with 
    | WeakVar(n) -> !n
    | _ -> raise (Failure "Not a weak variable")

let dest_constr ty = 
  match ty with 
    | Constr(f, args) -> (f, args)
    | _ -> raise (Failure "Not a constructed type")

(*
 * Specialised Manipulators 
 *)

(* Variable types *)
let is_any_var t = (is_var t) || (is_weak t)

let mk_typevar n =
  let nty = Var(ref(int_to_name (!n)))
  in
  n := (!n) + 1; nty

let get_var_names ty = 
  let seen = Lib.empty_env() in 
  let rec get_aux names typ =
    match typ with
      | Var n -> 
	if Lib.member (!n) seen
	then names
	else (ignore(Lib.bind (!n) true seen); (!n)::names)
      | Constr(_, args) -> List.fold_left get_aux names args
      | _ -> names
  in 
  get_aux [] ty 

(**
   [normalize_vars ty]: Make all type variables in [ty] with the same
   string name be the same variable.

   Useful when constructing types from existing types.
*)
let normalize_vars typ=
  let lookup tbl str =
    try (tbl, Lib.find str tbl)
    with Not_found -> 
      let nvar = mk_var str
      in 
      (Lib.bind str nvar tbl, nvar)
  in 
  let rec norm_aux tbl ty =
    match ty with
      | Var(n) -> 
	  let (tbl1, n1) = lookup tbl (get_var_name ty)
	  in 
          (tbl1, n1)
      | Constr(n, args) ->
	let (tbl1, args1) = norm_list tbl args []
	in 
	(tbl1, Constr(n, args1))
      | _ -> (tbl, ty)
  and norm_list tbl tys result = 
    match tys with 
      | [] -> (tbl, List.rev result)
      | (ty::xs) -> 
	let (tbl1, ty1) = norm_aux tbl ty
	in 
	norm_list tbl1 xs (ty1::result)
  in 
  let (_, typ1) = norm_aux (Lib.empty_env()) typ
  in 
  typ1

(* Unnamed type variables *)

let null_type_name = ""
let mk_null () = mk_var null_type_name
let is_null t = ((get_var_name t) = null_type_name)

(* Named typed constructors *)

let is_def t = 
  match t with
    | Constr(_ , _) -> true
    | _ -> false

let mk_def n args = mk_constr n args

let dest_def t = 
  match t with
    | Constr(n, args) -> (n, args)
    | _ -> raise (Failure "Not a defined type")

(*
 * Type definitions 
 *)

(* [typedef_record]: Records for type definitions/declarations *)
type typedef_record = Scope.type_record

(* Functions to access type definitions *)
let get_typdef scp r =  Scope.defn_of scp r


(***
    * Data storage indexed by gtypes
***)

(** [('a)tree]: Balanced trees indexed by gtypes *)
module TypeTreeData =
struct 
  type key = gtype
  let equals = equals
  let lessthan x y = (Pervasives.compare x y) < 0
end
module TypeTree=Treekit.BTree(TypeTreeData)      
type ('a)tree = ('a)TypeTree.t

(*  [('a)table]: Hash tables indexed by gtypes *)
module type RHASHKEYS =
sig 
  type t = gtype
  val equal: t -> t -> bool
  val hash: t -> int
end

module Rhashkeys:RHASHKEYS =
struct
  type t = gtype
  let equal = equals
  let hash= Hashtbl.hash
end
module type RHASH = (Hashtbl.S with type key = (gtype))
module Rhash:RHASH = Hashtbl.Make(Rhashkeys)
type ('a)table = ('a)Rhash.t

(*
 *   Substitution.
 *)

type substitution = (gtype)tree

let lookup t env = TypeTree.find env t
let member t env = 
  try lookup t env ; true
  with Not_found -> false

let subst_sz s = TypeTree.nil
let empty_subst () = TypeTree.nil
let bind t r env = TypeTree.replace env t r
let delete t env = TypeTree.delete env t
let subst_iter = TypeTree.iter

let bind_var t r env = 
  if is_any_var t 
  then bind t r env 
  else 
    raise (Failure "bind_var: Can't bind a non variable")

let rec subst t env =
  try lookup t env
  with Not_found -> 
    (match t with 
      | Constr(f, l) -> 
	Constr(f, List.map (fun x-> subst x env) l)
      | Var _ -> t
      | WeakVar _ -> t)
	  

(*
 * Operations which need substitution.
*)

(*
  Renaming type variables.

  [rename_type_vars_env env t]: make a copy of type t, which differs
  from [t] only in the names of variables. Use [env] as the store of
  previously renamed variables.

  [rename_type_vars t]: make a copy of type t, which differs from [t] only
  in the names of variables.
*)
let rec rename_type_vars_env env trm =
  match trm with
   | Var(x) -> 
        (try (lookup trm env, env)
         with Not_found -> 
	   let nt = mk_var (!x)
	   in 
	   (nt, bind trm nt env))
    | Constr(f, args) ->
      let rename_arg e (t: Basic.gtype) = 
        let (t1, e1) = rename_type_vars_env e t 
        in 
        (e1, (t1: Basic.gtype))
      in
      let (nenv, (nargs: Basic.gtype list)) = 
        Lib.fold_map rename_arg env args
      in 
      (Constr(f, nargs), nenv)
    | WeakVar(x) -> 
      (try (lookup trm env, env)
       with Not_found -> (trm, env))

let rename_type_vars t =
  let (nty, _) = rename_type_vars_env (empty_subst()) t
  in 
  nty

(*
 * Pretty printing
*)

type printer_info =
    { 
      tbl: substitution; (* used to store pretty replacement variable names *)
      ctr: int ref; (* used to generate variable names *)
    }

let empty_printer_info () =
  { tbl=empty_subst(); ctr=ref 0 }

let pplookup ppstate id =
  try Printer.get_record (ppstate.Printer.types) id
  with Not_found -> 
    Printer.mk_record 
      Printer.default_type_prec
      Printer.default_type_fixity
      None

let print_bracket = Printer.print_assoc_bracket

let rec print_type ppstate pr t = 
  let print_aux ppstate pr x =
    match x with
      | Var(_) -> Format.printf "@[<hov 2>'%s@]" (get_var_name x)
      | WeakVar(_) -> Format.printf "@[<hov 2>_%s@]" (get_weak_name x)
      | Constr(op, args) -> print_defined ppstate pr (op, args)
  in 
  print_aux ppstate pr t;
and print_defined ppstate (assoc, prec) (f, args) =
  let pprec = pplookup ppstate f in 
  let nfixity = pprec.Printer.fixity in 
  let (nassoc, nprec) = (nfixity, pprec.Printer.prec)
  in 
  try 
    let printer = Printer.get_printer (ppstate.Printer.types) f
    in 
    printer (assoc, prec) (f, args)
  with Not_found -> 
    if(Printer.is_infix nfixity)
    then 
      (match args with
	| [] -> ()
        | (lf::lr::rs) -> 
	  Format.printf "@[<hov 2>";
	  print_bracket (assoc, prec) (nassoc, nprec)
	    "(";
	  print_type ppstate (Printer.infixl, nprec) lf;
	  Printer.print_space();
	  Printer.print_identifier (pplookup ppstate) f;
	  Printer.print_space();
	  print_type ppstate (Printer.infixr, nprec) lr;
	  Printer.print_list 
	    (print_type ppstate (nassoc, nprec), 
	     Printer.print_space) 
	    rs; 
	  print_bracket (assoc, prec) (nassoc, nprec) ")";
	  Format.printf "@]"
        | (lf::rs) -> 
	  Format.printf "@[<hov 2>";
	  print_bracket (assoc, prec) (nassoc, nprec)
	    "(";
	  print_type ppstate (Printer.infixl, nprec) lf;
	  Printer.print_space();
	  Printer.print_identifier (pplookup ppstate) f;
	  Printer.print_space();
	  Printer.print_list 
	    (print_type ppstate (nassoc, nprec), 
	     Printer.print_space) 
	    rs; 
	  print_bracket (assoc, prec) (nassoc, nprec) ")";
	  Format.printf "@]")
    else 
      if (Printer.is_suffix nfixity)
      then 
	(Format.printf "@[<hov 2>";
	 print_bracket (assoc, prec) (nassoc, nprec) "(";
	 Printer.print_suffix 
	   ((fun pr -> Printer.print_identifier (pplookup ppstate)), 
	    (fun pr l-> 
	      match l with 
		| [] -> ()
	        | _ -> 
                  Printer.print_sep_list
		    (print_type ppstate (assoc, pr), ",") l))
	   nprec (f, args);
	 print_bracket (assoc, prec) (nassoc, nprec) ")";
	 Format.printf "@]")
      else 
	(Format.printf "@[<hov 2>";
	 (match args with 
	   | [] -> ()
	   | _ -> 
	     (Printer.print_string "(";
	      Printer.print_sep_list 
		(print_type ppstate (assoc, prec), ",") args;
	      Format.printf ")@,"));
	 Printer.print_identifier (pplookup ppstate) f;
	 Format.printf "@]")

let print ppinfo x =
  print_type ppinfo 
    (Printer.default_type_fixity, Printer.default_type_prec) x

(*
 * Error handling 
*)

class typeError s ts =
object (self)
  inherit Report.error s
  val trms = (ts: gtype list)
  method get() = ts
  method print st = 
    Format.printf "@[%s@ " (self#msg()); 
    Printer.print_sep_list 
      (print st, ",") (self#get());
    Format.printf "@]"
end
let type_error s t = mk_error((new typeError s t):>error)
let add_type_error s t es = raise (add_error (type_error s t) es)

(* String representation of a type *)

let rec string_gtype x =
  match x with
    | Var(a) -> "'"^(!a)
    | Constr(f, args) ->  
      string_tconst f (List.map string_gtype args)
    | WeakVar(a) -> "_"^(!a)

(*
 * Support functions to deal with type definitions.
*)

(**
   [rewrite_subst t env]: Rewrite [t], using substitution [env].  

   Only used to rewrite the rhs of a definition, instantiating its
   variables with the values of the given arguments.

   [rewrite_defn args params t]: Rewrite [t], substituting arguments
   [args] for the parameters [params] of the type definition.
*)
let rec rewrite_subst t env =
  match t with 
    | Var(a) -> 
      (try Lib.find (!a) env 
       with _ -> raise (type_error "rewrite_subst: Can't find parameter" [t]))
    | Constr(f, l) -> Constr(f, List.map (fun x-> rewrite_subst x env) l) 
    | _ -> t

let rewrite_defn given_args rcrd_args t=
  if (List.length rcrd_args) = (List.length given_args)
  then 
    let tenv = Lib.empty_env() in 
    let env_of_args() = 
       List.iter2 
	 (fun x y -> Lib.bind_env x y tenv) 
	 rcrd_args given_args
    in 
    (env_of_args(); 
     rewrite_subst t tenv)
  else 
    raise (type_error "rewrite_defn: Wrong number of arguments" [t])

let unfold scp t = 
  match t with
    | Constr(n, args) ->
        let recrd = get_typdef scp n 
        in 
        (match recrd.Scope.alias with
	  | None -> raise Not_found
          | Some(gt) -> rewrite_defn args (recrd.Scope.args)  gt)
    | _ -> raise Not_found

(**
   [has_defn scp n]: true if [n] has a definition in scope [scp] (and
   is therefore an alias for a type).
*)
let has_defn tyenv n = 
  (try 
     (match (get_typdef tyenv n).Scope.alias with 
         None -> false 
       | _ -> true)
   with Not_found -> false)

(*
 * Consistency tests.
 * Weak variables are not permitted in any definition (type or term).
*)

(*
  [check_term n vs t]: for definition [(n vs) = t], test name [n] and
  arguments [vs] for definition [t].

  Fails if a variable occurs in [t] which is not in the list [vs] or name
  [n] occurs in [t] (a recursive definition)

  [check_args args]: test that each [a] in [args] is a variable and
  occurs only once.

  Used by [check_defn].
*)
let rec check_term scp n vs t = 
  match t with
    | Var(x) -> 
      if List.mem (Var x) vs 
      then () 
      else raise Not_found
    | Constr(m, args) -> 
      if n = m 
      then raise Not_found
      else List.iter (check_term scp n vs) args
    | WeakVar _ -> raise Not_found

let check_args args =
  let arg_itr env x =
    if (is_var x) & (not (member x env))
    then (bind x true env)
    else raise Not_found
  in
  try
    ignore(List.fold_left arg_itr (empty_subst()) args); 
    true
  with Not_found -> false

(**
   [check_defn l r]: test definition of [l] as alias for [r]

   fails if the type is already defined,
   or if the definition is recursive
   or if the arguments occur on the lhs more than once
   or if there are variables in the rhs which are not in the arguments
*)
let check_defn  scp l r =
  if (is_def l) then 
    let n, largs = dest_def l 
    in 
    if has_defn scp n
    then false
    else 
      if check_args largs
      then 
        try (check_term scp n largs r); true
	with Not_found -> false
      else false
  else false

(**
   [check_decln l]: consistency check on declaration of type [l]
*)
let check_decln l =
  if is_def l
  then 
    let n, largs = dest_def l 
    in 
    try (ignore(check_args largs); true)
    with Not_found -> false
  else false

(**
   [well_defined scp args t]: ensure that all constructors in [t] are
   declared.
   [args]: check variables are in the list of args
*)
let rec well_defined scp args t =
  let lookup_var x = List.find (fun y -> x = y) args in 
  let rec well_def t = 
    match t with 
      | Constr(n, nargs) ->
	  (try 
             ignore(Scope.defn_of scp n);
	     List.iter well_def nargs
	   with Not_found -> 
	     raise (type_error "well_defined: " [t]))
      | Var(v) -> 
	(try ignore(lookup_var (!v))
	 with Not_found -> 
	   raise (type_error "well_defined, unexpected variable." [t]))
      | WeakVar(v) -> 
	raise (type_error "well_defined, unexpected weak variable." [t])
  in 
  well_def t

(**
   [quick_well_defined]: memoised, simpler version of well_defined without
   argument testing
*)
let rec quick_well_defined scp cache t =
  match t with 
    | Constr(n, args) ->
      let nargs = List.length args in
      let rslt = Lib.try_find (Hashtbl.find cache) (n, nargs)
      in
      (match rslt with
	| Some _ -> List.iter (quick_well_defined scp cache) args
	| None -> 
	  (match Lib.try_find (get_typdef scp) n with
	    | Some(recrd) ->
	      if nargs = (List.length recrd.Scope.args)
	      then 
		(Hashtbl.add cache (n, nargs) true; 
		 List.iter (quick_well_defined scp cache) args)
	      else raise 
		(Invalid_argument ("quick_well_defined: "^(string_gtype t)))
	    | None -> 
	      raise (Invalid_argument 
		       ("quick_well_defined, not found: "
			^(Ident.string_of n)
			^" in "^(string_gtype t)))))
    | _ -> ()

(**
   [check_decl_type scp ty]: Ensure type [ty] is suitable for
   a declaration.

   Fails if [ty] contains a weak variable.
*)
let check_decl_type scp ty =
  let rec check_aux t = 
    match t with 
      | Constr(n, nargs) ->
	  (try 
             ignore(Scope.defn_of scp n);
	     List.iter check_aux nargs
	   with Not_found -> raise (type_error "Invalid type" [t]))
      | WeakVar(v) -> 
	raise (type_error "Invalid type, unexpected weak variable." [t])
      | _ -> ()
  in 
  check_aux ty


(*
 * Unification
 *)

exception Occurs
exception Unify
exception Match

let lookup_var ty env =
  let rec chase t =
    if is_any_var t
    then
      try chase (lookup t env)
      with Not_found -> t
    else t
  in chase ty

let rec occurs ty1 ty2 =
  match (ty1, ty2) with
    | (Var(_), Constr(f, l)) ->  List.iter (occurs ty1) l
    | (WeakVar(_), Constr(f, l)) ->  List.iter (occurs ty1) l
    | (_, _) ->
      if equals ty1 ty2
      then raise (type_error ("occurs: ") [ty1;ty2])
      else ()

let rec occurs_env tenv ty1 ty2 =
  let nty1 = lookup_var ty1 tenv
  and nty2 = lookup_var ty2 tenv
  in 
  match (nty1, nty2) with
    | (Var(_), Constr(f, l)) ->  List.iter (occurs_env tenv nty1) l
    | (Var(_), _)->
      if (equals nty1 nty2)
      then raise (type_error ("occurs: ") [nty1;nty2])
      else ()
    | (WeakVar(_), Constr(f, l)) ->  List.iter (occurs_env tenv nty1) l
    | (WeakVar(_), _)->
      if equals nty1 nty2
      then raise (type_error ("occurs: ") [nty1;nty2])
      else ()
    | (Constr(_, l), _) -> 
      List.iter (fun x-> occurs_env tenv x nty2) l

let bind_occs t1 t2 env =
  if is_any_var t1
  then 
    let r1 = lookup_var t1 env
    and r2 = (lookup_var t2 env)
    in 
    (occurs_env env r1 r2; bind_var r1 r2 env)
  else 
    raise (type_error "bind_occs: Can't bind a non variable" [t1; t2])

(*
 * The main unification functions
 *)

(**
   [unify_env scp t1 t2 nenv]: Unify types [t1] and [t2], adding new
   bindings to substitution nenv.
   
   Variables in both [t1] and [t2] are available for binding.

   Scope scp is used to look up definitions of constructors occuring
   in t1 or t2 (if necessary).
*)
let unify_env scp t1 t2 nenv =  
  let rec unify_aux ty1 ty2 env =
    let s = lookup_var ty1 env
    and t = lookup_var ty2 env
    in 
    match (s, t) with
      (* Constructors *)
      | (Constr(f1, args1), Constr(f2, args2)) ->
        let expand_left x y = 
	  let x1 = unfold scp x
	  in 
          unify_aux x1 y env
        and expand_right x y = 
	  let y1 = unfold scp y
	  in
          unify_aux x y1 env
        in
	  if f1=f2   
	  then 
            (* Matching constructors. *)
	    (try 
	       List.fold_left2
	         (fun ev x y -> unify_aux x y ev)
	         env args1 args2
	       with x -> add_type_error "Can't unify types" [s; t] x)
	   else 
              (* Different constructors, try for type aliasing. *)
	      (try 
	         (try expand_left s t
	          with _ -> expand_right s t)
	       with x ->(add_type_error "x: Can't unify types" [s; t] x))
      (* Variables, bind if not equal, but test for occurence *)
      | (Var(_), Var(_)) -> 
	if equals s t 
	then env
	else bind_occs s t env
      | (Var(_), _) -> bind_occs s t env
      | (_, Var(_)) -> bind_occs t s env
      (* Weak variables, don't bind to variables *)
      | (WeakVar(_), WeakVar(_)) -> 
	if equals s t 
	then env
	else bind_occs s t env
      | (WeakVar(_), _) -> bind_occs s t env
      | (_, WeakVar(_)) -> bind_occs t s env
  in
  unify_aux t1 t2 nenv

(**
   [unify scp t1 t2]: Unify types t1 and t2, returning the
   substitution needed.

   @raise [type_error] if unification fails
*)
let unify scp t1 t2 = unify_env scp t1 t2 (empty_subst())

(*
 * Most general unifiers.
 *)

(**
   [mgu t env]: Get most general unifier for type [t] and substitution
   [env]. Replaces variables in [t] with their bindings in [env].
*)
let rec mgu t env =
  match t with 
    | Var(a) -> 
      let nt = lookup_var t env 
      in 
      if is_var nt 
      then nt 
      else mgu nt env
    | WeakVar(a) -> 
      let nt = lookup_var t env 
      in 
      if is_any_var nt 
      then nt 
      else mgu nt env
    | Constr(f, l) -> 
      Constr(f, List.map (fun x-> mgu x env) l)

(**
   [mgu_rename_env inf env env nenv typ]: Replace variables in [typ]
   with their bindings in substitution [env].  If a variable isn't bound
   in [env], then it is renamed and bound to that name in [nenv] (which is
   checked before a new name is created).
*)
let mgu_rename_env inf tyenv name_env typ =
  let new_name_env nenv x =
    try (lookup x nenv, nenv)
    with Not_found ->
      let newty = mk_typevar inf
      in 
      (newty, bind_var x newty nenv)
  in 
  let rec rename_aux (nenv: substitution) ty =
    match ty with
      | Var(_) ->
	let nt = lookup_var ty tyenv
	in 
	if (equals ty nt)
	then new_name_env nenv nt
	else rename_aux nenv nt
      | WeakVar(_) ->
	let nt=lookup_var ty tyenv
	in 
	if (is_weak nt) 
        then (nt, nenv)
	 else rename_aux nenv nt
      | Constr(f, args) -> 
        let rename_arg renv (arg: Basic.gtype) = 
          let (narg, ne) = rename_aux renv arg
          in 
          (ne, narg)
        in
        let (nenv1, nargs) = Lib.fold_map rename_arg nenv args
        in 
        (Constr(f, nargs), nenv1)
  in 
  rename_aux name_env typ

let mgu_rename inf env nenv typ =
  let nty, _ = mgu_rename_env inf env nenv typ
  in
  nty

(* Matching
 * 
 * Matching is a form of unification in which only the variables in one
 * of the two type arguments can be bound.
 *)

(**
   [matching_env scp t1 t2 nenv]: Like unify_env but only variables in
   type [t1] can be bound.
*)
let matching_env scp env t1 t2 =  
  let rec match_aux ty1 ty2 env =
    let s = lookup_var ty1 env
    and t = lookup_var ty2 env
    in 
    match (s, t) with
      | (Constr(f1, args1), Constr(f2, args2)) ->
	if f1 = f2
	then 
          (try List.fold_left2
	         (fun ev x y -> match_aux x y ev) 
	         env args1 args2
	   with x -> add_type_error "Can't match types" [s; t] x)
	else 
	  (try match_aux (unfold scp s) t env
           with _ -> 
             (try (match_aux s (unfold scp t) env)
              with x -> (add_type_error "Can't match types" [s; t] x)))
      | (Var(_), _) ->
	if equals s t 
	then env
	else bind_occs s t env
      | (_, Var(_)) -> env
      | (WeakVar(_), _) ->
	if equals s t 
	then env
	else bind_occs s t env
      | _ -> 
        if equals s t
        then env
        else (raise (type_error "Can't match types" [s; t]))
  in
  try match_aux t1 t2 env (* try to match t1 and t2 *)
  with x -> raise (type_error "Can't match types" [t1; t2])

let matches_env scp tyenv t1 t2 = 
  try matching_env scp tyenv t1 t2
  with _ -> tyenv

let matches scp t1 t2=
  try ignore(matching_env scp (empty_subst()) t1 t2); true
  with _ -> false

(**
   [matches_rewrite scp tyl tyr env]: Match type [tyl'] with [tyr] in
   given context [env], where [tyl' = rename_type_vars tyl]. If [sb]
   is the returned substitution, then the type formed by [mgu tyr sb]
   will not have any type variable in common with [tyl]. Only
   variables in [tyl'] can be bound.
*)
type match_data =
    {
      vars: substitution; (** The unification variables *)
      tyenv: substitution; (** The type environment being built-up *)
    }

let null_ty = mk_null()

let vbind s t env =
  if (member s env.vars)
  then { env with tyenv=bind_occs s t env.tyenv }
  else env

(* Note that it is no longer necessary to use matches_rewrite and it
   can now be replaced with matches_env. *)
let matches_rewrite scp t1 t2 data = 
  let rec matches_aux ty1 ty2 env =
    let s = lookup_var ty1 env.tyenv
    and t = lookup_var ty2 env.tyenv
    in 
    match (s, t) with
      | Constr(f1, args1), Constr(f2, args2) ->
	if f1 = f2
	then 
          (try List.fold_left2 
                 (fun ev x y -> matches_aux x y ev) 
	         env args1 args2
	   with _ -> raise (type_error "Can't match types: " [s; t]))
	else 
          (try matches_aux (unfold scp s) t env
	   with _ ->
	     (try matches_aux s (unfold scp t) env
	      with _ -> raise (type_error "Can't match types: " [s; t])))
      | (Var(_), _) ->
	if equals s t 
        then env
	else 
	  (try vbind s t env
	   with err -> raise (add_type_error "Can't match types: " [s; t] err))
      | (WeakVar(_), _) -> 
	if equals s t 
	then env
	else 
          (try vbind s t env
	   with err -> raise (add_type_error "Can't match types: " [s; t] err))
      (* Match constants *)
      | (_, _) -> 
	if equals s t 
	then env
	else raise (type_error "Can't match types: " [s; t])
  in
  matches_aux t1 t2 data


(*
 * More functions
 *)

(** [set_name ?strict ?memo scp typ]: Set names in type [typ] to their
    long form.

    If [strict=true], fail if any type name doesn't occur in scope [scp].
*)
let set_name ?(strict=false) ?(memo=Lib.empty_env()) scp trm = 
  let lookup_id n = 
    try Lib.find n memo
    with Not_found -> 
      let nth =
	try Scope.thy_of_type scp n 
	with Not_found -> 
	  if strict
	  then raise  (type_error "Type doesn't occur in scope" 
		         [mk_def (Ident.mk_name n) []])
	  else Scope.thy_of scp
      in 
      ignore(Lib.add n nth memo); nth
  in 
  let rec set_aux t =
    match t with
      | Constr(id, args) -> 
	let (th, n) = Ident.dest id in 
	let nth = 
          if th = Ident.null_thy
	  then lookup_id n
	  else th
	in 
	let nid = Ident.mk_long nth n
	in 
        Constr(nid, List.map set_aux args)
      | _ -> t
  in set_aux trm

(**
   [in_scope memo scp ty]: Check that [ty] is in scope by checking
   that every type constructor is decared or defined in scope [scp].

   The function is memoised: if a constructor name is found to be 
   in scope, it is added to [memo].
*)
let in_scope memo scp ty =
  let lookup_id n = 
    try Lib.find n memo
    with Not_found -> 
      if Scope.in_scope scp n
      then Lib.add n true memo
      else raise Not_found
  in
  let rec in_scp_aux t =
    match t with
      | Var(_) -> ()
      | Constr(id, args) ->
	ignore(lookup_id (Ident.thy_of id));
	List.iter in_scp_aux args
      | WeakVar _ -> ()
  in 
  try in_scp_aux ty; true
  with Not_found -> false

let extract_bindings tyvars src dst =
  let rec extract_aux vs r=
    match vs with
      | [] -> r
      | (x::xs) -> 
	let y = 
          try lookup_var x src
	  with Not_found -> x
	in 
	if is_any_var y
	then extract_aux xs r
	else extract_aux xs (bind x y r)
  in extract_aux tyvars dst

(*
 * Save types: for saving types to disk storage 
 *)

(* [stype]: Representation of types for storage on disk *)
type stype = ((string * int), typ_const) pre_typ

(* [stypedef]: Type definition/declaration records for disk storage *)
type stypedef_record =
    {sname: string; 
     sargs : string list; 
     salias: stype option;
     scharacteristics: string list}

let rec to_save_aux inf env ty =
  match ty with
    | Var(id) -> 
      let nty=
	(try Lib.assocp (fun x -> id == x) (!env)
	 with Not_found -> 
	   let nid= inf:=!inf+1; (!id, !inf)
	   in 
	   env:= ((id, nid)::(!env)); nid)
      in Var(nty)
    | Constr(f, ats) ->
      Constr(f, List.map (to_save_aux inf env) ats)
    | WeakVar _ -> 
      raise (type_error "Can't save a weak variable" [ty])

let to_save ty = to_save_aux (ref 0) (ref []) ty
let to_save_env env ty = to_save_aux (ref 0) env ty

let to_save_rec record = 
  {sname=record.Scope.name;
   sargs = record.Scope.args;
   salias = 
      (match record.Scope.alias with
          None -> None
        | Some(t) -> Some(to_save t));
   scharacteristics = record.Scope.characteristics}

let rec from_save_aux env ty =
  match ty with
    |Var(id) -> 
      let nty = 
	try Lib.assocp (fun x -> id = x) (!env)
	with Not_found ->
	  let nid = ref(fst id)
	  in 
	  (env:=(id, nid)::(!env)); nid
      in 
      Var(nty)
    | Constr(f, ats) ->
      Constr(f, List.map(from_save_aux env) ats)
    | WeakVar _ -> raise (type_error "Can't load a weak variable" [])

let from_save ty = from_save_aux (ref[]) ty
let from_save_env env ty = from_save_aux env ty

let from_save_rec record = 
  {
    Scope.name=record.sname;
    Scope.args = record.sargs;
    Scope.alias = 
      (match record.salias with
          None -> None
        | Some(t) -> Some(from_save t));
    Scope.characteristics = record.scharacteristics
  }


(* Debugging substitutions *)
let print_subst tenv = 
  let ppinfo = Printer.empty_ppinfo()
  in 
  Format.printf "@[";
  subst_iter 
    (fun x y -> 
      Format.printf "@[("; 
      print ppinfo x;
      Format.printf "@ =@ ";
      print ppinfo y;
      Format.printf "):@]")
    tenv;
  Format.printf "@]"


module Retired =
struct

(**
   [unify_for_rewrite]: same as unify_env_unique_left
   except it returns a list of the bindings made
   if any error, removes bindings and raises exception 
*)
  let unify_for_rewrite scp t1 t2 env = 
    let copy_ty ty1 env =
      match ty1 with
        |Var(x) -> 
	  (try (lookup_var ty1 env, env)
	   with Not_found -> 
	     let nt = mk_var (!x)
	     in 
             (nt, bind ty1 nt env))
        | WeakVar(x) -> 
	  (try (lookup_var ty1 env, env)
	   with Not_found -> (ty1, env))
        | _ -> (ty1, env)
    in 
    let rec unify_aux ty1 ty2 env =
      let s, senv = 
        (* make fresh variable if needed *)
        let s1, s1env = copy_ty ty1 env
        in 
        (lookup_var s1 s1env, s1env)
      in 
      let t = lookup_var ty2 senv
      in 
      match (s, t) with
        | (Constr(f1, args1), Constr(f2, args2)) ->
	  if f1=f2
	  then 
	    (try List.fold_left2
	           (fun ev x y -> unify_aux x y ev) senv args1 args2
	     with _ -> raise (Failure ("Can't unify " ^(string_gtype s)
				       ^" with " ^(string_gtype t))))
	  else 
	    (try unify_aux (unfold scp s) t senv
	     with _ ->
	       (try (unify_aux s (unfold scp t) senv)
	        with _ ->
	          raise (Failure ("Can't unify " ^(string_gtype s)
			          ^" with " ^(string_gtype t)))))
        | (Var(_), Var(_)) ->
	  if equals s t 
	  then senv
	  else bind_occs s t senv
        | (Var(v1), x) -> bind_occs s x senv
        | (x, Var(v2)) -> bind_occs t x senv
        (* Weak variables are tried after the variables *)
        | (WeakVar(_), WeakVar(_)) -> 
	  if equals s t 
	  then senv
	  else bind_occs s t senv
        | (WeakVar(_), x) -> bind_occs s x senv
        | (x, WeakVar(_)) -> bind_occs t x senv
  (* unify constants *)
    in
    unify_aux t1 t2 env (* try to unify t1 and t2 *)

end
