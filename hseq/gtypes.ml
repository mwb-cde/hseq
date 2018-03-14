(*----
  Name: gtypes.ml
  Copyright Matthew Wahab 2005-2018
  Author: Matthew Wahab <mwb.cde@gmail.com>

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

(* Ordering. *)

let rec compare_gtype a b =
  let rec atom_cmp x y =
    match (x, y) with
    | (Var(v1), Var(v2)) -> Basic.gtype_id_compare v1 v2
    | (Var(_), Weak(_)) -> Order.GreaterThan
    | (Var(_), Ident(_)) -> Order.GreaterThan
    | (Weak(v1), Weak(v2)) -> Basic.gtype_id_compare v1 v2
    | (Weak(_), Var(_)) -> Order.LessThan
    | (Weak(_), Ident(_)) -> Order.GreaterThan
    | (Ident(v1), Ident(v2)) -> Ident.compare v1 v2
    | (Ident(_), Var(_)) -> Order.LessThan
    | (Ident(_), Weak(_)) -> Order.LessThan
  and struct_cmp p =
    match p with
    | (Atom(x), Atom(y)) -> atom_cmp x y
    | (Atom(_), TApp(_)) -> Order.LessThan
    | (TApp(_), Atom(_)) -> Order.GreaterThan
    | (TApp(a, b), TApp(c, d)) ->
       begin
         match (compare_gtype a c) with
         | Order.Equal -> compare_gtype b d
         | rslt -> rslt
       end
  in
  if a == b then Order.Equal
  else struct_cmp (a, b)
and compare_list ps =
  match ps with
  | ([], []) -> Order.Equal
  | ([], _) -> Order.LessThan
  | (_, []) -> Order.GreaterThan
  | (x::xs, y::ys) ->
     let r = compare_gtype x y in
     if r = Order.Equal
     then compare_list (xs, ys)
     else r

let equals a b = (compare_gtype a b) = Order.Equal
let lessthan a b = (compare_gtype a b) = Order.LessThan
let compare = compare_gtype

(* Recognisers *)

let is_var t =
  match t with
  | Atom(Var(_)) -> true
  | _ -> false

let is_weak t =
  match t with
  | Atom(Weak(_)) -> true
  | _ -> false

let is_ident t =
  match t with
  | Atom(Ident(_)) -> true
  | _ -> false

let is_constr t =
  match t with
  | _ -> false

let is_app t =
  match t with
  | TApp _ -> true
  | _ -> false

(* Constructors *)

let mk_var n = Atom(Var(Basic.mk_gtype_id n))
let mk_weak n = Atom(Weak(Basic.mk_gtype_id n))
let mk_ident n = Atom(Ident(n))

let mk_constr f args =
  let rec mk_tapp t args =
    match args with
    | [] -> t
    | (x::xs) -> mk_tapp (TApp(t, x)) xs
  in
  mk_tapp (mk_ident f) args

let mk_app x y = TApp(x, y)

(* Destructors *)

let dest_var t =
  match t with
  | Atom(Var(n)) -> n
  | _ -> raise (Failure "Not a variable")

let get_var_name t =
  match t with
  | Atom(Var(n)) -> Basic.gtype_id_string n
  | _ -> raise (Failure "Not a variable")

let dest_weak t =
  match t with
  | Atom(Weak(n)) -> n
  | _ -> raise (Failure "Not a weak variable")

let get_weak_name t =
  match t with
  | Atom(Weak(n)) -> Basic.gtype_id_string n
  | _ -> raise (Failure "Not a weak variable")

let dest_ident t =
  match t with
  | Atom(Ident(n)) -> n
  | _ -> raise (Failure "Not an identifier")

let dest_constr ty =
  let rec dest_tapp t args =
    match t with
    | Atom(Ident(f)) -> (f, args)
    | TApp(l, r) -> dest_tapp l (r::args)
    | _ -> raise (Failure "Invalid type constructor")
  in
  begin
    match ty with
    | _ -> dest_tapp ty []
  end

let rec get_type_name ty =
  match ty with
    | TApp(l, _) -> get_type_name l
    | Atom(Ident(id)) -> id
    | _ -> failwith "get_type_name"

let dest_app ty =
  match ty with
  | TApp(x, y) -> (x, y)
  | _ -> raise (Failure "Gtypes.dest_app: invalid type")

let flatten_app = Basic.flatten_apptype
let split_app = Basic.split_apptype

(* [map f ty] Apply [f] to each [Atom(x)] in [ty] returning the resulting
   type. *)
let map_atom = Basic.map_atomtype
let fold_atom = Basic.fold_atomtype
let exists_atom = Basic.exists_atomtype
let exists = Basic.exists_type
let exists_data = Basic.exists_type_data

(*
 * Specialised Manipulators
 *)

(* Variable types *)
let is_any_var t = (is_var t) || (is_weak t)

let mk_typevar ctr =
  let nty = Atom(Var(Basic.mk_gtype_id (int_to_name ctr)))
  in
  (ctr + 1, nty)

let mk_plain_typevar ctr =
  let prefix = "type_" in
  let nty = Atom(Var(Basic.mk_gtype_id (prefix^(string_of_int ctr))))
  in
  (ctr + 1, nty)

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
    | Atom(Var(n)) ->
      let (tbl1, n1) = lookup tbl (get_var_name ty)
      in
      (tbl1, n1)
    | TApp(l, r) ->
       let (tbl1, l1) = norm_aux tbl l in
       let (tbl2, r1) = norm_aux tbl1 r in
       (tbl2, TApp(l1, r1))
    | _ -> (tbl, ty)
  in
  let (_, typ1) = norm_aux (Lib.empty_env()) typ
  in
  typ1

(* Unnamed type variables *)

let null_type_name = ""
let mk_null () = mk_var null_type_name
let is_null t = ((get_var_name t) = null_type_name)

(* Named typed constructors *)

let mk_def = mk_constr
let is_def t = (try_app dest_constr t) <> None
let dest_def = dest_constr

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

  let compare = compare_gtype
end
module TypeTree=Treekit.BTree(TypeTreeData)
type ('a)tree = ('a)TypeTree.t

(*
 *   Substitution.
 *)

type substitution = (gtype)tree

let lookup t env = TypeTree.find env t
let member t env =
  try lookup t env ; true
  with Not_found -> false

let subst_sz s = TypeTree.empty
let empty_subst () = TypeTree.empty
let bind t r env =
  match (t, r) with
  | (Atom(Weak(_)), Atom(Var(_))) ->
     (failwith "Can't bind weak variable to a variable.")
  | _ -> TypeTree.replace env t r
let delete t env = TypeTree.delete env t
let subst_iter = TypeTree.iter
let subst_fold = TypeTree.fold

let bind_var t r env =
  if is_any_var t
  then bind t r env
  else
    raise (Failure "bind_var: Can't bind a non variable")

let rec subst t env =
  try lookup t env
  with Not_found ->
    begin
      match t with
      | Atom(_) -> t
      | TApp(x, y) ->
         let x1 = subst x env
         and y1 = subst y env
         in
         TApp(x1, y1)
    end

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
  | Atom(Var(x)) ->
     begin
       try (lookup trm env, env)
       with Not_found ->
         let nt = mk_var (Basic.gtype_id_string x)
         in
         (nt, bind trm nt env)
     end
  | Atom(Weak(x)) ->
     begin
       try (lookup trm env, env)
       with Not_found -> (trm, env)
     end
  | Atom(Ident(_)) -> (trm, env)
  | TApp(x, y) ->
     let x1, env1 = rename_type_vars_env env x in
     let y1, env2 = rename_type_vars_env env1 y
     in
     (TApp(x1, y1), env2)
  and
    rename_vars_list env lst rslt =
    match lst with
    | [] -> (env, List.rev rslt)
    | (ty::tyl) ->
       let (ty1, env1) = rename_type_vars_env env ty
       in
       rename_vars_list env1 tyl (ty1::rslt)

let rename_type_vars t =
  let (nty, _) = rename_type_vars_env (empty_subst()) t
  in
  nty

let rename_index idx env top_type =
  let rec rename_aux ctr env ty =
    match ty with
    | Atom(Var(x)) ->
       begin
         try (lookup ty env, ctr, env)
         with Not_found ->
           let (ctr1, nt) = mk_typevar ctr in
           let nenv = bind ty nt env
           in
           (nt, ctr1, nenv)
       end
    | Atom(Weak(x)) ->
       begin
         try (lookup ty env, ctr, env)
         with Not_found -> (ty, ctr, env)
       end
    | Atom(Ident(_)) -> (ty, ctr, env)
    | TApp(x, y) ->
       let x1, ctr1, env1 = rename_aux ctr env x in
       let y1, ctr2, env2 = rename_aux ctr1 env1 y
       in
       (TApp(x1, y1), ctr2, env2)
  in
  rename_aux idx env top_type

(*
 * Pretty printing
 *)

let pplookup ppstate id =
  try Printer.get_record (ppstate.Printer.types) id
  with Not_found ->
    Printer.mk_record
      Printer.default_type_prec
      Printer.default_type_fixity
      None

let print_bracket = Printer.print_assoc_bracket

let rec print_type ppstate pr t =
  let rec print_aux ppstate pr x =
    match x with
    | Atom(Var(_)) -> Format.printf "@[<hov 2>'%s@]" (get_var_name x)
    | Atom(Weak(_)) -> Format.printf "@[<hov 2>_%s@]" (get_weak_name x)
    | Atom(Ident(op)) -> print_constr ppstate pr (op, [])
    | TApp(_) ->
       let op, args = dest_constr x in
       print_app ppstate pr (op, args)
  and print_infix (assoc, prec) (nassoc, nprec) (f, args) =
    begin
      match args with
      | [] ->
         (* Print '(OP)' *)
         Format.printf "@[";
         Printer.print_identifier (pplookup ppstate) f;
         Format.printf "@]"
      | (lf::lr::rs) ->
         (* Print '(<arg> OP <arg>) <rest>' *)
         Format.printf "@[<hov 2>";
         print_bracket (assoc, prec) (nassoc, nprec) "(";
         print_aux ppstate (Printer.infixl, nprec) lf;
         Printer.print_space();
         Printer.print_identifier (pplookup ppstate) f;
         Printer.print_space();
         print_aux ppstate (Printer.infixr, nprec) lr;
         Printer.print_list
           (print_type ppstate (nassoc, nprec),
            Printer.print_space)
           rs;
         print_bracket (assoc, prec) (nassoc, nprec) ")";
         Format.printf "@]"
      | (lf::rs) ->
         (* Print '(<arg> OP) <rest>' *)
         Format.printf "@[<hov 2>";
         print_bracket (assoc, prec) (nassoc, nprec) "(";
         print_type ppstate (Printer.infixl, nprec) lf;
         Printer.print_space();
         Printer.print_identifier (pplookup ppstate) f;
         Printer.print_list
           (print_type ppstate (nassoc, nprec),
            Printer.print_space)
           rs;
         print_bracket (assoc, prec) (nassoc, nprec) ")";
         Format.printf "@]"
    end
  and print_suffix (assoc, prec) (nassoc, nprec) (f, args) =
    begin
      Format.printf "@[<hov 2>";
      print_bracket (assoc, prec) (nassoc, nprec) "(";
      Printer.print_suffix
        ((fun pr -> Printer.print_identifier (pplookup ppstate)),
         (fun pr l ->
           if l = [] then ()
           else
             Printer.print_sep_list (print_type ppstate (assoc, pr), ",") l))
        nprec (f, args);
      print_bracket (assoc, prec) (nassoc, nprec) ")";
      Format.printf "@]"
    end
  and print_prefix (assoc, prec) (nassoc, nprec) (f, args) =
    Format.printf "@[<hov 2>";
    if args = [] then ()
    else
      begin
        Printer.print_string "(";
        Printer.print_sep_list (print_type ppstate (assoc, prec), ",") args;
        Format.printf ")@,"
      end;
    Printer.print_identifier (pplookup ppstate) f;
    Format.printf "@]"
  and print_app ppstate (assoc, prec) (f, args) =
    let pprec = pplookup ppstate f in
    let nfixity = pprec.Printer.fixity in
    let (nassoc, nprec) = (nfixity, pprec.Printer.prec)
    in
    let some_printer =
      try_find (Printer.get_printer (ppstate.Printer.types)) f
    in
    if some_printer <> None
    then (from_some some_printer) ppstate (assoc, prec) (f, args)
    else
      if Printer.is_infix nfixity
      then print_infix (assoc, prec) (nassoc, nprec) (f, args)
      else
        if Printer.is_suffix nfixity
        then print_suffix (assoc, prec) (nassoc, nprec) (f, args)
        else print_prefix (assoc, prec) (nassoc, nprec) (f, args)
  and print_constr ppstate (assoc, prec) (f, args) =
    print_app ppstate (assoc, prec) (f, args)
  in
  print_aux ppstate pr t

let print ppinfo x =
  print_type ppinfo
    (Printer.default_type_fixity, Printer.default_type_prec) x

(*
 * Error handling
 *)

let print_type_error s ts fmt pinfo =
    Format.fprintf fmt "@[%s@ " s;
    Printer.print_sep_list (print pinfo, ",") ts;
    Format.fprintf fmt "@]"
let type_error s ts = mk_error(print_type_error s ts)
let add_type_error s t es = raise (add_error (type_error s t) es)

(* String representation of a type *)

let rec string_gtype x =
  match x with
  | Atom(Var(a)) -> "'"^(Basic.gtype_id_string a)
  | Atom(Weak(a)) -> "_"^(Basic.gtype_id_string a)
  | Atom(Ident(f)) -> string_tconst f []
  | TApp(_) -> string_app_args x []
and string_app_args t lst =
  match t with
  | Atom(Ident(f)) -> string_tconst f lst
  | TApp(l, r) -> string_app_args l ((string_gtype r)::lst)
  | _ -> String.concat " " ((string_gtype t)::lst)

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
let rewrite_subst t env =
  let mapper ty =
    match ty with
    | Atom(Var(a)) ->
       begin
         try Lib.find (Basic.gtype_id_string a) env
         with Not_found ->
           raise (type_error "rewrite_subst: Can't find parameter" [t])
       end
    | _ -> ty
  in
  map_atom mapper t

let rewrite_defn given_args rcrd_args t =
  if (List.length rcrd_args) = (List.length given_args)
  then
    let tenv =
      List.fold_left2 (fun env x y -> Lib.bind x y env)
                      (Lib.empty_env())
                      rcrd_args given_args
    in
    rewrite_subst t tenv
  else
    raise (type_error "rewrite_defn: Wrong number of arguments" [t])

let unfold scp t =
  match t with
  | TApp(_) ->
     let (n, args) = (try dest_constr t with _ -> raise Not_found)
     in
     let recrd = get_typdef scp n in
     if recrd.Scope.alias = None
     then raise Not_found
     else rewrite_defn args (recrd.Scope.args) (from_some recrd.Scope.alias)
  | Atom(Ident(n)) ->
     let recrd = get_typdef scp n in
     if recrd.Scope.alias = None
     then raise Not_found
     else rewrite_defn [] (recrd.Scope.args) (from_some recrd.Scope.alias)
  | _ -> raise Not_found

(**
   [has_defn scp n]: true if [n] has a definition in scope [scp] (and
   is therefore an alias for a type).
*)
let has_defn tyenv n =
  try
     if (get_typdef tyenv n).Scope.alias = None
     then false
     else true
  with Not_found -> false

(*
 * Consistency tests.
 * Weak variables are not permitted in any definition (type or term).
 *)

let check_args args =
  let null_type = mk_def (Ident.null) [] in
  let arg_itr env x =
    if (is_var x) && (not (member x env))
    then (bind x null_type env)
    else raise Not_found
  in
  try
    ignore(List.fold_left arg_itr (empty_subst()) args);
    true
  with Not_found -> false

(**
   [check_decln l]: consistency check on declaration of type [l]
*)
let check_decln l =
  if is_def l
  then
    let n, largs = dest_def l
    in
    (Lib.try_find check_args largs) <> None
  else false

(** [well_formed_full pred cache scp t]: ensure that [t] is well-formed
    declared. Apply [pred] to each sub-type of [t], failing if it is false. If
    [cache] is given, use it as a cache of function names.

    A type is well-formed at depth [d] if it satisifes [pred] and is one of:

    - [Atom(Var(v))] at depth [d = 0]

    - [Atom(Weak(v))] at depth [d = 0]

    - [Atom(Ident(f))] and
      - [f] is in scope and
      - [d = arity(f)]

    - [TApp(l, r)] and
      - depth [d > 0] and
      - [l] is well-defined at depth [d + 1] and
      - [r] is well-defined at depth [0]

    Type constructor [F/n] has arity [arity(F) = n]. With arguments [a_0, ..,
    an], the type [(a_0, .., an)F] is formed with [TApp] by making [F] the
    left-most element with the [a_i] as the right branches. For [(a_0, ..,
    an)F], this is [TApp( .. (TApp(Atom(Ident(F)), a_0), a_1), a_n)].

    Specific constructors formed by [TApp]:

    - [()F = Atom(Ident(F))]: [F] has arity [0] and is well-defined at depth
      [0].

   - [(a)F = TApp(Atom(Ident(f)), a)]: [F] has arity [1] and is well-defined at
     depth [0].

   - [(a, b)F = TApp(TApp(Atom(Ident(f)), a), b)]: [F] has arity [2] and is
     well-defined at depth [0].  *)

let well_formed_full pred scp ty =
  let arity_of f =
    (* Get the arity of [f]. Return [None] if [f] is unknown. *)
    let defn_opt = try_find (Scope.defn_of scp) f in
    if defn_opt = None
    then None
    else
      let defn = from_some defn_opt in
      let num_args = List.length (defn.Scope.args) in
      Some(num_args)
  in
  let rec well_aux depth t =
    let prslt = pred t in
    if prslt <> None then prslt
    else
      begin
        match t with
        | Atom(Var(v)) ->
           if depth <> 0
           then Some("unexpected variable", t)
           else None
        | Atom(Weak(v)) ->
           if depth <> 0
           then Some("unexpected weak variable", t)
           else None
        | Atom(Ident(n)) ->
           let arity_opt = arity_of n in
           if arity_opt = None
           then Some("unknown type constructor", t)
           else if depth = (from_some arity_opt)
           then None
           else
             Some(("invalid type constructor, expected "
                   ^(string_of_int (from_some arity_opt))
                   ^" but got "^(string_of_int depth)),
                  t)
        | TApp(l, r) ->
           let lerr = well_aux (depth + 1) l in
           if lerr <> None
           then lerr
           else well_aux 0 r
      end
  in
  let rslt = well_aux 0 ty
  in
  if rslt = None
  then true
  else
    begin
      let (msg, t) = from_some rslt in
      raise (type_error msg [t])
    end

let well_formed scp ty =
  well_formed_full (fun _ -> None) scp ty

(** [well_defined scp args t]: check that t is well-formed and that every
    variable in [t] is represented in [args].

    A type is well-defined at depth [d] if it is one of:

    - [Atom(Var(v))] at depth [d = 0] and
      - the string representation of [v] is in [args]

    - [Atom(Ident(f))] and
    - [f] is in scope and
    - [d] is the arity of [f]

    - [TApp(l, r)] and
    - [l] is well-defined at depth [d + 1] and
    - [r] is  well-defined at depth [0]
*)
let well_defined scp (args: (string)list) ty =
  let var_is_arg n = List.exists (fun a -> n = a) args in
  let check_var t =
    match t with
    | Atom(Var(v)) ->
       if not (var_is_arg (Basic.gtype_id_string v))
       then Some("unexpected variable.", t)
       else None
    | Atom(Weak(v)) ->
       Some("unexpected weak variable.", t)
    | _ -> None
  in
  ignore(well_formed_full check_var scp ty)


(** [quick_well_defined]: Simpler version of well_defined. Only check that type
    constructors are well-defined. [cache] is intednded to be a cache of
    function identifiers and arities, shared across invocations of
    [quick_well_defined] but is currently unused.
 *)
type arity_cache = (Ident.t * int, bool) Hashtbl.t
let quick_well_defined scp cache ty =
  well_formed scp ty

(**
   [check_decl_type scp ty]: Ensure type [ty] is suitable for
   a declaration.

   Fails if [ty] contains a weak variable.
*)
let check_decl_type scp ty =
  let check_var t =
    match t with
    | Atom(Weak(v)) ->
       Some("unexpected weak variable.", t)
    | _ -> None
  in
  ignore(well_formed_full check_var scp ty)

(*
 * Unification
 *)

let lookup_var ty env =
  let rec chase t =
    if is_any_var t
    then
      try chase (lookup t env)
      with Not_found -> t
    else t
  in chase ty

let occurs atomty ty =
  let checker t = equals atomty ty
  in
  begin
  match atomty with
  | Atom(_) -> not (exists_atom checker ty)
  | _ -> false
  end

let rec occurs_env tenv ty1 ty2 =
  let nty1 = lookup_var ty1 tenv
  and nty2 = lookup_var ty2 tenv
  in
  match (nty1, nty2) with
  | (Atom(_), Atom(_))-> equals nty1 nty2
  | (Atom(_), TApp(l, r)) ->
     (occurs_env tenv nty1 l || occurs_env tenv nty1 r)
  | _ -> raise (type_error ("occurs_env: expected a type variable") [nty1])

let bind_occs t1 t2 env =
  if is_any_var t1
  then
    begin
      let r1 = lookup_var t1 env
      and r2 = lookup_var t2 env
      in
      if not (occurs_env env r1 r2)
      then bind_var r1 r2 env
      else raise (type_error
                    "bind_occs: variable occurs in binding type"
                    [t1; t2])
    end
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
let rec unify_aux scp ty1 ty2 env =
  begin
    let s = lookup_var ty1 env
    and t = lookup_var ty2 env
    in
    match (s, t) with
    | (TApp(l1, r1), TApp(l2, r2)) ->
       (* First try unifying the branches *)
       let env1_opt = try_app (unify_aux scp l1 l2) env in
       let env2_opt =
         if env1_opt = None then None
         else try_app (unify_aux scp r1 r2) (from_some env1_opt)
       in
       if env2_opt <> None
       then from_some env2_opt
       else
         (* Try expanding type aliases before unifying *)
         begin
           try expand_unify scp s t env
           with err -> (add_type_error "Can't unify types" [s; t] err)
         end
    (* Variables, bind if not equal, but test for occurence *)
    | (Atom(Var(_)), Atom(Var(_))) ->
       if equals s t
       then env
       else bind_occs s t env
    | (Atom(Var(_)), _) -> bind_occs s t env
    | (_, Atom(Var(_))) -> bind_occs t s env
    (* Weak variables, don't bind to variables *)
    | (Atom(Weak(_)), Atom(Weak(_))) ->
       if equals s t
       then env
       else bind_occs s t env
    | (Atom(Weak(_)), _) ->
       if is_var t
       then raise (type_error "Can't unify types" [s; t])
       else bind_occs s t env
    | (_, Atom(Weak(_))) ->
       if is_var s
       then raise (type_error "Can't unify types" [s; t])
       else bind_occs t s env
    | (Atom(Ident(f1)), Atom(Ident(f2))) ->
       if Ident.equals f1 f2
       then env
       else expand_unify scp s t env
    | _ -> raise (type_error "Can't unify types" [s; t])
  end
and unify_aux_list scp tyl1 tyl2 env =
  begin
    match (tyl1, tyl2) with
    | ([], []) -> env
    | (ty1::l1, ty2::l2) ->
       let env1 =
         try unify_aux scp ty1 ty2 env
         with x -> add_type_error "Can't unify types" [ty1; ty2] x
       in
       unify_aux_list scp l1 l2 env1
    | _ -> raise (type_error "Can't unbalanced constructor lists" [])
  end
and expand_unify scp ty1 ty2 env =
  (* Try to expand and unify type aliases *)
  let expand t =
    if (is_constr t) || (is_app t) || (is_ident t)
    then try_app (unfold scp) t
    else None
  in
  let s = expand ty1
  and t = expand ty2
  in
  if s = None && t = None
  then raise (type_error "Can't unify types" [ty1; ty2])
  else
    let s1 = if s = None then ty1 else from_some s
    and t1 = if t = None then ty2 else from_some t
    in
    unify_aux scp s1 t1 env

let unify_env scp t1 t2 nenv =
  unify_aux scp t1 t2 nenv

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
  | Atom(Var(a)) ->
    let nt = lookup_var t env
    in
    if is_var nt
    then nt
    else mgu nt env
  | Atom(Weak(a)) ->
    let nt = lookup_var t env
    in
    if is_any_var nt
    then nt
    else mgu nt env
  | Atom(Ident(_)) -> t
  | TApp(l, r) ->
     TApp(mgu l env, mgu r env)

(**
   [mgu_rename_env inf env env nenv typ]: Replace variables in [typ]
   with their bindings in substitution [env].  If a variable isn't bound
   in [env], then it is renamed and bound to that name in [nenv] (which is
   checked before a new name is created).
*)
let mgu_rename_env (inf, tyenv) name_env typ =
  let new_name_env (ctr, nenv) x =
    try (lookup x nenv, (ctr, nenv))
    with Not_found ->
      let (ctr1, newty) = mk_typevar ctr
      in
      (newty, (ctr1, bind_var x newty nenv))
  in
  let rec rename_aux (ctr, (nenv: substitution)) ty =
    match ty with
    | Atom(Var(_)) ->
      let nt = lookup_var ty tyenv
      in
      if equals ty nt
      then new_name_env (ctr, nenv) nt
      else rename_aux (ctr, nenv) nt
    | Atom(Weak(_)) ->
      let nt = lookup_var ty tyenv
      in
      if is_weak nt
      then (nt, (ctr, nenv))
      else rename_aux (ctr, nenv) nt
    | Atom(Ident(_)) -> (ty, (ctr, nenv))
    | TApp(l, r) ->
       let l1, (ctr1, env1) = rename_aux (ctr, nenv) l in
       let r1, (ctr2, env2) = rename_aux (ctr1, env1) r in
       (TApp(l1, r1), (ctr2, env2))
  in
  rename_aux (inf, name_env) typ

(**
   [mgu_rename_simple inf env env nenv typ]: Replace variables in [typ]
   with their bindings in substitution [env].  If a variable isn't bound
   in [env], then it is renamed and bound to that name in [nenv] (which is
   checked before a new name is created).

   This does the same thing as mgu_rename_env except that it takes
   [inf] as a scalar, rather than a reference, and returns a new value
   for [inf].
 *)
let mgu_rename_simple inf tyenv name_env typ =
  let new_name_env ctr nenv x =
    try (lookup x nenv, ctr, nenv)
    with Not_found ->
      let (ctr1, newty) = mk_typevar ctr
      in
      (newty, ctr1, bind_var x newty nenv)
  in
  let rec rename_aux ctr (nenv: substitution) ty =
    match ty with
    | Atom(Var(_)) ->
       let nt = lookup_var ty tyenv
       in
       if equals ty nt
       then new_name_env ctr nenv nt
       else rename_aux ctr nenv nt
    | Atom(Weak(_)) ->
       let nt = lookup_var ty tyenv
       in
       if is_weak nt
       then (nt, ctr, nenv)
       else rename_aux ctr nenv nt
    | Atom(Ident(_)) -> (ty, ctr, nenv)
    | TApp(l, r) ->
       let (l1, ctr1, env1) = rename_aux ctr nenv l in
       let (r1, ctr2, env2) = rename_aux ctr1 env1 r in
       (TApp(l1, r1), ctr2, env2)
  in
  rename_aux inf name_env typ

let mgu_rename inf env nenv typ =
  let nty, _ = mgu_rename_env (inf, env) nenv typ
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
  let expand t =
    if (is_constr t) || (is_app t) || (is_ident t)
    then try_app (unfold scp) t
    else None
  in
  let rec match_aux ty1 ty2 env =
    let s = lookup_var ty1 env
    and t = lookup_var ty2 env
    in
    match (s, t) with
    | (Atom(Var(_)), _) ->
      if equals s t
      then env
      else bind_occs s t env
    | (_, Atom(Var(_))) -> env
    | (Atom(Weak(_)), _) ->
      if equals s t
      then env
      else bind_occs s t env
    | (Atom(Ident(_)), Atom(Ident(_))) ->
      if equals s t
      then env
      else raise (type_error "Can't match types" [s; t])
    | (TApp(l1, r1), TApp(l2, r2)) ->
       (* First try matching the branches *)
       let env1_opt = try_app (match_aux l1 l2) env in
       let env2_opt =
         if env1_opt = None then None
         else try_app (match_aux r1 r2) (from_some env1_opt)
       in
       if env2_opt <> None
       then from_some env2_opt
       else
         (* Try expanding type aliases *)
         begin
           try expand_match s t env
           with err -> (add_type_error "Can't match types" [s; t] err)
         end
    | _ ->
      if equals s t
      then env
      else raise (type_error "Can't match types" [s; t])
  and expand_match ty1 ty2 env =
    let s = expand ty1
    and t = expand ty2
    in
    if (s = None) && t = None
    then raise (type_error "Can't match types" [ty1; ty2])
    else
    let s1 = if s = None then ty1 else from_some s
    and t1 = if t = None then ty2 else from_some t
    in
    match_aux s1 t1 env
  in
  try match_aux t1 t2 env (* try to match t1 and t2 *)
  with x -> add_type_error "Can't match types" [t1; t2] x

let matches_env scp tyenv t1 t2 =
  try matching_env scp tyenv t1 t2
  with _ -> tyenv

let matches scp t1 t2=
  try ignore(matching_env scp (empty_subst()) t1 t2); true
  with _ -> false

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
    | TApp(l, r) -> TApp(set_aux l, set_aux r)
    | Atom(Ident(id)) ->
      let (th, n) = Ident.dest id in
      let nth =
        if th = Ident.null_thy
        then lookup_id n
        else th
      in
      let nid = Ident.mk_long nth n
      in
      (mk_ident nid)
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
    | Atom(Var(_)) -> ()
    | Atom(Weak(_)) -> ()
    | Atom(Ident(f)) ->
       ignore(lookup_id (Ident.thy_of f))
    | TApp(l, r) -> (in_scp_aux l; in_scp_aux r)
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
type satom =
  | SVar of (string * int) (* Variables *)
  | SIdent of Ident.t      (* Identifier *)
type stype = (satom) pre_typ

type to_stype_env = (Basic.gtype_id * (string * int)) list
(** Data needed to construct a type storage representation. *)

type from_stype_env = ((string * int) * Basic.gtype_id) list
(** Data needed to construct a type storage representation. *)

(* [stypedef]: Type definition/declaration records for disk storage *)
type stypedef_record =
  {
    sname: string;
    sargs: (string)list;
    salias: (stype)option;
    scharacteristics: (string)list
  }

let rec to_save_aux tyenv ty =
  let (ctr, env) = tyenv
  in
  match ty with
  | Atom(Var(id)) ->
    let make_new() =
      let ctr1 = ctr + 1 in
      let nid = (get_var_name ty, ctr1) in
      let env1 = (id, nid)::env
      in
      (nid, (ctr1, env1))
    in
    let (nty, tyenv1) =
      try (Lib.assocp (gtype_id_equal id) env, tyenv)
      with Not_found -> make_new()
    in
    (Atom(SVar(nty)), tyenv1)
  | Atom(Weak(_)) ->
    raise (type_error "Can't save a weak variable" [ty])
  | Atom(Ident(f)) -> (Atom(SIdent(f)), tyenv)
  | TApp(l, r) ->
     let l1, tyenv1 = to_save_aux tyenv l in
     let r1, tyenv2 = to_save_aux tyenv1 r in
     (TApp(l1, r1), tyenv2)
and to_save_list tyenv lst rslt =
  match lst with
  | [] -> (tyenv, List.rev rslt)
  | (ty:: tyl) ->
    let (ty1, tyenv1) = to_save_aux tyenv ty
    in
    to_save_list tyenv1 tyl (ty1::rslt)

let to_save ty =
  let (ty1, _) = to_save_aux (0, []) ty
  in
  ty1

let to_save_env env ty =
  let (ty1, (_, env1)) = to_save_aux (0, env) ty
  in
  (ty1, env1)

let to_save_rec record =
  {
    sname = record.Scope.name;
    sargs = record.Scope.args;
    salias =
      begin
        match record.Scope.alias with
        | None -> None
        | Some(t) -> Some(to_save t)
      end;
    scharacteristics = record.Scope.characteristics
  }

let rec from_save_aux env (ty: stype) =
  match ty with
  | Atom(SVar(id)) ->
    let make() =
      let nid = Basic.mk_gtype_id (fst id) in
      let env1 = (id, nid)::env
      in
      (nid, env1)
    in
    let (nty, env1) =
      try
        let nid = Lib.assocp (fun x -> id = x) env
        in
        (nid, env)
      with Not_found -> make()
    in
    (Atom(Var(nty)), env1)
  | Atom(SIdent(f)) -> (Atom(Ident(f)), env)
  | TApp(l, r) ->
     let l1, env1 = from_save_aux env l in
     let r1, env2 = from_save_aux env1 r in
     (TApp(l1, r1), env2)
and from_save_list env lst rslt =
  match lst with
  | [] -> (env, List.rev rslt)
  | (ty::tyl) ->
    let (x1, env1) = from_save_aux env ty
    in
    from_save_list env1 tyl (x1:: rslt)

let from_save ty =
  let (ty1, _) = from_save_aux [] ty
  in
  ty1

let from_save_env env ty =  from_save_aux env ty

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
      Format.printf "@ :=@ ";
      print ppinfo y;
      Format.printf "):@]")
    tenv;
  Format.printf "@]"
