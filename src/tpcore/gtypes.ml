
open Basic
open Lib
open Result

(*
type ('idtyp, 'tfun, 'tcons) pre_typ =
    Var of 'idtyp
  | Constr of 'tfun * ('idtyp, 'tfun, 'tcons) pre_typ list
  | Base of 'tcons
  | WeakVar of 'idtyp

type gtype = (string ref, typ_const, base_typ)pre_typ
*)
type stype = ((string * int), typ_const, base_typ) pre_typ

type typedef_record =
    {name: string; 
     args : string list; 
     alias: gtype option;
     characteristics: string list}

type stypedef_record =
    {sname: string; 
     sargs : string list; 
     salias: stype option;
     scharacteristics: string list}

type scope = 
    {curr_thy: Basic.thy_id;
     typeof_fn : ident -> gtype; 
       typ_defn: ident -> typedef_record;
	 thy_of:  id_selector ->string -> thy_id;
	   prec_of: id_selector -> ident -> int;
	     thy_in_scope: thy_id -> thy_id -> bool}

let empty_typenv ()=
  { curr_thy = Basic.null_thy;
    typeof_fn = (fun x-> raise Not_found);
    typ_defn = (fun x-> raise Not_found);
    thy_of = (fun x y -> raise Not_found);
    prec_of = (fun x y -> -1);
    thy_in_scope =(fun x y-> false)
  }

let empty_scope ()=
  {curr_thy = Basic.null_thy; 
   typeof_fn = (fun x-> raise Not_found);
   typ_defn = (fun x-> raise Not_found);
   thy_of = (fun x y -> raise Not_found);
   prec_of = (fun x y -> -1);
   thy_in_scope = (fun x y -> false)}

let add_to_scope scp ls =
  { curr_thy= scp.curr_thy;
    typeof_fn = 
    (fun x ->
      try (List.assoc x ls)
      with _ -> scp.typeof_fn x);
    typ_defn = scp.typ_defn;
    thy_of = scp.thy_of;
    prec_of = scp.prec_of;
    thy_in_scope = scp.thy_in_scope}

let extend_scope scp f =
  { curr_thy = scp.curr_thy;
    typeof_fn = 
    (fun x ->
      try f x 
      with _ -> scp.typeof_fn x);
    typ_defn = scp.typ_defn;
    thy_of = scp.thy_of;
    prec_of = scp.prec_of;
    thy_in_scope = scp.thy_in_scope}

let get_typdef tyenv r =  tyenv.typ_defn r (* (get_typenv()) r *)

let is_var t  = 
  match t with
    Var _ -> true
  | _ -> false

let is_weak t  = 
  match t with
    WeakVar _ -> true
  | _ -> false


(* substitution *)
(* substitution types *)
(* Using balanced trees *)
(* The right way to do it *)

let rec equals x y = 
  (match (x, y) with
    (Var(v1), Var(v2)) -> (v1==v2)
  |	(Constr(f1, args1), Constr(f2, args2))
    -> (f1=f2) &
      (try
	((List.iter2 
	    (fun a b -> 
	      if (equals a b) 
	      then () 
	      else raise (Failure ""))
	    args1 args2); true)
      with _ -> false)
  | (WeakVar(v1), WeakVar(v2)) -> (v1==v2)
  | (_, _) -> x=y)


let rec safe_equal x y = 
  (match (x, y) with
    (Var(v1), Var(v2)) -> (v1==v2)
  |	(Constr(f1, args1), Constr(f2, args2))
    -> (f1=f2) &
      (try
	((List.iter2 
	    (fun a b -> 
	      if (safe_equal a b) 
	      then () 
	      else raise (Failure ""))
	    args1 args2); true)
      with _ -> false)
  | (WeakVar(v1), WeakVar(v2)) -> (v1==v2)
  | (_, _) -> x=y)

let eqvar v1 v2 = 
  if (is_var v1) & (is_var v2) 
  then equals v1 v2
  else raise (Failure "Not a variable")


module TypeTreeData=
  struct 
    type key=gtype
    let equals = equals
  end

module TypeTree=Treekit.BTree(TypeTreeData)      

type substitution = (gtype)TypeTree.t

let lookup t env = TypeTree.find env t
let member t env = try lookup t env ; true with Not_found -> false

let subst_sz s= TypeTree.nil
let empty_subst ()= TypeTree.nil
let bind t r env = TypeTree.replace env t r
let bind_env = bind
let delete t env = TypeTree.delete env t
let subst_iter =TypeTree.iter

let get_var t = 
  match t with 
    Var(n) -> !n
  | _ -> raise (Failure "Not a variable")

let get_weak t = 
  match t with 
    WeakVar(n) -> !n
  | _ -> raise (Failure "Not a weak variable")




let rec string_gtype x =
  match x with
    Var(a) -> "'"^(!a)
  | Constr(f, args) ->  
      string_tconst f (List.map string_gtype args)
  | Base(b) -> string_btype b
  | WeakVar(a) -> "_"^(!a)


(*renamed any_varp to is_any_var *)
let is_any_var t= (is_var t) or (is_weak t)

let is_constr t = 
  match t with
    Constr _ -> true
  | _ -> false

let is_func t = 
  match t with
    Constr (Func, _) -> true
  | _ -> false

let is_defined t = 
  match t with
    Constr (Defined _ , _) -> true
  | _ -> false

let is_base t  = 
  match t with
    Base _ -> true
  | _ -> false


(* constructurs *)

let mkbase x = (Base x)

let mk_bool = (mkbase Bool)
let mk_num = (mkbase Num)
let mk_ind = (mkbase Ind)

let mk_var n = (Var (ref n))

let dest_var t =
  match t with 
    Var(n) -> n
  | _ -> raise (Failure "Not a variable")

let mk_weak n = (WeakVar (ref n))
let dest_weak t =
  match t with 
    WeakVar(n) -> n
  | _ -> raise (Failure "Not a weak variable")


let mk_null () = (mk_var "")
let is_null = 
  function (Var(x)) -> (!x)=""
    | _ -> false

let mkconstr f l = Constr(f, l)
let destconstr t = 
  match t with
    Constr(f, l) -> (f, l)
  | _ -> raise (Failure "Not a constructor type")

let mk_fun l r = mkconstr Func [l; r]
let rec mkfun_from_list l r = 
  match l with
    [] -> raise (Failure "No argument types")
  | [t] -> mk_fun t r
  | t::ts -> mk_fun t (mkfun_from_list ts r)

let rec dest_constr ty = 
  match ty with 
    Constr(f, args) -> (f, args)
  | _ -> raise (Failure "Not a constructed type")

let mk_def n args = mkconstr (Defined n) args
let dest_def t = 
  match t with
    Constr(Defined n, args) -> (n, args)
  | _ -> raise (Failure "Not a defined type")


let eqbase b1 b2 = 
  match b1 with
    Base(n1) -> 
      (match b2 with 
	Base(n2) -> n1=n2 
      | _ -> raise (Failure "Not a basic type"))
  | _ -> raise (Failure "Not a basic type")

let eqconstr c1 c2 =
  match c1 with
    Constr(n1, _) -> 
      (match c2 with 
	Constr(n2, _) -> n1=n2 
      | _ -> raise (Failure "Not a constructor type"))
  | _ -> raise (Failure "Not a constructor type")


let arg_type t = 
  match t with
    (Constr(Func, (l::_))) -> l
  | (Constr(Func, [])) -> raise (Failure "No argument type")
  | _ -> raise (Failure "Not a function type")

let ret_type t = 
  match t with
    (Constr(Func, (l::r::[]))) -> r
  | _ -> raise (Failure "Not a function type")

let rec chase_ret_type t=
  match t with 
    Constr(Func, l::r::[]) -> chase_ret_type r
  | x -> x


(* pretty printing *)



type printer_info=
    { 
      tbl: substitution; (* used to store pretty replacement variable names *)
      ctr: int ref; (* used to generate variable names *)
    }
let empty_printer_info()=
  {tbl=empty_subst(); ctr=ref 0}

let mk_typevar n =
  let nty=Var(ref(int_to_name (!n)))
  in
  n:=(!n)+1; nty


let print_constr ppstate id=
  match id with
    Basic.Defined n -> Printer.print_identifier ppstate n
  | Basic.Func ->
      Printer.print_string "->"

let lookup_constr ppstate id = 
  match id with
    Basic.Defined n -> Printer.get_record ppstate n
  | Basic.Func -> 
      Printer.mk_record 6 Printer.infix None

let find_printer ppstate id = 
  match id with 
    Basic.Defined n -> raise Not_found
  | Basic.Func -> raise Not_found

let pplookup ppstate id =
  try
    (Printer.get_record (ppstate.Printer.type_info) id)
  with Not_found -> 
    Printer.mk_record 
      Printer.default_type_prec
      Printer.default_type_fixity
      None

let rec print_type ppstate pr t = 
  let print_aux ppstate pr x=
    match x with
      Var(_) -> 
	Format.print_string ("'"^(get_var x));
	Format.print_cut()
    | WeakVar(_) -> 
	Format.print_string ("'_"^(get_weak x));
	Format.print_cut()
    | Base(b) -> 
	Format.print_string (Basic.string_btype b);
	Format.print_cut()
    | Constr(Defined op, args) -> 
	print_defined ppstate pr (op, args);
	Format.print_cut()
    | Constr(Func, args) -> 
	print_func ppstate pr args;
	Format.print_cut()
  in 
  Format.open_box 2;
  print_aux ppstate pr t;
  Format.close_box ()
and print_defined ppstate prec (f, args) =
  let pprec = pplookup ppstate f
  in 
  try 
    let printer = Printer.get_printer (ppstate.Printer.type_info) f
    in 
    printer prec (f, args)
  with Not_found -> 
    if(Printer.is_infix pprec.Printer.fixity)
    then 
      (match args with
	[] -> ()
      | (lf::rs) -> 
	  Printer.print_bracket prec (pprec.Printer.prec) "(";
	  print_type ppstate (pprec.Printer.prec) lf;
	  Printer.print_space();
	  Printer.print_identifier (pplookup ppstate) f;
	  Printer.print_space();
	  Printer.print_list 
	    (print_type ppstate (pprec.Printer.prec), Printer.print_space) 
	    rs; 
	  Format.print_cut();
	  Printer.print_bracket prec (pprec.Printer.prec) ")")
    else 
      if(Printer.is_suffix pprec.Printer.fixity)
      then 
	(Printer.print_bracket prec (pprec.Printer.prec) "(";
	 Format.print_cut();
	 Printer.print_suffix 
	   ((fun pr -> Printer.print_identifier (pplookup ppstate)), 
	    (fun pr l-> 
	      match l with 
		[] -> ()
	      |_ -> Printer.print_sep_list
		    (print_type ppstate pr, ",") l))
	   (pprec.Printer.prec) (f, args);
	 Format.print_cut();
	 Printer.print_bracket prec (pprec.Printer.prec) ")")
      else 
	Format.print_cut();
    Printer.print_prefix
      ((fun pr -> Printer.print_identifier (pplookup ppstate)),
       (fun pr l -> 
	 match l with 
	   [] -> ()
	 | _ -> 
	     Printer.print_string "(";
	     Printer.print_sep_list
	       (print_type ppstate pr, ",") l;
	     Printer.print_string ")"))
      (pprec.Printer.prec) (f, args);
    Format.print_cut();
and print_func ppstate prec args =
  Printer.print_infix
    ((fun _ _ -> Printer.print_string "->"),
     (fun pr l -> 
       Printer.print_bracket pr prec "(";
       Printer.print_list 
	 (print_type ppstate pr, Printer.print_space) l;
       Printer.print_bracket pr prec ")"))
    prec (Func, args)

let print ppinfo x =
  print_type ppinfo 0 x

(* Error handling *)

class typeError s ts=
  object (self)
    inherit Result.error s
    val trms = (ts: gtype list)
    method get() = ts
    method print st = 
      Format.open_box 0; Format.print_string (self#msg()); 
      Format.print_break 1 2;
      Format.open_box 0; 
      Printer.print_sep_list 
	(print st, ",") (self#get());
      Format.close_box();
      Format.close_box();
  end
let typeError s t = (mkError((new typeError s t):>error))
let addtypeError s t es= raise (addError ((new typeError s t):>error) es)


(* save types *)

let rec to_save_aux inf env ty =
  match ty with
    Var(id) -> 
      let nty=
	(try 
	  Lib.assocp (==) id (!env)
	with Not_found -> 
	  let nid= inf:=!inf+1; (!id, !inf)
	  in 
	  env:= ((id, nid)::(!env)); nid)
      in Var(nty)
  | Constr(f, ats) ->
      Constr(f, List.map (to_save_aux inf env) ats)
  | Base(b) -> Base(b)
  | WeakVar _ -> 
      raise (typeError "Can't save a weak variable" [ty])

let to_save ty = to_save_aux (ref 0) (ref []) ty
let to_save_env env ty = to_save_aux (ref 0) env ty

let to_save_rec record = 
  {sname=record.name;
   sargs = record.args;
   salias = 
   (match record.alias with
     None -> None
   | Some(t) -> Some(to_save t));
   scharacteristics = record.characteristics}

let rec from_save_aux env ty =
  match ty with
    Var(id) -> 
      let nty = (
	try Lib.assocp (=) id (!env)
	with Not_found ->
	  let nid = ref(fst id)
	  in 
	  (env:=(id, nid)::(!env)); nid)
      in Var(nty)
  | Constr(f, ats) ->
      Constr(f, List.map(from_save_aux env) ats)
  | Base(b) -> Base(b)
  | WeakVar _ -> raise (typeError "Can't load a weak variable" [])

let from_save ty = from_save_aux (ref[]) ty
let from_save_env env ty = from_save_aux env ty

let from_save_rec record = 
  {name=record.sname;
   args = record.sargs;
   alias = 
   (match record.salias with
     None -> None
   | Some(t) -> Some(from_save t));
   characteristics = record.scharacteristics}

(* 
   USING HASHTABLES FOR SUBSTITUTION
   IS WRONG AND SHOULD BE REMOVED
 *)

module type RHASHKEYS=
  sig 
    type t = gtype
    val equal : t -> t -> bool
    val hash: t -> int
  end

module Rhashkeys:RHASHKEYS=
  struct
    type t = gtype
    let equal = equals
    let hash= Hashtbl.hash
  end
module type RHASH = (Hashtbl.S with type key = (gtype))
module Rhash:RHASH= Hashtbl.Make(Rhashkeys)



(* Type unification and matching *)

exception Occurs
exception Unify
exception Match

let rec lookup_ty t env =
  try
    (let nt = (lookup t env)
    in 
    if is_any_var nt then 
      lookup_ty nt env
    else nt)
  with Not_found -> t


let lookup_var ty env =
  let rec chase t =
    if is_any_var t
    then
      try (chase (lookup t env))
      with Not_found -> t
    else t
  in chase ty

let extract_bindings tyvars src dst=
  let rec extract_aux vs r=
    match vs with
      [] -> r
    | (x::xs) -> 
	let y= 
	  try lookup_var x src
	  with Not_found -> x
	in 
	if is_any_var y
	then extract_aux xs r
	else extract_aux xs (bind x y r)
  in extract_aux tyvars dst
    
let rec occurs ty1 ty2 =
  match (ty1, ty2) with
    (Var(_), (Constr(f, l))) ->  List.iter (occurs ty1) l
  | (WeakVar(_), (Constr(f, l))) ->  List.iter (occurs ty1) l
  | (_, _) ->
      if (equals ty1 ty2)
      then raise (typeError ("occurs: ") [ty1;ty2])
      else ()

let rec occurs_env tenv ty1 ty2 =
  let nty1=lookup_var ty1 tenv
  and nty2 = lookup_var ty2 tenv
  in 
  match (nty1, nty2) with
    (Var(_), (Constr(f, l))) ->  List.iter (occurs_env tenv nty1) l
  | (Var(_), _)->
      if (equals nty1 nty2)
      then raise (typeError ("occurs: ") [nty1;nty2])
      else ()
  | (WeakVar(_), (Constr(f, l))) ->  List.iter (occurs_env tenv nty1) l
  | (WeakVar(_), _)->
      if (equals nty1 nty2)
      then raise (typeError ("occurs: ") [nty1;nty2])
      else ()
  | (Constr(_, l), _) -> 
      List.iter (fun x-> occurs_env tenv x nty2) l
  | _ -> 
      if (equals nty1 nty2)
      then raise (typeError ("occurs: ") [nty1;nty2])
      else ()

(* copy_type t: make a copy of type t, which differs from t in the vars *)

let rec copy_type_env env trm=
  match trm with
    Var(x) -> 
      (try (lookup trm env, env)
      with 
	Not_found -> 
	  let nt=mk_var (!x)
	  in 
	  (nt, bind trm nt env))
  | Constr(f, args) ->
      let renv = ref env
      in 
      let nargs = 
	List.map
	  (fun t ->
	    let nt, ne= copy_type_env (!renv) t
	    in renv:=ne; nt) 
	  args
      in 
      (Constr(f, nargs), !renv)
  | WeakVar(x) -> 
      (try (lookup trm env, env)
      with 
	Not_found -> (trm, env))
  | _ -> (trm, env)

let copy_type t =
  let nty, _ = copy_type_env (empty_subst()) t
  in nty

let bind_var t r env = 
  if is_any_var t then bind t r env 
  else 
    raise (typeError "bind_var: Can't bind a non variable" [t; r])

let bind_occs t1 t2 env =
  if is_any_var t1
  then 
    (let r1 = lookup_var t1 env
    and r2 = (lookup_var t2 env)
    in 
    (occurs_env env r1 r2; bind_var r1 r2 env))
  else 
    raise (typeError "bind_occs: Can't bind a non variable" [t1; t2])

let rec subst t env =
  match t with 
    Var(a) -> (lookup_var t env)
  | WeakVar(a) -> (lookup_var t env)
  | Constr(f, l) -> 
      Constr(f, List.map (fun x-> subst x env) l)
  | x -> x

(* rewrite_subst t env: 

   Rewrite t, using substitution env.  

   Only used to rewrite the rhs of a definition,
   instantiating its variables with the values of the given arguments.
 *)


let rec rewrite_subst t env =
  match t with 
    Var(a) -> 
      (try Lib.find (!a) env 
      with Not_found -> raise
	  (typeError "rewrite_subst: Can't find parameter" [t])) 
  | Constr(f, l)
    -> Constr(f, List.map (fun x-> rewrite_subst x env) l) 
  | x -> x

let rewrite_defn given_args rcrd_args t=
  if (List.length rcrd_args)=(List.length given_args)
  then 
    (let tenv = Lib.empty_env()
    in 
    let env_of_args ()= 
      ((List.iter2 
	  (fun x y -> 
	    (Lib.bind_env x y tenv)) rcrd_args given_args))
    in (env_of_args(); rewrite_subst t tenv))
  else raise (typeError "rewrite_defn: Wrong number of arguments" [t])

let has_record tyenv t = 
  match t with
    Constr(Defined n, args) ->
      (try (ignore(get_typdef tyenv n)); true
      with _ -> false)
  | _ -> false

let has_defn tyenv n = 
  (try
    (match (get_typdef tyenv  n).alias with 
      None -> false | _ -> true)
  with Not_found -> false)

let get_defn tyenv t = 
  match t with
    Constr(Defined n, args) ->
      let recrd = get_typdef tyenv n 
      in 
      (match recrd.alias with
	None -> raise Not_found
      |	Some(gt) -> rewrite_defn args (recrd.args)  gt)
  | _ -> raise Not_found


let rec remove_bindings ls env =
  match ls with
    [] -> env
  | (s::bs) -> 
      remove_bindings bs (delete s env)


(* unify_env scp t1 t2 nenv:

   Unify types t1 and t2, adding new bindings to substitution nenv.
   
   Scope scp is used lookup definitions of constructors occuring 
   in t1 or t2 (if necessary).
 *)

let unify_env scp t1 t2 nenv =  
  let rec unify_aux ty1 ty2 env=
    let s = lookup_var ty1 env
    and t = lookup_var ty2 env
    in 
    match (s, t) with
(* Constructors *)
      (Constr(f1, args1), Constr(f2, args2)) ->
	(if f1=f2   (* matching constructors *)
	then 
	  (try 
	    (List.fold_left2
	       (fun ev x y -> unify_aux x y ev)
	       env args1 args2)
	  with 
	    x -> addtypeError "Can't unify types" [s; t] x)
	else 
	  (try (* different constructors, try for type aliasing *)
	    (try   (* try rewriting left constructor *)
	      let s1=get_defn scp s 
	      in unify_aux s1 t env
	    with 
              Not_found ->  (* failed so try with right constructor *)
		let t1= get_defn scp t
		in unify_aux s t1 env)
	  with
            x ->(addtypeError "x: Can't unify types" [s; t] x)))
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
(* All other types, try for equals *)
    | _ -> 
	if equals s t then env
	else (raise (typeError "Can't unify types" [s; t]))
  in
  unify_aux t1 t2 nenv (* try to unify t1 and t2 *)

(* unify scp t1 t2:

   Unify types t1 and t2, returning the substitution needed.
   Raise typeError unification fails
 *)
    
let unify scp t1 t2 =
  unify_env scp t1 t2 (empty_subst())

(* unify_env_unique_left:  type unification  
   for term rewriting in which each of a series of unificiations
   involving a term (and therefore its types) must be treated as
   unique. 

   [unify_env_unique_left scope t1 t2 env]
   is equivalent to
   [unify_env scope (copy_type t1) t2 env]
 *)

let unify_env_unique_left scp t1 t2 nenv =  
  let varenv= empty_subst()
  in 
  let copy_ty ty1 env =
    match ty1 with
      (Var(x)) -> 
	(try  ((lookup_var ty1 env), env)
	with Not_found -> 
	  (let nt=mk_var (!x)
	  in (nt, bind ty1 nt env)))
    | WeakVar(x) -> 
	(try  (lookup_var ty1 env, env)
	with Not_found -> (ty1, env))
    | _ -> (ty1, env)
  in 
  let rec unify_aux ty1 ty2 env =
    let s, senv = 
      (* make fresh variable if needed *)
      let (s1, s1env)=  
	copy_ty ty1 env 
      in 
      (lookup_var s1 s1env, s1env)
    in 
    let t = lookup_var ty2 senv
    in 
    match (s, t) with
      (Constr(f1, args1), Constr(f2, args2)) ->
	(if f1=f2
	then 
	  (try 
	    (List.fold_left2
	       (fun ev x y -> unify_aux x y ev) senv args1 args2)
	  with 
	    _ -> raise (Failure ("Can't unify " ^(string_gtype s)
				 ^" with " ^(string_gtype t))))
	else 
	  (try (unify_aux (get_defn scp s) t senv)
	  with _ ->
	    (try
	      (unify_aux s (get_defn scp t) senv)
	    with _ ->
	      raise (Failure ("Can't unify " ^(string_gtype s)
			      ^" with " ^(string_gtype t))))))
    | (Var(_), Var(_)) ->
	if equals s t 
	then senv
	else bind_occs s t senv
    | (Var(v1), x) -> bind_occs s x senv
    | (x, Var(v2)) -> bind_occs t x senv
    | (WeakVar(_), WeakVar(_)) ->
	if equals s t 
	then senv
	else bind_occs s t senv
    | (WeakVar(v1), x) -> bind_occs s x senv
    | (x, WeakVar(v2)) -> bind_occs t x senv

    | _ -> 
	if equals s t then senv
	else raise (Failure ("Can't unify " ^(string_gtype s)
			     ^" with " ^(string_gtype t)))
  in
  unify_aux t1 t2 nenv (* try to unify t1 and t2 *)

(*
   unify_for_rewrite: same as unify_env_unique_left
   except it returns a list of the bindings made
   if any error, removes bindings and raises exception 
 *)

let unify_for_rewrite scp t1 t2 env = 
  let varenv= empty_subst()
  in 
  let copy_ty ty1 env =
    match ty1 with
      (Var(x)) -> 
	(try  (lookup_var ty1 env, env)
	with Not_found -> 
	  let nt=mk_var (!x)
	  in (nt, bind ty1 nt env))
    | (WeakVar(x)) -> 
	(try (lookup_var ty1 env, env)
	with Not_found -> (ty1, env))
    | _ -> (ty1, env)
  in 
  let rec unify_aux ty1 ty2 env =
    let s, senv = 
      let s1, s1env=                (* make fresh variable if needed *)
	copy_ty ty1 env
      in (lookup_var s1 s1env, s1env)
    in 
    let t = lookup_var ty2 senv
    in 
    match (s, t) with
      (Constr(f1, args1), Constr(f2, args2)) ->
	(if f1=f2
	then 
	  (try 
	    (List.fold_left2
	       (fun ev x y -> unify_aux x y ev) senv args1 args2)
	  with 
	    _ -> raise (Failure ("Can't unify " ^(string_gtype s)
				 ^" with " ^(string_gtype t))))
	else 
	  (try (unify_aux (get_defn scp s) t senv)
	  with _ ->
	    (try
	      (unify_aux s (get_defn scp t) senv)
	    with _ ->
	      raise (Failure ("Can't unify " ^(string_gtype s)
			      ^" with " ^(string_gtype t))))))
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
    | _ -> 
	if equals s t then senv
	else raise (Failure ("Can't unify " ^(string_gtype s)
			     ^" with " ^(string_gtype t)))
  in
  unify_aux t1 t2 env (* try to unify t1 and t2 *)

(* mgu t env:

   Get most general unifier for type t and substitution env.
   Replaces variables in t with their bindings in env.
 *)

let rec mgu t env =
  match t with 
    Var(a) -> 
      (let nt = lookup_var t env 
      in 
      if is_var nt 
      then nt 
      else mgu nt env)
  | WeakVar(a) -> 
      (let nt = lookup_var t env 
      in 
      if is_any_var nt 
      then nt 
      else mgu nt env)
  | Constr(f, l) -> 
      Constr(f, List.map (fun x-> mgu x env) l)
  | x -> x

(* matching_env scp t1 t2 nenv:

   Like unify_env but only variables in type t1 can be bound.
 *)

let matching_env scp t1 t2 nenv =  
  let rec match_aux ty1 ty2 env=
    let s = lookup_var ty1 env
    and t =  ty2 
    in 
    match (s, t) with
      (Constr(f1, args1), Constr(f2, args2)) ->
	(if f1=f2
	then 
	  (try 
	    (List.fold_left2
	       (fun ev x y -> match_aux x y ev) env args1 args2)
	  with 
	    x -> addtypeError "Can't match types" [s; t] x)
	else 
	  (try match_aux (get_defn scp s) t env
	  with 
            Not_found -> (match_aux s (get_defn scp t) env)
          | x -> (addtypeError "Can't match types" [s; t] x)))
    | (Var(_), Var(_)) ->
	if equals s t 
	then env
	else bind_occs s t env
    | (Var(v1), x) -> bind_occs s x env
    | (_, Var(_)) -> env
    | (WeakVar(_), WeakVar(_)) ->
	if equals s t 
	then env
	else bind_occs s t env
    | (WeakVar(v1), x) -> bind_occs s x env
    | (_, WeakVar(_)) -> env
    | _ -> 
	if equals s t then env
	else (raise (typeError "Can't match types" [s; t]))
  in
  match_aux t1 t2 nenv (* try to match t1 and t2 *)

let matching scp t1 t2 =
  let tenv = empty_subst ()
  in 
  (ignore(unify_env scp t1 t2 tenv); (mgu t1 tenv))

let matches_env scp tyenv t1 t2 = 
  try 
    let nenv=matching_env scp t1 t2 tyenv
    in true, nenv
  with _ -> (false, tyenv)

let matches scp t1 t2=
  try ignore(matching_env scp t1 t2 (empty_subst())) ; true
  with _ -> false


(* Consistency tests for type definitions 

   Weak variables are not permitted in any definition (type or term)
 *)

(* check_term n vs t: 
   for definition (n vs = t)
   test name n and arguments vs for definition t 
   fails 
   if a variable occurs in t which is not in the list vs
   or name n occurs in t (a recursive definition)
 *)

let rec check_term scp n vs t = 
  match t with
    Var(x) -> if List.mem (Var x) vs then () else raise Not_found
  | Constr(Defined m, args) -> 
      if n=m then raise Not_found
      else 
(*	if (has_defn scp n) then raise Not_found
   else 
 *)
	List.iter (check_term scp n vs) args
  | Constr(_, args) -> 
      List.iter (check_term scp n vs) args
  | WeakVar _ -> raise Not_found
  | x -> ()


(* check_args args: 
   test each a in args is a variable and occurs only once 
 *)

let check_args args =
  let tenv = empty_subst()
  in 
  try
    ignore
      (List.fold_left
	 (fun ev x-> 
	   if (is_var x) & (not (member x tenv))
	   then (bind x true ev)
	   else raise Not_found)
	 tenv args); true
  with Not_found -> false

(* check_defn l r: test definition of l as alias for r

   fails if the type is already defined,
   or if the definition is recursive
   or if the arguments occur on the lhs more than once
   or if there are variables in the rhs which are not in the arguments
 *)

let check_defn  scp l r =
  if (is_defined l) then 
    let n, largs = dest_def l 
    in 
    if(has_defn scp n) then false
    else 
      if check_args largs
      then 
	(try (check_term scp n largs r); true
	with Not_found -> false)
      else false
  else false

(* check_decln l: consistency check on declaration of type l *)

let check_decln l =
  if (is_defined l) then 
    let n, largs = dest_def l 
    in try (ignore(check_args largs); true)
    with Not_found -> false
  else false


(* well_defined t: ensure that all constructors in t are declared *)
(* args: (optional) check variables are in the list of args *)

let rec well_defined scp ?args t =
  let lookup_fn = ref (fun x -> ())
  in 
  let lookup a = (!lookup_fn) a
  in 
  let rec well_def t = 
    match t with 
      Constr(Defined n, args) ->
	(try 
          (let recrd=get_typdef scp n
          in 
          if (List.length args)=(List.length recrd.args)
          then (List.iter well_def args)
          else raise (Invalid_argument ("well_defined:"^(string_gtype t))))
	with Not_found -> 
	  raise (Invalid_argument ("well_defined: "^(string_gtype t))))
    | Constr(f, args) ->
	List.iter well_def args
    | Var(v) -> lookup (!v)
    | WeakVar(v) -> 
	raise (Invalid_argument ("well_defined:"^(string_gtype t)))
    | _ -> ()
  in 
  match args with 
    None -> well_def t
  | Some(xs) -> 
      lookup_fn:=(fun x -> ignore(List.find (fun y -> x=y) xs));
      well_def t

(* quick_well_defined: memoised, simpler version of well_defined
   no argument testing
 *)

let rec quick_well_defined scp cache t =
  match t with 
    Constr(Defined n, args) ->
      let nargs=List.length args
      in
      (try
	(Hashtbl.find cache (n, nargs) ;
	 List.iter (quick_well_defined scp cache) args)
      with Not_found ->
	(try 
	  (let recrd=get_typdef scp n
	  in 
	  if nargs=(List.length recrd.args)
	  then 
	    (Hashtbl.add cache (n, nargs) true ; 
	     List.iter (quick_well_defined scp cache) args)
	  else raise 
	      (Invalid_argument ("quick_well_defined: "^(string_gtype t))))
	with Not_found -> 
	  raise (Invalid_argument 
		   ("quick_well_defined:"^(string_gtype t)))))
  |	Constr(f, args) ->
      List.iter (quick_well_defined scp cache) args
  |	x -> ()




(* Debugging *)

let print_subst tenv = 
  Format.open_box 0;
  (subst_iter 
     (fun x y -> print_string 
	 ("("^(string_gtype x)^" = "
	  ^(string_gtype  y)^"):"))
     tenv);
  Format.close_box()


(* from typing.ml *)

let typeof_cnst c =
  match c with
    Null_const _-> 
      raise (typeError "Null constant has no type" [mk_ind])
  |	Cnum _ -> mk_num
  | Cbool _ -> mk_bool

let bin_ty a1 a2 r = (mkfun_from_list [a1; a2] r)

let typeof_conn c =
  match c with
    Not -> mkfun_from_list [mk_bool] mk_bool
  | x -> mkfun_from_list 
	[mk_bool; mk_bool] mk_bool

(* set names in a type to their long form *)

let set_name scp trm = 
  let memo = Lib.empty_env()
  in 
  let lookup_id n = 
    (try (Lib.find n memo)
    with Not_found -> 
      let nth = try (scp.thy_of Basic.type_id n) with _ -> scp.curr_thy
      in ignore(Lib.add n nth memo); nth)
  in 
  let rec set_aux t =
    match t with
      Constr(Defined id, args) -> 
	let th, n = Basic.dest_fnid id
	in 
	let nth = 
	  (if th=Basic.null_thy
	  then lookup_id n
	  else th)
	in 
	let nid=Basic.mklong nth n
	in Constr(Defined nid, List.map set_aux args)
    | Constr(f, args) ->
	Constr(f, List.map set_aux args)
    | _ -> t
  in set_aux trm


(* in_thy_scope memo scp th ty:

   Check that ty is in valid for theory th.
   Every type constructor must be in scope of th.

   The function is memoised: if a constructor name is found to be 
   in scope, it is added to memo.
 *)

let in_thy_scope memo scp th ty =
  let lookup_id n = 
    (try (Lib.find n memo)
    with Not_found -> 
      if (scp.thy_in_scope th n)
      then Lib.add n true memo else raise Not_found)
  in
  let rec in_scp_aux t =
    match t with
      Var(_) -> ()
    | Base(_) -> ()
    | Constr(Func, args) ->
	List.iter in_scp_aux args
    | Constr(Defined(id), args) ->
	ignore(lookup_id (thy_of_id id));
	List.iter in_scp_aux args
    | WeakVar _ -> ()
  in 
  try in_scp_aux ty ; true
  with Not_found -> false

(* mgu_rename_env inf env env nenv typ:

   Replace variables in typ with their bindings in substitution env.
   If a variable isn't bound in env, then it is renamed and bound
   to that name in nenv (which is checked before a new name is created).
 *)

(*
   let mgu_rename_env inf env nenv typ =
   let new_name_env tenv x =
   try (lookup x env, tenv)
   with 
   Not_found ->
   (let newty=mk_typevar inf
   in 
   (newty, bind_var x newty tenv))
   in 
   let rec rename_aux (tenv: substitution) ty=
   match ty with
   Var(_) ->
   (let nt=lookup_var ty env
   in 
   if(is_var nt)
   then new_name_env tenv nt
   else rename_aux tenv nt)
   | WeakVar(_) ->
   (let nt=lookup_var ty env
   in 
   if(is_weak nt) then (nt, env)
   else rename_aux tenv nt)
   | Constr(f, args) -> 
   let renv=ref tenv
   in 
   let nargs=
   List.map
   (fun t -> 
   let nt, ne=rename_aux (!renv) t
   in 
   renv:=ne; nt) args
   in 
   (Constr(f, nargs), !renv)
   | _ -> (ty, tenv)
   in 
   rename_aux nenv typ
 *)

let mgu_rename_env inf tyenv name_env typ =
  let new_name_env nenv x =
    try (lookup x nenv, nenv)
    with 
      Not_found ->
	let newty=mk_typevar inf
	in 
	(newty, bind_var x newty nenv)
  in 
  let rec rename_aux (nenv: substitution) ty=
    match ty with
      Var(_) ->
	let nt=lookup_var ty tyenv
	in 
	if (equals ty nt)
	then new_name_env nenv nt
	else rename_aux nenv nt
    | WeakVar(_) ->
	(let nt=lookup_var ty tyenv
	in 
	if(is_weak nt) then (nt, nenv)
	else rename_aux nenv nt)
    | Constr(f, args) -> 
	let renv=ref nenv
	in 
	let nargs=
	  List.map
	    (fun t -> 
	      let nt, ne=rename_aux (!renv) t
	      in 
	      renv:=ne; nt) args
	in 
	(Constr(f, nargs), !renv)
    | _ -> (ty, nenv)
  in 
  rename_aux name_env typ


let mgu_rename inf env nenv typ =
  let nty, _ = mgu_rename_env inf env nenv typ
  in nty
