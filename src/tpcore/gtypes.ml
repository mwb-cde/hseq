
open Basic
open Lib
open Corepp
open Result

type ('idtyp, 'tfun, 'tcons) pre_typ =
    Var of 'idtyp
  | Constr of 'tfun * ('idtyp, 'tfun, 'tcons) pre_typ list
  | Base of 'tcons

type gtype = (string ref, typ_const, base_typ)pre_typ
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
     typeof_fn : fnident -> gtype; 
       typ_defn: fnident -> typedef_record;
	 thy_of:  id_selector ->string -> thy_id;
	   prec_of: id_selector -> fnident -> int;
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

(*
   type type_env = fnident -> typedef_record
   let empty_typing () = (fun x -> raise Not_found)

   let cur_typenv= ref (empty_typing())
   let set_typenv typenv = cur_typenv:=typenv
   let get_typenv () = !cur_typenv
 *)



(* pretty printing *)


let get_var t = 
  match t with 
    Var(n) -> !n
  | _ -> raise (Failure "Not a variable")


let print_type_info ppstate pr t = 
  let rec print_aux old_pr x =
    match x with
      Var(_) -> Format.print_string ("'"^(get_var x))
    | Base(b) -> Format.print_string (Basic.string_btype b)
    | Constr(Basic.Defined n, [a1; a2])  (* possible infix *)
      ->
	(let pp_rec=ppstate.type_info n
	in 
	let infix=pp_rec.infix 
	and prec = pp_rec.prec
	in 
 	let brckt=prec<old_pr
	in 
	(Format.open_box 0;
	 if infix 
	 then (if brckt then Format.print_string "(" else ();
	       print_aux prec a1; 
	       Format.print_string 
		 (" "^(string_identifier n pp_rec)^" ");
	       print_aux prec a2;
	       if brckt then Format.print_string ")" else ())
	 else (Format.print_string 
		 (string_identifier n pp_rec);
	       Format.print_string "(";
	       list_print (print_aux prec) (fun _ -> ", ") [a1;a2];
	       Format.print_string ")");
	 Format.close_box ()))
    | Constr(Basic.Defined n, args) ->  (* not infix *)
	(let pp_rec=ppstate.type_info n 
	in 
	let prec=pp_rec.prec
	in 
	(Format.open_box 0;
	 Format.print_string (string_identifier n pp_rec);
	 Format.print_string "(";
	 list_print (print_aux prec ) (fun _ -> ", ") args;
	 Format.print_string ")";
	 Format.close_box ()))
    | Constr(Basic.Func, [a1; a2]) ->  (* not infix *)
	Format.open_box 0;
	print_aux 0 a1; 
	Format.print_string "->";
	print_aux 0 a2;
	Format.close_box()
    | Constr(Basic.Func, args) ->  (* not infix *)	      
	(Format.open_box 0;
	 Format.print_string "->";
	 Format.print_string "(";
	 list_print (print_aux 0) (fun _ -> ", ") args;
	 Format.print_string ")";
	 Format.close_box ())
  in print_aux pr t

let print_type st x =
  Format.open_box 0; print_type_info st 0 x; Format.close_box ()

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
      Corepp.list_print (print_type st) 
	(fun _ -> Format.print_string ","; 
          Format.print_break 1 2; )
	(self#get());
      Format.close_box();
      Format.close_box();
  end
let typeError s t = (mkError((new typeError s t):>error))
let addtypeError s t es= raise (addError ((new typeError s t):>error) es)


let rec string_gtype x =
  match x with
    Var(a) -> "'"^(!a)
  | Constr(f, args) ->  
      string_tconst f (List.map string_gtype args)
  | Base(b) -> string_btype b


let varp t  = 
  match t with
    Var _ -> true
  | _ -> false

let constrp t = 
  match t with
    Constr _ -> true
  | _ -> false

let funcp t = 
  match t with
    Constr (Func, _) -> true
  | _ -> false

let definedp t = 
  match t with
    Constr (Defined _ , _) -> true
  | _ -> false

let basep t  = 
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

(* substitution types *)

(*
   let rec equality x y = 
   Dequals.is_equal x y
 *)
let rec equality x y = 
  (match (x, y) with
    (Var(v1), Var(v2)) -> (v1==v2)
  |	(Constr(f1, args1), Constr(f2, args2))
    -> (f1=f2) &
      (try
	((List.iter2 
	    (fun a b -> 
	      if (equality a b) 
	      then () 
	      else raise (Failure ""))
	    args1 args2); true)
      with _ -> false)
  |	(_, _) -> x=y)

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
  |	(_, _) -> x=y)


let eqvar v1 v2 = 
  if (varp v1) & (varp v2) 
  then equality v1 v2
  else raise (Failure "Not a variable")




(* save types *)

let rec to_save_aux inf env ty =
  match ty with
    Var(id) -> 
      let nty=
	(try Lib.assocp (==) id (!env)	    with Not_found -> 
	  let nid= inf:=!inf+1; (!id, !inf)
	  in 
	  env:= ((id, nid)::(!env)); nid)
      in Var(nty)
  |	Constr(f, ats) ->
      Constr(f, List.map (to_save_aux inf env) ats)
  |	Base(b) -> Base(b)

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
  |	Constr(f, ats) ->
      Constr(f, List.map(from_save_aux env) ats)
  |	Base(b) -> Base(b)

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


(* USING HASHTABLES 
   THIS IS WRONG AND MUST BE REMOVED
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
    let equal = equality
    let hash= Hashtbl.hash
  end
module type RHASH = (Hashtbl.S with type key = (gtype))
module Rhash:RHASH= Hashtbl.Make(Rhashkeys)

type substitution = (gtype)Rhash.t

let lookup t env = Rhash.find env t
let member t env = try lookup t env ; true with Not_found -> false

let bind_env t r env = (Rhash.remove env t; Rhash.add env t r)
let subst_sz s= Rhash.create s
let empty_subst ()= Rhash.create 5
let bind t r env = (Rhash.remove env t; Rhash.add env t r; r)
let delete t env = Rhash.remove env t
let subst_iter =Rhash.iter


(* Using balanced trees *)
(*
module TypeTreeData=
  struct 
    type key=gtype
    let equals = equality
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
*)

(* Type unification and matching *)

exception Occurs
exception Unify
exception Match

let rec lookup_ty t env =
  try
    (let nt = (lookup t env)
    in 
    if varp nt then 
      lookup_ty nt env
    else nt)
  with Not_found -> t


let lookup_var ty env =
  let rec chase t =
    if varp t
    then
      try (chase (lookup t env))
      with Not_found -> t
    else t
  in chase ty

(* raise Occurs *)

(*
   let rec occurs vt t =
   match vt with
   Var(a) -> 
   (match t with
   Var(_) ->  
   if equality vt t 
   then raise (typeError ("occurs: ") [vt;t])
   else ()
   | Constr(f, l) ->
   List.iter (occurs vt) l
   | x -> ())
   | _ -> ()
 *)

let rec occurs ty1 ty2 =
  match (ty1, ty2) with
    (Var(_), (Constr(f, l))) ->  List.iter (occurs ty1) l
  | (_, _) ->
      if (equality ty1 ty2)
      then raise (typeError ("occurs: ") [ty1;ty2])
      else ()

let rec occurs_env tenv ty1 ty2 =
  let nty1=lookup_var ty1 tenv
  and nty2 = lookup_var ty2 tenv
  in 
  match (nty1, nty2) with
    (Var(_), (Constr(f, l))) ->  List.iter (occurs_env tenv nty1) l
  | (Var(_), _)->
      if (equality nty1 nty2)
      then raise (typeError ("occurs: ") [nty1;nty2])
      else ()
  | (Constr(_, l), _) -> 
      List.iter (fun x-> occurs_env tenv x nty2) l
  | _ -> 
      if (equality nty1 nty2)
      then raise (typeError ("occurs: ") [nty1;nty2])
      else ()



(* copy_type t: make a copy of type t, which differs from t in the vars *)


let copy_type_env env trm=
  let rec copy_aux t=
    match t with
      Var(x) -> 
	(try lookup t env
	with 
	  Not_found -> 
	  let nt=mk_var (!x)
	  in 
	  ignore(bind t nt env);
	  nt)
    | Constr(f, args) ->
	Constr(f, List.map copy_aux args)
    | _ -> t
  in copy_aux trm

let copy_type t =
  copy_type_env  (empty_subst()) t


let bind_var t r env = 
  if varp t then bind_env t r env 
  else 
    raise (typeError "bind_var: Can't bind a non variable" [t; r])

let bind_occs t1 t2 env =
  if varp t1
  then 
    (let r1 = lookup_var t1 env
    and r2 = (lookup_var t2 env)
    in 
    (occurs_env env r1 r2; bind_var r1 r2 env))
  else raise (typeError "bind_occs: Can't bind a non variable" [t1; t2])


let rec subst t env =
  match t with 
    Var(a) -> (lookup_var t env)
  | Constr(f, l) -> 
      Constr(f, List.map (fun x-> subst x env) l)
  | x -> x


let rec rewrite_subst t env =
  match t with 
    Var(a) -> 
      (try Lib.find (!a) env
      with Not_found -> 
	raise (typeError "rewrite_subst: Can't find parameter" [t]))
  | Constr(f, l) -> 
      Constr(f, List.map (fun x-> rewrite_subst x env) l)
  | x -> x

(*
   let rewrite_defn dargs largs t=
   if (List.length dargs)=(List.length largs)
   then 
   (let tenv = Lib.empty_env() 
   in 
   let env_of_args ()= 
   ((List.iter2 
   (fun x y -> 
   (Hashtbl.add tenv (mk_var x) y )) largs dargs))
   in (env_of_args(); rewrite_subst t tenv))
   else raise (Failure "Wrong number of arguments")
 *)

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


let remove_bindings ls env =
  let rec remove_aux ds =
    match ds with
      [] -> env
    | (s::bs) -> 
	delete s env; 
	remove_aux bs
  in 
  remove_aux ls

let unify_env tyenv t1 t2 env =  
  let bindings = ref([])
  in 
  let add_binding a = bindings:=(a::!bindings)
  in 
  let rec unify_aux ty1 ty2 =
    let s = lookup_var ty1 env
    and t = lookup_var ty2 env
    in 
    match (s, t) with
      (Constr(f1, args1), Constr(f2, args2)) ->
	(if f1=f2
	then 
	  (try 
	    (List.iter2
	       (fun x y -> unify_aux x y) args1 args2)
	  with 
	    x -> addtypeError "Can't unify types" [s; t] x)
	else 
	  (try 
	    (try 
	      let s1=get_defn tyenv s
	      in unify_aux s1 t
	    with 
              Not_found -> 
		let t1= get_defn tyenv t
		in unify_aux s t1)
	  with
            x ->(addtypeError "x: Can't unify types" [s; t] x)))
    | (Var(_), Var(_)) ->
	if equality s t 
	then ()
	else bind_occs s t env; add_binding s
    | (Var(_), _) -> bind_occs s t env; add_binding s
    | (_, Var(_)) -> bind_occs t s env; add_binding t
    | _ -> 
	if equality s t then () 
	else (raise (typeError "Can't unify types" [s; t]))
  in
  try ((unify_aux t1 t2); env) (* try to unify t1 and t2 *)
  with x ->                    (* if can't unify, remove bindings *)
    (ignore(remove_bindings (!bindings) env); raise x)
      
let unify tyenv t1 t2 =
  let tenv = empty_subst()
  in
  (ignore(unify_env tyenv t1 t2 tenv); tenv)

(* unify_env_unique_left:  type unification  
   for term rewriting in which each of a series of unificiations
   involving a term (and therefore its types) must be treated as
   unique. 

   [unify_env_unique_left scope t1 t2 env]
   is equivalent to
   [unify_env scope (copy_type t1) t2 env]
 *)

let unify_env_unique_left tyenv t1 t2 env =  
  let varenv= empty_subst()
  and bindings = ref([])
  in 
  let add_binding a = bindings:=(a::!bindings)
  in 
  let rec unify_aux ty1 ty2 =
    let s = 
      let s1=                (* make fresh variable if needed *)
      	match ty1 with
	  (Var(x)) -> 
	    (try  (lookup_var ty1 env)
	    with Not_found -> bind ty1 (mk_var (!x)) env)
	| _ -> ty1
      in lookup_var s1 env
    and t = lookup_var ty2 env
    in 
    match (s, t) with
      (Constr(f1, args1), Constr(f2, args2)) ->
	(if f1=f2
	then 
	  (try 
	    (List.iter2
	       (fun x y -> unify_aux x y) args1 args2)
	  with 
	    _ -> raise (Failure ("Can't unify " ^(string_gtype s)
				 ^" with " ^(string_gtype t))))
	else 
	  (try (unify_aux (get_defn tyenv s) t)
	  with _ ->
	    (try
	      (unify_aux s (get_defn tyenv t))
	    with _ ->
	      raise (Failure ("Can't unify " ^(string_gtype s)
			      ^" with " ^(string_gtype t))))))
    | (Var(_), Var(_)) ->
	if equality s t 
	then ()
	else bind_occs s t env; add_binding s
    | (Var(v1), x) -> bind_occs s x env; add_binding s
    | (x, Var(v2)) -> bind_occs t x env; add_binding t
    | _ -> 
	if equality s t then () 
	else raise (Failure ("Can't unify " ^(string_gtype s)
			     ^" with " ^(string_gtype t)))
  in
  try ((unify_aux t1 t2); env) (* try to unify t1 and t2 *)
  with x ->                    (* if can't unify, remove bindings *)
    (ignore(remove_bindings (!bindings) env); raise x)

(*
   unify_for_rewrite: same as unify_env_unique_left
   except it returns a list of the bindings made
   if any error, removes bindings and raises exception 
 *)

let unify_for_rewrite tyenv t1 t2  env bindings=  
  let varenv= empty_subst()
(*  and bindings = ref([]) *)
  in 
  let add_binding a = bindings:=(a::!bindings)
  in 
  let rec unify_aux ty1 ty2 =
    let s = 
      let s1=                (* make fresh variable if needed *)
      	match ty1 with
	  (Var(x)) -> 
	    (try  (lookup_var ty1 env)
	    with Not_found -> bind ty1 (mk_var (!x)) env)
	| _ -> ty1
      in lookup_var s1 env
    and t = lookup_var ty2 env
    in 
    match (s, t) with
      (Constr(f1, args1), Constr(f2, args2)) ->
	(if f1=f2
	then 
	  (try 
	    (List.iter2
	       (fun x y -> unify_aux x y) args1 args2)
	  with 
	    _ -> raise (Failure ("Can't unify " ^(string_gtype s)
				 ^" with " ^(string_gtype t))))
	else 
	  (try (unify_aux (get_defn tyenv s) t)
	  with _ ->
	    (try
	      (unify_aux s (get_defn tyenv t))
	    with _ ->
	      raise (Failure ("Can't unify " ^(string_gtype s)
			      ^" with " ^(string_gtype t))))))
    | (Var(_), Var(_)) ->
	if equality s t 
	then ()
	else bind_occs s t env; add_binding s
    | (Var(v1), x) -> bind_occs s x env; add_binding s
    | (x, Var(v2)) -> bind_occs t x env; add_binding t
    | _ -> 
	if equality s t then () 
	else raise (Failure ("Can't unify " ^(string_gtype s)
			     ^" with " ^(string_gtype t)))
  in
  try ((unify_aux t1 t2); (* try to unify t1 and t2 *)
       !bindings) 
  with x ->                    (* if can't unify, remove bindings *)
    (ignore(remove_bindings (!bindings) env); raise x)



let rec mgu t env =
  match t with 
    Var(a) -> 
      (let nt = lookup_var t env 
      in 
      if varp nt 
      then nt 
      else mgu nt env)
  | Constr(f, l) -> 
      Constr(f, List.map (fun x-> mgu x env) l)
  | x -> x


let matching_env tyenv t1 t2 env =  
  let bindings = ref([])
  in 
  let add_binding a = bindings:=(a::!bindings)
  in 
  let rec match_aux ty1 ty2 =
    let s = lookup_var ty1 env
    and t =  ty2 
    in 
    match (s, t) with
      (Constr(f1, args1), Constr(f2, args2)) ->
	(if f1=f2
	then 
	  (try 
	    (List.iter2
	       (fun x y -> match_aux x y) args1 args2)
	  with 
	    x -> addtypeError "Can't match types" [s; t] x)
	else 
	  (try match_aux (get_defn tyenv s) t
	  with 
            Not_found -> (match_aux s (get_defn tyenv t))
          | x -> (addtypeError "Can't match types" [s; t] x)))
    | (Var(_), Var(_)) ->
	if equality s t 
	then ()
	else bind_occs s t env; add_binding s
    | (Var(v1), x) -> bind_occs s x env; add_binding s
    | (_, Var(_)) -> ()
    | _ -> 
	if equality s t then () 
	else (raise (typeError "Can't match types" [s; t]))
  in
  try ((match_aux t1 t2); env) (* try to match t1 and t2 *)
  with x ->                    (* if can't match, remove bindings *)
    (ignore(remove_bindings (!bindings) env); raise x)


(*
   let matching_env tyenv t1 t2 env =
   let rec match_aux ty1 ty2 =
   let s=lookup_var ty1 env
   and t = ty2
   in 
   (match (s, t) with
   (Var(_), _) -> bind_env s t env
   | (_, Var(_)) -> ()
   | (Constr(f1, args1), (Constr(f2, args2))) ->
   if f1=f2 
   then
   (try
   (List.iter2
   (fun x y -> match_aux x y)
   args1 args2)
   with _ -> raise Match)
   |	_ -> if equality s t then () else raise Match)
   in 
   ((match_aux t1 t2); (mgu t1 env))
 *)
let matching tyenv t1 t2 =
  let tenv = empty_subst ()
  in 
(*  ((matching_env tyenv t1 t2 tenv); (mgu t1 tenv))*)
  (ignore(unify_env tyenv t1 t2 tenv); (mgu t1 tenv))

let matches tyenv t1 t2=
  try ignore(matching_env tyenv t1 t2 (empty_subst())) ; true
  with _ -> false

(* Consistency tests for type definitions *)

(* check_term n vs t: test name n and arguments vs for definition of t *)

let rec check_term tyenv n vs t = 
  match t with
    Var(x) -> if List.mem (Var x) vs then () else raise Not_found
  |	Constr(Defined m, args) -> 
      if n=m then raise Not_found
      else 
	if (has_defn tyenv n) then raise Not_found
	else List.iter (check_term tyenv n vs) args
  |	Constr(_, args) -> 
      List.iter (check_term tyenv n vs) args
  |	x -> ()

(* check_args: ensure all arguments are variable types *)

(*
   let check_args args =
   let tenv = Lib.env_size 1 
   in 
   try
   (List.iter 
   (fun x-> 
   if (varp x) & (not (member x tenv))
   then
   (Hashtbl.add tenv x true)
   else raise Not_found)
   args);
   true
   with Not_found -> false
 *)
let check_args args =
  let tenv = empty_subst()
  in 
  try
    (List.iter 
       (fun x-> 
	 if (varp x) & (not (member x tenv))
	 then
	   (bind_env x true tenv)
	 else raise Not_found)
       args);
    true
  with Not_found -> false


(* check_defn l r: test defintion of l as alias for r *)

let check_defn  tyenv l r =
  if (definedp l) then 
    let n, largs = dest_def l 
    in 
    if check_args largs
    then 
      (try (check_term tyenv n largs r); true
      with Not_found -> false)
    else false
  else false

(* check_decln l: consistency check on declaration of type l *)

let check_decln l =
  if (definedp l) then 
    let n, largs = dest_def l 
    in try (ignore(check_args largs); true)
    with Not_found -> false
  else false

(* well_defined t: ensure that all constructors in t are declared *)
(* args: (optional) check variables are in the list of args *)

(*
   let rec well_defined tyenv t =
   match t with 
   Constr(Defined n, args) ->
   (try 
   (let recrd=get_typdef tyenv n
   in 
   if (List.length args)=(List.length recrd.args)
   then (List.iter (well_defined tyenv) args)
   else raise (Invalid_argument ("well_defined:"^(string_gtype t))))
   with Not_found -> 
   raise (Invalid_argument ("well_defined: "^(string_gtype t))))
   |	Constr(f, args) ->
   List.iter (well_defined tyenv) args
   |	x -> ()
 *)

let rec well_defined tyenv ?args t =
  let lookup_fn = ref (fun x -> ())
  in 
  let lookup a = (!lookup_fn) a
  in 
  let rec well_def t = 
    match t with 
      Constr(Defined n, args) ->
	(try 
          (let recrd=get_typdef tyenv n
          in 
          if (List.length args)=(List.length recrd.args)
          then (List.iter well_def args)
          else raise (Invalid_argument ("well_defined:"^(string_gtype t))))
	with Not_found -> 
	  raise (Invalid_argument ("well_defined: "^(string_gtype t))))
    |	Constr(f, args) ->
	List.iter well_def args
    |	Var(v) -> lookup (!v)
    | _  -> ()
  in 
  match args with 
    None -> well_def t
  | Some(xs) -> 
      lookup_fn:=(fun x -> ignore(List.find (fun y -> x=y) xs));
      well_def t

(* quick_well_defined: memoised, simpler version of well_defined
   no argument testing
 *)

let rec quick_well_defined tyenv cache t =
  match t with 
    Constr(Defined n, args) ->
      let nargs=List.length args
      in
      (try
	(Hashtbl.find cache (n, nargs) ;
	 List.iter (quick_well_defined tyenv cache) args)
      with Not_found ->
	(try 
	  (let recrd=get_typdef tyenv n
	  in 
	  if nargs=(List.length recrd.args)
	  then (Hashtbl.add cache (n, nargs) true ; 
		List.iter (quick_well_defined tyenv cache) args)
	  else raise 
	      (Invalid_argument ("quick_well_defined: "^(string_gtype t))))
	with Not_found -> 
	  raise (Invalid_argument 
		   ("quick_well_defined:"^(string_gtype t)))))
  |	Constr(f, args) ->
      List.iter (quick_well_defined tyenv cache) args
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
  in 
  try in_scp_aux ty ; true
  with Not_found -> false

let mk_typevar n =
  let nty=mk_var(int_to_name (!n))
  in
  n:=(!n)+1;
  nty

let mgu_rename inf env nenv typ =
  let new_name x =
    try (lookup x nenv)
    with 
      Not_found ->
	(let newty=mk_typevar inf
	in 
	bind_var x newty nenv;
	newty)
  in 
  let rec rename_aux ty=
    match ty with
      Var(_) ->
	(let nt=lookup_var ty env
	in 
	if(varp nt)
	then new_name nt
	else 
	  rename_aux nt)
    | Constr(f, args) -> 
	Constr(f, List.map rename_aux args)
    | _ -> ty
  in 
  rename_aux typ
