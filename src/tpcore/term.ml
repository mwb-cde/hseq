open Lib
open Basic
open Gtypes
open Result

(*
type q_type = {quant: quant_ty; qvar: string; qtyp: gtype}

type binders = q_type ref

type term =
    Var of ident* gtype
  | Qnt of q_type ref * term
  | Bound of q_type ref
  | Const of const_ty
  | Typed of term * Gtypes.gtype
  | App of term * term

let binder_equality x y = x==y
let mk_binding qn qv qt 
    = ref{quant=qn; qvar=qv; qtyp=qt}
let dest_binding b = ((!b.quant), (!b.qvar), (!b.qtyp))
*)

let rec equals x y = 
  (match (x, y) with
    (App(f1, arg1), App(f2, arg2))->
      (equals f1 f2) & (equals arg1 arg2)
  | (Bound(q1), Bound(q2)) -> q1==q2
  | (Qnt(k1, qn1, b1), Qnt(k2, qn2, b2)) -> 
      k1=k2 & qn1==qn2 & (equals b1 b2)
  | (Typed(t1, ty1), Typed(t2, ty2)) ->
      ty1=ty2  & (equals t1 t2)
  | (_, _) -> x=y)

(* simple pretty printing  and error handling *)

(*
let simple_term_printer trm= 
*)
let print_simple trm=
  let rec print_aux t =
    match t with
      Id(n, ty) -> Format.print_string(Basic.string_fnid n)
    | Bound(n) -> Format.print_string (".."^(binder_name n))
    | Const(c) -> Format.print_string (Basic.string_const c)
    | Typed (trm, ty) ->
	Format.print_string "(";
	Format.open_box 0;
	print_aux  trm;
	Format.close_box();
	Format.print_string ": ";
	Format.print_string (Gtypes.string_gtype ty);
	Format.print_string ")"
    | App(t1, t2) ->
	Format.open_box 0;
	Format.print_string "(";
	print_aux t1;
	Format.print_string " ";
	print_aux t2;
	Format.print_string ")";
	Format.close_box ()
    | Qnt(k, q, body) ->
	Format.print_string (Basic.quant_string k);
	Format.print_string 
	  ("("^(binder_name q)
	   ^": "^(Gtypes.string_gtype (binder_type q))^")"^": ");
	Format.open_box 0;
	print_aux body;
	Format.close_box ();
  in print_aux trm

class basictermError s ts =
  object (self)
    inherit Result.error s
    val trms = (ts :term list)
    method get() = trms
    method print st = 
      Format.open_box 0; print_string ((self#msg())^" "); 
      Format.open_box 0; 
      Printer.print_sep_list 
	(print_simple, ",")
	(self#get());
      Format.close_box();
      Format.close_box();
  end
let basicError s t = mk_error((new basictermError s t):>error)

let get_binder_name x =
  match x with
    Bound(n) -> binder_name n
  | Qnt(_, n, _) -> binder_name n
  | _ -> raise (basicError "Not a binder" [x])


let dest_qnt t=
  match t with 
    (Qnt(qnt, q,b)) -> 
      let (_, qv, qt) = Basic.dest_binding q
      in (q, qnt, qv, t, b)
  | _  -> raise (Failure "Not a quantifier")

let is_qnt x = 
  match x with 
    Qnt _ -> true
  | _ -> false

let strip_qnt q trm =
  let rec strip_aux t qs=
    if is_qnt t 
    then
      (let (bind, qnt, _, _, b) = dest_qnt t
      in 
      (if qnt=q 
      then (strip_aux b (bind::qs))
      else (qs, t)))
    else (qs, t)
  in 
  let qnts, nt= strip_aux trm []
  in (List.rev qnts, nt)


(* 
   flatten_app: flatten an application to a list
   that is: (((f a1) a2) a3) -> [f; a1; a2; a3]
   and (((f a1) (g a2)) a3) -> [f; a1; (g a2); a3]
 *)

let rec flatten_app trm =
  let rec flat_aux t rslt =
    match t with
      App(l, r) -> flat_aux l (r::rslt)
    | _ -> t::rslt
  in 
  (flat_aux trm [])

let get_fun_args t =
  match (flatten_app t) with
    [] -> failwith "Term.get_fun_args"
  | f::args -> (f,args)

let get_fun t = 
  let (f, _) = get_fun_args t
  in f

let get_args t = 
  let (_, args) = get_fun_args t
  in args
    

(* Substitutions for terms *)

type subst_alt= Rename | No_rename | Unknown

type subst_terms = ST of term * (subst_alt ref)

let set_subst_alt (ST(_, x)) a = x:=a
let deST (ST(t, a)) = (t, a)
let st_term (ST(t, _)) = t
let st_choice (ST(_, a)) =  !a
let sterm t a= ST(t, ref a)

(* 
   Hashtables with a term as the key
*)

module type TERMHASHKEYS=
  sig 
    type t = term
    val equal : t -> t -> bool
    val hash: t -> int
  end

module Termhashkeys:TERMHASHKEYS=
  struct
    type t = term
    let equal = equals
    let hash= Hashtbl.hash
  end
module type TERMHASH = (Hashtbl.S with type key = (term))
module Termhash:TERMHASH= Hashtbl.Make(Termhashkeys)

type ('a)table = ('a) Termhash.t
let empty_table() = Termhash.create 5
let table_find x env = Termhash.find env x
let table_remove t env = Termhash.remove env t
let table_add t r env = Termhash.add env t r
let table_rebind t r env 
    = (Termhash.remove env t; Termhash.add env t r)

let table_member x env = 
  try ignore(table_find x env); true
  with Not_found -> false


(* Substitution *)
(* Using balanced trees *)

module TermTreeData=
  struct
    type key=term
    let equals=equals
  end

module TermTree=Treekit.BTree(TermTreeData)

(* USING TREES *)

type substitution = (subst_terms)TermTree.t

let empty_subst() = TermTree.nil
let subst_size i = TermTree.nil
let basic_find x env = TermTree.find env x
let basic_rebind t r env = TermTree.replace env t r

let find x env =
  let st=basic_find x env 
  in 
  st_term st 


let term_of_substterm x = st_term x

let bind t r env = TermTree.replace env t (sterm r Unknown)
let add t r env = bind t r env
let remove t env = TermTree.delete env t
let quiet_remove t env = (try remove t env with _ -> env)

(* [chase varp x env]
   Find the end of the chain of bindings for [x] in [env]
   where [varp] determines what is a variable.
*)

let rec chase varp x env =
  try 
    let t = find x env 
    in if (varp t) then (chase varp t env) else t
  with Not_found -> x

let fullchase varp x env =
  let t1 = chase varp x env 
  in if varp t1 then x else t1

(* Rename terms: 
   renames the variables in a term t which are bound by  
   a binder in t 
   needs substitutions 
*)

exception No_quantifier

let rename_env typenv trmenv trm =
  let copy_binder q tyenv= 
    let qnt, qv, qty = Basic.dest_binding q
    in 
    let nt, nev=copy_type_env tyenv qty
    in 
    (mk_binding qnt qv nt, nev)
  and has_quantifier = ref false
  in 
  let rec rename_aux t tyenv env=
    match t with
      Bound(_) -> 
	(try (find t env, tyenv, env)
	with Not_found -> (t, tyenv, env))
    | Qnt(k, q, b) -> 
      	let nq, tyenv1 = copy_binder q tyenv
      	in 
	let env1=bind (Bound(q)) (Bound(nq)) env
	in 
	let nb, tyenv2, env2=rename_aux b tyenv1 env1
	in 
	has_quantifier:=true;
      	(Qnt(k, nq, nb), tyenv2, env2)
    | Typed(b, ty) -> 
	let nb, tyenv1, env1 = rename_aux b tyenv env
	in 
	(Typed(nb, ty), tyenv1, env1)
    | App(f, a) ->
	let nf, tyenv1, env1=rename_aux f tyenv env
	in let na, tyenv2, env2=rename_aux a tyenv env1
	in 
	(App(nf, na), tyenv2, env2)
    | _ -> (t, tyenv, env)
  in 
  let t, ntyenv, nenv = rename_aux trm typenv trmenv 
  in 
  if (!has_quantifier)=false then raise No_quantifier 
  else t, ntyenv, nenv

(* rename_silent t: silently rename term t *)

let rename_silent env t = 
  try
    rename_env (Gtypes.empty_subst()) env  t
  with No_quantifier -> (t, Gtypes.empty_subst(), env)

(* 
   [rename t]
   rename term [t], raise [No_quantifier] if no change 
   (carry out alpha conversion on [t])
*)

let rename t = 
  try
    let nt, _, _ = rename_env (Gtypes.empty_subst()) (empty_subst()) t
    in nt		
  with No_quantifier -> t

(* get the replacement of a term t from substitution env *)
(* renaming the replacement as neccessary *)

let do_rename env x nb =
  match st_choice nb with
    Rename -> rename (st_term nb)
  | No_rename -> st_term nb
  | Unknown -> 
      try 
    	let nt = rename (st_term nb)
	in 
	set_subst_alt nb Rename; nt
      with No_quantifier -> 
	set_subst_alt nb No_rename; (st_term nb)

let replace env x =  do_rename env x (basic_find x env)

(* [subst env trm]
   carry out substitutions of [env] in [trm]
*)
let rec subst env trm =
  try 
    let nt= replace env trm 
    in 
    subst env nt
  with Not_found ->
    (match trm with
      Qnt(k, q, b) ->  Qnt(k, q, subst env b)
    | App(f, a) -> App(subst env f, subst env a)
    | Typed(t, ty) -> Typed(subst env t, ty)
    | _ -> trm)

(* subst_mgu: substitution to construct the MGU formed by unificiation *)

let chase_var varp x env =
  let rec chase_var_aux r =
    (let y = term_of_substterm r
    in 
    if varp y
    then 
      (try
      	chase_var_aux (basic_find y env)
      with Not_found -> r)
    else r)
  in 
  let nb = chase_var_aux (sterm x Unknown)
  in 
  if not (equals x (term_of_substterm nb))
  then do_rename env x nb
  else x

let subst_mgu varp env trm = 
  let rec mgu_aux t=
    try 
      let nt = replace env (chase_var varp t env)
      in mgu_aux nt
    with Not_found ->
      (match trm with
	Qnt(k, q, b) ->  Qnt(k, q, mgu_aux b)
      | App(f, a) -> App(mgu_aux f, mgu_aux a)
      | Typed(t, ty) -> Typed(mgu_aux t, ty)
      | _ -> trm)
  in mgu_aux trm

let get_free_vars trm = 
  let rec get_free_vars_aux t ts =
    match t with 
      Id(x, ty) -> (x, ty)::ts
    | Qnt(k, q, b) -> get_free_vars_aux b ts
    | App(f, a) -> get_free_vars_aux a (get_free_vars_aux f ts)
    | Typed(tr, ty) ->get_free_vars_aux tr ts
    | _ -> ts
  in get_free_vars_aux trm []


let is_bound x = 
  match x with 
    Bound _ -> true
  | _ -> false
let mkbound n = (Bound n)

let dest_bound t = 
  match t with 
    Bound(n) -> n
  | _ -> raise (Failure "Not a binder")

let mkmeta n ty = Bound (mk_binding Meta n ty)
let is_meta trm = 
  match trm with
    Bound (q) ->
      (match (dest_binding q) with
	Meta, _, _ -> true
      | _ -> false)
  | _ -> false
  
let get_binder t = (dest_bound t)

let get_binder_type x =
  match x with
    Bound(n) -> Basic.binder_type n
  | Qnt(_, n, _) -> Basic.binder_type n
  | _ -> raise (Failure "Not a binder")

let is_var x = 
  match x with 
    Id _ -> true
  | _ -> false
let mk_typed_var n t= (Id(n, t))
let mkvar n = mk_typed_var n (Gtypes.mk_null ())
let mkshort_var n = mkvar (mkname n)
let dest_var vt =
  match vt with
    (Id (n, t)) -> (n, t)
  | _ -> raise (Failure "Not a variable")

let get_var_id vt= fst (dest_var vt)
let get_var_type vt= snd (dest_var vt)

let is_bound x = 
  match x with 
    Bound _ -> true
  | _ -> false
let mkbound n = (Bound n)

let dest_bound t = 
  match t with 
    Bound(n) -> n
  | _ -> raise (Failure "Not a binder")

let is_app x = 
  match x with 
    App _ -> true
  | _ -> false

let mkapp f a= (App(f, a))
let dest_app t = 
  match t with 
    (App(f, a)) -> (f, a)
  | _ -> raise (Failure "Not a function")

let rec mkcomb x y = 
  match y with 
    [] -> x
  | t::ts -> mkcomb (mkapp x t) ts

let get_args x =
  let rec get_args_aux t rs =
    match t with 
      App(f, a) -> get_args_aux f (a::rs)
    |	x -> rs
  in get_args_aux x []

let mkfun f args = 
  mkcomb (Id(f, Gtypes.mk_var 
		("_"^(Basic.string_fnid f)^"_ty"))) args
let is_fun t =
  (is_app t) & is_var (get_fun t)

let dest_fun t = 
  if is_fun t 
  then (fst(dest_var (get_fun t)), get_args t)
  else raise (Failure "Not a function")


let is_typed x = 
  match x with 
    Typed (_, _) -> true
  | _ -> false
let mktyped t ty= Typed(t, ty)
let dest_typed t = 
  match t with 
    Typed(trm, ty) -> (trm, ty)
  | _ -> raise (Failure "Not a typed term")

let is_const x = 
  match x with 
    Const _ -> true
  | _ -> false
let mkconst c = (Const c)

let dest_const t =
  match t with
    Const c -> c
  | _ -> raise (Failure "Not a constant")


let mknum n = mkconst(Cnum n)
let destnum n = 
  match (dest_const n) with
    (Cnum c) -> c
  | _ -> raise (Failure "Not a number")
let mk_int n = mkconst(Cnum (Num.num_of_int n))

let mkbool b = mkconst(Cbool b)
let destbool b = 
  match (dest_const b) with 
    (Cbool c) -> c
  | _ -> raise (Failure "Not a boolean")

let is_true t = 
  match t with 
    (Const (Cbool true)) -> true 
  | _ -> false

let dest_qnt t=
  match t with 
    (Qnt(qnt, q,b)) -> 
      let _, qv, qty = Basic.dest_binding q
      in (q, qnt, qv, qty, b)
  | _  -> raise (Failure "Not a quantifier")

let get_qnt_type t =
  match t with 
    Qnt(qnt, qb, b) -> Basic.binder_type qb
  | Bound(qb) -> Basic.binder_type qb
  | _ -> raise (Failure "not a quantifier")

let get_qnt_body t = 
  match t with 
    Qnt(_, _, b) -> b
  | _ -> raise (Failure "Not a quantified formula")


let get_free_binders t =
  let memo = empty_table()
  and qnts = ref []
  and trtrm = mkbool true
  in 
  let rec free_aux x =
    match x with
      Qnt(_, q, b) ->
	table_add (Bound q) (trtrm) memo;
	free_aux b
    | Bound(q) ->
	(try ignore(table_find x memo)
	with Not_found ->
	  table_add x trtrm memo;
	  qnts:=q::!qnts)
    | Typed(tr, _) -> free_aux tr
    | App(f, a) -> free_aux f; free_aux a
    | _ -> ()
  in 
  free_aux t; !qnts

(* instantiate a quantified formula t with term r *)

let subst_quick t r trm = 
  subst (bind t r (empty_subst())) trm

let inst t r =
  if is_qnt t 
  then 
    (let (q, qnt, n, ty, b) = dest_qnt t
    in 
    subst_quick (Bound(q)) r b)
  else raise (Failure "inst: not a quantified formula")

(* [subst_qnt_var]
   specialised form of substitution for constructing quantified terms. 
   replaces only free variables and only if name and type match the
   quantifying term.
*)

let subst_qnt_var scp env trm =
  let rec subst_aux t=
    match t with
      Id(n, ty) -> 
	(try 
	  (let r = Lib.find n env
	  in ignore(Gtypes.unify scp ty (get_binder_type r)); r)
	with _ -> t)
    | (Typed(tt, ty)) -> Typed(subst_aux tt, ty)
    | (App(f, a)) -> App(subst_aux f, subst_aux a)
    | Qnt(k, q, b) -> Qnt(k, q, subst_aux b)
    | _ -> t
  in subst_aux trm

let mktyped_qnt scp q ty n b =
  let t=mk_binding q n ty
  in 
  let nb=subst_qnt_var scp
      (Lib.bind (mkname n) (Bound t) (Lib.empty_env())) b
  in Qnt(q, t, nb)

let mkqnt tyenv q n b =
  mktyped_qnt tyenv q (Gtypes.mk_null()) n b

let binder_equiv tyenv s t = 
  match (s, t) with
    (Bound(b1), Bound(b2)) ->
      let (qnt1, _, ty1) =dest_binding b1
      and (qnt2, _, ty2) =dest_binding b2
      in 
      if (qnt1 = qnt2) 
      then (ignore(Gtypes.unify tyenv ty1 ty2); true)
      else false
  | (Qnt(qnt1, b1, _), Qnt(qnt2, b2, _)) ->
      let (_, _, ty1) =dest_binding b1
      and (_, _, ty2) =dest_binding b2
      in 
      if (qnt1 = qnt2) 
      then (ignore(Gtypes.unify tyenv ty1 ty2); true)
      else false
  | _ -> false


(* Debugging and printing *)

let string_typed_name n t = 
  ("("^n^": "^(Gtypes.string_gtype t)^")")

let rec string_term_basic t =
  match t with
    Id(n, ty) -> (Basic.string_fnid n) (*string_typed_name n ty*)
  | Bound(_) -> "?"^(get_binder_name t)
  | Const(c) -> string_const c
  | App(t1, t2) ->
      let f=get_fun t 
      and args = get_args t 
      in 
      ("("^(string_term_basic f)^" ("
       ^(Lib.list_string (string_term_basic ) ", " args)^"))")

  | Qnt(k, q, body) -> 
      (quant_string k)
      ^" ("^(string_typed_name (Basic.binder_name q) 
	       (Basic.binder_type q))^"): "
      ^(string_term_basic body)
  | Typed (trm, ty) -> 
      ("("^(string_term_basic trm)^": "
       ^(Gtypes.string_gtype ty)^")")

let rec string_term_prec i x =
  match x with
    Id(n, ty) -> (Basic.string_fnid n)
  | Bound(_) -> "?"^(get_binder_name x)
  | Const(c) -> Basic.string_const c
  | Qnt(k, q, body) -> 
      let ti = (Basic.prec_qnt k)
      in 
      if ti <= i 
      then ("("^(quant_string (Basic.binder_kind q))
	    ^" "^(string_typed_name (Basic.binder_name q) 
		    (Basic.binder_type q))^". "
	    ^(string_term_prec ti (body))^")")
      else 
	((quant_string (Basic.binder_kind q))
	 ^" "^(string_typed_name (Basic.binder_name q) 
		 (Basic.binder_type q))^". "
	 ^(string_term_prec ti (body)))
  | App(t1, t2) ->
      let f=get_fun x 
      and args = get_args x
      in 
      ("("^(string_term_prec i f)^" "
       ^(list_string (string_term_prec i) " " args)^")")
  | Typed (trm, ty) -> 
      ("("^(string_term_prec i trm)^": "
       ^(Gtypes.string_gtype ty)^")")


let string_term x = string_term_prec 0 x

let string_infix f ls =
  match ls with
    [l;r] -> (l^f^r)
  | _ -> raise (Failure ("Too many arguments to infix "^f^"."))

let cfun_string c =
  match c with 
    "not" -> "not "
  | "and" -> " and "
  | "or" -> " or "
  | "implies" -> " => "
  | "iff" -> "<=>"
  | "equals" -> "="
  | x -> x

let rec string_term_inf inf i x =
  match x with
    Id(n, ty) -> (cfun_string (string_fnid n))
  | Bound(_) -> (get_binder_name x)
  | Const(c) -> Basic.string_const c
  | Typed (trm, ty) ->
      ("("^(string_term_inf inf i trm)^": "
       ^(Gtypes.string_gtype ty)^")")
  | App(t1, t2) ->
      let f=get_fun x 
      and args = get_args x
      in 
      if is_var f 
      then 
	(let name= fst (dest_var f)
	in 
	let pr = (try (fst(inf) name) with _ -> -1)
	in let ti = if pr <=i then pr else i
	in if (try (snd(inf)) name with _ -> false)
	then 
	  (string_infix (cfun_string (string_fnid name))
	     (List.map (string_term_inf inf ti) args))
	else 
	  ("("^(string_term_inf inf i f)^" "
	   ^(list_string (string_term_inf inf i) " " args)^")"))
      else 
	("("^(string_term_inf inf i f)^" "
	 ^(list_string (string_term_inf inf i) " " args)^")")
  | Qnt(qnt, q, body) -> 
      let (qnts, b) = (strip_qnt qnt x)
      in let qnts_str = 
	(quant_string qnt)
	^(list_string 
	    (fun x -> (string_typed_name (Basic.binder_name x) 
			 (Basic.binder_type x)))
	    " " qnts)^": "
      in 
      let ti = (Basic.prec_qnt (Basic.binder_kind q))
      in 
      if ti < i 
      then ("("^qnts_str^(string_term_inf inf ti (b))^")")
      else
	(qnts_str^(string_term_inf inf ti (b)))

let string_inf_term inf x = string_term_inf inf 0 x


let retype tyenv t=
  let qenv=empty_table()
  in 
  let rec retype_aux t =
    match t with
      Id(n, ty) -> Id(n, Gtypes.mgu ty tyenv)
    | Bound(q) -> 
	(try table_find t qenv
	with Not_found -> t)
    | Const(c) -> t
    | Typed(trm, ty) -> retype_aux trm
    | App(f, a) -> 
	App(retype_aux f, retype_aux a)
    | Qnt(qnt, q, b) ->
	(let (_, oqnt, oqnm, oqty, _) = dest_qnt t
	in 
	let nty = Gtypes.mgu oqty tyenv
	in 
	let nq = mk_binding oqnt oqnm nty
	in 
	table_add (Bound(q)) (Bound(nq)) qenv;
	let rt= Qnt(qnt, nq, retype_aux b)
	in 
	table_remove (Bound(q)) qenv; rt)
  in 
  retype_aux t

(* retype_pretty: 
   as for retype, make substitution for type variables
   but also replace other type variables with new, prettier names 
 *)

let retype_pretty_env typenv trm=
  let inf=ref 0
  in 
  let qenv=empty_table()
  in 
  let rec retype_aux t name_env =
    match t with
      Id(n, ty) -> 
	let nt, nenv1=Gtypes.mgu_rename_env inf typenv name_env ty
	in 
	(Id(n, nt), nenv1)
    | Bound(q) -> 
	(let ntrm, nenv=
	  try (table_find t qenv, name_env)
	  with Not_found ->
	    let qnt, qnm, qty = Basic.dest_binding q
	    in 
	    let nty, nenv1=
	      Gtypes.mgu_rename_env inf typenv name_env qty
	    in 
	    let nq=mk_binding qnt qnm nty
	    in 
	    table_add (Bound q) (Bound nq) qenv;
	    (Bound nq, nenv1)
	in 
	(ntrm, nenv))
    | Const(c) -> (t, name_env)
    | Typed(trm, ty) -> retype_aux trm name_env
    | App(f, a) -> 
	let nf, nenv1=retype_aux f name_env
	in let na, nenv2=retype_aux a nenv1
	in 
	(App(nf, na), nenv2)
    | Qnt(qnt, q, b) ->
	(let (_, oqnt, oqnm, oqty, _) = dest_qnt t
	in 
	let nty, nenv1 =Gtypes.mgu_rename_env inf typenv name_env oqty
	in 
	let nq = mk_binding qnt oqnm nty
	in 
	table_add (Bound(q)) (Bound(nq)) qenv;
	let nb, nenv2=retype_aux b nenv1
	in let rt= Qnt(qnt, nq, nb)
	in 
	table_remove (Bound(q)) qenv; (rt, nenv2))
  in 
  retype_aux trm (Gtypes.empty_subst())


let retype_pretty typenv trm = 
  let nt, _ = retype_pretty_env typenv trm
  in nt

(*
let print prenv x = 
  Format.open_box 2;
  print_string 
    (string_inf_term  prenv x);
  Format.close_box()
*)

let rec print_termlist prenv x =
  match x with 
    [] -> print_string ""
  | [p] -> print prenv p
  | (p::ps) -> (print prenv p; print_string ", "; print_termlist prenv ps)

(* pretty printing *)


let pplookup ppstate id =
  try
    (Printer.get_record ppstate.Printer.terms id)
  with Not_found -> 
    Printer.mk_record 
      Printer.default_term_prec
      Printer.default_term_fixity
      None

let print_meta qnt =
  let _, qv, qty = dest_binding qnt 
  in 
  Format.open_box 0;
  Format.print_string "(";
  Format.print_string ("( _"^qv^": ");
  Format.print_string (Gtypes.string_gtype qty);
  Format.print_string ")";
  Format.close_box()

let print_typed_name ppstate (n, ty)=
  Format.open_box 2;
  Printer.print_string "(";
  Printer.print_string n;
  Format.print_cut();
  Printer.print_string ": ";
  Gtypes.print (ppstate) ty;
  Printer.print_string ")";
  Format.close_box()


let print_fn_app (fnpr, argpr) ppstate prec (f,args)=
  let printer =
    try
      Printer.get_printer (ppstate.Printer.terms) f
    with Not_found -> 
      Printer.print_operator
	(fnpr, 
	 (fun pr l -> 
	   Printer.print_list
	     (argpr pr, Printer.print_space) l),
	 (pplookup ppstate))
  in 
  printer prec (f, args)

let print_typed_term tpr ppstate prec (trm, ty)=
  Format.open_box 2;
  Printer.print_string "(";
  tpr ppstate prec trm;
  Printer.print_string ")";
  Format.print_cut();
  Printer.print_string ": ";
  Gtypes.print_type ppstate 0 ty;
  Format.close_box()

let print_qnt ppstate q =
  let _, qvar, qtyp = dest_binding q 
  in 
  print_typed_name ppstate (qvar, qtyp)

let print_qnts ppstate prec (qnt, qs) =
  Printer.print_string (Basic.quant_string qnt);
  Printer.print_list
    (print_qnt ppstate, 
     Printer.print_space) 
    qs;
  Printer.print_string ":";
  Format.print_cut()

let rec print_term ppstate prec x =
  match x with
    Id(n, ty) -> 
      Printer.print_identifier 
	(pplookup ppstate) n;
      Format.print_cut()
  | Bound(n) -> 
      Format.print_string ((get_binder_name x));
      Format.print_cut()
  | Const(c) -> 
      Format.print_string (Basic.string_const c);
      Format.print_cut()
  | Typed (trm, ty) -> 
      print_typed_term print_term ppstate prec (trm, ty);
      Format.print_cut()
  | App(t1, t2) ->
      let f, args=get_fun_args x 
      in 
      (match args with 
	[] -> print_term ppstate prec f
      | _ -> 
	  (if is_var f 
	  then 
	    (let n, ty=dest_var f
	    in 
	    print_fn_app 
	      ((fun _ -> 
		Printer.print_identifier 
		  (pplookup ppstate)),
	       print_term ppstate)
	      ppstate prec (n, args))
	  else 
	    (Format.open_box 2;
	     Printer.print_string "(";
	     Printer.print_list
	       (print_term ppstate prec, Printer.print_space)
	       (f::args);
	     Printer.print_string ")";
	     Format.close_box())));
      Format.print_cut()
  | Qnt(qnt, q, body) -> 
      let (_, _, qvar, qtyp, _) = dest_qnt x
      in 
      let (qnts, b) = (strip_qnt qnt x)
      in 
      let ti = (Basic.prec_qnt (qnt))
      in 
      Format.open_box 3;
      Printer.print_bracket prec ti "(";
      print_qnts ppstate prec (qnt, qnts); 
      Printer.print_space ();
      print_term ppstate ti b;
      Printer.print_bracket prec ti ")";
      Format.close_box();
      Format.print_cut()

let print ppstate x = 
  Format.open_box 0;
  print_term ppstate 0 (retype_pretty (Gtypes.empty_subst()) x);
  Format.close_box()

(* Error handling *)

class termError s ts =
  object (self)
    inherit Result.error s
    val trms = (ts :term list)
    method get() = trms
    method print st = 
      Format.open_box 0; 
      print_string ((self#msg())^" "); 
      Format.print_space();
      Format.open_box 0; 
      Printer.print_sep_list 
	(print st, ",") (self#get());
      Format.close_box();
      Format.print_newline();
      Format.close_box();
  end
let mktermError s t = ((new termError s t):>error)

let termError s t = mk_error((new termError s t):>error)
let addtermError s t es = raise (add_error (termError s t) es)

(* [set_names_types scp thy trm]
   find and set long identifiers and types for variables in [trm]
   theory is [thy] if no long identifier can be found in scope [scp]
 *)

let set_names scp trm=
  let term_memo = Lib.empty_env()
  in 
  let lookup_id n = 
    (try (Lib.find n term_memo)
    with Not_found -> 
      let nth = 
	try 
	  (scp.thy_of Basic.fn_id n) 
	with _ -> Basic.null_thy      (* scp.curr_thy *)
      in ignore(Lib.add n nth term_memo); nth)
  in 
  let rec set_aux t=
    match t with
      Id(id, ty) -> 
	let th, n = Basic.dest_fnid id
	in 
	let nth = 
	  (if th=Basic.null_thy
	  then lookup_id n 
	  else th)
	in 
	let nid = Basic.mklong nth n
	in Id(nid, ty)
    | Qnt(qnt, q, b) -> Qnt(qnt, q, set_aux b)
    | Typed(tt, tty) -> Typed(set_aux tt, tty)
    | App(f, a) -> App(set_aux f, set_aux a)
    | _ -> t
  in set_aux trm

let in_thy_scope memo scp th trm =
  let lookup_id n = 
    (try (Lib.find n memo)
    with Not_found -> 
      if (scp.thy_in_scope th n)
      then Lib.add n true memo else raise Not_found)
  in
  let rec in_scp_aux t =
    match t with
      Id(id, ty) -> 
	ignore(lookup_id (thy_of_id id));
	Gtypes.in_thy_scope memo scp th ty
    | Qnt(_, _, b) ->
	ignore(Gtypes.in_thy_scope memo scp th (get_qnt_type t));
	in_scp_aux b
    | Bound(_) ->
	Gtypes.in_thy_scope memo scp th (get_qnt_type t)
    | Typed(tr, ty) ->
	ignore(in_scp_aux tr);
	Gtypes.in_thy_scope memo scp th ty
    | App(a, b) ->
	ignore(in_scp_aux a);
	in_scp_aux b
    | _ -> true
  in 
  try ignore(in_scp_aux trm); true
  with Not_found -> false

let compare_term t1 t2 = compare t1 t2

let less_than (t1:term) (t2:term) = (compare t1 t2)<0

let least ts =
  let rec less_aux l xs =
    match xs with
      [] -> l
    | (y::ys) -> 
	if less_than y l then less_aux y ys
	else less_aux l ys
  in 
  match ts with
    [] -> raise (termError "least: No arguments" [])
  | (x::xs) -> less_aux x xs

(* 
   more complex ordering on terms:
   Const < Id <  Bound < App < Qnt
   (Typed t1) < t2 iff t1<t2
 *)

let rec term_lt t1 t2 = 
  let atom_lt (a1, ty1) (a2, ty2) =  a1<a2
  and 
      bound_lt (q1, n1, _) (q2, n2, _) =  n1<n2 & q1<q1
  in 
  match (t1, t2) with
    Typed (trm, _), _ -> term_lt trm t2
  | _, Typed (trm, _) -> term_lt t1 trm
  | (Const c1, Const c2) -> Basic.const_lt c1 c2
  | (Const _ , _ ) -> true
  | (Id _, Const _) -> false
  | (Id _, Id _) -> atom_lt (dest_var t1) (dest_var t2)
  | (Id _, _) -> true
  | (Bound _, Const _) -> false
  | (Bound _, Id _) -> false
  | (Bound b1, Bound b2) -> bound_lt (dest_binding b1) (dest_binding b2)
  | (Bound _ , _ ) -> true
  | (App _, Const _) -> false
  | (App _, Bound _) -> false
  | (App _, Id _) -> false
  | (App(f1, a1), App (f2, a2)) -> 
      if term_lt f1 f2 then true
      else if term_lt f2 f1 then false
      else term_lt a1 a2
  | (App _, _) -> true
  | (Qnt _, Const _) -> false
  | (Qnt _, Bound _) -> false
  | (Qnt _, Id _) -> false
  | (Qnt _, App _) -> false
  | (Qnt(qnt1, q1, b1), Qnt(qnt2, q2, b2)) ->
      if(term_lt b1 b2) then true
      else bound_lt (dest_binding q1) (dest_binding q2)


let rec term_leq t1 t2 = 
  let atom_leq (a1, ty1) (a2, ty2) =  a1<=a2 
  and 
      bound_leq (q1, n1, ty1) (q2, n2, ty2) =  n1<=n2 & q1<=q2
  in 
  match (t1, t2) with
    Typed (trm, _), _ -> term_leq trm t2
  | _, Typed (trm, _) -> term_leq t1 trm
  | (Const c1, Const c2) -> Basic.const_leq c1 c2
  | (Const _ , _ ) -> true
  | (Id _, Const _) -> false
  | (Id _, Id _) -> atom_leq (dest_var t1) (dest_var t2)
  | (Id _, _) -> true
  | (Bound _, Const _) -> false
  | (Bound _, Id _) -> false
  | (Bound b1, Bound b2) -> bound_leq (dest_binding b1) (dest_binding b2)
  | (Bound _ , _ ) -> true
  | (App _, Const _) -> false
  | (App _, Id _) -> false
  | (App _, Bound _) -> false
  | (App(f1, a1), App (f2, a2)) -> 
      if term_leq f1 f2 then term_leq a1 a2
      else false
  | (App _, _) -> true
  | (Qnt _, Const _) -> false
  | (Qnt _, Bound _) -> false
  | (Qnt _, Id _) -> false
  | (Qnt _, App _) -> false
  | (Qnt(qnt1, q1, b1), Qnt(qnt2, q2, b2)) ->
      if(term_leq b1 b2) then true
      else bound_leq (dest_binding q1) (dest_binding q2)

let term_gt t1 t2 = not (term_leq t2 t1)
