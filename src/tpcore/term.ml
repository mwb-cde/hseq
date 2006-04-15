(*-----
   Name: term.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

open Lib
open Basic
open Gtypes
open Result

(***
* Equality
***)
let rec equals x y = 
  (x == y) || 
  (match (x, y) with
    (App(f1, arg1), App(f2, arg2))->
      (equals f1 f2) & (equals arg1 arg2)
  | (Bound(q1), Bound(q2)) -> q1==q2
  | (Meta(q1), Meta(q2)) -> q1==q2
  | (Qnt(qn1, b1), Qnt(qn2, b2)) -> 
      (qn1==qn2) && (equals b1 b2)
  | (Typed(t1, ty1), Typed(t2, ty2)) ->
      (Gtypes.equals ty1 ty2)  & (equals t1 t2)
  | (_, _) -> x=y)


(**
   [binder_equiv tyenv s t]: Equivalence of binders of quantified or
   bound terms [s] and [t], w.r.t type substitution [tyenv].
*)
 let binder_equiv tyenv s t =
  match (s, t) with
    (Bound(b1), Bound(b2)) ->
      let (qnt1, _, ty1) =dest_binding b1
      and (qnt2, _, ty2) =dest_binding b2
      in 
      if (qnt1 = qnt2) 
      then (ignore(Gtypes.unify tyenv ty1 ty2); true)
      else false
  | (Meta(b1), Meta(b2)) ->
      let (qnt1, _, ty1) =dest_binding b1
      and (qnt2, _, ty2) =dest_binding b2
      in 
      if (qnt1 = qnt2) 
      then (ignore(Gtypes.unify tyenv ty1 ty2); true)
      else false
  | (Qnt(b1, _), Qnt(b2, _)) ->
      let (qnt1, _, ty1) =dest_binding b1
      and (qnt2, _, ty2) =dest_binding b2
      in 
      if (qnt1 = qnt2) 
      then (ignore(Gtypes.unify tyenv ty1 ty2); true)
      else false
  | _ -> false

(* [('a)tree]: Balanced trees indexed by terms *)
module TermTreeData=
  struct
    type key=term
    let equals=equals
    let lessthan x y= (Pervasives.compare x y) < 0
  end
module TermTree=Treekit.BTree(TermTreeData)
type ('a)tree = ('a) TermTree.t

(* [('a)table]: Hashtables indexed by terms *)
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

(* Recognisers *)

let is_qnt x = 
  match x with 
    Qnt _ -> true
  | _ -> false

let is_app x = 
  match x with 
    App _ -> true
  | _ -> false

let is_bound x = 
  match x with 
    Bound _ -> true
  | _ -> false

let is_free x = 
  match x with 
    Free _ -> true
  | _ -> false

let is_ident x = 
  match x with 
    Id _ -> true
  | _ -> false

let is_typed x = 
  match x with 
    Typed (_, _) -> true
  | _ -> false

let is_const x = 
  match x with 
    Const _ -> true
  | _ -> false

let is_true t = 
  match t with 
    (Const (Cbool true)) -> true 
  | _ -> false

(* Constructors *)

let mk_qnt b t= Qnt(b, t)
let mk_bound n = (Bound n)
let mk_free n ty= Free(n, ty)
let mk_app f a= (App(f, a))
let mk_typed t ty= Typed(t, ty)
let mk_const c = (Const c)

let mk_typed_ident n t= (Id(n, t))
let mk_ident n = mk_typed_ident n (Gtypes.mk_null ())
let mk_short_ident n = mk_ident (Ident.mk_name n)


(* Destructors *)

let dest_qnt t=
  match t with 
    (Qnt(q, b)) -> (q, b)
  | _  -> raise (Failure "Not a quantifier")

let dest_bound t = 
  match t with 
    Bound(n) -> n
  | _ -> raise (Failure "dest_bound: Not a binder")

let dest_free t = 
  match t with 
    Free(n, ty) -> (n, ty)
  | _ -> raise (Failure "Not a free variable")

let dest_ident vt =
  match vt with
    (Id (n, t)) -> (n, t)
  | _ -> raise (Failure "Not a variable")

let dest_app t = 
  match t with 
    (App(f, a)) -> (f, a)
  | _ -> raise (Failure "Not a function")

let dest_typed t = 
  match t with 
    Typed(trm, ty) -> (trm, ty)
  | _ -> raise (Failure "Not a typed term")

let dest_const t =
  match t with
    Const c -> c
  | _ -> raise (Failure "Not a constant")


(* Specialised Manipulators *)

(* Meta variables *)

let is_meta trm = 
  match trm with
    Meta _ -> true
  | _ -> false

let mk_meta n ty = Meta (mk_binding Gamma n ty)

let dest_meta t  = 
  match t with
      Meta(q) -> q
    | _ -> raise (Failure "Not a meta variable")


(** Typed terms *)

let rec strip_typed term =
  match term with 
    Basic.Typed(tt, _) -> strip_typed tt
  | _ -> term

(** Constants *)

let destnum n = 
  match (dest_const n) with
    (Cnum c) -> c
  | _ -> raise (Failure "Not a number")

let destbool b = 
  match (dest_const b) with 
    (Cbool c) -> c
  | _ -> raise (Failure "Not a boolean")

let mk_num n = mk_const(Cnum n)
let mk_int n = mk_const(Cnum (Num.num_of_int n))

let mk_bool b = mk_const(Cbool b)

(***
* Function application
***)

(**
   [mk_comb x y]: Make a function application from [x] and [y].
   [mk_comb f [a1;a2;...;an]] is [((((f a1) a2) ...) an)]
*)
let rec mk_comb x y = 
  match y with 
    [] -> x
  | t::ts -> mk_comb (mk_app x t) ts

let mk_fun f args = 
  mk_comb (Id(f, Gtypes.mk_var 
		("_"^(Ident.string_of f)^"_ty"))) args

(**
   [flatten_app trm]: flatten an application in [trm] to a list of
   terms.  [flatten_app (((f a1) a2) a3)] is [[f; a1; a2; a3]] and
   [flatten_app (((f a1) (g a2)) a3)] is [[f; a1; (g a2); a3]]
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

let is_fun t =
  (is_app t) & is_ident (get_fun t)

let rator t =
  match t with
      App(f, _) -> f
    | _ -> raise (Failure "rator: Not an application")

let rand t =
  match t with
      App(_, a) -> a
    | _ -> raise (Failure "rand: Not an application")

let dest_fun t = 
  try 
    let (f, args) = get_fun_args t
    in
    (fst(dest_ident f), args)
  with _ -> raise (Failure "Not a function")

let dest_unop t =
  match (dest_fun t) with
    (id, x::_) -> (id, x)
  | _ -> raise (Failure "not a unary operator")

let dest_binop t =
  match (dest_fun t) with
    (id, x::y::_) -> (id, x, y)
  | _ -> raise (Failure "not a binary operator")

let rec strip_fun_qnt f term qs = 
  let is_ident t = 
    if(is_ident t)
    then 
      let n, _ = dest_ident t
      in f = n
    else false
  in 
  let is_lambda b = 
    (let (q, _, _) = Basic.dest_binding b
    in 
    (q=Basic.Lambda))
  in 
  match term with 
    Basic.App(l, Basic.Qnt(q, body)) -> 
      let l0=strip_typed l
      in 
      if not((is_ident l0) && (is_lambda q))
      then (qs, term)
      else 
	strip_fun_qnt f body (q::qs)
  | Basic.Typed(t, _) -> strip_fun_qnt f t qs
  | _ -> (List.rev qs, term)

(***
* Identifier (Id) terms
***)

let get_ident_id vt= fst (dest_ident vt)
let get_ident_type vt= snd (dest_ident vt)

(***
* Free variables
***)

let get_free_name t = 
  match t with 
    Free(n, ty) -> n
  | _ -> raise (Failure "Not a free variable")

let get_free_vars trm = 
  let rec get_free_vars_aux t ts =
    match t with 
      Free(x, ty) -> t::ts
    | Qnt(q, b) -> get_free_vars_aux b ts
    | App(f, a) -> get_free_vars_aux a (get_free_vars_aux f ts)
    | Typed(tr, ty) ->get_free_vars_aux tr ts
    | _ -> ts
  in get_free_vars_aux trm []

(***
* Quantified and bound terms
***)

let get_binder t =
  match t with
    Bound(b) -> b
  | Qnt(b, _) -> b
  | Meta(b) -> b
  | _ -> raise (Failure "get_binder: No binder in term")

let get_binder_name x =
  match x with
    Bound(n) -> binder_name n
  | Qnt(n, _) -> binder_name n
  | Meta(n) -> binder_name n
  | _ -> raise (Failure "get_binder_name: Not a binder")

let get_binder_type x =
  match x with
    Bound(n) -> Basic.binder_type n
  | Qnt(n, _) -> Basic.binder_type n
  | Meta(n) -> Basic.binder_type n
  | _ -> raise (Failure "get_binder_type: Not a binder")

let get_binder_kind x =
  match x with
    Bound(n) -> Basic.binder_kind n
  | Qnt(n, _) -> Basic.binder_kind n
  | Meta(n) -> Basic.binder_kind n
  | _ -> raise (Failure "get_binder_kind: Not a binder")

let get_qnt_body t = 
  match t with 
    Qnt(_, b) -> b
  | _ -> raise (Failure "Not a quantified formula")

let get_free_binders t =
  let memo = empty_table()
  and trtrm = mk_free "" (Gtypes.mk_null())
  in 
  let rec free_aux qnts x =
    match x with
      Qnt(q, b) ->
	table_add (Bound q) (trtrm) memo;
	free_aux qnts b
    | Bound(q) ->
	(try (ignore(table_find x memo); qnts)
	with Not_found ->
	  ignore(table_add x trtrm memo);
	  (q::qnts))
    | Typed(tr, _) -> free_aux qnts tr
    | App(f, a) -> 
	let fqnts = free_aux qnts f
	in free_aux fqnts a
    | _ -> qnts
  in 
  free_aux [] t

let strip_qnt q trm =
  let rec strip_aux t qs=
    if is_qnt t 
    then
      (let (bind, b) = dest_qnt t
      in 
      (if (Basic.binder_kind bind)=q 
      then (strip_aux b (bind::qs))
      else (qs, t)))
    else (qs, t)
  in 
  let qnts, nt= strip_aux trm []
  in (List.rev qnts, nt)

let rec rebuild_qnt qs b=
  match qs with
    [] -> b
  | (x::xs) -> Basic.Qnt(x, rebuild_qnt xs b)


(***
* Simple pretty printing and error handling 
***)

let print_simple trm=
  let rec print_aux t =
    match t with
      Id(n, ty) -> 
	let (th, x) = Ident.dest n
	in 
	Format.printf "@[%s@]" (th^"."^x)
    | Bound(n) -> 
	Format.printf "@[%s@]" (".."^(binder_name n))
    | Free(n, ty) -> Format.printf "@[%s@]"  n
    | Meta(n) -> 
	Format.printf "@[%s@]" (binder_name n)
    | Const(c) -> Format.printf "@[%s@]" (Basic.string_const c)
    | Typed (trm, ty) ->
	Format.printf "@[<2>%s" "(";
	print_aux trm;
	Format.printf "@ %s %s)@]" ":" (Gtypes.string_gtype ty)
    | App(t1, t2) ->
	Format.printf "@[<2>(";
	print_aux t1;
	Format.printf "@ ";
	print_aux t2;
	Format.printf ")@]"
    | Qnt(q, body) ->
	Format.printf "@[<2>%s"  (Basic.quant_string (get_binder_kind t));
	Format.printf "(%s:@ %s) :@ "  
	  (binder_name q)
	  (Gtypes.string_gtype (binder_type q));
	print_aux body;
	Format.printf "@]"
  in 
  print_aux trm

class basictermError s ts =
  object (self)
    inherit Result.error s
    val trms = (ts :term list)
    method get() = trms
    method print st = 
      Format.printf "@[%s@ " (self#msg()); 
      Printer.print_sep_list 
	(print_simple, ",")
	(self#get());
      Format.printf "@]"
  end
let basic_error s t = mk_error((new basictermError s t):>error)

(***
* Substitution in terms 
***)

(**
   [type subst_terms]: Terms stored in a substitution.
*)
type subst_alt= Rename | No_rename | Unknown
type subst_terms = ST of term * (subst_alt ref)

let set_subst_alt (ST(_, x)) a = x:=a
let st_term (ST(t, _)) = t
let st_choice (ST(_, a)) = !a
let sterm t a= ST(t, ref a)

(*
   [type substitution]: the data structure holding the substitution to
   be made in a term.
 *)
type substitution = (subst_terms)tree

let empty_subst() = TermTree.nil
let basic_find x env = TermTree.find env x
let basic_rebind t r env = TermTree.replace env t r

let find x env = st_term (basic_find x env)
let bind t r env = TermTree.replace env t (sterm r Unknown)
let remove t env = TermTree.delete env t
let member t env = 
  try ignore(find t env); true
  with Not_found -> false
      

(**
   [rename t], [rename_env tyenv trmenv t]: 
   Rename terms. 

   Renames the variables in a term [t] which are bound by a binder in [t]
   needs substitutions
*)

exception No_quantifier

let rename_env typenv trmenv trm =
  let copy_binder q tyenv= 
    let qnt, qv, qty = Basic.dest_binding q
    in 
    let nt, nev=rename_type_vars_env tyenv qty
    in 
    (mk_binding qnt qv nt, nev)
  and has_quantifier = ref false
  in 
  let rec rename_aux t tyenv env=
    match t with
      Bound(_) -> 
	(try (find t env, tyenv, env)
	with Not_found -> (t, tyenv, env))
    | Qnt(q, b) -> 
      	let nq, tyenv1 = copy_binder q tyenv
      	in 
	let env1=bind (Bound(q)) (Bound(nq)) env
	in 
	let nb, tyenv2, env2=rename_aux b tyenv1 env1
	in 
	has_quantifier:=true;
      	(Qnt(nq, nb), tyenv2, env2)
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

(* rename_silent t: silently rename term t *)

let rename_silent env t = 
  try
    rename_env (Gtypes.empty_subst()) env  t
  with No_quantifier -> (t, Gtypes.empty_subst(), env)

(*
   [do_rename t]: Get the term of subst_term [t], renaming if necessary.
*)
let do_rename nb =
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

let replace env x =  do_rename (basic_find x env)

(*
* Substitution functions
*) 
let rec subst env trm =
  try 
    let nt= replace env trm 
    in 
    subst env nt
  with Not_found ->
    (match trm with
      Qnt(q, b) ->  Qnt(q, subst env b)
    | App(f, a) -> App(subst env f, subst env a)
    | Typed(t, ty) -> Typed(subst env t, ty)
    | _ -> trm)

let qsubst ts trm =
  let env = 
    List.fold_left (fun e (t, r) -> bind t r e) (empty_subst()) ts
  in 
    subst env trm

(***
* Chase functions
***)
let rec chase varp x env =
  try 
    let t = find x env 
    in if (varp t) then (chase varp t env) else t
  with Not_found -> x

let fullchase varp x env =
  let t1 = chase varp x env 
  in if varp t1 then x else t1

let chase_var varp x env =
  let rec chase_var_aux r =
    (let y = st_term r
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
  if not (equals x (st_term nb))
  then do_rename nb 
  else x

let subst_mgu varp env trm = 
  let rec mgu_aux t=
    try 
      let nt = replace env (chase_var varp t env)
      in mgu_aux nt
    with Not_found ->
      (match trm with
	Qnt(q, b) ->  Qnt(q, mgu_aux b)
      | App(f, a) -> App(mgu_aux f, mgu_aux a)
      | Typed(t, ty) -> Typed(mgu_aux t, ty)
      | _ -> trm)
  in mgu_aux trm

(***
* Operations using substitution.
***) 

let inst t r =
  if is_qnt t 
  then 
    (let (q, b) = dest_qnt t
    in 
    qsubst [(Bound(q), r)] b)
  else raise (Failure "inst: not a quantified formula")

(*
   [subst_qnt_var]: Specialised form of substitution for constructing
   quantified terms. replaces only free variables and only if name and
   type match the quantifying term.
*)
let subst_qnt_var scp env trm =
  let rec subst_aux t=
    match t with
      Id(n, ty) -> 
	(try 
	  (let r = Lib.find n env
	  in ignore(Gtypes.unify scp ty (get_binder_type r)); r)
	with _ -> t)
    | Free(n, ty) -> 
	(try 
	  (let r = Lib.find (Ident.mk_name n) env
	  in ignore(Gtypes.unify scp ty (get_binder_type r)); r)
	with _ -> t)
    | (Typed(tt, ty)) -> Typed(subst_aux tt, ty)
    | (App(f, a)) -> App(subst_aux f, subst_aux a)
    | Qnt(q, b) -> Qnt(q, subst_aux b)
    | _ -> t
  in subst_aux trm

let mk_typed_qnt_name scp q ty n b =
  let t=mk_binding q n ty
  in 
  let nb=subst_qnt_var scp
      (Lib.bind (Ident.mk_name n) (Bound t) (Lib.empty_env())) b
  in Qnt(t, nb)

let mk_qnt_name tyenv q n b =
  mk_typed_qnt_name tyenv q (Gtypes.mk_null()) n b

(***
* Debugging and printing 
***)

let string_typed_name n t = 
  ("("^n^": "^(Gtypes.string_gtype t)^")")

let rec string_term_basic t =
  match t with
    Id(n, ty) -> (Ident.string_of n) (*string_typed_name n ty*)
  | Free(n, ty) -> n
  | Bound(_) -> "?"^(get_binder_name t)
  | Meta(q) -> get_binder_name t
  | Const(c) -> string_const c
  | App(t1, t2) ->
      let f=get_fun t 
      and args = get_args t 
      in 
      ("("^(string_term_basic f)^" ("
       ^(Lib.list_string (string_term_basic ) ", " args)^"))")

  | Qnt(q, body) -> 
      (quant_string (get_binder_kind t))
      ^" ("^(string_typed_name (Basic.binder_name q) 
	       (Basic.binder_type q))^"): "
      ^(string_term_basic body)
  | Typed (trm, ty) -> 
      ("("^(string_term_basic trm)^": "
       ^(Gtypes.string_gtype ty)^")")

let rec string_term_prec i x =
  match x with
    Id(n, ty) -> (Ident.string_of n)
  | Free(n, ty) -> n
  | Bound(_) -> "?"^(get_binder_name x)
  | Meta(_) -> (get_binder_name x)
  | Const(c) -> Basic.string_const c
  | Qnt(q, body) -> 
      let qnt = Basic.binder_kind q
      in 
      let ti = (Printer.prec_qnt qnt)
      in 
      if ti <= i 
      then ("("^(quant_string qnt)
	    ^" "^(string_typed_name 
		    (Basic.binder_name q)
		    (Basic.binder_type q))^". "
	    ^(string_term_prec ti (body))^")")
      else 
	((quant_string qnt)
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
    Id(n, ty) -> (cfun_string (Ident.string_of n))
  | Free(n, ty) -> n
  | Bound(_) -> (get_binder_name x)
  | Meta(_) -> (get_binder_name x)
  | Const(c) -> Basic.string_const c
  | Typed (trm, ty) ->
      ("("^(string_term_inf inf i trm)^": "
       ^(Gtypes.string_gtype ty)^")")
  | App(t1, t2) ->
      let f=get_fun x 
      and args = get_args x
      in 
      if is_ident f 
      then 
	(let name= fst (dest_ident f)
	in 
	let pr = (try (fst(inf) name) with _ -> -1)
	in let ti = if pr <=i then pr else i
	in if (try (snd(inf)) name with _ -> false)
	then 
	  (string_infix (cfun_string (Ident.string_of name))
	     (List.map (string_term_inf inf ti) args))
	else 
	  ("("^(string_term_inf inf i f)^" "
	   ^(list_string (string_term_inf inf i) " " args)^")"))
      else 
	("("^(string_term_inf inf i f)^" "
	 ^(list_string (string_term_inf inf i) " " args)^")")
  | Qnt(q, body) -> 
      let qnt = Basic.binder_kind q
      in 
      let (qnts, b) = (strip_qnt qnt x)
      in let qnts_str = 
	(quant_string qnt)
	^(list_string 
	    (fun x -> (string_typed_name (Basic.binder_name x) 
			 (Basic.binder_type x)))
	    " " qnts)^": "
      in 
      let ti = (Printer.prec_qnt qnt)
      in 
      if ti < i 
      then ("("^qnts_str^(string_term_inf inf ti (b))^")")
      else
	(qnts_str^(string_term_inf inf ti (b)))

let string_inf_term inf x = string_term_inf inf 0 x


(**
   Retyping.

   [retype tyenv t]: Retype term [t], substituting type variables
   occuring in [t] with types in type substitution [tyenv].
*)
let retype tyenv t=
  let qenv=empty_table()
  in 
  let rec retype_aux t =
    match t with
      Id(n, ty) -> Id(n, Gtypes.mgu ty tyenv)
    | Free(n, ty) -> Free(n, Gtypes.mgu ty tyenv) 
    | Bound(q) -> 
	(try table_find t qenv
	with Not_found -> t)
    | Meta(q) -> t
    | Const(c) -> t
    | Typed(trm, ty) -> retype_aux trm
    | App(f, a) -> 
	App(retype_aux f, retype_aux a)
    | Qnt(q, b) ->
	(let (oqnt, oqnm, oqty) = Basic.dest_binding q
	in 
 	let nty = Gtypes.mgu oqty tyenv 
	in 
	let nq = mk_binding oqnt oqnm nty
	in 
	table_add (Bound(q)) (Bound(nq)) qenv;
	let rt= Qnt(nq, retype_aux b)
	in 
	table_remove (Bound(q)) qenv; rt)
  in 
  retype_aux t

(**
   [retype_with_check tyenv t]: Reset the types in term [t] using type
   substitution [tyenv].  Substitutes variables with their concrete
   type in [tyenv]. Check that the new types are in scope.

   Retyping collapses terms of the form [Typed(trm, ty)] to [trm].
*)
let retype_with_check scp tyenv t=
  let qenv=empty_table()
  in 
  let memo = Lib.empty_env ()
  in 
  let mk_new_type ty = 
    let nty = Gtypes.mgu ty tyenv
    in 
    if(Gtypes.in_scope memo scp nty)
    then nty
    else 
      raise 
	(Gtypes.type_error "Term.retype_with_check: Invalid type" [nty])
  in
  let rec retype_aux t =
    match t with
      Id(n, ty) -> 
	Id(n, mk_new_type ty)
    | Free(n, ty) -> Free(n, mk_new_type ty) 
    | Bound(q) -> 
	(try table_find t qenv
	with Not_found -> t)
    | Meta(q) -> t
    | Const(c) -> t
    | Typed(trm, ty) -> retype_aux trm
    | App(f, a) -> 
	App(retype_aux f, retype_aux a)
    | Qnt(q, b) ->
	(let (oqnt, oqnm, oqty) = Basic.dest_binding q
	in 
 	let nty = mk_new_type oqty
	in 
	let nq = mk_binding oqnt oqnm nty
	in 
	table_add (Bound(q)) (Bound(nq)) qenv;
	let rt= Qnt(nq, retype_aux b)
	in 
	table_remove (Bound(q)) qenv; rt)
  in 
  retype_aux t

(*
   retype_pretty: 
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
    | Free(n, ty) -> 
	let nt, nenv1=Gtypes.mgu_rename_env inf typenv name_env ty
	in 
	(Free(n, nt), nenv1)
    | Meta(q) -> (t, name_env)
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
    | Qnt(q, b) ->
	(let (oqnt, oqnm, oqty) = Basic.dest_binding q
	in 
	let nty, nenv1 =Gtypes.mgu_rename_env inf typenv name_env oqty
	in 
	let nq = mk_binding oqnt oqnm nty
	in 
	table_add (Bound(q)) (Bound(nq)) qenv;
	let nb, nenv2=retype_aux b nenv1
	in let rt= Qnt(nq, nb)
	in 
	table_remove (Bound(q)) qenv; (rt, nenv2))
  in 
  retype_aux trm (Gtypes.empty_subst())

let retype_pretty typenv trm = 
  let nt, _ = retype_pretty_env typenv trm
  in nt

(***
* Pretty Printing 
***)

let rec print_termlist prenv x =
  match x with 
    [] -> ()
  | [p] -> Format.printf "@[";print prenv p; Format.printf "@]"
  | (p::ps) -> 
      (Format.printf "@[";
       print prenv p; 
       Format.printf ",@ ";
       print_termlist prenv ps;
       Format.printf "@]")

let print_bracket  = Printer.print_assoc_bracket

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
  Format.printf "@[(_%s:@ %s)@]" qv (Gtypes.string_gtype qty)

let print_typed_obj level printer ppstate prec (obj, ty)=
  if(!Settings.print_type_level > level)
  then 
    (Format.printf "@[<2>(";
     printer ppstate prec obj;
     Format.printf ":@ ";
     Gtypes.print (ppstate) ty;
     Format.printf ")@]")
  else
    (Format.printf "@[<2>";
     printer ppstate prec obj;
     Format.printf "@]")

let print_typed_identifier ppstate (id, ty)=
  let printer ppstate _ i= 
    Printer.print_identifier (pplookup ppstate) i
  in 
  print_typed_obj 3 printer ppstate (Printer.nonfix, 0) (id, ty)

let print_typed_name ppstate (id, ty)=
  let printer ppstate _ n = 
    Format.print_string n
  in 
  print_typed_obj 2 printer ppstate (Printer.nonfix, 0) (id, ty)

let print_ident_as_identifier ppstate (f, p) v =
  let (id, ty) = dest_ident v 
  in 
  Printer.print_identifier (pplookup ppstate) id

let print_prefix (opr, tpr) (assoc, prec) (f, args)=
  Format.printf "@[<2>";
  opr (assoc, prec) f;
  Format.printf "@ ";
  Printer.print_list 
    (tpr (assoc, prec), Printer.print_space) args;
  Format.printf "@]"

let print_suffix (opr, tpr) (assoc, prec) (f, args)=
  Format.printf "@[<2>";
  Printer.print_list 
    (tpr (assoc, prec), Printer.print_space) args;
  Format.printf "@ ";
  opr (assoc, prec) f;
  Format.printf "@]"

let rec print_infix (opr, tpr) (assoc, prec) (f, args) = 
  Format.printf "@[<2>";
  (match args with
    (l::r::rest) -> 
      Format.printf "@[<2>";
      tpr (Printer.infixl, prec) l;
      Format.printf "@ ";
      opr (assoc, prec) f;
      Format.printf "@ ";
      tpr (Printer.infixr, prec) r;
      Printer.print_list (tpr (assoc, prec), Printer.print_space) rest;
      Format.printf "@]"
  | (l::rest) -> 
      Format.printf "@[<2>";
      tpr (Printer.infixl, prec) l;
      Format.printf "@ ";
      opr (assoc, prec) f;
      Printer.print_list (tpr (assoc, prec), Printer.print_space) rest;
      Format.printf "@]"
  | [] -> 
      Format.printf "@[<2>";
      opr (assoc, prec) f;
      Format.printf "@]");
  Format.printf "@]"

let print_fn_app ppstate (fnpr, argpr) (assoc, prec) (f, args)=
  let (id, ty) = dest_ident f
  in 
  let pprec = 
    try Printer.get_record ppstate.Printer.terms id
    with Not_found ->
      (Printer.mk_record 
	Printer.fun_app_prec
	Printer.default_term_fixity 
	None)
  in 
  let (nfixity, nprec) = (pprec.Printer.fixity, pprec.Printer.prec)
  in 
  let user_printer=
    try Some (Printer.get_printer (ppstate.Printer.terms) id)
    with Not_found -> None
  and std_printer = 
    if(Printer.is_infix nfixity)
    then print_infix (fnpr, argpr)
    else 
      if(Printer.is_suffix nfixity)
      then print_suffix (fnpr, argpr)
      else 
	print_prefix (fnpr, argpr)
  in 
  Format.printf "@[<2>";
  (match user_printer with 
    None -> 
      print_bracket (assoc, prec) (nfixity, nprec) "(";
      std_printer (nfixity, nprec) (f, args);
      print_bracket (assoc, prec) (nfixity, nprec) ")"
  | Some p -> p (assoc, prec) (f, args));
  Format.printf "@]"

let print_typed_term tpr ppstate (assoc, prec) (trm, ty)=
  Format.printf "@[<2>(";
  tpr ppstate (assoc, prec) trm;
  Format.printf ":@ ";
  Gtypes.print_type ppstate 
    (Printer.default_type_fixity, Printer.default_type_prec) ty;
  Format.printf ")@]"

let print_qnt ppstate q =
  let _, qvar, qtyp = dest_binding q 
  in 
  print_typed_name ppstate (qvar, qtyp)

let print_qnts ppstate prec (qnt, qs) =
  Format.printf "@[%s" qnt;
  Printer.print_list
    (print_qnt ppstate, 
     Printer.print_space) 
    qs;
  Format.printf":@]"

let rec print_term ppstate (assoc, prec) x =
  match x with
    Id(n, ty) -> 
      let user_printer=
	try Some (Printer.get_printer (ppstate.Printer.terms) n)
	with Not_found -> None
      in 
      (match user_printer with
	None -> print_typed_identifier ppstate (n, ty)
      | Some(p) -> p (assoc, prec) (x, []))
  | Free(n, ty) -> 
      print_typed_name ppstate (n, ty)
  | Bound(n) -> 
      Format.printf "@[%s@]" ((get_binder_name x))
  | Meta(n) -> 
      Format.printf "@[%s@]" ((get_binder_name x))
  | Const(c) -> 
      Format.printf "@[%s@]" (Basic.string_const c);
  | Typed (trm, ty) -> 
      Format.printf "@[";
      print_typed_term print_term ppstate (assoc, prec) (trm, ty);
      Format.printf "@]"
  | App(t1, t2) ->
      let f, args=get_fun_args x 
      in 
      if is_ident f 
      then 
	(Format.printf "@[";
	 print_fn_app ppstate
	   (print_ident_as_identifier ppstate,
	    (fun (a, p) t-> print_term ppstate (a, p) t))
	   (assoc, prec) (f, args);
	 Format.printf "@]")
      else 
	(let (tassoc, tprec) = 
	   (Printer.default_term_fixity, Printer.fun_app_prec)
	in 
	Format.printf "@[<hov 2>";
	print_bracket (assoc, prec) (tassoc, tprec) "(";
	print_prefix 
	  (print_term ppstate, print_term ppstate) 
	  (tassoc, tprec) (f, args);
	print_bracket (assoc, prec) (tassoc, tprec) ")";
	Format.printf "@,@]")
  | Qnt(q, body) -> 
      let (qnt, qvar, qtyp) = Basic.dest_binding q
      in 
      let (qnts, b) = (strip_qnt qnt x)
      in 
      let (tassoc, tprec) = 
	(Printer.fixity_qnt qnt, Printer.prec_qnt qnt)
      in 
      Format.printf "@[";
      print_bracket (assoc, prec) (tassoc, tprec) "(";
      Format.printf "@[<hov 3>";
      print_qnts ppstate tprec (Basic.quant_string qnt, qnts); 
      Printer.print_space ();
      print_term ppstate (tassoc, tprec) b;
      Format.printf "@]";
      print_bracket (assoc, prec) (tassoc, tprec) ")";
      Format.printf "@]"

let print ppstate x = 
  Format.open_box 0;
  print_term ppstate 
    (Printer.default_term_fixity, Printer.default_term_prec)
    (retype_pretty (Gtypes.empty_subst()) x);
  Format.close_box()

let simple_print_fn_app ppstate (assoc, prec) (f, args)=
  let (id, _) = dest_ident f
  in 
  let pprec = pplookup ppstate id
  in 
  let (nfixity, nprec) = (pprec.Printer.fixity, pprec.Printer.prec)
  in 
  let iprint (a, pr) = print_ident_as_identifier ppstate (a, pr)
  and tprint (a, pr) = print_term ppstate (a, pr)
  in 
  Format.printf "@[<2>";
  print_bracket (assoc, prec) (nfixity, nprec) "(";
  (if(Printer.is_infix nfixity)
  then print_infix (iprint, tprint) (assoc, prec) (f, args)
  else 
    if(Printer.is_suffix nfixity)
    then print_suffix (iprint, tprint) (assoc, prec) (f, args)
    else 
      print_prefix (iprint, tprint) (assoc, prec) (f, args));
  print_bracket (assoc, prec) (nfixity, nprec) ")"; 
  Format.printf "@]"

(**
   [print_as_binder (sym_assoc, sym_prec) f sym]
   Construct a printer to print function applications
   of the form [f (%x: P)] as [sym x: P].
*)
let print_qnt_body ppstate (assoc, prec) (qs, body) =
  Format.printf "@[";
  Printer.print_list
    (print_qnt ppstate, 
     Printer.print_space) 
    qs;
  Format.printf ":@ ";
  print_term ppstate (assoc, prec) body;
  Format.printf"@]"

let print_as_binder (sym_assoc, sym_prec) ident sym = 
  let print_qnt ppstate (assoc, prec) arg =
    let (qnts, body) = 
      strip_fun_qnt ident (mk_app (mk_ident ident) arg) []
    in 
    Printer.print_assoc_bracket (assoc, prec) (sym_assoc, sym_prec) "(";
    Format.printf "@[<hov 3>";
    print_qnts ppstate (sym_assoc, sym_prec) (sym, qnts); 
    Printer.print_space ();
    print_term ppstate (assoc, sym_prec) body;
    Format.printf "@]";
    Printer.print_assoc_bracket (assoc, prec) (sym_assoc, sym_prec) ")"
  in 
  let lambda_arg x = 
    match x with
      Basic.Qnt(q, body) -> (Basic.binder_kind q)=Basic.Lambda
    | _ -> false
  in 
  let printer ppstate prec (f, args) =
    (match args with 
      (a::rest) -> 
	Format.printf "@[<2>";
	(if(lambda_arg a)
	then 
	  print_qnt ppstate prec a
	else 
	  simple_print_fn_app ppstate prec (f, args));
	Printer.print_list 
	  (print_term ppstate prec, Printer.print_space) rest;
	Format.printf "@]"
    | _ ->
	Format.printf "@[";
	print_ident_as_identifier ppstate prec f;
	Format.printf "@]")
  in 
  printer

(***
* Error handling 
***)

class termError s ts =
  object (self)
    inherit Result.error s
    val trms = (ts :term list)
    method get() = trms
    method print st = 
      Format.printf "@[%s@ @[" (self#msg()); 
      Printer.print_sep_list 
	(print st, ",") (self#get());
      Format.printf "@]@]"
  end

let term_error s t = mk_error((new termError s t):>error)
let add_term_error s t es = raise (add_error (term_error s t) es)


(***
* Comparisons and orderings 
***)

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
    [] -> raise (term_error "least: No arguments" [])
  | (x::xs) -> less_aux x xs

(*
   [term_lt]: More complex ordering on terms.
   Const < Id <  Bound < App < Qnt
   (Typed t1) < t2 iff t1<t2
*)
let rec term_lt t1 t2 = 
  let atom_lt (a1, ty1) (a2, ty2) =  a1<a2
  and 
      bound_lt (q1, n1, _) (q2, n2, _) =  n1<n2 && q1<q1
  in 
  match (t1, t2) with
    Typed (trm, _), _ -> term_lt trm t2
  | _, Typed (trm, _) -> term_lt t1 trm
  | (Const c1, Const c2) -> Basic.const_lt c1 c2
  | (Const _ , _ ) -> true
  | (Id _, Const _) -> false
  | (Id _, Id _) -> atom_lt (dest_ident t1) (dest_ident t2)
  | (Id _, _) -> true
  | (Meta _, Const _) -> false
  | (Meta _, Id _) -> false
  | (Meta b1, Meta b2) -> bound_lt (dest_binding b1) (dest_binding b2)
  | (Meta _, _) -> true
  | (Bound _, Const _) -> false
  | (Bound _, Id _) -> false
  | (Bound _, Meta _) -> false
  | (Bound b1, Bound b2) -> bound_lt (dest_binding b1) (dest_binding b2)
  | (Bound _ , _ ) -> true
  | (Free _, Const _) -> false
  | (Free _, Id _) -> false
  | (Free _, Meta _) -> false
  | (Free _, Bound _) -> false
  | (Free (n1, _), Free (n2, _)) -> n1<n2
  | (Free _, _) -> true
  | (App _, Const _) -> false
  | (App _, Id _) -> false
  | (App _, Meta _) -> false
  | (App _, Bound _) -> false
  | (App _, Free _) -> false
  | (App(f1, a1), App (f2, a2)) -> 
      if term_lt f1 f2 then true
      else if term_lt f2 f1 then false
      else term_lt a1 a2
  | (App _, _) -> true
  | (Qnt _, Const _) -> false
  | (Qnt _, Id _) -> false
  | (Qnt _, Meta _) -> false
  | (Qnt _, Bound _) -> false
  | (Qnt _, Free _) -> false
  | (Qnt _, App _) -> false
  | (Qnt(q1, b1), Qnt(q2, b2)) ->
      if(term_lt b1 b2) then true
      else bound_lt (dest_binding q1) (dest_binding q2)


let rec term_leq t1 t2 = 
  let atom_leq (a1, ty1) (a2, ty2) = a1<=a2 
  and 
      bound_leq (q1, n1, ty1) (q2, n2, ty2) =  n1<=n2 && q1<=q2
  in 
  match (t1, t2) with
    Typed (trm, _), _ -> term_leq trm t2
  | _, Typed (trm, _) -> term_leq t1 trm
  | (Const c1, Const c2) -> Basic.const_leq c1 c2
  | (Const _ , _ ) -> true
  | (Id _, Const _) -> false
  | (Id _, Id _) -> atom_leq (dest_ident t1) (dest_ident t2)
  | (Id _, _) -> true
  | (Meta _, Const _) -> false
  | (Meta _, Id _) -> false
  | (Meta b1, Meta b2) -> bound_leq (dest_binding b1) (dest_binding b2)
  | (Meta _, _) -> true
  | (Bound _, Const _) -> false
  | (Bound _, Id _) -> false
  | (Bound _, Meta _) -> false
  | (Bound b1, Bound b2) -> bound_leq (dest_binding b1) (dest_binding b2)
  | (Bound _ , _ ) -> true
  | (Free _, Const _) -> false
  | (Free _, Id _) -> false
  | (Free _, Meta _) -> false
  | (Free _, Bound _) -> false
  | (Free (n1, _), Free (n2, _)) -> n1<=n2
  | (Free _, _) -> true
  | (App _, Const _) -> false
  | (App _, Id _) -> false
  | (App _, Meta _) -> false
  | (App _, Bound _) -> false
  | (App _, Free _) -> false
  | (App(f1, a1), App (f2, a2)) -> 
      if term_leq f1 f2 then term_leq a1 a2
      else false
  | (App _, _) -> true
  | (Qnt _, Const _) -> false
  | (Qnt _, Id _) -> false 
  | (Qnt _, Meta _) -> false
  | (Qnt _, Bound _) -> false
  | (Qnt _, Free _) -> false
  | (Qnt _, App _) -> false
  | (Qnt(q1, b1), Qnt(q2, b2)) ->
      if(term_leq b1 b2) then true
      else bound_leq (dest_binding q1) (dest_binding q2)

let term_gt t1 t2 = not (term_leq t2 t1)

let rec is_subterm x y = 
  if(equals x y) then true
  else 
    match y with 
      App (f, a) -> (is_subterm x f) || (is_subterm x a)
    | Qnt (_, b) -> (is_subterm x b)
    | Typed (t, _) -> is_subterm x t
    | _ -> false

      
