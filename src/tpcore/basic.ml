(*-----
 Name: basic.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

type thy_id = string
type ident = (thy_id* string)

let null_thy = ""
let null_id = ("", "")
let is_null_id x = x=null_id
let thy_of_id (t, _) = t
let name (_, n) = n
let mk_long t n = (t, n)
let mk_name n = (null_thy, n)
let is_short_id (t, _) = t=null_thy

type id_selector = bool
let fn_id = true
let type_id = not fn_id

let string_fnid n =
  if (thy_of_id n)=null_thy then name n
  else (thy_of_id n)^"."^(name n)


let dest_fnid (t, n) = (t, n)

type conns_ty =
    Not
  | And
  | Or
  | Implies
  | Iff
  | Equal

type quant_ty =
    All
  | Ex
  | Lambda
  | Meta (* used for skolem constants *)

type const_ty =
    Null_const of int
(*       | Cnum of int   *)
(* for big numbers *)
  | Cnum of Num.num    (* big numbers *)
  | Cbool of bool



let const_lt x y=
  match (x, y) with
    Cbool(true), _ -> true
  | Cbool(false), _ -> true
  | Cnum(_), Cbool(_) -> false
  | Cnum(a), Cnum(b) -> a<b
  | _,_ -> false

let const_leq x y=
  if x=y then true
  else
    match (x, y) with
      Cbool(true), _ -> true
    | Cbool(false), _ -> true
    | Cnum(_), Cbool(_) -> false
    | Cnum(a), Cnum(b) -> a<=b
    | _,_ -> false


let string_const c=
  match c with 
    Null_const _ -> "null_constant"
(*    | Cnum n -> (string_of_int n) *)
  | Cnum n -> (Num.string_of_num n) 
  | Cbool b -> (string_of_bool b)

type fns =
    Name of ident

let prec_con c =
  match c with
    Equal -> 1
  | Not -> 2
  | And -> 3
  | Or -> 3
  | Iff -> 4
  | Implies -> 5

let std_prec f =
  match f with
    "equals" -> 1
  | "not" -> 2
  | "and" -> 3
  | "or" -> 3
  | "iff" -> 4
  | "implies" -> 5
  | _ -> -1


let prec_qnt q = 
  match q with 
    Lambda -> 100
  | All -> 101
  | Ex -> 102
  | _ -> 0

let conns_string x =
  match x with
    Not -> "not"
  | And -> "and"
  | Or -> "or"
  | Implies -> "=>"
  | Iff -> "<=>"
  | Equal -> "="

let connc_string c args =
  match c with 
    Not -> "not "^(List.hd args)^""
  | And -> (List.hd args)^" and "^(List.hd (List.tl args))
  | Or -> (List.hd args)^" or "^(List.hd (List.tl args))
  | Implies -> (List.hd args)^" => "^(List.hd (List.tl args))
  | Iff -> (List.hd args)^" <=> "^(List.hd (List.tl args))
  | Equal -> (List.hd args)^" = "^(List.hd (List.tl args))


let quant_string x =
  match x with 
    All -> "!"
  | Ex -> "?" 
  | Lambda -> "%"
  | Meta -> "*??*"

let fns_string x = 
  match x with
    Name n -> string_fnid n

(* Types *)

type base_typ = Bool | Num | Ind
type typ_const = (* Func |*) Defined of ident

type ('idtyp, 'tfun, 'tcons) pre_typ =
    Var of 'idtyp
  | Constr of 'tfun * ('idtyp, 'tfun, 'tcons) pre_typ list
  | Base of 'tcons
  | WeakVar of 'idtyp

type gtype = (string ref, typ_const, base_typ)pre_typ

(* Terms *)

type q_type = {quant: quant_ty; qvar: string; qtyp: gtype}
type binders = q_type ref
type term =
    Id of ident* gtype  
  | Bound of q_type ref
  | Free of string * gtype
  | App of term * term
  | Qnt of quant_ty * binders * term 
  | Const of const_ty
  | Typed of term * gtype

(* Binder operations *)
let binder_equality x y = x==y
let mk_binding qn qv qt 
    = ref{quant=qn; qvar=qv; qtyp=qt}
let dest_binding b = ((!b.quant), (!b.qvar), (!b.qtyp))
let binder_kind b = 
  let (x, _, _) = dest_binding b
  in x
let binder_name b = 
  let (_, x, _) = dest_binding b
  in x
let binder_type b = 
  let (_, _, x) = dest_binding b
  in x


let string_btype x =
  match x with 
    Bool -> "bool"
  | Num  -> "num"
  | Ind -> "ind"

let string_tconst x l =
  match x with 
(*    Func -> "("^(List.nth l 0)^"->"^(List.nth l 1)^")" *)
  | Defined n -> ((string_fnid n)^"("^
		  (Lib.list_string (fun x-> x) ", " l)^")")


(*
let date ()= Unix.time()
let nice_date f= 
  let tm = Unix.localtime f
  in 
  (tm.Unix.tm_year + 1900, tm.Unix.tm_mon, tm.Unix.tm_yday, 
   tm.Unix.tm_hour, tm.Unix.tm_min)
*)

(*
module PP =
  struct
(* Pretty Printer *)

    open Format
    exception Error of string

(* Fixity *)
(* associativity of infix is not important in the printer *)

    type fixity=Parserkit.Info.fixity
    let nonfix=Parserkit.Info.nonfix 
    let prefix=Parserkit.Info.prefix
    let suffix=Parserkit.Info.suffix
    let infix=Parserkit.Info.infix Parserkit.Info.non_assoc

(* default precedence, fixity and associativity  *)
    let default_term_prec = 0
    let default_term_assoc = Parserkit.Info.non_assoc
    let default_term_fixity= Parserkit.Info.nonfix

    let default_type_prec = 0
    let default_type_assoc = Parserkit.Info.non_assoc
    let default_type_fixity= Parserkit.Info.nonfix

(* string representation of fixity (for printing) *)
   let assoc_to_string a =
     match a with
       Parserkit.Info.Leftassoc -> "left associative"
     | Parserkit.Info.Rightassoc -> "right associative"
     | Parserkit.Info.Nonassoc -> "non-associative"

   let fixity_to_string fx=
     match fx with
       Parserkit.Info.Nonfix -> "nonfix"
     | Parserkit.Info.Prefix -> "prefix"
     | Parserkit.Info.Suffix -> "suffix"
     | Parserkit.Info.Infix(a) -> 
	 ("infix ("^(assoc_to_string a)^")")
	 
   

(* fixity tests *)

    let is_infix fx=
      Parserkit.Info.is_infix fx

    let is_prefix fx =
      Parserkit.Info.is_prefix fx

    let is_suffix fx =
      Parserkit.Info.is_suffix fx

(* PP information stores *)

    type record =
	{
	 prec: int;            (* its precedence *)
	 fixity: fixity;       (* its fixity *)
	 repr: string option   (* its representation *)
       } 

    type info = 
	{
	 term_info: (ident, record)Hashtbl.t;
	 type_info: (ident, record)Hashtbl.t
       }

(*
   type pp_info = (ident * pp_rec) list
 *)

    let mk_record pr inf rep =
      {prec=pr; fixity = inf; repr= rep }

    let empty_record () = mk_record (-1) nonfix None

    let default_info_size = ref 53

    let mk_info sz = 
      { 
	term_info=Hashtbl.create(sz);
	type_info=Hashtbl.create(sz)
      }
    let empty_info() = mk_info (!default_info_size)


    let get_term_info info id =
      try 
	let r=Hashtbl.find (info.term_info) id
	in 
	(r.prec, r.fixity, r.repr)
      with 
	Not_found -> (default_term_prec, default_term_fixity, None)

    let remove_term_info inf id =
      Hashtbl.remove (inf.term_info) id
	
    let add_term_record inf id rcrd=
      Hashtbl.add (inf.term_info) id rcrd

    let add_term_info inf id pr fx rp =
      let r={prec=pr; fixity=fx; repr=rp}
      in 
      Hashtbl.add (inf.term_info) id r

    let get_type_info info id =
      try 
	let r=Hashtbl.find (info.type_info) id
	in 
	(r.prec, r.fixity, r.repr)
      with 
	Not_found -> (default_type_prec, default_type_fixity, None)

    let remove_type_info inf id =
      Hashtbl.remove (inf.type_info) id
	
    let add_type_info inf id pr fx rp =
      let r={prec=pr; fixity=fx; repr=rp}
      in 
      Hashtbl.add (inf.type_info) id r

    let add_type_record inf id rcrd =
      Hashtbl.add (inf.type_info) id rcrd


    let rec list_print f sep x =
      match x with 
	[] -> ()
      | (b::[]) -> (f b)
      | (b::bs) -> (f b); sep(); (list_print f sep bs)


    let string_identifier id pp_rec =
      match pp_rec.repr
      with 
	None -> (string_fnid id) 
      | Some(x) -> x


    let print_ident x = 
   Format.printf "@[%s@]" (string_fnid x)

(* print_bracket: print a bracket depending on relative priority *)

    let print_bracket pr i br =
      if pr < i then Format.printf "@[%s@]" br else ()

(* Identifier Printer *)
    let print_identifier x pprec=
      let str=
	(match pprec with
	  None -> name x
	| Some(s) -> s)
      in 
      Format.printf "@[%s@]" str

  end



*)
