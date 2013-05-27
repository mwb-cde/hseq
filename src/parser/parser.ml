(*----
 Name: parser.ml
 Copyright M Wahab 2005-2010
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

(***
 * Parsers for terms and types.
 ***)


(** Utility functions *)
let get_optvalue v f = 
  match v with 
  | Some(y) -> y
  | None -> (f())

(*** 
 * Token information 
 ***)

type associativity=Grammars.associativity
let non_assoc=Grammars.non_assoc
let left_assoc=Grammars.left_assoc
let right_assoc=Grammars.right_assoc

type fixity=Grammars.fixity
let nonfix=Grammars.nonfix
let infix=Grammars.infix
let prefix=Grammars.prefix
let suffix=Grammars.suffix
    
(*** Default token information ***)

let default_term_prec = Grammars.default_term_prec
let default_term_assoc = Grammars.default_term_assoc
let default_term_fixity= Grammars.default_term_fixity

let default_type_prec = Grammars.default_type_prec
let default_type_assoc = Grammars.default_type_assoc
let default_type_fixity= Grammars.default_type_fixity


(***
* Basic types used by the parsers. 
***)

type input = Grammars.input
type ('a)phrase = 'a Grammars.phrase
type ('a)parse = 'a Grammars.parse

type typedef_data = Grammars.typedef_data

exception ParsingError = Grammars.ParsingError


(********
(***
* Resolver for operator overloading
***)
module Resolver =
  struct

    open Basic
      
(** memo_find: Memoised lookup function. *)
    let memo_find cache find table n =
      try Lib.find n cache
      with Not_found -> 
	let ret = find n table 
	in 
	(ignore(Lib.bind n ret cache); ret)

(** reolve_memo: Memoised tables *)
    type resolve_memo =
	{ 
	  types : (Ident.t, Basic.gtype)Hashtbl.t;
	  idents: (string, Ident.t)Hashtbl.t;
	  symbols : (string, Ident.t)Hashtbl.t;
	  type_names: (string, Ident.thy_id) Hashtbl.t
	}

(** resolve_arg: The argument to the resolver *)
    type resolve_arg =
	{
	 scp: Scope.t;
	 inf : int ref;
	 memo: resolve_memo;
	 qnts: Term.substitution;
	 lookup: (string -> gtype -> (Ident.t * gtype))
       }
	  
(** 
   [resolve_aux data env expty term]: Resolve names in [term].
   
   Each type name is expanded to a long name. 

   Each free variable [n] (which may be an overloaded symbol
   or name) is resolved to an exact identifier.
   
   [expty] is the type the term is expected to have (can be
   a variable type).

   Returns the renamed term, the actual type and a type
   substitution from which the exact type can be obtained
   (using Gtypes.mgu).

   Never fails.
*)
    let rec resolve_aux data env expty term =
      let bind_qnt t1 t2 =
	{ data with qnts=(Term.bind t1 t2 data.qnts) }
      in 
      let binding_set_names binding =
	let (qnt, qname, qtype) = Basic.dest_binding binding
	in 
	Basic.mk_binding qnt qname 
	  (Gtypes.set_name ~memo:(data.memo.type_names) (data.scp) qtype)
      in 
      let set_type_name t =
	Gtypes.set_name ~memo:(data.memo.type_names) data.scp t
      in 
      let find_ident n = 
	let ident_find n s = 
	  let thy = Scope.thy_of_term s n
	  in 
	  Ident.mk_long thy n
	in 
	Lib.try_find (memo_find data.memo.idents ident_find data.scp) n
      in 
      let find_type n = 
	let type_find n s = Scope.type_of s n
	in 
	Lib.apply_option
	  (fun x -> Some (Gtypes.rename_type_vars x))
	  (Lib.try_find (memo_find data.memo.types type_find data.scp) n)
	  None
      in 
      let find_sym n ty= 
	Lib.try_find 
	  (fun atyp -> 
	    let (x, xty) = data.lookup n atyp
	    in 
	    (x, Gtypes.rename_type_vars xty)) ty
      in 
      match term with
	Id(n, ty) -> 
	  if(Ident.is_short n)
	  then resolve_aux data env expty (Free((Ident.name_of n), ty))
	  else
	    (let id_ty = find_type n
	    in 
	    let nty = set_type_name ty
	    in 
	    let (ty0, env0)=
	      try (nty, Gtypes.unify_env data.scp expty nty env)
	      with _ -> (nty, env)
	    in 
	    let (ty1, env1)=
	      (match id_ty with
		None -> (ty0, env0)
	      | Some(d_ty) -> 
		  (try (d_ty, Gtypes.unify_env data.scp ty0 d_ty env0)
		  with _ -> (d_ty, env0)))
	    in 
	    (Id(n, (Gtypes.mgu ty1 env1)), ty1, env1))
      | Free(n, ty) -> 
	  let nty = set_type_name ty
	  in 
	  let (ty0, env0)=
	    try (nty, Gtypes.unify_env data.scp expty nty env)
	    with _ -> (nty, env)
	  in 
	  let ty1=
	    try Gtypes.mgu ty0 env0 
	    with _ -> ty0
	  in 
	  (match (find_sym n ty1) with
	    None -> 
	      (match (find_ident n) with
		None -> (Free(n, ty1), ty1, env0) 
	      | Some(id3) -> 
		  (resolve_aux data env0 ty1 (Id(id3, ty1))))
	  | Some(id2, ty2) -> 
	      resolve_aux data env0 ty1 (Id(id2, ty2)))
      | Bound(q) -> 
	  let term1=
	    try Term.find term data.qnts
	    with Not_found -> term
	  in 
	  let ty = Term.get_binder_type term1
	  in
	  let (ty0, env0)=
	    try (ty, Gtypes.unify_env data.scp expty ty env)
	    with _ -> (ty, env)
	  in 
	  (term1, ty0, env0)
      | Meta(q) -> 
	  let ty = Term.get_binder_type term
	  in
	  let (ty0, env0)=
	    try (ty, Gtypes.unify_env data.scp expty ty env)
	    with _ -> (ty, env)
	  in 
	  (term, ty0, env0)
      | Const(c) ->
	  let ty = Lterm.typeof_cnst c
	  in
	  let (ty0, env0)=
	    try (ty, Gtypes.unify_env data.scp expty ty env)
	    with _ -> (ty, env)
	  in 
	  (term, ty0, env0)
      | Typed(trm, ty) -> 
	  let nty = set_type_name ty
	  in 
	  let (ty0, env0)=
	    try (nty, Gtypes.unify_env data.scp expty nty env)
	    with _ -> (nty, env)
	  in 
	  let trm1, nty1, env1 = 
	    resolve_aux data env0 nty trm
	  in 
	  let (nty2, env2)=
	    try 
	      (nty1, Gtypes.unify_env data.scp nty nty1 env1)
	    with _ -> (nty1, env1)
	  in 
	  (Typed(trm1, nty2), nty2, env2)
      | App(lf, a) -> 
	  let argty = Gtypes.mk_typevar data.inf
	  in 
	  let rty0 = Gtypes.mk_typevar data.inf
	  in 
	  let (rty1, env1)=
	    try (rty0, Gtypes.unify_env data.scp expty rty0 env)
	    with _ -> (rty0, env)
	  in 
	  let fty0 = Lterm.mk_fun_ty argty rty1
	  in 
	  let (atrm, aty, aenv) = 
	    resolve_aux data env1 (Gtypes.mgu argty env1) a
	  in  
	  let (ftrm, fty, fenv) = 
 	    resolve_aux data aenv (Gtypes.mgu fty0 aenv) lf
	  in 
	  (App(ftrm, atrm), (Gtypes.mgu rty1 fenv), fenv)
      | Qnt(qnt, body) ->
	  (match Basic.binder_kind qnt with
	    Lambda -> 
	      let qnt1=binding_set_names qnt
	      in 
	      let data1=bind_qnt (Bound(qnt)) (Bound(qnt1))
	      in 
	      let aty = Term.get_binder_type (Bound qnt1)
	      and rty = Gtypes.mk_typevar data1.inf
	      in 
	      let nty0 = Lterm.mk_fun_ty aty rty
	      in 
	      let (nty1, env1)=
		try (nty0, Gtypes.unify_env data1.scp expty nty0 env)
		with _ -> (nty0, env)
	      in 
	      let (body1, bty, benv) = 
		resolve_aux data1 env1 rty body
	      in
	      (Qnt(qnt1, body1), nty1, benv)
	  | _ -> 
	      let qnt1=binding_set_names qnt
	      in 
	      let data1=bind_qnt (Bound(qnt)) (Bound(qnt1))
	      in 
	      let (nty1, env1)=
		try (Lterm.mk_bool_ty(), 
		     Gtypes.unify_env 
		       data1.scp expty 
		       (Lterm.mk_bool_ty()) env)
		with _ -> (Lterm.mk_bool_ty(), env)
	      in 
	      let (body1, bty, benv)=
		resolve_aux data1 env1 nty1 body
	      in 
	      (Qnt(qnt1, body1), nty1, benv))


(**
   [default str ty lst]: Get the default identifier for symbol [str]
   of type [ty] from list [lst] of identifiers when no identifier
   matches. 

   Currently, this just raises Not_found
*)
    let default str ty list = None
(*
    let default str ty list= 
      match list with 
	[] -> None
      | (x::_) -> Some x
*)

(**
   [resolve_term env t]: Resolve the symbols in term [t].
   For each free variable [Free(s, ty)] in [t], Lookup [s]
   in [env] to get long identifier [id].  If not found,
   use [Free(s, ty)].  If found, replace [Free(s, ty)]
   with the identifier [Id(id, ty)].
*)
    let resolve_term scp lookup term=
      let rmemo=
	{ 
	  types = Lib.empty_env(); idents=Lib.empty_env();
	  symbols=Lib.empty_env(); type_names = Lib.empty_env()
	}
      in 
      let data = 
	{ 
	  scp = scp;
	  inf= ref 0;
	  memo = rmemo;
	  qnts = Term.empty_subst();
	  lookup = lookup
	}
      in 
      let expty = Gtypes.mk_null()
      in 
      let (term1, ty1, subst) = 
	resolve_aux data (Gtypes.empty_subst()) expty term
      in 
      (term1, subst)

(** 
   find_type scp sym ty list: 
   return first identifier-type pair for symbol sym in list 
   where ty is the type of sym.
	   
   Matching is by unification.
*)
    let find_type scp sym ty list =
      let matching_types t1 t2 = 
	try
	  ignore(Gtypes.unify scp t1 t2); true
	with _ -> false
      in 
      let rec find_aux l = 
	match l with
	  [] -> 
	    (match default sym ty list with
	      None ->  raise Not_found 
	    | Some x -> x)
	| ((id, id_type)::xs) -> 
	    if matching_types ty id_type
	    then (id, id_type)
	    else find_aux xs
      in 
      find_aux list

    let make_lookup scp db s ty= 
      let type_list = db s
      in 
      let (id, id_type) = find_type scp s ty type_list
      in 
      (id, id_type)

  end

********)

(***
* Parser data
***)

open Lexer
open Lterm


(*** Symbols ***)

(** Symbol tables *)
type table = Lexer.symtable

let syms_list = 
  [(".", Sym DOT); 
   ("(", Sym ORB);
   (")", Sym CRB); 
   (",", Sym comma_sym); 
   ("'", Sym PRIME);
   (":", Sym COLON);
   ("true", BOOL true); ("false", BOOL false);
   ("!", Key ALL); ("all", Key ALL); 
   ("forall", Key ALL);
   ("?", Key EX); ("exists", Key EX);
   ("%", Key LAM); "lambda", Key LAM]

let default_symtable_size = 51
let symtable_size= ref default_symtable_size
let symbols= ref (mk_symtable (! symtable_size))
let symtable()= !symbols

let add_symbol sym tok=
  try
    symbols:=Lexer.add_sym (!symbols) sym tok
  with _ -> ()

let find_symbol sym= Lexer.find_sym (!symbols) sym

let remove_symbol sym =
  symbols:=Lexer.remove_sym (!symbols) sym

let init_symtable sz = 
  symbols := (mk_symtable sz);
  List.iter (fun (s, t) ->  add_symbol s t) syms_list

(*** Tokens ***)

let token_table=Grammars.token_table_new Grammars.default_table_size

let add_token_info tok tok_info=
  Grammars.token_table_add token_table tok tok_info

let get_token_info tok=
  Grammars.token_table_find token_table tok

let remove_token_info tok =
  Grammars.token_table_remove token_table tok 

let type_token_table=Grammars.token_table_new Grammars.default_table_size

let add_type_token_info tok tok_info=
  Grammars.token_table_add type_token_table tok tok_info

let get_type_token_info tok =
  Grammars.token_table_find type_token_table tok

let remove_type_token_info tok =
  Grammars.token_table_remove type_token_table tok

let mk_info ()= Grammars.mk_inf token_table type_token_table

(*** Toplevel symbol and token functions *)

let add_token id sym fx pr=
  (* lexer information *)
  add_symbol sym (Sym (OTHER sym)); 
  (* parser information *)
  add_token_info (Sym(OTHER sym)) (Some(Ident.mk_name sym, fx, pr))

let remove_token sym=
  remove_symbol sym;
  remove_token_info (Sym(OTHER sym))

let add_type_token id sym fx pr=
  (* lexer information *)
  add_symbol sym (Sym (OTHER sym));
  (* parser information *)
  add_type_token_info (Sym(OTHER sym)) (Some(id, fx, pr))

let remove_type_token sym=
  remove_symbol sym;
  remove_type_token_info (Sym(OTHER sym))

(*** Overloading *)
type overload_table_t = (string, (Ident.t * Basic.gtype) list) Hashtbl.t

let default_overload_table_size = 127
let overload_table_size = ref default_overload_table_size
let mk_overload_table sz = Hashtbl.create sz
let overload_table = ref (mk_overload_table default_overload_table_size)
let get_overload_table () = ((!overload_table): overload_table_t)

let init_overload () = 
  overload_table := mk_overload_table default_overload_table_size

let get_overload_list ?ovltbl sym =
  let ovltab = get_optvalue ovltbl get_overload_table in 
  Hashtbl.find ovltab sym

let insert_pos pos d lst = 
  let rec split_at s l r =
    match l with 
      [] -> (r, [])
    | (x, ty)::ls -> 
	if(x=s) 
	then 
	  (List.rev r, l)
	else 
	  split_at s ls ((x, ty)::r)
  in 
  match pos with
    Lib.First -> d::lst
  | Lib.Last -> List.rev (d::(List.rev lst))
  | Lib.Before s -> 
      let (lt, rt) = split_at s lst []
      in 
      List.rev_append (List.rev lt) (d::rt)
  | Lib.After s ->
      let (lt, rt)=split_at s lst []
      in 
      let nrt=
	(match rt with
	  [] ->  [d]
	| x::rst -> x::d::rst)
      in 
      List.rev_append (List.rev lt) nrt
  | Lib.Level s -> 
      let (lt, rt)=split_at s lst []
      in 
      List.rev_append (List.rev lt) (d::rt)

let add_overload ?ovltbl sym pos (id, ty) =
  let table = get_optvalue ovltbl get_overload_table
  in 
  let list0 = 
    try (get_overload_list ~ovltbl:table sym)
    with Not_found -> []
  in 
  let list1 = insert_pos pos (id, ty) list0
  in 
  Hashtbl.replace table sym list1;
  table
    
let remove_overload ?ovltbl sym id =
  let table = get_optvalue ovltbl get_overload_table in
  let list0 = get_overload_list ~ovltbl:table sym
  in 
  let list1 = 
    List.remove_assoc id list0
  in 
  begin
    match list1 with
      [] -> Hashtbl.remove table sym
    | _ -> Hashtbl.replace table sym list1
  end;
  table

let print_overloads ?ovltbl info = 
  let table = get_optvalue ovltbl get_overload_table in
  let print_fn sym list= 
    Format.printf "@[<2>%s@ " sym;
    List.iter
      (fun (id, ty) -> 
	Printer.print_ident id;
	Format.printf ":@ ";
	Gtypes.print info ty;
	Format.printf ";@ ")
      list;
    Format.printf "@]@,"
  in 
  Format.printf "@[<v>";
  Hashtbl.iter print_fn table;
  Format.printf "@]"


(** Parser tables *)
module Table = 
struct
  type t =
      {
        tokens_f: Grammars.token_table;
        type_tokens_f: Grammars.token_table;
        symbols_f: Lexer.symtable;
        overloads_f: overload_table_t;
        term_parsers_f:
          (string, 
           Grammars.parser_info -> Pterm.t phrase) Lib.named_list;
        type_parsers_f: 
          (string, 
           Grammars.parser_info -> (Basic.gtype phrase)) Lib.named_list;
      }

  let default_size = (Grammars.default_table_size,
                      Grammars.default_table_size,
                      default_symtable_size,
                      default_overload_table_size)

  let empty (tok_size, tytok_size, stm_size, ov_size) = 
    {
      tokens_f = Grammars.token_table_new tok_size;
      type_tokens_f = Grammars.token_table_new tytok_size;
      symbols_f = Lexer.mk_symtable stm_size;
      overloads_f = mk_overload_table ov_size;
      term_parsers_f = Grammars.core_term_parsers;
      type_parsers_f = Grammars.core_type_parsers;
    }

  let init_symbols symtbl syms = 
    List.fold_left 
      (fun tbl (s, t) -> Lexer.add_sym tbl s t) 
      symtbl syms

  let init tbl = 
    let toks = Grammars.token_table_reset tbl.tokens_f
    and tytoks = Grammars.token_table_reset tbl.type_tokens_f
    and symtab = init_symbols tbl.symbols_f syms_list;
    and ovltab = mk_overload_table default_overload_table_size
    in 
    {
      tokens_f = toks; type_tokens_f = tytoks;
      symbols_f = symtab; overloads_f = ovltab;
      term_parsers_f = Grammars.core_term_parsers;
      type_parsers_f = Grammars.core_type_parsers;
    }

  let tokens t = t.tokens_f
  let set_tokens t x = {t with tokens_f = x}
  let type_tokens t = t.type_tokens_f 
  let set_type_tokens t x = {t with type_tokens_f = x}
  let symbols t = t.symbols_f
  let set_symbols t x = {t with symbols_f = x}
  let overloads t = t.overloads_f
  let set_overloads t x = {t with overloads_f = x}
  let term_parsers t = t.term_parsers_f
  let set_term_parsers t x = {t with term_parsers_f = x}
  let type_parsers t = t.type_parsers_f
  let set_type_parsers t x = {t with type_parsers_f = x}
end 

(*** Initialising functions ***)

let init_token_table()=
  Grammars.token_table_reset token_table

let init_type_token_table()=
  Grammars.token_table_reset type_token_table
    
let init_tables tbl =
  init_symtable default_symtable_size;
  ignore(init_type_token_table());
  ignore(init_token_table());
  init_overload()

let init_parsers tbl = 
  let tbl0 = Table.set_type_parsers tbl Grammars.core_type_parsers in
  let tbl1 = Table.set_term_parsers tbl Grammars.core_term_parsers in
  tbl1

let init () = 
  let tbl0 = init_parsers (Table.empty Table.default_size) in
  init_tables tbl0
  

(**
   Parsers
   read a given phrase followed by an end of file/string
*)

let parse ph inp = Grammars.Pkit.parse ph EOF inp

let identifier_parser inp =
  parse (Grammars.long_id Grammars.id_relaxed (mk_info ())) inp

let type_parser inp =
  parse (Grammars.types (mk_info ())) inp
let typedef_parser inp =
  parse (Grammars.typedef (mk_info ())) inp

let term_parser inp=
  parse (Grammars.form (mk_info ())) inp

let defn_parser inp = 
  parse (Grammars.defn (mk_info ())) inp

(*** User defined parsers ***)

let term_parser_list tbl = Table.term_parsers tbl
let add_term_parser tbl pos n ph = 
  let plist0 = Table.term_parsers tbl in 
  Table.set_term_parsers tbl (Lib.named_add plist0 pos n ph)

let remove_term_parser tbl n =
  let plist0 = Table.term_parsers tbl in 
  Table.set_term_parsers tbl (List.remove_assoc n plist0)

let type_parser_list tbl = Table.type_parsers tbl
let add_type_parser tbl pos n ph = 
  let plist0 = Table.type_parsers tbl in 
  Table.set_type_parsers tbl (Lib.named_add plist0 pos n ph)

let remove_type_parser tbl n = 
  let plist0 = Table.type_parsers tbl in 
  Table.set_type_parsers tbl (List.remove_assoc n plist0)

(*** Readers: read and parse a string ***)

let get_symtab tbl = 
  get_optvalue tbl symtable

let read ?tbl ph str =
  let symtab = get_symtab tbl in 
  Lexer.reader (scan symtab) ph str

let read_term ?tbl str = 
  read ?tbl term_parser str

let read_type ?tbl str = 
  read ?tbl type_parser str

let test_lex ?tbl str = 
  let symtab = get_symtab tbl in 
  scan symtab (Stream.of_string str);;

let test ?tbl str =  
  let symtab = get_symtab tbl in   
  reader (scan symtab) term_parser str


