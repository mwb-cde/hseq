(* Term representation and their basic manipulation *)

open Basic
open Gtypes

(*
(* Records for quantifiers *)
type q_type =
    { quant: Basic.quant_ty;
      qvar: string;
      qtyp: Gtypes.gtype}

(* the type of binding quantifers *)
(* primitive quanitifiers are All, Exists and Lambda *)

type binders

(* the representation of a term *)
type term =
    Var of Basic.ident * gtype
  | Qnt of binders * term
  | Bound of binders
  | Const of Basic.const_ty
  | Typed of term * Gtypes.gtype
  | App of term * term
*)
(*
(* construct/destruct/compare bindings *)
val mk_binding : Basic.quant_ty -> string -> Gtypes.gtype
  -> binders
val dest_binding : binders -> 
  (Basic.quant_ty * string * Gtypes.gtype)
val binder_equality: binders -> binders -> bool
*)
val binder_equiv : scope -> term -> term -> bool

(* equality of terms *)
val equals : term -> term -> bool
(* renamed to equals
   val equality : term -> term -> bool
 *)

(* 
   Hashtables with a term as the key
 *)

module type TERMHASHKEYS=
  sig 
    type t = term
    val equal : t -> t -> bool
    val hash: t -> int
  end
module type TERMHASH = (Hashtbl.S with type key = term)
module Termhash: TERMHASH

(* tables, usefull for memoising functions *)
type ('a)table (* = ('a) Termhash.t *)
val empty_table: unit -> ('a) table
val table_find: term -> 'a table -> 'a
val table_member: term -> 'a table -> bool
val table_remove: term -> 'a table -> unit
val table_add : term -> 'a -> 'a table -> unit
val table_rebind : term -> 'a -> 'a table -> unit

(* rename bound variables in term (alpha-conversion) *)
val rename: term -> term

(* The type of term substitutions *)

(* [TermTree]
   Trees indexed by terms
 *)
module TermTreeData: Treekit.TreeData

module TermTree: 
    sig
      val eql : 'a -> 'a -> bool
      val lessthan : 'a -> 'a -> bool
      type depth_t = int
      and 'a t =
	  'a Treekit.BTree(TermTreeData).t =
          Nil
	| Branch of ((TermTreeData.key * 'a) list * 'a t * 'a t * depth_t)
      val nil : 'a t
      val create : (TermTreeData.key * 'a) list -> 'a t -> 'a t -> 'a t
      val data : 'a t -> (TermTreeData.key * 'a) list
      val left : 'a t -> 'a t
      val right : 'a t -> 'a t
      val depth : 'a t -> depth_t
      val balance : 'a t -> 'a t
      val add : 'a t -> TermTreeData.key -> 'a -> 'a t
      val replace : 'a t -> TermTreeData.key -> 'a -> 'a t
      val delete : 'a t -> TermTreeData.key -> 'a t
      val find : 'a t -> TermTreeData.key -> 'a
      val find_all : 'a t -> TermTreeData.key -> 'a list
      val mem : 'a t -> TermTreeData.key -> bool
      val iter : (TermTreeData.key -> 'a -> 'b) -> 'a t -> unit
      val to_list : 'a t -> (TermTreeData.key * 'a) list list
    end

type substitution 

(* construct subsitutions *)
val empty_subst: unit -> substitution
val subst_size: int -> substitution

(* lookup/add/remove term from a substitution *)
val find: term -> substitution -> term
val bind: term -> term -> substitution -> substitution

val remove: term -> substitution -> substitution
val quiet_remove: term -> substitution -> substitution

val chase: (term -> bool) -> term -> substitution -> term
val fullchase: 
    (Termhash.key -> bool) -> term -> substitution -> term
val chase_var: (term -> bool) -> term -> substitution -> term
val replace: substitution -> term -> term

(* carry out a substitution in a term *)
val subst : substitution -> term -> term 
val subst_quick : term -> term -> term -> term

(* get free variables in a term (constructed with Var) *)
val get_free_vars : term -> term list

(* get bound vars with no matching binders *)
val get_free_binders : term -> binders list

(* destructors for bindings *)
(* val get_binder : term -> q_type *)
val get_binder : term -> binders
val get_binder_name : term -> string
val get_binder_type : term -> gtype

val dest_qnt :
    term -> binders * Basic.quant_ty * string * Basic.gtype * term

(* constructors/destructors for quanitifed terms *)
val get_qnt_type : term -> Basic.gtype
val get_qnt_body : term -> term
val mk_qnt : scope -> 
  Basic.quant_ty -> string -> term -> term
val mk_typed_qnt : scope -> 
  Basic.quant_ty -> Basic.gtype -> string -> term -> term

(* conversion to a string *)
val string_typed_name : string -> Basic.gtype -> string
val string_term : term -> string
val string_inf_term : ((Basic.ident -> int) * (Basic.ident -> bool))
  -> term -> string
val string_term_basic: term -> string

(* instantiate a quantified formula t with term r *)
val inst : term -> term -> term

(* constructors/destructors/reconisers for terms *)

val is_var : term-> bool
val mk_var : Basic.ident -> term
val mk_short_var :string -> term
val mk_typed_var : Basic.ident -> Basic.gtype -> term
val dest_var : term-> Basic.ident * Basic.gtype
val get_var_id : term-> Basic.ident
val get_var_type : term-> Basic.gtype

val is_bound : term-> bool
val mk_bound : binders -> term
val dest_bound : term-> binders

val is_free : term-> bool
val mk_free : string -> Basic.gtype -> term
val dest_free : term-> (string * Basic.gtype)
val get_free_name : term-> string

val mk_meta : string -> Basic.gtype -> term
val is_meta : term -> bool

val is_qnt : term-> bool

val is_fun : term-> bool
val mk_fun : Basic.ident -> term list -> term
val dest_fun : term-> Basic.ident * term list

val is_app : term-> bool
val mk_app : term -> term  -> term
val dest_app : term-> term * term 
val mk_comb: term -> term list -> term

val is_typed : term-> bool
val mk_typed : term -> Basic.gtype -> term
val dest_typed : term-> term* Basic.gtype

val is_const : term-> bool
val mk_const : Basic.const_ty -> term
val dest_const : term-> Basic.const_ty

val mk_num : Num.num -> term
val destnum : term -> Num.num
val mk_int : int -> term

val mk_bool : bool -> term
val destbool : term-> bool
val is_true :term-> bool

(* remove outermost quantifiers from a term *)
val strip_qnt : Basic.quant_ty -> term -> binders list * term

(* get function identifier and arguments of an application *)
val get_args: term -> term list
val get_fun: term -> term
val flatten_app : term -> term list
val get_fun_args: term -> (term* term list)


(* Retyping *)

(* reset the types in a term using a given context/subsitution *)
(* substitutes variables with their concrete type in the context *)
val retype: Gtypes.substitution -> term -> term

(* [retype_pretty]
   as for retype, make substitution for type variables
   but also replace other type variables with new, prettier names 

   retype_pretty_env: 
   like retype_pretty but also return the substitution storing
   from the bindings/replacements generated during retyping.
 *)

val retype_pretty_env: Gtypes.substitution -> term 
  -> (term * Gtypes.substitution)
(*
   val retype_pretty: Gtypes.substitution -> term -> term
*)

val retype_pretty: Gtypes.substitution -> term 
  -> term 


(* Pretty printing *)

val pplookup: Printer.ppinfo -> Basic.ident -> Printer.record
val print_term : Printer.ppinfo -> int -> term Printer.printer

(**
   [simple_print_fn_app]
   utility function for user defined pretty-printers.
   print an application as 'f a1 a2 .. an'
*)
val simple_print_fn_app: 
    Printer.ppinfo -> int -> (Basic.ident * term list) Printer.printer

(* 
   print_term renamed to print,
   simple_term_printer renamed to print_simple
*)
val print : Printer.ppinfo -> term -> unit
val print_simple: term -> unit
   
(* Error handling *)

class termError : string -> term list ->
  object
    inherit Result.error 
    val trms : term list
    method get : unit -> term list
  end
val mk_termError: string -> term list -> Result.error
val termError : string -> term list -> exn
val addtermError : string -> term list -> exn -> 'a

(*
   get and set full names and types in a term 

   if a term is free (not defined in any theory),
   its theory is set to Basic.null_thy.
*)
val set_names: Gtypes.scope  -> term -> term

(* check that term is in scope:
   all identifiers and types must be declared in the given scope *)
val in_thy_scope: (string, bool)Lib.substype 
  -> Gtypes.scope -> Basic.thy_id -> term -> bool

(* simple ordering on terms *)

val compare_term: term -> term -> int (* uses ocaml built-in compare *)
(* the following use compare_term *)
val less_than : term -> term  -> bool
val least: term list -> term

(* 
   more complex ordering on terms:
   Const < Var <  Bound < App < Qnt
   (Typed t1) < t2 iff t1<t2
 *)

val term_lt: term -> term -> bool

val term_leq: term -> term -> bool
val term_gt: term -> term -> bool



