(*-----
 Name: term.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** Term representation and their basic manipulation *)

open Basic
open Gtypes

val binder_equiv : Scope.t -> term -> term -> bool

(* equality of terms *)
val equals : term -> term -> bool

(**
   [Termhash]:
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

(** Substitution in terms *)

(**
   [TermTree]
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

(**
   [substitution]: the type of term substitutions.
*)
type substitution 

(** subsitution construction *)
val empty_subst: unit -> substitution
(*
val subst_size: int -> substitution
*)

(* lookup/add/remove term from a substitution *)
val find: term -> substitution -> term
val bind: term -> term -> substitution -> substitution
val member: term -> substitution -> bool
val remove: term -> substitution -> substitution
(*
val quiet_remove: term -> substitution -> substitution
*)

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

(* constructors/destructors for quantified terms *)
val get_qnt_type : term -> Basic.gtype
val get_qnt_body : term -> term


(**
   [mk_qnt_name scp qnt name t]: make a quantified term, with
   quantifier [qnt], from term [t], replacing all free variables in [t]
   with name [name] with the new bound variable.

   [mk_typed_qnt_name scp qnt ty name t]: make a quantified term, with
   quantifier [qnt], from term [t], replacing all free variables in
   [t] with name [name] with the bound variable. Set the type of the
   quantifier to [ty].
*)
val mk_qnt_name : Scope.t -> 
  Basic.quant_ty -> string -> term -> term
val mk_typed_qnt_name : Scope.t -> 
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
(**
   [mk_qnt scp b t]: make a quantified term, from bound variable [b]
   and term [t]. The quantifier type matches the bound variable [b].
*) 
val mk_qnt : binders -> term -> term

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


(**
   [dest_unop t]: Destruct unary operator [t], return the identifier
   and argument.

   [dest_binop t]: Destruct binary operator [t], return the identifier
   and two arguments.

   raise [Failure] if not enough arguments.
*)
val dest_unop : Basic.term -> (Basic.ident * Basic.term)
val dest_binop : Basic.term -> (Basic.ident * Basic.term * Basic.term)

(* Retyping *)

(*
   Reset the types in a term using a given context/subsitution 
   substitutes variables with their concrete type in the context 
*)
val retype: Gtypes.substitution -> term -> term

(*
   [retype_pretty]
   as for retype, make substitution for type variables
   but also replace other type variables with new, prettier names 

   retype_pretty_env: 
   like retype_pretty but also return the substitution storing
   from the bindings/replacements generated during retyping.
 *)

val retype_pretty_env: Gtypes.substitution -> term 
  -> (term * Gtypes.substitution)

val retype_pretty: Gtypes.substitution -> term 
  -> term 

(* Pretty printing *)

val print_qnts: 
    Printer.ppinfo -> (Printer.fixity * int)
      -> (string * (Basic.binders list)) Printer.printer 

(* 
   [print_typed_obj level printer ppstate prec (obj, ty)]: 

   If [Setting.print_type_level > level] print [obj] with [ty] as its type
   in the form [(obj: ty)] otherwise print [obj] only.

   Use [printer] to print [obj].
*)
val print_typed_obj:
    int 
  -> (Printer.ppinfo -> (Printer.fixity * int) -> ('a) Printer.printer)
    -> Printer.ppinfo
    -> (Printer.fixity * int)
      -> ('a * Basic.gtype) Printer.printer 

val print_bracket:
    (Printer.fixity * int) -> (Printer.fixity * int)
      -> string Printer.printer

val print_infix: 
    (((Printer.fixity * int) -> Basic.ident Printer.printer)
       * ((Printer.fixity * int) -> term Printer.printer))
    -> (Printer.fixity * int) 
      -> (Basic.ident * (term)list) Printer.printer
val print_prefix: 
    (((Printer.fixity * int) -> Basic.ident Printer.printer)
       * ((Printer.fixity * int) -> term Printer.printer))
    -> (Printer.fixity * int) 
      -> (Basic.ident * (term)list) Printer.printer
val print_suffix: 
    (((Printer.fixity * int) -> Basic.ident Printer.printer)
       * ((Printer.fixity * int) -> term Printer.printer))
    -> (Printer.fixity * int) 
      -> (Basic.ident * (term)list) Printer.printer

val print_fn_app :
    Printer.ppinfo 
  -> (((Printer.fixity * int) -> Basic.ident Printer.printer)
	* ((Printer.fixity * int) -> term Printer.printer))
    -> (Printer.fixity * int)
      -> (Basic.ident * (term)list) Printer.printer

val pplookup: Printer.ppinfo -> Basic.ident -> Printer.record
val print_term : 
    Printer.ppinfo -> (Printer.fixity * int) -> term Printer.printer

(**
   [simple_print_fn_app]
   utility function for user defined pretty-printers.
   print an application as 'f a1 a2 .. an'
*)
val simple_print_fn_app: 
    Printer.ppinfo -> (Printer.fixity * int) 
      -> (Basic.ident * term list) Printer.printer

val print : Printer.ppinfo -> term -> unit
val print_simple: term -> unit
   
(**
   [print_as_binder (sym_assoc, sym_prec) f sym]
   Construct a printer to print function applications
   of the form [f (%x: P)] as [sym x: P].
*)
val print_as_binder:
    (Printer.fixity * int) -> Basic.ident -> string
      -> Printer.ppinfo 
	-> (Printer.fixity * int)
	  -> (Basic.ident * Basic.term list) Printer.printer

(**
   [print_qnt_body (assoc, prec) qs body]
   Print term [body] quantified by variables [qs=[x1; x2; ...; xn]]
   as [x1 x2 ... xn : body]
*)
val print_qnt_body: 
    Printer.ppinfo -> (Printer.fixity * int)
      -> ((Basic.binders)list * Basic.term) Printer.printer

(** 
   [strip_fun_qnt f term qs]:
   strip applications of the form [f (% x: P)] returning 
   the bound variables and P.

   (qs should be [] initially)
*)
val strip_fun_qnt: 
    Basic.ident -> Basic.term -> Basic.binders list 
      -> (Basic.binders list * Basic.term)


(* Error handling *)

class termError : string -> term list ->
  object
    inherit Result.error 
    val trms : term list
    method get : unit -> term list
  end
val term_error : string -> term list -> exn
val add_term_error : string -> term list -> exn -> 'a

(*
   get and set full names and types in a term 

   if a term is free (not defined in any theory),
   its theory is set to Basic.null_thy.
*)
val set_names: Scope.t  -> term -> term

(*
   check that term is in scope:
   all identifiers and types must be declared in the given scope 
*)
val in_scope: (string, bool)Lib.substype 
  -> Scope.t -> Basic.thy_id -> term -> bool

(* Simple ordering on terms 
   [compare_term] uses built-in ocaml compare.

   [less_than]: uses [compare_term].
*)
val compare_term: term -> term -> int 
val less_than : term -> term  -> bool
val least: term list -> term

(**
   [term_lt]: more complex ordering on terms.
   Const < Var <  Bound < App < Qnt
   (Typed t1) < t2 iff t1<t2
*)
val term_lt: term -> term -> bool
val term_leq: term -> term -> bool
val term_gt: term -> term -> bool


(** [rebuild_qnt k qs b]
   rebuild quantified term of kind k from quantifiers [qs] and body [b]

   e.g. [rebuild_qnt All ["x", "y", "z"] << b >>]
   ->
   [ << !x y z : b >> ]
 *)
val rebuild_qnt: 
    quant_ty -> binders list -> term -> term

(**
   [close_term qnt free trm]: Close term [trm]. Make variables bound
   to quantifiers of kind [qnt] to replace free variables and bound
   variables with no binding quantifier and for which [free] is true.
 *)
val close_term: 
    quant_ty -> (term -> bool) -> term -> term
