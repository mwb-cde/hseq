(* Term representation and their basic manipulation *)

    open Gtypes

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
      	Var of Basic.fnident * gtype
      | Qnt of binders * term
      | Bound of binders
      | Const of Basic.const_ty
      | Typed of term * Gtypes.gtype
      |	App of term * term

(* construct/destruct/compare bindings *)
    val mk_binding : Basic.quant_ty -> string -> Gtypes.gtype
	-> binders
    val dest_binding : binders -> 
      (Basic.quant_ty * string * Gtypes.gtype)
   val binder_equality: binders -> binders -> bool
    val binder_equiv : scope -> term -> term -> bool

(* equality of terms (uses Dequals test) *)
    val equality : term -> term -> bool

(* rename bound variables in term (alpha-conversion) *)

val rename: term -> term

(* The type of term  subsitutions *)

module type TERMHASHKEYS=
  sig 
    type t = term
    val equal : t -> t -> bool
    val hash: t -> int
  end
module type TERMHASH = (Hashtbl.S with type key = term)
module Termhash: TERMHASH
type substitution (* = (term)Termhash.t*)

(* construct subsitutions *)
    val empty_subst: unit -> substitution
    val subst_size: int -> substitution

(* lookup/add/remove term from a substitution *)
    val find: term -> substitution -> term
    val bind: term -> term -> substitution -> substitution
    val bind_env: term -> term -> substitution -> unit
    val add: term -> term -> substitution -> term
    val remove: term -> substitution -> unit
    val quiet_remove: term -> substitution -> unit
    val chase: (term -> bool) -> term -> substitution -> term
    val fullchase: (Termhash.key -> bool) 
      -> term -> substitution -> term
    val chase_var: (term -> bool) -> term -> substitution -> term
val replace: substitution -> term -> term

(* carry out a substitution in a term *)
    val subst_env : substitution ->   term -> term
    val subst : term -> term -> term -> term
(*
    val subst_mgu : (term -> bool) -> substitution ->   term -> term
*)
(* get free variables in a term (constructed with Var) *)
    val get_free_vars : term -> (Basic.fnident *gtype) list

(* get bound vars with no matching binders *)
    val get_free_binders : term -> binders list

(* destructors for bindings *)
    val get_binder : term -> q_type
    val get_binder_name : term -> string
    val get_binder_type : term -> Gtypes.gtype

    val dest_qnt :
      term -> binders * Basic.quant_ty * string * Gtypes.gtype * term

(* constructors/destructors for quanitifed terms *)
    val get_qnt_type : term -> Gtypes.gtype
    val get_qnt_body : term -> term
    val mkqnt : scope -> 
      Basic.quant_ty -> string -> term -> term
    val mktyped_qnt : scope -> 
      Basic.quant_ty -> Gtypes.gtype -> string -> term -> term

(* conversion to a string *)
    val string_typed_name : string -> Gtypes.gtype -> string
    val string_term : term -> string
    val string_inf_term : ((Basic.fnident -> int) * (Basic.fnident -> bool))
      -> term -> string
    val string_term_basic: term -> string

(* instantiate a quantified formula t with term r *)
    val inst : term -> term -> term

(* constructors/destructors/reconisers for terms *)

    val is_var : term-> bool
    val mkvar : Basic.fnident -> term
    val mkshort_var :string -> term
    val mk_typed_var : Basic.fnident -> Gtypes.gtype -> term
    val dest_var : term-> Basic.fnident * Gtypes.gtype
    val get_var_id : term-> Basic.fnident
    val get_var_type : term-> Gtypes.gtype

    val is_bound : term-> bool
    val mkbound : binders -> term
    val dest_bound : term-> binders

    val is_qnt : term-> bool

    val is_fun : term-> bool
    val mkfun : Basic.fnident -> term list -> term
    val dest_fun : term-> Basic.fnident * term list

    val is_app : term-> bool
    val mkapp : term -> term  -> term
    val dest_app : term-> term * term 
    val mkcomb: term -> term list -> term

    val is_typed : term-> bool
    val mktyped : term -> Gtypes.gtype -> term
    val dest_typed : term-> term* Gtypes.gtype

    val is_const : term-> bool
    val mkconst : Basic.const_ty -> term
    val dest_const : term-> Basic.const_ty

(*
    val mknum : int -> term
    val destnum : term-> int
*)
    val mknum : Num.num -> term
    val destnum : term -> Num.num
    val mk_int : int -> term


    val mkbool : bool -> term
    val destbool : term-> bool
    val is_true :term-> bool

(* remove outermost quantifiers from a term *)
    val strip_qnt : Basic.quant_ty -> term -> binders list * term

(* get function identifier and arguments of an application *)
    val get_args: term -> term list
    val get_fun: term -> term

(* pretty printer *)

val print_term_aux : Corepp.pp_state -> int -> term -> unit
val print_term : Corepp.pp_state -> term -> unit
val simple_term_printer: term -> unit

(* Error handling *)

    class termError : string -> term list ->
      object
      inherit Result.error 
      val trms : term list
      method get : unit -> term list
    end
    val mktermError: string -> term list -> Result.error
    val termError : string -> term list -> exn
    val addtermError : string -> term list -> exn -> 'a

(* get and set full names and types in a term *)

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
