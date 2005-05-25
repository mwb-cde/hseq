(*-----
 Name: logicterm.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* Representation and manipulation of terms of the Logic *)
(* builds on term.ml *)

open Basic
open Gtypes
open Term

(** 
{c6 Types}
*)

(**
   Function types 

   [fun_id]: The identifier for function types.

   [is_fun_ty x]: Test whether [x] is a function.

   [mk_fun_ty x y]: Make type "x -> y".

   [mk_fun_from_list xs]: Make type "a1->(a2->.. (an-1 -> an))" 

   [dest_fun_ty x]: Destructor for function.
*)
val fun_ty_id: Basic.ident
val is_fun_ty: gtype -> bool
val mk_fun_ty : gtype -> gtype -> gtype
val mk_fun_ty_from_list: gtype list -> gtype -> gtype
val dest_fun_ty : gtype -> (gtype * gtype)

val bool_ty_id : Basic.ident
val mk_bool_ty: gtype
val is_bool_ty: gtype -> bool

(** type of primitive constructs *)
val typeof_cnst  : Basic.const_ty -> gtype
(*
val typeof_conn  : Basic.conns_ty -> gtype
*)

(** {c6 Terms} *)

(** identifiers/recognisers/constructors for basic logic functions *)
val trueid : Basic.ident
val falseid : Basic.ident
val notid : Basic.ident
val andid : Basic.ident
val orid : Basic.ident
val iffid : Basic.ident
val impliesid : Basic.ident
val equalsid : Basic.ident
val equalssym : string (* PP symbol for equals ("=") *)

val anyid: Basic.ident  (* base.any=base.epsilon(%x: true) *)

val is_true :term-> bool
val is_false : term -> bool
val is_neg: term -> bool
val is_conj: term -> bool
val is_disj: term -> bool
val is_implies: term -> bool
val is_equality: term -> bool


val mk_bool : bool -> term
val mk_true: term
val mk_false : term
val mk_not: term -> term
val mk_and: term -> term -> term
val mk_or: term -> term -> term
val mk_implies: term -> term -> term
val mk_equality: term -> term -> term
val mk_iff: term -> term -> term

val mk_all : Scope.t -> string -> term -> term
val mk_ex : Scope.t -> string -> term -> term
val mk_lam : Scope.t -> string -> term -> term
    
(* make typed quantifiers *)
val mk_all_ty : Scope.t -> string -> Basic.gtype -> term -> term
val mk_ex_ty : Scope.t -> string -> Basic.gtype -> term -> term
val mk_lam_ty : Scope.t -> string -> Basic.gtype -> term -> term

val is_all: term-> bool
val is_exists : term -> bool
val is_lambda: term-> bool

val dest_equality : term -> (term * term)
val dest_bool : term-> bool

val mk_any: term

(* equality under alpha conversion *)
(* val alpha_convp : scope -> term -> term -> bool  *)
(* alpha_equals: equality under alpha conversion 
   (renaming of alpha_convp)
 *)
val alpha_convp_full : 
    Scope.t 
  -> Gtypes.substitution -> term -> term 
    -> Gtypes.substitution
val alpha_convp : Scope.t -> term -> term -> Gtypes.substitution
val alpha_equals : Scope.t -> term -> term -> bool 
(* beta reduction *)
val beta_convp:  term -> bool
val beta_conv:  term -> term
val beta_reduce :  term -> term
(* eta abstraction *)
val eta_conv: term -> Basic.gtype -> term -> term

(* closed terms (every bound variable occurs within its binding term *)
val is_closed : term -> bool
val is_closed_scope: substitution -> term -> bool
val is_closed_aux: substitution -> term -> unit

(* close a term:
   constructs outermost universal quantifier for every
   bound variable without an enclosing binder *)

val close_term: term -> term
