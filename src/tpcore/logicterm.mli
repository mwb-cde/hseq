(* Representation and manipulation of terms of the Logic *)
(* builds on term.ml *)

open Basic
open Gtypes
open Term

(* identifiers/recognisers/constructors for basic logic functions *)
val notid : Basic.ident
val andid : Basic.ident
val orid : Basic.ident
val iffid : Basic.ident
val impliesid : Basic.ident
val equalsid : Basic.ident
val equalssym : string (* PP symbol for equals ("=") *)

val someid: Basic.ident  (* base.some=base.epsilon(%x: x) *)

val is_neg: term -> bool
val is_conj: term -> bool
val is_disj: term -> bool
val is_implies: term -> bool
val is_equal: term -> bool

val mknot: term -> term
val mkand: term -> term -> term
val mkor: term -> term -> term
val mkimplies: term -> term -> term
val mkequal: term -> term -> term
val mkiff: term -> term -> term

val mkall : scope -> string -> term -> term
val mkex : scope -> string -> term -> term
val mklam : scope -> string -> term -> term
    
(* make typed quantifiers *)
val mkall_ty : scope -> string -> Basic.gtype -> term -> term
val mkex_ty : scope -> string -> Basic.gtype -> term -> term
val mklam_ty : scope -> string -> Basic.gtype -> term -> term

val is_all: term-> bool
val is_exists : term -> bool
val is_lambda: term-> bool

val dest_equal : term -> (term * term)

val mksome: term

(* equality under alpha conversion *)
val alpha_convp : scope -> term -> term -> bool 
(* alpha_equals: equality under alpha conversion 
   (renaming of alpha_convp)
 *)
val alpha_equals : scope -> term -> term -> bool 
(* beta reduction *)
val beta_convp:  term -> bool
val beta_conv:  term -> term
val beta_reduce :  term -> term
(* eta abstraction *)
val eta_conv: term -> Basic.gtype -> term -> term

(* closed terms (every bound variable occurs within its binding term *)
val is_closed : term -> bool
val is_closed_scope: substitution -> term -> bool

(* close a term:
   constructs outermost universal quantifier for every
   bound variable without an enclosing binder *)

val close_term: term -> term
