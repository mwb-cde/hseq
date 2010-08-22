(*----
  Name: lterm.mli
  Copyright M Wahab 2005-2009, 2010
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

(** Constructing and manipulating logic terms. *)

open Basic
open Gtypes
open Term

(** {5 Theories} *)

val base_thy: Ident.thy_id 
(** The name of the base theory. This is the theory at the root of the
    theory tree. The basic types and functions must be defined or
    declared in this theory. [base_thy="base"]
*)

val nums_thy: Ident.thy_id 
(** The name of the nums theory. This is the theory in which numbers
    and their operators are defined.
*)

(** {5 Types} *)

(** {7 Identifiers for base types} *)

val bool_ty_id: Ident.t
(** The identifier for the type of booleans. *)

val fun_ty_id: Ident.t
(** The identifier for the type of functions. *)

val ind_ty_id: Ident.t
(** The identifier for the ind type. *)

val num_ty_id: Ident.t
(** The identifier for the number type. *)

(** {7 The type of individuals} *)

val mk_ind_ty: unit -> gtype
(** Make an instance of the type of individuals. *)

val is_ind_ty: gtype -> bool
(** Test for an instance of the type of individuals. *)

(** {7 The type of numbers} *)

val mk_num_ty: unit -> gtype
(** Make an instance of the type of num. *)

val is_num_ty: gtype -> bool
(** Test for an instance of the type of num. *)

(** {7 The type of booleans} *)

val mk_bool_ty: unit -> gtype
(** Make an instance of the type of individuals. *)

val is_bool_ty: gtype -> bool
(** Test for an instance of the type of individuals. *)

(** {7 Function types} *)

val mk_fun_ty: gtype -> gtype -> gtype
(** [mk_fun_ty a b]: Make function type [a->b] *)

val is_fun_ty: gtype -> bool
(** Test for a function type *)

val mk_fun_ty_from_list: gtype list -> gtype -> gtype
(** 
    [mk_fun_ty_from_list [a1; a2; ...; an]]: Make type "a1->(a2-> ...
    -> an)"
*)

val dest_fun_ty: gtype -> (gtype * gtype)
(** Destructor for function types. *)

(** {7 Other types} *)

val typeof_cnst : Basic.const_ty -> gtype
(** Get the type of a primitive construct *)

(** {5 Terms} *)

(** {7 Identifiers for logic functions and constants} *)

val trueid: Ident.t
val falseid: Ident.t
val notid: Ident.t
val andid: Ident.t
val orid: Ident.t
val iffid: Ident.t
val impliesid: Ident.t
val equalsid: Ident.t
val equalssym: string
(** 
    PP symbol for equals. (Should be in some other, more appropriate, module.)
*)

val anyid: Ident.t 
(** An arbitrary choice operator, [base.any=base.epsilon(%x: true)] *)

(** {7 Recognisers} *)

val is_true:term-> bool
val is_false: term -> bool
val is_neg: term -> bool
val is_conj: term -> bool
val is_disj: term -> bool
val is_implies: term -> bool
val is_equality: term -> bool

(** {7 Constructors} *)

val mk_true: term
val mk_false: term
val mk_bool: bool -> term
val mk_not: term -> term
val mk_and: term -> term -> term
val mk_or: term -> term -> term
val mk_implies: term -> term -> term
val mk_iff: term -> term -> term
val mk_equality: term -> term -> term
val mk_any: term

(** {7 Destructors} *)

val dest_bool: term-> bool
val dest_equality: term -> (term * term)

(** {7 Quantified terms} *)

val is_all: term-> bool
(** Test for a universally quantified term. *)
val is_exists: term -> bool
(** Test for an existentially quantified term. *)
val is_lambda: term-> bool
(** Test for a lambda term. *)

val mk_all: Scope.t -> string -> term -> term
(** [mk_all scp n t]: Make a universally quantified term from [t],
    binding all free variables named [n].
*)
val mk_all_ty: Scope.t -> string -> Basic.gtype -> term -> term
(** [mk_all_ty scp n t]: Make a universally quantified term from [t],
    binding all free variables named [n] with type [ty].
*)

val mk_ex: Scope.t -> string -> term -> term
(** [mk_ex scp n t]: Make an existentially quantified term from [t],
    binding all free variables named [n].
*)
val mk_ex_ty: Scope.t -> string -> Basic.gtype -> term -> term
(** [mk_ex_ty scp n t]: Make an existentially quantified term from
    [t], binding all free variables named [n] with type [ty].
*)

val mk_lam: Scope.t -> string -> term -> term
(** [mk_lam scp n t]: Make a lambda term from [t], binding all free
    variables named [n].
*)
val mk_lam_ty: Scope.t -> string -> Basic.gtype -> term -> term
(** [mk_lam_ty scp n t]: Make a lambda term from [t], binding all free
    variables named [n] with type [ty].
*)

(** {5 Lambda Conversions} *)

(** {7 Equality under alpha-conversion} *)

val alpha_convp_full: 
  Scope.t 
  -> Gtypes.substitution -> term -> term 
  -> Gtypes.substitution
(** Test for alpha-convertiblity of terms w.r.t a type context.
    [alpha_convp_full scp tyenv x y] succeeds, returning an updated
    type context, iff [x] and [y] are equal up to the renaming of
    bound variables.
*)

val alpha_convp: Scope.t -> term -> term -> Gtypes.substitution
(** A top-level for [alpha_convp_full]. *)

val alpha_equals: Scope.t -> term -> term -> bool 
(** Test for equality modulo renaming of bound variables. This is a
    wrapper for [alpha_convp].
*)

(** {7 Beta conversion} *)

val beta_convp: term -> bool
(** Test whether a term is beta-convertible *)

val beta_conv: term -> term
(** Apply the beta-conversion rule to a term: [beta_conv ((%x. f) y)]
    is [f[y/x]]. This only reduces the top-most term: [beta_conv
    (%a. (% x. f) y)] is not reduced.
*)

val beta_reduce: term -> term
(** Apply beta-conversion through-out a term, not just the top-level.
    [beta_reduce (%a. (% x. f) y)] is [(%a. (f[y/x]))].
*)

(** {7 Eta conversion} *)

val eta_conv: term list -> term -> term 
(** [eta_conv xs term]: Apply eta-conversion.  Return [ (((% a .. b:
    term) x) .. y) ] where [xs = [ x ; .. ; y] ].
*)

(** {5 Closed terms}

    A term is closed if every bound variable occurs within its binding
    term.
*)

val is_closed_env: substitution -> Basic.term -> bool
(** [is_closed ts f] is true iff all bound variables in [f] are in the
    body of a quantifier or occur in [ts].
*)

val is_closed: Basic.term list -> Basic.term -> bool
(** [is_closed ts f] is true iff all bound variables in [f] are in the
    body of a quantifier or occur in [ts].
*)

val close_term: 
  ?qnt:Basic.quant -> ?free:(Basic.term -> bool)
  -> term -> term
(** [close_term ?qnt ?free trm]: Close term [trm]. Make variables
    bound to quantifiers of kind [qnt] to replace free variables and
    bound variables with no binding quantifier and for which [free] is
    true.

    If [?qnt] is not given, it is [Basic.All].
    If [?free] is not given, it is [(fun _ -> true)].
*)


(*** {7 Generalising terms} *)

val gen_term: Basic.binders list -> Basic.term -> Basic.term
(** [gen_term qnts trm]: generalise term [trm]. Replace bound
    variables occuring outside their binder and free variables with
    universally quantified variables.

    Variables bound with a binder in [qnts] are ignored.

    (More thorough than [close_term]).
*)

(** {5 Resolving names} *)

val in_scope: (string, bool)Lib.substype 
  -> Scope.t -> term -> bool
(** [in_scope memo spc thy t]: Check that term is in scope.  All
    identifiers and types must be declared in the given scope.  [memo]
    is used to memoise the lookup of names of free variables.
*)

val binding_set_names: 
  ?strict:bool
  -> ?memo:(string, Ident.thy_id)Hashtbl.t
  -> Scope.t
  -> Basic.binders
  -> Basic.binders
(** [binding_set_names_types ?strict ?memo scp binding] Find and set
    names for types in a binding.  If [strict=true], unknown types
    cause an error.
*)

val set_names: Scope.t  -> term -> term
(** [set_names scp t]: Get and set full identifiers in terms and and
    types of term [t].

    Each free variable in [t] with the same name as an identifier
    defined in scope [scp] is replaced by the identifier ([Id]). 

    The type of the free variable is kept as a [Typed] construct around the
    new [Id].

    Free variables which are not found in scope are left in place,
    unlike {!Lterm.resolve_term} which replaces them with new
    binders.
*)
  
val resolve_term: 
  Scope.t 
  -> Term.substitution -> (Basic.term * Basic.term) list
  -> Basic.term 
  -> (Basic.term * Term.substitution * (Basic.term * Basic.term) list)
(** [resolve_term scp vars varlist trm]: Resolve names and variables
    in term [trm].
    
    {ul
    {- Replace each free variable [Var(x, _)] in [trm] with the term
    associated with [x] in scope [scp].}
    {- Expands all type names to their long form (theory+name).}
    {- Expands all identifier terms ([Id]) to their long form
    (theory+name).}
    {- Looks up the type [ty'] of each identifier term ([Id(n,
    ty)]). 

    [vars]: an environment binding unknown variables to their
    replacements. [varlist] is the list to which to add the record
    of new variables and their bindings.

    Replaces the term with [Typed(Id(n, ty), ty')], setting
    the type [ty'] of the identifier while retaining any information
    in the given type [ty].}}

    Replaces each free or bound variable which can't be resolved
    with a universally bound variable. Returns the resolved term,
    and the unknown variables and their replacments, as a
    substitution (for the variables) and as a
    (replacement-variable) list.

    Fails if
    {ul
    {- Any type name is not declared in scope [scp].}
    {- Any identifier is not declared in [scp].}
    {- Any free variable can't be replaced with an identifier in
    scope [scp].}
    {- Any bound variable occurs outside its binding term.}}
*)

val resolve: 
  Scope.t -> Basic.term -> (Basic.term * (Basic.term * Basic.term) list)
(** [resolve scp trm]: Resolve names and variables in term [trm].
    [resolve scp trm] is {!Lterm.resolve_term} [scp (empty_subst()) []
    trm].
*)

(** {5 Substitution} *)

val subst_closed: 
  substitution -> substitution
  -> Basic.term -> Basic.term 
(** [subst_closed qntenv sb t]: Substitute the bindings in [sb] in
    term [t]. Fail, raising [Failure], if any of the substituted terms
    lead to the term not being closed.
*)

val subst_equiv: Scope.t -> term -> (term * term) list -> term
(** Substition of equivalents under alpha-conversion. [subst scp f
    [(t1, r1); ... ; (tn, rn)]]: Substitute [ri] for terms alpha-equal
    to [ti] in [f]. Slower than {!Term.subst}.
*)


