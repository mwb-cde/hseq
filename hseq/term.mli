(*----
  Name: term.mli
  Copyright Matthew Wahab 2005-2019
  Author: Matthew Wahab <mwb.cde@gmail.com>

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

(** Term representation and basic functions. *)

open Gtype

(** {7 Terms} *)

(** Constants that can appear in terms  *)
module Const:
sig

  type t =
    | Cbool of bool

  val compare: t -> t -> Order.t
  (** Total ordering on constants. *)
  val lt: t -> t -> bool
  (** Less-than ordering on constants. *)
  val leq: t -> t -> bool
  (** Less-than-equal ordering on constants. *)
  val to_string: t -> string
  (** String representation of a constant. *)

end

(** {7 Basis of quantified terms} *)

(** Quantifiers for terms. *)
type quant =
  | All | Ex | Lambda
  | Gamma (** Meta-constants *)

val quant_string: quant -> string
(** The string representation of quantifiers. *)

(** {7 Binders} *)

module Binder:
sig

  type t
  (** Associating bound variables with their binding term. *)

  val make: quant -> string -> Gtype.t -> t
  (**
    [mk_binding k n ty] makes a binder of kind [k], with name [n] and
    type [ty]. This binder will be distinct from any other under
    [binder_equality].
   *)

  val dest: t -> (quant * string * Gtype.t)
  (** Destructor *)

  val kind_of: t -> quant
  (** [binder_kind b]: The kind of binder binding variable [b] *)

  val name_of: t -> string
  (** [binder_name b]: The name of bound variable [b] *)

  val type_of: t -> Gtype.t
  (** [type b]: The type of bound variable [b] *)

  val equality: t -> t -> bool
  (** Equality of binders *)

  val greaterthan: t -> t -> bool
  val lessthan: t -> t -> bool
  val compare: t -> t -> Order.t
(** Orderings on binders

    Maintains the invariant:
    - [binder_equality x y]
      =>
      [not (binder_lessthan x y)] and [not (binder_greaterthan x y)].
    - [binder_lessthan x y = not(binder_greaterthan x y)]
 *)

end

(** Atomic terms *)
type atom =
  | Id of Ident.t * Gtype.t
  | Free of string * Gtype.t
  | Meta of Binder.t
  | Const of Const.t

(** The representation of a term *)
type term =
  | Atom of atom
  | Bound of Binder.t
  | App of term * term
  | Qnt of Binder.t * term

(** {5 Very basic operations} *)

val equals : term -> term -> bool
(** [equals s t]: Syntactic equality of terms [s] and [t]. This is
    essentially the same as [Pervasives.=] over terms except that
    references (type [Binder.t]) are compared as first class objects
    (using [Pervasives.==]).
*)

val compare: term -> term -> Order.t
(** [compare s t]: Ordering of [s] and [t]. It is always true that [compare s t
    = 0] iff [equals s t ].

    Defines the order Const < Id < Bound < App < Qnt and includes types in the
    comparison.
 *)

(** {5 Data structures indexed by terms.} *)

(** {7 Balanced Trees.} *)

module Tree:
sig
  module TermTreeData: Treekit.TreeData
  module TermTree: (Treekit.BTreeType with type key = term)

  type ('a)t = ('a)TermTree.t
  (** Trees indexed by terms *)

  val empty: unit -> ('a)t
  val find: term -> ('a)t -> 'a
  val bind: term -> 'a -> ('a)t -> ('a)t
  val remove: term -> ('a)t -> ('a)t
  val member: term -> ('a)t -> bool
end

(** {5 Operations on terms} *)

(** {7 Recognisers} *)

val is_atom: term -> bool
val is_qnt: term -> bool
val is_app: term -> bool
val is_bound: term -> bool
val is_free: term -> bool
val is_ident: term -> bool
val is_const: term -> bool

(** {7 Constructors} *)

val mk_atom: atom -> term
val mk_qnt: Binder.t -> term -> term
val mk_bound: Binder.t -> term
val mk_free: string -> Gtype.t -> term
val mk_app: term -> term -> term
val mk_const: Const.t -> term
val mk_typed_ident: Ident.t -> Gtype.t -> term

val mk_ident: Ident.t -> term
val mk_short_ident: string -> term

(** {7 Destructors} *)
val dest_atom: term -> atom
val dest_qnt: term -> (Binder.t * term)
val dest_bound: term -> Binder.t
val dest_free: term -> (string * Gtype.t)
val dest_app: term -> (term * term)
val dest_const: term -> Const.t
val dest_ident: term -> (Ident.t * Gtype.t)

(** {6 Specialised Manipulators} *)

(** {7 Meta variables}

    qA meta variable is a Meta with quant [Term.Meta]. A meta
    variable is treated like a term identifier, not a bound variable.
*)

val mk_meta: string -> Gtype.t -> term
val is_meta: term -> bool
val dest_meta: term -> Binder.t

(** {7 Constants} *)

val destbool: term -> bool

(** {7 Function application} *)

val is_fun: term-> bool
(** Test for application of a function. *)

val mk_comb: term -> term list -> term
(** [mk_comb x y]: Make a function application from [x] and [y].
    [mk_comb f [a1;a2;...;an]] is [((((f a1) a2) ...) an).]
*)
val mk_fun: Ident.t -> term list -> term
(** [mk_fun f args]: make function application [f args]. *)

val flatten_app: term -> term list
(** [flatten_app trm]: flatten an application in [trm] to a list of
    terms.  [flatten_app (((f a1) a2) a3)] is [[f; a1; a2; a3]] and
    [flatten_app (((f a1) (g a2)) a3)] is [[f; a1; (g a2); a3]].
*)

val get_fun_args: term -> (term * term list)
(** Get the function and arguments of a function application. *)

val get_args: term -> term list
(** Get the arguments of a function application. *)
val get_fun: term -> term
(** Get the function of a function application .*)
val dest_fun: term-> Ident.t * term list
(** Get the function identifier and arguments of a function application. *)

val rator: term -> term
(** Get the operator of a function application.  [rator << f a >>] is
    [<< f >>].
*)
val rand: term -> term
(** Get the operand of a function application.  [rator << f a >>] is
    [<< a >>].
*)

val dest_unop: term -> (Ident.t * term)
(** [dest_unop t]: Destruct unary operator [t], return the identifier
    and argument.

    @raise [Failure] if not enough arguments.
*)

val dest_binop: term -> (Ident.t * term * term)
(** [dest_binop t]: Destruct binary operator [t], return the
    identifier and two arguments.

    @raise [Failure] if not enough arguments.
*)

val strip_fun_qnt:
  Ident.t -> term -> Binder.t list
  -> (Binder.t list * term)
(** [strip_fun_qnt f term qs]: Strip applications of the form [f (% x:
    P)] returning the bound variables and P. ([qs] should be [[]]
    initially).
*)

(** {7 Identifier (Id) terms} *)

val get_ident_id: term-> Ident.t
val get_ident_type: term-> Gtype.t

(** {7 Free variables} *)

val get_free_name: term-> string
val get_free_vars: term -> term list
(** Get the free variables in a term. *)

(** {7 Quantified and bound terms} *)

(** [get_binder t]: If [t] is [Qnt(q,_)] or [Bound(q)], return
    [q]. Otherwise raise [Failure].
*)
val get_binder: term -> Binder.t

val get_binder_name: term -> string
(** [get_binder_name t]: The name of the variable bound in [t]. *)
val get_binder_type: term -> Gtype.t
(** [get_binder_type t]: The type of the variable bound in [t]. *)
val get_binder_kind: term -> quant
(** [get_binder_kind t]: The kind of binder in [t]. *)
val get_qnt_body: term -> term
(** [get_qnt_body t]: Get the body quantified by term [t]. *)

val strip_qnt: quant -> term -> Binder.t list * term
(** [strip_qnt q t]: remove outermost quantifiers of kind [k] from
    term [t].
*)
val rebuild_qnt: Binder.t list -> term -> term
(** [rebuild_qnt qs t]: rebuild quantified term from quantifiers [qs]
    and body [b].
*)

(**  {5 Error handling}  *)

type error = { msg: string; terms: (term)list; next: (exn)option }
exception Error of error

val term_error: string -> term list -> exn
val add_term_error: string -> term list -> exn -> exn

(** {5 Substitution in terms} *)

val rename_opt: term -> (term) option
(** [rename_opt t] Rename bound variables in term [t] (carry out alpha
   conversion on [t]).  Return [None] if no change is needed (no binders or
   bound terms) *)

val rename: term -> term
(** [rename t]: Rename bound variables in term [t] (alpha-conversion). *)

(** {5 Retyping} *)

val retype: Gtype.Subst.t -> term -> term
(** [retype tyenv t]: Reset the types in term [t] using type
    substitution [tyenv].  Substitutes variables with their concrete
    type in [tyenv].
*)

val retype_pretty_env:
  Gtype.Subst.t -> term -> (term * Gtype.Subst.t)
(** [retype_pretty]: Like [retype], make substitution for type
    variables but also replace other type variables with new, prettier
    names
*)
val retype_pretty: Gtype.Subst.t -> term -> term
(** [retype_pretty_env]: Like [retype_pretty] but also return the
    substitution storing from the bindings/replacements generated
    during retyping.
*)

(** {5 Combined type/binder renaming} *)

val full_rename: Gtype.Subst.t -> term -> (term * Gtype.Subst.t)
(** [full_rename env t]: Rename all type variables and bound variables
    in term [t]. *)

val retype_index:
  int -> term -> (term * int * Gtype.Subst.t)
(** [retype idx t]: Rename all type variables in term [t]. *)

(** {6 Substitutions} *)
module Subst:
  sig
    type t
    (** The type of term substitutions. *)

    (** {7 Operations on a substitution} *)

    val empty: unit -> t
    (** Make an empty substitution. *)
    val find: term -> t -> term
    (** [find t env]: Find term [t] in [env]. *)
    val bind: term -> term -> t -> t
    (** [bind t r env]: Bind term [r] to term [t] in [env]. *)
    val member: term -> t -> bool
    (** [member t env]: True if term [t] has a binding in [env]. *)
    val remove: term -> t -> t
    (** [remove t env]: Remove the binding for term [t] in [env].  *)
    val replace: t -> term -> term
    (** [replace env t]: Replace term [t] with its binding in [env],
        renaming as necessary to ensure that binders are unique.
     *)

    (** {7 Chase functions, needed for unification (some redundancy).} *)

    val chase: (term -> bool) -> term -> t -> term
    (** [chase varp t env]: Follow the chain of bindings in [env]
    beginning with term [t] and ending with the first term for which
    [varp] is false or which has no binding in [env]. ([varp] is true
    for terms which can be given a binding in [env] e.g. for
    unification.)
     *)

    val fullchase: (term -> bool) -> term -> t -> term
    (** [fullchase varp t env]: chase term [t] in [env]. If [varp] is true
    for the result, return [t] otherwise return the result. This is
    like [chase], but only returns terms which aren't variable.
     *)

    val chase_var: (term -> bool) -> term -> t -> term
  (** [chase_var varp t env]: Follow the chain of bindings in [env]
    beginning with term [t] and ending with the first term for which
    [varp] is false or which has no binding in [env]. ([varp] is true
    for terms which can be given a binding in [env] e.g. for
    unification.)
   *)
  end

(** {7 Substitution functions} *)

val subst: Subst.t -> term -> term
(** [subst env t]: Substitute the bindings in [env] in term [t].
*)

val qsubst: (term * term) list -> term -> term
(** [qsubst_quick [(v1, r1); ..; (vn, rn)] t]: Substitute term [ri]
    for term [vi] in term [t].
*)

val subst_mgu: (term -> bool) -> Subst.t -> term -> term
(** [subst_mgu varp env t]: Construct the most general unifier from
    substitution [env] and term [t]. Predicate [varp] determines which
    terms are considered variable by the unifier. This is only needed
    if variables in the unification of [x] and [y] can occur in both
    [x] and [y]. If the variables only occur in [x], then [subst env
    x] is enough.
*)

(** {6 Operations using substitution} *)

val inst: term -> term -> term
(** [inst t r]: Instantiate a quantified term [t] with term [r] *)

(**  {5 Conversion of a term to a string} *)

val string_typed_name: string -> Gtype.t -> string
val string_term: term -> string
val string_inf_term:
  ((Ident.t -> int) * (Ident.t -> bool)) -> term -> string
val string_term_basic: term -> string


(** {5 Comparisons} *)

val least: term list -> term
(** [least ts]: The least term in [ts], using [term_lt].
*)

val term_lt: term -> term -> bool
(** [term_lt]: Less-Than-or-Equal. Defines the same order as [compare], but
    ignores types. *)

val term_leq: term -> term -> bool
(** [term_leq]: Less-Than-or-Equal, ignores types. *)

val term_gt: term -> term -> bool
(** [term_gt]: Greater-Than, defined in terms of [term_leq].
*)

val is_subterm: term -> term -> bool
(** [is_subterm x y]: Test whether [x] is a subterm of [y].
*)
