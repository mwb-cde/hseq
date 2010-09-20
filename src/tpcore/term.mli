(*----
  Name: term.mli
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

(** Term representation and basic functions. *)

open Basic
open Gtypes

(** {5 Very basic operations} *)

val equals : term -> term -> bool
(** [equals s t]: Syntactic equality of terms [s] and [t]. This is
    essentially the same as [Pervasives.=] over terms except that
    references (type [binders]) are compared as first class objects
    (using [Pervasives.==]).
*)

val binder_equiv : Scope.t -> term -> term -> bool
(** [binder_equiv scp a b]: if [a] and [b] are both [Bound] or both
    [Qnt] terms then [true] if the binders of [a] and [b] are for the
    same quantifier kind and have the same type.
*)

(** {5 Data structures indexed by terms.} *)

(** {7 Balanced Trees.} *)

module TermTreeData: Treekit.TreeData
module TermTree: (Treekit.BTreeType with type key = term)

type ('a)tree = ('a)TermTree.t
(** Trees indexed by terms *)

(** {7 Hashtables} *)

type ('a)table 
(** Hashtables with a term as the key. *)

val empty_table: unit -> ('a) table
val table_find: term -> 'a table -> 'a
val table_member: term -> 'a table -> bool
val table_remove: term -> 'a table -> unit
val table_add : term -> 'a -> 'a table -> unit
val table_rebind : term -> 'a -> 'a table -> unit

(** {5 Operations on terms} *)

(** {7 Recognisers} *)

val is_qnt: term -> bool
val is_app: term -> bool
val is_bound: term -> bool
val is_free: term -> bool
val is_ident: term -> bool
val is_const: term -> bool

(** {7 Constructors} *)

val mk_qnt: binders -> term -> term
val mk_bound: binders -> term
val mk_free: string -> gtype -> term
val mk_app: term -> term -> term
val mk_const: Basic.const_ty -> term
val mk_typed_ident: Ident.t -> gtype -> term

val mk_ident: Ident.t -> term
val mk_short_ident: string -> term

(** {7 Destructors} *)

val dest_qnt: term -> (binders * term) 
val dest_bound: term -> binders
val dest_free: term -> (string * gtype)
val dest_app: term -> (term * term)
val dest_const: term -> Basic.const_ty 
val dest_ident: term -> (Ident.t * gtype)

(** {6 Specialised Manipulators} *)

(** {7 Meta variables}

    qA meta variable is a Meta with quant [Basic.Meta]. A meta
    variable is treated like a term identifier, not a bound variable.
*)

val mk_meta: string -> gtype -> term
val is_meta: term -> bool
val dest_meta: term -> binders

(** {7 Constants} *)

val destnum: term -> Num.num
val destbool: term -> bool

val mk_num: Num.num -> term
val mk_int: int -> term

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

val dest_unop: Basic.term -> (Ident.t * Basic.term)
(** [dest_unop t]: Destruct unary operator [t], return the identifier
    and argument. 

    @raise [Failure] if not enough arguments.
*)

val dest_binop: Basic.term -> (Ident.t * Basic.term * Basic.term)
(** [dest_binop t]: Destruct binary operator [t], return the
    identifier and two arguments.

    @raise [Failure] if not enough arguments.
*)

val strip_fun_qnt: 
  Ident.t -> Basic.term -> Basic.binders list 
  -> (Basic.binders list * Basic.term)
(** [strip_fun_qnt f term qs]: Strip applications of the form [f (% x:
    P)] returning the bound variables and P. ([qs] should be [[]]
    initially).
*)

(** {7 Identifier (Id) terms} *)

val get_ident_id: term-> Ident.t
val get_ident_type: term-> Basic.gtype

(** {7 Free variables} *)

val get_free_name: term-> string
val get_free_vars: term -> term list
(** Get the free variables in a term. *)

(** {7 Quantified and bound terms} *)

(** [get_binder t]: If [t] is [Qnt(q,_)] or [Bound(q)], return
    [q]. Otherwise raise [Failure].
*)
val get_binder: term -> binders

val get_binder_name: term -> string
(** [get_binder_name t]: The name of the variable bound in [t]. *)
val get_binder_type: term -> gtype
(** [get_binder_type t]: The type of the variable bound in [t]. *)
val get_binder_kind: term -> quant
(** [get_binder_kind t]: The kind of binder in [t]. *)
val get_qnt_body: term -> term
(** [get_qnt_body t]: Get the body quantified by term [t]. *)

val get_free_binders: term -> binders list
(** [get_free_binders t]: Get the list of bound variables loose in [t]
    (those which occur outside their binding term).
*)

val strip_qnt: Basic.quant -> term -> binders list * term
(** [strip_qnt q t]: remove outermost quantifiers of kind [k] from
    term [t].
*)
val rebuild_qnt: binders list -> term -> term
(** [rebuild_qnt qs t]: rebuild quantified term from quantifiers [qs]
    and body [b].
*)

(** {5 Substitution in terms} *)

val rename: term -> term
(** [rename t]: Rename bound variables in term [t] (alpha-conversion). *)

(***
val full_rename: Gtypes.substitution -> term -> (term * Gtypes.substitution)
(** [full_rename env t]: Rename all type variables and bound variables
    in term [t]. *)
***)

type substitution 
(** The type of term substitutions. *)

(** {7 Operations on a substitution} *)

val empty_subst: unit -> substitution
(** Make an empty substitution. *)
val find: term -> substitution -> term
(** [find t env]: Find term [t] in substitution [env]. *)
val bind: term -> term -> substitution -> substitution
(** [bind t r env]: Bind term [r] to term [t] in [env]. *)
val member: term -> substitution -> bool
(** [member t env]: True if term [t] has a binding in [env]. *)
val remove: term -> substitution -> substitution
(** [remove t env]: Remove the binding for term [t] in [env].  *)
val replace: substitution -> term -> term
(** [replace env t]: Replace term [t] with its binding in [env],
    renaming as necessary to ensure that binders are unique.
*)

(** {7 Substitution functions} *)

val subst: substitution -> term -> term 
(** [subst env t]: Substitute the bindings in [env] in term [t].
*)

val qsubst: (term * term) list -> term -> term
(** [qsubst_quick [(v1, r1); ..; (vn, rn)] t]: Substitute term [ri]
    for term [vi] in term [t].
*)

(** {7 Chase functions, needed for unification (some redundancy).} *)

val chase: (term -> bool) -> term -> substitution -> term
(** [chase varp t env]: Follow the chain of bindings in [env]
    beginning with term [t] and ending with the first term for which
    [varp] is false or which has no binding in [env]. ([varp] is true
    for terms which can be given a binding in [env] e.g. for
    unification.)
*)

val fullchase: (term -> bool) -> term -> substitution -> term
(** [fullchase varp t env]: chase term [t] in [env]. If [varp] is true
    for the result, return [t] otherwise return the result. This is
    like [chase], but only returns terms which aren't variable.
*)

val chase_var: (term -> bool) -> term -> substitution -> term
(** [chase_var varp t env]: Follow the chain of bindings in [env]
    beginning with term [t] and ending with the first term for which
    [varp] is false or which has no binding in [env]. ([varp] is true
    for terms which can be given a binding in [env] e.g. for
    unification.)
*)

val subst_mgu: (term -> bool) -> substitution -> term -> term 
(** [subst_mgu varp env t]: Construct the most general unifier from
    subsitution [env] and term [t]. Predicate [varp] determines which
    terms are considered variable by the unifier. This is only needed
    if variables in the unification of [x] and [y] can occur in both
    [x] and [y]. If the variables only occur in [x], then [subst env
    x] is enough.
*)

(** {6 Operations using substitution} *)

val inst: term -> term -> term
(** [inst t r]: Instantiate a quantified term [t] with term [r] *)

val mk_qnt_name: Scope.t -> Basic.quant -> string -> term -> term
(** [mk_qnt_name scp qnt n t]: Make a quantified term, with quantifier
    [qnt], from term [t], binding free variables named [n].
*)
val mk_typed_qnt_name: 
  Scope.t -> Basic.quant -> Basic.gtype -> string -> term -> term
(** [mk_typed_qnt_name scp qnt ty n t]: Make a quantified term, of
    kind [qnt], from term [t], binding all free variables named
    [n]. Set the type of the quantifier to [ty].
*)

(**  {5 Conversion of a term to a string} *)

val string_typed_name: string -> Basic.gtype -> string
val string_term: term -> string
val string_inf_term: 
  ((Ident.t -> int) * (Ident.t -> bool)) -> term -> string
val string_term_basic: term -> string

(** {5 Retyping} *)

val retype: Gtypes.substitution -> term -> term
(** [retype tyenv t]: Reset the types in term [t] using type
    substitution [tyenv].  Substitutes variables with their concrete
    type in [tyenv].
*)

val retype_with_check: Scope.t -> Gtypes.substitution -> term -> term
(** [retype_with_check scp tyenv t]: Reset the types in term [t] using
    type substitution [tyenv].  Substitutes variables with their
    concrete type in [tyenv]. Check that the new types are in scope
    [scp].
*)

val retype_pretty_env: 
  Gtypes.substitution -> term -> (term * Gtypes.substitution)
(** [retype_pretty]: Like [retype], make substitution for type
    variables but also replace other type variables with new, prettier
    names
*)
val retype_pretty: Gtypes.substitution -> term -> term 
(** [retype_pretty_env]: Like [retype_pretty] but also return the
    substitution storing from the bindings/replacements generated
    during retyping.
*)

(** {5 Pretty printing} *)

val pplookup: Printer.ppinfo -> Ident.t -> Printer.record
(** Get the printer record for a term identifier.
*)

val print_qnts: 
  Printer.ppinfo -> (Printer.fixity * int)
  -> (string * (Basic.binders list)) Printer.printer 
(** [print_qnts ppstate prec (str, qnts)]: Print binders [qnts] using
    symbol [str].
*)

val print_typed_obj:
  int 
  -> (Printer.ppinfo -> (Printer.fixity * int) -> ('a) Printer.printer)
  -> Printer.ppinfo
  -> (Printer.fixity * int)
  -> ('a * Basic.gtype) Printer.printer 
(** [print_typed_obj level printer ppstate prec (obj, ty)]: If
    [Setting.print_type_level > level] print [obj] with [ty] as its
    type in the form [(obj: ty)] otherwise print [obj] only. Uses
    [printer] to print [obj].
*)

val print_bracket:
  (Printer.fixity * int) -> (Printer.fixity * int)
  -> string Printer.printer
(** [print_bracket ppstate prec str]: Print bracket [str].
*)

val print_ident_as_identifier:
  Printer.ppinfo ->  (Printer.fixity * int) 
  -> term Printer.printer
(** [print_ident_as_identifier ppstate]: Print a [Id(id, _)] term as
    an identifier using [Printer.print_identifier ppstate].
*)

val print_infix: 
  (((Printer.fixity * int) -> Ident.t Printer.printer)
   * ((Printer.fixity * int) -> term Printer.printer))
  -> (Printer.fixity * int) 
  -> (Ident.t * (term)list) Printer.printer
(** [print_infix]: print [(f, args)] as an infix operator. *)

val print_prefix: 
  (((Printer.fixity * int) -> Ident.t Printer.printer)
   * ((Printer.fixity * int) -> term Printer.printer))
  -> (Printer.fixity * int) 
  -> (Ident.t * (term)list) Printer.printer
(**
   [print_suffix]: Print [(f, args)] as a suffix operator.
*)   
val print_suffix: 
  (((Printer.fixity * int) -> Ident.t Printer.printer)
   * ((Printer.fixity * int) -> term Printer.printer))
  -> (Printer.fixity * int) 
  -> (Ident.t * (term)list) Printer.printer
(** [print_prefix]: Print [(f, args)] as a prefix operator.  *)

val print_fn_app:
  Printer.ppinfo 
  -> (((Printer.fixity * int) -> term Printer.printer)
      * ((Printer.fixity * int) -> term Printer.printer))
  -> (Printer.fixity * int)
  -> (term * (term)list) Printer.printer
(** [print_fn_app ppstate id_printer term_printer (f, args)]: Print
    [(f, args)] as a function application.

    If there is a printer in [ppstate] for [f] then that is used
    otherwise [f] as an identifier and [term_printer] to print each of
    the [args].
*)

val simple_print_fn_app: 
  Printer.ppinfo -> (Printer.fixity * int) 
  -> (term * term list) Printer.printer
(** [simple_print_fn_app]: Print an application as [f a1 a2 .. an].

    Utility function for user defined pretty-printers. Unlike
    [print_fn_app], doesn't try to find a printer for [f] so this
    should be used as a default if a user defined printer can't be
    used.
*)

val print_term: 
  Printer.ppinfo -> (Printer.fixity * int) -> term Printer.printer
(** [print_term]: Print a term. *)

val print: Printer.ppinfo -> term -> unit
(** Toplevel for [print_term]. *)

val print_simple: term -> unit
(** [print_simple]: Simple term printer, no infixes, operator symbols
    or user defined printers. Prints a term in lisp format [(f a b c)].
*)

(** {7 Helper functions for user defined printers} *)

val print_as_binder:
  (Printer.fixity * int) -> Ident.t -> string
  -> Printer.ppinfo 
  -> (Printer.fixity * int)
  -> (term * term list) Printer.printer
(** [print_as_binder (sym_assoc, sym_prec) f sym]: Construct a printer
    to print function applications of the form [f (%x: P)] as [sym x:
    P].
*)

val print_qnt_body: 
  Printer.ppinfo -> (Printer.fixity * int)
  -> ((Basic.binders)list * Basic.term) Printer.printer
(** [print_qnt_body (assoc, prec) qs body]: Print term [body]
    quantified by variables [qs=[x1; x2; ...; xn]] as [x1 x2 ... xn:
    body].
*)

(**  {5 Error handling}  *)

class termError: string -> term list ->
object
  inherit Report.error 
  val trms: term list
  method get: unit -> term list
end
val term_error: string -> term list -> exn
val add_term_error: string -> term list -> exn -> 'a

(** {5 Comparisons} *)

val compare_term: term -> term -> int 
(** [compare_term x y]: Compare [x] and [y] using
    [Pervasives.compare].
*)
val less_than: term -> term  -> bool
(** [less_than x y]: Less-Than defined in terms of [compare_term].
*)
val least: term list -> term
(** [least ts]: The least term in [ts], using [less_than].
*)

val term_lt: term -> term -> bool
(** [term_lt]: more complex (and usefull) ordering on terms.  [Const <
    Var < Bound < App < Qnt < t2 iff t1<t2]
*)

val term_leq: term -> term -> bool
(** [term_leq]: Less-Than-or-Equal. *)

val term_gt: term -> term -> bool
(** [term_gt]: Greater-Than, defined in terms of [term_leq].
*)

val is_subterm: term -> term -> bool
(** [is_subterm x y]: Test whether [x] is a subterm of [y].
*)
