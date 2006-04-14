(*-----
 Name: term.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** Term representation and basic functions. *)

open Basic
open Gtypes

(** {5 Very basic operations} *)

val equals : term -> term -> bool
(** 
   [equals s t]: syntactic equality of terms [s] and [t]. This is
   essentially the same as [Pervasives.=] over term except that
   references (type [binders]) are compared as first class objects
   (using [Pervasives.==]). (If the OCaml [=] was the same as the SML
   [=], this function would be unnecessary.)
*)

val binder_equiv : Scope.t -> term -> term -> bool
(**
   [binder_equiv scp a b]: if [a] and [b] are both [Bound] or both
   [Qnt] terms then [true] if the binders of [a] and [b] are for the
   same quanitifier kind and have the same type.
*)

(** {5 Data structures indexed by terms.} *)

(** {7 Balanced Trees.} *)

module TermTreeData: Treekit.TreeData
module TermTree: 
    (Treekit.BTreeType with type key = term)

type ('a)tree=('a)TermTree.t
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

val is_qnt : term -> bool
val is_app : term -> bool
val is_bound: term -> bool
val is_free : term -> bool
val is_var : term -> bool
val is_typed : term -> bool
val is_const : term -> bool

(* val is_true : term -> bool *)

(** {7 Constructors} *)

val mk_qnt: binders -> term -> term
val mk_bound: binders -> term
val mk_free : string -> gtype -> term
val mk_app : term -> term -> term
val mk_typed: term -> gtype -> term
val mk_const : Basic.const_ty -> term
val mk_typed_var : Ident.t -> gtype -> term

val mk_var: Ident.t -> term
val mk_short_var: string -> term

(** {7 Destructors} *)

val dest_qnt: term -> (binders * term) 
val dest_bound: term -> binders
val dest_free :term -> (string * gtype)
val dest_app : term -> (term * term)
val dest_typed: term -> (term * gtype)
val dest_const : term -> Basic.const_ty 
val dest_var : term -> (Ident.t * gtype)

(** {6 Specialised Manipulators} *)

(** 
    {7 Meta variables} 

    A meta variable is a Meta with quant [Basic.Meta]. A meta
    variable is treated like a term identifier, not a bound variable.
*)

val mk_meta : string -> gtype -> term
val is_meta : term -> bool
val dest_meta : term -> binders

(** {7 Typed terms} *)

val strip_typed: term -> term
(** Strip the outermost [Typed] constructors from [t] *)

(** {7 Constants} *)

val destnum : term -> Num.num
val destbool : term -> bool

val mk_num: Num.num -> term
val mk_int: int -> term

(** {7 Function application} *)

val is_fun : term-> bool
(** Test for application of a function. *)

val mk_comb: term -> term list -> term
(**
   [mk_comb x y]: Make a function application from [x] and [y].
   [mk_comb f [a1;a2;...;an]] is [((((f a1) a2) ...) an)]
*)
val mk_fun : Ident.t -> term list -> term
(** [mk_fun f args]: make function application [f args]. *)

val flatten_app : term -> term list
(**
   [flatten_app trm]: flatten an application in [trm] to a list of
   terms.  [flatten_app (((f a1) a2) a3)] is [[f; a1; a2; a3]] and
   [flatten_app (((f a1) (g a2)) a3)] is [[f; a1; (g a2); a3]]
 *)

val get_fun_args: term -> (term * term list)
(** Get the function and arguments of a function application *)

val get_args: term -> term list
(** Get the arguments of a function application. *) 
val get_fun: term -> term
(** Get the function of a function application .*)
val dest_fun : term-> Ident.t * term list
(** Get the function identifier and arguments of a function application. *)

val rator: term -> term
(** 
    Get the operator of a function application. 
    [rator << f a >>] is [<< f >>].
*)
val rand: term -> term
(** 
    Get the operand of a function application. 
    [rator << f a >>] is [<< a >>].
*)

val dest_unop : Basic.term -> (Ident.t * Basic.term)
(**
   [dest_unop t]: Destruct unary operator [t], return the identifier
   and argument. Raise [Failure] if not enough arguments.
*)

val dest_binop : Basic.term -> (Ident.t * Basic.term * Basic.term)
(**
   [dest_binop t]: Destruct binary operator [t], return the identifier
   and two arguments. Raise [Failure] if not enough arguments.
*)

val strip_fun_qnt: 
    Ident.t -> Basic.term -> Basic.binders list 
      -> (Basic.binders list * Basic.term)
(** 
   [strip_fun_qnt f term qs]: strip applications of the form [f (% x:
   P)] returning the bound variables and P. ([qs] should be [[]]
   initially)
*)

(** {7 Identifier (Id) terms} *)

val get_var_id : term-> Ident.t
val get_var_type : term-> Basic.gtype

(** {7 Free variables} *)

val get_free_name : term-> string

val get_free_vars : term -> term list
(** Get the free variables in a term. *)

(** {7 Quantified and bound terms} *)

(**
   [get_binder t]: If [t] is [Qnt(q,_)] or [Bound(q)], return
   [q]. Otherwise raise [Failure].
*)
val get_binder : term -> binders

val get_binder_name : term -> string
(** [get_binder_name t]: The name of the variable bound in [t]. *)
val get_binder_type : term -> gtype
(** [get_binder_type t]: The type of the variable bound in [t]. *)
val get_binder_kind : term -> quant
(** [get_binder_kind t]: The kind of binder in [t]. *)
val get_qnt_body : term -> term
(** [get_qnt_body t]: Get the body quantified by term [t]. *)

val get_free_binders : term -> binders list
(**
   [get_free_binders t]: Get the list of bound variables loose in [t]
   (those which occur outside their binding term).
*)

val strip_qnt : Basic.quant -> term -> binders list * term
(**
   [strip_qnt q t]: remove outermost quantifiers of kind [k] from term
   [t]
*)
val rebuild_qnt: binders list -> term -> term
(**
   [rebuild_qnt qs t]: rebuild quantified term from quantifiers [qs]
   and body [b].
*)


(** {5 Substitution in terms} *)

val rename: term -> term
(** [rename t]: Rename bound variables in term [t] (alpha-conversion) *)

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

val subst : substitution -> term -> term 
(**
   [subst env t]: Substitute the bindings in [env] in term [t].
*)

val qsubst: (term * term) list -> term -> term
(**
   [qsubst_quick [(v1, r1); ..; (vn, rn)] t]: Substitute term [ri] for
   term [vi] in term [t].
*)

(** {7 Chase functions, needed for unification (some redundancy).} *)

val chase: (term -> bool) -> term -> substitution -> term
(**
   [chase varp t env]: Follow the chain of bindings in [env] beginning
   with term [t] and ending with the first term for which [varp] is
   false or which has no binding in [env]. ([varp] is true for terms
   which can be given a binding in [env] e.g. for unification.)
*)

val fullchase: 
    (term -> bool) -> term -> substitution -> term
(**
   [fullchase varp t env]: chase term [t] in [env]. If [varp] is true
   for the result, return [t] otherwise return the result. This is
   like [chase], but only returns terms which aren't variable.
*)

val chase_var: (term -> bool) -> term -> substitution -> term
(**
   [chase_var varp t env]: Follow the chain of bindings in [env]
   beginning with term [t] and ending with the first term for which
   [varp] is false or which has no binding in [env]. ([varp] is true
   for terms which can be given a binding in [env] e.g. for
   unification.)
*)

val subst_mgu: (term -> bool) -> substitution -> term -> term 
(** 
   [subst_mgu varp env t]: Construct the most general unifier from
   subsitution [env] and term [t]. Predicate [varp] determines which
   terms are considered variable by the unifier. This is only needed
   if variables in the unification of [x] and [y] can occur in both
   [x] and [y]. If the variables only occur in [x], then [subst env x]
   is enough.
*)


(** {6 Operations using substitution} *)

val inst : term -> term -> term
(** [inst t r]: instantiate a quantified term [t] with term [r] *)

val mk_qnt_name : 
    Scope.t -> Basic.quant -> string -> term -> term
(**
   [mk_qnt_name scp qnt n t]: make a quantified term, with quantifier
   [qnt], from term [t], binding free variables named [n].
*)
val mk_typed_qnt_name : 
    Scope.t -> Basic.quant -> Basic.gtype -> string -> term -> term
(**
   [mk_typed_qnt_name scp qnt ty n t]: make a quantified term, of
   kind [qnt], from term [t], binding all free variables named
   [n]. Set the type of the quantifier to [ty].
*)

(**  {5 Conversion of a term to a string} *)

val string_typed_name : string -> Basic.gtype -> string
val string_term : term -> string
val string_inf_term : 
    ((Ident.t -> int) * (Ident.t -> bool)) -> term -> string
val string_term_basic: term -> string


(** {5 Retyping} *)

val retype: Gtypes.substitution -> term -> term
(**
   [retype tyenv t]:
   Reset the types in term [t] using type substitution [tyenv].
   Substitutes variables with their concrete type in [tyenv].

   Retyping collapses terms of the form [Typed(trm, ty)] to [trm].
*)

val retype_with_check: Scope.t -> Gtypes.substitution -> term -> term
(**
   [retype_with_check scp tyenv t]: Reset the types in term [t] using type
   substitution [tyenv].  Substitutes variables with their concrete
   type in [tyenv]. Check that the new types are in scope [scp].

   Retyping collapses terms of the form [Typed(trm, ty)] to [trm].
*)


val retype_pretty_env: 
    Gtypes.substitution -> term 
      -> (term * Gtypes.substitution)
(**
   [retype_pretty]: Like [retype], make substitution for type
   variables but also replace other type variables with new, prettier
   names
*)
val retype_pretty: 
    Gtypes.substitution -> term -> term 
(**
   [retype_pretty_env]: Like [retype_pretty] but also return the
   substitution storing from the bindings/replacements generated
   during retyping.
*)

(** {5 Pretty printing} *)

val pplookup: Printer.ppinfo -> Ident.t -> Printer.record
(**
   Get the printer record for a term identifier.
*)

val print_qnts: 
    Printer.ppinfo -> (Printer.fixity * int)
      -> (string * (Basic.binders list)) Printer.printer 
(**
   [print_qnts ppstate prec (str, qnts)]: Print binders [qnts] using
   symbol [str].
*)

val print_typed_obj:
    int 
  -> (Printer.ppinfo -> (Printer.fixity * int) -> ('a) Printer.printer)
    -> Printer.ppinfo
    -> (Printer.fixity * int)
      -> ('a * Basic.gtype) Printer.printer 
(**
   [print_typed_obj level printer ppstate prec (obj, ty)]: If
   [Setting.print_type_level > level] print [obj] with [ty] as its
   type in the form [(obj: ty)] otherwise print [obj] only. Uses
   [printer] to print [obj].
*)

val print_bracket:
    (Printer.fixity * int) -> (Printer.fixity * int)
      -> string Printer.printer
(**
   [print_bracket ppstate prec str]: Print bracket [str].
*)

val print_var_as_identifier:
  Printer.ppinfo ->  (Printer.fixity * int) 
      -> term Printer.printer
(** 
   [print_var_as_identifier ppstate]: 
   Print a [Var(id, _)] term as an identifier using 
   [Printer.print_identifier ppstate].
*)

val print_infix: 
    (((Printer.fixity * int) -> Ident.t Printer.printer)
       * ((Printer.fixity * int) -> term Printer.printer))
    -> (Printer.fixity * int) 
      -> (Ident.t * (term)list) Printer.printer
(**
   [print_infix]: print [(f, args)] as an infixoperator.
*)   
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
(**
   [print_prefix]: Print [(f, args)] as a prefix operator.
*)   

val print_fn_app :
    Printer.ppinfo 
  -> (((Printer.fixity * int) -> term Printer.printer)
	* ((Printer.fixity * int) -> term Printer.printer))
    -> (Printer.fixity * int)
      -> (term * (term)list) Printer.printer
(**
   [print_fn_app ppstate id_printer term_printer (f, args)]: Print
   [(f, args)] as a function application. 

   If there is a printer in [ppstate] for [f] then that is used
   otherwise [f] as an identifier and [term_printer] to print each of
   the [args].
*)

val simple_print_fn_app: 
    Printer.ppinfo -> (Printer.fixity * int) 
      -> (term * term list) Printer.printer
(**
   [simple_print_fn_app]: Print an application as 'f a1 a2
   .. an'.

   Utility function for user defined pretty-printers. Unlike
   [print_fn_app], doesn't try to find a printer for [f] so this should
   be used as a default if a user defined printer can't be used.
*)

val print_term : 
    Printer.ppinfo -> (Printer.fixity * int) -> term Printer.printer
(**
   [print_term]: Print a term. 
*)

val print : Printer.ppinfo -> term -> unit
(** Toplevel for [print_term] *)

val print_simple: term -> unit
(**
   [print_simple]: Simple term printer, no infixes, operator symbols
   or user defined printers. Prints a term in lisp format [(f a b c)].
*)
   
(** {7 Helper functions for user defined printers} *)

val print_as_binder:
    (Printer.fixity * int) -> Ident.t -> string
      -> Printer.ppinfo 
	-> (Printer.fixity * int)
	  -> (term * term list) Printer.printer
(**
   [print_as_binder (sym_assoc, sym_prec) f sym]: Construct a printer
   to print function applications of the form [f (%x: P)] as [sym x:
   P].
*)

val print_qnt_body: 
    Printer.ppinfo -> (Printer.fixity * int)
      -> ((Basic.binders)list * Basic.term) Printer.printer
(**
   [print_qnt_body (assoc, prec) qs body]: Print term [body]
   quantified by variables [qs=[x1; x2; ...; xn]] as [x1 x2 ... xn :
   body]
*)


(**  {5 Error handling}  *)

class termError : string -> term list ->
  object
    inherit Result.error 
    val trms : term list
    method get : unit -> term list
  end
val term_error : string -> term list -> exn
val add_term_error : string -> term list -> exn -> 'a

(** {5 More operations} *)

(**
   [binding_set_names_types ?strict ?memo scp binding]
   Find and set names for types in a binding.
   If [strict=true], unknown types cause an error.
*)
val binding_set_names : 
    ?strict:bool
  -> ?memo:(string, Ident.thy_id)Hashtbl.t
    -> Scope.t
      -> Basic.binders
	-> Basic.binders

val set_names: Scope.t  -> term -> term
(**
   [set_names scp t]: Get and set full identifiers in terms and and types of
   term [t].

   Each free variable in [t] with the same name as an identifier
   defined in scope [scp] is replaced by the identifier ([Id]).  The
   type of the free variable is kept as a [Typed] construct around the
   new [Id].
*)

val in_scope: (string, bool)Lib.substype 
  -> Scope.t -> term -> bool
(**
   [in_scope memo spc thy t]: Check that term is in scope.
   All identifiers and types must be declared in the given scope.
   [memo] is used to memoise the lookup of names of free variables.
*)

val close_term: quant -> (term -> bool) -> term -> term
(**
   [close_term qnt free trm]: Close term [trm]. Make variables bound
   to quantifiers of kind [qnt] to replace free variables and bound
   variables with no binding quantifier and for which [free] is true.
 *)

val is_closed_env: substitution -> Basic.term -> bool
(**
   [is_closed ts f] is true iff all bound variables in [f] are in the
   body of a quantifier or occur in [ts].
*)

val is_closed: Basic.term list -> Basic.term -> bool
(**
   [is_closed ts f] is true iff all bound variables in [f] are in the
   body of a quantifier or occur in [ts].
*)

val subst_closed: 
  substitution
  -> substitution
    -> Basic.term 
      -> Basic.term 
(**
   [subst_closed qntenv sb t]: Substitute the bindings in [sb] in term
   [t]. Fail, raising [Failure], if any of the substituted terms lead
   to the term not being closed.
*)

val resolve_closed_term: 
  Scope.t -> Basic.term -> (Basic.term * (Basic.term * Basic.term) list)
(**
   [resolve_closed_term scp trm]: Resolve names and variables in
   term [trm].
     
   {ul
   {- Replace each free variable [Var(x, _)] in [trm] with the term
   associated with [x] in scope [scp].}
   {- Expands all type names to their long form (theory+name).}
   {- Expands all identifier terms ([Id]) to their long form
   (theory+name).}
   {- Looks up the type [ty'] of each identifier term ([Id(n,
   ty)]). Replaces the term with [Typed(Id(n, ty), ty')], setting
   the type [ty'] of the identifier while retaining any information
   in the given type [ty].}}

   Replaces each free or bound variable which can't be resolved with a
   universally bound variable. Returns the resolved term, the list of
   unknown variables and their replacments.
     

   Fails if
   {ul
   {- Any type name is not declared in scope [scp].}
   {- Any identifier is not declared in [scp].}
   {- Any free variable can't be replaced with an identifier in scope [scp].}
   {- Any bound variable occurs outside its binding term.}}
*)


(** {5 Comparisons} *)

val compare_term: term -> term -> int 
(**
   [compare_term x y]: Compare [x] and [y] using [Pervasives.compare].
*)
val less_than : term -> term  -> bool
(**
   [less_than x y]: Less-Than defined in terms of [compare_term].
*)
val least: term list -> term
(**
   [least ts]: The least term in [ts], using [less_than].
*)

val term_lt: term -> term -> bool
(**
   [term_lt]: more complex (and usefull) ordering on terms.
   [Const < Var <  Bound < App < Qnt
   (Typed t1) < t2 iff t1<t2]
*)

val term_leq: term -> term -> bool
(**
   [term_leq]: Less-Than-or-Equal.
*)

val term_gt: term -> term -> bool
(**
   [term_gt]: Greater-Than, defined in terms of [term_leq].
*)

val is_subterm : term -> term -> bool
(**
   [is_subterm x y]: Test whether [x] is a subterm of [y]. 
*)
