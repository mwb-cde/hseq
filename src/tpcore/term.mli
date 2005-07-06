(*-----
 Name: term.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** {2 Term representation and their basic manipulation} *)

open Basic
open Gtypes

(** {5 Very basic operations} *)

(** 
   [equals s t]: syntactic equality of terms [s] and [t]. This is
   essentially the same as [Pervasives.=] over term except that
   references (type [binders]) are compared as first class objects
   (using [Pervasives.==]). (If the OCaml [=] was the same as the SML
   [=], this function would be unnecessary.)
*)
val equals : term -> term -> bool

(**
   [binder_equiv scp a b]: if [a] and [b] are both [Bound] or both
   [Qnt] terms then [true] if the binders of [a] and [b] are for the
   same quanitifier kind and have the same type.
*)
val binder_equiv : Scope.t -> term -> term -> bool

(** {5 Data structures indexed by terms.} *)

(**
   [TermTree]: Trees indexed by terms
*)
module TermTreeData: Treekit.TreeData
module TermTree: 
    sig
      val eql : 'a -> 'a -> bool
      val lessthan : 'a -> 'a -> bool
      type 'a t 
      val nil : 'a t
      val create : (term * 'a) list -> 'a t -> 'a t -> 'a t
      val data : 'a t -> (term * 'a) list
      val left : 'a t -> 'a t
      val right : 'a t -> 'a t
      val balance : 'a t -> 'a t
      val add : 'a t -> term -> 'a -> 'a t
      val replace : 'a t -> term -> 'a -> 'a t
      val delete : 'a t -> term -> 'a t
      val find : 'a t -> term -> 'a
      val find_all : 'a t -> term -> 'a list
      val mem : 'a t -> term -> bool
      val iter : (term -> 'a -> 'b) -> 'a t -> unit
      val to_list : 'a t -> (term * 'a) list list
    end
type ('a)tree=('a)TermTree.t

(**
   [Termhash]: Hashtables with a term as the key

   Useful for memoising functions
*)
type ('a)table 
val empty_table: unit -> ('a) table
val table_find: term -> 'a table -> 'a
val table_member: term -> 'a table -> bool
val table_remove: term -> 'a table -> unit
val table_add : term -> 'a -> 'a table -> unit
val table_rebind : term -> 'a -> 'a table -> unit

(** {5 Operations on terms} *)

(* Recognisers *)

val is_qnt : term -> bool
val is_app : term -> bool
val is_bound: term -> bool
val is_free : term -> bool
val is_var : term -> bool
val is_typed : term -> bool
val is_const : term -> bool

(* val is_true : term -> bool *)

(* Constructors *)

val mk_qnt: binders -> term -> term
val mk_bound: binders -> term
val mk_free : string -> gtype -> term
val mk_app : term -> term -> term
val mk_typed: term -> gtype -> term
val mk_const : Basic.const_ty -> term
val mk_typed_var : ident -> gtype -> term

val mk_var: ident -> term
val mk_short_var: string -> term


(* Destructors *)

val dest_qnt: term -> (binders * term) 
val dest_bound: term -> binders
val dest_free :term -> (string * gtype)
val dest_app : term -> (term * term)
val dest_typed: term -> (term * gtype)
val dest_const : term -> Basic.const_ty 
val dest_var : term -> (ident * gtype)

(** {5 Specialised Manipulators} *)

(* Meta variables (not used) *)

val mk_meta : string -> gtype -> term
val is_meta : term -> bool

(** Typed terms *)

(** [strip_typed t]: Strip the outermost [Typed] constructors from [t] *)
val strip_typed: term -> term

(** Constants *)

val destnum : term -> Num.num
val destbool : term -> bool

val mk_num: Num.num -> term
val mk_int: int -> term


(** Function application *)

val is_fun : term-> bool

(**
   [mk_comb x y]: Make a function application from [x] and [y].
   [mk_comb f [a1;a2;...;an]] is [((((f a1) a2) ...) an)]

   [mk_fun f args]: make function application [f args].
*)
val mk_comb: term -> term list -> term
val mk_fun : Basic.ident -> term list -> term

(**
   [flatten_app trm]: flatten an application in [trm] to a list of
   terms.  [flatten_app (((f a1) a2) a3)] is [[f; a1; a2; a3]] and
   [flatten_app (((f a1) (g a2)) a3)] is [[f; a1; (g a2); a3]]
 *)
val flatten_app : term -> term list
val get_fun_args: term -> (term * term list)
val get_args: term -> term list
val get_fun: term -> term
val dest_fun : term-> Basic.ident * term list

(**
   [dest_unop t]: Destruct unary operator [t], return the identifier
   and argument.

   [dest_binop t]: Destruct binary operator [t], return the identifier
   and two arguments.

   raise [Failure] if not enough arguments.
*)
val dest_unop : Basic.term -> (Basic.ident * Basic.term)
val dest_binop : Basic.term -> (Basic.ident * Basic.term * Basic.term)

(** 
   [strip_fun_qnt f term qs]: strip applications of the form [f (% x:
   P)] returning the bound variables and P. ([qs] should be [[]]
   initially)
*)
val strip_fun_qnt: 
    Basic.ident -> Basic.term -> Basic.binders list 
      -> (Basic.binders list * Basic.term)

(** Identifier (Id) terms *)

val get_var_id : term-> Basic.ident
val get_var_type : term-> Basic.gtype

(** Free variables *)

val get_free_name : term-> string

(** [get_free_vars t]: get free variables in term [t] *)
val get_free_vars : term -> term list

(** Quantified and bound terms *)

(**
   [get_binder t]: If [t] is [Qnt(q,_)] or [Bound(q)], return
   [q]. Otherwise raise [Failure].

   [get_binder_name t]: The name of the variable bound in [t].

   [get_binder_type t]: The type of the variable bound in [t].

   [get_binder_kind t]: The kind of binder in [t].

   [get_qnt_body t]: Get the body quantified by term [t].

   [get_free_binders t]: Get the list of bound variables loose in [t]
   (those which occur outside their binding term).
*)
val get_binder : term -> binders
val get_binder_name : term -> string
val get_binder_type : term -> gtype
val get_binder_kind : term -> quant_ty
val get_qnt_body : term -> term
val get_free_binders : term -> binders list

(**
   [strip_qnt q t]: remove outermost quantifiers of kind [k] from term
   [t]

   [rebuild_qnt qs t]: rebuild quantified term from quantifiers [qs]
   and body [b].
*)
val strip_qnt : Basic.quant_ty -> term -> binders list * term
val rebuild_qnt: binders list -> term -> term


(** {5 Substitution in terms} *)

(** [rename t]: Rename bound variables in term [t] (alpha-conversion) *)
val rename: term -> term

(**
   [substitution]: the type of term substitutions.
*)
type substitution 

(** 
   Operations on a substitution 

   [empty_subst()]: Make an empty substitution.

   [find t env]: Find term [t] in substitution [env].

   [bind t r env]: Bind term [r] to term [t] in [env]

   [member t env]: True if term [t] has a binding in [env].

   [remove t env]: Remove the binding for term [t] in [env]. 

   [replace env t]: Replace term [t] with its binding in [env],
   renaming as necessary to ensure that binders are unique.
*)
val empty_subst: unit -> substitution
val find: term -> substitution -> term
val bind: term -> term -> substitution -> substitution
val member: term -> substitution -> bool
val remove: term -> substitution -> substitution
val replace: substitution -> term -> term

(**
   {6 Substitution functions}

   [subst env t]: Substitute the bindings in [env] in term [t].

   [subst_quick v r t]: Substitute term [r] for term [v] in term [t].
*)
val subst : substitution -> term -> term 
val subst_quick : term -> term -> term -> term


(**
   Chase functions, needed for unification (some redundancy).

   [chase varp t env]: Follow the chain of bindings in [env] beginning
   with term [t] and ending with the first term for which [varp] is
   false or which has no binding in [env]. ([varp] is true for terms
   which can be given a binding in [env] e.g. for unification.)

   [fullchase varp t env]: chase term [t] in [env]. If [varp] is true
   for the result, return [t] otherwise return the result. This is
   like [chase], but only returns terms which aren't variable.

   [chase_var varp t env]: Follow the chain of bindings in [env]
   beginning with term [t] and ending with the first term for which
   [varp] is false or which has no binding in [env]. ([varp] is true
   for terms which can be given a binding in [env] e.g. for
   unification.)
*)
val chase: (term -> bool) -> term -> substitution -> term
val fullchase: 
    (term -> bool) -> term -> substitution -> term
val chase_var: (term -> bool) -> term -> substitution -> term

(** 
   [subst_mgu varp env t]: Construct the most general unifier from
   subsitution [env] and term [t]. Predicate [varp] determines which
   terms are considered variable by the unifier.
*)
val subst_mgu: (term -> bool) -> substitution -> term -> term 


(** {6 Operations using substitution} *)

(** [inst t r]: instantiate a quantified term [t] with term [r] *)
val inst : term -> term -> term

(**
   [mk_qnt_name scp qnt n t]: make a quantified term, with quantifier
   [qnt], from term [t], binding free variables named [n].

   [mk_typed_qnt_name scp qnt ty n t]: make a quantified term, of
   kind [qnt], from term [t], binding all free variables named
   [n]. Set the type of the quantifier to [ty].
*)
val mk_qnt_name : 
    Scope.t -> Basic.quant_ty -> string -> term -> term
val mk_typed_qnt_name : 
    Scope.t -> Basic.quant_ty -> Basic.gtype -> string -> term -> term

(**  {5 Conversion of a term to a string} *)
val string_typed_name : string -> Basic.gtype -> string
val string_term : term -> string
val string_inf_term : 
    ((Basic.ident -> int) * (Basic.ident -> bool)) -> term -> string
val string_term_basic: term -> string


(** {5 Retyping} *)

(**
   [retype tyenv t]:
   Reset the types in term [t] using type substitution [tyenv].
   Substitutes variables with their concrete type in [tyenv].

   [retype_pretty]: Like [retype], make substitution for type
   variables but also replace other type variables with new, prettier
   names

   [retype_pretty_env]: Like [retype_pretty] but also return the
   substitution storing from the bindings/replacements generated
   during retyping.
*)
val retype: Gtypes.substitution -> term -> term
val retype_pretty_env: 
    Gtypes.substitution -> term 
      -> (term * Gtypes.substitution)
val retype_pretty: 
    Gtypes.substitution -> term 
      -> term 

(** {5 Pretty printing} *)

(**
   [pplookup info id]: Get the printer record for term identifier [id].
*)
val pplookup: Printer.ppinfo -> Basic.ident -> Printer.record

(**
   [print_qnts ppstate prec (str, qnts)]: Print binders [qnts] using
   symbol [str].

   [print_typed_obj level printer ppstate prec (obj, ty)]: If
   [Setting.print_type_level > level] print [obj] with [ty] as its
   type in the form [(obj: ty)] otherwise print [obj] only. Uses
   [printer] to print [obj].

   [print_bracket ppstate prec str]: Print bracket [str].
*)
val print_qnts: 
    Printer.ppinfo -> (Printer.fixity * int)
      -> (string * (Basic.binders list)) Printer.printer 
val print_typed_obj:
    int 
  -> (Printer.ppinfo -> (Printer.fixity * int) -> ('a) Printer.printer)
    -> Printer.ppinfo
    -> (Printer.fixity * int)
      -> ('a * Basic.gtype) Printer.printer 
val print_bracket:
    (Printer.fixity * int) -> (Printer.fixity * int)
      -> string Printer.printer

(**
   [print_infix], [print_prefix], [print_suffix]:
   print [(f, args)] as an infix, prefix or suffix operator.
*)   
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

(**
   [print_fn_app ppstate id_printer term_printer (f, args)]: Print
   [(f, args)] as a function application. 

   If there is a printer in [ppstate] for [f] then that is used
   otherwise uses [id_printer] to print [f] and [term_printer] to
   print each of the [args].
*)
val print_fn_app :
    Printer.ppinfo 
  -> (((Printer.fixity * int) -> Basic.ident Printer.printer)
	* ((Printer.fixity * int) -> term Printer.printer))
    -> (Printer.fixity * int)
      -> (Basic.ident * (term)list) Printer.printer

(**
   [simple_print_fn_app]: Print an application as 'f a1 a2
   .. an'.

   Utility function for user defined pretty-printers. Unlike
   [print_fn_app], doesn't try to find a printer for [f] so this should
   be used as a default if a user defined printer can't be used.
*)
val simple_print_fn_app: 
    Printer.ppinfo -> (Printer.fixity * int) 
      -> (Basic.ident * term list) Printer.printer

(**
   [print_term], [print]: Print a term. 

   [print_simple]: Simple term printer, no infixes, operator symbols
   or user defined printers. Prints a term in lisp format [(f a b c)].
*)
val print_term : 
    Printer.ppinfo -> (Printer.fixity * int) -> term Printer.printer

val print : Printer.ppinfo -> term -> unit
val print_simple: term -> unit
   
(** {6 Helper functions for user defined printers} *)
(**
   [print_as_binder (sym_assoc, sym_prec) f sym]: Construct a printer
   to print function applications of the form [f (%x: P)] as [sym x:
   P].
*)
val print_as_binder:
    (Printer.fixity * int) -> Basic.ident -> string
      -> Printer.ppinfo 
	-> (Printer.fixity * int)
	  -> (Basic.ident * Basic.term list) Printer.printer

(**
   [print_qnt_body (assoc, prec) qs body]: Print term [body]
   quantified by variables [qs=[x1; x2; ...; xn]] as [x1 x2 ... xn :
   body]
*)
val print_qnt_body: 
    Printer.ppinfo -> (Printer.fixity * int)
      -> ((Basic.binders)list * Basic.term) Printer.printer


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
   [set_names scp t]: Get and set full identifiers in terms and and types of
   term [t].

   Each free variable in [t] with the same name as an identifier
   defined in scope [scp] is replaced by the identifier ([Id]).  The
   type of the free variable is kept as a [Typed] construct around the
   new [Id].
*)
val set_names: Scope.t  -> term -> term

(**
   [in_scope memo spc thy t]: Check that term is in scope.
   All identifiers and types must be declared in the given scope.
   [memo] is used to memoise the lookup of names of free variables.
*)
val in_scope: (string, bool)Lib.substype 
  -> Scope.t -> Basic.thy_id -> term -> bool

(**
   [close_term qnt free trm]: Close term [trm]. Make variables bound
   to quantifiers of kind [qnt] to replace free variables and bound
   variables with no binding quantifier and for which [free] is true.
 *)
val close_term: quant_ty -> (term -> bool) -> term -> term

(** {5 Comparisons} *)

(**
   Simple ordering on terms.

   [compare_term x y]: Compare [x] and [y] using [Pervasives.compare].

   [less_than x y]: Less-Than defined in terms of [compare_term].

   [least ts]: The least term in [ts], using [less_than].
*)
val compare_term: term -> term -> int 
val less_than : term -> term  -> bool
val least: term list -> term

(**
   [term_lt]: more complex (and usefull) ordering on terms.
   [Const < Var <  Bound < App < Qnt
   (Typed t1) < t2 iff t1<t2]

   [term_leq]: Less-Than-or-Equal.

   [term_gt]: Greater-Than, defined in terms of [term_leq].
*)
val term_lt: term -> term -> bool
val term_leq: term -> term -> bool
val term_gt: term -> term -> bool

