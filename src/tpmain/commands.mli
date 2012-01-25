(*----
  Name: commands.mli
  Copyright M Wahab 2005-2010
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

(** Theory specification.

    Functions for specifying theories, including definitions,
    declarations, axioms, theorems and their proofs and printer and
    parser information. 
*)

(** {5 Utility functions} *)

type fixity = Parserkit.Info.fixity
(** Fixity of symbols for printers and parsers. *)
val nonfix: fixity  
(** Non-fix. This is the default *)
val prefix: fixity
(** Prefix. *)
val suffix: fixity
(** Suffix. *)
val infixl: fixity  
(** Infix, left associative. *)
val infixr: fixity  
(** Infix, right associative. *)
val infixn: fixity  
(** infix, non-associative *)

val catch_errors: ('a -> 'b) -> 'a -> 'b
(** Error handling. [catch_errors f a] evaluates [(f a)] catching any
    exception [e]. If [e] is {!Report.Error} then it is printed and
    exception [Failure] is raised. If [e] is not {!Report.Error} then
    it is raised.
*)

val save_theory: Context.t -> Theory.thy -> bool -> unit
(** [save_theory thy prot]: Save theory [thy] to disk, setting its
    protection to [prot].
*)

val load_theory_as_cur: 
  Context.t -> string -> Context.t
(** [load_theory n]: Load the theory named [n] into the database,
    making it the current theory.
*)

val read: string -> Basic.term
(** User level parsing of a string as a term. *)
val read_unchecked: string -> Basic.term
(** User level parsing of a string as a raw term.. *)

val read_defn: 
  string 
  -> (((string * Basic.gtype) * Basic.term list) * Basic.term)
(** User level parsing of a string as a term definition. *)

(** {5 Theories} *)

val scope: Context.t -> Scope.t
(** Get the current scope. *)

val theories: Context.t -> Thydb.thydb
(** Get the theory database. *)

val curr_theory: Context.t -> Theory.thy
(** Get the current theory. *)

val curr_theory_name: Context.t -> string
(** Get the name of the current theory. *)

val theory: Context.t -> string -> Theory.thy
(** [theory n]: Get the theory named [n] if it is in the theory
    database, raising Not_found it it isn't. if [n=""], get the
    current theory.
*)

(** {7 Begining and Ending theories} *)

val begin_theory: 
  Context.t -> string -> string list -> Context.t
(** [begin_theory ctxt th ths]: Begin a new theory named [th] with
    parents [ths] and make it the current theory.
*)

val end_theory:
  Context.t -> ?save:bool -> unit -> Context.t
(** [end_theory ~save ()]: End the current theory (protect it from
    being extended) and save it to disk (if [save=true]).Calling
    [end_theory] allows the theory to be used as a parent to subsequent
    theories. [save] is [true] by default.
*)

val open_theory: Context.t -> string -> Context.t
(** [open_theory th]: Load theory [th] as the current theory, to allow
    it to be extended. Fails if the theory is protected.  [open_theory]
    Allows a theory to be defined in a series of sessions.
*)

val close_theory: Context.t -> unit
(** [close_theory()]: Save the current theory to disk, but don't
    protect it. Calling [close_theory] allows the theory to be opened
    with [open_theory] but not to be a parent to a theory.
*)

(** {7 Theory properties} *)

val parents: Context.t -> string list -> Context.t
(** Add parents to the current theory, loading the parents theories if
    necessary.
*)

val add_file: Context.t -> ?use:bool -> string -> unit
(** [add_file ?(use=false) f]: Add file [f] to the list to be
    loaded/used when the theory is loaded. If [use=true] then also
    load/use [f] immediately.

    A file is loaded if it is a byte-code library (with suffix .cmo)
    and used otherwise (see {!Global.Files.load_use_file}).
*)

val remove_file: Context.t -> string -> unit 
(** [remove_file f]: Remove file [f] from the list to be loaded/used
    when the theory is loaded.
*)

(** {5 Printer and Parser information} *)

(** {7 Basic PP functions} *)

val add_type_pp_rec: Context.t -> Ident.t -> Printer.record -> Context.t 
(** Add a PP record for a type identifier. Updates Printer and Parser. *)
val remove_type_pp_rec: Context.t -> Ident.t -> unit
(** Remove the PP record for a type identifier. Updates Printer and
    Parser.
*)
val get_type_pp_rec: 
  Ident.t -> (int * fixity * string option)
(** Get the PP record for a type identifier. *)

val add_term_pp_rec: 
  Context.t -> Ident.t -> ?pos:Theory.sym_pos -> Printer.record 
  -> Context.t 
(** Add a PP record for a term identifier. Updates Printer and Parser
    tables. (Experimental) Add overloading information to the parser
    with relative position [pos] (Default is [First]).
*)

val remove_term_pp_rec: Context.t -> Ident.t -> unit
(** Remove the PP record for a term identifier. Updates Printer and
    Parser tables.
*)
val get_term_pp_rec: 
  Ident.t -> (int * fixity * string option)
(** Get the PP record for a term identifier. *)

val add_overload: 
  Context.t -> string -> ?pos:Theory.sym_pos -> Ident.t -> unit
(** [add_overload sym ?post id]: Overload [sym] with term identifier
    [id]. Make identifier [id] have position [?pos] (default [First])
    in the list of options for symbol [sym]. (Experimental.)
*)
val remove_overload: string -> Ident.t -> unit
(** [remove_overload sym id]: Remove overloading of term identifier
    [id] for [sym]. (Experimental.)
*)

(** {7 Toplevel Printer and Parser information functions} *)

val add_type_pp: 
  Context.t -> Ident.t -> int -> fixity -> string option -> Context.t
(** Add a PP information for a type identifier. Updates Printer and
    Parser tables.
*)

val remove_type_pp: Context.t -> Ident.t -> unit
(** Remove PP information for a type identifier. Updates Printer and
    Parser tables.
*)

val get_type_pp: Ident.t -> (int * fixity * string option)
(** Get PP information for a type identifier. *)

val add_term_pp: 
  Context.t -> Ident.t -> ?pos:Theory.sym_pos 
  -> int -> fixity -> string option -> Context.t
(** Add a PP information for a term identifier. Updates Printer and
    Parser tables. (Experimental) Add overloading information to the
    parser.
*)

val remove_term_pp: Context.t -> Ident.t -> unit
(** Remove PP information for a term identifier. Updates Printer and
    Parser tables.
*)

val get_term_pp: Ident.t -> (int * fixity * string option)
(** Get PP information for a term identifier. *)

(** {5 Axioms and theorems} 

    Functions to add and extract theorems from the current theory. The
    main user-level functions are {!Commands.lemma}, to get a theorem
    or definition, {!Commands.axiom} to assert an axiom.

    Batch proofs are supported by {!Commands.prove_thm} to prove and
    add a theorem to a theory.

    Interactive proofs are supported by {!Commands.qed} to extract a
    theorem from a goal.
*)

val defn: Context.t -> string -> Logic.thm
(** Get a named definition from the current theory. *)

val get_theorem: Context.t -> string -> Logic.thm
(** Get a named axiom or theorem from the current theory. *)

val thm: Context.t -> string -> Logic.thm
(** [thm id]: Get the axiom or theorem or definition named [id], which
    be a long identifier (of the form [th.name])
*)

val axiom: 
  Context.t -> ?simp:bool -> string -> Basic.term 
  -> (Context.t * Logic.thm)
(** [axiom ?simp n thm]: Assert [thm] as an axiom and add it to the
    current theory under the name [n].

    [?simp]: Wether to use the axiom as a simplifier rule (default: false).

    Returns the new axiom.
*)

val prove: 
  Context.t -> Basic.term -> Logic.tactic -> Logic.thm
(** [prove ?scp trm tac]: Prove [trm] is a theorem using tactic [tac]
    in scope [scp]. This is a structured proof. If [scp] is not given,
    it is [scope()]. The theorem is not added to the theory.
*)

val save_thm: 
  Context.t -> ?simp:bool -> string ->  Logic.thm 
  -> (Context.t * Logic.thm)
(** [save_thm n thm]: Add theorem [thm] to the current theory, storing
    it under name [n].

    [?simp]: whether to use the theorem as a simplifier rule (default: false).

    Returns the theorem.
*)

val prove_thm: 
  Context.t ->
  ?simp:bool -> string -> Basic.term -> Logic.tactic list 
  -> (Context.t * Logic.thm)
(** [prove_thm n trm tacs]: Prove theorem [trm] using the list of
    tactics [tacs] and add it to the current theory under name [n].

    The list of tactics is treated as an unstructured proof, using
    {!Goals.by_list} to prove the theorem. Use {!Commands.prove} to prove
    a theorem using a structured proof.

    [?simp]: whether to use the theorem as a simplifier rule (default: false).

    Returns the new theorem.
*)

val theorem: 
  Context.t 
  -> ?simp:bool -> string -> Basic.term -> Logic.tactic list 
  -> (Context.t * Logic.thm)
(** [theorem n trm tacs]: Prove theorem [trm] using the list of
    tactics [tacs] and add it to the current theory under name [n].

    The list of tactics is treated as an unstructured proof, using
    {!Goals.by_list} to prove the theorem. Use {!Commands.prove} to
    prove a theorem using a structured proof.

    [?simp]: whether to use the theorem as a simplifier rule (default:
    false).

    Returns the new theorem.

    A synonym for {!Commands.prove_thm}.
*)

val lemma:
  Context.t 
  -> ?simp:bool -> string -> Basic.term -> Logic.tactic list 
  -> (Context.t * Logic.thm)
(** A synonym for {!Commands.theorem}. *)

val qed: string -> Logic.thm
(** Declare a proof results in a theorem and store this theorem under
    the given name.
*)

val get_or_prove: 
  Context.t 
  -> string -> Basic.term -> Logic.tactic -> Logic.thm
(** [get_or_prove n trm tacs ()]: Try to find the definition or
    theorem named [n], using {!Commands.thm}. If not found, prove
    theorem [trm] using tactic [tac]. This function allows tactic
    writers to ensure that a necessary theorem is can be rebuilt if
    necessary.
*)

(** {5 Definitions and Declarations} *)

val typedef:
  Context.t 
  -> ?pp:(int*fixity*string option) 
  -> ?simp:bool
  -> ?thm:Logic.thm
  -> ?rep:string -> ?abs:string
  -> Defn.Parser.typedef
  -> (Context.t * Logic.Defns.cdefn)

(** Define or declare a type. The exact behaviour of [typedef] depends
    on the form of its argument and is either a declaration, a alias
    definition or a subtype definition. In all cases, the PP
    information for the new type is taken from argument [?pp] if it is
    given.

    {b Type declaration} [typedef <:def<: ty>>]: Declare a new type
    [ty], which may optionally take arguments.

    {b Alias definition} [typedef <:def<: A=B >>]: Define type [A] as a
    synonym for type [B]. Types [A] and [B] may take arguments, but all
    variables in [B] must occur in the argument list of [A].

    {b Subtype definition} [typedef <:def<: A=B: trm >> ~thm ?rep ?abs
    ?simp]: Define type [A] as a subtype of type [B] containing those
    elements [x:A] for which [(trm x)] is [true]. Both [A] and [B] may
    take arguments. All variables in [A] must occur in [B].

    [thm]: the existance theorem for [A], must be in the form [|- ?x:
    trm x]. The expression [Defn.mk_subtype_exists trm] constructs the
    formula stating the existance property.

    [?rep], [?abs]: (optional) the names for the representation and
    abstraction functions. Default: [rep = REP_T] and [abs = ABS_T]
    where [T] is the name of the type being defined.

    [?simp]: (optional) whether the theorems constructed by the subtype
    package should be added to the standard simpset. Default
    [simp=true].

    {b Subtype construction}

    Assume [A = X], [B=Y], [trm=set] and [?rep=REP],
    [?abs=ABS].

    The subtype package declares two functions, [REP] and [ABS] and
    three axioms [REP_X_mem], [REP_X_inverse] and [ABS_X_inverse] and
    adds them to the current theory. If [?simp=true], the axioms are
    also added to the standard simpset.

    Function declarations:
    {ul {- [REP: X -> Y]} 
    {- [ABS: Y -> X]}}
    
    Axioms:
    {ul
    {- [REP_X_mem: |- !x: set (REP x)]}
    {- [REP_X_inverse: |- !x: ABS (REP x) = x]}
    {- [ABS_X_inverse: |- !x: (set x) => (REP (ABS x) = x)]}}

    The parser for type definitions and declarations is
    {!Global.read_type_defn}.
*)

val define: 
  Context.t 
  -> ?pp: (int * fixity * string option) 
  -> ?simp:bool
  -> (((string * Basic.gtype) * Basic.term list) * Basic.term)
  -> (Context.t * Logic.Defns.cdefn)
(**
   [define ?simp term pp]: Define a term. 

   A term definition is of the from [define <:def< f a1 .. an = X>>].
   The term identifier is [f], the arguments (if any) are [a1 .. an]
   where each [ai] is a unversally bound variable and (f a1 .. an) is
   defined by axiom as [X]. Term [X] must be closed, w.r.t. the
   arguments [a1 .. an].

   [?pp]: Printer-Parser information for the defined identifier.

   [?simp]: Whether to use the definition as a simplifier rule
   (default: false).

   The axiom defining [f] is added to the current theory, as a
   definition, with the name [f].

   The parser for term definitions is {!Global.read_defn}.
*)

val declare: 
  Context.t -> ?pp:(int* fixity* string option) 
  -> Basic.term -> (Context.t * Ident.t * Basic.gtype)
(** [declare trm pp]: Declare a term identifier.

    The term name and type is extracted from [trm] which must be a
    free variable ([Free(n, ty)]), a typed free variable
    [Typed(Free(n, _), ty)], an identifier ([Id(n, ty)]) or a typed
    identifier ([Typed(Id(n, _), ty)]).

    [?pp]: Printer-Parser information for the defined identifier.

    Returns the name [n] and type [ty] of the term.

    The parser for term definitions is {!Global.read}.
*)

(** {7 Debugging} *)

val simple_typedef: 
  Context.t 
  -> (string * string list * Basic.gtype option) 
  -> (Context.t * Logic.Defns.cdefn)
val subtypedef: 
  Context.t 
  -> (string * string list * Basic.gtype * Basic.term) 
  -> (string option * string option)
  -> ?simp:bool -> Logic.thm 
  -> (Context.t * Logic.Defns.cdefn)
