(*-----
 Name: userlib.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(**
   The user level commands.

   Commands to define theories and to carry out interactive proofs as
   well as useful functions.

   UserLib is needed to allow simplification rules to be added by commands
   such as prove_thm.

   Most commands here simply call the necessary function in a
   different module.
*)

(** {5 Utility functions} *)

val compile: string list -> string -> int
(**
   [compiler dirs name]: Compile file [name] with include directories
   [dirs].

   [compile ["../include"] "test.ml"] is equivalent to [Sys.system
   "ocamlc -c -I base_include -I ../include test.ml"] where
   [base_include = Settings.include_dir()].
*)
   
val catch_errors : ('a -> 'b) -> 'a -> 'b
(** 
   Error handling. [catch_errors f a] evaluates [f a]. If an exception
   is raised, it is printed and exception [Failure] raised.
*)

(** {5 Printing and Parsing} *)

(** Infixes *)
type fixity = Commands.fixity
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

val first_pos : Theory.sym_pos
val last_pos : Theory.sym_pos
val before_pos : string -> Theory.sym_pos
val after_pos : string -> Theory.sym_pos
val at_pos : string -> Theory.sym_pos

val add_term_pp : 
    string 
    -> ?pos:Theory.sym_pos
  -> int -> Printer.fixity -> string option -> unit
(**
   [add_term_pp id ?pos prec fixity sym]: Add printer-parser information
   for term identifier [id]. Parser/print term identifier [id] as [sym] with
   precedent [prec] and fixity [fixity].
*)

val get_term_pp : string -> (int * Printer.fixity * string option)
(**
   [get_term_pp id]: get printer-parser information for term identifier
   [id].
*)

val remove_term_pp : string -> unit
(**
   [remove_term_pp id]: remove last-added printer-parser information
   for term identifier [id].
*)

val add_type_pp : 
    string -> int -> Printer.fixity 
      -> string option -> unit
(**
   [add_type_pp id prec fixity sym]: add printer-parser information
   for term identifier [id]. Parser/print term identifier [id] as [sym] with
   precedent [prec] and fixity [fixity].
*)

val get_type_pp : string -> (int * Printer.fixity * string option)
(**
   [get_type_pp id]: get printer-parser information for term identifier
   [id].
*)

val remove_type_pp : string -> unit
(**
   [remove_type_pp id]: remove last-added printer-parser information
   for term identifier [id].
*)


(** {5 Theories} *)

val begin_theory : string -> string list -> unit
(** 
   [begin_theory th ths]: Begin a new theory named [th] with parents
   [ths] and make it the current theory.
*)

val end_theory : ?save:bool -> unit -> unit
(**
   [end_theory ~save ()]: End the current theory (protect it from
   being extended) and save it to disk (if [save=true]).Calling
   [end_theory] allows the theory to be used as a parent to subsequent
   theories. [save] is [true] by default.
*)

val open_theory : string -> unit
(**
   [open_theory th]: Load theory [th] as the current theory, to allow
   it to be extended. Fails if the theory is protected.  [open_theory]
   Allows a theory to be defined in a series of sessions.
*)

val close_theory : unit -> unit
(**
   [close_theory ()]: Save the current theory to disk, but don't protect
   it. Calling [close_theory] allows the theory to be opened with
   [open_theory] but not to be a parent to a theory.
*)

(** {7 Theory properties} *)

val parents : string list -> unit
(** 
   Add parents to the current theory, loading the parents theories if
   necessary .
*)

val add_file: ?use:bool -> string -> unit
(**
   [add_file ?(use=false) f]: Add file [f] to the list to be
   loaded/used when the theory is loaded. If [use=true] then also
   load/use [f] immediately.

   A file is loaded if it is a byte-code library (with suffix .cmo)
   and used otherwise (see {!Unsafe.load_use_file}).
*)

val remove_file: string -> unit
(**
   [remove_file f]: Remove file [f] from the list to be loaded/used
   when the theory is loaded.
*)

(** {7 Type declaration and definition} *)

val typedef:
    ?pp:(int*fixity*string option) 
    -> ?simp:bool
      -> ?thm:Logic.thm
	-> ?rep:string -> ?abs:string
	-> Parser.typedef_data
	-> Logic.Defns.cdefn
(**
   Define or declare a type. The exact behaviour of [typedef] depends
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


(** {7 Term Declaration and definition} *)


val define : 
    ?pp:(int*fixity*string option) 
  -> ?simp:bool
  -> ((string * (string * Basic.gtype) list) * Basic.term) 
  -> Logic.Defns.cdefn
(**
   [define ?simp term pp]: Define a term. 

   A term definition is of the from [define <:def< f a1 .. an = X>>].
   The term identifier is [f], the arguments (if any) are [a1 .. an]
   and (f a1 .. an) is defined by axiom as [X]. Term [X] must be
   closed, w.r.t. the arguments [a1 .. an]. 

   [?pp]: Printer-Parser information for the defined identifier.

   [?simp]: Whether to use the definition as a simplifier rule
   (default: false).

   The axiom defining [f] is added to the current theory, as a
   definition, with the name [f].

   The parser for term definitions is {!Global.read_defn}.
*)

val declare : 
    ?pp:(int* fixity* string option) 
  -> Basic.term -> (Basic.ident * Basic.gtype)
(**
   [declare trm pp]: Declare a term identifier. 

   The term name and type is extracted from [trm] which must be a free
   variable ([Free(n, ty)]), a typed free variable [Typed(Free(n, _),
   ty)], an identifier ([Id(n, ty)]) or a typed identifier
   ([Typed(Id(n, _), ty)]).

   [?pp]: Printer-Parser information for the defined identifier.

   Returns the name [n] and type [ty] of the term.

   The parser for term definitions is {!Global.read}.
 *)

(** {7 Axioms and theorems} *)

val axiom : ?simp:bool -> string -> Basic.term -> Logic.thm
(**
   [axiom ?simp n thm]: Assert [thm] as an axiom and add it to the
   current theory under the name [n].

   [?simp]: Whether to use the axiom as a simplifier rule (default: false).

   Returns the new axiom.
*)

val save_thm : ?simp:bool -> string ->  Logic.thm ->  Logic.thm
(** Store a theorem under the given name in the current theory. *)

val prove_thm : 
    ?simp:bool -> string -> Basic.term -> Tactics.tactic list -> Logic.thm
(**
   [prove_thm n trm tacs]: Prove theorem [trm] using the list of
   tactics [tacs] and add it to the current theory under name [n].

   The list of tactics is treated as an unstructured proof, using
   {!Goals.by_list} to prove the theorem. Use {!Commands.prove} to prove
   a theorem using a structured proof.

   [?simp]: whether to use the theorem as a simplifier rule (default: false).

   Returns the new theorem.

   @deprecated Use [theorem] or [lemma].
*)

val theorem: 
    ?simp:bool -> string -> Basic.term -> Tactics.tactic list -> Logic.thm
(**
   [theorem n trm tacs]: Prove theorem [trm] using the list of
   tactics [tacs] and add it to the current theory under name [n].

   The list of tactics is treated as an unstructured proof, using
   {!Goals.by_list} to prove the theorem. Use {!Commands.prove} to prove
   a theorem using a structured proof.

   [?simp]: whether to use the theorem as a simplifier rule (default: false).

   Returns the new theorem.

   A synonym for {!Commands.prove_thm}.
*)

val lemma:
    ?simp:bool -> string -> Basic.term -> Tactics.tactic list -> Logic.thm
(** A synonym for {!Userlib.theorem}. *)


(** {5 Information access} *)

val theory : string -> Theory.thy
(** 
   [theory n]: Get the theory named [n] if it is in the theory
   database, raising Not_found it it isn't. if [n=""], get the
   current theory.
*)

val theories : unit -> Thydb.thydb
(** Get the theory database. *)

val defn : string -> Logic.thm
(**
   [defn id]: get the definition of [id].
   [id] can be a long identifier (of the form th.name) 
*)

val thm : string -> Logic.thm
(**
   [thm id]: get the axiom or theorem or definition named [id].
   [id] can be a long identifier (of the form th.name).
*)

val scope: unit -> Scope.t
(** The current scope. *)

val goal_scope: unit -> Scope.t
(** The scope of the current subgoal (if any). *)


(** {5 Proof commands} 

   Note that most proof commands are in module {!Goals}.
*)

val prove: 
    ?scp:Scope.t -> Basic.term -> Tactics.tactic -> Logic.thm
(** 
   [prove ?scp trm tac]: Prove [trm] is a theorem using tactic [tac]
   in scope [scp]. This is a structured proof. If [scp] is not given,
   it is [scope()]. The theorem is not added to the theory.
*)

val by : Tactics.tactic -> Goals.Proof.t
(** 
   Apply a tactic to the current sub-goal in a proof attempt, 
   catching and printing errors.
*)

val qed : string -> Logic.thm
(** 
   Declare a proof results in a theorem to be stored in the current
   theory under the given name.
*)

(** {5 Initialising functions} *)

val init: unit -> unit
(** Initialise the system. *)

val reset: unit -> unit
(** Reset then initialise the system. *)
