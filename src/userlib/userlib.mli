(**----
   Name: userlib.mli
   Copyright M Wahab 2005-2014
   Author: M Wahab  <mwb.cde@gmail.com>

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

(** {5 Global state} *)

module Global: 
sig
  type state_t = Userstate.State.t

  (** Global state *)
  val state : unit -> Userstate.State.t
  val set_state: Userstate.State.t -> unit

  (** Short cut to {!Thys.get_theories.} *)
  val theories: unit -> Thydb.thydb

  (** Short cut to {!Thys.current.} *)
  val current: unit -> Theory.thy

  (** Short cut to {!Thys.current_name.} *)
  val current_name: unit -> string

  (** Access/update the global variables *)
  val context: unit -> Context.t
  val set_context: Context.t -> unit

  val scope: unit -> Scope.t
  val set_scope: Scope.t -> unit

  val simpset: unit -> Simpset.simpset
  val set_simpset: Simpset.simpset -> unit

  (** {5 Printing and Parsing}

      Global printer tables and functions to add, query and remove
      combined printer-parser information.  *)

  (** Initialise the global state. *)
  val init: unit -> unit
end

(** The user level commands.

    Commands to define theories and to carry out interactive proofs as
    well as useful functions.

    UserLib is needed to allow simplification rules to be added by
    commands such as prove_thm.

    Most commands here simply call the necessary function in a
    different module.
*)

val load_file_func: unit -> (string -> unit)
val set_load_file_func: (string -> unit) -> unit
(** Set the load file function *)

val use_file_func: unit -> (?silent:bool -> string -> unit)
val set_use_file_func: (?silent:bool -> string -> unit) -> unit
(** Set the use file function *)

val set_proof_hook: (unit -> unit) -> unit
val get_proof_hook: unit -> (unit -> unit) 
(** Get/set proof hook *)

(** {5 Utility functions} *)

val compile: string list -> string -> int
(** [compiler dirs name]: Compile file [name] with include directories
    [dirs].

    [compile ["../include"] "test.ml"] is equivalent to [Sys.system
    "ocamlc -c -I base_include -I ../include test.ml"] where
    [base_include = Settings.include_dir()].
*)
  
val catch_errors: ('a -> 'b) -> 'a -> 'b
(** Error handling. [catch_errors f a] evaluates [f a]. If an
    exception is raised, it is printed and exception [Failure] raised.
*)

(** {5 Printing and Parsing} *)

(** Infixes *)
type fixity = Printer.fixity
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

val first_pos: Theory.sym_pos
val last_pos: Theory.sym_pos
val before_pos: string -> Theory.sym_pos
val after_pos: string -> Theory.sym_pos
val at_pos: string -> Theory.sym_pos

val add_symbol: string -> string -> unit
(** [add_symbol sym tok]: Add symbol [sym] to produce lexical token [tok] *)

val add_term_pp: 
  string 
  -> ?pos:Theory.sym_pos
  -> int -> Printer.fixity -> string option -> unit
(** [add_term_pp id ?pos prec fixity sym]: Add printer-parser
    information for term identifier [id]. Parser/print term identifier
    [id] as [sym] with precedent [prec] and fixity [fixity].
*)

val get_term_pp:
  string -> (int * Printer.fixity * string option)
(** [get_term_pp id]: get printer-parser information for term
    identifier [id].
*)

val remove_term_pp: string -> unit
(** [remove_term_pp id]: remove last-added printer-parser information
    for term identifier [id].
*)

val add_type_pp: 
  string -> int -> Printer.fixity -> string option -> unit
(** [add_type_pp id prec fixity sym]: add printer-parser information
    for term identifier [id]. Parser/print term identifier [id] as
    [sym] with precedent [prec] and fixity [fixity].
*)

val get_type_pp: string -> (int * Printer.fixity * string option)
(** [get_type_pp id]: get printer-parser information for term
    identifier [id].
*)

val remove_type_pp: string -> unit
(** [remove_type_pp id]: remove last-added printer-parser information
    for term identifier [id].
*)

val read: string -> Basic.term
(** User level parsing of a string as a term. *)
val read_unchecked: string -> Basic.term
(** User level parsing of a string as a raw term.. *)
val read_defn: 
  string -> (((string * Basic.gtype) * Basic.term list) * Basic.term)
(** User level parsing of a string as a term definition. *)

val read_type: string -> Basic.gtype
(** Parse a string a type, resolving short names and symbols where
    possible.  *)
val read_type_defn : string -> Defn.Parser.typedef
(** Parse a string as a type definition. *)

val read_identifier: string -> Ident.t
(** Parse a string as an identifier. *)

(** {5 Theories} *)

val begin_theory: string -> string list -> unit
(** [begin_theory th ths]: Begin a new theory named [th] with parents
    [ths] and make it the current theory.
*)

val end_theory: ?save:bool -> unit -> unit
(** [end_theory ~save ()]: End the current theory (protect it from
    being extended) and save it to disk (if [save=true]).Calling
    [end_theory] allows the theory to be used as a parent to subsequent
    theories. [save] is [true] by default.
*)

val open_theory: string -> unit
(** [open_theory th]: Load theory [th] as the current theory, to allow
    it to be extended. Fails if the theory is protected.  [open_theory]
    Allows a theory to be defined in a series of sessions.
*)

val close_theory: unit -> unit
(** [close_theory ()]: Save the current theory to disk, but don't
    protect it. Calling [close_theory] allows the theory to be opened
    with [open_theory] but not to be a parent to a theory.
*)

(** {7 Theory properties} *)

val parents: string list -> unit
(** Add parents to the current theory, loading the parents theories if
    necessary .
*)

val add_file: ?use:bool -> string -> unit
(** [add_file ?(use=false) f]: Add file [f] to the list to be
    loaded/used when the theory is loaded. If [use=true] then also
    load/use [f] immediately.

    A file is loaded if it is a byte-code library (with suffix .cmo)
    and used otherwise (see {!Global.Files.load_use_file}).
*)

val remove_file: string -> unit
(** [remove_file f]: Remove file [f] from the list to be loaded/used
    when the theory is loaded.
*)

(** {7 Type declaration and definition} *)

val typedef:
  ?pp:(int*fixity*string option) 
  -> ?simp:bool
  -> ?thm:Logic.thm
  -> ?rep:string -> ?abs:string
  -> Defn.Parser.typedef
  -> Logic.Defns.cdefn
(** Define or declare a type. The exact behaviour of [typedef] depends
    on the form of its argument and is either a declaration, a alias
    definition or a subtype definition. In all cases, the PP
    information for the new type is taken from argument [?pp] if it is
    given.

    {b Type declaration} [typedef <:def<: ty>>]: Declare a new type
    [ty], which may optionally take arguments.

    {b Alias definition} [typedef <:def<: A=B >>]: Define type [A] as
    a synonym for type [B]. Types [A] and [B] may take arguments, but
    all variables in [B] must occur in the argument list of [A].

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

    [?simp]: (optional) whether the theorems constructed by the
    subtype package should be added to the standard simpset. Default
    [simp=true].

    {b Subtype construction}

    Assume [A = X], [B = Y], [trm = set] and [?rep = REP], [?abs = ABS].

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

val define: 
  ?pp:(int*fixity*string option) 
  -> ?simp:bool
  -> (((string * Basic.gtype) * Basic.term list) * Basic.term) 
  -> Logic.Defns.cdefn
(** [define ?simp term pp]: Define a term.

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

val declare: 
  ?pp:(int* fixity* string option) 
  -> Basic.term -> (Ident.t * Basic.gtype)
(** [declare trm pp]: Declare a term identifier.

    The term name and type is extracted from [trm] which must be a
    free variable ([Free(n, ty)]), a typed free variable
    [Typed(Free(n, _), ty)], an identifier ([Id(n, ty)]) or a typed
    identifier ([Typed(Id(n, _), ty)]).

    [?pp]: Printer-Parser information for the defined identifier.

    Returns the name [n] and type [ty] of the term.

    The parser for term definitions is {!Global.read}.
*)

(** {7 Axioms and theorems} *)

val axiom: ?simp:bool -> string -> Basic.term -> Logic.thm
(** [axiom ?simp n thm]: Assert [thm] as an axiom and add it to the
    current theory under the name [n].

    [?simp]: Whether to use the axiom as a simplifier rule (default:
    false).

    Returns the new axiom.
*)

val save_thm: ?simp:bool -> string ->  Logic.thm ->  Logic.thm
(** Store a theorem under the given name in the current theory. *)

val prove_thm: 
  ?simp:bool -> string -> Basic.term -> Tactics.tactic list -> Logic.thm
(** [prove_thm n trm tacs]: Prove theorem [trm] using the list of
    tactics [tacs] and add it to the current theory under name [n].

    The list of tactics is treated as an unstructured proof, using
    {!Goals.by_list} to prove the theorem. Use {!Commands.prove} to
    prove a theorem using a structured proof.

    [?simp]: whether to use the theorem as a simplifier rule (default:
    false).

    Returns the new theorem.

    @deprecated Use [theorem] or [lemma].
*)

val theorem: 
  ?simp:bool -> string -> Basic.term -> Tactics.tactic list -> Logic.thm
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
  ?simp:bool -> string -> Basic.term -> Tactics.tactic list -> Logic.thm
(** A synonym for {!Userlib.theorem}. *)


(** {5 Information access} *)

val theory: string -> Theory.thy
(** [theory n]: Get the theory named [n] if it is in the theory
    database, raising Not_found it it isn't. if [n=""], get the
    current theory.
*)

val theories: unit -> Thydb.thydb
(** Get the theory database. *)

val defn: string -> Logic.thm
(** [defn id]: get the definition of [id].  [id] can be a long
    identifier (of the form th.name)
*)

val thm: string -> Logic.thm
(** [thm id]: get the axiom or theorem or definition named [id].  [id]
    can be a long identifier (of the form th.name).
*)

val scope: unit -> Scope.t
(** The current scope. *)

val goal_scope: unit -> Scope.t
(** The scope of the current subgoal (if any). *)

val curr_sqnt : unit -> Logic.Sequent.t
(** The current sequent. *)
val get_asm: int -> (Tag.t * Basic.term)
(** Get an assumption from the current sequent. *)
val get_concl: int -> (Tag.t * Basic.term)
(** Get a conclusion from the current sequent. *)

val top : unit -> Goals.Proof.t
(** The current proof attempt *)

val top_goal : unit -> Logic.goal
(** The current goal. *)

val drop : unit -> Goals.Proof.t
(** Drop the current proof.  *)

val goal: Basic.term -> Goals.Proof.t
(** Start a proof attempt. Creates a goal and pushes it on the top of
    the proof stack. 

    Info: [ subgoals=[gl] ] [ cformulas=[trm] ]
    Where [gl] is the tag of the goal and [trm] the conclusion.
*)

val postpone: unit -> Goals.Proof.t
(** Postpone the current proof, pushing it to the bottom of the stack.
*)

val lift: int -> Goals.Proof.t
(** [lift n]: Focus on the nth proof to the top of the stack, making
    it the current proof attempt. Fails if there is no nth proof.
*)

val undo : unit -> Goals.Proof.t
(** Go back. Pop the top goal off the proof. Fails if there is only
    one goal in the proof.
*)

(** {5 Proof commands} 

    Note that most proof commands are in module {!Goals}.
*)

val prove: Basic.term -> Tactics.tactic -> Logic.thm
(** [prove ?scp trm tac]: Prove [trm] is a theorem using tactic [tac]
    in scope [scp]. This is a structured proof. If [scp] is not given,
    it is [scope()]. The theorem is not added to the theory.
*)

val prove_goal: 
  Basic.term -> Tactics.tactic -> Logic.thm
(** [prove_goal ?info scp trm tac]: Prove the goal formed from [trm]
    using tactic [tac] in scope [scp]. Used for batch proofs. If
    [?info] is given, the tag of the goal and conclusion ([trm]) are
    stored in it before the tactic [tac] is applied.
*)

val result: unit -> Logic.thm
(** Claim that proof is completed. Make a theorem from the proof, fail
    if the current goal has subgoals and is therefore not a theorem.
*)

val by: Tactics.tactic -> Goals.Proof.t
(** Apply a tactic to the current sub-goal in a proof attempt,
    catching and printing errors.
*)

val by_com : Tactics.tactic -> unit
(** Apply a tactic to the current goal. If the tactic succeeds, call
    [save_hook]. Used for interactive proofs.
*)

val by_list : Basic.term -> Tactics.tactic list -> Logic.thm
(** [by_list trm tacl]: Apply the list of tactics [tacl] to the
    goal formed from term [trm] in the standard scope.
*)

val qed: string -> Logic.thm
(** Declare a proof results in a theorem to be stored in the current
    theory under the given name.
*)

val apply: 
  ?report:(Logic.node -> Logic.branch -> unit) 
  -> Tactics.tactic -> Logic.goal -> Logic.goal
(** [apply ?report tac goal]: Apply tactic [tac] to [goal] using
    {!Logic.Subgoals.apply_to_goal}.

    Applies [tac] to the first subgoal [n] of [goal]. Returns the goal 
    with the subgoals [tac n] appended to the remaining subgoals of goal.
*)

(** Top-level pretty printers *)
module Display : 
sig
  val print_term: Basic.term -> unit
  val print_formula: Formula.t -> unit
  val print_type: Basic.gtype -> unit
  val print_theory: Theory.thy -> unit

  val print_sqnt: Logic.Sequent.t -> unit
  val print_node: Logic.node -> unit
  val print_branch: Logic.branch -> unit

  val print_thm: Logic.thm -> unit
  val print_defn: Logic.Defns.cdefn -> unit

  val print_prf: Goals.Proof.t -> unit
  val print_prfstk: Goals.ProofStack.t -> unit

  val print_fnident: Ident.t -> unit

  val print_subst: ('a, 'a)Hashtbl.t -> ('a -> string) -> unit
  val print_error: Report.error -> unit

  val print_simpset: Simpset.simpset -> unit
end (* Display *)


(** {5 Initialising functions} *)

val init: unit -> unit
(** Initialise the system. *)

val reset: unit -> unit
(** Reset then initialise the system. *)


(** {6 Simplifier} *)

val add_simps: Logic.thm list -> unit
(** [add_simps thms]: Add [thms] to the standard simpset. *)

val add_simp: Logic.thm -> unit
(** [add_simp thm]: Add [thm] to the standard simpset. *)

val add_conv: 
  Basic.term list -> (Context.t -> Logic.conv) -> unit
(** [add_conv trms conv]: Add conversion [conv] to the standard
    simpset, with [trms] as the representative keys.  Example:
    [add_conv [<< !x A: (%y: A) x >>] Logic.Conv.beta_conv] applies
    [beta_conv] on all terms matching [(%y: A) x].
*)

val simpA_tac:
  ?cntrl:Simplifier.control
  -> ?ignore:Logic.label list
  -> ?set:Simpset.simpset
  -> ?add:Simpset.simpset
  -> ?a:Logic.label
  -> Logic.thm list
  -> Tactics.tactic
(** [simpA_tac ?cntrl ?ignore ?asms ?set ?add ?a rules goal]
    
    Simplify assumptions.

    If [a] is not given then all assumptions are to be simplified.

    {ul
    {- Add all conclusions as simp rules.}
    {- Add all assumptions other than the targets as simp
    rules.}
    {- Simplify the assumption and then add it as a simp rule.
    Repeat for all assumptions to be simplified.}}

    Doesn't use formulas identified by a label in [ignore].
*)

val simpA:
  ?set:Simpset.simpset -> ?a:Logic.label -> Logic.thm list
  -> Tactics.tactic
(** [simp ?a]: Shorthand for {!Simplib.simpA_tac}.
    
    @raise No_change If no change is made.
*)

val simpC_tac:
  ?cntrl:Simplifier.control
  -> ?ignore:Logic.label list
  -> ?set:Simpset.simpset
  -> ?add:Simpset.simpset
  -> ?c:Logic.label
  -> Logic.thm list
  -> Tactics.tactic
(** [simpC_tac ?cntrl ?ignore ?asms ?set ?add ?c rules goal]
    
    Simplify assumptions.

    If [c] is not given then all conclusions are to be simplified.

    {ul
    {- Add all assumptions as simp rules.}
    {- Add all conclusions other than the target conclusions as simp
    rules.}
    {- Simplify the conclusions and then add it as a simp rule.
    Repeat for all assumptions to be simplified.}}

    Doesn't use formulas identified by a label in [ignore].
*)

val simpC: 
  ?set:Simpset.simpset -> ?c:Logic.label -> Logic.thm list
  ->  Tactics.tactic
(** [simp ?c]: Shorthand for {!Simplib.simpC_tac}.
    
    @raise No_change If no change is made.
*)

val simp_all_tac:
  ?cntrl:Simplifier.control
  -> ?ignore:Logic.label list
  -> ?set:Simpset.simpset
  -> ?add:Simpset.simpset
  -> Logic.thm list
  -> Tactics.tactic
(** [simp_all_tac ?cntrl ?ignore ?asms ?set ?add rules goal]
    
    Simplify each formula in the subgoal.

    {ul 
    {- Simplify each assumption, starting with the first (most recent),
    adding it to the simpset}
    {- Simplify each conclusion, starting with the last (least recent),
    adding it to the simpset.}}

    Don't use formulas identified by a label in [ignore].
*)

val simp_all: 
  ?set:Simpset.simpset -> Logic.thm list -> Tactics.tactic
(** [simp_all]: Shorthand for {!Simplib.simp_all_tac}.
    
    @raise No_change If no change is made.
*)

val simp_tac:
  ?cntrl:Simplifier.control
  -> ?ignore:Logic.label list
  -> ?set:Simpset.simpset
  -> ?add:Simpset.simpset
  -> ?f:Logic.label
  -> Logic.thm list
  -> Tactics.tactic
(** [simp_tac ?cntrl ?ignore ?asms ?set ?add ?f rules goal]
    
    Simplifier tactic.
*)

val simp: 
  ?set:Simpset.simpset -> ?f:Logic.label -> Tactics.tactic
(** [simp ?f]: Shorthand for {!Simplib.simp_tac}.
    
    @raise No_change If no change is made.
*)

