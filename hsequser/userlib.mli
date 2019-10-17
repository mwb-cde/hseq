(**----
   Name: userlib.mli
   Copyright Matthew Wahab 2005-2019
   Author: Matthew Wahab <mwb.cde@gmail.com>

   This file is part of HSeq

   HSeq is free software; you can redistribute it and/or modify it under the
   terms of the Lesser GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   HSeq is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public License for
   more details.

   You should have received a copy of the Lesser GNU General Public
   License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
   ----*)

open HSeq
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
  val ppinfo: unit -> Printers.ppinfo
  (** The global pretty printers *)
  val set_ppinfo: Printers.ppinfo -> unit
  (** Set the global pretty printers *)

  val parsers: unit -> Parser.Table.t
  (** The global parser tables *)
  val set_parsers: Parser.Table.t -> unit
  (** Set the global parser tables *)

  val thyset: unit -> Lib.StringSet.t
  val set_thyset: Lib.StringSet.t -> unit
  val thyset_add: string -> unit
  val thyset_mem: string -> bool

  val proofstack: unit -> Goals.ProofStack.t
  (** The standard proofstack *)
  val set_proofstack: Goals.ProofStack.t -> unit
  (** Set the global proofstack *)

  val path: unit -> string list
  (** The search path for theory files and libraries. *)
  val set_path: string list -> unit
  (** Set the search path for theory files and libraries. *)

  (** Initialise the global state. *)
  val init: unit -> unit
end

(** The user level commands.

    Commands to define theories and to carry out interactive proofs as
    well as useful functions.

    UserLib is needed to allow simplification rules to be added by
    commands such as prove_rule.

    Most commands here simply call the necessary function in a
    different module.
*)

val load_file_func: unit -> (string -> unit)
val set_load_file_func: (string -> unit) -> unit
(** Set the load file function *)

val use_file_func: unit -> (bool -> string -> unit)
val set_use_file_func: (bool -> string -> unit) -> unit
(** Set the use file function *)

val set_proof_hook: (unit -> unit) -> unit
val get_proof_hook: unit -> (unit -> unit)
(** Get/set proof hook *)

val path: unit -> string list
(** The search path for theory files and libraries. *)
val set_path: string list -> unit
(** Set the search path for theory files and libraries. *)
val add_to_path: string -> unit
(** Add a directory to the search path for theory files and libraries. *)

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

val first_pos: Parser.sym_pos
val last_pos: Parser.sym_pos
val before_pos: string -> Parser.sym_pos
val after_pos: string -> Parser.sym_pos
val at_pos: string -> Parser.sym_pos

val add_symbol: string -> string -> unit
(** [add_symbol sym tok]: Add symbol [sym] to produce lexical token [tok] *)

val add_term_pp:
  string -> int -> fixity -> string option -> unit
(** [add_term_pp id prec fixity sym]: Add printer-parser
    information for term identifier [id]. Parser/print term identifier
    [id] as [sym] with precedent [prec] and fixity [fixity].
*)

val get_term_pp:
  string -> (int * fixity * string option)
(** [get_term_pp id]: get printer-parser information for term
    identifier [id].
*)

val remove_term_pp: string -> unit
(** [remove_term_pp id]: remove last-added printer-parser information
    for term identifier [id].
*)

val add_type_pp:
  string -> int -> fixity -> string option -> unit
(** [add_type_pp id prec fixity sym]: add printer-parser information
    for term identifier [id]. Parser/print term identifier [id] as
    [sym] with precedent [prec] and fixity [fixity].
*)

val get_type_pp: string -> (int * fixity * string option)
(** [get_type_pp id]: get printer-parser information for term
    identifier [id].
*)

val remove_type_pp: string -> unit
(** [remove_type_pp id]: remove last-added printer-parser information
    for term identifier [id].
*)

val read: string -> Term.term
(** User level parsing of a string as a term. *)

val hterm: string -> Term.term
(** [hterm x] is [read x]. *)

val (!%): string -> Term.term
(** [!% x] is [hterm x]. *)

val read_unchecked: string -> Term.term

(** User level parsing of a string as a raw term.. *)
val read_defn:
  string -> (((string * Gtype.t) * Term.term list) * Term.term)
(** User level parsing of a string as a term definition. *)

val hdefn: string -> (((string * Gtype.t) * Term.term list) * Term.term)
(** [hdefn x] is [read_defn x]. *)

val (?<%): string -> (((string * Gtype.t) * Term.term list) * Term.term)
(** [?<% x] is [hdefn x]. *)

val read_type: string -> Gtype.t
(** Parse a string a type, resolving short names and symbols where
    possible.  *)

val htype: string -> Gtype.t
(** [htype x] is [read_type x]. *)

val (!:): string -> Gtype.t
(** [!: x] is [htype x]. *)

val read_type_defn : string -> Defn.Parser.typedef
(** Parse a string as a type definition. *)

val htype_defn: string -> Defn.Parser.typedef
(** [htype_defn x] is [read_type_defn x]. *)

val (?<:): string -> Defn.Parser.typedef
(** [?<: x] is [htype_defn x]. *)

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
    allows a theory to be defined in a series of sessions.
*)

val close_theory: unit -> unit
(** [close_theory ()]: Save the current theory to disk, but don't
    protect it. Calling [close_theory] allows the theory to be opened
    with [open_theory] but not to be a parent to a theory.
*)

(***
val load_theory: string -> unit
(** [load_theory th]: Load theory [th] as the current theory, for read-only
    access.
*)
***)

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

(** Options for definitions and declarations. Mirror those in
   Commands.Option *)

val opt_symbol: (int * fixity * (string)option) -> Commands.Option.t
val opt_simp: bool -> Commands.Option.t
val opt_repr: string -> Commands.Option.t
val opt_abs: string -> Commands.Option.t
val opt_thm: Logic.thm -> Commands.Option.t

val typedef:
  (Commands.Option.t)list -> Defn.Parser.typedef -> Logic.Defns.cdefn
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
  (Commands.Option.t)list
  -> (((string * Gtype.t) * Term.term list) * Term.term)
  -> Logic.Defns.cdefn
(** [define ?simp term pp]: Define a term.

    A term definition is of the from [define <:def< f a1 .. an = X>>].
    The term identifier is [f], the arguments (if any) are [a1 .. an]
    and (f a1 .. an) is defined by axiom as [X]. Term [X] must be
    closed, w.r.t. the arguments [a1 .. an].

    The axiom defining [f] is added to the current theory, as a
    definition, with the name [f].

    The parser for term definitions is {!Global.read_defn}.

    Options:
    [symbol]: Printer-Parser information for the defined identifier.

    [simp]: Whether to use the definition as a simplifier rule
    (default: false).
*)

val declare:
  (Commands.Option.t)list
  -> Term.term -> (Ident.t * Gtype.t)
(** [declare trm pp]: Declare a term identifier.

    The term name and type is extracted from [trm] which must be a
    free variable ([Free(n, ty)]), a typed free variable
    [Typed(Free(n, _), ty)], an identifier ([Id(n, ty)]) or a typed
    identifier ([Typed(Id(n, _), ty)]).

    Returns the name [n] and type [ty] of the term.

    The parser for term definitions is {!Global.read}.

    Options:
    [symbol]: Printer-Parser information for the defined identifier.

    [simp]: Whether to use the definition as a simplifier rule
    (default: false).
*)

(** {7 Axioms and theorems} *)

val axiom: string -> Term.term -> Logic.thm
(** [axiom ?simp n thm]: Assert [thm] as an axiom and add it to the
    current theory under the name [n].

    [?simp]: Whether to use the axiom as a simplifier rule (default:
    false).

    Returns the new axiom.
*)

val save_thm: bool -> string ->  Logic.thm ->  Logic.thm
(** Store a theorem under the given name in the current theory. *)

(*
val prove_thm: string -> Term.term -> Tactics.tactic list -> Logic.thm
val prove_rule: string -> Term.term -> Tactics.tactic list -> Logic.thm
 *)
(** [prove_thm simp n trm tacs]: Prove theorem [trm] using the list of
   tactics [tacs] and add it to the current theory under name [n].

    [[prove_rule simp n trm tacs]: Like [prove_thm] but also use the theorem
   as a simplifier rule.

    The list of tactics is treated as an unstructured proof, using
   {!Goals.by_list} to prove the theorem. Use {!Commands.prove} to prove a
   theorem using a structured proof.

    Returns the new theorem.

    @deprecated Use [theorem] or [lemma].  *)

val theorem: string -> Term.term -> Tactics.tactic list -> Logic.thm
val rule: string -> Term.term -> Tactics.tactic list -> Logic.thm
(** [theorem n trm tacs]: Prove theorem [trm] using the list of
    tactics [tacs] and add it to the current theory under name [n].

    [rule n trm tacs]: Like [theorem] but also use as a simplifier rule.

    The list of tactics is treated as an unstructured proof, using
    {!Goals.by_list} to prove the theorem. Use {!Commands.prove} to
    prove a theorem using a structured proof.

    [?simp]: whether to use the theorem as a simplifier rule (default:
    false).

    Returns the new theorem.
*)

val lemma: string -> Term.term -> Tactics.tactic list -> Logic.thm
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
val get_asm: int -> (Logic.ftag_ty * Term.term)
(** Get an assumption from the current sequent. *)
val get_concl: int -> (Logic.ftag_ty * Term.term)
(** Get a conclusion from the current sequent. *)

val top : unit -> Goals.Proof.t
(** The current proof attempt *)

val top_goal : unit -> Logic.goal
(** The current goal. *)

val drop : unit -> Goals.Proof.t
(** Drop the current proof.  *)

val goal: Term.term -> Goals.Proof.t
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

val prove: Term.term -> Tactics.tactic -> Logic.thm
(** [prove ?scp trm tac]: Prove [trm] is a theorem using tactic [tac]
    in scope [scp]. This is a structured proof. If [scp] is not given,
    it is [scope()]. The theorem is not added to the theory.
*)

val prove_goal:
  Term.term -> Tactics.tactic -> Logic.thm
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

val by_list : Term.term -> Tactics.tactic list -> Logic.thm
(** [by_list trm tacl]: Apply the list of tactics [tacl] to the
    goal formed from term [trm] in the standard scope.
*)

val qed: string -> Logic.thm
(** Declare a proof results in a theorem to be stored in the current
    theory under the given name.
*)

val apply: Tactics.tactic -> Logic.goal -> Logic.goal
(** [apply tac goal]: Apply tactic [tac] to [goal] using
    {!Logic.Subgoals.apply_to_goal}.

    Applies [tac] to the first subgoal [n] of [goal]. Returns the goal
    with the subgoals [tac n] appended to the remaining subgoals of goal.
*)

(** Top-level pretty printers *)
module Display :
sig
  val print_term: Term.term -> unit
  val print_formula: Formula.t -> unit
  val print_type: Gtype.t -> unit
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
  val print_report: int -> exn -> unit
  val print_type_error: Gtype.error -> unit

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
  Term.term list -> (Context.t -> Logic.conv) -> unit
(** [add_conv trms conv]: Add conversion [conv] to the standard
    simpset, with [trms] as the representative keys.  Example:
    [add_conv [<< !x A: (%y: A) x >>] Logic.Conv.beta_conv] applies
    [beta_conv] on all terms matching [(%y: A) x].
*)

(** {5 Tactics} *)
module Tactics: sig
  val simpA_tac:
    Simpset.simpset -> Logic.thm list -> Tactics.tactic
  val simpA_at_tac:
    Simpset.simpset -> Logic.thm list -> Logic.label -> Tactics.tactic

  (** [simpA_tac set rules]
      [simpA_at set rules a]

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

  val simpA: Logic.thm list -> Tactics.tactic
  val simpA_at: Logic.thm list -> Logic.label -> Tactics.tactic
  (** [simpA]: Shorthand for {!Simplib.simpA}

    @raise No_change If no change is made.
   *)

  val simpC_tac:
    Simpset.simpset -> Logic.thm list -> Tactics.tactic
  val simpC_at_tac:
    Simpset.simpset -> Logic.thm list -> Logic.label -> Tactics.tactic

  (** [simpC_tac set rules goal]
      [simpC_at_tac set rules c goal]

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

  val simpC: Logic.thm list ->  Tactics.tactic
  val simpC_at: Logic.thm list -> Logic.label ->  Tactics.tactic
  (** [simp]: Shorthand for {!Simplib.simpC_tac}.
      [simp_at]: Shorthand for {!Simplib.simpC_at_tac}.

    @raise No_change If no change is made.
   *)

  val simp_all_tac: Simpset.simpset -> Logic.thm list -> Tactics.tactic
  (** [simp_all_tac cntrl ignore asms set rules goal]

    Simplify each formula in the subgoal.

    {ul
    {- Simplify each assumption, starting with the first (most recent),
    adding it to the simpset}
    {- Simplify each conclusion, starting with the last (least recent),
    adding it to the simpset.}}

    Don't use formulas identified by a label in [ignore].
   *)

  val simp_all: Logic.thm list -> Tactics.tactic
  (** [simp_all]: Shorthand for {!Simplib.simp_all_tac}.

    @raise No_change If no change is made.
   *)

  val simp_tac: Simpset.simpset -> Logic.thm list -> Tactics.tactic
  (** [simp_tac cntrl ignore asms set f rules goal]

    Simplifier tactic.
   *)

  val simp: (Logic.thm)list -> Tactics.tactic
(** [simp]: Shorthand for {!Simplib.simp_tac}.

    @raise No_change If no change is made.
 *)

end
