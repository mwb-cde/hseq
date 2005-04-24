(*-----
 Name: userlib.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(*

   UserLib: User level commands.

   Principally to define theories and to carry out interactive proofs.

   UserLib is needed to allow simplification rules to be added by commands
   such as prove_thm. 

   Most commands here simply call the necessary function in a
   different module.
*)


(* from Commands *)

(* Infixes *)
type fixity = Commands.fixity
val nonfix: fixity  (* use this as the default *)
val prefix: fixity
val suffix: fixity
val infixl: fixity  (* infix, left associative *)
val infixr: fixity  (* infix, right associative *)
val infixn: fixity  (* infix, non-associative *)

(* error handling *)
val catch_errors : ('a -> 'b) -> 'a -> 'b

(** [theory n]: Get the theory named [n] if it is in the theory database.
   if [n=""], get the current theory.
*)
val theory : string -> Theory.thy

(* save/load theories *)
val save_theory : Theory.thy -> bool -> unit
val load_theory : string -> unit

(* begin/restart/suspend/finish a theory *)

(** 
   [begin_theory th ths]: begin new theory named [th] with parents [ths].

   [open_theory th]: load theory [th] for use as the current theory.
   This allows a theory to be defined in a series of sessions.
   [open_theory] fails if theory [th] is protected.

   [close_theory ()]: save the current theory to disk, but don't protect
   it. Calling [close_theory] allows the theory to be opened with
   [open_theory] but not to be a parent to a theory.

   [end_theory ~save ()]: end the current theory (protect it from being
   extended) and save it to disk (if [save=true]) and protect
   it. Calling [end_theory] allows the theory to be used as a parent
   to subsequent theories. [save] is [true] by default.
*)
val begin_theory : string -> string list -> unit
val open_theory : string -> unit
val close_theory : unit -> unit
val end_theory : ?save:bool -> unit -> unit


(* declare and define types and definitions *)
(**
   [new_type <<:! t>>] declares type t, 
   [new_type <<:! ty1=ty2 >>] declares type ty1 as a synonym for ty2 
*)
(*
   [typedef <:def<: t>>]: declare a new type [t], which may take arguments.

   [typedef <:def<: ty1=ty2 >>]: define type [ty1] as a synonym for
   type [ty2]. Both [ty1] and [ty2] may take arguments, but all
   variables in [ty2] occur in the argument list of [ty1].

   [typedef <:def<: ty1=ty2: trm >> ~thm ?rep ?abs ?simp]:
   define type [ty1] as a subtype of type [ty2] containing those
   elements [x:ty2] for which [trm x] is [true]. 

   Both [ty1] and [ty2] may take arguments. All variables in [ty2]
   must occur in [ty1].

   [thm]: the existance theorem for [ty1], must be in the form
   [|- ?x: trm x ]. The expression [Defn.mk_subtype_exists trm] returns the
   form that the theorem must be in for term [trm].

   [?rep], [?abs]: (optional) the names for the representation and
   abstraction functions. Default: [rep= REP_T] and [abs = ABS_T]
   where [T] is the name of the type being defined.

   [?simp]: (optional) whether the theorems constructed by the subtype package
   should be added to the standard simpset. Default [simp=true].

   Subtype construction:

   Assume [ty1 = (args) T], [ty2], [trm=set] and 
   [?rep=REP], [?abs=ABS].

   The subtype package declares two functions, [REP] and [ABS] and
   three axioms [REP_T_mem], [REP_T_inverse] and [ABS_T_inverse] and
   adds them to the current theory. If [?simp=true], the axioms are
   also added to the standard simpset.

   Declarations:
    REP:(args)T -> A
    ABS:A-> (args)T 
 
   Axioms:
    REP_T_mem: |- !x: set (REP x)
    REP_T_inverse: |- !x: ABS (REP x) = x
    ABS_T_inverse: |- !x: (set x) => (REP (ABS x) = x)
*)
val typedef:
    ?pp:(int*fixity*string option) 
    -> ?simp:bool
      -> ?thm:Logic.thm
	-> ?rep:string -> ?abs:string
	-> Parser.typedef_data
	-> Logic.cdefn


(* [define ?simp term pp]
   full definition of an identifier:
   parameters in order are definition, is infix, precedence 
   and PP representation 

   [?simp]: whether to use the definition as a simplifier rule.

   return name, type and definition.
*)
val define : 
    ?pp:(int*fixity*string option) 
  -> ?simp:bool
  -> ((string * (string * Basic.gtype) list) * Basic.term) 
  -> Logic.cdefn

(* 
   [declare trm pp]
   full declaration of identifier [trm], including PP information 

   [trm] is either a free variable ([Free(n, ty)]), a typed free variable 
   [Typed(Free(n, _), ty)], an identifier ([Id(n, ty)]) or a typed identifier 
   ([Typed(Id(n, _), ty)]). 

   returns name [n] and type [ty].
 *)
val declare : 
    ?pp:(int* fixity* string option) 
  -> Basic.term -> (Basic.ident * Basic.gtype)

(*
   [add_term_pp id prec fixity sym]: add printer-parser information
   for term identifier [id]. Parser/print term identifier [id] as [sym] with
   precedent [prec] and fixity [fixity].

   [get_term_pp id: get printer-parser information for term identifier
   [id].

   [remove_term_pp id]: remove last-added printer-parser information
   for term identifier [id].

   [add_type_pp id prec fixity sym]: add printer-parser information
   for term identifier [id]. Parser/print term identifier [id] as [sym] with
   precedent [prec] and fixity [fixity].

   [get_type_pp id: get printer-parser information for term identifier
   [id].

   [remove_type_pp id]: remove last-added printer-parser information
   for term identifier [id].

*)
val add_term_pp : 
    string -> int -> Printer.fixity 
      -> string option -> unit
val get_term_pp : 
    string -> (int * Printer.fixity * string option)
val remove_term_pp : 
    string -> unit

val add_type_pp : 
    string -> int -> Printer.fixity 
      -> string option -> unit
val get_type_pp : 
    string -> (int * Printer.fixity * string option)
val remove_type_pp : 
    string -> unit


(*
   [new_axiom ?simp id thm]
   declare thm a new axiom with name id.
*)
val new_axiom : ?simp:bool -> string -> Basic.term -> Logic.thm

(**
   [axiom/theorem/defn id] 
   get the axiom/theorem/definition named id
   id can be a long identifier (of the form th.name) 
*)
val axiom : string -> Logic.thm
val theorem : string -> Logic.thm
val defn : string -> Logic.thm

(**
   [lemma id] 
   get the axiom or theorem or definition named id
   id can be a long identifier (of the form th.name) 
*)
val lemma : string -> Logic.thm

(* declare parents of the current theory *)
val parents : string list -> unit

(**
   Add/remove load files.

   [add_file ?(use=false) f] Add file [f] to the list to be
   loaded/used when the theory is loaded.  if [use=true] then also
   load/use [f] immediately.

   [remove_file f] Remove file [f] from the list to be loaded/used
   when the theory is loaded.
*)
val add_file: ?use:bool -> string -> unit
val remove_file: string -> unit

(* declare a proof results in a theorem and store this theorem
   under the given name *)
val qed : string -> Logic.thm

(* prove a theorem name using the list of tactics and 
   store it under the given name 

   [?simp]: whether to use the theorem as a simplifier rule.
*)
val prove_thm : 
    ?simp:bool -> string -> Basic.term -> Tactics.tactic list -> Logic.thm

(* store a given theorem under the given name *)
val save_thm : ?simp:bool -> string ->  Logic.thm ->  Logic.thm

(** Get the current scope *)
val scope: unit -> Scope.t

(* apply a tactic to the current sub-goal in a proof attempt *)
val by : Tactics.tactic -> Goals.prf
