(* user level commands *) 

(* Infixes *)
type fixity = Parserkit.Info.fixity
val nonfix: fixity  (* use this as the default *)
val prefix: fixity
val suffix: fixity
val infixl: fixity  (* infix, left associative *)
val infixr: fixity  (* infix, right associative *)
val infixn: fixity  (* infix, non-associative *)

(* error handling *)
val catch_errors : ('a -> 'b) -> 'a -> 'b

(* get current theories *)
val theories : unit -> Thydb.thydb
val curr_theory : unit -> Theory.thy
val get_theory_name : 'a -> string

(** [theory n]: Get the theory named [n] if it is in the theory database.
   if [n=""], get the current theory.
*)
val theory : string -> Theory.thy

(* save/load theories *)
val save_theory : Theory.thy -> bool -> unit
val load_theory : string -> unit
val load_theory_as_cur : string -> unit

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

(* new_theory renamed to begin_theory *)

(* Parsing/Printing manipulation *)

val add_pp_rec: Basic.id_selector -> Basic.ident -> Printer.record -> unit
val add_term_pp: Basic.ident -> int -> fixity -> string option -> unit
val add_type_pp: Basic.ident -> int -> fixity -> string option -> unit

val remove_pp_rec : Basic.id_selector -> Basic.ident -> unit
val remove_term_pp : Basic.ident -> unit
val remove_type_pp : Basic.ident -> unit

val get_pp_rec : Basic.id_selector -> Basic.ident 
  -> (int * fixity * string option)
val get_term_pp : Basic.ident -> (int * fixity * string option)
val get_type_pp : Basic.ident -> (int * fixity * string option)

(* declare and define types and definitions *)
(**
   [new_type <<:! t>>] declares type t, 
   [new_type <<:! ty1=ty2 >>] declares type ty1 as a synonym for ty2 
*)
val new_type :
    ?pp:(int*fixity*string option) 
    -> (string * string list * Basic.gtype option) -> unit

(* new_defn/define define an identifier *)

(* [define term pp]
   full definition of an identifier:
   parameters in order are definition, is infix, precedence 
   and PP representation 

   return name, type and definition.
*)
val define : 
    ?pp:(int*fixity*string option) 
  -> ((string * (string * Basic.gtype) list) * Basic.term) 
  -> Defn.defn

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
   [new_axiom id thm]
   declare thm a new axiom with name id.
*)
val new_axiom : string -> Basic.term -> Logic.thm

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

(* declare a proof results in a theorem and store this theorem
   under the given name *)
val qed : string -> Logic.thm

(* prove a theorem name using the list of tactics and 
   store it under the given name *)

val prove_theorem : string -> Basic.term -> Tactics.tactic list -> Logic.thm

(*
val prove_theorem_string : string -> string -> Tactics.tactic list -> Logic.thm
*)
(* store a given theorem under the given name *)
val save_theorem : string ->  Logic.thm ->  Logic.thm

(* apply a tactic to the current sub-goal in a proof attempt *)
val by : Tactics.tactic -> Goals.prf

(* user-level function to get the current scope *)
val scope: unit -> Gtypes.scope

(* user level parsing of string *)
val read : string -> Basic.term
val read_defn : string -> ((string * (string * Basic.gtype) list) * Basic.term)
val read_unchecked : string -> Basic.term
