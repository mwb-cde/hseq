(* user level commands *) 

(* error handling *)
val catch_errors : ('a -> 'b) -> 'a -> 'b

(* get current theories *)
val theories : unit -> Thydb.thydb
val curr_theory : unit -> Theory.thy
val get_theory_name : 'a -> string

(* save/load theories *)
val save_theory : Theory.thy -> bool -> unit
val load_theory : string -> unit
val load_theory_as_cur : string -> unit

(* begin/restart/suspend/finish a theory *)
(* new_theory begins a new theory,
   open_theory loads a theory from disk 
   (allowing a theory to be defined in series of sessions)
   it fails if the theory is protected,
   close_theory saves the theory to disk but does not protect it,
   end_theory saves the theory to disk and protects it *)

val new_theory : string -> unit
val open_theory : string -> unit
val close_theory : unit -> unit
val end_theory : unit -> unit

(*
val mk_typedef_rec :
  string ->
  string list -> Gtypes.gtype option -> string list -> Gtypes.typedef_record
*)

(* declare and define types and definitions *)
(* new_type "t" declares type t, 
   new_type "ty1=ty2" defines ty1 as a synonym for ty2.
*)
val new_type : string -> unit

(* new_defn/define define an identifier *)
val new_defn : string -> string * Gtypes.gtype * Logic.thm
val define : string -> string * Gtypes.gtype * Logic.thm

(* fuller version of definiiton*)
val new_infix_defn : string -> int -> string * Gtypes.gtype * Logic.thm

(* declare an identifier *)
val new_decl : string -> string -> unit
val declare : string ->unit

(* full definition of an identifier:
   parameters in order are definition, is infix, precedence 
   and PP representation *)
val new_full_defn : string -> bool -> int -> string
  -> string * Gtypes.gtype * Logic.thm

(* full declaration of an identifier *)
val new_full_decln :
  string -> string -> bool -> int -> string -> Basic.fnident * Gtypes.gtype

(* [new_axiom id thm]
   declare thm a new axiom with name id. *)

val new_axiom : string -> string -> Logic.thm
(* [axiom/theorem/defn id] 
   get the axiom/theorem/definition named id
   id can be a long identifier (of the form th.name) *)

val axiom : string -> Logic.thm
val theorem : string -> Logic.thm
val defn : string -> Logic.thm

(* [lemma id] 
   get the axiom or theorem or definition named id
   id can be a long identifier (of the form th.name) *)

val lemma : string -> Logic.thm

(* declare parents of the current theory *)

val parents : string list -> unit

(* declare a proof results in a theorem and store this theorem
   under the given name *)
val qed : string -> Logic.thm

(* prove a theorem name using the list of tactics and 
   store it under the given name *)
val prove_theorem : string -> string -> Tactics.tactic list -> Logic.thm

(* store a given theorem under the given name *)
val save_theorem : string ->  Logic.thm ->  Logic.thm

(* apply a tactic to the current sub-goal in a proof attempt *)
val by : Tactics.tactic -> Goals.prf

(* user-level function to get the current scope *)
val scope: unit -> Gtypes.scope
