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

(* Parsing/Printing manipulation *)

val add_pp_rec: Basic.id_selector -> Basic.ident -> Basic.PP.record -> unit
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
(* new_type "t" declares type t, 
   new_type "ty1=ty2" defines ty1 as a synonym for ty2.
*)
val new_type : string -> unit

(* new_defn/define define an identifier *)

(* [define_full str pp]
   full definition of an identifier:
   parameters in order are definition, is infix, precedence 
   and PP representation 

   [define str]
   definition of an identifier.
   
   Both return name, type and definition.
*)

val define_full : string -> (int*fixity*string option) 
  -> Defn.defn
val define : string -> Defn.defn

(* 
   [declare_full str pp]
   full declaration of identifier  
   including PP information 

   return name and type.
 *)
val declare_full : string -> (int* fixity* string option) 
  -> (Basic.ident * Gtypes.gtype)

(* 
   [declare str]
   declare an identifier 
   return name and type.
*)
val declare : string -> (Basic.ident * Gtypes.gtype)

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
