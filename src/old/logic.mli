(* theorems and rules of the logic *)
(* rules are based on a sequent calculus *)

(* tags *)
module Tags:
sig
  type tag

  val named: string->tag
  val name: tag->string
  val null:tag
  val new_tag: unit->tag

  val equal: tag->tag->bool
end

(* theorems of the logic *)
  type thm
  type saved_thm


(* sequents and goals of the sequent calculus *)

  type skolem_cnst
  type skolem_type
  type sqnt

  type goal
  type rule= goal -> goal 

(* conversion: a function from a theorem to one or more theorems *)
 type conv= thm list -> thm list

(*
   rr_type: where to get rewrite rule from
   Asm : numbered assumption
   Tagged: tagged assumption
   RRThm: given theorem
*)

type rr_type = 
   Asm of int 
   | Tagged of Tags.tag
   | RRThm of thm


(* cdefn:
   Checked Definitions: 
   checking of type and term definitions and declarations
*)

type cdefn =
    TypeDef of Basic.fnident * string list * Gtypes.gtype option
  | TermDef of Basic.fnident * Gtypes.gtype
	* (string*Gtypes.gtype) list * thm option
	 
(* theorem destructors  and constructors *)
(* (axioms do not need to be proved) *)

  val mk_axiom : Formula.form -> thm

  val dest_thm : thm -> Formula.form
  val string_thm: thm -> string

  val to_save: thm -> saved_thm
  val from_save: saved_thm -> thm

(* Sequents *)

(* skolem variables/constants are used for quantifier rules *)

  val get_sklm_name: skolem_cnst -> Basic.fnident
  val get_sklm_indx: skolem_cnst -> int
  val get_sklm_type: skolem_cnst -> Gtypes.gtype
  val get_new_sklm: Basic.fnident -> Gtypes.gtype -> skolem_type 
	-> (Term.term * skolem_type)

  val sqntError : string ->  exn
  val addsqntError : string -> exn -> 'a

(* information from a sequent *)
  val asms : sqnt -> Formula.form list
  val concls : sqnt -> Formula.form list
  val scope_of: sqnt -> Gtypes.scope
  val sklm_cnsts: sqnt -> skolem_cnst list
  val sqnt_tag: sqnt->Tags.tag

(* get/delete/copy particular assumptions/conclusions *)
  val get_asm : int -> sqnt -> Formula.form
  val get_cncl : int -> sqnt -> Formula.form

  val delete_asm : int -> Formula.form list  -> Formula.form list
  val delete_cncl : int -> Formula.form list  -> Formula.form list


(* manipulation of subgoals *)

val has_subgoals: goal -> bool
val get_sqnt:goal -> sqnt

(* make a goal from a formula  *)

(* val num_of_subsqnts: goal -> int *)
val num_of_subgoals: goal -> int

val get_subgoals: goal -> sqnt list
val get_nth_subgoal_sqnt: int -> goal-> sqnt
val goal_has_subgoals: goal -> bool

val get_subgoal_tags : goal -> Tags.tag list

(* manipulating goals *)
  val get_goal : goal -> Formula.form

(* put the tagged sqnt at the front, raise Not_found if not found *)
val goal_focus: Tags.tag->rule

val mk_goal : Gtypes.scope -> Formula.form -> goal

(* make a theorem from an established goal *)
(* only suceeds if the goal has no sub-goals *)
  val mk_thm : goal -> thm

(* errors *)

    val mklogicError: string ->Formula.form list -> Result.error
    val logicError : string -> Formula.form list -> exn
    val addlogicError : string -> Formula.form list -> exn -> 'a


module Rules:
sig

(* apply a rule to a goal *)
  val goal_apply : rule -> goal -> goal
  val goal_postpone: goal -> goal
  val postpone :  goal -> goal

(* functions for combining rules *)

(*  val foreach: rule -> rule *)
  val thenl: rule list -> rule
  val apply_list: rule list -> rule
  val repeat: rule -> rule
  val skip: rule
  val fail: rule
  val orl : rule list -> rule


(* copy_asm i: 
   .., Ai, ..|- C
   ->
   .., Ai, Ai, .. |- C*)

  val copy_asm : int -> rule

(* copy_cncl i: 
   A|- .., Ci, ..
   ->
   A|- .., Ci, Ci, ..*)

  val copy_cncl : int -> rule

(* rotate assumptions conclusions *)

(* rotate_asm
   A1, A2 .., An |-
   ->
   A2, .., An, A1 |-*)

  val rotate_asms: rule

(* rotate_cncls:
   A|- C1, C2 .., Cn
   ->
   A|-  C2, .., Cn, C1*)

  val rotate_cncls : rule

(* logic rules  *)

(* delete x sq: delete assumption (x<0) or conclusion (x>0) from sq*)
  val delete : int -> rule

(* cut x sq: adds theorem x to assumptions of sq 

   asm |- cncl      --> x, asm |- cncl
*)

  val cut : thm -> rule


(* unify i j sq: if asm i unifies  with cncl j of sq, 
   then result is the theorem concl j 

   asm, a_{i}, asm' |- concl, c_{j}, concl' 
   -->
   true if a_{i}=c_{j}
*)

  val unify : int -> int -> rule

(* 
conjI i sq: 
   asm |- a /\ b, concl    tag(t)
   -->
   asm |- a                tag(t1)
   and asm |- b            tag(t2)
*)

  val conjI: int -> rule

(* 
conjE i sq: 
   a/\ b, asm |- concl   
   -->
   a, b, asm |- concl 
*)

  val conjE: int -> rule

(*
 disjI i sq: 
  a\/b, asm |-  concl   tag(t)
   -->
   a, asm |- concl      tag(t1)
   and b, asm |- concl  tag(t2)
 *)

  val disjI: int -> rule

(* disjE i sq: 
   asm |- a\/b, concl   
   -->
   asm |- a, b, concl *)

  val disjE: int -> rule

(* negA i sq:
   ~a, asms |- concl
   -->
   asms |- a, concl
*)

  val negA: int -> rule

(* negC i sq:
   asms |- ~c, concl
   -->
   c, asms |- concl
*)

  val negC: int -> rule

(* implI i sq
   asms |- a-> b, cncl 
   -->
   a, asms |- b, cncl
*)

  val implI: int -> rule

(* implE i sq
   a-> b,asms |-cncl  tag(t)
   -->
   asms |- a, cncl    tag(t1)
   and 
   b, asms |- cncl    tag(t2)
*)

  val implE: int -> rule

(* allI i sq
   asm |- !x. P(c), concl
   -->
   asm |- P(c'), concl   where c' is a new identifier
*)

  val allI : int -> rule

(* allE i sq
  !x. P(c), asm |-  concl
   -->
   P(c'), asm |- concl   where c' is a given term
*)

  val allE : Term.term -> int -> rule

(* existI i sq
   ?x. P(c), asm |- concl
   -->
   P(c'), asm |- concl   where c' is a new identifier
*)
  val existI : int -> rule

(* existE i sq
   asm |- ?x. P(c), concl
   -->
   asm |- P(c'), concl   where c' is a given term
*)

  val existE : Term.term -> int -> rule

(* beta i sq:  (beta reduction of asm (i<0) or concl (i>0) in sq) *)
  val beta: int -> rule

(* trueR i sq
   asm |- true, concl
   --> true
*)

  val trueR: int -> rule

(* name_rule: introduce a new name in the sqnt as a synonym for a term  *)
(* name id trm:
  Asm|-Cncl -> id=trm, Asm|-Cncl

the long name thy.id must be unique (where thy is the current theory name)

*)

val name_rule: string -> Term.term -> rule



(* rewrite_any dir thms j sq:
   list of theorems or assumptions containing x=y
   asm |- P(x), concl
   -->
   asm |- P(y), concl
  where dir is =true for right-left and false for left-right
  theorems must be in scope.
  silently discards theorems not in scope and assumptions which don't exist
*)


val rewrite_any: ?dir:bool -> rr_type list -> int -> rule

(* rewrite dir i j  sq
   x=y, asm |- P(x), concl
   -->
   x=y, asm |- P(y), concl
where dir is =true for right-left and false for left-right
*)

  val rewrite : ?dir:bool -> int list -> int -> rule

(* rewrite_thms dir thms j sq:
   list of theorems containing x=y
   asm |- P(x), concl
   -->
   asm |- P(y), concl
  where dir is =true for right-left and false for left-right
  theorems must be in scope.
*)

val rewrite_thms: ?dir:bool -> thm list -> int -> rule


end

open Rules

module ThmRules:
sig

(* functions for manipulating theorems (alpha quality) *)

(* conversions which apply to only one theorem 
   (e.g. conjE_conv [|- a and b ] -> [|- a; |- b])
*)
val conjE_conv: conv
val allI_conv: Gtypes.scope -> Term.term -> conv
val eta_conv: Gtypes.scope -> Formula.form -> conv
val beta_conv: Gtypes.scope -> conv

(* conversions which apply to all theorems in the list *)

(* rewrite_conv: apply rewrite, fail if any rewrite fails *)
val rewrite_conv: Gtypes.scope -> ?dir:bool-> thm list -> conv

(* rewriting with nets in subgoals in theorems *)
(*
type rulesDB 

val empty_db: Gtypes.scope -> rulesDB
val add: Gtypes.scope -> bool -> thm list -> rulesDB -> rulesDB
val rescope_db: Gtypes.scope -> Basic.thy_id -> rulesDB -> rulesDB

val rewrite_net_conv: Gtypes.scope -> rulesDB -> thm -> thm
val rewrite_net : rulesDB -> int -> rule
*)
end

module Defns :
sig

  val is_typedef: cdefn -> bool
  val is_termdef: cdefn -> bool

  val dest_typedef: cdefn ->  Basic.fnident * string list * Gtypes.gtype option
  val dest_termdef: cdefn 
    -> Basic.fnident * Gtypes.gtype * (string* Gtypes.gtype) list * thm option

  val mk_typedef: Gtypes.scope 
    -> string -> string list -> Gtypes.gtype option -> cdefn

  val mk_termdef: Gtypes.scope 
    -> string -> Gtypes.gtype 
      -> string list -> thm option -> cdefn

end
