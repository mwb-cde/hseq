(* theorems and rules of the logic *)
(* rules are based on a sequent calculus *)

(* theorems of the logic *)
type thm
type saved_thm

(* sequents and goals of the sequent calculus *)

type tagged_form = (Tag.t* Formula.form)

(*  type skolem_cnst*)
type skolem_cnst = (Basic.ident * (int * Basic.gtype))

type skolem_type
type sqnt
type subgoal

type goal
type rule= goal -> goal 

(* conversion: a function from a theorem to one or more theorems *)
type conv= thm list -> thm list


(* fident: sequent formula identifiers *)

type fident = 
    FNum of int
  | FTag of Tag.t

val fident_to_tag: fident -> sqnt -> Tag.t
val fident_to_index: fident -> sqnt -> int


(*
   rr_type: where to get rewrite rule from
   Asm : numbered assumption
   Tagged: tagged assumption
   RRThm: given theorem
 *)
type rr_type = 
    Asm of fident
  | RRThm of thm

(* 
   cdefn:
   Checked Definitions: 
   checking of type and term definitions and declarations
 *)

type cdefn =
    TypeDef of Basic.ident * string list * Basic.gtype option
  | TermDef of Basic.ident * Basic.gtype
	* (string*Basic.gtype) list * thm option
	
(* theorem destructors and constructors *)
(* (axioms do not need to be proved) *)

val mk_axiom : Formula.form -> thm

val dest_thm : thm -> Formula.form
val string_thm: thm -> string
val to_save: thm -> saved_thm
val from_save: saved_thm -> thm

(* Sequents *)

(* skolem variables/constants are used for quantifier rules *)

val get_sklm_name: skolem_cnst -> Basic.ident
val get_sklm_indx: skolem_cnst -> int
val get_sklm_type: skolem_cnst -> Basic.gtype
val get_new_sklm: Basic.ident -> Basic.gtype -> skolem_type 
  -> (Basic.term * skolem_type)

val sqntError : string ->  exn
val addsqntError : string -> exn -> 'a

(* information from a sequent *)

val asms : sqnt -> tagged_form list
val concls : sqnt -> tagged_form list
val scope_of: sqnt -> Gtypes.scope
val sklm_cnsts: sqnt -> skolem_cnst list
val sqnt_tyvars: sqnt -> Basic.gtype list
val sqnt_tag: sqnt->Tag.t

(* get/delete/copy particular assumptions/conclusions *)
val get_asm : int -> sqnt -> tagged_form
val get_cncl : int -> sqnt -> tagged_form

val delete_asm : int -> tagged_form list  -> tagged_form list
val delete_cncl : int -> tagged_form list  -> tagged_form list

(* tag of formula *)
val tag_of_form: tagged_form -> Tag.t
val drop_tag: tagged_form -> Formula.form

(* get tagged assumptions/conclusions *)
val get_tagged_asm : Tag.t -> sqnt -> tagged_form
val get_tagged_cncl : Tag.t -> sqnt -> tagged_form
val get_tagged_form: Tag.t -> sqnt -> tagged_form

(* get tagged assumptions/conclusions *)
val get_fident_asm : fident -> sqnt -> tagged_form
val get_fident_cncl : fident -> sqnt -> tagged_form
val get_fident_form: fident -> sqnt -> tagged_form

(* assumption/conclusion tag <-> index *)

val tag_to_index : Tag.t -> sqnt -> int
val index_to_tag : int -> sqnt -> Tag.t 

(* manipulation of subgoals *)

val has_subgoals: goal -> bool
val get_sqnt:goal -> sqnt

val goal_tyenv: goal -> Gtypes.substitution

(* get tags of all subgoals *)
val get_all_goal_tags: goal -> Tag.t list

(* get tag of first subgoal *)
val get_goal_tag: goal -> Tag.t

(* make a goal from a formula  *)

(* val num_of_subsqnts: goal -> int *)
val num_of_subgoals: goal -> int

val subgoal_nth_sqnt : int -> subgoal -> sqnt

val get_subgoals: goal -> subgoal list
val get_nth_subgoal: int -> goal -> subgoal

val get_nth_subgoal_sqnt: int -> goal-> sqnt
val goal_has_subgoals: goal -> bool

val get_subgoal_tags : goal -> Tag.t list

(* manipulating goals *)
val get_goal : goal -> Formula.form

(* put the tagged sqnt at the front, raise Not_found if not found *)
val goal_focus: Tag.t->rule

(**
   [mk_goal scp f]
   Make formula [f] a goal to be proved in scope [scp] 
 *)
val mk_goal : Gtypes.scope -> Formula.form -> goal

(* make a theorem from an established goal *)
(* only suceeds if the goal has no sub-goals *)
val mk_thm : goal -> thm

(* errors *)

val mklogicError: string ->Formula.form list -> Result.error
val logicError : string -> Formula.form list -> exn
val addlogicError : string -> Formula.form list -> exn -> 'a

(* Printing *)
val print_thm: Printer.ppinfo -> thm -> unit

(* tag information for rules *)
(* goals: new goals produced by rule *)
(* forms: new forms produced by rule *)
(* terms: new constants produced by rule *)
type tag_record = 
    { 
      goals:Tag.t list; 
      forms : Tag.t list;
      terms: Basic.term list
    }
type info = tag_record ref

val make_tag_record: 
    Tag.t list 
  -> Tag.t list 
    -> Basic.term list 
      -> tag_record

val do_info: 
    info option ->
      Tag.t list-> Tag.t list -> Basic.term list -> unit

val add_info: 
    info option ->
      Tag.t list-> Tag.t list -> Basic.term list -> unit

module Rules:
    sig

(* apply a rule to a goal *)
(*      val goal_apply : rule -> goal -> goal *)
      val postpone :  goal -> goal


(*
   [lift_asm id sqnt] 
   [lift_concl id sqnt]
   [lift id sqnt]
   Move assumption/conclusion with identifier [id] to 
   to top of the assumptions/conclusions of sequent sqnt.

   Raise Not_found if identified formula is not in 
   assumptions/conclusions.

   [lift] tries lift_asm then tries lift_concl.
   Doesn't change the formula tag.
 *)
      val lift_asm : info option -> fident -> rule
      val lift_concl : info option -> fident -> rule
      val lift : info option -> fident -> rule

(* copy_asm i: 
   .., Ai, ..|- C
   ->
   .., Ai, Ai, .. |- C
 *)
      val copy_asm : info option -> fident -> rule

(* copy_cncl i: 
   A|- .., Ci, ..
   ->
   A|- .., Ci, Ci, ..
 *)
      val copy_cncl : info option -> fident -> rule

(* rotate assumptions conclusions *)

(* rotate_asm
   A1, A2 .., An |-
   ->
   A2, .., An, A1 |-
 *)
      val rotate_asms: info option -> rule

(* rotate_cncls:
   A|- C1, C2 .., Cn
   ->
   A|-  C2, .., Cn, C1
 *)
      val rotate_cncls : info option -> rule

(* logic rules  *)

(* delete x sq: delete assumption (x<0) or conclusion (x>0) from sq*)
      val delete : info option -> fident -> rule

(* cut x sq: adds theorem x to assumptions of sq 

   asm |- cncl      --> x, asm |- cncl
 *)
      val cut : info option -> thm -> rule

(* assume i j sq: if asm i is alpha-equal to cncl j of sq, 

   asm, a_{i}, asm' |- concl, c_{j}, concl' 
   -->
   true if a_{i}=c_{j}
 *)
      val assume : info option -> fident -> fident -> rule

(* 
   conjI i sq: 
   asm |- a /\ b, concl    tag(t)
   -->
   asm |- a                tag(t1)
   and asm |- b            tag(t2)
 *)
      val conjI: info option -> fident -> rule

(* 
   conjE i sq: 
   a/\ b, asm |- concl   
   -->
   a, b, asm |- concl 
 *)
      val conjE: info option -> fident -> rule

(*
   disjI i sq: 
   a\/b, asm |-  concl   tag(t)
   -->
   a, asm |- concl      tag(t1)
   and b, asm |- concl  tag(t2)
 *)
      val disjI: info option -> fident -> rule

(* disjE i sq: 
   asm |- a\/b, concl   
   -->
   asm |- a, b, concl 
 *)
      val disjE: info option -> fident -> rule

(* negA i sq:
   ~a, asms |- concl
   -->
   asms |- a, concl
 *)
      val negA: info option-> fident -> rule

(* negC i sq:
   asms |- ~c, concl
   -->
   c, asms |- concl
 *)
      val negC: info option -> fident -> rule

(* implI i sq
   asms |- a-> b, cncl 
   -->
   a, asms |- b, cncl
 *)
      val implI: info option -> fident -> rule

(* implE i sq
   a-> b,asms |-cncl  tag(t)
   -->
   asms |- a, cncl    tag(t1)
   and 
   b, asms |- cncl    tag(t2)
 *)
      val implE: info option -> fident -> rule

(* allI i sq
   asm |- !x. P(c), concl
   -->
   asm |- P(c'), concl   where c' is a new identifier
 *)
      val allI : info option -> fident -> rule

(* allE i sq
   !x. P(c), asm |-  concl
   -->
   P(c'), asm |- concl   where c' is a given term
 *)
      val allE : info option -> Basic.term -> fident -> rule

(* existI i sq
   ?x. P(c), asm |- concl
   -->
   P(c'), asm |- concl   where c' is a new identifier
 *)
      val existI : info option -> fident -> rule

(* existE i sq
   asm |- ?x. P(c), concl
   -->
   asm |- P(c'), concl   where c' is a given term
 *)
      val existE : info option  -> Basic.term -> fident -> rule


(* beta i sq:  (beta reduction of asm (i<0) or concl (i>0) in sq) *)
      val beta : info option -> fident -> rule

(* trueR i sq
   asm |- true, concl
   --> true
 *)
      val trueR: info option -> fident -> rule

(* name_rule: introduce a new name in the sqnt as a synonym for a term  *)
(* name id trm:
   Asm|-Cncl -> id=trm, Asm|-Cncl

   the long name thy.id must be unique (where thy is the current theory name)

 *)
      val name_rule : info option -> string -> Basic.term -> rule


(* rewrite_any dir thms j sq:
   list of theorems or assumptions containing x=y
   asm |- P(x), concl
   -->
   asm |- P(y), concl
   where dir is =true for right-left and false for left-right
   theorems must be in scope.
   silently discards theorems not in scope and assumptions which don't exist
 *)
      val rewrite_any : info option 
	-> ?dir:Rewrite.direction -> ?simple:bool 
	  -> rr_type list -> fident -> rule

    end

open Rules

module ThmRules:
    sig

(* functions for manipulating theorems (alpha quality) *)

(* conversions which apply to only one theorem 
   (e.g. conjE_conv |- a and b  --> [|- a; |- b])
 *)
      val conjE_conv: thm-> thm list
      val allI_conv: Gtypes.scope -> Basic.term -> conv
      val eta_conv: Gtypes.scope -> Formula.form -> conv
      val beta_conv: Gtypes.scope -> conv

(* conversions which apply to all theorems in the list *)

(* rewrite_conv: apply rewrite, fail if any rewrite fails *)
      val rewrite_conv: Gtypes.scope -> ?dir:Rewrite.direction -> ?simple:bool
	-> thm list -> thm -> thm

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

      val dest_typedef: cdefn ->
	Basic.ident * string list * Basic.gtype option
      val dest_termdef: cdefn -> 
	Basic.ident * Basic.gtype * (string* Basic.gtype) list * thm option

      val mk_typedef: Gtypes.scope 
	-> string -> string list -> Basic.gtype option -> cdefn

      val mk_termdef: Gtypes.scope 
	-> string -> Basic.gtype 
	  -> string list -> thm option -> cdefn

    end

type skolem_info=
    {
     name: Basic.ident;
     ty: Basic.gtype;
     tyenv: Gtypes.substitution;
     scope: Gtypes.scope;
     skolems: skolem_type;
     tylist: (string*int) list
   } 
val mk_new_skolem: 
    skolem_info
  -> Basic.term * Basic.gtype 
      * (Basic.ident * (int * Basic.gtype)) list 
      * Gtypes.substitution * (string * int) list

val add_sklms_to_scope: 
    (Basic.ident * ('a * Basic.gtype)) list ->
      Gtypes.scope -> Gtypes.scope
