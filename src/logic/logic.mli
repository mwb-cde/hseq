(* theorems and rules of the logic *)
(* rules are based on a sequent calculus *)

(* theorems of the logic *)
type thm
type saved_thm

(* sequents and goals of the sequent calculus *)

type tagged_form = (Tag.t* Formula.form)

module Skolem:
    sig

(*  type skolem_cnst*)
      type skolem_cnst = (Basic.ident * (int * Basic.gtype))
      type skolem_type

      type skolem_info=
	  {
	   name: Basic.ident;
	   ty: Basic.gtype;
	   tyenv: Gtypes.substitution;
	   scope: Gtypes.scope;
	   skolems: skolem_type;
	   tylist: (string*int) list
	 } 

(* skolem variables/constants are used for quantifier rules *)

      val get_sklm_name: skolem_cnst -> Basic.ident
      val get_sklm_indx: skolem_cnst -> int
      val get_sklm_type: skolem_cnst -> Basic.gtype
      val get_new_sklm: Basic.ident -> Basic.gtype -> skolem_type 
	-> (Basic.term * skolem_type)

      val mk_new_skolem: 
	  skolem_info
	-> Basic.term * Basic.gtype 
	    * (Basic.ident * (int * Basic.gtype)) list 
	    * Gtypes.substitution * (string * int) list

      val add_skolem_to_scope: 
	  Basic.term -> Basic.gtype 
	    -> Gtypes.scope -> Gtypes.scope

      val add_skolems_to_scope: 
	  (Basic.ident * ('a * Basic.gtype)) list ->
	    Gtypes.scope -> Gtypes.scope

    end

module Sequent:
    sig
      type t

(* information from a sequent *)

      val asms : t -> tagged_form list
      val concls : t -> tagged_form list
      val scope_of: t -> Gtypes.scope
      val sklm_cnsts: t -> Skolem.skolem_cnst list
      val sqnt_tyvars: t -> Basic.gtype list
      val sqnt_tag: t->Tag.t

(* get/delete/copy particular assumptions/conclusions *)
      val get_asm : int -> t -> tagged_form
      val get_cncl : int -> t -> tagged_form

      val delete_asm : int -> tagged_form list  -> tagged_form list
      val delete_cncl : int -> tagged_form list  -> tagged_form list


(* get tagged assumptions/conclusions *)
      val get_tagged_asm : Tag.t -> t -> tagged_form
      val get_tagged_cncl : Tag.t -> t -> tagged_form
      val get_tagged_form: Tag.t -> t -> tagged_form

(* assumption/conclusion tag <-> index *)

      val tag_to_index : Tag.t -> t -> int
      val index_to_tag : int -> t -> Tag.t 

    end

type goal
(*
   type rule= goal -> goal 
 *)

(* conversion: a function from a theorem to one or more theorems *)
type conv= thm list -> thm list

(* label: sequent formula identifiers *)

type label = 
    FNum of int
  | FTag of Tag.t

val label_to_tag: label -> Sequent.t -> Tag.t
val label_to_index: label -> Sequent.t -> int

(*
   rr_type: where to get rewrite rule from
   Asm : numbered assumption
   Tagged: tagged assumption
   RRThm: given theorem
 *)
type rr_type = 
    Asm of label
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


val sqntError : string ->  exn
val addsqntError : string -> exn -> 'a

(* tag of formula *)
val form_tag: tagged_form -> Tag.t 
val drop_tag: tagged_form -> Formula.form

(* get tagged assumptions/conclusions *)
val get_label_asm : label -> Sequent.t -> tagged_form
val get_label_cncl : label -> Sequent.t -> tagged_form
val get_label_form: label -> Sequent.t -> tagged_form


(* manipulation of subgoals *)
val has_subgoals: goal -> bool
val get_sqnt:goal -> Sequent.t

val goal_tyenv: goal -> Gtypes.substitution

(* get tag of first subgoal *)
val get_goal_tag: goal -> Tag.t
val num_of_subgoals: goal -> int
val get_subgoals: goal -> Sequent.t list
val get_nth_subgoal_sqnt: int -> goal-> Sequent.t
val goal_has_subgoals: goal -> bool
val get_subgoal_tags : goal -> Tag.t list

(* manipulating goals *)
val get_goal : goal -> Formula.form

(* put the tagged sqnt at the front, raise Not_found if not found *)
val goal_focus: Tag.t-> goal -> goal

(* rotate subgoals left and right
   raise No_subgoals if no subgoals
 *)
val rotate_subgoals_left : int -> goal -> goal 
val rotate_subgoals_right : int -> goal -> goal

(**
   [mk_goal scp f]
   Make formula [f] a goal to be proved in scope [scp] 
 *)
val mk_goal : Gtypes.scope -> Formula.form -> goal

(* make a theorem from an established goal *)
(* only suceeds if the goal has no sub-goals *)
val mk_thm : goal -> thm

(* errors *)

val mk_logicError: string ->Formula.form list -> Result.error
val logicError : string -> Formula.form list -> exn
val addlogicError : string -> Formula.form list -> exn -> 'a


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

module Subgoals:
    sig
      
      (* 
	 [node]
	 A node is a sequent and a type environment.
       *)
      type node
      val node_tyenv: node -> Gtypes.substitution
      val node_sqnt: node -> Sequent.t

	  (* 
	     [branch]
	     A branch is a list of sequents, a type environment
	     and the tag of the sequent from which the list of sequents 
	     is derived.
	   *)
      type branch
      val branch_tag: branch -> Tag.t
      val branch_tyenv: branch -> Gtypes.substitution
      val branch_sqnts: branch -> Sequent.t list

(* 
   [branch_node node]
   Make a branch from [node] without doing anything.
*)
      val branch_node : node -> branch
(*
   Functions to apply a tactic to subgoals.
   (A tactic has type node -> branch)
 *)
(*
   [apply_to_node ?report tac (Node(tyenv, sqnt))]

   Apply tactic [tac] to node, getting [result].  If tag of result is
   the same as the tag of sqnt, then return result.  Otherwise raise
   logicError.

   If [report] is given, apply to first subgoal and the branch
   resulting from application of [tac]. (This is to allow interactive
   proof support to print result of the tactic).
 *)
      val apply_to_node: 
	  ?report:(node->branch->unit) -> (node->branch) -> node -> branch

(*
   [apply_to_first ?report tac (Branch(tg, tyenv, sqnts))]

   Apply tactic [tac] to firsg sequent of [sqnts] using
   [apply_to_node].  replace original sequent with resulting branches.
   return branch with tag [tg].

   If [report] is given, apply to first subgoal and the branch
   resulting from application of [tac]. (This is to allow interactive
   proof support to print result of the tactic).

   raise No_subgoals if [sqnts] is empty.
*)
      val apply_to_first: 
	  ?report:(node->branch->unit) ->
	    (node->branch) -> branch -> branch
(*
   [apply_to_each tac (Branch(tg, tyenv, sqnts))]
   Apply tactic [tac] to each sequent in [sqnts] 
   using [apply_to_node].
   replace original sequents with resulting branches.
   return branch with tag [tg].

   raise No_subgoals if [sqnts] is empty.
 *)
      val apply_to_each: (node->branch) -> branch -> branch

(*
   [apply_to_goal ?report tac goal] 

   Apply tactic [tac] to first subgoal of in [goal] using
   [apply_to_first].  Replace original list of subgoals with resulting
   subgoals.  

   If [report] is given, apply to first subgoal and the branch
   resulting from application of [tac]. (This is to allow interactive
   proof support to print result of the tactic).

   raise logicError "Invalid Tactic" 
   if tag of result doesn't match tag originaly assigned to it.
 *)
      val apply_to_goal: 
	  ?report:(node->branch->unit) ->  (node->branch) -> goal -> goal

(* 
   [zip tacl branch]
   apply each of the tactics in [tacl] to the corresponding 
   subgoal in branch.
   e.g. [zip [t1;t2;..;tn] (Branch [g1;g2; ..; gm])
   is Branch([t1 g1; t2 g2; .. ;tn gn]) 
   (with [t1 g1] first and [tn gn] last)
   if n<m then untreated subgoals are attached to the end of the new
   branch.
   if m<n then unused tactic are silently discarded.
   typenv of new branch is that produced by the last tactic 
   ([tn gn] in the example).
   tag of the branch is the tag of the original branch.
 *)
      val zip: (node -> branch) list -> branch -> branch


(* simple tactical (really only an example) *)
(*
   [seq tac1 tac2 node]
   apply tactic [tac1] to [node] then [tac2] 
   to each of the resulting subgoals.
 *)
(*
      val seq: (node -> branch) -> (node -> branch) -> node -> branch
*)
    end

type node = Subgoals.node
type branch = Subgoals.branch
type rule = Subgoals.node -> Subgoals.branch

val postpone :  goal -> goal
val foreach: rule -> Subgoals.branch -> Subgoals.branch
val first_only: rule -> Subgoals.branch -> Subgoals.branch

module Rules:
    sig

(** [check_term scp trm]
   Ensure that term [trm] is in the scope [scp].
   All identifiers must be bound to a quantifier or defined/declared 
   in a theory. 
   Free variables are not permitted.
 *)
      val check_term: Gtypes.scope -> Basic.term -> unit

(**
   [check_term_memo]
   Memoised version of [check_term].
 *)
      val check_term_memo: 
	  (string, bool) Lib.substype -> Gtypes.scope -> Basic.term -> unit


(* apply a rule to a goal *)
(*      val goal_apply : rule -> goal -> goal *)


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
      val lift_asm : info option -> label -> rule
      val lift_concl : info option -> label -> rule
      val lift : info option -> label -> rule

(* copy_asm i: 
   .., Ai, ..|- C
   ->
   .., Ai, Ai, .. |- C
 *)
      val copy_asm : info option -> label -> rule

(* copy_cncl i: 
   A|- .., Ci, ..
   ->
   A|- .., Ci, Ci, ..
 *)
      val copy_cncl : info option -> label -> rule


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
      val delete : info option -> label -> rule

(*
   [skip]
   The do nothing tactic
*)
      val skip : info option -> rule


(* cut x sq: adds theorem x to assumptions of sq 

   asm |- cncl      --> x, asm |- cncl
 *)
      val cut : info option -> thm -> rule

(* basic i j sq: if asm i is alpha-equal to cncl j of sq, 

   asm, a_{i}, asm' |- concl, c_{j}, concl' 
   -->
   true if a_{i}=c_{j}
 *)
      val basic : info option -> label -> label -> rule

(* 
   conjI i sq: 
   asm |- a /\ b, concl    tag(t)
   -->
   asm |- a                tag(t1)
   and asm |- b            tag(t2)
 *)
      val conjC: info option -> label -> rule

(* 
   conjE i sq: 
   a/\ b, asm |- concl   
   -->
   a, b, asm |- concl 
 *)
      val conjA: info option -> label -> rule

(*
   disjI i sq: 
   a\/b, asm |-  concl   tag(t)
   -->
   a, asm |- concl      tag(t1)
   and b, asm |- concl  tag(t2)
 *)
      val disjA: info option -> label -> rule

(* disjE i sq: 
   asm |- a\/b, concl   
   -->
   asm |- a, b, concl 
 *)
      val disjC: info option -> label -> rule

(* negA i sq:
   ~a, asms |- concl
   -->
   asms |- a, concl
 *)
      val negA: info option-> label -> rule

(* negC i sq:
   asms |- ~c, concl
   -->
   c, asms |- concl
 *)
      val negC: info option -> label -> rule

(* implI i sq
   asms |- a-> b, cncl 
   -->
   a, asms |- b, cncl
 *)
      val implC: info option -> label -> rule

(* implE i sq
   a-> b,asms |-cncl  tag(t)
   -->
   asms |- a, cncl    tag(t1)
   b, asms |- cncl    tag(t2)
 *)
      val implA: info option -> label -> rule

(* allI i sq
   asm |- !x. P(c), concl
   -->
   asm |- P(c'), concl   where c' is a new identifier
 *)
      val allC : info option -> label -> rule

(* allE i sq
   !x. P(c), asm |-  concl
   -->
   P(c'), asm |- concl   where c' is a given term
 *)
      val allA : info option -> Basic.term -> label -> rule


(* existI i sq
   ?x. P(c), asm |- concl
   -->
   P(c'), asm |- concl   where c' is a new identifier
 *)
      val existA : info option -> label -> rule

(* existE i sq
   asm |- ?x. P(c), concl
   -->
   asm |- P(c'), concl   where c' is a given term
 *)
      val existC : info option  -> Basic.term -> label -> rule


(* beta i sq:  (beta reduction of asm (i<0) or concl (i>0) in sq) *)
      val beta : info option -> label -> rule

(* trueR i sq
   asm |- true, concl
   --> true
 *)
      val trueR: info option -> label -> rule

(* name_rule: introduce a new name in the sqnt as a synonym for a term  *)
(* name id trm:
   Asm|-Cncl -> id=trm, Asm|-Cncl

   the long name thy.id must be unique (where thy is the current theory name)

 *)
      val name_rule : info option -> string -> Basic.term -> rule


(* rewrite ctrl thms j sq:
   list of theorems or assumptions containing x=y
   asm |- P(x), concl
   -->
   asm |- P(y), concl

   where ctrl is the rewriting control.
   theorems must be in scope.
   silently discards theorems not in scope and assumptions which don't exist
 *)
      val rewrite : info option 
	-> ?ctrl:Rewrite.control
	  -> rr_type list -> label -> rule

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
      val rewrite_conv: 
	  Gtypes.scope -> ?ctrl:Rewrite.control
	    -> thm list -> thm -> thm
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

(* Printer for sequents *)

(* Printing *)

val print_thm: Printer.ppinfo -> thm -> unit

(**
   [print_sqnt ppinfo sq]
   Print sequent [sq] using PP info [ppinfo].
*)
val print_sqnt : Printer.ppinfo -> Sequent.t -> unit
