(*-----
Name: logic.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(**
   Theorems and rules of the logic.
   Rules are based on a sequent calculus.
 *)

(** Theorems of the logic *)
type thm
type saved_thm

(** Sequents and goals of the sequent calculus *)
type goal

type tagged_form = (Tag.t* Formula.form)

module Skolem:
    sig

      type skolem_cnst = (Basic.ident * (int * Basic.gtype))
(*
   type skolem_cnst = ((Basic.ident * Basic.gtype) * int)
 *)
      type skolem_type

      type skolem_info=
	  {
	   name: Basic.ident;
	   ty: Basic.gtype;
	   tyenv: Gtypes.substitution;
	   scope: Scope.t;
	   skolems: skolem_type;
	   tylist: (string*int) list
	 } 

(** Skolem variables/constants are used for quantifier rules. *)

      val get_sklm_name: skolem_cnst -> Basic.ident
      val get_sklm_indx: skolem_cnst -> int
      val get_sklm_type: skolem_cnst -> Basic.gtype
      val decln_of_sklm: skolem_cnst -> (Basic.ident * Basic.gtype)

      val get_new_sklm: 
	  Basic.ident -> Basic.gtype -> skolem_type
	    -> (Basic.term * skolem_type)

      val mk_new_skolem: 
	  skolem_info
	-> Basic.term * Basic.gtype 
	    * skolem_type
	    * Gtypes.substitution * (string * int) list

      val add_skolem_to_scope: 
	  Basic.term -> Basic.gtype 
	    -> Scope.t -> Scope.t

      val add_skolems_to_scope: 
	  skolem_cnst list -> Scope.t -> Scope.t

    end

type label = 
    FNum of int
  | FTag of Tag.t

(**
   [rr_type]: where to get rewrite rule from
   [Asm] : labelled assumption
   [RRThm]: given theorem
   [OAsm] : labelled assumption, with ordering
   [ORRThm]: given theorem, with ordering
 *)
type rr_type = 
    Asm of label
  | RRThm of thm
  | OAsm of label * Rewrite.order
  | ORRThm of thm * Rewrite.order

module Sequent:
    sig
      type t

(** Information from a sequent *)
      val asms : t -> tagged_form list
      val concls : t -> tagged_form list
      val scope_of: t -> Scope.t
      val sklm_cnsts: t -> Skolem.skolem_cnst list
      val sqnt_tyvars: t -> Basic.gtype list
      val sqnt_tag: t->Tag.t

(**
   [split_at_cond x]
   Split [x] into [(l, c, r)] so that [x=List.revappend x (c::r)]
   and [c] is element satisfying [?cond?].

   @raise Not_found if no element of [x] satisifies the condition.

   [split_at_index i x]:
   [c] is the [i]th element of [x] (counting from 0).

   [split_at p x]:
   [c] is the first element of [x] such that [p x] is true.

   [split_at_tag t x]:
   [c] is the formula in [x] tagged with [t].

   [split_at_label t x]:
   [c] is the formula in [x] labelled [l].
 *)
      val split_at_index: int -> 'a list -> ('a list * 'a * 'a list)
      val split_at: ('a -> bool) -> 'a list -> ('a list * 'a * 'a list)
      val split_at_tag: 
	  Tag.t -> (Tag.t * 'a) list 
	    -> ((Tag.t * 'a) list * (Tag.t * 'a) * (Tag.t * 'a) list)
      val split_at_label: 
	  label -> (Tag.t * 'a) list 
	    -> ((Tag.t * 'a) list * (Tag.t * 'a) * (Tag.t * 'a) list)

(**
   [split_at_asm lbl x]:
   [split_at_concl lbl x]:

   Split [x] into [(l, c, r)] so that [x=List.revappend x (c::r)]
   and [c] is the formula in [x] identified by label [lbl].

   [split_at_asm lbl x]:
   raise Not_found if [lbl=FNum i] and i>=0

   [split_at_concl lbl x]:
   raise Not_found if [lbl=FNum i] and i<0
 *)


(**
   Get/Delete/Copy particular assumptions/conclusions.
 *)
      val get_asm : int -> t -> tagged_form
      val get_cncl : int -> t -> tagged_form

      val delete_asm : int -> tagged_form list  -> tagged_form list
      val delete_cncl : int -> tagged_form list  -> tagged_form list


(** 
   Get tagged assumptions/conclusions. 
 *)
      val get_tagged_asm : Tag.t -> t -> tagged_form
      val get_tagged_cncl : Tag.t -> t -> tagged_form
      val get_tagged_form: Tag.t -> t -> tagged_form

(**
   Assumption/conclusion tag <-> index 
 *)
      val tag_to_index : Tag.t -> t -> int
      val index_to_tag : int -> t -> Tag.t 

    end

val label_to_tag: label -> Sequent.t -> Tag.t
val label_to_index: label -> Sequent.t -> int

(**
   [cdecln]: Checked term declarations.
   Checking of type and term definitions and declarations

   [cdefn]: Checked Definitions.  
   Checking of type and term definitions and declarations
 *)
type cdefn
(*
   TypeAlias of 
   Basic.ident * string list * Basic.gtype option
   | TypeDef of ctypedef
   | TermDecln of Basic.ident * Basic.gtype
   | TermDef of 
   Basic.ident * Basic.gtype
 * (string*Basic.gtype) list * thm option
 *)
and ctypedef =
    {
     type_name : Basic.ident;  (* name of new type *)
     type_args : string list;  (* arguments of new type *)
     type_base: Basic.gtype;   (* the base type *)
     type_rep: cdefn;          (* representation function *)
     type_abs: cdefn;          (* abstraction function *)
     type_set: Formula.form;      (* defining set *)
     rep_type: thm;
     rep_type_inverse: thm;
     abs_type_inverse: thm
   }

(*
   type ctypedef =
   {
   type_name : Basic.ident;  (* name of new type *)
   type_args : string list;  (* arguments of new type *)
   type_rep: cdefn;          (* representation function *)
   type_thm: thm;          (* subtype theorem *)
   type_set: Formula.form       (* defining set *)
   }
 *)      
type saved_cdefn =
    STypeAlias of Basic.ident * string list * Gtypes.stype option
  | STypeDef of saved_ctypedef
  | STermDecln of Basic.ident * Gtypes.stype
  | STermDef of Basic.ident * Gtypes.stype * saved_thm 
and saved_ctypedef =
    {
     stype_name : Basic.ident;  (* name of new type *)
     stype_args : string list;  (* arguments of new type *)
     stype_base: Gtypes.stype; 
     stype_rep: saved_cdefn;          (* representation function *)
     stype_abs: saved_cdefn;          (* abstraction function *)
     stype_set: Formula.saved_form;      (* defining set *)
     srep_type: saved_thm;
     srep_type_inverse: saved_thm;
     sabs_type_inverse: saved_thm
   }
(*
   and saved_ctypedef =
   {
   stype_name : Basic.ident;  (* name of new type *)
   stype_args : string list;  (* arguments of new type *)
   stype_rep: saved_cdefn;          (* representation function *)
   stype_thm: saved_thm;          (* subtype theorem *)
   stype_set: Formula.saved_form      (* defining set *)
   }
 *)
      
(** 
   Theorem destructors and constructors
   (axioms do not need to be proved).
 *)
val mk_axiom : Formula.form -> thm
(*val dest_thm : thm -> Formula.form *)
val formula_of : thm -> Formula.form 
val term_of : thm -> Basic.term

val string_thm: thm -> string
val to_save: thm -> saved_thm
val from_save: saved_thm -> thm

(** Sequents *)

val sqntError : string ->  exn
val addsqntError : string -> exn -> 'a

(** tag of formula *)
val form_tag: tagged_form -> Tag.t 
val drop_tag: tagged_form -> Formula.form

(** get tagged assumptions/conclusions *)
val get_label_asm : label -> Sequent.t -> tagged_form
val get_label_cncl : label -> Sequent.t -> tagged_form
val get_label_form: label -> Sequent.t -> tagged_form


(** manipulation of subgoals *)
val has_subgoals: goal -> bool
val get_sqnt:goal -> Sequent.t

val goal_tyenv: goal -> Gtypes.substitution

(** get tag of first subgoal *)
val get_goal_tag: goal -> Tag.t
val num_of_subgoals: goal -> int
val get_subgoals: goal -> Sequent.t list
val get_nth_subgoal_sqnt: int -> goal-> Sequent.t
val goal_has_subgoals: goal -> bool
val get_subgoal_tags : goal -> Tag.t list

(** manipulating goals *)
val get_goal : goal -> Formula.form

(** put the tagged sqnt at the front, raise Not_found if not found *)
val goal_focus: Tag.t-> goal -> goal

(** rotate subgoals left and right
   raise No_subgoals if no subgoals
 *)
val rotate_subgoals_left : int -> goal -> goal 
val rotate_subgoals_right : int -> goal -> goal

(**
   [mk_goal scp f]
   Make formula [f] a goal to be proved in scope [scp] 
 *)
val mk_goal : Scope.t -> Formula.form -> goal

(** make a theorem from an established goal *)
(** only suceeds if the goal has no sub-goals *)
val mk_thm : goal -> thm

(** errors *)

(*
   val mk_logicError: string ->Formula.form list -> Result.error
 *)
val logic_error : string -> Formula.form list -> exn
val add_logic_error : string -> Formula.form list -> exn -> 'a


(**
   [tag_record]: tag information for rules.
   [goals]: new goals produced by rule 
   [forms]: new forms produced by rule 
   [terms]: new constants produced by rule
 *)
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
      
      (** 
	 [node]
	 A node is a sequent and a type environment.
       *)
      type node
      val node_tyenv: node -> Gtypes.substitution
      val node_sqnt: node -> Sequent.t

	  (** 
	     [branch]
	     A branch is a list of sequents, a type environment
	     and the tag of the sequent from which the list of sequents 
	     is derived.
	   *)
      type branch
      val branch_tag: branch -> Tag.t
      val branch_tyenv: branch -> Gtypes.substitution
      val branch_sqnts: branch -> Sequent.t list

(** 
   [branch_node node]
   Make a branch from [node] without doing anything.
 *)
      val branch_node : node -> branch

(**
   Functions to apply a tactic to subgoals.
   (A tactic has type node -> branch)
 *)

(**
   [merge env1 env2]: merge type environments.

   Create a [env3] which has the binding of each weak variable in
   [env1 + env2].

   raise [Failure] if a variable ends up bound to itself.
 *)
      val merge_tyenvs:
	  Gtypes.substitution 
	-> Gtypes.substitution	
	  -> Gtypes.substitution


(**
   [merge_tac_tyenvs n b]:
   Merge the type environment of [b], resulting from applying a tactic
   to [n], with the type environment of [n].  Make a new branch with
   the components of [b] but with the new type environment.
 *)
      val merge_tac_tyenvs : node -> branch -> branch

(**
   [apply tac node]: Apply tactic [tac] to [node].

   This is the work-horse for applying tactics. All other functions
   should call this to apply a tactic.

   Approach:
   1. Create a new tag [ticket].
   3. Apply tac to [node] getting branch [b'].
   4. If the tag of [b'] is not [ticket] then fail.

   5. Merge the type environment of [b'] with [n']. 
   (This may be unnecessary.)
   Almost certainly unnecessary so not done.

   6. Return the branch formed from [b'] with the tag of [node].

   raise logicError on failure.
 *)
      val apply: (node -> branch) -> node -> branch

(**
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

(**
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

(**
   [apply_to_each tac (Branch(tg, tyenv, sqnts))]
   Apply tactic [tac] to each sequent in [sqnts] 
   using [apply_to_node].
   replace original sequents with resulting branches.
   return branch with tag [tg].

   raise No_subgoals if [sqnts] is empty.
 *)
      val apply_to_each: (node->branch) -> branch -> branch

(**
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

(** 
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


(** simple tactical (really only an example) *)
(**
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
type conv = Scope.t -> Basic.term -> thm

val postpone :  goal -> goal
val foreach: rule -> Subgoals.branch -> Subgoals.branch
val first_only: rule -> Subgoals.branch -> Subgoals.branch

module Tactics :
    sig

(** 
   [check_term scp trm]
   Ensure that term [trm] is in the scope [scp].
   All identifiers must be bound to a quantifier or defined/declared 
   in a theory. 
   Free variables are not permitted.
 *)
      val check_term: Scope.t -> Formula.form -> unit

(**
   [check_term_memo]
   Memoised version of [check_term].
 *)
      val check_term_memo: 
	  (string, bool) Lib.substype -> Scope.t -> Formula.form -> unit


(** apply a rule to a goal *)
(**      val goal_apply : rule -> goal -> goal *)


(**
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

(** 
   [copy_asm i] 

   .., Ai, ..|- C 
   -> 
   .., Ai, Ai, .. |- C
 *)
      val copy_asm : info option -> label -> rule

(** 
   [copy_cncl i]

   A|- .., Ci, ..
   ->
   A|- .., Ci, Ci, ..
 *)
      val copy_cncl : info option -> label -> rule


(** rotate assumptions conclusions *)

(** 
   [rotate_asm]

   A1, A2 .., An |-
   ->
   A2, .., An, A1 |-
 *)
      val rotate_asms: info option -> rule

(** 
   [rotate_cncls]

   A|- C1, C2 .., Cn
   ->
   A|-  C2, .., Cn, C1
 *)
      val rotate_cncls : info option -> rule

(** logic rules  *)

(** delete x sq: delete assumption (x<0) or conclusion (x>0) from sq*)
      val delete : info option -> label -> rule

(*
   [skip]
   The do nothing tactic
 *)
      val skip : info option -> rule


(** [cut x sq]: add theorem [x] to assumptions of [sq].

   asm |- cncl      --> x, asm |- cncl
 *)
      val cut : info option -> thm -> rule

(** 
   [basic i j sq]: if asm [i] is alpha-equal to cncl [j] of [sq].

   asm, a_{i}, asm' |- concl, c_{j}, concl' 
   -->
   true if a_{i}=c_{j}
 *)
      val basic : info option -> label -> label -> rule

(** 
   [conjI i sq] 

   asm |- a /\ b, concl    tag(t)
   -->
   asm |- a                tag(t1)
   and asm |- b            tag(t2)
 *)
      val conjC: info option -> label -> rule

(** 
   [conjE i sq]
   
   a/\ b, asm |- concl   
   -->
   a, b, asm |- concl 
 *)
      val conjA: info option -> label -> rule

(*
   [disjI i sq]

   a\/b, asm |-  concl   tag(t)
   -->
   a, asm |- concl      tag(t1)
   and b, asm |- concl  tag(t2)
 *)
      val disjA: info option -> label -> rule

(** 
   [disjE i sq]

   asm |- a\/b, concl   
   -->
   asm |- a, b, concl 
 *)
      val disjC: info option -> label -> rule

(** 
   [negA i sq]

   ~a, asms |- concl
   -->
   asms |- a, concl
 *)
      val negA: info option-> label -> rule

(** 
   [negC i sq]

   asms |- ~c, concl
   -->
   c, asms |- concl
 *)
      val negC: info option -> label -> rule

(** 
   [implI i sq]

   asms |- a-> b, cncl 
   -->
   a, asms |- b, cncl
 *)
      val implC: info option -> label -> rule

(** 
   [implE i sq]

   a-> b,asms |-cncl  tag(t)
   -->
   asms |- a, cncl    tag(t1)
   b, asms |- cncl    tag(t2)
 *)
      val implA: info option -> label -> rule

(** 
   [allI i sq]

   asm |- !x. P(c), concl
   -->
   asm |- P(c'), concl   where c' is a new identifier
 *)
      val allC : info option -> label -> rule

(** 
   [allE i sq]

   !x. P(c), asm |-  concl
   -->
   P(c'), asm |- concl   where c' is a given term
 *)
      val allA : info option -> Basic.term -> label -> rule


(** 
   [existI i sq]

   ?x. P(c), asm |- concl
   -->
   P(c'), asm |- concl   where c' is a new identifier
 *)
      val existA : info option -> label -> rule

(** 
   [existE i sq]

   asm |- ?x. P(c), concl
   -->
   asm |- P(c'), concl   where c' is a given term
 *)
      val existC : info option  -> Basic.term -> label -> rule


(** 
   [beta i sq]:  (beta reduction of asm (i<0) or concl (i>0) in sq) 
 *)
      val beta : info option -> label -> rule

(** 
   [trueR i sq]

   asm |- true, concl
   --> true
 *)
      val trueR: info option -> label -> rule

(** 
   [rewrite ctrl thms j sq]

   (list of theorems or assumptions containing [x=y])
   asm |- P(x), concl
   -->
   asm |- P(y), concl

   where [ctrl] is the rewriting control.

   Theorems [thms] must be in scope of sequent.

   Theorems not in scope and assumptions which don't exist are
   silently discarded.

   [rewriteA ctrl simple thms j sq]: rewrite assumption j
   [rewriteC ctrl simple thms j sq]: rewrite conclusion j
 *)
      val rewriteA : info option 
	-> ?ctrl:Rewrite.control
	  -> rr_type list -> label -> rule
      val rewriteC : info option 
	-> ?ctrl:Rewrite.control
	  -> rr_type list -> label -> rule

      val rewrite : info option 
	-> ?ctrl:Rewrite.control
	  -> rr_type list -> label -> rule

(*
   [rewrite_rule scp ctrl rrl thm]

   rewrite theorem [thm] with rules [rrl] in scope [scp].
 *)
      val rewrite_rule: 
	  Scope.t -> ?ctrl:Rewrite.control
	    -> thm list -> thm -> thm
    end

module Conv:
    sig

      (** 
	 [beta_conv scp term]: Apply a single beta conversion to [term].

	 Fails if [term] is not of the form [(%x: F)y]
	 or the resulting formula is not in scope.
       *)

      val beta_conv : conv
    end 


module Defns :
    sig

      val to_saved_cdefn: cdefn -> saved_cdefn
      val from_saved_cdefn: saved_cdefn -> cdefn 

(* Term definition *)
      val is_termdef: cdefn -> bool
      val dest_termdef: cdefn -> 
	Basic.ident * Basic.gtype * thm
      val mk_termdef: 
	  Scope.t 
	-> Basic.ident
	  -> (string * Basic.gtype) list -> Basic.term -> cdefn

(* 
   [mk_termdecln scp name ty]: Declare identifier [name] of type [ty] in
   scope [scp].
   Fails if identifier [name] is already defined in [scp]
   or if [ty] is not well defined.
 *)
      val is_termdecln: cdefn -> bool
      val dest_termdecln: cdefn 
	-> Basic.ident * Basic.gtype 
      val mk_termdecln:
	  Scope.t -> string -> Basic.gtype -> cdefn

(*Type definition: alias *)
      val is_typealias: cdefn -> bool
      val dest_typealias: cdefn ->
	Basic.ident * string list * Basic.gtype option
      val mk_typealias: Scope.t 
	-> string -> string list -> Basic.gtype option -> cdefn

(*Type definition: subtype *)
      val is_subtype: cdefn -> bool
      val dest_subtype: cdefn -> ctypedef

(*
   mk_subtype scp name args d setP rep abs:
   - check name doesn't exist already
   - check all arguments in args are unique
   - check def is well defined 
   (all constructors exist and variables are in the list of arguments)
   - ensure setP has type (d -> bool)
   - declare rep as a function of type (d -> n)
   - make subtype property from setp and rep.
 *)


(*
   [prove_subtype_exists scp setp thm]
   Use [thm] to prove the goal << ?x. setp x >> (built by mk_subtype_exists).

   [mk_subtype_thm scp setp rep]:
   make the subtype theorem
   << (!x1 x2: (((rep x1) = (rep x2)) => (x1 = x2)))
   and 
   (!x: (setp x) = (?x1: x=(rep x1)))>>
 *)
      val prove_subtype_exists: 
	  Scope.t -> Basic.term -> thm -> thm
(*
   val mk_subtype_thm: 
   Scope.t -> Basic.term -> Basic.ident -> thm
 *)
      val mk_subtype_thm: 
	  Scope.t -> Basic.term -> thm

      val mk_subtype: 
	  Scope.t -> string -> string list 
	    -> Basic.gtype -> Basic.term -> string -> string
	      -> thm  (* existance *)
		-> cdefn



    end

(** Printer for sequents *)

(** Printing *)

val print_thm: Printer.ppinfo -> thm -> unit

val print_cdefn: Printer.ppinfo -> cdefn -> unit

(**
   [print_sqnt ppinfo sq]
   Print sequent [sq] using PP info [ppinfo].

   [print_node ppinfo n]
   Print node [n] using PP info [ppinfo].

   [print_branch ppinfo branch]
   Print branch [branch] using PP info [ppinfo].
 *)
val print_sqnt : Printer.ppinfo -> Sequent.t -> unit
val print_node : Printer.ppinfo -> node -> unit
val print_branch : Printer.ppinfo -> branch -> unit
