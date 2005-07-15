(*-----
Name: logic.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(**
   Theorems and rules of the logic.

   The rules of the logic are based on a sequent calculus.  A {e
   theorem} is a formula which has been proved true, by applying rules
   to a {e goal}. A goal is made up of {e subgoals} to be proved. If a
   goal has no subgoals, it can be converted to a theorem. The rules
   of the logic are implemented as {e tactics} which reduce a subgoal
   or replace it with a list of subgoals.

   The logic includes a number of features which are experimental or
   are intended to improve efficiency. These include some functions
   which construct theorems without passing through the rules of the
   logic. In particular, beta reduction is available as a conversion
   (a function constructing a theorem [|- t=x] from term [t]).  Term
   and type definitions are currently handled in this module.

   The implementation of the logic is experimental and is likely to be
   faulty. The logic should not be considered sound.

   {7 Goals, subgoals and tactics}

   A goal has type {!Logic.goal} and consists of the formula to be
   proved, a list of subgoals and a type environment. The type
   environment holds bindings for weak variables occuring in any of
   the subgoals. This means that when a weak variable is assigned a
   constant (e.g. by unification) in one subgoal, the binding is
   propagated to all the other subgoals. This is necessary for the
   soundness of the logic.

   A subgoal (also called {e sequents}) has type {!Logic.Sequent.t}
   and consists of a tag, a scope, possibly empty lists of {e
   assumptions} and {e conclusions} and information used to generate
   terms (by the quantifier rules). Subgoals are implemented in module
   {!Logic.Sequent}.

   The tag of a subgoal is unique within a goal and can be used to
   identify the subgoal. This can be used to determine whether a
   subgoal is present in a goal or whether it has been solved).

   The assumptions and conclusions of a subgoal are collectively
   refered as the {e formulas} of the subgoal and each subgoal has at
   least one formula. Each formula of a subgoal has a {e tag} which is
   unique to in the subgoal. Tags are intended for use in tactics, to
   keep track of a formula which may be moved around by tactics. For
   interactive proof, it is more convenient to use integers indicating
   the position of the formula in the list of assumptions or
   conclusions. A {e label} is either the tag of the formula in the
   subgoal or an integer giving the formulas' position. A negative
   integer {i -i} identifies the {i i}th assumption with the first
   assumption at position -1.  A positive integer {i i} identifies
   the {i i}th conclusion with the first conclusion at position 1.

   Note that integer labels are only intended to be used during
   interactive proofs. A tactic should convert an integer label to a
   tag as its first action. Tactics written using tags to identify
   formulas will be more robust and efficient than those using
   positional identifiers since the tag of a formula in a subgoal
   always stays the same while the position of the formula can change.

   The application of tactics to subgoals and the integration of the
   result into the goal is handled by a subgoal package (module
   {!Logic.Subgoals}). This package manages the proof tree contructed
   during a proof and allows tactics to be chained together, applying
   each tactic in a sequence to the subgoals resulting from the
   previous tactic.

   A tactic is a function from a {e node} to a {e branch} of the proof
   tree. A node has type {!Logic.Subgoals.node} consists of a single
   subgoal and the type environment of the goal. A branch has type
   {!Logic.Subgoals.branch} and consists of a list of subgoals and the
   new type environment of the goal. The subgoals and type environment
   of a node or a branch can be examined, e.g. to determine what
   action to take. Nodes and branches can only by constructed by
   functions in module {!Logic}. A new tactic must use the logic rules
   implemented by module {!Logic.Tactics} to construct a branch from a
   node. The subgoal package tags nodes and constructed branches to
   ensure that the branch {e b} returned by a tactic applied to node
   {e n} was constructed from {e n}. This is intended to make it
   impossible to return an arbitrary branch for a given node. The
   correct working of this mechanism is required to ensure the
   soundness of the logic.

   To assist the development of new tactics, each of the standard
   tactics provides information about subgoals and formulas created or
   affected and contants introduced by the tactic. This information
   consists of the tags of subgoals, formulas and terms generated by a
   tactic. Terms are generated by quantifier rules , which instantiate
   quantifiers with arbitrary {e Skolem} contants. These are
   identifiers which are unique to the scope of a subgoal. The
   generation of Skolem constants is handled by module
   {!Logic.Skolem}.

   {7 Notation}

   A theorem is written |- t

   A subgoal is written 
   {L t:\[A{_ ta{_ 1}}, ... , A{_ ta{_ n}} |- 
   C{_ tc{_ 1}}, ..., C{_ tc{_ n}}\]}
   where 
   {ul {- t is the tag of the subgoal}
   {- ta{_ i} is the tag of assumption A{_ ta{_ i}}}
   {- tc{_ i} is the tag of conclusion C{_ tc{_ i}}}
   {- A{_ i} are the assumptions} 
   {- C{_ i} are the conclusions} 
   {- A{_ ta{_ 1}} is the first assumption} 
   {- C{_ tc{_ 1}} is the first conclusion}} 
   When the subgoal tag or a formula tag is irrelevant, they may be
   omitted. When the subgoal tag is omitted, the enclosing
   brackets ('\[' and '\]') may also be omitted.

   A list of subgoals is written as a semi-colon seperated list:
   [sg1; sg2; ...; sg3].

   The information provided by tactics is written 
   {L [info: goals = goal_tags, forms=form_tags, terms = term_list]}
   where
   {ul {- [goal_tags] is a list of tags which identify subgoals.}
   {- [form_tags] is a list of tags which identify subgoal formulas.}
   {- [term_list] is a list of terms which have been introduced by 
   the tactic.}}
 *)

(** {5 Theorems} *)

type thm
(** The type of theorems *)

val mk_axiom : Formula.form -> thm
(** Make an axiom. (Axioms do not need to be proved true.) *)

val formula_of : thm -> Formula.form 
(** Convert a theorem to a formula *)
val term_of : thm -> Basic.term
(** Convert a theorem to a term *)

(** {7 Permanent storage} *)

type saved_thm
(** The representation for saving on permanent storage *)
val to_save: thm -> saved_thm
(** Convert to the representation for saving on permanent storage *)
val from_save: saved_thm -> thm
(** Convert from the representation for saving on permanent storage *)

(** {7 Pretty printing} *)

val print_thm: Printer.ppinfo -> thm -> unit
(** Pretty printing of a theorem *)

val string_thm: thm -> string
(** String representation of a theorem (using [print_thm] instead) *)


(** {5 Error reporting} *)

val logic_error : string -> Formula.form list -> exn
(** Make a logic error *)
val add_logic_error : string -> Formula.form list -> exn -> 'a
(** Add a logic error to an existing list of errors *)

val sqntError : string ->  exn
(** Make a sequent error *)
val addsqntError : string -> exn -> 'a
(** Add a sequent error to an existing list of errors *)


(** {5 Subgoals} *)

(** {7 Types used in subgoals} *)

(** 
   Labels for identifying sequent formulas.

   [FNum i]: The formula at position [i]. If [i] is negative, [i=-j],
   the formula is the assumption at position [j]. If [i] is positive,
   the formula is the conclusion at position [i].

   [FTag t]: The formula with tag [t].
 *)
type label = 
    FNum of int
  | FTag of Tag.t

type tagged_form = (Tag.t* Formula.form)
(** Tagged formulas. Each formula in a subgoal has a tag. *)

val form_tag: tagged_form -> Tag.t 
(** The tag of a formula *)
val drop_tag: tagged_form -> Formula.form
(** Drop the tag of a formula *)

(** 
   Skolem constants 
 *)
module Skolem:
    sig
(** 
   Skolem constants are introduced by quantifier rules, to instantiate
   a quantified formula.

   The generation of skolem constants uses information stored in
   subgoals. Each skolem constant is an identifier, built by
   [Basic.Id], with a name which is unique in the subgoal. The
   identifier is added to the scope of the subgoal and can be used to
   instantiate a formula in the subgoal.

   A skolem constant is initially assigned a weak type variable which is then
   unified with the type required of the constant.
 *)

      (** The record of an individual skolem constant *)
      type skolem_cnst = (Basic.ident * (int * Basic.gtype))

      val get_sklm_name: skolem_cnst -> Basic.ident
      val get_sklm_indx: skolem_cnst -> int
      val get_sklm_type: skolem_cnst -> Basic.gtype

      val make_skolem_name : Basic.ident -> int -> Basic.ident
	  (** 
	     [make_skole_name i n]: Make the name of a skolem constant
	     from [i] and [n]
	   *)

      val decln_of_sklm: skolem_cnst -> (Basic.ident * Basic.gtype)
	  (** Make a declaration from a skolem record *)

	  (** The record of generated skolem constants *)
      type skolem_type

      val get_old_sklm: Basic.ident -> skolem_type -> skolem_cnst
	  (** 
	     Get the record of the skolem constant previously
	     generated from an identifier. 
	   *)

	  (** Information needed to generate a new skolem constant *)
      type new_skolem_data=
	  {
	   name: Basic.ident;
	   ty: Basic.gtype;
	   tyenv: Gtypes.substitution;
	   scope: Scope.t;
	   skolems: skolem_type;
	   tylist: (string*int) list
	 } 
	    (** 
	       [name]: The desired name of the skolem constant. The
	       theory part must be the theory of the goal. The name
	       part is usually the name of the bound variable being
	       instantiated.

	       [ty]: The type of the bound variable being instantiated.

	       [tyenv]: The type environment of the subgoal.
	       
	       [scope]: The scope of the subgoal.

	       [tylist]: A list of type names already in use (used to
	       generate the name of the type variable.
	     *)

      val mk_new_skolem: 
	  new_skolem_data
	-> (Basic.term 
	      * Basic.gtype 
	      * skolem_type
	      * Gtypes.substitution 
	      * (string * int) list)
(** 
   [mk_new_skolem data] constructs a new skolem. Returns [(sv, sty,
   skolems, tyenv, tylist]) where [sv] is the new skolem constant,
   [sty] is the type of the skolem constant, [skolems] is the updated
   skolems record, [tyenv] is the type environment updated when making
   the skolems' type and [tylist] is the updated list of type variable
   names.
 *)

    end


(** {6 Sequents} *)

(** {7 Utility functions} 

   Utility functions for use with sequents
 *)

val join_up : 'a list -> 'a list -> 'a list
(** 
   [join_up l r] is [List.rev_append l r]. Reverses [l] and appends it to [r].
*)

val split_at_tag: 
    Tag.t -> (Tag.t * 'a) list 
      -> ((Tag.t * 'a) list * (Tag.t * 'a) * (Tag.t * 'a) list)
(**
   [split_at_tag t x]: Split [x] into [(l, c, r)] so that
   [x=join_up l (c::r)] and [c] is the first element of [x]
   tagged with [t].
 *)

val split_at_label: 
    label -> (Tag.t * 'a) list 
      -> ((Tag.t * 'a) list * (Tag.t * 'a) * (Tag.t * 'a) list)
(**
   [split_at_label t x]: Split [x] into [(l, c, r)] so that
   [x=join_up l (c::r)] and [c] is the formula in [x] labelled
   [l].
 *)

val split_at_asm: 
    label -> (Tag.t * 'a) list 
      -> ((Tag.t * 'a) list * (Tag.t * 'a) * (Tag.t * 'a) list)
(**
   [split_at_asm lbl x]: Split [x] into [(l, c, r)] so that
   [x=join_up l (c::r)] and [c] is the assumption in [x] labelled
   [l].

   raise Not_found if [lbl=FNum i] and i>=0
 *)

val split_at_concl: 
    label -> (Tag.t * 'a) list 
      -> ((Tag.t * 'a) list * (Tag.t * 'a) * (Tag.t * 'a) list)
(**
   [split_at_concl lbl x]: Split [x] into [(l, c, r)] so that
   [x=join_up l (c::r)] and [c] is the conclusion in [x] labelled
   [l].
   raise Not_found if [lbl=FNum i] and i<0
 *)


(** Sequents and their components *)
module Sequent:
    sig
(**
   A sequent is made up of a unique tag, a scope, a list of
   assumptions, a list of conclusions and information about skolem
   constants and weak types,
 *)

      type t
(** The type of sequents *)

(** {7 Components of a sequent} *)

      val asms : t -> tagged_form list
	  (** The assumptions *)
      val concls : t -> tagged_form list
	  (** The conclusions *)
      val scope_of: t -> Scope.t
	  (** The scope of a sequent *)
      val sklm_cnsts: t -> Skolem.skolem_cnst list
	  (** The skolem constants of the sequent *)
      val sqnt_tyvars: t -> Basic.gtype list
	  (** All weak type variables that were generated in the sequent *)
      val sqnt_tag: t->Tag.t
	  (** The tag of the sequent. (This is the tag of the subgoal.) *)


(** {7 Operations on formulas} *)

      val get_asm : int -> t -> tagged_form
	  (** Get an assumption by position *)
      val get_cncl : int -> t -> tagged_form
	  (** Get a conclusion by position *)

      val get_tagged_asm : Tag.t -> t -> tagged_form
	  (** Get an assumption by tag *)
      val get_tagged_cncl : Tag.t -> t -> tagged_form
	  (** Get a conclusion by tag *)
      val get_tagged_form: Tag.t -> t -> tagged_form
	  (** Get a formula by tag *)

      val delete_asm : label -> t -> t
	  (** Delete an assumption by label *)
      val delete_cncl : label -> t -> t
	  (** Delete a conclusion by label *)

      val tag_to_index : Tag.t -> t -> int
	  (** Get the position of a formula from its tag *)
      val index_to_tag : int -> t -> Tag.t 
	  (** Get the tag of a formula from its position *)

    end

(** {7 Operations on sequent formulas} *)

val label_to_tag: label -> Sequent.t -> Tag.t
(** Convert a label to the tag of the formula it identifies. *)
val label_to_index: label -> Sequent.t -> int
(** 
   Convert a label to the position of the formula it identifies. If the label
   identifies an assumption, the position will be a negative integer.
*)

val get_label_asm : label -> Sequent.t -> tagged_form
(** Get the assumption identified by a label *)
val get_label_cncl : label -> Sequent.t -> tagged_form
(** Get the conclusion identified by a label *)
val get_label_form: label -> Sequent.t -> tagged_form
(** Get the formula identified by a label *)

(** {5 Goals} *)

type goal

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


(** {7 Applying Rules to Subgoals} *)

(** 
   The subgoal package.
   Manages the application of rules to the subgoals of a goal.
 *)
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
(*      val branch_tag: branch -> Tag.t *)
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
   [zip tacl branch]: Apply each of the tactics in [tacl] to the
   corresponding subgoal in branch.

   [zip [t1;t2;..;tn] (Branch [g1;g2; ..; gm])] is [Branch([t1 g1; t2
   g2; .. ;tn gn])] (with [t1 g1] first and [tn gn] last) if [n<m]
   then untreated subgoals are attached to the end of the new branch.
   if m<n then unused tactic are silently discarded.  typenv of new
   branch is that produced by the last tactic ([tn gn] in the
   example).  tag of the branch is the tag of the original branch.
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

(** Sequents and goals of the sequent calculus *)

type node = Subgoals.node
type branch = Subgoals.branch
type rule = Subgoals.node -> Subgoals.branch
type conv = Scope.t -> Basic.term -> thm

val postpone :  goal -> goal
val foreach: rule -> Subgoals.branch -> Subgoals.branch
val first_only: rule -> Subgoals.branch -> Subgoals.branch


(** {5 Tactics} *)

(** 
   {7 Tactic Information} 

   Tactics pass information by assigning values to elements of type
   {!Logic.info}, which are references to elements of type
   {!Logic.tag_record}. The standard tactics take parameters of type
   [Logic.info option], and provide the information only if it is
   requested (by an argument [(Some r)]).
 *)

(**
   The record holding information generated by tactics.
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

(** Type used to pass information from a tactic  *)
type info = tag_record ref

val make_tag_record: 
    Tag.t list -> Tag.t list -> Basic.term list -> tag_record
(** Construct a [tag_record] *)

val do_info: info option ->
  Tag.t list-> Tag.t list -> Basic.term list -> unit
(** 
   [do_info info gs fs ts]: 
   If [info] is [Some r] then [r:=mk_tag_record gs fs ts]
   otherwise do nothing.
 *)

val add_info: 
    info option ->
      Tag.t list-> Tag.t list -> Basic.term list -> unit
(** 
   [do_info info gs fs ts]: 
   If [info] is [Some r] then add [gs], [fs] and [ts] to 
   [!r.goals], [!r.forms] and [!r.terms] respectively.
   otherwise do nothing.
 *)


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




module Tactics :
    sig

(** 
   Tactics for proving goals.
 *)

(* 
   [check_term scp trm]
   Ensure that term [trm] is in the scope [scp].
   All identifiers must be bound to a quantifier or defined/declared 
   in a theory. 
   Free variables are not permitted.
 *)
(*
   val check_term: Scope.t -> Formula.form -> unit
 *)
(*
   [check_term_memo]
   Memoised version of [check_term].
 *)
(*
   val check_term_memo: 
   (string, bool) Lib.substype -> Scope.t -> Formula.form -> unit
 *)

(* apply a rule to a goal *)
(*      val goal_apply : rule -> goal -> goal *)

(** {5 Manipulating subgoal formulas} *)

      val lift_asm : info option -> label -> rule
(**
   [lift_asm id sqnt]: Move assumption with identifier [id] to to top
   of the assumptions of subgoal [sqnt]. Raise [Not_found] if identified
   formula is not in assumptions.
 *)
      val lift_concl : info option -> label -> rule
(**
   [lift_concl id sqnt]: Move conclusion with identifier [id] to to
   top of the conclusions of subgoal [sqnt]. Raise [Not_found] if
   identified formula is not in assumptions.
 *)
      val lift : info option -> label -> rule
(**
   [lift id sqnt]: Lift formula with label [id] to the top of
   assumption/conclusions. [lift] tries [lift_asm] then tries
   [lift_concl].  Doesn't change the formula tag.
 *)

      val copy_asm : info option -> label -> rule
(** 
   [copy_asm i] 

   Ai, asms|- C 
   -> 
   Ai, Ai, asms |- C
 *)

      val copy_cncl : info option -> label -> rule
(** 
   [copy_cncl i]

   A|- Ci, concls
   ->
   A|- Ci, Ci, concls
 *)


(** rotate assumptions conclusions *)

      val rotate_asms: info option -> rule
(** 
   [rotate_asm]

   A1, asms |- C
   ->
   asms, A1 |- C
 *)

      val rotate_cncls : info option -> rule
(** 
   [rotate_cncls]

   A|- C1, concls
   ->
   A|-  concls, C1
 *)

      val delete : info option -> label -> rule
(** 
   [delete x sq]: Delete assumption [x] or conclusion [x] from
   subgoal [sq].
 *)


(** {5 Logic rules}  *)

      val skip : info option -> rule
(*
   [skip]: The do nothing tactic
 *)


(** [cut x sq]: add theorem [x] to assumptions of [sq].

   asm |- cncl      --> x, asm |- cncl
 *)
      val cut : info option -> thm -> rule

(** 
   [basic i j sq]: if asm [i] is alpha-equal to cncl [j] of [sq].

   asm, a{_ i}, asm' |- concl, c{_ j}, concl'
   -->
   true if a{_ i}=c{_ j}
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


(** {5 Declarations and Definitions} *)

(**
   [cdecln]: Checked term declarations.
   Checking of type and term definitions and declarations

   [cdefn]: Checked Definitions.  
   Checking of type and term definitions and declarations
 *)
type cdefn
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


(*** Miscelaneous ***)

(*
   type cdefn=
   TypeAlias of 
   Basic.ident * string list * Basic.gtype option
   | TypeDef of ctypedef
   | TermDecln of Basic.ident * Basic.gtype
   | TermDef of Basic.ident * Basic.gtype 
 * (string*Basic.gtype) list * thm option
   type ctypedef =
   {
   type_name : Basic.ident;  (* name of new type *)
   type_args : string list;  (* arguments of new type *)
   type_rep: cdefn;          (* representation function *)
   type_thm: thm;          (* subtype theorem *)
   type_set: Formula.form       (* defining set *)
   }
   and saved_ctypedef =
   {
   stype_name : Basic.ident;  (* name of new type *)
   stype_args : string list;  (* arguments of new type *)
   stype_rep: saved_cdefn;          (* representation function *)
   stype_thm: saved_thm;          (* subtype theorem *)
   stype_set: Formula.saved_form      (* defining set *)
   }
 *)
    


(*
   val add_skolem_to_scope: 
   Basic.term -> Basic.gtype 
   -> Scope.t -> Scope.t

   val add_skolems_to_scope: 
   skolem_cnst list -> Scope.t -> Scope.t
 *)
