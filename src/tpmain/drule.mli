(* Useful functions for writing tactics *)

(* Utility functions *)

val ftag : Tag.t -> Logic.label
val fnum : int -> Logic.label

(* untagged formulas of a sequent *)
val asm_forms : Logic.Sequent.t -> Formula.form list
val concl_forms : Logic.Sequent.t -> Formula.form list

(* tagged formulas of a sequent *)
val asms_of : Logic.Sequent.t -> Logic.tagged_form list
val concls_of : Logic.Sequent.t -> Logic.tagged_form list

(** [sequent g]
   get first subgoal of of goal [g].
 *)
val sequent : Logic.node -> Logic.Sequent.t

(** [scope_of g]
   get scope of first subgoal of of goal [g].
 *)
val scope_of : Logic.node -> Gtypes.scope

(** [typenv_of n]
   get type environment of node [n].
 *)
val typenv_of : Logic.node -> Gtypes.substitution


(** [get_asm i g]:
   Get assumption [i] of first sequent of goal [g].

   [get_cncl i g]:
   Get conclusion [i] of first sequent of goal [g].
 *)
val get_asm: Logic.label -> Logic.node -> Formula.form
val get_cncl: Logic.label -> Logic.node -> Formula.form

(** [get_tagged_asm i g]:
   Get assumption [i] of first sequent of goal [g].

   [get_tagged_cncl i g]:
   Get conclusion [i] of first sequent of goal [g].
 *)
val get_tagged_asm: Logic.label -> Logic.node -> Logic.tagged_form
val get_tagged_cncl: Logic.label -> Logic.node -> Logic.tagged_form


(** 
   [sqnt_tag n]: Get tag of the sequent of node n.
 *)
val sqnt_tag : Logic.Sequent.t -> Tag.t

(** 
   [node_tag n]: Get tag of the sequent of node n.
 *)
val node_tag : Logic.node -> Tag.t

(** [branch_tag b]: Tag of branch [b].

   [branch_typenv b]: type environment of branch [b].

   [branch_subgoals b]: subgoals of branch [b].

   [has_subgoals b]: [true] if [b] has subgoals, [false] otherwise

   [num_subgoals b]: number of subgoals in branch [b]
 *)
val branch_tag: Logic.branch -> Tag.t
val branch_tyenv: Logic.branch -> Gtypes.substitution
val branch_subgoals: Logic.branch -> Logic.Sequent.t list
val has_subgoals : Logic.branch -> bool
val num_subgoals : Logic.branch -> int

(** [mk_info()]
   make an empty information record.
 *)
val mk_info: unit -> Logic.info

(** [empty_info inf]
   empty information record [inf]
 *)
val empty_info: Logic.info -> Logic.info

(** [subgoals info]
   get subgoals of [info].
   equivalent to [(!info).goals]
 *)
val subgoals: Logic.info -> Tag.t list

(** [formulas info]
   get formulas of [info].
   equivalent to [(!info).forms]
 *)
val formulas: Logic.info -> Tag.t list

(** [constants info]
   get constants of [info].
   equivalent to [(!info).terms]
 *)
val constants: Logic.info -> Basic.term list

(** [make_consts l sb]
   make a list of terms suitable for instantiating a quantifier.
   [l] is the list of binders to be instantiated.
   [sb] stores the terms to be used (typically found by substitution)
 *)
val make_consts: 
    Basic.binders list -> Term.substitution -> Basic.term list

(**
   Tactics needed for primitive tactic building.
   [skip node]: Do nothing. Useful for converting a node to a branch.

   [foreach tac branch]: Apply [tac] to each node of branch.

   [seq tac1 tac2 node]: Apply tactic [tac1] to [node] then [tac2] to
   each of the resulting subgoals.  If [tac1] solves the goal (no
   subgoals), then [tac2] is not used.
 *)
val skip : Logic.rule
val foreach : Logic.rule -> Logic.branch -> Logic.branch
val seq : Logic.rule -> Logic.rule -> Logic.rule

(* 
   Utility tactics
 *)
(**
   [inst_list rule cs id goal]: 
   instantiate formula [id] in [goal] with constants [cs]
   using tactic [rule].
   do nothing if [cs] is empty.
 *)
val inst_list : 
    (Basic.term -> Logic.label -> Logic.rule)
  -> Basic.term list -> Logic.label -> Logic.rule

(* Search functions *)

(** [first p l]
   first formula in assumption or conclusion list [l]
   satisfying predicate [p] 

   Search starts at (-1)/1 
 *)
val first : ('a -> bool) -> (Tag.t * 'a) list -> Logic.label
val first_asm : (Formula.form -> bool) -> Logic.Sequent.t -> Logic.label
val first_concl : (Formula.form -> bool) -> Logic.Sequent.t -> Logic.label

(* first rule which can be applied to an assumption/conclusion *)

val find_rule : 'a -> (('a -> bool) * 'b) list -> 'b

(* Apply functions *)
(* apply test and rules to each/all assumption/conclusion *)
val foreach_asm :
    ((Formula.form -> bool) * (Logic.label -> Logic.rule)) list ->
      Logic.rule

val foreach_asm_except : Tag.t list->
  ((Formula.form -> bool) * (Logic.label -> Logic.rule)) list ->
    Logic.rule

val foreach_conc :
    ((Formula.form -> bool) * (Logic.label -> Logic.rule)) list ->
      Logic.rule

val foreach_formula :
    ((Formula.form -> bool) * (Logic.label -> Logic.rule)) list ->
      Logic.rule

val foreach_conc_except : Tag.t list -> 
  ((Formula.form -> bool) * (Logic.label -> Logic.rule)) list ->
    Logic.rule

val foreach_except:
    Tag.t list -> 
      ((Formula.form -> bool) * (Logic.label -> Logic.rule)) list ->
	Logic.rule

(*
   val foreach_in_sq :
   ((Formula.form -> bool) * (int -> Logic.rule)) list ->
   ((Formula.form -> bool) * (int -> Logic.rule)) list ->
   Logic.rule
 *)

(* apply rules once *)
val foreach_conc_once :
    (Logic.label -> Logic.rule) -> Logic.rule
val foreach_asm_once :
    (Logic.label -> Logic.rule) -> Logic.rule
val foreach_once :
    (Logic.label -> Logic.rule) -> Logic.rule

(* 
   [find_qnt_opt ?exclude qnt ?f pred forms] 

   Find the first formula in [forms] to satisfy [pred].
   The formula may by quantified by [qnt].
   Return the binders, the tag and the formula.

   if [f] is given, the formula must be tagged with [f].
   if [exclude] is given, ignore the formulas for which it is true.

   raise [Not_found] if no formula can be found which satisfies all the
   conditions.
 *)
val find_qnt_opt:
    ?exclude:(Logic.tagged_form -> bool)
  -> Basic.quant_ty
    -> ?f:Tag.t
      -> (Basic.term -> bool)
	-> Logic.tagged_form list
	  -> (Tag.t * Basic.binders list * Basic.term)

(*
   [unify_sqnt_form varp trm ?f forms]
   Unify [trm] with formula [ft] in forms, return substitution and tag 
   of formula which matches ([ft] if given).

   [varp] determines what is a bindable variable.
   raise Not_found if no unifiable formula is found.
 *)
val unify_sqnt_form:
    Gtypes.substitution 
  -> Gtypes.scope
    -> (Basic.term -> bool)
      -> Basic.term
	-> ?exclude:(Logic.tagged_form -> bool)
	  -> ?f:Tag.t
	    -> Logic.tagged_form list 
	      -> (Tag.t * Term.substitution)


(**
   [match_formulas scp varp t fs]

   Match a list of tagged formulas .  Return the tag of the first
   formula in [fs] to unify with term [t] in scope [scp].  [varp]
   determines which terms can be bound by unification.

   raise Not_found if no match.
 *)
val match_formulas: 
    Gtypes.substitution
  -> Gtypes.scope -> (Basic.term -> bool) 
    -> Basic.term -> Logic.tagged_form list -> Logic.label

(** [match_asm t sq]

   Find a match for [t] in the assumptions of [sq].
   Return the tag of the first formula in the assumptions to unify 
   with term [t] in the scope of sequent [sq].

   raise Not_found if no match.

   Only free variables are bound in the matching process.
   e.g. in [<< !x. y and x >>] only [y] is a bindable variable 
   for the match.
 *)
val match_asm : 
    Gtypes.substitution
  -> Basic.term -> Logic.Sequent.t -> Logic.label

(** [match_concl t sq]

   Find a match for [t] in the assumptions of [sq].
   Return the tag of the first formula in the assumptions to unify 
   with term [t] in the scope of sequent [sq].

   raise Not_found if no match.

   Only free variables are bound in the matching process.
   e.g. in [<< !x. y and x >>] only [y] is a bindable variable 
   for the match.
 *)
val match_concl :     
    Gtypes.substitution
  -> Basic.term -> Logic.Sequent.t -> Logic.label


(* Predicates on terms *)
(**
   [qnt_opt_of qnt p t]
   apply predicate [p] to [b] where [(_, b)=strip_qnt qnt t].
 *)
val qnt_opt_of: 
    Basic.quant_ty -> (Basic.term -> bool) -> Basic.term -> bool
(**
   [dest_qnt_opt qnt d t]
   return [(vs, (d b))] 
   where [(vs, b)=strip_qnt qnt t].
 *)
val dest_qnt_opt: 
    Basic.quant_ty 
  -> (Basic.term -> 'a) -> Basic.term -> (Basic.binders list * 'a) 

(** [rebuild_qnt k qs b]
   rebuild quantified term of kind k from quantifiers [qs] and body [b]

   e.g. [rebuild_qnt All ["x", "y", "z"] << b >>]
   ->
   [ << !x y z : b >> ]
 *)
val rebuild_qnt: 
    Basic.quant_ty -> Basic.binders list -> Basic.term -> Basic.term

(** 
   [find_formula p fs]: Return the first formula in [fs] to satisfy [p].

   raise Not_found if no such formula.
 *)
val find_formula : ('a -> bool) -> 'a list -> 'a
(*
   [find_asm p n]: 
   Return the first assumption of [n] to satisfy [p].

   [find_concl p n]: 
   Return the first conclusion of [n] to satisfy [p].

   raise Not_found if no such formula.
 *)
val find_asm:
    ((Logic.tagged_form) -> bool) -> Logic.node -> Logic.tagged_form

val find_concl:
    ((Logic.tagged_form) -> bool) -> Logic.node -> Logic.tagged_form

(**
   [unify_formula_for_consts scp trm f]

   Unify [trm] with formula [f] returning the list of terms needed to
   make [trm] alpha-equal to [f] by instantiating the topmost
   quantifiers of [trm].

   raise Not_found, if no unifiable formula found.
 *)
val unify_formula_for_consts:
    Gtypes.substitution
  -> Gtypes.scope
    -> (Basic.binders list * Basic.term) 
      -> Basic.term -> Basic.term list

(**
   [unify_concl_for_consts ?c trm g]

   if [c] is given, unify [trm] with the conclusion labelled [c],
   returning the list of terms needed to make [trm] alpha-equals
   to the conclusion by instantiating the topmost quantifiers of trm.

   [trm] must be universally quantified.
 *)
val unify_concl_for_consts:
    Basic.quant_ty
  -> ?c:Logic.label
    -> Basic.term -> Logic.node -> Basic.term list

(**
   [unify_asm_for_consts ?a qnt trm g]

   if [a] is given, unify [trm] with the assumption labelled [a],
   returning the list of terms needed to make [trm] alpha-equals
   to the conclusion by instantiating the topmost quantifiers of trm.

   [trm] must be quantified by [qnt].
 *)
val unify_asm_for_consts:
    Basic.quant_ty
  -> ?a:Logic.label
    -> Basic.term -> Logic.node -> Basic.term list
