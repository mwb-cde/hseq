(*----
 Name: drule.mli
 Copyright M Wahab 2005-2010
 Author: M Wahab  <mwb.cde@googlemail.com>

 This file is part of HSeq

 HSeq is free software; you can redistribute it and/or modify it under
 the terms of the Lesser GNU General Public License as published by
 the Free Software Foundation; either version 3, or (at your option)
 any later version.

 HSeq is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
 License for more details.

 You should have received a copy of the Lesser GNU General Public
 License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
----*)

(** Utility functions for writing tactics. *)

(*
(* first rule which can be applied to an assumption/conclusion *)
(*

val find_rule : 'a -> (('a -> bool) * 'b) list -> 'b

(* Apply functions *)
(* apply test and rules to each/all assumption/conclusion *)

val foreach_except:
    Tag.t list -> 
      ((Formula.form -> bool) * (Logic.label -> Logic.tactic)) list ->
	Logic.tactic

(* apply rules once *)
val foreach_conc_once :
    (Logic.label -> Logic.tactic) -> Logic.tactic
val foreach_asm_once :
    (Logic.label -> Logic.tactic) -> Logic.tactic
val foreach_once :
    (Logic.label -> Logic.tactic) -> Logic.tactic

val foreach_formula :
    ((Formula.form -> bool) * (Logic.label -> Logic.tactic)) list ->
      Logic.tactic

(*
val find_qnt_opt:
    ?exclude:(Logic.tagged_form -> bool)
  -> Basic.quant_ty
    -> ?f:Tag.t
      -> (Basic.term -> bool)
	-> Logic.tagged_form list
	  -> (Tag.t * Basic.binders list * Basic.term)
*)
(*
   [unify_sqnt_form varp trm ?f forms]
   Unify [trm] with formula [ft] in forms, return substitution and tag 
   of formula which matches ([ft] if given).

   [varp] determines what is a bindable variable.
   raise Not_found if no unifiable formula is found.
 *)
(*
val unify_sqnt_form:
    Gtypes.substitution 
  -> Scope.t
    -> (Basic.term -> bool)
      -> Basic.term
	-> ?exclude:(Logic.tagged_form -> bool)
	  -> ?f:Tag.t
	    -> Logic.tagged_form list 
	      -> (Tag.t * Term.substitution)
*)


(*
val match_formulas: 
    Gtypes.substitution
  -> Scope.t -> (Basic.term -> bool) 
    -> Basic.term -> Logic.tagged_form list -> Logic.label
(**
   [match_formulas scp varp t fs]

   Match a list of tagged formulas .  Return the tag of the first
   formula in [fs] to unify with term [t] in scope [scp].  [varp]
   determines which terms can be bound by unification.

   raise Not_found if no match.
 *)

val match_asm : 
    Gtypes.substitution
  -> Basic.term -> Logic.Sequent.t -> Logic.label
(** [match_asm t sq]

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
(** [match_concl t sq]

   Find a match for [t] in the assumptions of [sq].
   Return the tag of the first formula in the assumptions to unify 
   with term [t] in the scope of sequent [sq].

   raise Not_found if no match.

   Only free variables are bound in the matching process.
   e.g. in [<< !x. y and x >>] only [y] is a bindable variable 
   for the match.
 *)
*)

(* Predicates on terms *)
(**
   [qnt_opt_of qnt p t]
   apply predicate [p] to [b] where [(_, b)=strip_qnt qnt t].
 *)
(*
val qnt_opt_of: 
    Basic.quant_ty -> (Basic.term -> bool) -> Basic.term -> bool
*)
(**
   [dest_qnt_opt qnt d t]
   return [(vs, (d b))] 
   where [(vs, b)=strip_qnt qnt t].
 *)
(*
val dest_qnt_opt: 
    Basic.quant_ty 
  -> (Basic.term -> 'a) -> Basic.term -> (Basic.binders list * 'a) 
*)
(** [rebuild_qnt qs b]
   rebuild quantified term from quantifiers [qs] and body [b]

   e.g. [rebuild_qnt ["! x", "! y", "! z"] << b >>]
   ->
   [ << !x y z : b >> ]
 *)
(*
val rebuild_qnt: 
    Basic.quant_ty -> Basic.binders list -> Basic.term -> Basic.term
*)

(** 
   [find_formula p fs]: Return the first formula in [fs] to satisfy [p].

   raise Not_found if no such formula.
 *)
(*
val find_formula : ('a -> bool) -> 'a list -> 'a
*)
(*
   [find_asm p n]: 
   Return the first assumption of [n] to satisfy [p].

   [find_concl p n]: 
   Return the first conclusion of [n] to satisfy [p].

   raise Not_found if no such formula.
 *)
(*
val find_asm:
    ((Logic.tagged_form) -> bool) -> Logic.node -> Logic.tagged_form

val find_concl:
    ((Logic.tagged_form) -> bool) -> Logic.node -> Logic.tagged_form
*)
(*

(**
   [unify_formula_for_consts scp trm f]

   Unify [trm] with formula [f] returning the list of terms needed to
   make [trm] alpha-equal to [f] by instantiating the topmost
   quantifiers of [trm].

   raise Not_found, if no unifiable formula found.
 *)
val unify_formula_for_consts:
    Gtypes.substitution
  -> Scope.t
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
*)
*)

(*
*)

val foreach_asm :
    ((Formula.form -> bool) * (Logic.label -> Logic.tactic)) list ->
      Logic.tactic
val foreach_conc :
    ((Formula.form -> bool) * (Logic.label -> Logic.tactic)) list ->
      Logic.tactic

val foreach_asm_except : Tag.t list->
  ((Formula.form -> bool) * (Logic.label -> Logic.tactic)) list ->
    Logic.tactic

val foreach_conc_except : Tag.t list -> 
  ((Formula.form -> bool) * (Logic.label -> Logic.tactic)) list ->
    Logic.tactic

*)
