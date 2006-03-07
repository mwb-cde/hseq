(*-----
 Name: simputils.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** Utility functions for the simplifier. *)


val is_variable: Basic.binders list -> Basic.term -> bool
(** 
   [is_variable qnts x]: Test for variables (universal quantifiers) in
   an entry.
*)

val equal_upto_vars: 
    (Basic.term -> bool) -> Basic.term -> Basic.term -> bool
(**
   [equal_upto_vars varp x y]: Terms [x] and [y] are equal upto the
   position of the terms for which [varp] is true (which are
   considered to be variables).

   This is used to determine whether a rewrite- or simp-rule could
   lead to an infinite loop (e.g. [ |- (x and y) = (y and x) ] ).
*)

val find_variables :
    (Basic.binders -> bool) ->
      Term.substitution -> Basic.term -> Term.substitution
(** 
   [find_variables is_var vars trm]: Find all subterms [t] of [trm]
   s.t. [(is_var t)] is true. Add [t] to [vars] then return [vars].
*)

val check_variables :
    (Basic.binders -> bool) -> Term.substitution -> Basic.term -> unit
(**
   [check_variables is_var vars trm]:
   Check that all subterms [t] of [trm] s.t. [is_var t] 
   are in [vars].
*)

val strip_qnt_cond :
    Basic.term -> Basic.binders list * Basic.term option * Basic.term
(** 
  [strip_qnt_cond trm]: Split rule [trm] into variable binders,
  condition, equality rules are of the form: [a=>c] or [c] 
*)


val apply_merge_list : ('a -> 'a list) -> 'a list -> 'a list
(**
   [apply_merge_list f lst]: Apply [f] to each element [x] in [lst]
   and repeat for the resulting list. Concatenate the list of lists
   that result. If [f x] fails, treat [[x]] as the result.
*)

val fresh_thm : Logic.thm -> bool
(** 
   [fresh_thm th]: Test whether theorem [th] is fresh in the global scope. 
*)

val simp_beta_conv: Logic.conv
(**
   [simp_beta_conv scp t]: Apply {!Logic.Conv.beta_conv} to [t] if
   [t] is of the form << (% x: F) a >>.
   Raise [Failure] if [t] is not an application.
*)

