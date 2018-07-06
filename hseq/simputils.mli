(*----
  Name: simputils.mli
  Copyright Matthew Wahab 2005-2018
  Author: Matthew Wahab <mwb.cde@gmail.com>

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

(** Utility functions for the simplifier. *)


val is_variable: Term.Binder.t list -> Term.term -> bool
(** [is_variable qnts x]: Test for variables (universal quantifiers)
    in an entry.
*)

val equal_upto_vars:
  (Term.term -> bool) -> Term.term -> Term.term -> bool
(** [equal_upto_vars varp x y]: Terms [x] and [y] are equal upto the
    position of the terms for which [varp] is true (which are
    considered to be variables).

    This is used to determine whether a rewrite- or simp-rule could
    lead to an infinite loop (e.g. [ |- (x and y) = (y and x) ] ).
*)

val find_variables:
  (Term.Binder.t -> bool) ->
  Term.Subst.t -> Term.term -> Term.Subst.t
(** [find_variables is_var vars trm]: Find all subterms [t] of [trm]
    s.t. [(is_var t)] is true. Add [t] to [vars] then return [vars].
*)

val check_variables:
  (Term.Binder.t -> bool) -> Term.Subst.t -> Term.term -> bool
(** [check_variables is_var vars trm]: Check that all subterms [t] of
    [trm] s.t. [is_var t] are in [vars].
*)

val strip_qnt_cond:
  Term.term -> Term.Binder.t list * Term.term option * Term.term
(** [strip_qnt_cond trm]: Split rule [trm] into variable binders,
    condition, equality rules are of the form: [a=>c] or [c]
*)


val apply_merge_list: ('a -> 'a list) -> 'a list -> 'a list
(** [apply_merge_list f lst]: Apply [f] to each element [x] in [lst]
    and repeat for the resulting list. Concatenate the list of lists
    that result. If [f x] fails, treat [[x]] as the result.
*)

val simp_beta_conv: Scope.t -> Logic.conv
(** [simp_beta_conv scp t]: Apply {!Logic.Conv.beta_conv} to [t] if
    [t] is of the form << (% x: F) a >>.  Raise [Failure] if [t] is not
    an application.
*)
