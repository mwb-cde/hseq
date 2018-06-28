(*----
  Name: boolinduct.mli
  Copyright Matthew Wahab 2006-2016
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

(** Induction tactics *)

(** {7 Utility tactics} *)

val mini_scatter_tac:
  Logic.label -> Tactics.tactic
(** [mini_scatter_tac c goal]: Mini scatter tactic for
    induction.

    Scatter conclusion [c], using [falseA], [conjA], [existA],
    [trueC], [implC] and [allC].
*)

val mini_mp_tac:
  Logic.label -> Logic.label -> Tactics.tactic
(** [mini_mp_tac asm1 asm2 goal]: Modus Ponens for the induction
    tactics.

    Apply modus ponens to [asm1 = A => C] and [asm2 = A] to get [asm3
    = C]. info: aformulas=[asm3]; subgoals = [goal1]. Fails if [asm2]
    doesn't match the assumption of [asm1].
*)

(** {5 The induction tactic [induct_tac]} *)

val asm_induct_tac:
  Logic.label -> Logic.label -> Tactics.tactic
(** [asm_induct_tac a c]: Apply the induction scheme of
    assumption [a] to conclusion [c].

    See {!Boolinduct.induct_tac} for details about the form of the
    induction scheme.
*)

val induct_tac:
  ?c:Logic.label -> Logic.thm -> Tactics.tactic
(** [induct_tac ?c thm]: Apply induction theorem [thm] to conclusion
    [c] (or the first conclusion to succeed).

    Theorem [thm] must be in the form:
    {L ! P a .. b: (thm_asm P a .. b) => (thm_concl P a .. b)}
    where
    {L
    thm_concl P d .. e = (! x .. y: (pred x .. y) => (P d .. e x .. y))
    }
    The order of the outer-most bound variables is not relevant.

    The conclusion must be in the form:
    {L ! a .. b f .. g: (pred a .. b) => (C a .. b f ..g) }

    info:
    cformulas=the new conclusions (in arbitray order)
    subgoals=the new sub-goals (in arbitray order)
*)

(** {5 The induction tactic [induct_on]} *)

val induct_on:
  ?thm:Logic.thm -> ?c:Logic.label
  -> string -> Tactics.tactic
(** [induct_on ?thm ?c n]: Apply induction to the first
    universally quantified variable named [n] in conclusion [c] (or the
    first conclusion to succeed). The induction theorem is [thm], if
    given or the theorem [thm "TY_induct"] where [TY] is the name of
    the type constructor of [n].

    Theorem [thm] must be in the form:
    {L ! P a .. b: (thm_asm P a .. b) => (thm_concl P a .. b)}
    where
    {L
    thm_concl P a .. b= (! x: (P x a .. b))
    }
    The order of the outer-most bound variables is not relevant.

    The conclusion must be in the form:
    {L ! n f .. g: (C n f ..g) }
    [n] does not need to be the outermost quantifier.
*)

(** {7 Debugging information} *)

val induct_tac_bindings:
  Gtype.substitution -> Scope.t
  -> (Basic.binders list * 'a * Basic.term)
  -> Basic.term
  -> (Gtype.substitution * Term.Subst.t)
(** [induct_tac_bindings tyenv scp aterm cterm]: Extract bindings for
    the induction theorem in [aterm] from conclusion term [cterm] in
    type environment [tyenv] and scope [scp].

    [aterm] is in the form [(vars, asm, concl)], obtained by
    splitting a theorem of the form [! vars: asm => concl].

    Returns an updated type environment and a substitution containing
    bindings for the variables in [vars], with which to instantiate
    the induction theorem.

    This function is specialized for use by {!Boolinduct.induct_tac}.
*)

val induct_tac_solve_rh_tac:
  Logic.label -> Logic.label -> Tactics.tactic
(** [solve_rh_tac a c goal]: solve the right sub-goal of an
    induction tactic ([t2]).

    Formula [a] is of the form [ ! a .. b: A => C ]. Formula [c] is of
    the form [ ! a .. b x .. y: A => C] or of the form [ ! a .. b: A
    => (! x .. y: C)]

    Specialize [c], instantiate [a], implC [c] to get [a1] and [c1]
    mini_mp_tac [a] and [a2] to replace [a] with [a3] specialize [c1]
    again, intantiate [a3] basic [c1] and [a3].

    Completely solves the goal or fails.
*)

val induct_on_bindings:
  Gtype.substitution -> Scope.t
  -> Basic.binders
  -> (Basic.binders list * 'a * Basic.term)
  -> Basic.term
  -> (Gtype.substitution * Term.Subst.t)
(** [induct_on_bindings tyenv scp nbind aterm cterm]: Extract bindings
    for the induction theorem in [aterm] from conclusion term [cterm]
    in type environment [tyenv] and scope [scp], to induct on term
    [Bound nbind].

    [aterm] is in the form [(vars, asm, concl)], obtained by splitting
    a theorem of the form [! vars: asm => concl].

    [cterm] is in the form [! xs: body].

    [nbind] must be a universally quantified binder.

    Tries to unify [body] and [concl], returns an updated type
    environment and a substitution containing bindings for the
    variables in [vars], with which to instantiate the induction
    theorem.

    This function is specialized for use by [induct_on].
*)

val induct_on_solve_rh_tac:
  Logic.label -> Logic.label
  -> Tactics.tactic
(** [induct_on_solve_rh_tac a c goal]: solve the right sub-goal
    of an induction tactic ([t2]).

    Formula [a] is of the form [ ! a .. b: C ].  Formula [c] is of the
    form [ ! a .. b x .. y: C].

    Specialize [c], instantiate [a], basic [a] and [c].

    Completely solves the goal or fails.
*)

val basic_induct_tac:
  Logic.label -> Logic.thm -> Tactics.tactic
