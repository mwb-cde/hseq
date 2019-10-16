(*----
  Name: subgoalkit.mli
  Copyright Matthew Wahab 2015-2019
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

(**
   Toolkit for node branching.
*)

(** {5 Error reporting} *)

val subgoals_error: string -> Formula.t list -> exn
(** Make a logic error. *)
val add_subgoals_error: string -> Formula.t list -> exn -> 'a
(** Add a logic error to an existing list of errors. *)

module type SQNTS =
sig
  (** Type of unique identifiers. *)
  type tag_ty
  (** Make a unique tag. *)
  val tag_create: unit -> tag_ty
  (** Compare tags for equality. *)
  val tag_equals: tag_ty -> tag_ty -> bool

  (** Type of sequents. *)
  type sqnt_ty
  (** Tag of sequent. *)
  val sqnt_tag : sqnt_ty -> tag_ty

  (** Exceptions. *)
  exception No_subgoals
end

module Make : functor (T: SQNTS) ->
sig
  type tag_ty = T.tag_ty
  type env_ty = (Gtype.Subst.t * Changes.t)
  val mk_env : Gtype.Subst.t -> Changes.t -> env_ty

  (** {7 Nodes and branches} *)

  (** [node]: A node holds the subgoal to be solved and the type
      environment of the goal. A node also holds a tag, which is not
      visible.  *)
  type node
  val mk_node : tag_ty -> env_ty -> T.sqnt_ty -> node

  val node_tag : node -> tag_ty
  (** The tag of the node. *)
  val node_tyenv: node -> Gtype.Subst.t
  (** The type environment of the goal. *)
  val node_sqnt: node -> T.sqnt_ty
  (** The subgoal to be proved. *)
  val node_changes: node -> Changes.t
  (** The subgoals of the branch. *)

  (** [branch]: A branch is a list of subgoals and the type
      environment of the goal. A branch also holds the tag of the
      node from which it was produced.  *)
  type branch
  val mk_branch : tag_ty -> env_ty -> (T.sqnt_ty)list -> branch

  val branch_tag : branch -> tag_ty
  (** The tag of the branch. *)
  val branch_tyenv: branch -> Gtype.Subst.t
  (** The type environment of the branch. *)
  val branch_sqnts: branch -> T.sqnt_ty list
  (** The subgoals of the branch. *)
  val branch_changes: branch -> Changes.t
  (** The changes made in the branch. *)
  val branch_set_changes: branch -> Changes.t -> branch
  (** Update the changes made in the branch. *)

  val branch_node : node -> branch
  (** [branch_node node]: make a branch from [node]. *)

  (** {7 Utility functions} *)

  val merge_tyenvs:
    Gtype.Subst.t
    -> Gtype.Subst.t
    -> Gtype.Subst.t
  (** [merge tyenv1 tyenv2]: Merge type environments.

      Create a type environment [env3] which has the binding of each weak
      variable in [env1 + env2].

      Used to combine the type environment resulting from the application
      of a tactic with the original type environment of a goal.

      raise [Failure] if a variable ends up bound to itself.
  *)

  (** {7 Applying tactics} *)

  val apply: (node -> branch) -> node -> branch
  (** [apply tac node]: Apply tactic [tac] to [node].

      This is the work-horse for applying tactics. All other functions
      should call this to apply a tactic.

      Approach:
      {ol
      {- Create a new tag [ticket] and make it the tag of [node].}
      {- Apply tac to [node] getting branch [b].}
      {- If the tag of [b] is not [ticket] then fail.}
      {- Merge the type environment of [b] with [n']. (This may be
      unnecessary.) (Almost certainly unnecessary so not done.)}
      {- Make the original tag of [node] the tag of [b].}
      {- Return the branch [b].}}

      @raise [logicError] on failure.

      The ticket passing method should ensure that it is not possible
      to return an arbitrary branch as the result of a tactic. A
      tactic can only succeed if it returns a branch produced from the
      original node by passing through [apply]. Since the tags of
      nodes and branches can only be set from functions in module
      {!Logic}, this should ensure that a tactic cannot fake a
      result.  *)

  val apply_to_node:
    (node->branch->unit)option -> (node->branch) -> node -> branch
  (** [apply_to_node report tac n]: A wrapper around [apply] to allow
      reporting of the argument and result of a tactic.

      Evaluate [apply tac n] to get a branch [b] then,
      if [report] is given, evaluate [report n b]. Return [b].
  *)

  val apply_to_first:
    (node->branch->unit)option
    -> (node -> branch) -> branch -> branch
  (** [apply_to_first report tac (Branch(tg, tyenv, sqnts))]:
      Apply a tactic to the first subgoal in a branch.

      Apply tactic [tac] to [List.hd sqnts] using [apply_to_node].
      Replace original sequent with resulting branch to form the result.

      If [report] is given, apply to first subgoal and the branch
      resulting from application of [tac]. (This is to allow interactive
      proof support to print result of the tactic).

      @raise [No_subgoals] if [sqnts] is empty.
  *)

  val apply_to_each: (node->branch) -> branch -> branch
  (** [apply_to_each tac (Branch(tg, tyenv, sqnts))] Apply tactic
      [tac] to each subgoal in a branch.

      Apply tactic [tac] to each subgoal in [sqnts] using
      [apply_to_node].  Collapse the resulting branches, merging the type
      environments, to form the branch which is returned.

      @raise [No_subgoals] if [sqnts] is empty.
  *)

  val apply_zip: (node -> branch) list -> branch -> branch
  (** [apply_zip tacl branch]: Apply each of the tactics in [tacl] to the
      corresponding subgoal in branch.

      [zip [t1;t2;..;tn] (Branch [g1;g2; ..; gm])] is [Branch([t1 g1; t2
      g2; .. ;tn gn])] (with [t1 g1] first and [tn gn] last). The type
      environment from (t{_ i} g{_ i}) is used when evaluating (t{_ i+1}
      g{_ i+1}). The type environment of the returned branch is the type
      environment of [(tn gn)].

      If there are more subgoals than tactics (n < m) then untreated
      subgoals are attached to the end of the new branch. If there are
      more tactics then subgoals (m < n) then the unused tactics are silently
      discarded.
  *)

  val apply_fold:
    ('a -> node -> ('a * branch))  -> 'a -> branch -> ('a * branch)
(** [apply_fold rl i (Branch(tg, tyenv, sqnts))]: Apply tactic [tac] to
    each subgoal in a branch, folding the initial value across the
    nodes in the branch. Returns [(x, g)] where [x] is the result of
    the fold and [g] the new goal. The new goal is produced in the
    same way as [apply_to_each].

    @raise [No_subgoals] if [sqnts] is empty.
*)

end
