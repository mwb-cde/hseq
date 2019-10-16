(*----
  Name: subgoalkit.ml
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

let subgoals_error s t =
  Term.term_error s (List.map Formula.term_of t)
let add_subgoals_error s t es =
  raise (Report.add_error (subgoals_error s t) es)

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

module Make = functor (T: SQNTS) ->
struct

  type tag_ty = T.tag_ty
  let tag_create = T.tag_create
  let tag_equals = T.tag_equals

  type sqnt_ty = T.sqnt_ty
  let sqnt_tag = T.sqnt_tag

  type tyenv_ty = Gtype.Subst.t

  type env_ty = (tyenv_ty * Changes.t)
  let mk_env tyenv chngs = (tyenv, chngs)
  let dest_env (tyenv, chngs) = (tyenv, chngs)

  (** Subgoals: *)
  type node = Node of (tag_ty * env_ty * sqnt_ty)
  let mk_node tg env sqnt = Node(tg, env, sqnt)
  let node_tag (Node(tg, _, _)) = tg
  let node_env (Node(_, env, _)) = env
  let node_sqnt (Node(_, _, sqnt)) = sqnt

  let node_tyenv nd =
    let (tyenv, _) = dest_env (node_env nd) in
    tyenv
  let node_changes nd =
    let (_, chngs) = dest_env (node_env nd) in
    chngs
  let node_set_changes nd c =
    let env1 = mk_env (node_tyenv nd) c in
    mk_node (node_tag nd) env1 (node_sqnt nd)

  type branch =
      Branch of (tag_ty * env_ty * sqnt_ty list)

  let mk_branch tg env gs = Branch(tg, env, gs)

  let branch_tag (Branch(tg, _, _)) = tg
  let branch_env (Branch(_, env, _)) = env
  let branch_sqnts (Branch(_, _, s)) = s
  let branch_tyenv br =
    let (tyenv, _) = dest_env (branch_env br) in
    tyenv
  let branch_changes br =
    let (_, chngs) = dest_env (branch_env br) in
    chngs

  let branch_set_changes br c =
    mk_branch
      (branch_tag br) (mk_env (branch_tyenv br) c) (branch_sqnts br)

  (** [replace_branch_tag b tg]: Replace the tag of branch [b] with
      [tg].  *)
  let replace_branch_tag b tg =
    mk_branch tg (branch_env b) (branch_sqnts b)

  (** [branch_node node]: make a branch from [node]. *)
  let branch_node nd =
    mk_branch
      (node_tag nd) (node_env nd) [(node_sqnt nd)]

  (** [merge env1 env2]: merge type environments.

      Create a [env3] which has the binding of each weak variable in
      [env1 + env2].

      @raise [Failure] if a variable ends up bound to itself.
  *)
  let merge_tyenvs env1 env2 =
    let assign _ x env =
      try
        let y = Gtype.lookup_var x env2
        in
        if Gtype.equals x y
        then raise (Failure "Can't merge type environments")
        else Gtype.Subst.bind x y env
      with Not_found -> env
    in
    Gtype.Subst.subst_fold assign env1 env1

  (** [apply_basic tac node]: Apply tactic [tac] to [node].

      This is the work-horse for applying tactics. All other functions
      should call this to apply a tactic.

      Supports both basic tactic application and applying a fold.

      Approach:
      {ol
      {- Create a new tag [ticket].}
      {- Apply tac to [node] getting branch [b'].}
      {- If the tag of [b'] is not [ticket] then fail.}
      {- Merge the type environment of [b'] with [n']. (This may be
      unnecessary.) (Almost certainly unnecessary so not done.)}
      {- Return the branch formed from [b'] with the tag of [node].}}

      @raise [subgoals_error] on failure.
  *)
  let apply_basic tac d node =
    let ticket = tag_create() in
    let n1 =
      mk_node ticket
        (mk_env (node_tyenv node) (node_changes node)) (node_sqnt node)
    in
    let (value, new_branch) = tac d n1
    in
    if not (tag_equals ticket (branch_tag new_branch))
    then
      raise (subgoals_error "apply_basic: Invalid result from tactic" [])
    else
      begin
        let result = replace_branch_tag new_branch (node_tag n1)
        in
        (value, result)
      end

  let apply tac node =
    let app_aux _ n = ((), tac n) in
    let (_, result) = apply_basic app_aux () node
    in
    result

  (** [fold tac d (Node(tyenv, sqnt))]: Apply tactic [tac i] to node,
      getting [(x, result)].  If tag of goal [result] is the same as
      the tag of sqnt, then return [(x, result)].  Otherwise raise
      subgoals_error.
  *)
  let fold tac d node =
    apply_basic tac d node

  (** [apply_to_node tac (Node(tyenv, sqnt))]: Apply tactic [tac] to
      node, getting [result].  If tag of result is the same as the
      tag of sqnt, then return result.  Otherwise raise
      subgoals_error.  *)
  let apply_report report node branch =
    match report with
    | Some f -> f node branch
    | _ -> ()

  let apply_to_node report tac node =
    let result = apply tac node
    in
    apply_report report node result;
    result

  (** [apply_to_first tac (Branch(tg, tyenv, sqnts))]: Apply tactic
      [tac] to firsg sequent of [sqnts] using [apply_to_node].  replace
      original sequent with resulting branches.  return branch with tag
      [tg].

      @raise No_subgoals if [sqnts] is empty.
  *)
  let apply_to_first report tac br =
    let tg = branch_tag br
    and tyenv = branch_tyenv br
    and sqnts = branch_sqnts br
    and chngs = branch_changes br
    in
    match sqnts with
    | [] -> raise T.No_subgoals
    | (x::xs) ->
       let branch1 =
         apply_to_node report tac
           (mk_node (sqnt_tag x) (mk_env tyenv chngs) x)
       in
       mk_branch tg
         (branch_env branch1)
         ((branch_sqnts branch1) @ xs)

  (** [apply_to_each tac (Branch(tg, tyenv, sqnts, chngs))]: Apply
      tactic [tac] to each sequent in [sqnts] using [apply_to_node].
      replace original sequents with resulting branches.  return
      branch with tag [tg].

      @raise [No_subgoals] if [sqnts] is empty.
  *)
  let apply_to_each tac br =
    let tg = branch_tag br
    and tyenv = branch_tyenv br
    and sqnts = branch_sqnts br
    and chngs = branch_changes br
    in
    let rec app_aux ty gs cs lst =
      begin
        match gs with
        | [] ->
           mk_branch tg (mk_env ty (Changes.rev cs)) (List.rev lst)
        | (x::xs) ->
           let branch1 =
             apply_to_node None tac
               (mk_node (sqnt_tag x) (mk_env ty  chngs) x)
           in
           app_aux
             (branch_tyenv branch1) xs
             (Changes.rev_append (branch_changes branch1) cs)
             (List.rev_append (branch_sqnts branch1) lst)
      end
    in
    if sqnts = []
    then raise T.No_subgoals
    else app_aux tyenv sqnts (Changes.empty()) []

  (** [apply_fold tac iv (Branch(tg, tyenv, sqnts, chngs))]: Apply
      tactic [tac] to each subgoal in a branch, folding the initial
      value across the nodes in the branch. Returns [(x, g)] where [x]
      is the result of the fold and [g] the new goal. The new goal is
      produced in the same way as [apply_to_each].

      @raise [No_subgoals] if [sqnts] is empty.
  *)
  let apply_fold tac iv br =
    let tg = branch_tag br
    and tyenv = branch_tyenv br
    and sqnts = branch_sqnts br
    and chngs = branch_changes br
    in
    let rec app_aux ty d gs cs lst =
      begin
        match gs with
        | [] ->
           (d, mk_branch tg (mk_env ty (Changes.rev cs)) (List.rev lst))
        | (x::xs) ->
           let (v, branch1) =
             fold tac d (mk_node (sqnt_tag x) (mk_env ty chngs) x)
           in
           app_aux
             (branch_tyenv branch1) v xs
             (Changes.rev_append (branch_changes branch1) cs)
             (List.rev_append (branch_sqnts branch1) lst)
      end
    in
    if sqnts = []
    then raise T.No_subgoals
    else app_aux tyenv iv sqnts (Changes.empty()) []

  (** [apply_zip tacl branch]: Apply each of the tactics in [tacl] to the
      corresponding subgoal in branch.  e.g. [zip [t1;t2;..;tn] (Branch
      [g1;g2; ..; gm])] is Branch([t1 g1; t2 g2; .. ;tn gn]) (with [t1 g1]
      first and [tn gn] last) if n<m then untreated subgoals are attached to
      the end of the new branch.  if m<n then unused tactic are silently
      discarded.  typenv of new branch is that produced by the last tactic
      ([tn gn] in the example). The tag of the branch is the tag of the
      original branch.
  *)
  let apply_zip tacl br =
    let tg = branch_tag br
    and tyenv = branch_tyenv br
    and sqnts = branch_sqnts br
    and chngs = branch_changes br
    in
    let rec zip_aux ty tacs subgs cs lst =
      match (tacs, subgs) with
      | (_, []) ->
         mk_branch tg (mk_env ty (Changes.rev cs)) (List.rev lst)
      | ([], gs) ->
         mk_branch tg (mk_env ty (Changes.rev cs)) (List.rev_append lst gs)
      | (tac::ts, (g:sqnt_ty)::gs) ->
         let branch1 =
           apply_to_node None tac
             (mk_node (sqnt_tag g) (mk_env ty chngs) g)
         in
         zip_aux (branch_tyenv branch1) ts gs
           (Changes.rev_append (branch_changes branch1) cs)
           (List.rev_append (branch_sqnts branch1) lst)
    in
    match sqnts with
    | [] -> raise T.No_subgoals
    | _ -> zip_aux tyenv tacl sqnts (Changes.empty()) []

end (* Make *)
