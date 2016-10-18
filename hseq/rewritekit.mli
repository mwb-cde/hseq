(*----
  Name: rewritekit.mli
  Copyright Matthew Wahab 2005-2016
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

(** Planned Rewriting

   Splits rewriting into two parts: a {e planner} constructs a plan
   which specifies how rewriting is to be carried out and a {e
   rewriter} which follows the plan. Rewriting is defined for {e
   nodes} and a rewriting plan specifies the rules to be applied to a
   node and the plans to be applied to the subnodes of the
   node. Rewriting plans support both top-down and bottom-up rewriting
   as well as rewriting spefic sub-nodes.

   This module provides rewriting plans ({!Rewritekit.plan}) and a
   generic rewriter, built by {!Rewritekit.Make}. The rewriter must be
   instantiated for a particular node type, by providing substitution,
   matching and other functions in a module of type
   {!Rewritekit.Data}. Rewrite planners are not provided here but are
   implemented seperately.
*)

exception Stop of exn
(**
   [Stop err]: Stop the rewriting with error [err]. Generally, this
   will cause the rewriter to return the result so far.
*)

exception Quit of exn
(**
   [Quit err]: Abandon rewriting with error [err]. This signals
   complete and unrecoverable failure.
*)

(** {5 Rewriting plans} *)

    (** The specification of a rewriting plan *)
type ('k, 'a)plan =
    Node of ('k, 'a)plan list
        (** The rewriting plan for a node *)
  | Keyed of 'k * (('k, 'a)plan list)
        (** The rewriting plan for a specified kind of node *)
  | Rules of 'a list
        (** The rules to use to rewrite the current node *)
  | Subnode of (int * ('k, 'a)plan)
        (** The rewriting plan for the branch of a node *)
  | Branches of ('k, 'a)plan list
        (** The rewriting plans for all branches of the node *)
  | Skip  (** The null rewriting plan *)
      (**
         A rewrite plan specificies how a node [n] is to
         be rewritten in terms of the rules to be applied
         to each sub-node [n].

         Plans can be keyed to particular kinds of node. If a plan
         [p=Keyed(k, ps)] is applied to node [n] which does not have
         key [k], the sub-nodes of [n] are searched for a node with
         the right key. The search is top-down and left-right and the
         test on keys uses predicate [A.is_key]. It is therefore
         possible to direct the search using plans and an appropriate
         value for [A.is_key].

         Rules for rewriting node [n] by plan [p],
         starting with the initial data [d]:

         {b [p=Node(ps)]}: Rewrite node [n] with plans [ps] and data
         [d]. The plans [ps] are used in order: if [ps = [x1; x2;
         .. ]], node [n] is rewritten with plan [x1] and data [d] to
         get node [n'] and data [d']. Plan [x2] is then used to
         rewrite node [n'], with data [d'], to get node [n''] and data
         [d''], and so on until all plans in [ps] have been used.

         {b [p=Keyed(k, ps)]}: If [is_key k n]: for each
         [x] in [ps], rewrite [n] with [x] and [d] to get
         new node [n'] and data [d'].

         If [not(is_key k n)]: Let [ns] be the subnodes of
         [n].  Try rewriting each [x] in [ns], starting
         with the left-most, and return the result of the
         first which does not raise [Not_found]. If all
         subnodes in [ns] fail with [Not_found] then raise
         [Not_found].

         {b [p=Rules rs]}: Rewrite node [n] with each rule
         in [rs] in order. If either [matches] or [subst]
         raise [Stop] then stop rewriting and return the
         result so far. If either [matches] or [subst]
         raise [Quit e] then abandon the attempt and fail,
         raising [e].

         {b [p=Subnode(i, p)]}: Replace the [i]'th subnode
         of [n] with the result of rewriting the subnode
         using plan [p]. (The left-most subnode of [n] is
         the [0]'th.)

         {b [p=Branches(ps)]}: Rewrite each subnode of [n]
         with the matching plan in [ps]. If there are more
         plans then subnodes, ingore the extra plans. If
         there are more subnodes, use the extra sub-nodes
         unchanged.

         {b [p=Skip]}: Do nothing, succeeding quietly.
                *)

(** {7 Functions on plans} *)

val mapping : ('a -> 'b) -> ('k, 'a)plan -> ('k, 'b)plan
    (** [mapping f p]: Map [f] over rules of plan [p]. *)

val iter : (('k, 'a)plan -> unit) -> ('k, 'a)plan -> unit
    (** [iter f p]: Iterate [f] over plan [p]. *)

(** {5 Rewriter data} *)

module type Data =
  sig
    (**
       The data to be used to instantiate the generic rewriter.

       Most of the data is concerned with the type
       {!Rewritekit.Data.node} for which rewriting is to be
       implemented and the matching and substitution functions,
       {!Rewritekit.Data.matches} and {!Rewritekit.Data.subst}, used
       by the rewriter. Rewrite rules are of type
       {!Rewritekit.Data.rule}.

       The basic operation is to match the left-hand-side of a rule
       against a node, generating a substitution of type
       {!Rewritekit.Data.substn}. This is passed to the substitution
       function, together with the right-hand-side of the rule, to get
       the result of the rewriting with the rule.

       The matching and substitution functions are also passed
       elements of {!Rewritekit.Data.data}, which can be used to
       gather additional data about the node. Functions
       {!Rewritekit.Data.add_data} and {!Rewritekit.Data.drop_data}
       can be used to manipulate the extra
       data. {!Rewritekit.Data.add_data} is called as each node is
       encountered and {!Rewritekit.Data.drop_data} is called when all
       operations on that node have finished. Functions
       {!Rewritekit.Data.matches} and {!Rewritekit.Data.subst} can
       also manipulate the data
     *)

    type node  (** Nodes to be rewritten *)
    type rule  (** Rewrite rules *)
    type data  (** Data to be passed to the matching and substitution
                  functions *)
    type substn (** The substition generated by a match *)
    type key    (** Node identifiers used in plans *)

    val key_of : node -> key
        (** [key_of n]: Get an identifier for [n] *)

    val is_key : key -> node -> bool
(** [is_key k n]: Test whether node [n] matches key [k] *)

    val num_subnodes : node -> int
(** [num_subnodes n]: The number of subnodes of [n]. *)

    val subnodes_of : node -> node list
(** [subnodes_of n]: Get the list of subnodes of [n] *)

    val set_subnodes : node -> node list -> node
(**
   [set_subnodes n xs]: Set subnodes of [n] to [xs], replacing the
   [i]'th subnode of [n] with the [i]'th element of [xs]. Fails with
   [Quit] if the number of new nodes in [xs] is not the same as the
   number of subnodes.
 *)

    val get_subnode : node -> int -> node
(**
   [get_subnode n i]: Get subnode [i] of node [n]. Subnodes are
   counted from the left and starting from [0]
 *)

    val set_subnode : node -> int -> node -> node
(**
   [set_subnode n i x]: set subnode [i] of [n] to [x]. Subnodes are
   counted from the left and starting from [0]
 *)

    val matches :
        data -> rule -> node -> (data * substn)
(** [matches data r n]: Try to match node [n] with rule [r].

   [data] is extra data to pass to [matches]. [r] is the rewrite rule
   being tried.  Returns [(new_data, env)] where [new_data] is to be
   passed on to the next application of [matches] or [subst] and [env]
   is the substitution to be passed to [subst].
 *)

    val subst :
        data -> rule -> substn -> (data * node)
(** [subst data env rhs]: Apply the substitutions in [env] to rule [r]
   to get a new node.

   [data] is extra data to pass to [subst], usually generated from the
   last invocation of [matches]. [env] is the substitution generated
   from the invocation of [matches] and [r] is the matched rewrite
   rule.  Returns [(new_data, n)] where [new_data] is to be passed on
   to the next application of [matches] or [subst] and [n] is the
   result of the substititution
 *)

    val add_data : data -> node -> data
(**
   [add_data data n]: Add to data from node [n]. Called for each node
   before any rewriting takes place. Resulting data is used in
   matching/substitutions for subnodes.
 *)

    val drop_data : (data * node) -> (data * node) -> data
(**
   [drop_data (d1, n1) (d2, n2)]: Drop data generated from a
   node. Called for each node before all rewriting of the node has
   completed. Node [n1] is the original node and [d1] is original
   data. Node [n2] and data [d2] are the result of rewriting [n1]
   using data [d1].
 *)
  end


(**
   {5 The generic rewriter}

   Module {!Rewritekit.Make} constructs a rewriter of type
   {!Rewritekit.T} from a module of type {!Rewritekit.Data}
   containing the rewriter data. The rewrite function to use is
   {!Rewritekit.T.rewrite}.
*)

module type T =
  sig
(**
   The type of a specific rewriter module.
*)


(** {7 Term data}

   Types and values from the {!Rewritekit.Data} passed to the rewriter.
*)

    type node (** Nodes to be rewritten *)
    type rule (** Rewrite rules *)
    type data (** Data to be passed to the matching and substitution
                  functions *)
    type substn (** The substition generated by a match *)
    type key (** Node identifiers used in plans *)

    val is_key : key -> node -> bool
(** [is_key k n]: Test whether node [n] matches key [k] *)

    val matches :
        data -> rule -> node -> (data * substn)
(** [matches data r n]: Try to match node [n] with rule [r].
*)

    val subst :
        data -> rule -> substn -> (data * node)
(** [subst data env rhs]: Apply the substitutions in [env] to rule [r]
   to get a new node.
*)

    val subnodes_of : node -> node list
(** [subnodes_of n]: Get the list of subnodes of [n] *)

    val set_subnodes : node -> node list -> node
(**
   [set_subnodes n xs]: Set subnodes of [n] to [xs], replacing the
   [i]'th subnode of [n] with the [i]'th element of [xs]. Fails with
   [Quit] if the number of new nodes in [xs] is not the same as the
   number of subnodes.
 *)
    val get_subnode : node -> int -> node
(**
   [get_subnode n i]: Get subnode [i] of node [n]. Subnodes are
   counted from the left and starting from [0]
 *)
    val set_subnode : node -> int -> node -> node
(**
   [set_subnode n i x]: set subnode [i] of [n] to [x]. Subnodes are
   counted from the left and starting from [0]
 *)

    val add_data : data -> node -> data
(**
   [add_data data n]: Add to data from node [n].
 *)

    val drop_data : (data * node) -> (data * node) -> data
(**
   [drop_data (d1, n1) (d2, n2)]: Drop data generated from a
   node.
*)

(** {7 Rewriting functions}

   Functions used in the rewriter. Not for general use.
*)

    val rewrite_first :
        data -> (key, rule)plan -> node list
          -> (data * (node list))
    val rewrite_branches :
        data -> node list -> (key, rule)plan list
          -> (data * (node list))
    val rewrite_rules :
        (data * node) -> rule list -> (data * node)
    val rewrite_aux :
        (data * node) -> (key, rule)plan
          -> (data * node)

(** {7 Toplevel rewrite function} *)

    val rewrite :
        data -> (key, rule)plan -> node -> (data * node)
(**
   [rewrite d p n]: Rewrite node [n] with plan [p], using [d] as the
   initial data to be passed to the rewrite functions. Returns [(d1,
   n1)] where [d1] is the data generated by rewriting and [n1] is the
   result of rewriting [n].
*)
  end

module Make :
functor (A : Data) ->
  (T with type data = A.data
  and type rule = A.rule
  and type node = A.node
  and type substn = A.substn
  and type key = A.key)
(** [Make(A)]: Instantiate the generic rewriter with data [A]. *)
