(*
   Author: Matthew Wahab
   Date: March 4, 2004
*)


(**
   Term Nets

   Structures to store data indexed by a term.  

   Lookup is by inexact matching of a given term against those
   indexing the data. Resulting list of data pair would then be
   subject to more exact mactching (such as unification) to select
   required data.

   Used to cut the number of elements that need to be
   considered by more expensive exact matching.
*) 

(** type [label]: Used to index data *)
type label = 
    Var 
  | App
  | Bound of Basic.quant_ty
  | Quant of Basic.quant_ty
  | Const of Basic.const_ty 
  | Cname of Basic.ident


(** type ['a net] : Node data, rest of net, Var tagged net (if any) *)
type 'a net =  
    Node of ('a list                  
(* data held at this node *)
	       * (label * 'a net) list  
(* nets tagged by labels *)
	       * ('a net) option )           
(* net tagged by Var *)

(** Make an empty net *)
val empty: unit -> 'a net
(** Test for an empty net. *)
val is_empty: 'a net -> bool

(**
   [label varp t]:
   Return the label for term t. Not used, [term_to_label] is better.
*)
val label: (Basic.term -> bool) -> Basic.term -> label

(**
   [term_to_label varp t]:

   Return the label for term [t] together with the remainder
   of the the term as a list of terms.

   [varp] determines which terms are treated as variables.

   [rst] is the list of terms built up by repeated calls
   to [term_to_label]. Initially, it should be [\[\]].

   examples:
   
   ?y: ! x: (x or z) and y  (with variable z)
   -->
   [Qnt(?); Qnt(!); App; App; Bound(!); Var; Bound(?)]

   ?y: ! x: (x or z) and y  (with no variables,  z is free)
   -->
   [Qnt(?); Qnt(!); App; App; Bound(!); Cname(z); Bound(?)]
 *)
val term_to_label : 
    (Basic.term -> bool) -> Basic.term -> Basic.term list 
      -> (label * Basic.term list)


(** [update f net trm]:

   Apply function [f] to the subnet of [net] identified by [trm] to update
   the subnet. Propagate the changes through the net. 
   If applying function [f] results in an empty subnet, than remove
   these subnets.
 *)
val update: 
    ('a net -> 'a net) -> (Basic.term -> bool) 
      -> 'a net -> Basic.term -> 'a net

(** Functions to use Nets *)

(** [lookup net t]:

   Return the list of items indexed by terms matching term [t].
   Orderd with the best matches first. 

   Term t1 is a better match than term t2 if variables in t1 occur
   deeper in its term structure than those for t2.  e.g. with variable
   x and t=(f 1 2), t1=(f x y) is a better match than t2=(x 1 2)
   because x occurs deeper in t1 than in t2. (t1 is likely to be
   rejected by exact matching more quickly than t2 would be.)
 *)
val lookup: 'a net -> Basic.term -> 'a list

(** [add varp net t r]:

   Add [r], indexed by term [t] with variables identified by [varp]
   to [net].
   Replaces but doesn't remove previous bindings of [t].
*)
val add: 
    (Basic.term -> bool) -> 'a net 
      -> Basic.term -> 'a
	-> 'a net

(** [insert order varp net t r]:

   Add data [r], indexed by term [t] with variables identified by [varp]
   to [net]. Data [r] is stored in the order defined by order.
   Replaces but doesn't remove previous bindings of [t]
*)
val insert: 
    ('a -> 'a -> bool) ->
      (Basic.term -> bool) -> 'a net 
	-> Basic.term -> 'a
	  -> 'a net

(** [delete varp net t test]:

   Remove data indexed by [t] in net and satisfying test.  Fails
   silently if [t] is not found.  Needs the same [varp] as used to add
   the term to the net.  
*) 
val delete: (Basic.term -> bool) -> 'a net
   -> Basic.term -> ('a -> bool) -> 'a net

