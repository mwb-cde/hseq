(* Simplification sets *)


(** [rule]: A simpset rule.

   Made up of variables, optional condition, lhs , rhs, 
   source of the rule (theorem or assumption).
 *)
type rule =
    (Basic.binders list 
      * Basic.term option * Basic.term * Basic.term 
       *  Logic.rr_type)
val dest_rule : 
    rule -> 
      (Basic.binders list 
	 * Basic.term option * Basic.term * Basic.term 
	 * Logic.rr_type)

(** Simpsets 

   [simpset]: the type of simpsets.
   [basic]: the rewrite-rules stored in a single simpset.
   [next]: link to the next [simpset] in the list.

   [empty_set()]: Make a new, empty simpset.
*)
type simpset = { basic : rule Net.net; next : simpset option; }
val empty_set : unit -> simpset


(** [termnet_lt varp x y]:

   Less-than ordering of terms for use with Net.insert. Makes
   variables (for which [varp] is true) larger than any other term.
 *)
val termnet_lt : (Basic.term -> bool) -> rule -> rule -> bool

(** [add_rule rl s]: Add rule [rl] to set [s].

   [lookup trm s]: Find list of possible matches for term [trm] in set [s].

   [join s t]: Join sets s and t together.
   In set [join s t], set [s] will be searched before set [t]

   [split s]:  Split simpset [s] into two parts.
   fails if [s] is not joined to another set.
 *)
val add_rule : rule -> simpset -> simpset
val lookup : simpset -> Basic.term -> rule list
val join : simpset -> simpset -> simpset
val split : simpset -> simpset * simpset


(**
   [dest_rr_rule trm]: 
   Split [trm] into binders, condition, lhs, rhs 
   rules are of the form:
   [c=>(l=r)] or [l=r]
 *)
val dest_rr_rule : 
    Basic.term -> 
      (Basic.binders list 
	 * Basic.term option
	 * Basic.term 
	 * Basic.term)

(**
   [make_rule rl trm]
   make a rule from [trm], store as key for [src].
*)
val make_rule: Logic.rr_type -> Basic.term -> rule


val print_rule: rule -> unit
