(*-----
 Name: simpset.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

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
val termnet_lt : rule -> rule -> bool

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


(* Printers *)

val print_rule: Printer.ppinfo -> rule -> unit
val print_rule_net: Printer.ppinfo -> rule Net.net -> unit

val print_aux: Printer.ppinfo -> simpset -> unit
val print: Printer.ppinfo -> simpset -> unit



(**
   {6 Adding/removing rules to a simpset.}
*)

(** [make_thm_rule thm]: make rule from theorem [thm]
 *)
val make_thm_rule :
    Logic.thm ->
      rule
val thm_to_entries :
    Gtypes.scope ->
      Logic.thm ->
	rule list


(** 
   [add_simp_rule set rules]: Add [rules] to simpset [set].
   [simp_add_thm scp set thms]: Add theorem [thm] to simpset [set].
   [simp_add_thms scp set thms]: Add theorems [thms] to simpset [set].
*)
val add_simp_rule :
    simpset -> rule list -> simpset
val simpset_add_thm :
    Gtypes.scope -> simpset -> Logic.thm -> simpset
val simpset_add_thms :
    Gtypes.scope -> simpset -> Logic.thm list -> simpset


val simpset_add_asm: 
    simpset -> Tag.t -> Logic.node -> simpset

val make_simp_asm :
  Tag.t ->
  Logic.node ->
  Tag.t *
      rule
val make_simp_asms :
  Tag.t list ->
  (Tag.t -> bool) ->
  Logic.node ->
  (Tag.t *
     rule ) list

val make_simp_asm_rule :
  Tag.t 
  -> Formula.form
      -> rule
val make_simp_asm_rules :
    (Tag.t * Formula.form -> bool) 
  -> (Tag.t * Formula.form) list 
      -> rule list
