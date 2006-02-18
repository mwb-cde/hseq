(*-----
 Name: display.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(** Top-level pretty printers *)

open Basic

val print_term : Basic.term -> unit
val print_formula : Formula.t -> unit
val print_type : Basic.gtype -> unit
val print_theory: Theory.thy -> unit

val print_sqnt: Logic.Sequent.t -> unit
val print_node: Logic.node -> unit
val print_branch: Logic.branch -> unit

val print_thm: Logic.thm -> unit
val print_defn: Logic.Defns.cdefn -> unit

val print_prf: Goals.Proof.t -> unit

val print_fnident: Basic.ident -> unit

val print_subst : ('a, 'a) Hashtbl.t -> ('a -> string) -> unit
val print_error: Result.error -> unit
