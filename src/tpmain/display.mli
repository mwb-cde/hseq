(* functions for printing types/terms/errors etc *)
open Basic

val print_term : Basic.term -> unit
val print_formula : Formula.form -> unit
val print_type : Basic.gtype -> unit
val print_theory: Theory.thy -> unit

val print_sqnt: Logic.Sequent.t -> unit
val print_thm: Logic.thm -> unit
val print_defn: Defn.defn -> unit

val print_prf: Goals.prf -> unit

val print_fnident: Basic.ident -> unit

val print_subst : ('a, 'a) Hashtbl.t -> ('a -> string) -> unit
val print_error: Result.error -> unit
