(* functions for printing types/terms/errors etc *)
open Basic

  val print_termlist : Term.term list -> unit
  val print_term : Term.term -> unit
  val print_typ : Gtypes.gtype -> unit
  val print_sqnt: Logic.sqnt -> unit
  val print_thm: Logic.thm -> unit

  val print_prf: Goals.prf -> unit

  val print_fnident: Basic.fnident -> unit

  val print_subst : ('a, 'a) Hashtbl.t -> ('a -> string) -> unit
  val print_error: Result.error -> unit
