module Info :
    sig

(* pretty printer information records *)

      exception Error of string

      type fixity=Parserkit.Info.fixity
      val nonfix : Parserkit.Info.fixity
      val infix : Parserkit.Info.fixity
      val prefix : Parserkit.Info.fixity
      val suffix : Parserkit.Info.fixity


(*  pretty printer information for function and type identifiers *)

      type pp_rec = 
	  { prec: int; 
	    fixity: fixity;
	    repr: string option }

      type pp_state = 
	  { id_info: Basic.fnident -> pp_rec;
	    type_info: Basic.fnident -> pp_rec}

      val mk_pp_rec :  int -> fixity -> string option -> pp_rec
      val empty_pp_rec :unit ->  pp_rec

      val mk_empty_pp_state: unit-> pp_state
      val mk_base_pp_state: unit-> pp_state

(* get PP information *)

      val prec_of: pp_state -> Basic.id_selector -> Basic.fnident ->  int
      val fixity_of: pp_state -> Basic.id_selector -> Basic.fnident ->  fixity

      val is_infix: fixity -> bool
      val is_prefix: fixity -> bool
      val is_suffix: fixity -> bool

    end

(* functions for printing types/terms/errors etc *)
open Basic
open Info

val list_print : ('a -> unit) -> (unit -> 'b) -> 'a list -> unit
val string_identifier : Basic.fnident -> pp_rec -> string

val print_termlist : Info.pp_state -> Term.term list -> unit
val print_term : Info.pp_state -> Term.term -> unit
val print_type : Info.pp_state -> Gtypes.gtype -> unit

val print_fnident: Basic.fnident -> unit

val print_subst : ('a, 'a) Hashtbl.t -> ('a -> string) -> unit
val print_error: Result.error -> unit

