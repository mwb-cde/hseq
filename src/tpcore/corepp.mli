(* pretty printer information records *)

exception Error of string

(*  pretty printer information for function and type identifiers *)
type pp_rec = { prec: int; infix: bool; repr: string option }
type pp_state = 
    { id_info: Basic.ident -> pp_rec;
      type_info: Basic.ident -> pp_rec}

val mk_pp_rec :  int -> bool -> string option -> pp_rec
val empty_pp_rec :unit ->  pp_rec

val mk_empty_pp_state: unit-> pp_state
val mk_base_pp_state: unit-> pp_state

(* get PP information *)

val prec_of: pp_state -> Basic.id_selector -> Basic.ident ->  int
val is_infix: pp_state -> Basic.id_selector -> Basic.ident ->  bool

val list_print : ('a -> unit) -> (unit -> 'b) -> 'a list -> unit
val string_identifier : Basic.ident -> pp_rec -> string

