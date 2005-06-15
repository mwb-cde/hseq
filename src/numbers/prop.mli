(*-----
 Name: prop.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)


type ('a, 'b) boolexpr =
  | Bool of bool
  | Not of ('a, 'b) boolexpr
  | And of ('a, 'b) boolexpr * ('a, 'b) boolexpr
  | Or of ('a, 'b) boolexpr * ('a, 'b) boolexpr
  | Implies of ('a, 'b) boolexpr * ('a, 'b) boolexpr
  | Iff of ('a, 'b) boolexpr * ('a, 'b) boolexpr
  | Equals of ('a, 'b) boolexpr * ('a, 'b) boolexpr
  | Bexpr of 'a
  | Var of 'b

val dest_var : ('a, 'b) boolexpr -> 'b

val mk_true: unit -> ('a, 'b) boolexpr
val mk_false: unit -> ('a, 'b) boolexpr
val mk_and : ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_not : ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_or : ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr 
val mk_implies: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_iff: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_equals: ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_bexpr : 'a -> ('a, 'b) boolexpr

val is_true : ('a, 'b) boolexpr -> bool
val is_false : ('a, 'b) boolexpr -> bool


val conj_to_list : ('a, 'b) boolexpr -> ('a, 'b) boolexpr list
val disj_to_list : ('a, 'b) boolexpr -> ('a, 'b) boolexpr list
val filter_empty : 'a list list -> 'a list list

val conj_to_cnf : ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val disj_to_dnf : ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list

val list_to_conj : ('a, 'b) boolexpr list -> ('a, 'b) boolexpr
val list_to_disj : ('a, 'b) boolexpr list -> ('a, 'b) boolexpr

val mk_dnf : ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val dnf_to_disj : ('a, 'b) boolexpr list list -> ('a, 'b) boolexpr

val mk_cnf : ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val cnf_to_conj : ('a, 'b) boolexpr list list -> ('a, 'b) boolexpr

(* replace boolean variables with (true or false) *)
val strip_vars: ('a, 'b) boolexpr -> ('a, 'b) boolexpr

(* Debugging *)

val dest_var : ('a, 'b) boolexpr -> 'b
val mk_and : ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_not : ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_or : ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_true : unit -> ('a, 'b) boolexpr
val mk_false : unit -> ('a, 'b) boolexpr
val mk_implies : ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_iff : ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_equals : ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val mk_bexpr : 'a -> ('a, 'b) boolexpr
val conj_to_list : ('a, 'b) boolexpr -> ('a, 'b) boolexpr list
val disj_to_list : ('a, 'b) boolexpr -> ('a, 'b) boolexpr list
val filter_empty : 'a list list -> 'a list list
val conj_to_cnf : ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val disj_to_dnf : ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val push_conj : ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val dnf_top :
  bool -> ('c, 'd) boolexpr -> ('c, 'd) boolexpr
val mk_dnf : ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val list_to_conj : ('a, 'b) boolexpr list -> ('a, 'b) boolexpr
val list_to_disj : ('a, 'b) boolexpr list -> ('a, 'b) boolexpr
val dnf_to_disj : ('a, 'b) boolexpr list list -> ('a, 'b) boolexpr
val push_disj : ('a, 'b) boolexpr -> ('a, 'b) boolexpr -> ('a, 'b) boolexpr
val cnf_top :
  bool -> ('c, 'd) boolexpr -> ('c, 'd) boolexpr
val mk_cnf : ('a, 'b) boolexpr -> ('a, 'b) boolexpr list list
val cnf_to_conj : ('a, 'b) boolexpr list list -> ('a, 'b) boolexpr
val strip_vars : ('a, 'b) boolexpr -> ('a, 'b) boolexpr

val reduce: ('a, 'b) boolexpr -> ('a, 'b)boolexpr
