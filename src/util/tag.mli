(* 
   Tags: 
   unique identifiers with which to identify
   sequents in a goal and formulas in a sequent
 *)
type t

val named: string->t
val name: t->string
val null:t
val create: unit->t

val equal: t->t->bool
