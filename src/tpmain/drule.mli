(* Derived logic rules and usefull functions *)

val asm_forms : Logic.sqnt -> Formula.form list
val concl_forms : Logic.sqnt -> Formula.form list

val get_asm: int -> Logic.sqnt -> Formula.form
val get_cncl: int -> Logic.sqnt -> Formula.form

(* first formula in assumption/conclusion satisfying a given predicate *)
(* search starts at (-1)/1 *)

val first : ('a -> bool) -> 'a list -> int
val first_asm : (Formula.form -> bool) -> Logic.sqnt -> int
val first_concl : (Formula.form -> bool) -> Logic.sqnt -> int

(* find assumption/conclusion which are equivalent under alpha-conv*)
val find_basic : Logic.sqnt -> int * int

(* the following apply the basic rules to the first assumption/conclusion
   which will succeed *)
(*
   val basic : Logic.rule
   val implI : Logic.rule
   val implE : Logic.rule
   val conjI : Logic.rule
   val conjE : Logic.rule
   val disjI : Logic.rule
   val disjE : Logic.rule
   val negC : Logic.rule
   val negA : Logic.rule
   val allI : Logic.rule
   val existI : Logic.rule
 *)

(*  modus ponens in terms of a logic rule *)
val mp_basic_rule : int -> Logic.rule 

(* mp applied to first possible instance *)
(*    val mp_rule : Logic.rule *)

val trueR : Logic.rule
val existE : Term.term -> Logic.rule
val allE : Term.term -> Logic.rule

(* delete a list of assumptions/conclusions *)
val deleten: int list -> Logic.rule

(* rewriting direction *)
val leftright : bool (* = false *)
val rightleft : bool (* = true *)

(* rewrite with a list of theorems *)
val rewrite_thm: Logic.thm list -> bool -> int -> Logic.rule

(* first rule which can be applied to an assumption/conclusion *)
val find_rule : 'a -> (('a -> bool) * 'b) list -> 'b

(* apply test and rules to each/all assumption/conclusion *)
val foreach_asm :
    ((Formula.form -> bool) * (int -> Logic.rule)) list ->
      Logic.rule

val foreach_asm_except : Tag.t list->
  ((Formula.form -> bool) * (int -> Logic.rule)) list ->
    Logic.rule

val foreach_conc :
    ((Formula.form -> bool) * (int -> Logic.rule)) list ->
      Logic.rule

val foreach_conc_except : Tag.t list -> 
  ((Formula.form -> bool) * (int -> Logic.rule)) list ->
    Logic.rule

(*
val foreach_in_sq :
    ((Formula.form -> bool) * (int -> Logic.rule)) list ->
      ((Formula.form -> bool) * (int -> Logic.rule)) list ->
	Logic.rule
*)

(* apply rules once *)
val foreach_conc_once :
    (int -> Logic.rule) -> Logic.rule
val foreach_asm_once :
    (int -> Logic.rule) -> Logic.rule
val foreach_once :
    (int -> Logic.rule) -> Logic.rule
