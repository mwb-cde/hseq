(* Derived logic rules and usefull functions *)
val ftag : Tag.t -> Logic.fident
val fnum : int -> Logic.fident

val asm_forms : Logic.sqnt -> Formula.form list
val concl_forms : Logic.sqnt -> Formula.form list

val get_asm: int -> Logic.sqnt -> Formula.form
val get_cncl: int -> Logic.sqnt -> Formula.form

(* first formula in assumption/conclusion satisfying a given predicate *)
(* search starts at (-1)/1 *)

val first : ('a -> bool) -> (Tag.t * 'a) list -> Logic.fident
val first_asm : (Formula.form -> bool) -> Logic.sqnt -> Logic.fident
val first_concl : (Formula.form -> bool) -> Logic.sqnt -> Logic.fident

(* find assumption/conclusion which are equivalent under alpha-conv*)

val find_basic : Logic.sqnt -> Logic.fident * Logic.fident

(* first rule which can be applied to an assumption/conclusion *)

val find_rule : 'a -> (('a -> bool) * 'b) list -> 'b

(* apply test and rules to each/all assumption/conclusion *)
val foreach_asm :
    ((Formula.form -> bool) * (Logic.fident -> Logic.rule)) list ->
      Logic.rule

val foreach_asm_except : Tag.t list->
  ((Formula.form -> bool) * (Logic.fident -> Logic.rule)) list ->
    Logic.rule

val foreach_conc :
    ((Formula.form -> bool) * (Logic.fident -> Logic.rule)) list ->
      Logic.rule

val foreach_conc_except : Tag.t list -> 
  ((Formula.form -> bool) * (Logic.fident -> Logic.rule)) list ->
    Logic.rule

(*
val foreach_in_sq :
    ((Formula.form -> bool) * (int -> Logic.rule)) list ->
      ((Formula.form -> bool) * (int -> Logic.rule)) list ->
	Logic.rule
*)

(* apply rules once *)
val foreach_conc_once :
    (Logic.fident -> Logic.rule) -> Logic.rule
val foreach_asm_once :
    (Logic.fident -> Logic.rule) -> Logic.rule
val foreach_once :
    (Logic.fident -> Logic.rule) -> Logic.rule

(* Utility functions *)

val make_consts: 
    Basic.binders list -> Term.substitution -> Basic.term list

val inst_list : 
    (Basic.term -> Logic.fident -> Logic.rule)
    -> Basic.term list -> Logic.fident -> Logic.rule

(** [get_one l e] 
   get single element of list [l].
   raise exception [e] if length of [l] ~= 1
*)
val get_one : 'a list -> exn -> 'a

(** [get_two l e] 
   get both elements of list [l].
   raise exception [e] if length of [l] ~= s
*)
val get_two : 'a list -> exn -> ('a * 'a)
