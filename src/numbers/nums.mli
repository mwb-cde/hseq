(*-----
 Name: nums.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

val raiseError : string -> Basic.term list -> 'a

val num_thy : string
val plusid : Basic.ident
val minusid : Basic.ident
val multid : Basic.ident
val negid : Basic.ident
val maxid : Basic.ident
val minid : Basic.ident

val gtid : Basic.ident
val geqid : Basic.ident
val ltid : Basic.ident
val leqid : Basic.ident

val numterm_to_expr :
    int * (int * Basic.term) list ->
  Gtypes.scope -> Basic.term -> (Supinf.expr * (int * (int * Basic.term) list) )

val bool_type : Basic.gtype

val bterm_to_boolexpr :
  int * (int * Basic.term) list ->
  int * (int * Basic.term) list ->
  Gtypes.scope ->
  Basic.term ->
  (Supinf.compfn * Supinf.expr * Supinf.expr, int) Prop.boolexpr *
  (int * (int * Basic.term) list) * (int * (int * Basic.term) list)

val term_to_boolexpr :
  Gtypes.scope ->
  Basic.term ->
  (Supinf.compfn * Supinf.expr * Supinf.expr, int) Prop.boolexpr *
  (int * (int * Basic.term) list) * (int * (int * Basic.term) list) *
  Basic.binders list
val expr_to_numterm : 'a * ('b * Basic.term) list -> Supinf.expr -> Basic.term
val compexpr_to_term :
  'a * ('b * Basic.term) list ->
  Supinf.compfn -> Supinf.expr -> Supinf.expr -> Basic.term
val boolexpr_to_bterm :
  'a * ('b * Basic.term) list ->
  'c * ('d * Basic.term) list ->
  (Supinf.compfn * Supinf.expr * Supinf.expr, int) Prop.boolexpr -> Basic.term
val boolexpr_to_term :
  'a ->
  (Supinf.compfn * Supinf.expr * Supinf.expr, int) Prop.boolexpr ->
  'b * ('c * Basic.term) list ->
  'd * ('e * Basic.term) list -> Basic.binders list -> Basic.term


val simp_term_basic : Gtypes.scope -> Basic.term -> Basic.term
val simp_term_rewrite : Gtypes.scope -> Basic.term -> Basic.term
val simp_rewrite : Gtypes.scope -> Formula.form -> Logic.thm
val decide_term_basic : Gtypes.scope -> Basic.term -> bool
val decide_term : Gtypes.scope -> Basic.term -> Basic.term
val decide_rewrite : Gtypes.scope -> Formula.form -> Logic.thm
