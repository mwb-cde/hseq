val raiseError : string -> Term.term list -> 'a

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
    int * (int * Term.term) list ->
  Gtypes.scope -> Term.term -> (Supinf.expr * (int * (int * Term.term) list) )

val bool_type : Gtypes.gtype

val bterm_to_boolexpr :
  int * (int * Term.term) list ->
  int * (int * Term.term) list ->
  Gtypes.scope ->
  Term.term ->
  (Supinf.compfn * Supinf.expr * Supinf.expr, int) Prop.boolexpr *
  (int * (int * Term.term) list) * (int * (int * Term.term) list)

val term_to_boolexpr :
  Gtypes.scope ->
  Term.term ->
  (Supinf.compfn * Supinf.expr * Supinf.expr, int) Prop.boolexpr *
  (int * (int * Term.term) list) * (int * (int * Term.term) list) *
  Term.binders list
val expr_to_numterm : 'a * ('b * Term.term) list -> Supinf.expr -> Term.term
val compexpr_to_term :
  'a * ('b * Term.term) list ->
  Supinf.compfn -> Supinf.expr -> Supinf.expr -> Term.term
val boolexpr_to_bterm :
  'a * ('b * Term.term) list ->
  'c * ('d * Term.term) list ->
  (Supinf.compfn * Supinf.expr * Supinf.expr, int) Prop.boolexpr -> Term.term
val boolexpr_to_term :
  'a ->
  (Supinf.compfn * Supinf.expr * Supinf.expr, int) Prop.boolexpr ->
  'b * ('c * Term.term) list ->
  'd * ('e * Term.term) list -> Term.binders list -> Term.term


val simp_term_basic : Gtypes.scope -> Term.term -> Term.term
val simp_term_rewrite : Gtypes.scope -> Term.term -> Term.term
val simp_rewrite : Gtypes.scope -> Formula.form -> Logic.thm
val decide_term_basic : Gtypes.scope -> Term.term -> bool
val decide_term : Gtypes.scope -> Term.term -> Term.term
val decide_rewrite : Gtypes.scope -> Formula.form -> Logic.thm
