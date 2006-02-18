(*-----
 Name: nums.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

val error : string -> Basic.term list -> exn

val num_thy : string
val plusid : Ident.t
val minusid : Ident.t
val multid : Ident.t
val negid : Ident.t
val maxid : Ident.t
val minid : Ident.t

val gtid : Ident.t
val geqid : Ident.t
val ltid : Ident.t
val leqid : Ident.t

val numterm_to_expr :
    int * (int * Basic.term) list 
  -> Scope.t -> Basic.term 
    -> (Exprs.expr * (int * (int * Basic.term) list) )

val bool_type : Basic.gtype

val bterm_to_prop :
  Scope.t ->
  int * (int * Basic.term) list ->
  int * (int * Basic.term) list ->
  Basic.term ->
  (Supinf.compfn * Exprs.expr * Exprs.expr, int) Prop.boolexpr *
  (int * (int * Basic.term) list) * (int * (int * Basic.term) list)

val term_to_prop :
  Scope.t ->
  Basic.term ->
  (Supinf.compfn * Exprs.expr * Exprs.expr, int) Prop.boolexpr *
  (int * (int * Basic.term) list) * (int * (int * Basic.term) list) *
  Basic.binders list
val expr_to_numterm : 'a * ('b * Basic.term) list -> Exprs.expr -> Basic.term
val comprsn_to_term :
  'a * ('b * Basic.term) list ->
  Supinf.compfn -> Exprs.expr -> Exprs.expr -> Basic.term
val prop_to_bterm :
  'a * ('b * Basic.term) list ->
  'c * ('d * Basic.term) list ->
  (Supinf.compfn * Exprs.expr * Exprs.expr, int) Prop.boolexpr -> Basic.term
val prop_to_term :
    'a ->
    'b * ('c * Basic.term) list ->
    'd * ('e * Basic.term) list ->
    Basic.binders list ->
    (Supinf.compfn * Exprs.expr * Exprs.expr, int) Prop.boolexpr ->
    Basic.term



(**
   [reduce_conv scp t]: Conversion to simplify term [t].
   Term [t] must be in the set which can be dealt which by SupInf.
   Returns a theorem of the form [ |- t=t' ] where [t'] is the 
   simplified term.

   [decide_conv scp t]: Conversion to decide whether [t] is true.
   Term [t] must be in the set which can be dealt which by SupInf.

   Returns a theorem of the form [ |- t=t' ] where [t'] is 
   either true or false.
*)
val simp_term_basic : Scope.t -> Basic.term -> Basic.term
val reduce_conv: Scope.t -> Basic.term -> Logic.thm

val decide_term_basic : Scope.t -> Basic.term -> bool
val decide_conv: Scope.t -> Basic.term -> Logic.thm

(**
   [is_numterm scp t]: [true] if term [t] is a number, an integer variable
   or the application of an integer function (for which [is_num_fn] is [true])
   to numterms.

   Used to see whether term [t] is suitable for simplifiying (with
   Exprs.simp).
*)
val is_numterm: Scope.t -> Basic.term -> bool

(**
   [is_compterm scp t]: [true] if term [t] is a comparison between
   number terms.

   Used to see whether term [t] is suitable for simplifiying (with
   Exprs.simp).
*)
val is_compterm: Scope.t -> Basic.term -> bool

(**
   [is_boolterm scp t]: [true] if term [t] is a quantifier-free
   boolean term in which subterms satisfy either [is_boolterm] or
   [is_compterm].

   Used to see whether term [t] is suitable for the decision procedure
   Supinf.decide.
*)
val is_boolterm: Scope.t -> Basic.term -> bool

(**
   [is_presburger scp t]: [true] if term [t] is a possibly universally
   quantified boolean term in which subterms are 
   quantifier-free boolean terms (satisfying [is_boolterm]) 

   Used to see whether term [t] is suitable for the decision procedure
   Supinf.decide.
*)
val is_presburger: Scope.t -> Basic.term -> bool

(** 
   [arith_conv scp trm]: Use the decision procedure to simplify term [trm].
   
   if [trm] satisfies [is_numterm], calls [reduce_conv]
   otherwise calls [decide_conv].

   Fails if [trm] doesn't satisfy [is_presburger] or [is_numterm].
*)
val arith_conv : Scope.t -> Basic.term -> Logic.thm


(**
   Conversions for use with the simplifier.

   [simp_reduce_conv scp asms trm]: call [reduce_conv] with [trm], 

   [simp_decide_conv scp asms trm]: call [decide_conv] with [asms => trm], 
   pulling topmost quantifiers of term to the outside.

   [simp_arith_conv scp asms trm]: call [arith_conv] with [asms => trm], 
   pulling topmost quantifiers of term to the outside.
*)
val simp_reduce_conv : 
    Scope.t -> Basic.term list -> Basic.term -> Logic.thm
val simp_decide_conv : 
    Scope.t -> Basic.term list -> Basic.term -> Logic.thm
val simp_arith_conv : 
    Scope.t -> Basic.term list -> Basic.term -> Logic.thm


(* 
   Debugging information
*)


val error : string -> Basic.term list -> exn
val add_error : string -> Basic.term list -> exn -> 'a
val num_type : Basic.gtype
type varenv = int * (int ref * Basic.term) list
val new_env : unit -> int * 'a list
val var_ctr : 'a * 'b -> 'a
val var_env : 'a * 'b -> 'b
val get_var : 'a * ('b * Basic.term) list -> Basic.term -> 'b
val get_index : 'a * ('b * 'c) list -> int -> 'c
val add_var :
  int * (int * Basic.term) list ->
  Basic.term -> int * (int * (int * Basic.term) list)
val mk_plus : Exprs.expr list -> Exprs.expr
val mk_max : Exprs.expr list -> Exprs.expr
val mk_min : Exprs.expr list -> Exprs.expr
val mk_mult : Exprs.expr list -> Exprs.expr
val mk_minus : Exprs.expr list -> Exprs.expr
val mk_negate : Exprs.expr list -> Exprs.expr
val num_thy : string
val plusid : Ident.t
val minusid : Ident.t
val multid : Ident.t
val negid : Ident.t
val maxid : Ident.t
val minid : Ident.t
val num_fns : (Ident.t * (Exprs.expr list -> Exprs.expr)) list
val is_num_fn : Ident.t -> bool
val get_num_fn : Ident.t -> Exprs.expr list -> Exprs.expr
val numterm_to_expr :
  int * (int * Basic.term) list ->
  Scope.t -> Basic.term -> Exprs.expr * (int * (int * Basic.term) list)
val mk_num_equals : 'a -> 'b -> (Supinf.compfn * 'a * 'b, 'c) Prop.boolexpr
val mk_gt : 'a -> 'b -> (Supinf.compfn * 'a * 'b, 'c) Prop.boolexpr
val mk_geq : 'a -> 'b -> (Supinf.compfn * 'a * 'b, 'c) Prop.boolexpr
val mk_lt : 'a -> 'b -> (Supinf.compfn * 'a * 'b, 'c) Prop.boolexpr
val mk_leq : 'a -> 'b -> (Supinf.compfn * 'a * 'b, 'c) Prop.boolexpr
val gtid : Ident.t
val geqid : Ident.t
val ltid : Ident.t
val leqid : Ident.t
val comp_fns :
  (Ident.t * ('a -> 'b -> (Supinf.compfn * 'a * 'b, 'c) Prop.boolexpr))
  list
val is_equals :
  Scope.t -> Basic.gtype -> Ident.t -> Basic.term -> Basic.term -> bool
val is_num_equals :
  Scope.t -> Ident.t -> Basic.term -> Basic.term -> bool
val is_bool_equals :
  Scope.t -> Ident.t -> Basic.term -> Basic.term -> bool

val is_bool_app :
  Scope.t -> Ident.t -> Basic.term list -> bool
val is_num_app : 
  Scope.t -> Ident.t -> Basic.term list -> bool
val is_comp_app : 
  Scope.t -> Ident.t -> Basic.term list -> bool

val is_comp_fn : Ident.t -> bool
val get_comp_fn :
  Scope.t ->
  Ident.t ->
  Basic.term ->
  Basic.term -> 'a -> 'b -> (Supinf.compfn * 'a * 'b, 'c) Prop.boolexpr
val strip_typed : Basic.term -> Basic.term
val compterm_to_comprsn :
  int * (int * Basic.term) list ->
  Scope.t ->
  Ident.t ->
  Basic.term list ->
  (Supinf.compfn * Exprs.expr * Exprs.expr, 'a) Prop.boolexpr *
  (int * (int * Basic.term) list)
val bool_type : Basic.gtype
val strip_univs :
  Scope.t ->
  int * (int * Basic.term) list ->
  int * (int * Basic.term) list ->
  Basic.term ->
  Basic.binders list * Basic.term * (int * (int * Basic.term) list) *
  (int * (int * Basic.term) list)
val mk_not : ('a, 'b) Prop.boolexpr list -> ('a, 'b) Prop.boolexpr
val mk_and : ('a, 'b) Prop.boolexpr list -> ('a, 'b) Prop.boolexpr
val mk_or : ('a, 'b) Prop.boolexpr list -> ('a, 'b) Prop.boolexpr
val mk_implies : ('a, 'b) Prop.boolexpr list -> ('a, 'b) Prop.boolexpr
val mk_iff : ('a, 'b) Prop.boolexpr list -> ('a, 'b) Prop.boolexpr
val mk_bool_equals : ('a, 'b) Prop.boolexpr list -> ('a, 'b) Prop.boolexpr
val bool_fns :
  (Ident.t * (('a, 'b) Prop.boolexpr list -> ('a, 'b) Prop.boolexpr))
  list
val is_bool_fn : Ident.t -> bool
val get_bool_fn :
  Scope.t ->
  Ident.t ->
  Basic.term list -> ('a, 'b) Prop.boolexpr list -> ('a, 'b) Prop.boolexpr
val bterm_to_prop :
  Scope.t ->
  int * (int * Basic.term) list ->
  int * (int * Basic.term) list ->
  Basic.term ->
  (Supinf.compfn * Exprs.expr * Exprs.expr, int) Prop.boolexpr *
  (int * (int * Basic.term) list) * (int * (int * Basic.term) list)
val term_to_prop :
  Scope.t ->
  Basic.term ->
  (Supinf.compfn * Exprs.expr * Exprs.expr, int) Prop.boolexpr *
  (int * (int * Basic.term) list) * (int * (int * Basic.term) list) *
  Basic.binders list
val expr_to_numterm : 'a * ('b * Basic.term) list -> Exprs.expr -> Basic.term
val comprsn_to_term :
  'a * ('b * Basic.term) list ->
  Supinf.compfn -> Exprs.expr -> Exprs.expr -> Basic.term
val prop_to_bterm :
  'a * ('b * Basic.term) list ->
  'c * ('d * Basic.term) list ->
  (Supinf.compfn * Exprs.expr * Exprs.expr, int) Prop.boolexpr -> Basic.term
val prop_to_term :
    'a ->
    'b * ('c * Basic.term) list ->
    'd * ('e * Basic.term) list ->
    Basic.binders list ->
    (Supinf.compfn * Exprs.expr * Exprs.expr, int) Prop.boolexpr ->
    Basic.term
val simp_term_basic : Scope.t -> Basic.term -> Basic.term
val decide_term_basic : Scope.t -> Basic.term -> bool
val reduce_conv : Scope.t -> Basic.term -> Logic.thm
val decide_conv : Scope.t -> Basic.term -> Logic.thm

val arith_conv : Scope.t -> Basic.term -> Logic.thm

