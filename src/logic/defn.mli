(* Type and term definition and declaration *)

(*    exception Error*)
(* the type of definition *)

(*
    type defn 
    type saved_defn 

    val to_save : defn -> saved_defn
    val from_save : saved_defn -> defn
*)
type decln 

(* destructors *)
(*
    val destdefn : defn -> Formula.form
*) 
   val dest_decln : decln -> Basic.fnident * Gtypes.gtype

(* destruct a term of the form (f a1 a2 ..)=G to (f, [a1; a2; ..]) *)
    val get_lhs : Term.term -> 
      Basic.fnident * (Basic.fnident * Gtypes.gtype) list   

(* function declarations of the type (f: ty) *)
    val mkdecln :
      Gtypes.scope ->
      Basic.fnident -> Gtypes.gtype -> decln

(* make the type of a defined term *)
    val mk_defn_type :
      Gtypes.substitution ->
      ('a * Gtypes.gtype) list ->
      Gtypes.gtype -> ('a * Gtypes.gtype) list -> Gtypes.gtype

(* make a definition *)
(* [mkdefn scp id args t]
   scp is the scope of the definition
   id is the identifier
   args are the list of parameters identifiers and types 
   t is the body of the definition 
*)
(*
    val mkdefn :
      Gtypes.scope ->
	string ->
      (string * Gtypes.gtype) list ->
      Term.term -> string * Gtypes.gtype * defn
*)
    val mkdefn :
      Gtypes.scope ->
	string ->
      (string * Gtypes.gtype) list ->
      Term.term -> string * Gtypes.gtype * Logic.thm

