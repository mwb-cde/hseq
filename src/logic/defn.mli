(* Type and term definition and declaration *)

(* 
   decln: type of declarations. 

   This is stored in theories and its 
   definition must thereforebe kept hidden.
*)
type decln 

(* defn: the ocaml type of a definition 
   This is used only for prettyprinting and can be.
   (The Logic.thm describing the definition is an abstracted type)
*)
type defn = Defn of (Basic.ident * Basic.gtype * Logic.thm)

(* destructors *)
val dest_decln : decln -> Basic.ident * Basic.gtype
val dest_defn : defn -> (Basic.ident * Basic.gtype * Logic.thm)

(* destruct a term of the form (f a1 a2 ..)=G to (f, [a1; a2; ..]) *)
val get_lhs : Basic.term -> 
  Basic.ident * (Basic.ident * Basic.gtype) list   

(* function declarations of the type (f: ty) *)
val mkdecln :
    Gtypes.scope ->
      Basic.ident -> Basic.gtype -> decln

(* make the type of a defined term *)
val mk_defn_type :
    Gtypes.substitution ->
      ('a * Basic.gtype) list ->
	Basic.gtype -> ('a * Basic.gtype) list -> Basic.gtype

(* make a definition *)
(* [mkdefn scp id args t]
   scp is the scope of the definition
   id is the identifier
   args are the list of parameters identifiers and types 
   t is the body of the definition 
 *)

val mkdefn :
    Gtypes.scope ->
      Basic.ident->
	(string * Basic.gtype) list -> Basic.term 
	  -> defn

