(*-----
   Name: scope.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(* Scope of terms and types *)

open Basic

(***
* Data structures
***)

(* Records for type definitions *)
type type_record =
    {
     name: string; 
     args : string list; 
     alias: gtype option;
     characteristics: string list
   }

(** Scope records. *)
type t=
    { 
      curr_thy : thy_id;
      term_type : ident -> gtype; 
	term_thy : string -> thy_id;
	  type_defn: ident -> type_record;
	    type_thy : string -> thy_id;
		thy_in_scope : thy_id -> bool

    }

(***
* Operations on scopes
***)

(** Construct an empty scope *)
let empty_scope () = 
  let dummy x = raise Not_found
  in 
  {
   curr_thy = null_thy;
   term_type = dummy;
   term_thy = dummy;
   type_defn = dummy;
   type_thy = dummy;
   thy_in_scope = (fun x -> false)
 }

(** [thy_of scp]: Get the theory of scope [scp] *)
let thy_of scp = scp.curr_thy

(** Lookup the type of an identifier *)
let type_of scp id = scp.term_type id

(** Lookup the theory of an identifier *)
let thy_of_term scp id = scp.term_thy id

(** Get the definition of a type. *)
let defn_of scp id = scp.type_defn id

(** Lookup the theory of a type. *)
let thy_of_type scp id = scp.type_thy id

(** Test whether a theory is in scope *)
let in_scope scp  th1 = scp.thy_in_scope th1 

(***
* Extending scopes
***)

(** 
   Extend a scope with a list of identifiers [[(I1, T1); ...; (In,
   Tn)]]. Each identifier [Ii] is given type [Ti].
*)
let extend_with_terms scp declns =
  let ext_type x = 
    try (List.assoc x declns)
    with Not_found -> type_of scp x
  and ext_thy x = 
    try 
      let (id, _) = List.find (fun (y, _) -> x = (name y)) declns
      in thy_of_id id
    with Not_found -> thy_of_term scp x
  in 
  {scp with term_type = ext_type; term_thy = ext_thy }

(** 
   Extend a scope with a list of type definitions [[(I1, D1); ...;
   (In, Dn)]]. Each identifier [Ii] has definition [Di].
*)
let extend_with_typedefs scp declns =
  let ext_defn x= 
    try (List.assoc x declns)
    with Not_found -> defn_of scp x
  and ext_thy x = 
    try 
      let (id, _) = List.find (fun (y, _) -> x = (name y)) declns
      in thy_of_id id
    with Not_found -> thy_of_type scp x
  in 
  {scp with type_defn = ext_defn; type_thy = ext_thy }

(** 
   Extend a scope with a list of type declarations [[(I1, A1); ...;
   (In, An)]]. Each identifier [Ii] has arguments [Ai], but
   no definition.
*)
let extend_with_typedeclns scp declns=
  let mk_def (id, args)= 
    (id, 
     {name = name id; args = args; 
      alias = None; characteristics = []})
  in 
  extend_with_typedefs scp (List.map mk_def declns)

