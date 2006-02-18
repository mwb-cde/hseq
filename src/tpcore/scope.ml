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

(** Theory markers. *)
type marker = Tag.t

let mk_marker = Tag.named
let marker_name  = Tag.name 

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
      curr_thy : marker;
      term_type : Ident.t -> gtype; 
	term_thy : string -> Ident.thy_id;
	  type_defn: Ident.t -> type_record;
	    type_thy : string -> Ident.thy_id;
		thy_in_scope : Ident.thy_id -> bool;
		    marker_in_scope : marker -> bool
    }


(***
* Operations on scopes
***)

(** Construct an empty scope *)
let empty_scope () = 
  let dummy x = raise Not_found
  in 
  {
   curr_thy = mk_marker Ident.null_thy;
   term_type = dummy;
   term_thy = dummy;
   type_defn = dummy;
   type_thy = dummy;
   thy_in_scope = (fun x -> false);
   marker_in_scope = (fun x -> false)
 }

(** [marker_of scp]: Get the theory marker of scope [scp] *)
let marker_of scp = scp.curr_thy

(** [thy_of scp]: Get the theory of scope [scp] *)
let thy_of scp = marker_name (scp.curr_thy)

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

(** Test whether a theory marker is in scope *)
let in_scope_marker scp th1 = scp.marker_in_scope th1 

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
      let (id, _) = List.find (fun (y, _) -> x = (Ident.name_of y)) declns
      in Ident.thy_of id
    with Not_found -> thy_of_term scp x
  in 
  {scp with 
   term_type = ext_type; term_thy = ext_thy}

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
      let (id, _) = List.find (fun (y, _) -> x = (Ident.name_of y)) declns
      in Ident.thy_of id
    with Not_found -> thy_of_type scp x
  and ext_scope n = 
    List.exists 
      (fun (id, _) -> (String.compare n (Ident.name_of id)) = 0) declns
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
     {name = Ident.name_of id; args = args; 
      alias = None; characteristics = []})
  in 
  extend_with_typedefs scp (List.map mk_def declns)

