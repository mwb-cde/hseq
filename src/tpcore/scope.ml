(*-----
   Name: scope.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(* Scope of terms and types *)

open Basic

(* records for type definitions *)
type type_record =
    {
     name: string; 
     args : string list; 
     alias: gtype option;
     characteristics: string list
   }

type t=
    { 
      curr_thy : thy_id;
      term_type : ident -> gtype; 
	term_thy : string -> thy_id;
	  type_defn: ident -> type_record;
	    type_thy : string -> thy_id;
(*
		thy_in_scope : thy_id -> thy_id -> bool
*)
		thy_in_scope : thy_id -> bool

    }

let empty_scope () = 
  let dummy x = raise Not_found
  in 
  {
   curr_thy = null_thy;
   term_type = dummy;
   term_thy = dummy;
   type_defn = dummy;
   type_thy = dummy;
(*
   thy_in_scope = (fun x y -> false)
*)
   thy_in_scope = (fun x -> false)
 }

let thy_of scp = scp.curr_thy
let type_of scp id = scp.term_type id
let thy_of_term scp id = scp.term_thy id
let defn_of scp id = scp.type_defn id
let thy_of_type scp id = scp.type_thy id
(*
let in_scope_of scp th1 th2 = scp.thy_in_scope th1 th2
*)
let in_scope scp  th1 = scp.thy_in_scope th1 

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

let extend_with_typedeclns scp declns=
  let mk_def (id, args)= 
    (id, 
     {name = name id; args = args; 
      alias = None; characteristics = []})
  in 
  extend_with_typedefs scp (List.map mk_def declns)

