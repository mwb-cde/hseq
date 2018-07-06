(*----
  Name: scope.ml
  Copyright Matthew Wahab 2005-20016
  Author: Matthew Wahab <mwb.cde@gmail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under
  the terms of the Lesser GNU General Public License as published by
  the Free Software Foundation; either version 3, or (at your option)
  any later version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
  License for more details.

  You should have received a copy of the Lesser GNU General Public
  License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
  ----*)

(* Scope of terms and types *)

(*
 * Data structures
 *)

(** Theory markers. *)
type marker = (string)Tag.t
let mk_marker = Tag.named
let marker_name = Tag.name

(** Meta variables *)
type meta_db = (Term.Binder.t)Treekit.StringTree.t
let empty_meta_db ()= Treekit.StringTree.empty
let meta_db_add n b db =
  Treekit.StringTree.add db n b

let meta_db_find n db =
  Treekit.StringTree.find db n

(* Records for type definitions *)
type type_record = Gtype.typedef_record

let mk_type_record n xs al =
  {
    Gtype.TypeScope.name = n;
    Gtype.TypeScope.args = xs;
    Gtype.TypeScope.alias = al;
  }

let dest_type_record r =
  (r.Gtype.TypeScope.name,
   r.Gtype.TypeScope.args,
   r.Gtype.TypeScope.alias)

(** Scope records. *)
type t =
    {
      curr_thy : marker;
      term_type : Ident.t -> Gtype.t;
      term_thy : string -> Ident.thy_id;
      types: Gtype.TypeScope.t;
      thy_in_scope : Ident.thy_id -> bool;
      marker_in_scope : marker -> bool;
      meta_vars: meta_db
    }

(*
 * Operations on scopes
 *)

(** Construct an empty scope *)
let empty_scope () =
  let dummy x = raise Not_found
  in
  {
    curr_thy = mk_marker Ident.null_thy;
    term_type = dummy;
    term_thy = dummy;
    types = Gtype.TypeScope.empty();
    thy_in_scope = (fun x -> false);
    marker_in_scope = (fun x -> false);
    meta_vars = empty_meta_db()
  }

(** [marker_of scp]: Get the theory marker of scope [scp] *)
let marker_of scp = scp.curr_thy

(** [thy_of scp]: Get the theory of scope [scp] *)
let thy_of scp = marker_name (scp.curr_thy)

(** Lookup the type of an identifier *)
let type_of scp id = scp.term_type id

(** Lookup the theory of an identifier *)
let thy_of_term scp id = scp.term_thy id

(** Get the types scope *)
let types_scope scp = scp.types

(** Get the definition of a type. *)
let defn_of scp id = Gtype.TypeScope.defn_of (types_scope scp) id

(** Lookup the theory of a type. *)
let thy_of_type scp id = Gtype.TypeScope.thy_of (types_scope scp) id

(** Test whether a theory is in scope *)
let in_scope scp th1 = scp.thy_in_scope th1

(** Test whether a theory marker is in scope *)
let in_scope_marker scp m =
  scp.marker_in_scope m

(*
 * Extending scopes
 *)

(** Extend a scope with a list of identifiers [[(I1, T1); ...; (In,
    Tn)]]. Each identifier [Ii] is given type [Ti].
*)
let extend_with_terms scp declns =
  let ext_type x =
    try (List.assoc x declns)
    with Not_found -> type_of scp x
  and ext_thy x =
    try
      let (id, _) =
        List.find (fun (y, _) -> x = (Ident.name_of y)) declns
      in
      Ident.thy_of id
    with Not_found -> thy_of_term scp x
  in
  {scp with
    term_type = ext_type; term_thy = ext_thy}

(** Extend a scope with a list of type definitions [[(I1, D1); ...;
    (In, Dn)]]. Each identifier [Ii] has definition [Di].
*)
let extend_with_typedefs scp declns =
  let tyscp = Gtype.TypeScope.add_defns (types_scope scp) declns
  in
  {
    scp with types = tyscp
  }

(** Extend a scope with a list of type declarations [[(I1, A1); ...;
    (In, An)]]. Each identifier [Ii] has arguments [Ai], but no
    definition.
*)
let extend_with_typedeclns scp declns=
  let tyscp = Gtype.TypeScope.add_declns (types_scope scp) declns
  in
  {
    scp with types = tyscp
  }

(** Introduce a new local scope, derived from the current theory
    marker.
*)
let new_local_scope scp =
  let n = marker_name scp.curr_thy in
  let m = mk_marker n in
  let mfn x = (Tag.equal m x) || (scp.marker_in_scope x)
  in
  {
    scp with
      curr_thy = m;
      marker_in_scope = mfn
  }

(** [add_meta scp b]: Add [b] as a new meta variable. Fails if there
    is already a meta variable with the same name as [b].
*)
let add_meta scp v =
  let db = scp.meta_vars in
  let n = Term.Binder.name_of v in
  match Lib.try_find (meta_db_find n) db with
    | Some _ ->
      raise (Failure ("add_meta: "^n^" is already a meta variable"))
    | _ -> {scp with meta_vars = meta_db_add n v db}

let find_meta scp n =
  let db = scp.meta_vars
  in
  meta_db_find n db

let is_meta scp v =
  let n = Term.Binder.name_of v in
  try ignore(find_meta scp n); true
  with Not_found -> false

(** Relaxed scopes *)
let relaxed scp =
  let curr_thy = thy_of scp
  in
  let get_thy_of_type id =
    begin
    try thy_of_type scp id
    with Not_found -> curr_thy
    end
  in
  let tyscp0 = types_scope scp in
  let tyscp =
    { tyscp0 with Gtype.TypeScope.type_thy = get_thy_of_type }
  in
  {
    scp with
    types = tyscp
  }
