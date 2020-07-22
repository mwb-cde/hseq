(*----
  Copyright (c) 2019-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(*
 * Skolem constants
 *)

type skolem_cnst = (Ident.t * (int * Gtype.t))

let make_sklm x ty i = (x, (i, ty))
let get_sklm_name (x, (_, _)) = x
let get_sklm_indx (_, (i, _)) = i
let get_sklm_type (_, (_, t)) = t

let get_old_sklm n sklms =
  (n, List.assoc n sklms)

let make_skolem_name id indx =
  let suffix =
    if (indx = 0)
    then ""
    else (string_of_int indx)
  in
  ("_"^(Ident.name_of id)^suffix)


(*
 * Constructing skolem constants
 *)

(** Data needed to generate a skolem constant. *)
type new_skolem_data=
  {
    name: Ident.t;
    ty: Gtype.t;
    tyenv: Gtype.Subst.t;
    scope: Scope.t;
    skolems: (skolem_cnst)list;
    tylist: (string * int)list
  }

(** [new_weak_type n names]

      Make a new weak type with a name derived from [n] and [names].
      Return this type and the updated names.  *)
let new_weak_type n names=
  (* get the index of the name (if any) *)
  let nm_int=
    try List.assoc n names
    with Not_found -> 0
  in
  let nnames = Lib.replace n (nm_int+1) names in
  let nm_s = (Lib.int_to_name nm_int)
  in
  (nm_s, nnames)

(** [mk_new_skolem scp n ty]

      Make a new skolem constant with name [n] and type [ty] scope
      [scp] is needed for unification return the new identifier, its
      type and the updated information for skolem building *)
let mk_new_skolem info =
  (* tyname: if ty is a variable then use its name for the weak
       variable otherwise use the empty string *)
  let tyname x =
    if Gtype.is_var info.ty
    then new_weak_type (Gtype.get_var_name info.ty) info.tylist
    else new_weak_type (x^"_ty") info.tylist
  in
  (* make the weak type *)
  let mk_nty x =
    let ty_name, nnames = tyname x in
    let tty=Gtype.mk_weak ty_name in
    (* unify the weak type with the given type *)
    let ntyenv = Ltype.unify_env info.scope tty info.ty info.tyenv
    in
    (Gtype.mgu tty ntyenv, ntyenv, nnames)
  in
  (* Make a name not already associated with a skolem or meta variable *)
  let skname0, skindx0 =
    (* First the skolems *)
    begin
      match (Lib.try_find (get_old_sklm info.name) info.skolems) with
      | Some(oldsk) ->
         (* Get next available skolem index for the name *)
         let nindx = (get_sklm_indx oldsk)+1 in
         (info.name, nindx)
      | _ -> (info.name, 0)
    end
  in
  (* Next, the meta variables *)
  let skname1, skindx1 =
    let rec find_new_meta scp n idx =
      let nname = make_skolem_name n idx in
      if Scope.is_meta scp
           (Term.dest_meta (Term.mk_meta nname (Gtype.mk_weak "_ty")))
      then find_new_meta scp n (idx + 1)
      else (n, idx)
    in
    find_new_meta info.scope skname0 skindx0
  in
  let nnam = make_skolem_name skname1 skindx1 in
  let nty, ntyenv, new_names = mk_nty nnam in
  (Term.mk_meta nnam nty, nty,
   (info.name, (skindx1, nty))::info.skolems, ntyenv, new_names)
