(*-----
 Name: dbterm.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* Representation of terms suitable for saving on disk *)

    open Gtypes

    type binder =
      { quant: Basic.quant_ty;
        qvar: string;
        qtyp: Gtypes.stype}

    type dbterm =
      	Id of Basic.ident * Gtypes.stype
      | Free of string * Gtypes.stype
      | Qnt of binder * dbterm
      | Bound of int
      | App of dbterm * dbterm
      | Const of Basic.const_ty

val binder_kind: binder -> Basic.quant_ty
val binder_name: binder -> string
val binder_type: binder -> Gtypes.stype


(* conversion to and from terms *)
    val of_term : Basic.term -> dbterm
    val to_term : dbterm -> Basic.term
