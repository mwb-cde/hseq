(* Representation of terms suitable for saving on disk *)

    open Gtypes

    type binder =
      { quant: Basic.quant_ty;
        qvar: string;
        qtyp: Gtypes.stype}

    type dbterm =
      	Var of Basic.ident * stype
      | Qnt of binder * dbterm
      | Bound of int
      | App of dbterm * dbterm
      | Const of Basic.const_ty

(* conversion to and from terms *)
    val of_term : Term.term -> dbterm
    val to_term : dbterm -> Term.term
