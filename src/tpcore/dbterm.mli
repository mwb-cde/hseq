(*-----
   Name: dbterm.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(** Term representation for permanent storage. *)

(** 
   A term is stored as a de Bruijn term. Functions [of_term] and
   [to_term] convert between the de Bruijn and the standard
   representation. 
*)

open Gtypes

(** Binders for de Bruijn terms *)
type binder =
    { quant: Basic.quant;
      qvar: string;
      qtyp: Gtypes.stype}

(**
   Representation of de Bruijn terms. This mirrors [Basic.term], the
   difference being that bound variables are represented by indices
   and binders are not stored as references.
*)
type dbterm =
    Id of Ident.t * Gtypes.stype
  | Free of string * Gtypes.stype
  | Qnt of binder * dbterm
  | Bound of int
  | App of dbterm * dbterm
  | Const of Basic.const_ty


(** {5 Conversion to and from terms} *)

val of_term : Basic.term -> dbterm
(** Convert a term to its de Bruijn representation. *)

val to_term : dbterm -> Basic.term 
(** 
   Convert a de Bruijn term to its reference carrying representation. 
*)
