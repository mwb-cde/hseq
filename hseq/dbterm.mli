(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** Term representation for permanent storage. *)

(**
    A term is stored as a de Bruijn term. Functions [of_term] and
    [to_term] convert between the de Bruijn and the standard
    representation.
*)

open Gtype

(** Binders for de Bruijn terms *)
type binder =
    { quant: Term.quant;
      qvar: string;
      qtyp: Gtype.stype }

(**
   Representation of de Bruijn terms. This mirrors [Term.term], the
   difference being that bound variables are represented by indices
   and binders are not stored as references.
*)
type dbterm =
  | Id of Ident.t * Gtype.stype
  | Free of string * Gtype.stype
  | Qnt of binder * dbterm
  | Bound of int
  | App of dbterm * dbterm
  | Const of Term.Const.t

(** {5 Conversion to and from terms} *)

val of_term : Term.term -> dbterm
(** Convert a term to its de Bruijn representation. *)

val to_term: dbterm -> Term.term
(** Convert a de Bruijn term to its reference carrying representation.
*)
