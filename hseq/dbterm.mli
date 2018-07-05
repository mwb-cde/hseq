(*----
  Name: dbterm.mli
  Copyright Matthew Wahab 2005-2016
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

(** Term representation for permanent storage. *)

(**
    A term is stored as a de Bruijn term. Functions [of_term] and
    [to_term] convert between the de Bruijn and the standard
    representation.
*)

open Gtype

(** Binders for de Bruijn terms *)
type binder =
    { quant: Basic.quant;
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
  | Const of Basic.const_ty

(** {5 Conversion to and from terms} *)

val of_term : Term.term -> dbterm
(** Convert a term to its de Bruijn representation. *)

val to_term: dbterm -> Term.term
(** Convert a de Bruijn term to its reference carrying representation.
*)
