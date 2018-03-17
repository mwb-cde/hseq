(*----
  Name: printers.ml
  Copyright Matthew Wahab 2018
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

(*
 * Combined printer information tables}
 *)

open Printerkit
open Basic

(** The combined printer information for terms and types. *)
type ppinfo =
    {
      terms: (ppinfo, (Basic.term * (Basic.term)list))info;
      types: (ppinfo, (Ident.t * (Basic.gtype)list))info
    }

let mk_ppinfo sz =
  {
    terms = mk_info sz;
    types = mk_info sz;
  }
let empty_ppinfo() = mk_ppinfo default_info_size

(** Operations involving term identifiers *)

type term_printer =
  ppinfo -> (fixity * int) -> (Basic.term * Basic.term list) printer

let get_term_info info x = get_info (info.terms) x
let set_term_info info x = {info with terms = x}
let add_term_info info id prec fixity repr =
  set_term_info info (add_info (info.terms) id prec fixity repr)
let add_term_record info id record =
  set_term_info info (add_record (info.terms) id record)
let remove_term_info info id =
  set_term_info info (remove_info (info.terms) id)

let get_term_printer info x = get_printer (info.terms) x
let add_term_printer info id prnt =
  set_term_info info (add_printer (info.terms) id prnt)
let remove_term_printer info id =
  set_term_info info (remove_printer (info.terms) id)

(** Operations involving type identifiers *)
type gtype_printer =
  ppinfo -> (fixity * int) -> (Ident.t * (Basic.gtype list)) printer
let get_type_info info x = get_info (info.types) x
let set_type_info info x = {info with types = x}
let add_type_info info id prec fixity repr =
  set_type_info info (add_info (info.types) id prec fixity repr)
let add_type_record info id record =
  set_type_info info (add_record (info.types) id record)
let remove_type_info info id =
  set_type_info info (remove_info (info.types) id)

let get_type_printer info x = get_printer (info.types) x
let add_type_printer info id prnt =
  set_type_info info (add_printer (info.types) id prnt)
let remove_type_printer info id =
  set_type_info info (remove_printer (info.types) id)

(** Precedence of function application *)
let fun_app_prec = 90

(** Precedence of Quantifiers *)
let prec_qnt q =
  match q with
      Lambda -> 60
    | All -> 55
    | Ex -> 55
    | _ -> 55

(** Associativity of Quantifiers *)
let assoc_qnt q =
  match q with
      Lambda -> non_assoc
    | All -> non_assoc
    | Ex -> non_assoc
    | _ -> non_assoc

(** Fixity of Quantifiers *)
let fixity_qnt q = nonfix

