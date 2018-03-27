(*----
  Name: printer.ml
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

(* Pretty Printer *)

open Format
open Basic

exception Error of string

(*
 * Fixities and associativities
 *)

(** Associativity of an identifier *)
type assoc = Parserkit.Info.associativity

let non_assoc = Parserkit.Info.non_assoc
let left_assoc = Parserkit.Info.left_assoc
let right_assoc = Parserkit.Info.right_assoc

(** Fixity of an identifier. *)
type fixity = Parserkit.Info.fixity

let nonfix = Parserkit.Info.nonfix
let prefix = Parserkit.Info.prefix
let suffix = Parserkit.Info.suffix

let infix = Parserkit.Info.infix
let infixl = Parserkit.Info.infix left_assoc
let infixr = Parserkit.Info.infix right_assoc
let infixn = Parserkit.Info.infix non_assoc

let assoc_of = Parserkit.Info.assoc_of

(** Tests on fixity and associativity *)

let is_non_assoc a = Parserkit.Info.is_non_assoc a
let is_right_assoc a = Parserkit.Info.is_right_assoc a
let is_left_assoc a = Parserkit.Info.is_left_assoc a

let is_infix fx = Parserkit.Info.is_infix fx
let is_prefix fx = Parserkit.Info.is_prefix fx
let is_suffix fx = Parserkit.Info.is_suffix fx

(** String representation of fixity and associativity (for printing) *)

let assoc_to_string a =
  match a with
      Parserkit.Info.Leftassoc -> "left associative"
    | Parserkit.Info.Rightassoc -> "right associative"
    | Parserkit.Info.Nonassoc -> "non-associative"

let fixity_to_string fx =
  match fx with
      Parserkit.Info.Nonfix -> "nonfix"
    | Parserkit.Info.Prefix -> "prefix"
    | Parserkit.Info.Suffix -> "suffix"
    | Parserkit.Info.Infix(a) ->
      ("infix ("^(assoc_to_string a)^")")

(** Default precedence, fixity and assoc *)

let default_term_prec = 10
let default_term_assoc = Parserkit.Info.non_assoc
let default_term_fixity = Parserkit.Info.nonfix

let default_type_prec = 10
let default_type_assoc = Parserkit.Info.non_assoc
let default_type_fixity = Parserkit.Info.nonfix

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

(*
 * Pretty printer information for function and type identifiers
 *)

(** The type of printers *)
type 'a printer = 'a -> unit

(** The print record of an identifier. *)
type record =
    {
      prec: int;            (* its precedence *)
      fixity: fixity;       (* its fixity *)
      repr: string option   (* its representation *)
    }

let mk_record pr inf rep =
  {prec = pr; fixity = inf; repr = rep}

let empty_record() = mk_record (-1) nonfix None

(*
 * Printer information storage for identifiers
 *)

(** The table of records and printers for a set of identifiers. *)
type ('a, 'b) info =
    {
      records: (record) Ident.tree;
      printers: ('a -> (fixity * int) -> ('b printer)) Ident.tree
    }

let mk_info sz =
  {records = Ident.Tree.empty; printers = Ident.Tree.empty}

let default_info_size = 53
let empty_info() = mk_info default_info_size

(** Add/access printer records *)

let get_record info id =
  Ident.Tree.find info.records id
let add_record info id rcrd =
  { info with records = Ident.Tree.replace info.records id rcrd }
let remove_record info id  =
  { info with records = Ident.Tree.remove info.records id }

(** Construct printer records to be added/accessed from [info] *)

let get_info info id =
  try
    let r = Ident.Tree.find (info.records) id in
    (r.prec, r.fixity, r.repr)
  with Not_found -> (default_term_prec, default_term_fixity, None)

let add_info (inf:('a, 'b)info) id pr fx rp =
  let r = {prec = pr; fixity = fx; repr = rp}
  in
  add_record inf id r

let remove_info info id =
  { info with records = Ident.Tree.remove info.records id }

(** User defined printers *)

let get_printer info id =
  Ident.Tree.find info.printers id

let add_printer info id prntr =
  { info with printers = Ident.Tree.add info.printers id prntr }

let remove_printer info id =
  { info with printers = Ident.Tree.remove info.printers id }


(*
 * Combined printer information tables}
 *)

(** The combined printer information for terms and types. *)
type ppinfo =
    {
      terms:  (ppinfo, Basic.term * (Basic.term)list) info;
      types:  (ppinfo, Ident.t * (Gtypes.t)list) info
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

let get_term_printer info x =
  get_printer (info.terms) x
let add_term_printer info id prnt =
  set_term_info info (add_printer (info.terms) id prnt)
let remove_term_printer info id =
  set_term_info info (remove_printer (info.terms) id)

(** Operations involving type identifiers *)
type gtype_printer =
  ppinfo -> (fixity * int) -> (Ident.t * (Gtypes.t list)) printer
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

(*
 * Pretty-printing utility functions
 *)

let string_identifier id pp_rec =
  match pp_rec.repr
  with
    | None -> (Ident.string_of id)
    | Some(x) -> x

(** Print a string *)
let print_string str = Format.printf "%s" str

(** Print a space *)
let print_space _ =  Format.printf "@ "

(** Print a precedence marking bracket. *)
let print_bracket nprec prec br =
  if prec < nprec then Format.printf "%s" br else ()

(** Print a precedence and fixity aware bracket *)
let print_assoc_bracket cr nr br =
  let (cassoc, cprec) = cr
  and (nassoc, nprec) = nr
  in
  if nprec < cprec
  then Format.printf "%s" br
  else
    if nprec = cprec
    then
      (if (is_non_assoc cassoc || is_non_assoc nassoc)
       then ()
       else
          (if not (cassoc = nassoc)
           then Format.printf "%s" br
           else ()))
    else ()

(** List printer *)
let print_list (term_printer, sep) x =
  let rec print_aux ts =
    match ts with
      | [] -> ()
      | p::[] -> term_printer p
      | p::ps ->
         begin
           term_printer p;
           sep ();
           print_aux ps
         end
  in
  print_aux x

(** Print a list with a given string as seperator *)
let print_sep_list (term_printer, sep) x =
  print_list (term_printer, (fun _ -> Format.printf "%s@ " sep)) x

(** Printer a simple identifier *)
let print_ident x =
  Format.printf "%s" (Ident.string_of x)

(** Print an identifier, using stored representation if available *)
let print_identifier info x =
  let pprec = info x in
  let str =
    (match pprec.repr with
      | None ->
        if (!Settings.long_identifier)
        then Ident.string_of x
        else Ident.name_of x
      | Some(s) -> s)
  in
  Format.printf "%s" str

(** Print as infix *)
let print_infix (opr, tpr) prec (op, trms) =
  match trms with
    | l::rargs ->
      Format.printf "@[<2>";
      tpr prec [l];
      Format.printf "@ ";
      opr prec op;
      Format.printf "@ ";
      tpr prec rargs;
      Format.printf "@]"
    | [] ->
      Format.printf "@[<2>";
      opr prec op;
      Format.printf "@]"

(** Print as suffix *)
let print_suffix (opr, tpr) prec (op, trms) =
  Format.printf "@[<2>";
  tpr prec trms;
  Format.printf "@ ";
  opr prec op;
  Format.printf "@]"

(** Print as prefix *)
let print_prefix (opr, tpr) prec (op, trms) =
  Format.printf "@[<2>";
  opr prec op;
  Format.printf "@ ";
  tpr prec trms;
  Format.printf "@]"

(**
    Print an operator, looking up information to determine whether it
    is infix, suffix or prefix.
*)
let print_operator (opr, tpr, ppinfo) prec (op, trms) =
  let pprec = ppinfo op in
  let printer =
    if (is_infix pprec.fixity)
    then print_infix (opr, tpr)
    else
      if (is_suffix pprec.fixity)
      then print_suffix (opr, tpr)
      else print_prefix (opr, tpr)
  in
  Format.printf "@[<2>";
  print_bracket prec (pprec.prec) "(";
  printer (pprec.prec) (op, trms);
  print_bracket prec (pprec.prec) ")";
  Format.printf "@]"
