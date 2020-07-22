(*----
  Copyright (c) 2018-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(* Pretty Printer *)

open Format

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

(*
 * Pretty printer information for function and type identifiers
 *)

(** The type of printers *)
type ('a)printer = 'a -> unit

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
type ('a, 'b)info =
    {
      records: (record)Ident.map;
      printers: ('a -> (fixity * int) -> ('b)printer) Ident.map
    }

let mk_info sz =
  {records = Ident.Map.empty; printers = Ident.Map.empty}

let default_info_size = 53
let empty_info() = mk_info default_info_size

(** Add/access printer records *)

let get_record info id =
  Ident.Map.find id info.records
let add_record info id rcrd =
  { info with records = Ident.Map.add id rcrd info.records }
let remove_record info id  =
  { info with records = Ident.Map.remove id info.records }

(** Construct printer records to be added/accessed from [info] *)

let get_info info id =
  try
    let r = Ident.Map.find id (info.records) in
    (r.prec, r.fixity, r.repr)
  with Not_found -> (default_term_prec, default_term_fixity, None)

let add_info (inf:('a, 'b)info) id pr fx rp =
  let r = {prec = pr; fixity = fx; repr = rp}
  in
  add_record inf id r

let remove_info info id =
  { info with records = Ident.Map.remove id info.records }

(** User defined printers *)

let get_printer info id =
  Ident.Map.find id info.printers

let add_printer info id prntr =
  { info with printers = Ident.Map.add id prntr info.printers }

let remove_printer info id =
  { info with printers = Ident.Map.remove id info.printers }

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
