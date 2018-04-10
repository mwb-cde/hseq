(*----
  Name: printerkit.mli
  Copyright Matthew Wahab 2018
  Author: Matthew Wahab <mwb.cde@gmail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under
  the terms of the Lesser GNU General Public License as published by the
  Free Software Foundation; either version 3, or (at your option) any
  later version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public License for
  more details.

  You should have received a copy of the Lesser GNU General Public License
  along with HSeq.  If not see <http://www.gnu.org/licenses/>.
----*)

(**
   Pretty printer library.

   Support for pretty printing terms and types including printer
   information records to store symbolic representations and user
   defined printers.
*)

exception Error of string

(**
   {5 Fixities and associativities}
*)

type assoc = Parserkit.Info.associativity
(** Associativity of an identifier *)

val non_assoc: assoc
(** Non-associativity *)
val left_assoc: assoc
(** Left associativity *)
val right_assoc: assoc
(** Right associativity *)

type fixity = Parserkit.Info.fixity
(** Fixity of an identifier. *)

val nonfix: fixity
val prefix: fixity
val suffix: fixity

val infix: assoc -> fixity
(**
    Infix operators take an associativity argument. Use one of
    [infixl], [infixr] or [infixn].
*)

val infixl: fixity
(** Left associative infix *)
val infixr: fixity
(** Right associative infix *)
val infixn: fixity
(** Non-associative infix *)
val assoc_of: fixity -> assoc
(** Extract the associativity from a fixity operator. *)

(** {7 Tests on fixity and associativity} *)

val is_left_assoc: fixity -> bool
val is_right_assoc: fixity -> bool
val is_non_assoc: fixity -> bool

val is_infix: fixity -> bool
val is_prefix: fixity -> bool
val is_suffix: fixity -> bool

(** {7 String representation of fixity and associativity (for printing)} *)

val assoc_to_string: assoc -> string
val fixity_to_string: fixity -> string

(** {7 Default precedence, fixity and assoc} *)

val default_term_prec: int
(** Default precedence for terms *)
val default_term_assoc: assoc
(** Default associativity for terms *)
val default_term_fixity: fixity
(** Default fixity for terms *)

val default_type_prec: int
(** Default precedence for types *)
val default_type_assoc: assoc
(** Default associativity for types *)
val default_type_fixity: fixity
(** Default fixity for types *)

(** {5 Pretty printer information for function and type identifiers} *)

type 'a printer = 'a -> unit
(** A printer [print: 'a printer] prints the representation of an
    object [print obj].
*)

type record =
    {
      prec: int;
      fixity: fixity;
      repr: string option;
    }
(** The print record of an identifier. *)

val mk_record: int -> fixity -> string option -> record
(** Make a print record *)
val empty_record: unit -> record
(** Make an empty print record *)

(** {7 Printer information storage for identifiers} *)

type ('a, 'b)info =
    {
      records: (record) Ident.tree;
      printers: ('a -> (fixity * int) -> ('b)printer)Ident.tree
    }

(** The table of records and printers for a set of identifiers. There
    are seperate [info] tables for terms and types. User defined
    printers are indexed and triggered by identifiers appearing in a
    term or type.

    [records]: Print records, storing the precedence, fixity and
    optional representation of an identifier.

    [printers]: User defined printers. A printer takes the current fixity
    and precedence as an argument.
*)

val mk_info: int ->  ('a, 'b)info
(** Make an info store of size [sz]. *)

val default_info_size: int
(** The size of the tables created by [empty_info].  *)

val empty_info: unit-> ('a, 'b)info
(** Create a PP information store using the default size given by
    [default_info_size].
*)

(** Two sets of functions to manipulate printer [info]. [*_record]
    deal with printer records, [*_info] construct the record to be
    added/accessed.
*)

val get_record: ('a, 'b)info -> Ident.t -> record
(** Get the pretty printing record for identifer [id].

    @raise [Not_found] if no record.
*)

val add_record: ('a, 'b)info -> Ident.t -> record -> ('a, 'b)info
(** Add a pretty printing record for an identifer. *)

val remove_record: ('a, 'b)info -> Ident.t -> ('a, 'b)info
(** Remove record for identifer. *)

val get_info: ('a, 'b)info -> Ident.t -> (int * fixity * (string)option)
(** [get_info info id]: Get pretty printing information for identifer
    [id] from [info].

    Returns [(prec, fixity, repr)] where [prec] is precedence [fixity]
    is fixity [repr] is representation to use (if any)

    Returns [(default_term_prec, default_term_fixity, None)] if [id] is
    not found.
*)

val add_info:
  ('a, 'b)info -> Ident.t -> int -> fixity
  -> (string)option -> ('a, 'b)info
(**
   [add_info info id prec fixity repr]: Add pretty printing
   information for an identifer.

   [prec] is the precedence, [fixity] the fixity, [repr] the
   (optional) string representation to use rather than [id].
*)

val remove_info: ('a, 'b)info -> Ident.t -> ('a, 'b)info
(** Remove pretty printing information for identifer. *)

val get_printer:
  ('a, 'b)info -> Ident.t -> ('a -> (fixity * int) -> ('b)printer)
(** Get the user defined printer for an identifier *)

val add_printer:
  ('a, 'b)info -> Ident.t -> ('a -> (fixity * int) -> ('b)printer)
  -> ('a, 'b)info

(** Add a user defined printer for an identifier *)

val remove_printer: ('a, 'b)info -> Ident.t -> ('a, 'b)info
(** Remove a user defined printer for an identifier *)

(* Printer constructors *)

val string_identifier: Ident.t -> record -> string
(** Convert an identifier to a string, using its PP representation if
    any.
*)

val print_string: string printer
(** Print string [str]. *)

val print_space: 'a printer
(** [print_space info] Print a space (using Format.print_space)
*)

val print_bracket: int -> int -> string printer
(** [print_bracket cpr prec br]: Print a precedence bracket.

    Print bracket [br] using [print_string info prec br]
    if current precedence [cpr] is less than new precedence [prec].
*)

val print_assoc_bracket:
  (fixity * int) -> (fixity * int) -> string printer
(** [print_assoc_bracket (cfix, cprec) (nfix, nprec) br]: Print a
    precedence bracket if appropriate with current and given fixities.

    Print bracket [br] using [print_string info prec br] if new
    precedence [nprec] is less than current precedence [cprec] or
    [cprec=nprec] and [nfix] is not non-associative and [nfix] differs
    from [cfix].
*)

val print_list: ('a printer * unit printer) -> 'a list printer
(** [print_list (pr, sep) l]: Print elements of list [l] using printer
    [pr].  Printer [sep ()] prints the separator.
*)

val print_sep_list: ('a printer * string) -> 'a list printer
(** [print_sep_list (pr, sep) l] Print elements of list [l] using
    printer [pr].  Printer [print_string sep] prints the separator.
*)

val print_ident: Ident.t printer
(** [print_ident i]: Simple printer for identifier [i]. Printer theory
    part and name part as strings seperated by "."
*)

val print_identifier: (Ident.t -> record) -> Ident.t printer
(** [print_identifier info id]: Print identifier [id] using
    representation given in [info]. If no representation in [info] then
    just print the identifier.
*)

val print_infix:
  ((int -> ('a) printer) * (int -> ('b list) printer))
  -> int
  -> ('a  * ('b) list) printer
(** [print_infix (opr, tpr, spr) prec (op, trm)]: Infix Printer.
    Print first element [trm] using printer [tpr], print [op] using
    [opr] print rest of [trm] using [print_list (tpr prec, spr)]. If
    [trm] is empty, just print [op].
*)

val print_suffix:
  ((int -> ('a)printer) * (int -> ('b list) printer))
  -> int
  ->  ('a * ('b) list) printer
(** [print_suffix (tpr, spr) prec (op, trm)]: Suffix printer.  Print
    elements [trm] using [print_list (tpr prec, spr)], print [op] using
    [opr].  If [trm] is empty, just print [op].
*)

val print_prefix:
  ((int -> ('a) printer) * (int -> (('b) list) printer))
  -> int
  -> ('a * ('b) list) printer
(** [print_prefix (tpr, spr) prec (op, trm)]: Prefix printer.  Print
    [op] using [opr] print elements [trm] using [print_list (tpr prec,
    spr)].  If [trm] is empty, just print [op].
*)

val print_operator:
  ((int -> ('a)printer)
   * (int -> ('b list) printer)
   * ('a -> record))
  -> int
  -> ('a * ('b) list) printer
(** [print_operator (opr, tpr, spr, info, printers) prec (op, trm)]:
    Printer for infix, suffix, prefix operators.

    Use [printer op] to determine whether there is a user defined
    printer for op.  Otherwise use [info op] to determine fixity and
    choose [print_infix], [print_suffix], [print_prefix] as
    appropriate.  Use chosen printer to printer [op] then print
    elements of using [tpr], with seperator printed by [spr] If [trm]
    is empty, just use [opr] to print [op]
*)
