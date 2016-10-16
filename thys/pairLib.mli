(*----
 Name: pairLib.mli
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

(*
   pairLib.mli:

   Support code for pairScript.ml.

   Adds a pretty printer for pair terms.
   This prints [pair a b] as [a, b] rather than the standard printer
   which produces [a , b] (with an extra space before the comma).

   The standard parser handles commas correctly.

   compile with
        ocamlc -c -I INCLUDEDIR pairLib.mli
   where INCLUDEDIR is the directory containing the hseq header files.
*)

open HSeq

val pair_thy : string
(** [pair_thy]: The name of the theory of pairs *)

val pair_id : Ident.t
(** [pair_id] The identifier of the pair constructor *)

(**
   [pair_prec] The precedence of the pair identifier

   [pair_fixity] The fixity of the pair identifier
*)
val pair_prec: int
val pair_fixity: Printer.fixity

val pair_printer:
    Printer.ppinfo -> (Printer.fixity * int)
      -> (Basic.term * Basic.term list) Printer.printer
