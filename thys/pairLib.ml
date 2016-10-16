(*----
  Name: pairLib.ml
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
   pairLib.ml:

   Support code for pairScript.ml.

   Adds a pretty printer for pair terms.
   This prints [pair a b] as [a, b] rather than the standard printer
   which produces [a , b] (with an extra space before the comma).

   The standard parser handles commas correctly.

   compile with
        ocamlc -c -I INCLUDEDIR pairLib.ml
   where INCLUDEDIR is the directory containing the hseq header files.
*)

open HSeq
open HSeqUser
open Userlib

module PairLibImpl =
struct
  let pair_thy = "Pair"
  let pair_id = Ident.mk_long pair_thy "pair"
  let pair_prec = 10
  let pair_fixity = Printer.infixr
  let ppdata = (pair_fixity, pair_prec)

  let pair_printer ppstate prec (f, args) =
    match args with
    | (left::right::rest) ->
       begin
         Format.printf "@[<2>";
         Printer.print_assoc_bracket prec ppdata "(";
         Term.print_term ppstate ppdata left;
         Format.printf ",@ ";
         Term.print_term ppstate ppdata right;
         Printer.print_assoc_bracket prec ppdata  ")";
         Format.printf "@]";
         begin
           match rest with
           | [] -> ()
           | _ ->
              begin
	        Format.printf "@[";
	        Printer.print_list
	          (Term.print_term ppstate prec,
	           (fun () -> Format.printf "@ "))
	          rest;
	        Format.printf "@]"
              end
         end
       end
    | _ ->
       Term.simple_print_fn_app ppstate ppdata (f, args)

  let init_pair_printer() =
    let inf0 = Global.ppinfo () in
    let inf1 = Printer.add_term_printer inf0 pair_id pair_printer in
    Global.set_ppinfo inf1

end

let init() =
  PairLibImpl.init_pair_printer()

let _ = init()
