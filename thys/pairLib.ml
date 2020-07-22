(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
  let pair_fixity = Printkit.infixr
  let ppdata = (pair_fixity, pair_prec)

  let pair_printer ppstate prec (f, args) =
    match args with
    | (left::right::rest) ->
       begin
         Format.printf "@[<2>";
         Printkit.print_assoc_bracket prec ppdata "(";
         Printers.Terms.print_term ppstate ppdata left;
         Format.printf ",@ ";
         Printers.Terms.print_term ppstate ppdata right;
         Printkit.print_assoc_bracket prec ppdata  ")";
         Format.printf "@]";
         begin
           match rest with
           | [] -> ()
           | _ ->
              begin
                Format.printf "@[";
                Printkit.print_list
                  (Printers.Terms.print_term ppstate prec,
                   (fun () -> Format.printf "@ "))
                  rest;
                Format.printf "@]"
              end
         end
       end
    | _ ->
       Printers.Terms.simple_print_fn_app ppstate ppdata (f, args)

  let init_pair_printer() =
    let inf0 = Global.ppinfo () in
    let inf1 = Printers.add_term_printer inf0 pair_id pair_printer in
    Global.set_ppinfo inf1

end

let init() =
  PairLibImpl.init_pair_printer()

let _ = init()
