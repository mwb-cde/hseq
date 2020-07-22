(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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

val init: unit -> unit
