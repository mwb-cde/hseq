(*----
  Copyright (c) 2018-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(**
   hseqc: Support for compiling against the HSeq library.

   usage: hseqc {hseqc options} -- {ocamlc options}

   Example:

   Compile test.ml using the native-code compiler
     hseqc -o test test.ml

   Compile test.ml using the native-code compiler
     hseqc --native -- -o test test.ml
*)

val main: unit -> int
