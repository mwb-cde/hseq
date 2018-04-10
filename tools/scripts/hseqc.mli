(*----
  Name: hseqc.mli
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
