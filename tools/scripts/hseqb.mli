(*----
  Copyright (c) 2018-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(**
   A script to run hseq as a batch compiler.

   Use as [hseqb <args> <script> <script-args>]
   where
   [args] are the arguments to hseqb,
   [script] is the script to run,
   [script-args] are the arguments to pass to the script.
*)

val main: unit -> int
