(*----
  Copyright (c) 2017-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(**
  Example of linking with the HSeq library.

  Compile with
   ocamlc -o test
      -I HSEQLIB unix.cma hseq.cma
      libtest.ml

  where HSEQLIB is the installation directory for the the HSeq libraries.

  or
   hseqc -o test libtest.ml
  or
   hseqc --native -o test libtest.ml
*)

(**
   Open HSeq modules.
*)

open HSeq
open Tactics
open Boollib
open Simplib
open HSeqUser
open Userlib

(**
   Simple test
*)

(** Initialise HSeq (quietly) *)
let initialize() =
  Global.init();
  Global.set_context (BaseTheory.builder false (Global.context()))

let run () =
  prove (hterm "! x y z: ((x | y) & z) => ((x & z) | (y & z)) ")
  blast_tac

let main() =
  initialize();
  (** Run the proof *)
  let th = catch_errors run ()
  in
  begin
    Format.open_box 0;
    Display.print_thm th;
    Format.close_box();
    Format.print_newline()
  end

let _ = main()
