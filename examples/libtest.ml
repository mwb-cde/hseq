(*----
  Name: libtest.ml
  Copyright Matthew Wahab 2017
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
  Global.set_context (BaseTheory.builder (Global.context()))

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
