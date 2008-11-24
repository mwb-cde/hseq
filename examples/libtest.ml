
(*----
   Name: libtest.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2006
   ----*)

(**
  Example of linking with the HSeq library.

  Compile with 
   ocamlc -o test 
      -pp "camlp4o q_MLast.cmo HSEQLIB/tpquote.cma pa_extend.cmo" -I +camlp4
      -I HSEQLIB nums.cma unix.cma hseq.cma
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
open Userlib


(**
   Simple test
*)

(** Initialise HSeq (quietly) *)
let initialise() = 
  (Global.init();
   BaseTheory.builder())

let run () =
  prove << ! x y z: ((x | y) & z) => ((x & z) | (y & z)) >>
  blast_tac

let _ = 
  initialise();
(** Run the proof *)
  let th = catch_errors run ()
  in 
    (Display.print_thm th;
    Format.printf "\n")
