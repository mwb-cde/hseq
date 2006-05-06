
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
*)

(**
   open Modules
*)

open HSeq
open Tactics
open Boollib
open Simplib
open Userlib


(**
   Simple test
*)

let run () =
  prove << ! x y z: ((x | y) & z) => ((x & z) | (y & z)) >>
  blast_tac

(** Initialise HSeq (quietly) *)
let initialise() = 
  Global.init()

let _ = 
  initialise();
(** Run the proof *)
  catch_errors run ()
