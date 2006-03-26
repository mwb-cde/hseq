#!/usr/local/bin/ocamlrun /home/mw/src/tp/src/bin/hseqc

(*-----
 Name: buildall.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(*
   Build all theories.

   Run from a shell with 
   `hseq -I INCLUDEDIR buildall.ml`
   where INCLUDEDIR is the path to the hseq include directory.
   e.g. hseq -I ../include buildall
   or hseq -I HSEQ/include buildall
   where HSEQ is the hseq install directory.
*)

(* Initialise hseq *)
let _ = Init.load_init();;

(* 
   This is part of the standard library so clear the theory base name.
*)
(* Clear the base theory name. *)
let buildall_base_name = 
  let str = try Global.Thys.get_base_name() with _ -> "Main"
  in 
  Global.Thys.clear_base_name(); str;;

#use "0MainScript.ml";;


(* Reset the base theory name *)
let _ = Global.Thys.set_base_name buildall_base_name;;
