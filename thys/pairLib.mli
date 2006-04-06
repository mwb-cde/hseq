(*-----
Name: pairLib.mli
Author: M Wahab <mwahab@users.sourceforge.net>
Copyright M Wahab 2006
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

val pair_thy : string
(** [pair_thy]: The name of the theory of pairs *)

val pair_id : Ident.t
(** [pair_id] The identifier of the pair constructor *)

(** 
   [pair_prec] The precedence of the pair identifier 

   [pair_fixity] The fixity of the pair identifier 
*)
val pair_prec: int
val pair_fixity: Printer.fixity

val pair_printer:
    Printer.ppinfo -> (Printer.fixity * int) 
      -> (Basic.term * Basic.term list) Printer.printer
