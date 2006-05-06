(*----
   Name: hseqstart.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2006
   ----*)

(**
   HSeq start-up file.
*)

open HSeq

(**
   Install printers
*)

#install_printer Display.print_term;; 
#install_printer Display.print_formula;;
#install_printer Display.print_type;;
#install_printer Display.print_sqnt;;
#install_printer Display.print_node;;
#install_printer Display.print_branch;;
#install_printer Display.print_thm;;
#install_printer Display.print_prf;;
#install_printer Display.print_fnident;;
#install_printer Display.print_error;;
#install_printer Display.print_defn;;
#install_printer Display.print_theory;;
#install_printer Simplib.print_set;;
#install_printer Thydb.print;;

(**
   Open modules
*)
open Goals;;
open Tactics;;
open Boollib;;
open Simplib;;
open Userlib;;


(** 
    Initialise the system

    Must be done here since module Init is linked in to HSeq too
    early.
*)
let _ = Init.init();;

