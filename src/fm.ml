(*----
 Name: fm.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* 
   Start up file loaded when the system starts.
*)

(*#install_printer Term.simple_term_printer;;*)

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

open Goals;;
open Tactics;;
open Boollib;;
open Simplib;;
open Userlib;;

let _ =
  Format.open_box 0;
  Format.print_string "Initialising";
  Format.print_newline();
  Format.close_box();
  Init.init();
  Global.add_thy_path "./thys";
  Global.add_thy_path "../thys"

