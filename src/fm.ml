(* 
   Start up file loaded when the TP system starts.
*)

#directory "/home/mw/src/tp/src";;
#directory "/home/mw/src/tp/src/include";;

(*
#load "cli.cmo";;
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

open Goals;;
open Commands;;
open Tactics;;
open Boollib;;
open Simplib;;

let _ =
  Format.open_box 0;
  Format.print_string "Initialising";
  Format.print_newline();
  Format.close_box();
  Tpenv.add_thy_path "./thys";
  Tpenv.add_thy_path "../thys";
  Tpenv.init()

