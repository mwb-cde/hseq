#directory "/home/mw/src/tp/src";;
#directory "/home/mw/src/tp/src/include";;

(*
#load "cli.cmo";;

#load "bool_tacs.cmo";;
*)

#install_printer Term.simple_term_printer;;
(*#install_printer Printer.print_term;;*)
#install_printer Printer.print_typ;;
#install_printer Printer.print_sqnt;;
#install_printer Printer.print_thm;;
#install_printer Printer.print_prf;;
#install_printer Printer.print_fnident;;
#install_printer Printer.print_error;;


let _ =
  print_string "fm.ml: initialising\n";
  Tpenv.init();
  Tpenv.add_thy_path "thys";
  Tpenv.add_thy_path "../thys"

(*
Tpenv.init();;
Tpenv.add_thy_path "thys";;
Tpenv.add_thy_path "../thys";;
*)

open Goals;;
open Commands;;
open Tactics;;
open Bool_tacs;;
