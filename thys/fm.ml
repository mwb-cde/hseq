
#directory "/home/mwahab/src/tp/src";;


#load "cli.cmo";;
#load "bool_tacs.cmo";;

#install_printer Printer.print_term;;
#install_printer Printer.print_typ;;
#install_printer Printer.print_sqnt;;
#install_printer Printer.print_thm;;
#install_printer Printer.print_prf;;
#install_printer Printer.print_fnident;;

Tpenv.init();;
Tpenv.add_thy_path "thys";;

open Goals;;
open Commands;;
open Tactics;;
open Bool_tacs;;

(*
#load "prop.cmo";;
#load "supinf.cmo";;
#load "nums.cmo";;
*)
