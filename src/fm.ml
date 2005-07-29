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
#install_printer Thydb.print;;

let no_print_simpset (s:Simpset.simpset) = ();;

let simple_theory_print (thy:Theory.thy) =
  Format.printf "@[<hov 2>Theory: %s,@ " (Theory.get_name thy);
  Format.printf "@[Parents: @[<hov 2>";
  Printer.print_sep_list (Format.print_string, ",") (Theory.get_parents thy);
  Format.printf ".@]@]";;

#install_printer Thydb.NameSet.print;; 
#install_printer simple_theory_print;; 
#install_printer Term.print_simple;;

open Goals;;
open Tactics;;
open Boollib;;
open Simplib;;
open Userlib;;

let _ =
  Format.printf "@[<v>HSeq@,";
  Init.init();
  Global.add_thy_path "./thys";
  Global.add_thy_path "../thys";
  Format.printf "@]" ;
  ()


