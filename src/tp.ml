
(*
#directory "./include";;
#directory "./src";;

#load "./src/lib.cmo";;
#load "./src/basic.cmo";;
*)

open Lib;;

#load "corepp.cmo"
#load "result.cmo";;

#load "types.cmo";;
#load "term.cmo";;


#load "logicterm.cmo";;
#load "dbterm.cmo";;
#load "typing.cmo";;
#load "unify.cmo";;


#load "net.cmo";;
#load "rewrite.cmo";;
#load "formula.cmo";;

#load "defn.cmo";;
#load "logic.cmo";;


#load "lexers.cmo";;
#load "termlex.cmo";;
#load "termlexer.cmo";;

#load "parsers.cmo";;
#load "parser.cmo";;

#load "theory.cmo";;
#load "thydb.cmo";;
#load "env.cmo";;


#load "drule.cmo";;
#load "tactics.cmo";;
#load "tacticals.cmo";;

#load "goals.cmo";;

#load "printer.cmo";;
#load "commands.cmo";;
#load "interface.cmo";;
#load "bool_tacs.cmo";;

(*#install_printer Printer.print_term;;*)
#install_printer Term.simple_term_printer;;
#install_printer Printer.print_typ;;
#install_printer Printer.print_sqnt;;
#install_printer Printer.print_thm;;
#install_printer Printer.print_prf;;
#install_printer Printer.print_fnident;;
(*
#install_printer Printer.print_errors;;
#install_printer Printer.print_result;;
*)

Env.build_id_info();;
Env.build_type_info();;

open Goals;;
open Commands;;
open Interface;;
open Tactics;;
open Tacticals;;
open Bool_tacs;;


