(*----
 Name: hseqstart.ml
 Copyright M Wahab 2005-2010
 Author: M Wahab  <mwb.cde@googlemail.com>

 This file is part of HSeq

 HSeq is free software; you can redistribute it and/or modify it under
 the terms of the Lesser GNU General Public License as published by
 the Free Software Foundation; either version 3, or (at your option)
 any later version.

 HSeq is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
 License for more details.

 You should have received a copy of the Lesser GNU General Public
 License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
----*)

(**
   HSeq start-up file.
*)

open HSeq;;

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
#install_printer Display.print_prfstk;;
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

    Must be done here to make sure that all modules are loaded before
    the system is started.

let _ = Init.init();;
*)
