(*----
  Name: hseqstart.ml
  Copyright Matthew Wahab 2005-2016
  Author: Matthew Wahab <mwb.cde@gmail.com>

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

(**
   Install printers
*)

#install_printer HSeqUser.Userlib.Display.print_term;;
#install_printer HSeqUser.Userlib.Display.print_formula;;
#install_printer HSeqUser.Userlib.Display.print_type;;
#install_printer HSeqUser.Userlib.Display.print_sqnt;;
#install_printer HSeqUser.Userlib.Display.print_node;;
#install_printer HSeqUser.Userlib.Display.print_branch;;
#install_printer HSeqUser.Userlib.Display.print_thm;;
#install_printer HSeqUser.Userlib.Display.print_prf;;
#install_printer HSeqUser.Userlib.Display.print_prfstk;;
#install_printer HSeqUser.Userlib.Display.print_fnident;;
#install_printer HSeqUser.Userlib.Display.print_error;;
#install_printer HSeqUser.Userlib.Display.print_defn;;
#install_printer HSeqUser.Userlib.Display.print_theory;;
#install_printer HSeqUser.Userlib.Display.print_simpset;;
#install_printer HSeq.Thydb.print;;

(**
   Open modules
*)

open HSeq.Goals;;
open HSeq.Tactics;;
open HSeq.Boollib;;

open HSeq;;
open HSeqUser;;
open HSeqUser.Userlib;;
