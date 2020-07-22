(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
#install_printer HSeqUser.Userlib.Display.print_type_error;;
#install_printer HSeqUser.Userlib.Display.print_defn;;
#install_printer HSeqUser.Userlib.Display.print_theory;;
#install_printer HSeqUser.Userlib.Display.print_simpset;;
#install_printer HSeq.Thydb.print;;

(**
   Open modules
*)

open HSeq;;
open HSeq.Goals;;
open HSeq.Tactics;;
open HSeq.Boollib;;

open HSeqUser;;
open HSeqUser.Userlib;;
open HSeqUser.Userlib.Tactics;;
