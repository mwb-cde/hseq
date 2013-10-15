(*----
  Name: userlib.ml
  Copyright M Wahab 2013
  Author: M Wahab  <mwb.cde@gmail.com>

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

open Userstate

let init () = 
  let st = 
    List.fold_left (fun a f -> f a) (State.empty())
      [init_context; init_scope;
       init_ppinfo; init_parsers;
       init_simpset; init_proofstack;
       init_base_thy_builder]
  in
  set_state st

(** {5 Initialising functions} *)
(**
let init () = Userstate.init()
**)
let reset() = init()
