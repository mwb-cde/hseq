(*----
 Name: 0MainScript.ml
 Copyright M Wahab 2005-2013
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

(**
   Create theory "Main" 
   Theory Main is the top-level theory (the "base theory") for the
   standard theory library.  It is imported into all theories (except
   those in the standard library).

   To avoid having theory Main imported into the standard theories,
   the base theory name is zero'd out while the standard library is built.

   This script is called 0MainScript to avoid it being called from the
   theories that Main depends on.    
*)

(* Clear the base theory name. *)
let mainScript_base_name = 
  let str = 
    try Context.Thys.get_base_name(Global.context()) 
    with _ -> "Main"
  in 
  Global.set_context(Context.Thys.clear_base_name (Global.context())); 
  str;;

(** Build theory Main and the theories it depends on *)
let _ = 
begin_theory "Main" 

 ["Set"; "Relation"; "Bool"; "base"];; 
(*
 ["Relation"; "Bool"; "base"];; 
*)
(*
 ["Set"; "Sum"; "Pair"; "Fun"; "Relation"; "Bool"; "base"];; 
*)


let _ = end_theory();;

(* Reset the base theory name *)
let _ = 
  Global.set_context(Context.Thys.set_base_name 
                       (Global.context()) 
                       mainScript_base_name);;
