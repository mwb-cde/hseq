(*-----
 Name: 0MainScript.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
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
  let str = try Global.Thys.get_base_name() with _ -> "Main"
  in 
  Global.Thys.clear_base_name(); str;;

(** Build theory Main and the theories it depends on *)
let _ = 
begin_theory "Main" 
["Set"; "Sum"; "Pair"; "Fun"; "Relation"; "Bool"; "base"];;

let _ = end_theory();;

(* Reset the base theory name *)
let _ = Global.Thys.set_base_name mainScript_base_name;;
