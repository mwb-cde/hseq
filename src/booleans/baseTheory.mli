(*-----
  Name: baseTheory.ml
  Author: M Wahab <mwahab@users.sourceforge.net>
  Copyright M Wahab 2006
  ----*)

(**
   A minimal base theory, used if no other 
   theory can be found.
*)

val builder: ?save:bool -> unit -> unit
  (** 
      Build the minimal theory. If [?save] is true, save the theory.
      (default: save=false)
  *)

(** {7 Initialising function} *)
val init: unit -> unit
  (** 
      Set {!Global.Init.set_base_thy_builder} to
      {!BaseTheory.builder}.
 *)

