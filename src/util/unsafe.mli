(*-----
 Name: unsafe.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(*
   Utility functions which depend on unsafe/undocumented features.
*)

(**
   [use_file ?silent name]

   Use file [name] as a script. Equivalent to [#use name].
   If [silent] is [true], no output, otherwise emits output like [#use].
*)
val use_file : ?silent:bool -> string -> unit

(**
   [load_file name]

   load fine [name] as a script. Equivalent to [#load name].
*)
val load_file : string -> unit

(*
   [object_suffixes]: the suffixes of file types which can be loaded
   with load_file (=[".cmo"; ".cmi"]).
*)
val object_suffix : string list

(**
   [load_use_file ?silent name]:
   if name is an object file (with a suffix in [object_suffixes]), 
   then call [load_file name]
   otherwise call [use_file ?silent name].
*)
val load_use_file : ?silent:bool -> string -> unit

(* 
   [add_directory s]: Add directory [s] to the OCaml path.
   Equivalent to [#directory s].
*)
val add_directory : string -> unit

(**
   [add_init f]:

   Setup function [f] to be called when OCaml starts.
   [f] is typically an initialisation function.
*)
val add_init: (unit -> unit) -> unit
