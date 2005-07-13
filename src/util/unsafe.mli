(*-----
 Name: unsafe.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(**
  Functions which depend on unsafe/undocumented features of OCaml.
*)

val use_string: string -> unit
(**
   [use_string str]: Evaluate string [str] as an ML expression.
   Raises [Failure] if evaluating [str] fails for any reason.
*)

val use_file : ?silent:bool -> string -> unit
(**
   [use_file ?silent name]: Use file [name] as a script. Equivalent to
   [#use name].  If [silent] is [true], no output, otherwise emits
   output like [#use].
*)

val object_suffix : string list
(*
   [object_suffixes]: the suffixes of file types which can be loaded
   with load_file (=[".cmo"; ".cmi"]).
*)

val load_file : string -> unit
(**
   [load_file name]: Load file [name] as a script. Equivalent to
   [#load name].
*)

val load_use_file : ?silent:bool -> string -> unit
(**
   [load_use_file ?silent name]: If name is an object file (with a
   suffix in [object_suffixes]) then call [load_file name] otherwise
   call [use_file ?silent name].
*)

val add_directory : string -> unit
(** 
   [add_directory s]: Add directory [s] to the OCaml path.
   Equivalent to [#directory s].
*)

val add_init: (unit -> unit) -> unit
(**
   [add_init f]:  Setup function [f] to be called when OCaml starts.
   [f] is typically an initialisation function.
*)
