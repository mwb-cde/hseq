
(*
   Utility functions which depend on unsafe/undocumented features.
*)

(**
   [use_file ?silent name]

   Use file [name] as a script. Equivalent to [#use name].
   If [silent] is [true], no output, otherwise emits output like [#use].
*)
val use_file : ?silent:bool -> string -> unit

