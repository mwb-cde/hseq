
(*
   Utility functions which depend on unsafe/undocumented features.
*)


let use_file ?(silent=false) f = 
  if(silent)
  then ignore(Toploop.use_silently Format.std_formatter f)
  else ignore(Toploop.use_file Format.std_formatter f)


