(* link libraries into a runtime *)

(* fmhome: where fmcaml lives *)
let fmhome = "/home/mw/src/ocaml/tp"

(* fmhome: where the libraries live *)
let fmlibs = fmhome^"/src"


let tplibs = 
  ["basiclib.cma"; "termlib.cma";
    "logiclib.cma"; "parserlib.cma"; "coretplib.cma"] 

let mllibs = ["nums.cma"; "unix.cma"]

let clibs = ["dequals"; "nums"; "unix"]

let includedir = 
  [fmlibs^"/exts"; fmlibs]

let libdirs = [fmlibs^"/exts"]

let name="fmcaml"

let prefix p xs = 
  let rec pre_aux ls =
    match ls with 
      [] -> ""
    | y::ys -> (p^y)^pre_aux ys
  in pre_aux xs

let _ =
  let arglist = List.tl(Array.to_list Sys.argv )
  in let comm = 
    ("ocamlmktop -custom -o "^name^" "
     ^(prefix " -I " includedir)^" "
     ^"-ccopt "
     ^(prefix " -L" libdirs)^" "
     ^(prefix " -cclib -l" clibs)^" "
     ^(String.concat " " mllibs)^" "
     ^(String.concat " " tplibs)^" "
     ^(String.concat " " arglist))
  in 
  exit
    (print_string (comm^"\n"); 
     Sys.command comm)

