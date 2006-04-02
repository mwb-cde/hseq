(*-----
 Name: hseqc.mlp
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* *)

let bindir = "/home/mw/src/tp/src" ^ "/bin"
let includedir = "/home/mw/src/tp/src" ^ "/include"

let bin = Filename.concat bindir "hseq"
let bin_args = Array.of_list [""; "-I"; includedir]
let args =
  Array.append bin_args (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))

let _ = Unix.execv bin args


