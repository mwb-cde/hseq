(* Directory and files  *)

let base_dir = "/home/mw/src/tp/src/"

let make_filename f = base_dir^"/"^f

let init_file = "fm.ml"

(* suffixes *)

let thy_suffix="tho"
let script_suffix="Script.ml"


(* Sequent display *)

(** 
   [set_nice_sequent x]
   if [x] is [true], print sequents with assumption indices prefixed by 
   [!nice_sequent_prefix].
   if [x] is false, print sequent assumption indices as negative numbers.
*)
let nice_sequent_prefix=ref "~"
let nice_sequent = ref true
let set_nice_sequent x = nice_sequent:=x
let get_nice_sequent () = !nice_sequent
