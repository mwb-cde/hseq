(*-----
   Name: cli.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

open Lib

(***
* Utility functions
***)

let read_input () = input_line stdin

(***
* Data
***)
let history = ref []
let should_save = ref false 
let repl_flag = ref false

(** signal: Function to attach to proof command hooks. *)
let signal x = should_save:=true


(***
* The read-eval-print loop
***)
let get_input () = 
  Format.printf "@[> @]@?";
  Userlib.catch_errors read_input ()

let repl () =
  while !repl_flag do
    should_save:=false;
    let str= get_input()
    in 
    (try
      Unsafe.use_string str;
      if !should_save 
      then history:=str::(!history)
      else ()
    with _ -> Format.printf "@[Input error@]@.")
  done

(***
* Functions 
***)

let clear ()= history := []
let get () = List.rev (!history)

let start () = 
  clear(); repl_flag:=true; Goals.set_hook signal; repl()

let stop ()= 
  repl_flag:=false; should_save:=false; 
  Goals.set_hook (fun () -> ()) 

let restart () = 
  repl_flag:=true; Goals.set_hook signal; repl()

let print () = 
  let rec print_aux hs = 
    match hs with
      [] -> ()
    | (h::hhs) -> 
	Format.printf "@[%s@]@," h; print_aux hhs
  in 
  Format.printf "@[<v>";
  print_aux (get());
  Format.printf "@]"
    
let save fn = ()


