(*-----
   Name: cli.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

open Lib

let history = ref []

let should_save = ref false 
let repl_flag = ref false

let clear ()= history := []

let signal x = should_save:=true

let read_input () = input_line stdin

let catch_errors f a =
  try f a 
  with _ -> ()

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

let stop ()= 
  repl_flag:=false; should_save:=false; 
  Goals.set_hook (fun () -> ()) 

let start () = 
  clear(); repl_flag:=true; Goals.set_hook signal; repl()

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
  print_aux (List.rev (!history));
  Format.printf "@]"
    
let save fn = ()


