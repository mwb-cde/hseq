(*-----
 Name: cli.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Lib

let use_string st =  
  ignore(List.map 
     (Toploop.execute_phrase true Format.std_formatter) 
     ((!Toploop.parse_use_file ) (Lexing.from_string st)))


  let history = ref []

  let should_save = ref false 
  let repl_flag = ref false

  let clear ()= history := []


  let signal () = should_save:=true

let read_input () = input_line stdin

let catch_errors f arg =
  try Commands.catch_errors f arg
  with x -> 
    (Format.printf "@[%s@]@." (Printexc.to_string x))

  let get_input () = 
    Format.printf "@[> @]@?";
    Commands.catch_errors read_input ()

  let repl () =
    while !repl_flag do
      should_save:=false;
      let str= get_input()
      in 
      use_string str;
      if !should_save 
      then history:=str::(!history)
      else ()
    done

  let stop ()= 
    repl_flag:=false; should_save:=false; 
    Goals.set_hook (fun () -> ()) 

  let start () = 
    clear();repl_flag:=true; Goals.set_hook signal; repl()

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


