(*----
 Name: cli.ml
 Copyright M Wahab 2005-2010
 Author: M Wahab  <mwb.cde@googlemail.com>

 This file is part of HSeq

 HSeq is free software; you can redistribute it and/or modify it under
 the terms of the Lesser GNU General Public License as published by
 the Free Software Foundation; either version 3, or (at your option)
 any later version.

 HSeq is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
 License for more details.

 You should have received a copy of the Lesser GNU General Public
 License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
----*)

open HSeq
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
  clear(); repl_flag:=true; 
  Userlib.set_proof_hook signal;
  repl()

let stop ()= 
  repl_flag:=false; should_save:=false; 
  Userlib.set_proof_hook (fun () -> ())

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


