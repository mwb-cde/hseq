(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

open HSeq
open HSeqUser
open HSeqUser.Userlib
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
  repl_flag:=true;
  Global.set_state
    (Userstate.set_proofstack (Global.state())
       (Goals.set_hook signal (Userstate.proofstack (Global.state()))));
  repl()

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
