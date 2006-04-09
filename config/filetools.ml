#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml
  (*-----
    Name: toolbox.ml
    Author: M Wahab <mwahab@users.sourceforge.net>
    Copyright M Wahab 2006
    ----*)

(**
   File tools:
   Unix-like front-end to OS file tools.

   pwd: Print the current directory
   ls: List the files
   rm: Delete one or more files
   mkdir: Make one or more directories
*)

let set x v = x:=v
let get x = !x
let add x l = l:=(x::(!l))

let set_d x v= 
  match x with
      None -> ()
    | Some(n) -> (n:=v)

let get_d x d = 
  match x with
      None -> d
    | Some(y) -> (!y)

type cmd = Null | Pwd | Ls | Rm | Mkdir | Cp

let error s = Format.printf "@[%s@]@." s
let fatal s = Format.printf "@[%s@]@." s; exit (-1)
let report f a = Format.printf "@[%s: %s@]@." f a
	

(** [ostype]: The supported operating systems *)
type os = Unix | Win32

let ostype = ref Unix

let _ = 
  if Sys.os_type = "Win32" 
  then ostype := Win32
  else ()

(** Utilities *)

module UnixCmds =
struct

  let pwd = "pwd"
  let ls = "ls"
  let rm = "rm"
  let mkdir = "mkdir"
  let copy = "cp"

  let file n = n
end

module WinCmds =
struct

  let pwd = "pwd"
  let ls = "dir"
  let rm = "del /Q"
  let mkdir = "mkdir"
(*  let copy = "xcopy /C/R/Y" *)
  let copy = "copy /B/Y" 

  let repl chr =
    match chr with
	'/' -> '\\'
      | _ -> chr

  let rec replace ctr str =
    String.set str ctr (repl (String.get str ctr));
    if ctr =0 then str
    else replace (ctr -1) str

  let file n = 
    let sz = (String.length n)-1
    in
      replace sz n

end

module Cmd =
struct

  let pwd () = 
    match !ostype with
	Win32 -> WinCmds.pwd
      | _ -> UnixCmds.pwd

  let ls ()= 
    match !ostype with
	Win32 -> WinCmds.ls
      | _ -> UnixCmds.ls

  let rm ()= 
    match !ostype with
	Win32 -> WinCmds.rm
      | _ -> UnixCmds.rm

  let mkdir ()= 
    match !ostype with
	Win32 -> WinCmds.mkdir
      | _ -> UnixCmds.mkdir

  let copy ()= 
    match !ostype with
	Win32 -> WinCmds.copy
      | _ -> UnixCmds.copy

end

let file = 
  match (!ostype) with
      Win32 -> WinCmds.file
    | _ -> UnixCmds.file

let exec cmd args = 
  let args1 = List.map file args
  in 
  let cl = String.concat " " (cmd::args1)
  in 
    Sys.command cl

module Copy =
struct

  let copy_file src dst = 
    exec (Cmd.copy()) [src; dst]

  let rec copy_many src dst = 
    match src with
      [] -> 0
     | (x::xs) -> ignore(copy_file x dst); copy_many xs dst

  let run args = 
    match List.rev args with 
	[] -> fatal "cp: Not enough arguments"
      | [x] -> fatal "cp: Not enough arguments"
      | [dst; src] -> copy_file src dst
      | dst::srcs -> copy_many (List.rev srcs) dst


end

module Pwd =
struct

  let run args = 
    (Format.printf "@[%s@]@." (Sys.getcwd())); 0

end

module Ls =
struct

  let run args = 
    exec (Cmd.ls()) args

end

module Rm =
struct

  let delete s =
    exec (Cmd.rm()) [s]
      
  let run args = 
    match args with 
	[] -> fatal "rm: No arguments"
      | _ ->  List.iter (fun x -> ignore(delete x)) args; 0

end

module Mkdir =
struct

  let mkdir n = 
    exec (Cmd.mkdir()) [n]

  let run args = 
    match args with
	[] -> fatal "mkdir: No arguments"
      | _ -> 
	  List.iter (fun x -> ignore(mkdir x)) args; 0

end

module ArgParse =
struct

  open Arg

  let cmd = ref Null

  let args = ref []

  let add_arg str = add str args
  let set_cmd x ()= cmd:=x 

  let anon_fun = add_arg

  let usage_msg = ""

  let arg_list =
    [
      "--pwd", Unit (set_cmd Pwd), "Current directory";
      "--ls", Unit (set_cmd Ls), "List directory";
      "--rm", Unit (set_cmd Rm), "Remove file";
      "--mkdir", Unit (set_cmd Mkdir), "Make a directory";
      "--cp", Unit (set_cmd Cp), "Copy file"
    ]


  let usage() = Arg.usage arg_list usage_msg
  let alist = 
      ("--usage", Unit usage, "Usage")::arg_list

  let run () = 
    Arg.parse alist anon_fun usage_msg;
    let c = !cmd
    and ags = List.rev (!args)
    in 
      (c, ags)

end

let run_cmd (c, args) = 
  match c with
      Pwd -> Pwd.run ()
    | Ls -> Ls.run args
    | Rm -> Rm.run args
    | Mkdir -> Mkdir.run args
    | Cp -> Copy.run args
    | Null -> (ArgParse.usage(); 0)

let _ = 
  try ignore(run_cmd (ArgParse.run ())); ()
  with err -> fatal (Printexc.to_string err)
      
