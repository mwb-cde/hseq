#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml
  (*-----
    Name: filetools.ml
    Author: M Wahab <mwahab@users.sourceforge.net>
    Copyright M Wahab 2006
    ----*)

(**
   File tools:

   pwd: Print the current directory
   ls: List the files
   rm: Delete one or more files
   mkdir: Make one or more directories

*)

#load "unix.cma"
#load "str.cma"

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

exception Error of string

(* let error s = raise (Error s) *)
let error s = Format.printf "@[%s@]@." s
let fatal s = Format.printf "@[%s@]@." s; exit (-1)
let report f a = Format.printf "@[%s: %s@]@." f a
	
module Util =
struct

  open Unix

  let std_perm = 0o755

  let get_perm n =
    try 
      (let stf = Unix.stat n
       in 
	 stf.Unix.st_perm )
    with _ -> std_perm

    let is_directory n = 
      try
	(let stf = Unix.stat n
	in 
	  match stf.Unix.st_kind with
	      Unix.S_DIR -> true
	    | _ -> false)
      with _ -> false
	    
  (** 
      [strexp_of f]: make a regexp from a file name f.
  *)
  let strexp_of f = 
    let alpha_num = ""
    in 
    let rec make strm l =
      match Stream.peek strm with
	  None -> String.concat "" (List.rev ("$"::l))
	| Some (chr) -> 
	    Stream.next strm;
	    let str =
	      match chr with
		  '*' -> ".*"
		| '?' -> "."
		| '.' -> "\\."
		| _ -> Str.quote (String.make 1 chr)
	    in 
	      make strm (str::l)
    in 
      make (Stream.of_string f) [alpha_num]

  (**
     [file_list regexp dir_name]: Get the list of file names in
     directory [dir_name], matching [regexp].
  *)
  let file_list regexp dir_name = 
    let dir_array = Sys.readdir dir_name
    in 
    let pred str = Str.string_match regexp str 0
    in 
    let file_names = List.filter pred (Array.to_list dir_array)
    in 
      if dir_name = Filename.current_dir_name 
      then file_names
      else
	List.map (Filename.concat dir_name) file_names

  (** [files str]: Get the list of filenames matching [str] *)
  let files str = 
    let dir_name = Filename.dirname str
    and file_name = 
      let tmp = Filename.basename str
      in 
  	if is_directory tmp
	then "*"
	else tmp
    in 
    let strexp = strexp_of file_name
    in 
    let regexp = Str.regexp strexp
    in
    file_list regexp dir_name
      

end

module Pwd =
struct

  let run () = 
    Format.printf "@[%s@]@." (Unix.getcwd())

end

module Ls =
struct

  let print s =
    Format.printf "%s@ " s

  let print_list l = 
    match l with 
	[] -> ()
      | _ -> 
	  Format.printf "@[";
	  List.iter print l;
	  Format.printf "@]@."

  let dir s = 
    print_list (Util.files s)
	
  let run args = 
    match args with
	[] -> dir "."
      | _ -> List.iter dir args

end

module Rm =
struct

  let delete s =
    let f x = 
      try Unix.unlink x
      with _ -> error ("rm: failed to delete "^x)
    in 
      List.iter f (Util.files s)
      
  let run args = 
    match args with 
	[] -> fatal "rm: No arguments"
      | _ -> List.iter delete args

end

module Mkdir =
struct

  let mkdir n = 
    try Unix.mkdir n Util.std_perm 
    with _ -> error ("mkdir: failed to make directory "^n)

  let run args = 
    match args with
	[] -> fatal "mkdir: No arguments"
      | _ -> List.iter mkdir args

end

module Copy =
struct
  open Unix

  let out_flags = 
    [O_WRONLY; O_CREAT; O_TRUNC]

  let in_flags = 
    [O_RDONLY]

  let mk_srcfile src =
    open_in_bin src 

  let mk_dstfile dst = 
    open_out_bin dst

  let copy src dst = 
    let src_file = mk_srcfile src
    and dst_file = mk_dstfile dst
    in 
    let buff_len = 128
    in 
    let buff = String.make buff_len (char_of_int 0)
    in 
    let read_len = ref buff_len
    in 
      read_len := input src_file buff 0 buff_len;
      while (!read_len)>0 do
	output dst_file buff 0 (!read_len);
	read_len := input src_file buff 0 buff_len
      done;
      flush dst_file; close_out dst_file;
      close_in src_file

  let copy_single src dst =
    let dst_name = 
      if Util.is_directory dst
      then Filename.concat dst (Filename.basename src)
      else dst
    in 
      if Util.is_directory src
      then error ("cp: "^src^" is a directory")
      else 
	if (dst_name = src)
	then error ("cp: Source and destination files are the same")
	else
	    try copy src dst_name
	    with _ -> error ("cp: failed to copy "^src)

  let copy_file src dst = 
    let files = Util.files src
    in 
      List.iter (fun s -> copy_single s dst) files

  let run args = 
    match List.rev args with 
	[] -> fatal "cp: Not enough arguments"
      | [x] -> fatal "cp: Not enough arguments"
      | [df; src] -> copy_file src df
      | df::srcs -> 
         if (Util.is_directory df)
	 then 
	    List.iter (fun s -> copy_file s df) (List.rev srcs)
	 else fatal "cp: Destination of multiple files must be a directory"
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

  let alist = 
    let f ()= Arg.usage arg_list usage_msg
    in 
      ("--usage", Unit f, "Usage")::arg_list

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
    | Null -> ()

let _ = 
  try run_cmd (ArgParse.run ()); exit 0
  with 
      Error m -> fatal m
    | err -> 
	(Format.printf "@[%s@]@." (Printexc.to_string err); exit (-1))
      
