(*----
  Name: hseqb.ml
  Copyright Matthew Wahab 2005-2016
  Author: Matthew Wahab <mwb.cde@gmail.com>

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

(**
   A script to run hseq as a batch compiler.

   Use as [hseqb ocaml-args file1 file2 ... filen]
   where
   [ocaml-args] are the arguments to pass to ocaml
   [file1], [file2], ..., [filen] are the files to compile.

   Each of the files is run through hseq, starting with [file1] and
   continuing to [filen].
*)

let bindir = Hseqb_config.value_BinDir
let includedir = Hseqb_config.value_LibDir

(** Start of script *)

(** Default values *)

let default_bin =
  let hseq_name = Filename.concat bindir "hseq"
  in
  let suffix =
    if (Sys.os_type = "Win32")
    then ".exe"
    else ""
  in
  hseq_name ^ suffix

let default_bin_args = []
let default_libs = [includedir]
let default_silent = false

(** [use_command f]: make the command to use file [f] *)
let use_command ?(silent=false) file =
  let use =
    if silent
    then "Unsafe.use_file ~silent:true"
    else "Unsafe.use_file ~silent:false"
  and quote = "\""
  and eocl = ";;\n"
  and quit = "#quit"
  in
  use ^quote^ file ^quote ^eocl ^ quit ^eocl

(**
   [run files]: Run each file in [files] as a script.
*)
let run silent bin args file =
  let prog = String.concat " " (bin::args)
  in
  let command = use_command ~silent:silent file
  in
  let outstrm =
    Unix.open_process_out prog
  in
    output_string outstrm command;
    flush outstrm;
    ignore(Unix.close_process_out outstrm)

(**
    [process_args ()] Process the command line arguments.

    {ul
    {- --bin s : use file [s] as the binary}
    {- --include d: add directory [d] to the search path}
    {- --quiet: don't echo the script }
    {- --verbose: echo the script }
    {- + s: pass argument [s] to the binary}
    {- s: run file [s] as a script}}
*)
let process_args () =
  let bin = ref default_bin
  in
  let lib_list = ref default_libs
  in
  let ocaml_args = ref default_bin_args
  in
  let file_list = ref []
  in
  let silent = ref default_silent
  in
  let add_to_list l s = (l:=s::(!l))
  in
  let args =
   Arg.align
    [
      ("--bin", Arg.Set_string bin,
       "<file> set the name of the binary to <file>");
      ("--include", Arg.String (add_to_list lib_list),
       "<dir> add directory <dir> to the search list");
      ("--quiet", Arg.Set silent,
       " don't echo the results of the script");
      ("--verbose", Arg.Clear silent,
       " echo the results of the script");
      ("+", Arg.String (add_to_list ocaml_args),
       "<option> pass <option> directly to the binary")
    ]
  in
    Arg.parse args (add_to_list file_list) "";
    let files = List.rev (!file_list)
    and libs = List.rev (!lib_list)
    and bin_args = List.rev (!ocaml_args)
    in
    (!bin, files, libs, bin_args, !silent)

(** [test_bin b] Make sure that binary file b exists *)
let test_bin b =
  if (Sys.file_exists b)
  then ()
  else
    (Format.printf "@[hseqb: can't find executable %s@]@." b;
     exit (-1))

(** [test_files files]: make sure that there are files to process *)
let test_files files =
  match files with
      [] ->
        (Format.printf "@[hseqb: no files@]@.";
         exit (-1))
    | _ -> ()

(**
    [construct_args libs bin_args]: construct the
    argument array to pass to the binary.

    [libs] are set as the include directories
    [bin_args] are passed as is.
*)
let construct_args libs bin_args =
  let rec make_include l rs =
    match l with
        [] -> rs
      | (x::xs) -> make_include xs (x::"-I"::rs)
  in
  let full_list = ""::(List.rev_append (make_include libs []) bin_args)
  in
    full_list

(**
   [main ()] The main function
*)
let main () =
  let (bin, files, libs, bin_args, silent) = process_args()
  in
    let args = construct_args libs bin_args
    in
      test_bin bin;
      test_files files;
      List.iter (run silent bin args) files

let _ =
  main();
  Format.printf "@.";
  exit 0
