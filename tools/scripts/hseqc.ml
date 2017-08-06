(*----
  Name: hseqc.ml
  Copyright Matthew Wahab 2005-2017
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
   hseqc: Support for compiling against the HSeq library.

   usage: hseqc {hseqc options} -- {ocamlc options}

   Example:

   Compile test.ml using the native-code compiler
     hseqc -o test test.ml

   Compile test.ml using the native-code compiler
     hseqc --native -- -o test test.ml
*)

let program_name = "hseqc"

(** [installdir_bin]: The binaries directory of the HSeq installation. *)
let installdir_bin = HSeq.Config.value_BinDir

(** [installdir_include]: The include directory of the HSeq installation. *)
let installdir_include = HSeq.Config.value_LibDir

(** [installdir_thys]: The theories directory of the HSeq installation. *)
let installdir_thys = HSeq.Config.value_ThyDir

(** Whether native builds are available. *)
let can_build_native = HSeq.Config.value_CONFIG_ENABLE_NATIVECODE = "true"

(** Start of script *)

(** Default values *)

(** [hseq_include]: The location of the hseq libs in the order that they
    should appear on the comand line. *)
let hseq_includes = [installdir_include]

(** [hseq_libs]: The base libraries need to compile against HSeq in the
    order that they should appear on the comand line.*)
let hseq_libs = ["nums"; "unix"; "hseq"; "hsequser"]

(** [pp_include]: Includes for the preprocessor *)
let pp_include =
  String.concat " " (List.map (fun x -> "-I "^x) hseq_includes)

(** [ocamlc_include]: include directories for the ocamlc compiler (in order)*)
let ocamlc_include = []

(** [ocamlc_libs]: standard librariess for the ocamlc compiler (in order)*)
let ocamlc_libs = ["nums"; "unix"]

(** Functions *)

(** [error s]: exit print error message [s] *)
let error s =
  Format.printf "@[%s: %s@]@." program_name s;
  exit (-1)

let extract o =
  match o with
  | Some(x) -> x
  | _ -> raise (Invalid_argument ("extract"))


(** [bytelib n]: Make the name of byte-code library [n] *)
let bytelib n = n^".cma"

(** [natlib n]: Make the name of native-code library [n] *)
let natlib n = n^".cmxa"

let ocamlc = "ocamlc"
let ocamlopt = "ocamlopt"

(** Argument processing *)

type options =
  {
    print_lib: bool;              (* Print the list of required libraries. *)
    print_include: bool;          (* Print the list of include directories. *)
    print_all: bool;              (* Print the libraries and includes. *)
    ocaml_includes: string list;  (* OCaml include directories. *)
    ocaml_libs: string list;      (* Ocaml libraries to link in. *)
    ocaml_output: string option;  (* Name of the output file. *)
    compile_only: bool;           (* Compile but don't link. *)
    native: bool;                 (* Compile for native code. *)
    verbose: bool;                (* Print the compilation command-line. *)
    dryrun: bool;                 (* Don't run the compilation. *)
    pp: string option;            (* Preprocessor command. *)
    sources: string list;         (* The source files. *)
    rest: string option;          (* Remainder of the command line. *)
  }

(** Initial option values *)
let default_options ()=
  {
    print_lib = false;
    print_include = false;
    print_all = false;
    ocaml_includes = [];
    ocaml_libs = [];
    ocaml_output = None;
    compile_only = false;
    native = false;
    verbose = false;
    dryrun = false;
    pp = None;
    sources = [];
    rest = None;
  }

let set_print_all (option: (options)ref) () =
   option := { (!option) with print_all = true }

let set_print_lib option () =
   option := { (!option) with print_lib = true }

let set_print_include option () =
   option := { (!option) with print_include = true }

let set_native option () =
   option := { (!option) with native = true }

let set_verbose option () =
   option := { (!option) with verbose = true }

let set_dryrun option () =
   option := { (!option) with dryrun = true }

let set_pp option p =
   option := { (!option) with pp = Some(p) }

let set_ocaml_output option p =
   option := { (!option) with ocaml_output = Some(p) }

let set_compile_only option () =
   option := { (!option) with compile_only = true }

let add_ocaml_include option d =
  let incs = d::(!option).ocaml_includes in
  option := { (!option) with ocaml_includes = incs }

let add_ocaml_lib option d =
  let libs = d::(!option).ocaml_libs in
  option := { (!option) with ocaml_libs = libs }

let add_source option d =
  let srcs = d::(!option).sources in
  option := { (!option) with sources = srcs }

let set_rest option r =
  option := { (!option) with rest = Some(r) }

(** Check the validity of options. *)
let check_options (opts: options) =
  if opts.native && (not can_build_native)
  then
    error "@[can't build for native code (--native selected)@]"
  else ()

(** The command line arguments *)

let parse_args () =
  let options = ref (default_options())
  in
  let cli_args =
    Arg.align
      [
        ("--native", Arg.Unit (set_native options),
         " native-code compilation");
        ("--verbose", Arg.Unit (set_verbose options),
         " print the command-line passed to the compiler");
        ("--dryrun", Arg.Unit (set_dryrun options),
         " print but don't the command-line"^
           " that would be passed to the compiler");
        ("--print-libs", Arg.Unit (set_print_lib options),
         " print library information");
        ("--print-includes", Arg.Unit (set_print_include options),
         " print includes directory information");
        ("--print-all", Arg.Unit (set_print_all options),
         " print all information");
        ("--pp", Arg.String (set_pp options),
         "<command> Use preprocessor <command>");
        ("-I", Arg.String (add_ocaml_include options),
         "<directory> Add <directory> to the list of"^
           " OCaml include directories");
        ("-L", Arg.String (add_ocaml_lib options),
         "<library> Add <library> to the list of OCaml libraries to link");
        ("-o", Arg.String (set_ocaml_output options),
         "<file> pass option \"-o <file>\" to the OCaml compiler");
        ("-c", Arg.Unit (set_compile_only options),
         "<file> pass option \"-c <file>\" to the OCaml compiler");
        ("--", Arg.Rest (set_rest options),
         "<rest> pass remaining options directly to the underlying compiler");
      ]
  in
  let usage_msg =
  String.concat ""
  [
    "hseqc: Support for compiling with HSeq\n";
    "  usage: hseqc {hseqc options} -- {compiler options} {source files}\n";
    "hseqc options:"
  ]
  in
  let anon_fun s = add_source options s
  in
  begin
    Arg.parse cli_args anon_fun usage_msg;
    !options
  end

(** Add a flag to a list of strings. *)
let rec add_flag flag xs result =
  match xs with
  | [] -> List.rev result
  | (y::ys) -> add_flag flag ys (y::flag::result)

(** Construct the preprocessor flags for the command line. *)
let make_cmdline_pp opts =
  if opts.pp = None
  then []
  else ["-pp"; extract opts.pp ]

let make_cmdline_includes opts =
  let includes = List.rev_append opts.ocaml_includes hseq_includes
  in
  add_flag "-I" includes []

let make_cmdline_libs opts =
  let libs =
    List.rev (List.rev_append opts.ocaml_libs (List.rev hseq_libs))
  in
  if opts.native
  then List.map natlib libs
  else List.map bytelib libs

(** Make the arguments part of a command line. (That is, everything except the
    compiler command. Returns a reversed list of the arguements to be passed
    in. *)
let make_base_cmdline_flags (opts: options) =
  (* Constructs a reversed list of flags. *)
  let rev_add xs ys = List.rev_append xs ys
  in
  let include_flags = rev_add (make_cmdline_includes opts) [] in
  let pp_flag = rev_add (make_cmdline_pp opts) include_flags in
  let lib_flags = rev_add (make_cmdline_libs opts) pp_flag
  in
  lib_flags

(** Make a command line. *)
let make_cmdline_flags (opts: options) =
  let add xs ys = List.rev_append xs ys in
  let base_flags = make_base_cmdline_flags opts in
  let c_flag =
    if opts.compile_only
    then add ["-c"] base_flags
    else base_flags
  in
  let output_flag =
    if opts.ocaml_output <> None
    then add ["-o"; extract opts.ocaml_output] c_flag
    else c_flag
  in
  let sources_part = add opts.sources output_flag in
  let rest_part =
    if opts.rest <> None
    then add [extract opts.rest] sources_part
    else sources_part
  in
  List.rev rest_part

(** Choose a compiler. *)
let make_compiler (opts: options) =
  if opts.native
  then ocamlopt
  else ocamlc

(** Make a command line. *)
let make_cmdline (opts: options) =
  let flags = make_cmdline_flags opts in
  let compiler = make_compiler opts in
  compiler::flags

let print_info info args =
  let args_str = String.concat " " args
  in
  let info_str = String.concat " " [info; args_str]
  in
  (Format.printf "@[%s@]@." info_str); 0

let compile (opts: options) =
  let cmdline = make_cmdline opts in
  let command = String.concat " " cmdline
  in
  begin
    if opts.verbose || opts.dryrun
    then Format.printf "@[%s@]@." command
    else ();
    if not opts.dryrun
    then Sys.command command
    else 0
  end

let print_libs options =
  let libs = make_cmdline_libs options in
  let str = String.concat " " libs
  in
  Format.printf "@[%s@]@." str

let print_includes sep options =
  let dirs = options.ocaml_includes in
  if sep = None
  then
    List.iter
      (fun s -> Format.printf "@[%s@]@." s)
      dirs
  else
    let str = String.concat (extract sep) dirs in
    Format.printf "@[%s@]@." str

let print_all options =
  Format.printf "@[includes: ";
  print_includes (Some ", ") options;
  Format.printf "libraries: ";
  print_libs options;
  Format.printf "@]@."

let process_print options =
  let opts =
    [(options.print_lib, print_libs);
     (options.print_include, print_includes None);
     (options.print_all, print_all)]
  in
  List.fold_left
    (fun x (b, f) -> if b then ((f options); true) else x)
    false opts

let process_cmdline options =
  check_options options;
  if process_print options
  then 0
  else compile options

let main() =
  let options = parse_args() in
  process_cmdline options

let _ = exit (main())
