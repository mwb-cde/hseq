(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(**
   A script to run hseq as a batch compiler.

   Use as [hseqb <args> <script> <script-args>]
   where
   [args] are the arguments to hseqb,
   [script] is the script to run,
   [script-args] are the arguments to pass to the script.
*)

let program_name = "hseqb"

let installdir_bin = HSeq.Config.value_BinDir
let installdir_include = HSeq.Config.value_LibDir

(** Default values *)

(* The default hseq binary. *)
let default_hseq = Filename.concat installdir_bin "hseq"

(** Default values *)

(** [hseq_include]: Includes to pass to hseq. *)
let hseq_includes = [installdir_include]

(** [hseq_libs]: Libraries to pass to hseq. .*)
let hseq_libs = []

(** [pp_include]: Includes for the preprocessor *)
let pp_include =
  String.concat " " (List.map (fun x -> "-I "^x) hseq_includes)

(** [error s]: exit print error message [s] *)
let error s =
  Printf.fprintf stderr "%s: %s\n%!" program_name s;
  exit (-1)

let extract o =
  match o with
  | Some(x) -> x
  | _ -> raise (Invalid_argument ("extract"))


(** Argument processing *)

type options =
  {
    hseq: string option;          (* The hseq binary to use. *)
    hseq_includes: string list;   (* Include directories. *)
    hseq_libs: string list;       (* Libraries to link in. *)
    verbose: bool;                (* Print the compilation command-line. *)
    dryrun: bool;                 (* Don't run the compilation. *)
    pp: string option;            (* Preprocessor command. *)
    rest: string list;            (* The remaining arguments. *)
  }

(** Initial option values *)
let default_options ()=
  {
    hseq = None;
    hseq_includes = [];
    hseq_libs = [];
    verbose = false;
    dryrun = false;
    pp = None;
    rest = [];
  }

let set_hseq option p =
   option := { (!option) with hseq = Some(p) }

let set_verbose option () =
   option := { (!option) with verbose = true }

let set_dryrun option () =
   option := { (!option) with dryrun = true }

let set_pp option p =
   option := { (!option) with pp = Some(p) }

let add_include option d =
  let incs = d::(!option).hseq_includes in
  option := { (!option) with hseq_includes = incs }

let add_lib option d =
  let libs = d::(!option).hseq_libs in
  option := { (!option) with hseq_libs = libs }

let add_other option d =
  let rs = d::(!option).rest in
  option := { (!option) with rest = rs }

(** The command line arguments *)

let parse_args () =
  let options = ref (default_options())
  in
  let cli_args =
    Arg.align
      [
        ("--with-hseq", Arg.String (set_hseq options),
         " <command> Use <command>");
        ("--verbose", Arg.Unit (set_verbose options),
         " print the command-line");
        ("--dryrun", Arg.Unit (set_dryrun options),
         " print but don't the command-line");
        ("--pp", Arg.String (set_pp options),
         "<command> Use preprocessor <command>");
        ("-I", Arg.String (add_include options),
         "<directory> Add <directory> to the list of"^
           " include directories");
        ("-L", Arg.String (add_lib options),
         "<library> Add <library> to the list of libraries to link");
      ]
  in
  let usage_msg =
  String.concat ""
  [
    "hseq: Run a script with HSeq\n";
    "  usage: hseq {hseq options} <script> {script-options}\n";
    "hseq options:"
  ]
  in
  let anon_fun s = add_other options s
  in
  begin
    Arg.parse cli_args anon_fun usage_msg;
    !options
  end

(** Check the validity of options. *)
let check_options (opts: options) =
  if opts.rest = []
  then error "No script file given."
  else ()


(** Process the command line arguments. *)

let print_cmdline cmd cmd_args script script_args =
  Printf.fprintf stdout "%s %s\n%!"
                 (String.concat " " (cmd::cmd_args))
                 (String.concat " " (script::script_args))

let rec zip_rev_add x lst rest =
  match lst with
  | [] -> rest
  | y::ys -> zip_rev_add x ys (x::y::rest)

let make_cmdline_args opts =
  let lib_args = List.rev opts.hseq_libs in
  let include_args = zip_rev_add "-I" opts.hseq_includes lib_args
  in
  let hseq_args =
    if opts.pp = None
    then include_args
    else (extract opts.pp)::include_args
  in
  let script, script_args =
    match opts.rest with
    | [] -> error "No script to run."
    | (s::a) -> (s, a)
  in
  (hseq_args, script, script_args)

let make_cmdline opts =
  let (cmd_args, script, script_args) = make_cmdline_args opts
  in
  let cmd =
    if opts.hseq = None
    then default_hseq
    else extract opts.hseq
  in
  if not (Sys.file_exists cmd)
  then error ("Can't find "^cmd)
  else (cmd, cmd_args, script, script_args)

let run_hseq_as_script (opts: options) cmd cmd_args script args =
  let commands =
    let quote = "\"" in
    [
      String.concat
        " "
        [
          "Unsafe.use_file"; "~silent:false";
          quote ^ script ^ quote;
          ";;\n"
        ];
      "#quit;;\n"
    ]
  in
  let outstrm =
    Unix.open_process_out (String.concat " " (cmd::cmd_args))
  in
  List.iter
    (fun c -> output_string outstrm c)
    commands;
  flush outstrm;
  ignore(Unix.close_process_out outstrm); 0

let run_hseq (opts: options) cmd cmd_args script args =
  let script_line = script::args in
  let cmd_line = List.rev_append (List.rev cmd_args) script_line in
  if opts.verbose || opts.dryrun
  then
    Printf.fprintf stdout "%s\n%!" (String.concat " " (cmd::cmd::cmd_line))
  else ();
  if opts.dryrun
  then 0
  else Unix.execv cmd (Array.of_list (cmd::cmd_line))

let process_cmdline (opts: options) =
  check_options opts;
  let cmd, cmd_args, script, script_args = make_cmdline opts in
(*
  if opts.dryrun || opts.verbose
  then print_cmdline cmd cmd_args script script_args
  else ();
  if opts.dryrun
  then 0
  else
 *)
  run_hseq opts cmd cmd_args script script_args

let main() =
  let options = parse_args() in
  process_cmdline options

let _ = exit (main())
