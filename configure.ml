#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml
(*-----
 Name: configure.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(*
   Script to generate configuration data.
   Emits ML code (for configure.data)
   and definitions for use in a makefile.

   ML code is output to configure.data
   Makefile code is output to data.make

   Usage:
   ocaml configure.ml [options] 
   where main options are:
   --output: output file name (default data)
   --ml: emit for ML (default)
   --noml: don't emit for ML 
   --makefile: emit for makefile (default)
   --nomakefile: don't emit for makefile 
   --output f: output to file name f. 
   --prefix: the prefix for directories.
   --basedir: the install directory
   --bindir: the executables directory
   --libdir: the libraries directory.
   --thys: the theories directory.
   --toolbox: use the os-neutral file tools
*)

(* File name utilities *)

let filename x y = 
  if x != ""
  then Filename.concat x y
  else y

let filename_opt x y = 
  match x with
    None -> y
  | (Some d) -> Filename.concat d y

(* Names of output files *)
let output_dir = "config"
let ml_data=filename output_dir "configure.data"
let make_data=filename output_dir "data.make"

(** The current directory *)
let cwd = Sys.getcwd()

(* The variables and default values *)

let ml_code= ref true
let make_code= ref true
let bin = ref None
let prefix = ref None
let basedir = ref None
let bindir = ref None
(*let includedir = ref None*)
let libdir = ref None
let thys = ref None
let output = ref None
let toolbox = ref None

let set p x = p := Some x
let get p =
  match p with
    None -> ""
  | Some x -> x

let get_opt p d=
  match p with
    None -> d
  | Some x -> x

(** The default values *)
let bin_d () = "hseq"

let prefix_d () = "/usr/local"
let basedir_d () = filename (filename (prefix_d()) "lib") (bin_d())
(*
let includedir_d () = 
  filename (get_opt !basedir  (basedir_d())) "include"
*)
let bindir_d () = get_opt !basedir (basedir_d())
let libdir_d () = 
  filename (get_opt !basedir (basedir_d())) "lib"
let thys_d () = 
  filename (get_opt !basedir (basedir_d())) "thys"

(** The standard Unix values **)
module Unix=
struct
let prefix_d () = "/usr/local"
let basedir_d () = 
  filename (get_opt !prefix (prefix_d()))
    (filename "lib" (get_opt !bin (bin_d())))
(*
let includedir_d () = 
  filename (get_opt !basedir  (basedir_d())) "include"
*)
let bindir_d () = 
  filename (get_opt !prefix (prefix_d())) "bin"
let libdir_d () = 
  filename (get_opt !basedir (basedir_d())) "lib"
let thys_d () = 
  filename (get_opt !basedir (basedir_d())) "thys"
end

let toolbox_file = 
  Filename.concat cwd (Filename.concat "config" "filetools.ml")

let set_toolbox () = 
  set toolbox toolbox_file

let output_d () = get_opt !output ml_data
let output_make_d () = get_opt !output make_data


(* Command line arguments *)

let arglist =
[
(*
("--ml", Arg.Set ml_code, "Emit ML data [default]");
("--noml", Arg.Clear ml_code, "Don't emit ML data");
("--makefile", Arg.Set ml_code, "Emit makefile definitions [default]" );
("--nomakefile", Arg.Set ml_code, "don't emit makefile definitions");
("--output", Arg.String (set output), 
 "The output file ["^(output_d())^", "^(output_make_d())^"]");
("-o", Arg.String (set output), 
 "The output file ["^(output_d())^", "^(output_make_d())^"]");
("--bin", Arg.String (set bin),
 "The name of the binary ["^(bin_d())^"]");
("--prefix", Arg.String (set prefix), 
 "The path to the top of the installation directory ["^(prefix_d())^"]");
*)
("--basedir", Arg.String (set basedir), 
 "The installation directory ["^(basedir_d())^"]");
("--bindir", Arg.String (set bindir), 
 "The executables directory ["^(bindir_d())^"]");
("--libdir", Arg.String (set libdir), 
 "The libraries directory ["^(libdir_d())^"]");
("--thydir", Arg.String (set thys), 
 "The theories directory ["^(thys_d())^"]");
("--toolbox", Arg.Unit set_toolbox, 
 "Use the os-neutral files (unreliable)")
]

let usage_msg = ""
let anon_fun _= raise (Arg.Bad "unknown option")

let parse_args () = Arg.parse arglist anon_fun usage_msg

(* Emitter functions *)

let varlist = 
  [
   ("Bin", bin, bin_d);
   ("Prefix", prefix, prefix_d);
   ("BinDir", bindir, bindir_d);
   ("BaseDir", basedir, basedir_d);
(*   ("IncludeDir", includedir, includedir_d); *)
   ("LibDir", libdir, libdir_d);
   ("ThyDir", thys, thys_d);
 ]

let settinglist =
  [ 
    ("TOOLBOX", toolbox) 
  ]

let print_ml_var oc (v, d, _) =
  match (!d) with
      None -> ()
    | _ -> Printf.fprintf oc "DEFINE %s = \"%s\"\n" v (get !d)

let print_ml_setting oc (v, d) =
  match (!d) with
      None -> ()
    | _ -> Printf.fprintf oc "DEFINE %s = \"%s\"\n" v (get !d)

let print_make_var oc (v, d, _) =
  match (!d) with
      None -> ()
    | _ -> Printf.fprintf oc "%s = %s\n" v (get !d)

let print_make_setting oc (v, d) =
  match (!d) with
      None -> ()
    | _ -> Printf.fprintf oc "%s = %s\n" v (get !d)

let make_outfile n = 
  if n = "" 
  then stdout
  else open_out n

let emit_ml ()=
  let oc = make_outfile (output_d())
  in 
  List.iter (print_ml_var oc) varlist;
  List.iter (print_ml_setting oc) settinglist;
  close_out oc

let emit_make() = 
  let oc = make_outfile (output_make_d())
  in 
  List.iter (print_make_var oc) varlist;
  List.iter (print_make_setting oc) settinglist;
  close_out oc

let emit () = 
  if(!ml_code)
  then emit_ml()
  else ();
  if(!make_code) 
  then emit_make() 
  else ()

(* Main *)

let set_values () =
  let set_value (_, v, f) = 
    match !v with
      None -> set v (f())
    | _ -> ()
  in
  List.iter set_value varlist

let _ = 
  parse_args();
  set_values ();
  emit()

  
