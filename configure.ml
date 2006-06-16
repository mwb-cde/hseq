#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml
(*-----
 Name: configure.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(**
   Script to generate configuration data.
   Emits ML code (for configure.data)
   and definitions for use in a makefile.

   ML code is output to configure.data
   Makefile code is output to data.make

   Usage:
   ocaml configure.ml [options] 
   where main options are:
   --prefix: the prefix for directories.
   --basedir: the install directory
   --bindir: the executables directory
   --libdir: the libraries directory.
   --thys: the theories directory.
   --fast: whether to use the optimised compiler (ocamlc.opt)
*)

(** File name utilities **)

let filename x y = 
  if x != ""
  then Filename.concat x y
  else y

let filename_opt x y = 
  match x with
    None -> y
  | (Some d) -> Filename.concat d y


(** Test that a command exists *)

let has_program s = 
  Sys.command s = 0


(** Names of output files **)

let output_dir = "config"
let ml_data=filename output_dir "configure.data"
let make_data=filename output_dir "data.make"

(** The current directory *)
let cwd = Sys.getcwd()


(** Variables *)

let ml_code= ref true
let make_code= ref true
let bin = ref None
let prefix = ref None
let basedir = ref None
let bindir = ref None
let libdir = ref None
let thys = ref None
let output = ref None
let fast_compilers = ref None
let native_compilers = ref None

let set p x = p := Some x
let get p =
  match p with
    None -> ""
  | Some x -> x

let get_opt p d=
  match p with
    None -> d
  | Some x -> x

(** Default values **)

let bin_d () = "hseq"

let os_type = Sys.os_type

(** The standard Unix values **)
module Unix=
struct
  let prefix_d () = "/usr/local"
  let basedir_d () = 
    filename (get_opt !prefix (prefix_d()))
      (filename "lib" (get_opt !bin (bin_d())))
  let bindir_d () = 
    get_opt !basedir (basedir_d())
(*    filename (get_opt !basedir (basedir_d())) "bin" *)
  let libdir_d () = 
    filename (get_opt !basedir (basedir_d())) "lib"
  let thys_d () = 
    filename (get_opt !basedir (basedir_d())) "thys"
end

(** The standard Win32 values **)
(*
module Windows=
struct

  let prefix_d () = "\\Program\\ Files" 

  let basedir_d () = 
    filename (get_opt !prefix (prefix_d())) "HSeq"
  let bindir_d () = 
    get_opt !basedir (basedir_d())
  let libdir_d () = 
    filename (get_opt !basedir (basedir_d())) "lib"
  let thys_d () = 
    filename (get_opt !basedir (basedir_d())) "thys"
end
*)

module Windows = Unix

let prefix_d () = 
   match os_type with
     "Win32" -> Windows.prefix_d()
     | _ -> Unix.prefix_d()

let basedir_d () = 
   match os_type with
     "Win32" -> Windows.basedir_d()
     | _ -> Unix.basedir_d()

let bindir_d () = 
   match os_type with
     "Win32" -> Windows.bindir_d()
     | _ -> Unix.bindir_d()

let libdir_d () = 
   match os_type with
     "Win32" -> Windows.libdir_d()
     | _ -> Unix.libdir_d()

let thys_d () = 
   match os_type with
     "Win32" -> Windows.thys_d()
     | _ -> Unix.thys_d()


let has_fast_compilers = has_program "ocamlc.opt" 

let fast_compilers_d ()= 
  if has_fast_compilers 
  then "true"
  else "false"

let set_fast_compilers flag = 
  if flag 
  then set fast_compilers "true"
  else set fast_compilers "false"

(* let has_native_compilers = has_program "ocamlopt" *)
let has_native_compilers = false

let native_compilers_d ()= 
  if has_native_compilers 
  then "true"
  else "false"

let set_native_compilers flag = 
  if flag 
  then set native_compilers "true"
  else set native_compilers "false"

let output_ml_d () = get_opt !output ml_data
let output_make_d () = get_opt !output make_data

(** Variable list *)

let varlist = 
  [
   ("Bin", bin, bin_d);
   ("Prefix", prefix, prefix_d);
   ("BinDir", bindir, bindir_d);
   ("BaseDir", basedir, basedir_d);
   ("LibDir", libdir, libdir_d);
   ("ThyDir", thys, thys_d);
 ]

let settinglist =
  [ 
    ("FastCompilers", fast_compilers, fast_compilers_d);
    ("NativeCompilers", native_compilers, native_compilers_d)
  ]

let set_values () =
  let set_value (_, v, f) = 
    match !v with
      None -> set v (f())
    | _ -> ()
  in
  List.iter set_value varlist;
  List.iter set_value settinglist

(** Emitter functions **)

let print_ml_var oc (v, d, _) =
  match (!d) with
      None -> ()
    | _ -> 
	let str = String.escaped (get !d)
	in 
	  Printf.fprintf oc "DEFINE %s = \"%s\"\n" v str

let print_ml_setting oc (v, d, _) =
  match (!d) with
      None -> ()
    | _ -> 
	let str = String.escaped (get !d)
	in 
	  Printf.fprintf oc "DEFINE %s = \"%s\"\n" v str

let print_make_var oc (v, d, _) =
  match (!d) with
      None -> ()
    | _ -> 
	let str = String.escaped (get !d)
	in 
 	  Printf.fprintf oc "%s = '%s'\n" v str 

let print_make_setting oc (v, d, _) =
  match (!d) with
      None -> ()
    | _ -> 
	let str = String.escaped (get !d)
	in 
	  Printf.fprintf oc "%s = %s\n" v str

let make_outfile n = 
  if n = "" 
  then stdout
  else open_out n

let emit_ml ()=
  let oc = make_outfile (output_ml_d())
  in 
  List.iter (print_ml_var oc) varlist;
  List.iter (print_ml_setting oc) settinglist;
  close_out oc;
  Printf.printf "Wrote file %s\n" (output_ml_d())

let emit_make() = 
  let oc = make_outfile (output_make_d())
  in 
  List.iter (print_make_var oc) varlist;
  List.iter (print_make_setting oc) settinglist;
  close_out oc;
  Printf.printf "Wrote file %s\n" (output_make_d())

let emit () = 
  if(!ml_code)
  then emit_ml()
  else ();
  if(!make_code) 
  then emit_make() 
  else ()


(** Command line arguments **)

let arglist =
[
("--basedir", Arg.String (set basedir), 
 "The installation directory ["^(basedir_d())^"]");
("--bindir", Arg.String (set bindir), 
 "The executables directory ["^(bindir_d())^"]");
("--libdir", Arg.String (set libdir), 
 "The libraries directory ["^(libdir_d())^"]");
("--thydir", Arg.String (set thys), 
 "The theories directory ["^(thys_d())^"]");
("--fast", Arg.Bool set_fast_compilers, 
 "Use the fast compilers (ocamlc.opt) ["^(fast_compilers_d())^"]");
("--nativeocaml", Arg.Bool set_native_compilers, 
 "Use the native code compilers (ocamlopt) ["^(native_compilers_d())^"]")
]

let usage_msg = ""
let anon_fun _= raise (Arg.Bad "unknown option")

let parse_args () = Arg.parse arglist anon_fun usage_msg

(** Main **)

let _ = 
  parse_args();
  set_values ();
  emit()

  
