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

   ML code is output to data
   Makefile code is output to data.make

   Usage:
   ocaml configure.ml [options] 
   where main options are:
   --output: output file name (default data)
   --ml: emit for ML (default)
   --noml: don't emit for ML 
   --makefile: emit for makefile (default)
   --nomakefile: don't emit for makefile 
   --output f: output to file name f. (default is stdout).
   --prefix: the prefix for directories.
   --base: the install directory
   --bindir: the executables directory
   --includedir: the headers directory
   --libs: the libraries directory.
   --thys: the theories directory.
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

(* The variables and default values *)

let ml_code= ref true
let make_code= ref true
let bin = ref None
let prefix = ref None
let base = ref None
let bindir = ref None
let includedir = ref None
let libs = ref None
let thys = ref None
let output = ref None

let set p x = p := Some x
let get p =
  match p with
    None -> ""
  | Some x -> x

let get_opt p d=
  match p with
    None -> d
  | Some x -> x

(* The default values *)
let bin_d () = "hseq"
let prefix_d () = "/usr/local/"
let base_d () = 
  filename (get_opt !prefix (prefix_d()))
    (filename "lib" (get_opt !bin (bin_d())))
let includedir_d () = 
  filename (get_opt !base  (base_d())) "include"
let bindir_d () = 
  filename (get_opt !prefix (prefix_d())) "bin"
let libs_d () = 
  filename (get_opt !base (base_d())) "libs"
let thys_d () = 
  filename (get_opt !base (base_d())) "thys"
let output_d () = get_opt !output "data"
let output_make_d () = (get_opt !output (output_d()))^".make"

(* Command line arguments *)

let arglist =
[
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
("--base", Arg.String (set base), 
 "The installation directory ["^(base_d())^"]");
("--bindir", Arg.String (set bindir), 
 "The executables directory ["^(bindir_d())^"]");
("--includedir", Arg.String (set includedir), 
 "The includedir directory ["^(includedir_d())^"]");
("--libdir", Arg.String (set libs), 
 "The libraries directory ["^(libs_d())^"]");
("--thydir", Arg.String (set thys), 
 "The theories directory ["^(thys_d())^"]")
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
   ("BaseDir", base, base_d);
   ("IncludeDir", includedir, includedir_d);
   ("LibDir", libs, libs_d);
   ("ThyDir", thys, thys_d)
 ]

let printer_ml oc (v, d, _) =
  Printf.fprintf oc "DEFINE %s = \"%s\" \n" v (get !d)

let printer_make oc (v, d, _) =
  Printf.fprintf oc "%s = %s \n" v (get !d)


let make_outfile n = 
  if n = "" 
  then stdout
  else open_out n

let emit_ml ()=
  let oc = make_outfile (output_d())
  in 
  List.iter (printer_ml oc) varlist;
  close_out oc

let emit_make() = 
  let oc = make_outfile (output_make_d())
  in 
  List.iter (printer_make oc) varlist;
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

  
