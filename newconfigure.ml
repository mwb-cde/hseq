#!/usr/bin/ocamlrun /usr/bin/ocaml
(*----
 Name: configure.ml
 Copyright M Wahab 2005-2014
 Author: M Wahab  <mwb.cde@gmail.com>

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
   --docdir: the documenation directory.
   --datadir: the data directory (not used).
*)

#warnings "-13";;

(** File name utilities **)

let filename x y = 
  if x != ""
  then Filename.concat x y
  else y

let filename_opt x y = 
  match x with
    None -> y
  | (Some d) -> Filename.concat d y


(** String utilities **)

(** 
    [stringify str]: Make [str] suitable for passing to OCaml.
    Escapes the string using String.escaped.
    then replaces ' ' with '\ '.
*)

let stringify str = String.escaped str

let set p x = p := Some x

let get_str p =
  match p with
    None -> ""
  | Some x -> x

let get_opt p d=
  match p with
    None -> d
  | Some x -> x

(** Test that a command exists *)
let has_program s = 
  Sys.command s = 0

let test_program s args = 
  let cmd = List.fold_left (fun a b -> (a^" "^b)) s args
  in
  Sys.command cmd = 0

(** Test that a file exists *)
let has_file s = 
  Sys.file_exists s

(** The current directory *)
let cwd = Sys.getcwd()

(** Names of output files **)
let output_dir = cwd
let ml_data = filename output_dir "configure.data"
let make_data = filename output_dir "data.make"

(** Settings *)
class setting = 
object (self)
  (* Variable description *)
  val description = "<unknown>"
  method get_description () = description
  (* Variable name *)
  val variable = "<undefined>"
  method get_variable () = variable

  (* Variable value *)
  val mutable value = None
  method get_value () = value

  (* Whether the value has been set by a command line option *)
  val mutable specified_value = false
  method is_specified () = specified_value
  method set_option_value v = 
    begin
      value <- v;
      specified_value <- true
    end

  (* Whether variable is optional or required *)
  val required = false;
  method is_required() = required

  (* Command line option *)
  val option = None
  method has_option () = option != None

  (* Find: Try to deduce the value, return true iff successful. *)
  method find() = false

  (* Existence test *)
  method test () = true
    
  (* Report value *)
  method report oc =
    begin
      match self#get_value() with
      | Some(x) ->
          (Printf.fprintf oc "%s: %s\n" variable x)
      | _ -> 
        if self#is_required()
        then
          begin
            Printf.fprintf oc "%s: required but not set. " variable;
            if (option != None)
            then Printf.fprintf oc "Use option --%s.\n" (get_str option)
            else ()
          end
    end

  (* Print help *)
  val help_msg = None
  method make_help_msg () = 
    let msg = 
      if help_msg = None 
      then description 
      else (get_str help_msg)
    in
    begin
      String.concat " "
        ["<str>"; msg;
         (if (self#is_required()) then "(required)" else "");
         ("["^(get_str (self#get_value()))^"]")]
    end

  method help oc = 
    if option = None 
    then ()
    else
      begin
        Printf.fprintf oc "%s %s"
          (get_str option) (self#make_help_msg())
      end

  (* Get an Arg.spec object *)
  method get_arg_spec() = 
    let set_value str = ignore(self#set_option_value (Some(str))) in
    (get_str option, Arg.String set_value, self#make_help_msg())

  (* Outputs *)
  method print_var_ml oc =
    begin
      match self#get_value() with
        Some(x) -> 
 	  Printf.fprintf oc 
            "DEFINE %s=\"%s\"\n" variable (stringify x) 
      | _ -> ()
    end

  method print_var_make oc =
    begin
      match self#get_value() with
        Some(x) -> 
 	  Printf.fprintf oc "%s=%s\n" 
            variable (String.escaped x) 
      | _ -> ()
    end
end

class relative_directory base = 
object 
  inherit setting
  val base_dir = base
  val description = "Directory relative to installation directory"
  val mutable value = None
  method get_value () = 
    let base_path = get_str (base_dir#get_value()) in
    let rel_path = get_str value in
    Some(Filename.concat base_path rel_path)
end

class base_directory =
object 
  inherit setting
  val description = "root of installation directory"
  val variable = "BaseDir"
  val mutable value = Some("/usr/local")
  val option = Some "--prefix"
  val required = true
end

class bin_directory base =
object 
  inherit relative_directory base
  val description = "binaries directory"
  val variable = "BinDir"
  val mutable value = Some("bin")
  val option = Some "--bindir"
  val required = true
end

class src_directory =
object 
  inherit setting
  val description = "Source directory"
  val variable = "SrcDir"
  val mutable value = Some(Sys.getcwd())
  val required = true
end

class lib_directory base =
object 
  inherit relative_directory base
  val description = "libraries directory"
  val variable = "LibDir"
  val mutable value = Some("lib")
  val option = Some "--libdir"
  val required = true
end

class thy_directory lib =
object 
  inherit relative_directory lib
  val description = "theories directory"
  val variable = "ThyDir"
  val mutable value = Some("thy")
  val option = None
end

class doc_directory base =
object 
  inherit relative_directory base
  val description = "libraries directory"
  val variable = "DocDir"
  val mutable value = Some("share/doc")
  val option = Some "--docdir"
  val required = true
end

(** An ocaml tool *)
class octool = 
object (self)
  inherit setting
  val description = "An OCaml Tool"
  val variable = "OCAML-TOOL"
  val mutable value = None
  val option = Some "--ocaml-tool"
  val tool_name = "ocaml-tool"

  (* Directories to search for the tool *)
  val search_path = ["/usr/bin"; "/usr/local/bin"]
  method find () =
    if self#is_specified () 
    then false
    else
      begin
        let bin_dir = Filename.dirname (Sys.executable_name) in
        let tool_exists dir = has_file (Filename.concat dir tool_name)
        in
        try 
          let dir = List.find tool_exists (bin_dir::search_path) in
          (value <- Some(Filename.concat dir tool_name);
           true)
        with Not_found -> false
      end 

  method test () = 
    let ocamltool = get_str value in
    test_program ocamltool ["-v"]
end

class tool_ocamlc =
object (self)
  inherit octool
  val description = "OCaml byte-code compiler"
  val variable = "OCAMLC"
  val mutable value = Some("/usr/bin/ocamlc")
  val option = Some "--ocamlc"
  val tool_name = "ocamlc"
  val required = true
end

class tool_ocamlopt =
object (self)
  inherit octool
  val description = "OCaml native-code compiler"
  val variable = "OCAMLOPT"
  val mutable value = Some("/usr/bin/ocamlopt")
  val option = Some "--ocamlopt"
  val tool_name = "ocamlopt"
  val required = false
end

class tool_ocamlmktop =
object (self)
  inherit octool
  val description = "OCaml toplevel builder"
  val variable = "OCAMLMKTOP"
  val mutable value = Some("/usr/bin/ocamlmktop")
  val option = Some "--ocamlmktop"
  val tool_name = "ocamlmktop"
  val required = true
end

class tool_camlp4 =
object (self)
  inherit octool
  val description = "Camlp4"
  val variable = "CAMLP4"
  val mutable value = Some("/usr/bin/camlp4")
  val option = Some "--camlp4"
  val tool_name = "camlp4"
  val required = true
end

(** Settings *)

(* Directories *)
let base_dir = new base_directory
let src_dir = new src_directory
let bin_dir = new bin_directory base_dir
let lib_dir = new lib_directory base_dir
let thy_dir = new thy_directory lib_dir
let doc_dir = new doc_directory base_dir

(* Tools *)
let ocamlc_prog = new tool_ocamlc
let camlp4_prog = new tool_camlp4
let ocamlmktop_prog = new tool_ocamlmktop
let ocamlopt_prog = new tool_ocamlopt

let settings = 
[
  base_dir;
  src_dir;
  bin_dir;
  lib_dir;
  thy_dir;
  doc_dir;

  ocamlc_prog;
  camlp4_prog;
  ocamlmktop_prog;
  ocamlopt_prog;
]

let find_tools () = 
  let find_obj obj = ignore(obj#find()) in
  List.iter find_obj settings

let help () = 
  let print_obj obj = 
    obj#help stdout;  
    Printf.fprintf stdout "\n"
  in
  List.iter print_obj settings

let report () = 
  let print_obj obj = obj#report stdout in
  List.iter print_obj settings

let check () =
  let test_obj obj = not((obj#is_required()) && ((obj#get_value()) = None))
  in
  not (List.exists test_obj settings)

(** Emiiters *)
let make_outfile n = 
  if n = "" 
  then stdout
  else open_out n

let emit_ml ()=
  let oc = make_outfile ml_data in
  let print_obj oc obj = obj#print_var_ml oc 
  in 
    Printf.fprintf oc 
      "(* Definitions for OCaml pre-processor (auto-generated) *)\n\n";
    List.iter (print_obj oc) settings;
    close_out oc;
    Printf.printf "Wrote file %s\n" ml_data

let emit_make() = 
  let oc = make_outfile make_data in
  let print_obj oc obj = obj#print_var_make oc 
  in 
    Printf.fprintf oc 
      "# Definitions for makefiles (auto-generated) \n\n";
    List.iter (print_obj oc) settings;
    close_out oc;
    Printf.printf "Wrote file %s\n" make_data

let emit () = 
  emit_ml();
  emit_make() 

(** Command line arguments **)

let get_options_list () =
  let get_option lst obj = 
    if obj#has_option ()
    then (obj#get_arg_spec())::lst
    else lst
  in 
  Arg.align
    (List.rev(List.fold_left get_option [] settings))

let usage_msg = ""
let anon_fun _ = raise (Arg.Bad "unknown option")
let parse_args () = Arg.parse (get_options_list()) anon_fun usage_msg

(** Main **)
let _ = 
  find_tools();
  parse_args();
  report();
  emit()

  
