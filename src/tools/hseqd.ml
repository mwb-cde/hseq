(*-----
 Name: hseqd.mlp
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(**
   A script to run hseq as a batch compiler.

   Use as [hseqd ocaml-args file1 file2 ... filen]
   where 
   [ocaml-args] are the arguments to pass to ocaml 
   [file1], [file2], ..., [filen] are the files to compile.

   Each of the files is run through hseq, starting with [file1] and
   continuing to [filen].
*)

(* *)

let bindir = "/home/mw/src/tp/src" ^ "/bin"
let includedir = "/home/mw/src/tp/src" ^ "/include"

(** Start of script *)

let bin = Filename.concat bindir "hseq"
let bin_args = Array.of_list [""; "-I"; includedir]

let files = Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))

let _ =
  if Sys.file_exists bin then ()
  else
    begin
      output_string stdout "hseqd: can't find executable ";
      output_string stdout bin;
      print_newline ();
      flush stdout;
      exit (-1)
    end

let (outpipe, inpipe) = Unix.pipe ()
let outstrm = Unix.out_channel_of_descr inpipe
let instrm = Unix.in_channel_of_descr outpipe

let rec run l =
  match l with
    [] -> ()
  | x :: xs ->
      let pid =
        Unix.create_process bin bin_args outpipe Unix.stdout Unix.stderr
      in
      output_string stdout ("File " ^ x);
      print_newline ();
      output_string outstrm ("#use \"" ^ x ^ "\";;\n#quit;;\n");
      flush outstrm;
      ignore (Unix.waitpid [Unix.WUNTRACED] pid);
      run xs

let _ = run files

let _ = Unix.close outpipe; Unix.close inpipe

let _ = output_string stdout "\n"; flush stdout
let _ = exit 0


