(*-----
 Name: tpquote.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(*
   TPQuotation

   Using the OCaml/Camlp4 quotation system to read terms and types.
   
   syntax: << [str] >> 
   e.g. <<1>>, <<true>>, <<!a b: a and b>>, <<:'a>>, <<:bool -> 'a>>

   If the first character in the string is a colon (':')
   then [str] parsed as a type 
   otherwise it is parsed as a term.

   The quotation parser is set up with name "tp" and is set as
   the default quotation parser.

   Antiquotation is not supported.
*)

(* 
   [type_char]
   The character distinguishing types from terms
*)
val type_char: char ref

(* 
   [init()]
   Set up quotation parsing.
*)
