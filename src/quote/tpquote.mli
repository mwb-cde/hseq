(*----
 Name: tpquote.mli
 Copyright M Wahab 2005-2010
 Author: M Wahab  <mwb.cde@googlemail.com>

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

(* val test_astexpander_fn : (string -> MLast.expr) ref *)
val test_astexpander_fn : (string -> Camlp4.PreCast.Ast.expr) ref

