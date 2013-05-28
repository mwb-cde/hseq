(*----
 Name: tpquote.ml
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

(**
   TPQuotation

   Using the OCaml/Camlp4 quotation system to read terms and types.
   
   syntax: << [str] >> 
   e.g. <<1>>, <<true>>, <<!a b: a and b>>, <<:'a>>, <<:bool -> 'a>>

   If the first character in the string is a colon (':')
   then [str] parsed as a type 
   otherwise it is parsed as a term.
*)


(**
   [expander str]
   If [str] begins with type_char then parse as type
   If [str] begins with type_char immediately followed by def_char 
   then parse as a type definition. 
   otherwise parse as term.

   i.e. with type_char = ':' and def_char = '!'
   << : ... >> is a type
   << :! ... >> is a typedef
   << ... >> is a term
   << !: ... >> is a term def
*)


open Camlp4.PreCast

let type_char = ref ':'
let def_char = ref '!'

type inputtype= Type | Typedef | Def | Term | Unknown

let is_space ch =
  match ch with
    ' '
  | '\t' 
  | '\n'
  | '\r' -> true
  | _ -> false

let std_check_string str = 
  let size=String.length str
  in 
  let rec check i =
    if i<size 
    then
      let ch=String.get str i
      in 
      if is_space ch
      then check (i+1)
      else 
	if ch= (!type_char)
	then (Type, String.sub str (i+1) (size - (i+1)))
	else (Term, String.sub str i (size - i))
    else (Unknown, str)
  in 
  if size>0 
  then check 0
  else (Unknown, str)

let def_check_string str = 
  let size=String.length str
  in 
  let rec check i =
    if i<size 
    then
     let ch=String.get str i
      in 
      if is_space ch
      then check (i+1)
      else 
	if ch= (!type_char)
	then 
	  (Typedef, String.sub str (i+1) (size - (i+1)))
	else 
	  (Def, String.sub str i (size - i))
    else (Unknown, str)
  in 
  if size>0 
  then check 0
  else (Unknown, str)


(* Syntax tree expander *)

let std_astexpander instr =
  let str = String.escaped instr
  in 
  match std_check_string str with 
    (Type, nstr) -> <:expr@here< BoolPP.read_type $str:nstr$ >>
  | (Term, nstr) -> <:expr@here<(BoolPP.read $str:nstr$)>>
  | _ -> <:expr@here<$str:str$>>

let def_astexpander str =
  let pos=  
    { 
      Lexing.pos_fname="Input"; Lexing.pos_lnum=0;
      Lexing.pos_bol = 0;  Lexing.pos_cnum = 0 
    }
  in
  let _loc= (pos, pos)
  in 
  match def_check_string str with 
    (Typedef, nstr) -> <:expr@here<BoolPP.read_type_defn $str:nstr$>>
  | (Def, nstr) -> <:expr@here<BoolPP.read_defn $str:nstr$>>
  | _ -> <:expr@here<$str:str$>>


(* OCaml-3.08 version
let pattexpander str =
  let loc=(0, 0)
  in 
  match check_string str with 
    (Type, nstr) -> 
      <:patt<BoolPP.read_type $str:nstr$>>
  | (Term, nstr) -> <:patt<BoolPP.Parsing.read_unchecked $str:nstr$>>
  | (_, nstr) -> <:patt< $str:nstr$ >>
*)

let pattexpander str = failwith "Pattern expander not implemented"

(* 
   Tester
*)
let test_astexpander_fn = ref (fun _ -> failwith "test_astexpander")
let test_astexpander x = (!test_astexpander_fn) x

let expand_quot exp loc _loc_name_opt contents = exp contents

let init() = 
  Syntax.Quotation.add "def" Quotation.DynAst.expr_tag 
    (expand_quot def_astexpander);
  Syntax.Quotation.add "tp" Quotation.DynAst.expr_tag 
    (expand_quot std_astexpander);
  Syntax.Quotation.add "test" Quotation.DynAst.expr_tag 
    (expand_quot test_astexpander);
  Syntax.Quotation.default:="tp"


let _ = init()
