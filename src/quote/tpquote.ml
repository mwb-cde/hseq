(*-----
 Name: tpquote.ml
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
*)


(*
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
	then 
	  (Type, String.sub str (i+1) (size - (i+1)))
	else 
	  (Term, String.sub str i (size - i))
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


(* OCaml-3.07 version *)
let std_astexpander str =
  let pos=  { Lexing.pos_fname="Input"; Lexing.pos_lnum=0;
	      Lexing.pos_bol = 0;  Lexing.pos_cnum = 0 }
  in
  let loc= (pos, pos)
  in 
  match std_check_string str with 
    (Type, nstr) -> 
      <:expr<Global.read_type $str:nstr$>>
  | (Term, nstr) -> <:expr<(Global.read_unchecked $str:nstr$)>>
  | _ -> <:expr< $str:str$ >>

let def_astexpander str =
  let pos=  { Lexing.pos_fname="Input"; Lexing.pos_lnum=0;
	      Lexing.pos_bol = 0;  Lexing.pos_cnum = 0 }
  in
  let loc= (pos, pos)
  in 
  match def_check_string str with 
    (Typedef, nstr) -> 
      <:expr<Global.read_type_defn $str:nstr$>>
  | (Def, nstr) -> 
      <:expr<Global.read_defn $str:nstr$>>
  | _ -> <:expr< $str:str$ >>

(* OCaml-3.08 version *)
(*
let astexpander str =
  let loc=(Lexing.dummy_pos,Lexing.dummy_pos)
  in 
  match check_string str with 
    (Type, nstr) -> 
      <:expr<Global.read_type $str:nstr$>>
  | (Term, nstr) -> <:expr<(Global.read_unchecked $str:nstr$)>>
  | (Typedef, nstr) -> 
      <:expr<Global.read_type_defn $str:nstr$>>
  | _ -> <:expr< $str:str$ >>
*)

(*
let pattexpander str =
  let loc=(0, 0)
  in 
  match check_string str with 
    (Type, nstr) -> 
      <:patt<Global.read_type $str:nstr$>>
  | (Term, nstr) -> <:patt<Global.read_unchecked $str:nstr$>>
  | (_, nstr) -> <:patt< $str:nstr$ >>
*)

let pattexpander str = failwith "Pattern expander not implemented"

(* 
   Tester
*)
let test_astexpander_fn = ref (fun _ -> failwith "test_astexpander")
let test_astexpander x = (!test_astexpander_fn) x

let init() = 
  Quotation.add "def" (Quotation.ExAst (def_astexpander, pattexpander));
  Quotation.add "tp" (Quotation.ExAst (std_astexpander, pattexpander));
  Quotation.add "test" (Quotation.ExAst (test_astexpander, pattexpander));
  Quotation.default:="tp"

let _ = init()
