
(*
#load "camlp4o.cma";;
#load "pa_extend.cmo";;
*)

#use "../fm.ml";;

#use "counter.ml";;
#use "oclex.ml";;
#use "ocparse.ml";;


let test_lex str = Oclexer.lex Oclexer.symbols (Stream.of_string str);;

let test str = Ocparse.term_parser str;;
(*
  Grammar.Entry.parse Ocparse.term (Stream.of_string str) env
*)
