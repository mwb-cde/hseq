(*----
  Name: boolPP.ml
  Copyright M Wahab 2006-2010
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

(*** Printer-Parser for Boolean functions. ***)

(** Printer for negation. Prints [ << base.not x >> ] as [~x] rather
    than [~ x].
*)
let negation_pprec = Printer.mk_record 205 Printer.prefix None

let negation_printer ppstate (fixity, prec) (f, args) =
  let cprec= negation_pprec.Printer.prec
  and fixity = negation_pprec.Printer.fixity
  in 
  match args with 
    | t::rest -> 
      Format.printf "@[<2>";
      Printer.print_bracket prec cprec "(";
      Format.printf "~";
      Term.print_term ppstate (fixity, cprec) t;
      Printer.print_bracket prec cprec ")";
      Format.printf "@]";
      begin
	match rest with
	  | [] -> ()
	  | _ -> 
	    Format.printf "@[";
	    Printer.print_list
	      ((fun x ->
		Term.print_term ppstate (fixity, prec) x),
	       (fun () -> Format.printf "@ "))
	      rest;
	    Format.printf "@]"
        end
    | _ -> Term.simple_print_fn_app ppstate (fixity, cprec) (f, args)

let init_negation_printer() =
  Global.PP.add_term_printer Lterm.notid negation_printer

(*** Support for if-then-else ***)

(** Parser-Printer for If-Then-else *)
open Grammars
open Pkit
open Utility
open Lexer

let ifthenelse_id = Hident.mk_long Lterm.base_thy "IF"

let ifthenelse_parser inf =
  ((seq
      [?$(Lexer.Sym (Lexer.OTHER "IF"));
       Grammars.form inf;
       ?$(Lexer.Sym(Lexer.OTHER "THEN"));
       Grammars.form inf;
       ?$(Lexer.Sym(Lexer.OTHER "ELSE"));
       Grammars.form inf])
   >>
     (fun l -> 
       match l with 
	   [_; test; _; tbr; _; fbr] ->
	     Pterm.mk_fun ifthenelse_id [test; tbr; fbr]
	 | _ -> raise (ParsingError "Error parsing if-then-else")))
    
let ifthenelse_pprec = 
  let prec=Printer.default_term_prec
  and fixity = Printer.default_term_fixity
  in 
  Printer.mk_record prec fixity None

let init_ifthenelse_parser() = 
  Parser.add_symbol "if" (Sym(OTHER "IF"));
  Parser.add_symbol "then" (Sym(OTHER "THEN"));
  Parser.add_symbol "else" (Sym(OTHER "ELSE"));
  Parser.add_term_parser Lib.First "IfThenElse" ifthenelse_parser

(** Printer for if-then-else **)
let ifthenelse_printer ppstate (fixity, prec) (f, args) =
  let cfixity = Printer.default_term_fixity in 
  let cprec = ifthenelse_pprec.Printer.prec in 
  match args with 
    | b::tbr::fbr::rest -> 
      Format.printf "@[<2>";
      Printer.print_bracket prec cprec "(";
      Format.printf "if@ ";
      Term.print_term ppstate (cfixity, cprec) b;
      Format.printf "@ then@ ";
      Term.print_term ppstate (cfixity, cprec) tbr;
      Format.printf "@ else@ ";
      Term.print_term ppstate (cfixity, cprec) fbr;
      Printer.print_bracket prec cprec  ")";
      if (prec<cprec) then Format.printf "@ " else ();
      Format.printf "@]";
      begin
        match rest with
	  | [] -> ()
	  | _ -> 
	    Format.printf "@[";
	    Printer.print_list
	      ((fun x ->
		Term.print_term ppstate (cfixity, prec) x),
	       (fun () -> Format.printf "@ "))
	      rest;
	    Format.printf "@]"
      end
    | _ -> Term.simple_print_fn_app ppstate (cfixity, cprec) (f, args)

let init_ifthenelse_printer() =
  Global.PP.add_term_printer ifthenelse_id ifthenelse_printer

let init_ifthenelse() =
  init_ifthenelse_parser();
  init_ifthenelse_printer()

(* Support for printing/parsing [epsilon(%x: P)] as [@x: P] *)

let choice_ident = Hident.mk_long Lterm.base_thy "epsilon"
let choice_sym = "@"
let choice_pp = (Printer.default_term_fixity, Printer.default_term_prec) 

let choice_parser = Grammars.parse_as_binder choice_ident choice_sym

let init_choice_parser () =
  Parser.add_symbol choice_sym (Lexer.Sym(Lexer.OTHER choice_sym));
  Parser.add_term_parser 
    (Lib.After "lambda") "epsilon" choice_parser
    
let choice_printer = 
  Term.print_as_binder choice_pp choice_ident choice_sym

let init_choice_printer () =
  let printer = choice_printer in 
  Global.PP.add_term_printer choice_ident 
    (fun ppstate ppenv -> printer ppstate ppenv)

let init_epsilon() = 
  init_choice_parser();
  init_choice_printer()

(* Support for printing/parsing [EXISTS_UNIQUE(%x: P)] as [?! x: P] *)

let exists_unique_ident = 
  Hident.mk_long Lterm.base_thy "EXISTS_UNIQUE"
let exists_unique_sym = "?!"
let exists_unique_pp = 
  (Printer.default_term_fixity, Printer.default_term_prec) 

let exists_unique_parser =
  Grammars.parse_as_binder exists_unique_ident exists_unique_sym

let init_exists_unique_parser() =
  Parser.add_symbol 
    exists_unique_sym 
    (Lexer.Sym(Lexer.OTHER exists_unique_sym));
  Parser.add_term_parser 
    (Lib.After "lambda") "exists_unique" exists_unique_parser
    
let exists_unique_printer = 
  Term.print_as_binder 
    exists_unique_pp exists_unique_ident exists_unique_sym

let init_exists_unique_printer() =
  let printer = exists_unique_printer in 
  Global.PP.add_term_printer exists_unique_ident 
    (fun ppstate ppenv -> printer ppstate ppenv)

let init_exists_unique() = 
  init_exists_unique_parser();
  init_exists_unique_printer()

(* PP Initialising functions *)

let init_parsers () = 
  init_ifthenelse_parser();
  init_choice_parser();
  init_exists_unique_parser()
    
let init_printers () =
  init_negation_printer();
  init_ifthenelse_printer();
  init_choice_printer() ;
  init_exists_unique_printer() 

let init () =
  init_printers();
  init_parsers()

