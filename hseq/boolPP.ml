(*----
  Copyright (c) 2006-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(*** Printer-Parser for Boolean functions. ***)

type symbol = Ident.t * int * Printkit.fixity * (string option)

(** Printer for negation. Prints [ << base.not x >> ] as [~x] rather
    than [~ x].
*)
let negation_pprec = Printkit.mk_record 205 Printkit.prefix None

let negation_printer ppstate ((fixity: Printkit.fixity), prec) (f, args) =
  let cprec= negation_pprec.Printkit.prec
  and fixity = negation_pprec.Printkit.fixity
  in
  match args with
    | t::rest ->
      Format.printf "@[<2>";
      Printkit.print_bracket prec cprec "(";
      Format.printf "~";
      Printers.Terms.print_term ppstate (fixity, cprec) t;
      Printkit.print_bracket prec cprec ")";
      Format.printf "@]";
      begin
        match rest with
          | [] -> ()
          | _ ->
            Format.printf "@[";
            Printkit.print_list
              ((fun x ->
                Printers.Terms.print_term ppstate (fixity, prec) x),
               (fun () -> Format.printf "@ "))
              rest;
            Format.printf "@]"
        end
    | _ ->
       Printers.Terms.simple_print_fn_app
         ppstate (fixity, cprec) (f, args)


let init_negation_printer inf =
  Printers.add_term_printer inf Lterm.notid negation_printer

(*** Support for if-then-else ***)

(** Parser-Printer for If-Then-else *)
open Grammars
open Pkit
open Utility
open Lexer

let ifthenelse_id = Ident.mk_long Lterm.base_thy "IF"

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
  let prec=Printkit.default_term_prec
  and fixity = Printkit.default_term_fixity
  in
  Printkit.mk_record prec fixity None

let init_ifthenelse_parser ppstate =
  let ite_syms =
    [
      ("if", (Sym(OTHER "IF")));
      ("then", (Sym(OTHER "THEN")));
      ("else", (Sym(OTHER "ELSE")));
    ]
  in
  let ppinf1 =
    List.fold_left
      (fun t (x, y) -> Parser.add_symbol t x y)
      ppstate
      ite_syms
  in
  Parser.add_term_parser ppinf1 Lib.First "IfThenElse" ifthenelse_parser

(** Printer for if-then-else **)
let ifthenelse_printer ppstate (fixity, prec) (f, args) =
  let cfixity = Printkit.default_term_fixity in
  let cprec = ifthenelse_pprec.Printkit.prec in
  match args with
    | b::tbr::fbr::rest ->
      Format.printf "@[<2>";
      Printkit.print_bracket prec cprec "(";
      Format.printf "if@ ";
      Printers.Terms.print_term ppstate (cfixity, cprec) b;
      Format.printf "@ then@ ";
      Printers.Terms.print_term ppstate (cfixity, cprec) tbr;
      Format.printf "@ else@ ";
      Printers.Terms.print_term ppstate (cfixity, cprec) fbr;
      Printkit.print_bracket prec cprec  ")";
      if (prec<cprec) then Format.printf "@ " else ();
      Format.printf "@]";
      begin
        match rest with
          | [] -> ()
          | _ ->
            Format.printf "@[";
            Printkit.print_list
              ((fun x ->
                Printers.Terms.print_term ppstate (cfixity, prec) x),
               (fun () -> Format.printf "@ "))
              rest;
            Format.printf "@]"
      end
    | _ ->
       Printers.Terms.simple_print_fn_app
         ppstate (cfixity, cprec) (f, args)


let init_ifthenelse_printer inf =
  Printers.add_term_printer inf ifthenelse_id ifthenelse_printer

(* Support for printing/parsing [epsilon(%x: P)] as [@x: P] *)

let choice_ident = Ident.mk_long Lterm.base_thy "epsilon"
let choice_sym = "@"
let choice_pp = (Printkit.default_term_fixity, Printkit.default_term_prec)

let choice_parser = Grammars.parse_as_binder choice_ident choice_sym

let init_choice_parser tbl =
  let tbl0 =
    Parser.add_symbol tbl choice_sym (Lexer.Sym(Lexer.OTHER choice_sym))
  in
  Parser.add_term_parser tbl0
    (Lib.After "lambda") "epsilon" choice_parser

let choice_printer =
  Printers.Terms.print_as_binder choice_pp choice_ident choice_sym

let init_choice_printer inf =
  Printers.add_term_printer inf choice_ident choice_printer

(* Support for printing/parsing [EXISTS_UNIQUE(%x: P)] as [?! x: P] *)

let exists_unique_ident =
  Ident.mk_long Lterm.base_thy "EXISTS_UNIQUE"
let exists_unique_sym = "?!"
let exists_unique_pp =
  (Printkit.default_term_fixity, Printkit.default_term_prec)

let exists_unique_parser =
  Grammars.parse_as_binder exists_unique_ident exists_unique_sym

let init_exists_unique_parser tbl =
  let tbl0 =
    Parser.add_symbol tbl
      exists_unique_sym
      (Lexer.Sym(Lexer.OTHER exists_unique_sym))
  in
  Parser.add_term_parser tbl0
    (Lib.After "lambda") "exists_unique" exists_unique_parser

let exists_unique_printer  =
  (Printers.Terms.print_as_binder
    exists_unique_pp exists_unique_ident exists_unique_sym)

let init_exists_unique_printer inf =
  Printers.add_term_printer inf
    exists_unique_ident
    exists_unique_printer

let bool_parsers =
  [ ifthenelse_parser; choice_parser; exists_unique_parser ]

let bool_parsers_init =
  [ init_ifthenelse_parser; init_choice_parser; init_exists_unique_parser ]

let init_bool_parsers ptbl0 =
  let ptbl1 =
    List.fold_left (fun x f -> f x) ptbl0 bool_parsers_init
  in
  ptbl1

let bool_printers_init =
  [
    init_ifthenelse_printer;
    init_choice_printer;
    init_exists_unique_printer;
  ]

let init_bool_printers ppinfo =
  let ppinf1 =
    List.fold_left (fun x f -> f x) ppinfo bool_printers_init
  in
  ppinf1

let add_type_token ptable id repr fixity prec =
  Parser.add_type_token ptable
    id (Lib.from_option repr (Ident.name_of id)) fixity prec

let add_token ptable id repr fixity prec =
  Parser.add_token ptable
    id (Lib.from_option repr (Ident.name_of id)) fixity prec

let basethy_type_symbols =
  [
  ]
and basethy_term_symbols =
  [
  ]

let quote_type_symbols =
  [
    (Lterm.fun_ty_id, 100, Printkit.infixr, Some("->"));
  ]
and quote_term_symbols =
  [
    (Lterm.notid, negation_pprec.Printkit.prec,
     negation_pprec.Printkit.fixity,
     Some "not");
    (Lterm.notid, negation_pprec.Printkit.prec,
     negation_pprec.Printkit.fixity,
     Some "~");
    (Lterm.equalsid, 200, Printkit.infixl, (Some "=")) ;
    (Lterm.andid, 185, Printkit.infixr, Some "and") ;
    (Lterm.andid, 185, Printkit.infixr, Some "&") ;
    (Lterm.orid, 190, Printkit.infixr, Some "or") ;
    (Lterm.orid, 190, Printkit.infixr, Some "|") ;
    (Lterm.impliesid, 195, Printkit.infixr, Some "=>") ;
    (Lterm.iffid, 180, Printkit.infixn, Some "iff") ;
  ]

let init_bool_tokens ptable (tysyms, trmsyms) =
  let ptable1 =
    List.fold_left
      (fun infa (id, p, f, r) -> add_type_token infa id r f p)
      ptable tysyms
  in
  let ptable2 =
    List.fold_left
      (fun infa (id, p, f, r) ->  add_token infa id r f p)
      ptable1 trmsyms
  in
  ptable2

let init_bool_ppinfo ppinfo (tysyms, trmsyms) =
  let ppinfo1 =
    List.fold_left
      (fun infa (id, p, f, r) ->
        Printers.add_type_info infa id p f r)
      ppinfo tysyms
  in
  let ppinfo2 =
    List.fold_left
      (fun infa (id, p, f, r) ->
        Printers.add_term_info infa id p f r)
      ppinfo1 trmsyms
  in
  ppinfo2

(** {7 OCaml Quotations support} *)

let basethy_context () =
  let syms = (basethy_type_symbols, basethy_term_symbols) in
  let ctxt0 = Context.empty() in
  let ptbl1 = Parser.init_parsers (Context.parsers ctxt0) in
  let ptbl2 = init_bool_parsers ptbl1 in
  let ptbl3 = init_bool_tokens ptbl2 syms in
  let ctxt1 = Context.set_parsers ctxt0 ptbl3 in
  ctxt1

let quote_context =
  let syms = (quote_type_symbols, quote_term_symbols) in
  let ctxt0 = Context.empty() in
  let ptbl1 = Parser.init_parsers (Context.parsers ctxt0) in
  let ptbl2 = init_bool_parsers ptbl1 in
  let ptbl3 = init_bool_tokens ptbl2 syms in
  let ctxt1 = Context.set_parsers ctxt0 ptbl3 in
  let ppinf0 = Context.ppinfo ctxt1 in
  let ppinf1 = init_bool_ppinfo ppinf0 syms in
  Context.set_ppinfo ctxt1 ppinf1

let ppinfo () = Context.ppinfo quote_context

(** Parse a string as a term, resolving short names and symbols. *)
let read str = Context.PP.read quote_context str

(** Parse a string as a term, resolving short names and symbols. *)
let read_unchecked str = Context.PP.read_unchecked quote_context str

(** Parse a string as a term definition. *)
let read_defn str = Context.PP.read_defn quote_context str

(** Parse a string a type, resolving short names and symbols where
    possible.  *)
let read_type str = Context.PP.read_type quote_context str

(** Parse a string as a type definition. *)
let read_type_defn str = Context.PP.read_type_defn quote_context str
let read_identifier str = Context.PP.read_identifier quote_context str
