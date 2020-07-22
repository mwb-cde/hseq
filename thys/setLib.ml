(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

open HSeq
open HSeqUser
open Userlib

module SetPP =
  struct

    let set_thy = "Set"
    let set_id = Ident.mk_long set_thy "SET"
    let set_data = (Printkit.default_term_fixity,
                    Printkit.default_term_prec)
    let set_ty ()=
      Gtype.mk_def (Ident.mk_long set_thy "set") [Gtype.mk_null()]

    let single_id = Ident.mk_long set_thy "single"
    let single_data = (Printkit.default_term_fixity,
                       Printkit.default_term_prec)

    let empty_id = Ident.mk_long set_thy "empty"
    let empty_data = (Printkit.default_term_fixity,
                      Printkit.default_term_prec)

    let add_id = Ident.mk_long set_thy "add"

    let (ocb_sym, ccb_sym) = ("{", "}")
    let semicolon_sym = ";"

    (* Parser *)
    open Grammars
    open Pkit
    open Utility
    open Lexer

    let empty_term ()= Pterm.mk_typed_ident empty_id (set_ty())

    let ocb_tok = Sym(OTHER ocb_sym)
    and ccb_tok = Sym(OTHER ccb_sym)

    let wrapper t =
      let id_term = Pterm.mk_ident set_id in
      Pterm.mk_app id_term t

    let colon = Sym(COLON)

    let set_body (inf: Grammars.parser_info) inp =
      (((((id_type_opt (short_id id) inf)
          -- (?$ colon))
         >>
           (fun (v, _) ->
             Grammars.qnt_setup_bound_names inf Term.Lambda [v]))
        -- (form inf))
       >>
         (fun (xs, body) ->
           make_term_remove_names inf wrapper xs body)) inp

    let set_list inf inp =
      ((Grammars.comma_list (form inf))
       >>
         (fun ts ->
           List.fold_left
             (fun st elt -> Pterm.mk_fun add_id [elt; st])
             (empty_term()) ts)) inp

    let main_parser (inf: Grammars.parser_info) inp =
      (seq
         [?$ ocb_tok;
          (optional
             (alt
                [
                  set_body inf;
                  set_list inf
          ]))
          >>
            (fun s -> Lib.from_option s (empty_term()));
          ?$ ccb_tok]
       >>
         (fun l ->
           match l with
             [_; s; _] -> s
           | _ -> raise (ParsingError "Not a set"))) inp

    let set_parser inf inp =
      main_parser inf inp

    let init_set_parser () =
      let ptable0 = Context.parsers (Userlib.Global.context()) in
      let ptable1 =
        Parser.add_symbol ptable0 ocb_sym (Lexer.Sym(Lexer.OTHER ocb_sym))
      in
      let ptable2 =
        Parser.add_symbol ptable1 ccb_sym (Lexer.Sym(Lexer.OTHER ccb_sym))
      in
      let ptable3 =
        Parser.add_term_parser ptable2 Lib.Last set_thy set_parser
      in
      Global.set_context (Context.set_parsers (Global.context()) ptable3)

    (* Printer *)
    let set_printer () =
      let lambda_arg x =
        match x with
          Term.Qnt(q, body) -> (Term.Binder.kind_of q) = Term.Lambda
        | _ -> false
      in
      let printer ppstate (fixity, prec) (f, args) =
        (match args with
           (a::rest) ->
           let set_fix, set_prec = set_data
           in
           let (qnts, body) =
             Term.strip_fun_qnt
               set_id (Term.mk_app (Term.mk_ident set_id) a) []
           in
           Format.printf "@[<2>";
           (if(lambda_arg a)
            then
              (Format.printf "%s" ocb_sym;
               Printers.Terms.print_qnt_body
                 ppstate (set_fix, set_prec) (qnts, body);
               Format.printf "%s" ccb_sym)
            else
              Printers.Terms.simple_print_fn_app
                ppstate (set_fix, set_prec) (f, args));
           Printkit.print_list
             (Printers.Terms.print_term ppstate (fixity, prec),
              Printkit.print_space)
             rest;
           Format.printf "@]"
         | _ ->
            Format.printf "@[";
            Printers.Terms.print_ident_as_identifier
              ppstate (fixity, prec) f;
            Format.printf "@]")
      in
      printer

    let empty_set_printer ppstate prec (f, args) =
      Format.printf "@[<2>";
      Format.printf "%s%s" ocb_sym ccb_sym;
      Printkit.print_list
        (Printers.Terms.print_term ppstate prec, Printkit.print_space) args;
      Format.printf "@]"

    let single_set_printer ppstate prec (f, args) =
      match args with
        (a::rest) ->
        Format.printf "@[<2>";
        Format.printf "%s" ocb_sym;
        Printers.Terms.print_term ppstate prec a;
        Format.printf "%s" ccb_sym;
        Printkit.print_list
          (Printers.Terms.print_term ppstate prec,
           Printkit.print_space)
          rest;
        Format.printf "@]"
      | _ ->
         Format.printf "@[<2>";
         Printers.Terms.print_ident_as_identifier ppstate prec f;
         Format.printf "@]"

    let init_set_printer()=
      let set_print = set_printer()
      in
      let inf0 = Userlib.Global.ppinfo() in
      let inf1 = Printers.add_term_printer inf0 set_id set_print in
      let inf2 = Printers.add_term_printer inf1 single_id single_set_printer
      in
      Global.set_ppinfo inf2

    let init () =
      init_set_printer();
      init_set_parser()
  end

let _ = SetPP.init();
