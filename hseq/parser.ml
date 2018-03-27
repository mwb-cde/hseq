(*----
  Name: parser.ml
  Copyright Matthew Wahab 2005-2016
  Author: Matthew Wahab <mwb.cde@gmail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under the
  terms of the Lesser GNU General Public License as published by the Free
  Software Foundation; either version 3, or (at your option) any later
  version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public License for
  more details.

  You should have received a copy of the Lesser GNU General Public License
  along with HSeq.  If not see <http://www.gnu.org/licenses/>.
  ----*)

(***
 * Parsers for terms and types.
 ***)

(***
 * Token information
 ***)

type associativity=Grammars.associativity
let non_assoc=Grammars.non_assoc
let left_assoc=Grammars.left_assoc
let right_assoc=Grammars.right_assoc

type fixity=Grammars.fixity
let nonfix=Grammars.nonfix
let infix=Grammars.infix
let prefix=Grammars.prefix
let suffix=Grammars.suffix

(*** Default token information ***)

let default_term_prec = Grammars.default_term_prec
let default_term_assoc = Grammars.default_term_assoc
let default_term_fixity= Grammars.default_term_fixity

let default_type_prec = Grammars.default_type_prec
let default_type_assoc = Grammars.default_type_assoc
let default_type_fixity= Grammars.default_type_fixity


(***
* Basic types used by the parsers.
***)

type input = Grammars.input
type ('a)phrase = 'a Grammars.phrase
type ('a)parse = 'a Grammars.parse
type typedef_data = Grammars.typedef_data
exception ParsingError = Grammars.ParsingError

(***
* Parser data
***)

open Lexer
open Lterm

(*** Symbols ***)

type symbol_table = Lexer.symtable

let core_symbols =
  [
    (".", Sym DOT);
    ("(", Sym ORB);
    (")", Sym CRB);
    (",", Sym comma_sym);
    ("'", Sym PRIME);
    (":", Sym COLON);
    ("true", BOOL true); ("false", BOOL false);
    ("!", Key ALL); ("all", Key ALL);
    ("forall", Key ALL);
    ("?", Key EX); ("exists", Key EX);
    ("%", Key LAM); "lambda", Key LAM
  ]

let default_symtable_size = 51
let mk_symtable sz = Lexer.mk_symtable sz
let clear_symtable tb = Lexer.clear_symtable tb

(*** Overloading *)

module OverloadTree = Treekit.StringTree

type overload_table_t = ((Ident.t * Gtype.t) list) OverloadTree.t
type sym_pos = Ident.t Lib.position

let default_overload_table_size = 127
let mk_overload_table sz = OverloadTree.empty

(** Parser tables *)
module Table =
struct
  type t =
      {
        tokens_f: Grammars.token_table;
        type_tokens_f: Grammars.token_table;
        symbols_f: symbol_table;
        overloads_f: overload_table_t;
        term_parsers_f:
          (string, Grammars.parser_info -> Pterm.t phrase) Lib.named_list;
        type_parsers_f:
          (string, Grammars.parser_info -> (Gtype.t phrase)) Lib.named_list;
      }

  let default_size = (Grammars.default_table_size,
                      Grammars.default_table_size,
                      default_symtable_size,
                      default_overload_table_size)

  let empty (tok_size, tytok_size, stm_size, ov_size) =
    {
      tokens_f = Grammars.token_table_new tok_size;
      type_tokens_f = Grammars.token_table_new tytok_size;
      symbols_f = mk_symtable stm_size;
      overloads_f = mk_overload_table ov_size;
      term_parsers_f = Grammars.core_term_parsers;
      type_parsers_f = Grammars.core_type_parsers;
    }

  let init_symbols symtbl syms =
    List.fold_left
      (fun tbl (s, t) -> Lexer.add_sym tbl s t)
      symtbl syms

  let init tbl =
    let toks = Grammars.token_table_reset tbl.tokens_f
    and tytoks = Grammars.token_table_reset tbl.type_tokens_f
    and symtab = init_symbols tbl.symbols_f core_symbols;
    and ovltab = mk_overload_table default_overload_table_size
    in
    {
      tokens_f = toks; type_tokens_f = tytoks;
      symbols_f = symtab; overloads_f = ovltab;
      term_parsers_f = Grammars.core_term_parsers;
      type_parsers_f = Grammars.core_type_parsers;
    }

  let tokens t = t.tokens_f
  let set_tokens t x = {t with tokens_f = x}
  let type_tokens t = t.type_tokens_f
  let set_type_tokens t x = {t with type_tokens_f = x}
  let symbols t = t.symbols_f
  let set_symbols t x = {t with symbols_f = x}
  let overloads t = t.overloads_f
  let set_overloads t x = {t with overloads_f = x}
  let term_parsers t = t.term_parsers_f
  let set_term_parsers t x = {t with term_parsers_f = x}
  let type_parsers t = t.type_parsers_f
  let set_type_parsers t x = {t with type_parsers_f = x}

end

let mk_info t =
  Grammars.mk_info
    (Table.tokens t, Table.type_tokens t,
     Table.term_parsers t, Table.type_parsers t)

let add_symbol tbl sym tok =
  let syms1 = Lexer.add_sym (Table.symbols tbl) sym tok in
  Table.set_symbols tbl syms1

let find_symbol tbl sym =
  Lexer.find_sym (Table.symbols tbl) sym

let remove_symbol tbl sym =
  Table.set_symbols tbl (Lexer.remove_sym (Table.symbols tbl) sym)

let init_symtable tbl sz =
  Table.set_symbols tbl (mk_symtable sz)

(*** Tokens ***)

let token_table = Grammars.token_table_new Grammars.default_table_size

let add_token_info tbl tok tok_info =
  let toks0 = Grammars.token_table_add (Table.tokens tbl) tok tok_info
  in
  Table.set_tokens tbl toks0

let get_token_info tbl tok =
  Grammars.token_table_find (Table.tokens tbl) tok

let remove_token_info tbl tok =
  Table.set_tokens tbl
    (Grammars.token_table_remove (Table.tokens tbl) tok)

(***
let type_token_table = Grammars.token_table_new Grammars.default_table_size
***)
let add_type_token_info tbl tok tok_info =
  let toks0 =
    Grammars.token_table_add (Table.type_tokens tbl) tok tok_info
  in
  Table.set_type_tokens tbl toks0

let get_type_token_info tbl tok =
  Grammars.token_table_find (Table.type_tokens tbl) tok

let remove_type_token_info tbl tok =
  let toks0 =
    Grammars.token_table_remove (Table.type_tokens tbl) tok
  in
  Table.set_type_tokens tbl toks0

(*** Toplevel symbol and token functions *)

let add_token tbl id sym fx pr=
  (* lexer information *)
  let tbl0 = add_symbol tbl sym (Sym (OTHER sym)) in
  (* parser information *)
  add_token_info tbl0 (Sym(OTHER sym)) (Some(id, fx, pr))

let remove_token tbl sym =
  let tbl0 = remove_symbol tbl sym in
  remove_token_info tbl0 (Sym(OTHER sym))

let add_type_token tbl id sym fx pr=
  (* lexer information *)
  let tbl0 = add_symbol tbl sym (Sym (OTHER sym)) in
  (* parser information *)
  add_type_token_info tbl0 (Sym(OTHER sym)) (Some(id, fx, pr))

let remove_type_token tbl sym =
  let tbl0 = remove_symbol tbl sym in
  remove_type_token_info tbl0 (Sym(OTHER sym))

(*** Overloading  *)
let overload_table tbl = Table.overloads

let init_overload tbl =
  Table.set_overloads tbl (mk_overload_table default_overload_table_size)

let get_overload_list tbl sym =
  OverloadTree.find (Table.overloads tbl) sym

let insert_pos pos d lst =
  let rec split_at s l r =
    match l with
      [] -> (r, [])
    | (x, ty)::ls ->
        if(x=s)
        then
          (List.rev r, l)
        else
          split_at s ls ((x, ty)::r)
  in
  match pos with
    Lib.First -> d::lst
  | Lib.Last -> List.rev (d::(List.rev lst))
  | Lib.Before s ->
      let (lt, rt) = split_at s lst []
      in
      List.rev_append (List.rev lt) (d::rt)
  | Lib.After s ->
      let (lt, rt)=split_at s lst []
      in
      let nrt=
        (match rt with
          [] ->  [d]
        | x::rst -> x::d::rst)
      in
      List.rev_append (List.rev lt) nrt
  | Lib.Level s ->
      let (lt, rt)=split_at s lst []
      in
      List.rev_append (List.rev lt) (d::rt)

let add_overload tbl sym pos (id, ty) =
  let list0 =
    try get_overload_list tbl sym
    with Not_found -> []
  in
  let list1 = insert_pos pos (id, ty) list0 in
  let ovltbl = Table.overloads tbl in
  let ovltbl1 = OverloadTree.replace ovltbl sym list1 in
  Table.set_overloads tbl ovltbl1

let remove_overload tbl sym id =
  let list0 = get_overload_list tbl sym in
  let list1 = List.remove_assoc id list0 in
  let ovltbl = Table.overloads tbl in
  let ovltbl1 =
    begin
      match list1 with
        [] -> OverloadTree.remove ovltbl sym
      | _ -> OverloadTree.replace ovltbl sym list1
    end
  in
  Table.set_overloads tbl ovltbl1

let print_overloads tbl info =
  let table = Table.overloads tbl in
  let print_fn sym list=
    Format.printf "@[<2>%s@ " sym;
    List.iter
      (fun (id, ty) ->
        Printer.print_ident id;
        Format.printf ":@ ";
        Printers.print_type info ty;
        Format.printf ";@ ")
      list;
    Format.printf "@]@,"
  in
  Format.printf "@[<v>";
  OverloadTree.iter print_fn table;
  Format.printf "@]"

(*** Initialising functions ***)

let init_symbols tbl =
  let symtab = Table.init_symbols (Table.symbols tbl) core_symbols
  in
  Table.set_symbols tbl symtab

let init_token_table tbl =
  Table.set_tokens tbl (Grammars.token_table_reset (Table.tokens tbl))

let init_type_token_table tbl =
  Table.set_type_tokens tbl
    (Grammars.token_table_reset (Table.type_tokens tbl))

let init_tables tbl =
  let tbl0 = init_symbols tbl in
  let tbl1 = init_token_table tbl0 in
  let tbl2 = init_type_token_table tbl1 in
  init_overload tbl2

let init_parsers tbl =
  let tbl0 = init_tables tbl in
  let tbl1 = Table.set_type_parsers tbl0 Grammars.core_type_parsers in
  let tbl2 = Table.set_term_parsers tbl1 Grammars.core_term_parsers in
  tbl2

let init () =
  init_parsers (Table.empty Table.default_size)


(**
   Parsers
   read a given phrase followed by an end of file/string
*)

let parse ph inp = Grammars.Pkit.parse ph EOF inp

let identifier_parser tbl inp =
  parse (Grammars.long_id Grammars.id_relaxed (mk_info tbl)) inp

let type_parser tbl inp =
  parse (Grammars.types (mk_info tbl)) inp
let typedef_parser tbl inp =
  parse (Grammars.typedef (mk_info tbl)) inp

let term_parser tbl inp =
  parse (Grammars.form (mk_info tbl)) inp

let defn_parser tbl inp =
  parse (Grammars.defn (mk_info tbl)) inp

(*** User defined parsers ***)

let term_parser_list tbl = Table.term_parsers tbl
let add_term_parser tbl pos n ph =
  let plist0 = Table.term_parsers tbl in
  Table.set_term_parsers tbl (Lib.named_add plist0 pos n ph)

let remove_term_parser tbl n =
  let plist0 = Table.term_parsers tbl in
  Table.set_term_parsers tbl (List.remove_assoc n plist0)

let type_parser_list tbl = Table.type_parsers tbl
let add_type_parser tbl pos n ph =
  let plist0 = Table.type_parsers tbl in
  Table.set_type_parsers tbl (Lib.named_add plist0 pos n ph)

let remove_type_parser tbl n =
  let plist0 = Table.type_parsers tbl in
  Table.set_type_parsers tbl (List.remove_assoc n plist0)

(*** Readers: read and parse a string ***)

let get_symtab tbl = Table.symbols tbl

let read tbl ph str =
  let symtab = get_symtab tbl in
  Lexer.reader (scan symtab) (ph tbl) str

let read_term tbl str =
  read tbl term_parser str

let read_type tbl str =
  read tbl type_parser str

let test_lex tbl str =
  let symtab = get_symtab tbl in
  scan symtab (Stream.of_string str)

let test tbl str =
  let symtab = get_symtab tbl in
  reader (scan symtab) (term_parser tbl) str
