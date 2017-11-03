(*----
  Name: grammars.ml
  Copyright Matthew Wahab 2005-2016
  Author: Matthew Wahab <mwb.cde@gmail.com>

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

(** Parser constructors for types and terms *)

(*
 * Token information
 *)

type associativity=Parserkit.Info.associativity
let non_assoc=Parserkit.Info.non_assoc
let left_assoc=Parserkit.Info.left_assoc
let right_assoc=Parserkit.Info.right_assoc

type fixity=Parserkit.Info.fixity
let nonfix=Parserkit.Info.nonfix
let infix=Parserkit.Info.infix
let prefix=Parserkit.Info.prefix
let suffix=Parserkit.Info.suffix

(*** Default token information ***)

let default_term_prec = 0
let default_term_assoc = non_assoc
let default_term_fixity=nonfix

let default_type_prec = 0
let default_type_assoc = non_assoc
let default_type_fixity= nonfix

(** Parser grammars *)

(*
 * Parser constructors specialised to tokens from {!Lexer}.
 *)
module Pkit=Parserkit.Make
  (struct
     type tokens=Lexer.tok
     let matches = Lexer.match_tokens
     let string_of_token = Lexer.message_of_token
   end)

(*
 * Useful parser constructors
 *)
module Utility=
struct
  open Lexer
  open Pkit

  let (?$) tok =
    ((!$ tok) >> (fun _ -> Pterm.mk_short_ident (Lexer.string_of_token tok)))
  let (?%) tok =
    ((!$ tok) >> (fun _ -> Gtypes.mk_var (Lexer.string_of_token tok)))
end

(*** Basic types used by the parsers. ***)

type input = Pkit.input
type ('a)phrase = 'a Pkit.phrase
type ('a)parse = input -> 'a

exception ParsingError = Pkit.ParsingError

(** [typdef_data]: Information returned by the typedef parsers
*)
type typedef_data =
  | NewType of (string * (string list))
  | TypeAlias of (string * (string list) * Basic.gtype)
  | Subtype of (string * (string list)
                * Basic.gtype * Pterm.t)


(*** Grammars ***)
open Lexer
open Pkit

type token_info = (Ident.t * fixity * int) option

(** [string_of_tok], [string_tokens]: Get string representation of
    tokens.
*)
let string_of_tok tok = Lexer.message_of_token tok
let string_tokens toks = Lib.list_string string_of_tok " " toks

(*
 * Token tables
 *)

(** [token table] Stores the identifier associated with a symbol, and
    the fixity and precedence of the symbol.  The table is memoised.
*)
let default_table_size = 253;

module TokenTree = Treekit.SimpleTree (struct type key = token end)
type token_table =
    {
      mutable memo: (token * token_info) option;
      table: (token_info) TokenTree.t
    }

let token_table_new sz =
  { memo = None; table = TokenTree.empty }

let token_table_reset tbl =
  { memo = None; table = TokenTree.empty }

(* token_table_add: should fail if token already exists (but doesn't
 *fixme* )
 *)
let token_table_add tbl s tok =
  { tbl with table = TokenTree.add (tbl.table) s tok }

let token_table_find tbl s =
  let mfind =
    match tbl.memo with
      | Some(x, y) ->
          if Lexer.match_tokens s x
          then Some(y)
          else None
      | _ -> None
  in
  match mfind with
    | Some(r) -> r
    | _ ->
      let ret = TokenTree.find (tbl.table) s in
      tbl.memo <- Some(s, ret);
      ret

let token_table_remove tbl s =
  let table1 = TokenTree.remove tbl.table s in
  let memo1 =
    match tbl.memo with
    | Some(x, y) ->
      if Lexer.match_tokens s x
      then None
      else tbl.memo
    | _ -> None
  in
  { memo = memo1; table = table1 }

(** [token_info tbl t]: Look up the information of token [t] in token
    table [tbl]. Return [None] if [t] is not in [tbl].
*)
let token_info tbl t =
  match t with
    | Sym s ->
      (try token_table_find tbl t
       with Not_found -> None)
    | _ -> None

(** [parser_info] Information used when parsing terms and gtypes.

    For terms:
    [bound_names]:
    string representation of bound variables and their associated terms.
    [token_info]:
    precedence and fixity of tokens.

    For gtypes:
    [typ_index]:
    Counter used to generate unique names for type variables.
    [type_token_info]:
    precedence and fixity of gtype tokens.
*)
type parser_info =
    {
      (* term information *)
      bound_names: (string* Pterm.t) list ref;
      token_info: (token -> token_info);
      term_parsers:
        (string, parser_info -> Pterm.t phrase) Lib.named_list;
      (* type information *)
      typ_indx: int ref;
      typ_names: (string* Basic.gtype) list ref;
      type_token_info: (token ->token_info);
      type_parsers:
        (string, parser_info -> (Basic.gtype phrase)) Lib.named_list;
    }

(** [mk_inf tbl type_tbl] Make parsing information from table [tbl]
    and [type_tbl] of term and type token information.
*)
let mk_info (tbl, type_tbl, tparsers, typarsers) =
  {
    bound_names = ref [];
    token_info = (fun x -> token_info tbl x);
    term_parsers = tparsers;

    typ_indx = ref 0;
    typ_names = ref [];
    type_token_info = (fun x -> token_info type_tbl x);
    type_parsers = typarsers;
  }

(*** Utility functions ***)

(** [lookup_name n inf]: Look up [n] in [inf.bound_names].  raise
    Not_found if not found.
*)
let lookup_name n inf =
  List.assoc n !(inf.bound_names)

(** [add_name n trm inf] Associate [trm] with name [n] in
    [inf.bound_names].  Used to associate variable name with a bound
    variable,

    E.g. at the top of a binding term [!x. t],
    [add_name x (Bound b) inf]
    is called to associate [x] with [Bound b] when parsing [t].
*)
let add_name n trm inf =
  inf.bound_names := (n, trm)::!(inf.bound_names)

(** [drop_name n inf] Remove [n] from the list of bound names
    e.g. after parsing the body [t] of a binding term [! x. t]
*)
let drop_name n inf =
  let rec d_aux ls =
    match ls with
      | [] -> []
      | (x, t)::xs ->
        if x = n
        then xs
        else (x, t)::(d_aux xs)
  in
  inf.bound_names:=d_aux !(inf.bound_names)

(** [get_term n inf] Get the term associated with bound name [n].  if
    there is no term associated with [n] then return an unqualified
    identifier.
*)
let get_term n inf =
  try lookup_name n inf
  with Not_found -> Pterm.mk_free n (Gtypes.mk_null())

(**
   [clear_names inf]
   Clear the bound names of [inf]
*)
let clear_names inf = { inf with bound_names = ref [] }

let get_type_indx inf =
  inf.typ_indx := (!(inf.typ_indx)) + 1; !(inf.typ_indx)

(** [mk_vartyp inf]: Make a new, uniquely named, type variable.
    Increments [inf.typ_indx].
*)
let mk_vartyp inf =
  Gtypes.mk_var ("typ"^(string_of_int (get_type_indx inf)))

(** [lookup_type_name n inf]: Lookup type variable name [n].  if not
    found, raise [Not_found].
*)
let lookup_type_name n inf = List.assoc n !(inf.typ_names)

(** [add_type_name n ty inf]: Add [n] as the string representation of
    gtype [ty].
*)
let add_type_name n ty inf =
  inf.typ_names := (n, ty)::!(inf.typ_names); ty

(** [get_type n inf]: Get the type variable represented by name [n].
    If not found, create a type variable, with a unique name, add
    associate [n] with this type variable in [inf] and return this type
    variable.
*)
let get_type n inf =
  try lookup_type_name n inf
  with Not_found ->
    add_type_name n (Gtypes.mk_var n) inf

(** [clear_type_names inf] Clear the record of type variable names.
*)
let clear_type_names inf = { inf with typ_names = ref [] }


(*
 * Token information utility functions
 *)

(** [mk_token_info tbl x]: Extract the precedence and fixity
    information from term token [x], if any.  If not, return the
    default fixity and precedence.  For use with the Parserkit.operator
    parser.
*)
let mk_token_info x =
  match x with
    | Some(_, f, p) -> { fixity = f; prec = p }
    | _ ->
      {
        fixity = default_term_fixity;
        prec = default_term_prec
      }

(** [mk_type_token_info x]: Extract the precedence and fixity
    information from type token [x], if any.  If not, return the
    default fixity and precedence.  Used with the Parserkit.operator
    parser.
*)
let mk_type_token_info tbl t =
  match (token_info tbl t) with
    | Some(_, f, p)->
        { fixity = f; prec = p }
    | _ ->
      {
        fixity = default_type_fixity;
        prec = default_type_prec
      }

(*** Basic parsers ***)

(** [message m _]: Fail, raising [ParsingError m]
*)
let message m _ = raise (ParsingError m)

let parse_error ?(msg="") f inp =
  let test x = true in
  let getn n inp =
    let rec get_aux n toks l =
      if n = 0 then List.rev l
      else
        try
          let (token, toks1) = get test (fun x -> x) toks
          in
          if Lexer.match_tokens Lexer.eof_tok token
          then List.rev l
          else get_aux (n - 1) toks1 (token::l)
        with _ -> List.rev l
    in
    get_aux n inp []
  in
  let token_list = getn 5 inp in
  let string =
    match token_list with
      | [] -> "<end of input>"
      | _ -> Lib.list_string f " " token_list
  in
  raise (ParsingError (msg^"["^string^"]"))

let error msg inp =
  parse_error ~msg:msg string_of_token inp

let type_error msg inp =
    error ("Syntax error in type: "^msg) inp

let term_error msg inp =
  error ("Syntax error in term: "^msg) inp

let comma_list ph toks =
  list0 ph (!$(Sym comma_sym)) toks


let rec repeat_term ph term toks =
  (((ph -- (repeat_term ph term )) >> (fun (x, y) -> x ::y))
   // (term >> (fun _ -> []))) toks

(***
    Identifier parsers.

    Compilicated by the need to look up symbols
    to test if the symbol maps to an identifier
    [id]: read an identifier

    [named_id]: read a specific (given) identifier
***)

(**
   [id_parser info inp]
   General identifier parser.
   matches identifiers and symbols which translate to identifiers.
*)
let id_parser info inp =
  let get_info x = info x in
  let comp x =
    match x with
      | ID _ -> true
      | Sym(OTHER _) -> true
      | _ -> false
  and mk x =
    match x with
      | ID(s) -> s
      | _ ->
        begin
          match get_info x with
            | Some(name, _, _) -> name
            | _ -> error "Not an identifier" inp
        end
  in
    try Pkit.get comp mk inp
    with
        No_match -> error "Not an identifier" inp
      | _ -> error "Not an identifier" inp

(** [id_strict info inp]: String identifier parser.  matches possibly
    qualified identifiers only *)
let id_strict info inp =
  let comp x =
    match x with
      | ID _ -> true
      | _ -> false
  and mk x =
    match x with
      | ID(s) -> s
      | _ -> error "Not an identifier" inp
  in
  try Pkit.get comp mk inp
  with No_match -> error "Not an identifier" inp
    | _ -> error "Not an identifier" inp


(** [id_relaxed info inp]: String identifier parser.  matches as much
    as possible.  *)
let id_relaxed info inp =
  let comp x =
    match x with
      | ID _ -> true
      | Sym(OTHER _) -> true
      | _ -> false
  and mk x =
    match x with
      | ID(s) -> s
      | Sym(OTHER x) -> (Ident.mk_name x)
      | _ -> error "Not an identifier" inp
  in
  try Pkit.get comp mk inp
  with No_match -> error "Not an identifier" inp
    | _ -> error "Not an identifier" inp

(** [named_id info idparser name inp]: Parse an identifier [name],
    using parser [idparser info].  fail if token doesn't match.  *)
let named_id info idparser name inp =
  ((idparser info) >>
      (fun x ->
        if x = name then x
        else
          error
            ("Expected identifier "
             ^(Ident.string_of name)^" but got "
             ^(Ident.string_of x)) inp))
    inp

(** [short_id idparser inf toks]: Parse a short (unqualified)
    identifier, using parser [idparser inf].
*)
let short_id idparser inf toks =
  (idparser inf >>
     (fun x ->
       match Ident.dest x with
         | ("", s) -> s
         | _ -> error "Not a short identifier" toks))
    toks

(** [long_id idparser inf toks]: Parse a possibly qualified
    identifier, using parser [idparser inf].
*)
let long_id idparser inf toks =
  (idparser inf >>
     (fun x ->
       match Ident.dest x with
         | (_, "") -> error "Badly formed identifier" toks
         | _ -> x))
    toks

(** [mk_short_id id inf]: Parse a possibly qualified identifer with
    [id inf], make it a short identifier.  *)
let mk_short_id id inf toks =
  (long_id id inf >> (fun x -> Ident.name_of x)) toks

(*
 * Type parsers
 *)

(** [mk_type_binary_constr inf t]: Construct a gtype from binary
    operators.
*)
let mk_type_binary_constr inf t =
  let lookup x =
    try inf.type_token_info x
    with Not_found -> None
  in
  match t with
    | ID(s) -> (fun x y -> Gtypes.mk_def s [x; y])
    | _ ->
      begin
        match lookup t with
          | Some(name, _, _) ->
            (fun x y-> Gtypes.mk_def name [x; y])
          | _ ->
            raise (ParsingError
                     ((string_of_tok t)
                      ^" is not a type constructor"))
      end

(**
    [mk_type_unary_constr inf t]
    Construct a gtype from unary operators.
*)
let mk_type_unary_constr inf t=
  let lookup x =
    try inf.type_token_info x
    with Not_found -> None
  in
    match t with
      | ID(s) -> (fun x -> Gtypes.mk_def s [x])
      | _ ->
        begin
          match (lookup t) with
            | Some(name, _, _) ->
              (fun x -> Gtypes.mk_def name [x])
            | _ ->
              raise (ParsingError
                       ((string_of_tok t)
                        ^" is not a unary type constructor"))
        end

(**
    [type_id info inp]
    n	Read an identifier for the type parser.
    Take into account that symbol -> should be
    treated as an identifer
*)
let type_id info inp =
  let get_info x =
    try info.type_token_info x
    with Not_found -> None
  in
  let comp x=
    match x with
      | ID _ -> true
      | _ ->
        begin
          match (get_info x) with
            | Some (name, _, _) -> true
            | _ -> false
        end
  and mk x =
    match x with
      | ID(s) -> s
      | _ ->
        begin
          match (get_info x) with
            | Some(name, _, _) -> name
            | _ ->  failwith "parser: id."
        end
  in
    try Pkit.get comp mk inp
    with No_match -> type_error "Not an identifier" inp
      | _ -> type_error "Not an identifier" inp

(** [primed_id inf]: Read a type variable name.
*)
let primed_id inf toks =
  let comp x =
    match x with
      | PrimedID _ -> true
      | _ -> false
  and mk x =
    match x with
      | PrimedID s -> get_type s inf
      | _ -> type_error "Expected type variable" toks
  in
  try Pkit.get comp mk toks
  with No_match -> type_error "Expected type variable" toks
    | _ -> type_error "Expected type variable" toks

(** [bool_type info]: Parse type "bool". *)
let bool_type info toks =
  try
    ((named_id info type_id (Ident.mk_name "bool"))
     >> (fun _ -> Lterm.mk_bool_ty())) toks
  with _ -> type_error "Not a boolean type" toks

(** [num_type info]: Parse type "num".
*)
let num_type info toks =
  try
    ((named_id info type_id (Ident.mk_name "num"))
     >> (fun _ -> Lterm.mk_num_ty())) toks
  with _ -> type_error "Not a number type" toks

(**
    The builtin type parser.

    [inner_types]: parse types built with infix/prefix/suffix operators.
    [atomic_types]:
    Parse types satisfying the grammar
    primed_id
    | num_type
    | bool_type
    | '(' list0_sep inner_type ',' ')' long_id
    | '(' inner_type ')'
    | type_parsers
    | error
*)
let rec inner_types inf toks =
  operators
    (atomic_types inf,
     (fun x -> mk_token_info (inf.type_token_info x)),
     mk_type_binary_constr inf, mk_type_unary_constr inf) toks
and atomic_types inf toks =
  ((type_parsers inf) // type_error "") toks
    (* Core Type Parsers: *)
and type_parsers_list inf = inf.type_parsers
  (**
     [type_parsers inf]
     Try each of the parsers in the list
     [type_parsers_list].
  *)
and type_parsers inf toks =
  named_alt (type_parsers_list inf) inf toks

(** [types inf]: Toplevel for the type parser.
*)
let rec types inf toks =
  inner_types (clear_type_names inf) toks

let type_constructor_parser inf toks =
  let form_type (a, i) =
    match a with
      None -> Gtypes.mk_def i []
    | Some(ts) -> Gtypes.mk_def i ts
  in
  ((((optional
       (((!$(Sym ORB)
          -- ((comma_list (inner_types inf))
              -- (!$(Sym CRB)))))
        >> (fun (_, (args, _)) -> args))))
    -- (long_id id_strict inf))
   >> form_type) toks

let bracketed_type_parser inf toks =
  ((!$(Sym ORB) -- ((inner_types inf) -- !$(Sym CRB)))
       >> (fun x -> fst (snd x))) toks

let core_type_parsers =
  [
    "primed_id", primed_id;
    "num_type", num_type;
    "bool_type", bool_type;
    "type_constructor", type_constructor_parser;
    "bracketed_type", bracketed_type_parser;
  ]

let init_type_parsers inf =
  { inf with type_parsers = core_type_parsers }
(** Support for adding type parsers.
*)

(** [add_type_parser pos n ph]: Add type parser [ph] at position [pos]
    with name [n].
*)
let add_type_parser inf pos n ph =
  { inf with
    type_parsers = (Lib.named_add (type_parsers_list inf) pos n ph) }

(** [remove_type_parser n]: Remove the type parser named [n].
*)
let remove_type_parser inf n =
  { inf with type_parsers = (List.remove_assoc n (type_parsers_list inf)) }


(*
 * Term parsers
 *)

(*** Utility functions for use with the term parser. ***)

(** [mk_conn idsel inf t]: Construct a function application term from
    a binary operator.
*)
let mk_conn inf t =
  let lookup x =
    try inf.token_info x
    with Not_found -> None
  in
  match t with
    | ID(i) -> (fun x y -> Pterm.mk_fun i [x; y])
    | _ ->
      begin
        match (lookup t) with
          | Some (name, _, _) ->
            (fun x y ->
              let fty =
                Gtypes.mk_var ("_"^(Ident.string_of name)^"_ty")
              in
              let f = Pterm.mk_typed_ident name fty
              in
              Pterm.mk_comb f [x; y])
        | _ ->
          raise (ParsingError ((string_of_tok t)
                               ^" is not a connective"))
      end

(** [mk_prefix inf t]: Construct a function application term from a
    unary operator.
*)
let mk_prefix inf t=
  let lookup x =
    try inf.token_info x
    with Not_found -> None
  in
  match t with
    | ID(i) -> (fun x -> Pterm.mk_fun i [x])
    | _ ->
      begin
        match (lookup t) with
          | Some(name, _, _) ->
            (fun x -> Pterm.mk_fun name [x])
          | _ ->
            raise (ParsingError ((string_of_tok t)
                                 ^" is not a prefix"))
      end


(** [qnt_setup_bound_names inf qnt xs]: Make bound variables from the
    name-type pairs in [xs], add them to [inf.bound_names]. [qnt] is the
    quantifier type (All, Ex or Lambda).
*)
let qnt_setup_bound_names
    inf (qnt: Basic.quant) (xs : (string* Basic.gtype) list) =
  let setup_aux (n, ty) =
    let b_id = Pterm.mk_bound(Basic.mk_binding qnt n ty)
    in
    add_name n b_id inf;
    (n, b_id)
  in
  List.map setup_aux xs

(** [qnt_term_remove inf xs body]: Use bound names in [xs] to form a
    quantified term, with body as the initial term.  Simplified
    example: [!x, ?y, !z] t -> (!x: (?y: (!z: t))).

    Remove each name in [xs] from [inf.bound_names] as it is used.
*)
let qnt_term_remove_names inf (xs: (string* Pterm.t) list) body =
  let fold_fn (x, y) b =
    let binder = Pterm.dest_bound y in
    let nt = Pterm.mk_qnt binder b
    in
    drop_name x inf; nt
  in
  List.fold_right fold_fn xs body

(** [qnt_remove_bound_names inf xs body]: Remove each name in [xs]
    from [inf.bound_names] as it is used, replace it with the term it
    was bound to.
*)
let qnt_remove_bound_names inf xs =
  List.map (fun (x, nt) ->  drop_name x inf; nt) xs

(** [make_term_remove_names info wrapper vs body]:

    Remove the variables in [vs] from [info].  Return the term
    constructed by quantifying [body] with the variables [vs],
    applying [wrapper] to each constructed term.
*)
let make_term_remove_names inf wrapper xs body =
  let fold_fn (x, y) b =
    let binder = Pterm.dest_bound y in
    let nt = wrapper (Pterm.mk_qnt binder b)
    in
    drop_name x inf; nt
  in
  List.fold_right fold_fn xs body

(*** The parsers ***)

(** [number]: Read a number. *)
let number inp =
  let comp x = match x with NUM _ -> true | _ -> false
  and mk x =
    match x with
      | NUM s -> Num.num_of_string s
      | _ -> failwith "parser: number"
  in
    try get comp mk inp
    with No_match -> term_error "Not a number" inp
      | _ -> term_error "Not a number" inp

(** [boolean]: Read a boolean constant. *)
let boolean inp =
  let comp x =
    match x with
        BOOL _ -> true
      | _ -> false
  and mk x =
    match x with
      | BOOL b -> b
      | _ -> term_error "Not a boolean" inp
  in
    try get comp mk inp
    with No_match -> term_error "Not a boolean" inp
      | _ -> term_error "Not a boolean" inp

(** [optional_type inf]: Parse an optional type.  [ [':' types] ]
*)
let optional_type inf toks =
  ((( !$(Sym COLON) -- (types inf)) >> (fun (_, ty) -> Some(ty)))
    // (empty >> (fun x -> None))) toks

(** [id]: parse an identifier occuring as a term *)
let id info inp =
  let lookup x = info.token_info x
  in
  id_parser lookup inp

(** [id_type_opt idnt inf]: Parse identifier [idnt inf] with optional
    type.
*)
let id_type_opt idnt inf toks =
  ((((!$(Sym ORB)) -- (idnt inf)
     -- (!$(Sym COLON))-- (types inf) -- (!$(Sym CRB)))
    >> (fun ((((_, i), _), t), _) -> (i, t)))
   // ( (idnt inf) >> (fun x -> (x, mk_vartyp inf))))
    toks

(** [term_identifer inf]: Parse a possibly typed identifer satisfying
    [id_type_opt inf].  Lookup identifier in [inf], to check if it is a
    bound variable.  If not, it is a free variable.
*)
let term_identifier inf toks =
  let action ((n, i), t) =
       let nid=Ident.mk_long n i
       in
       if(Ident.is_short nid)
       then
         (try (lookup_name i inf)
          with Not_found -> Pterm.mk_free i t)
       else
         Pterm.mk_typed_ident nid t
  in
  ((id_type_opt (long_id id) inf)
   >> action) toks

(**
   [form]/[formula]/[type_primary]/[primary]
   Main term parser.

   form: formula {formula}*
   formula: prefix/infix/suffix operators built around typed_primary.
   typed_primary: primary optional_type
   primary:
   '(' form ')'
   | 'ALL' { id_type_opt }+ ':' form
   | 'EX' { id_type_opt }+ ':' form
   | 'LAM' { id_type_opt }+ ':' form
   | id_type_opt
   | number
   | boolean
   | alternative_parsers
   | error
*)
let mk_typed_opt (t, oty) =
  match oty with
    | None -> t
    | Some(ty) -> Pterm.mk_typed t ty

let mk_app_opt (f, oa) =
  match oa with
    | None -> f
    | Some(a) -> Pterm.mk_app f a

let rec form inf toks =
  (
    ((formula inf)-- (repeat (formula inf)))
    >> (fun (x, y) -> Pterm.mk_comb x y)
  ) toks
and formula inf toks=
  (
    operators(typed_primary inf,
              (fun x-> mk_token_info (inf.token_info x)),
              mk_conn inf, mk_prefix inf)
  ) toks
and typed_primary inf toks =
  (
    ((primary inf) --  (optional_type inf))
    >>
      mk_typed_opt
  ) toks
and
    primary inf toks =
  ((term_parsers inf) // (term_error "")) toks

(** [term_parsers_list]: List of term parsers.
*)
and term_parsers_list inf = inf.term_parsers
(** [term_parsers inf tok]: Parse using parsers in
    [term_parsers_list].  *)
and term_parsers inf toks =
  named_alt (term_parsers_list inf) inf toks

module Alt =
struct
  (**
     [alt_form]/[alt_formula]/[alt_typed_primary]/[alt_primary]
     Alternative term parser.

     formula: operators optional_type | appl_term
     appl_term: appl_term typed_primary | typed_primary
     typed_primary: primary optional_type
     primary:
     '(' formula ')'
     | 'ALL' { id_type_opt }+ ':' form
     | 'EX' { id_type_opt }+ ':' form
     | 'LAM' { id_type_opt }+ ':' form
     | id_type_opt
     | number
     | boolean
     | alternative_parsers
     | error
  *)

  let rec form inf toks=
    alt
      [
        (operators (formula inf,
                    (fun x-> mk_token_info (inf.token_info x)),
                    mk_conn inf, mk_prefix inf)
         -- (optional_type inf))
        >> mk_typed_opt
      ] toks
  and formula inf toks =
    alt
      [
        ((((typed_primary inf) -- (optional (formula inf)))
          >> mk_app_opt)
         -- (optional_type inf))
        >> mk_typed_opt;
        typed_primary inf
      ] toks
  and typed_primary inf toks =
    (((primary inf) -- (optional_type inf))
     >> mk_typed_opt) toks
  and primary inf toks =
    ((term_parsers inf) // (term_error "")) toks
  and term_parsers_list =  ref []
  and term_parsers inf toks =
    named_alt (!term_parsers_list) inf toks
end

(** [core_term_parser_list]: The primary term parsers are stored in a
    named list.
*)
let core_term_parsers =
  [
    (* id '(' id ':' type ')' *)
    "identifier", term_identifier;
    (*   | boolean *)
    "boolean",
    (fun _ ->
       (boolean >> (fun x -> Pterm.mk_bool x)));
    (* '(' form ')' *)
    "bracketed_term",
    (fun inf ->
       ( (( !$ (Sym ORB) -- ((form inf) -- !$(Sym CRB))))
         >> (fun (_, (x, _)) ->  x)));

    (* 'ALL' { id_type_opt }+ ':' form *)
    "forall",
    (fun inf ->
       (((( !$ (Key ALL)
            -- ((id_type_opt (short_id id_strict) inf)
                -- ((repeat (id_type_opt (short_id id_strict) inf))
                    -- (!$(Sym COLON)))))
          >>
            (fun (_, (v, (vs, _))) ->
               qnt_setup_bound_names inf Basic.All (v::vs)))
         -- (form inf))
        >>
            (fun ((xs: (string*Pterm.t)list), body) ->
               qnt_term_remove_names inf xs body)));

    (* 'EX' { id_type_opt }+ ':' form *)
    "exists",
    (fun inf ->
       (((( !$ (Key EX)
            -- ((id_type_opt (short_id id_strict) inf)
                -- ((repeat (id_type_opt (short_id id_strict) inf))
                    -- (!$(Sym COLON)))))
          >>
            (fun (_, (v, (vs, _))) ->
               qnt_setup_bound_names inf Basic.Ex (v::vs)))
         -- (form inf))
        >>
            (fun ((xs: (string*Pterm.t)list), body) ->
               qnt_term_remove_names inf xs body)));
    (* 'LAM' { id_type_opt }+ ':' form *)
    "lambda",
    (fun inf ->
       (((( !$ (Key LAM)
            -- ((id_type_opt (short_id id_strict) inf)
                -- ((repeat (id_type_opt (short_id id_strict) inf))
                    -- (!$(Sym COLON)))))
          >>
            (fun (_, (v, (vs, _))) ->
               qnt_setup_bound_names inf Basic.Lambda (v::vs)))
         -- (form inf))
        >>
            (fun ((xs:(string*Pterm.t)list), body) ->
               qnt_term_remove_names inf xs body)))
  ]

(** [init_term_parsers]: Initilalise the term parsers *)
let init_term_parsers inf =
  {inf with term_parsers = core_term_parsers}

(*
 *    Support functions
 *)

(** [add_parser pos n ph]: Add term parser [ph] with name [n] in
    position [pos].
*)
let add_parser inf pos n ph =
  { inf with
    term_parsers = (Lib.named_add (term_parsers_list inf) pos n ph) }

(** [remove_parser n]: Remove term parser named [n].
*)
let remove_parser inf n =
  { inf with
    term_parsers = (List.remove_assoc n (term_parsers_list inf)) }

(** [parse_as_binder f sym]: Construct a grammar to parse function
    applications of the form [f (%x: P)] as [sym x: P].

    Symbol [sym] should be added to the lexer seperately.
    (e.g. using [Parser.add_symbol sym (Lexer.Sym(Lexer.OTHER sym))]).
*)
let parse_as_binder ident sym =
  let sym_tok = Sym(OTHER sym)
  and colon = Sym(COLON)
  in
  let id_term = Pterm.mk_ident ident in
  let wrapper b = Pterm.mk_app id_term b in
  let grammar inf inp =
    (((((!$ sym_tok)
        -- (id_type_opt (short_id id_strict) inf)
        -- (repeat (id_type_opt (short_id id_strict) inf))
        -- (!$ colon))
       >>
        (fun (((_, v), vs), _) ->
           qnt_setup_bound_names inf Basic.Lambda (v::vs)))
      --
        (form inf))
     >>
        (fun ((xs:(string*Pterm.t)list), body) ->
           make_term_remove_names inf wrapper xs body)) inp
  in
  grammar


(*** Definitions ***)

(*
 *    Type definitions
 *)

(** [typedef inf]: Parse a type definition.

    Grammar:
    typedef ::= simple_typedef
    | subtypedef

    simple_typedef::= ('(' {primed_id}* ')')? short_id ( '=' type )?
    subtypedef ::= type ':' term
*)
let simple_typedef inf toks =
  (((optional
       ((!$(Sym ORB)-- ((comma_list (primed_id inf)) -- (!$(Sym CRB))))
        >> (fun (_, (x, _)) -> List.map Gtypes.get_var_name x)))
    --
       ((short_id type_id inf)
        --
        (optional
           (((!$(mk_symbol Lterm.equalssym))
             -- (types inf)) >> (fun (_, x) -> x)))))
   >> (fun (args, (name, defn))
         -> (name, args, defn))) toks

let subtypedef inf toks =
  let lhs inp=
    ((optional
        ((!$(Sym ORB)-- ((comma_list (primed_id inf)) -- (!$(Sym CRB))))
         >> (fun (_, (x, _)) -> List.map Gtypes.get_var_name x)))
     --
       (short_id type_id inf)) inp
  and rhs inp =
    (((types inf)
      -- (!$(Sym COLON))
      -- (form inf))
     >> (fun ((ty, _), f) -> (ty, f))) inp
  in
    ((lhs -- (!$(mk_symbol Lterm.equalssym)) -- rhs)
     >> (fun (((args, name), _), (ty, trm))
           -> (name, args, ty, trm))) toks

let typedef inf toks =
  let simple_typedef_action (n, args, dtyp) =
    match dtyp with
      | None -> NewType (n, Lib.from_option args [])
      | (Some dt) -> TypeAlias(n, Lib.from_option args [], dt)
  in
  (((subtypedef inf) >>
      (fun (n, args, dtyp, set) ->
        Subtype(n, Lib.from_option args [], dtyp, set)))
   //
     ((simple_typedef inf) >> simple_typedef_action)) toks

(** [defn inf toks]: Parse a definition.

    Grammar:
    (id_type_opt short_id) (id_type_opt short_id)* '=' form
*)
let rec lhs inf toks=
  ((((id_type_opt (short_id id_strict) inf)
     -- (args_opt inf))
    >>
     (fun ((n, t), args) -> (n, t), args))
   // error "Badly formed identifier for definition")
    toks
and args_opt inf=
  (((optional (repeat (id_type_opt (short_id id_strict) inf)))
    -- (!$(mk_symbol Lterm.equalssym)))
   >> (fun (x, _) -> match x with None -> [] | Some l -> l))
  // error "Badly formed argument list for definition"
and defn inf toks =
  (
    ((((lhs inf) >>
         (fun (n, arg_ns) ->
            (n, qnt_setup_bound_names inf Basic.All arg_ns)))
      --
         (form inf))
     >>
       (fun ((n, arg_ns), r) ->
         let args = qnt_remove_bound_names inf arg_ns
         in
         ((n, args), r))))
    toks
