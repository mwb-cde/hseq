(* top-down parser for logical terms *)
(* alpha quality *)

module TpParsing :
  sig
    exception ParsingError of string
    type token = Termlex.tok
    val empty : token list -> 'a list * token list
    val (||) : (token list -> 'a) -> (token list -> 'a) -> token list -> 'a
    val (!!) : (token list -> 'a) -> token list -> 'a
    val (--) :
      (token list -> 'a * token list) ->
      (token list -> 'b * token list) -> token list -> ('a * 'b) * token list
    val (>>) :
      (token list -> 'a * token list) ->
      ('a -> 'b) -> token list -> 'b * token list
    val orl : (token list -> 'a) list -> token list -> 'a
    val (!$) : token -> token list -> token * token list
    val ($--) : token -> (token list -> 'a * 'b) -> token list -> 'a * 'b
    val optional :
      (token list -> 'a * token list) -> token list -> 'a option * token list
    val repeat :
      (token list -> 'a * token list) -> token list -> 'a list * token list
    val infixes :
      (token list -> 'a * token list) * (token -> int) *
      (token -> 'a -> 'a -> 'a) -> token list -> 'a * token list
    val parse : (token list -> 'a * 'b list) -> token list -> 'a
  end

module type TPPARSER =
  sig

    type infotyp =
      { scope: Corepp.pp_state;
	typ_indx: int ref;
        typ_names: (string, Gtypes.gtype) Lib.substype;
        prec_of: Basic.id_selector -> Basic.fnident -> int }

    val string_of_tok : Termlex.tok -> string
    val string_tokens : Termlex.tok list -> string

    val mk_empty_inf : Corepp.pp_state -> infotyp
    val mk_inf : Corepp.pp_state -> infotyp

    val add_type_parser :
      (infotyp -> TpParsing.token list -> Gtypes.gtype * TpParsing.token list) 
      ->  unit
    val other_type_parsers :
      infotyp -> TpParsing.token list -> Gtypes.gtype * TpParsing.token list

    val add_parser :
      (infotyp -> TpParsing.token list -> Term.term * TpParsing.token list) 
      ->  unit
    val other_parsers :
      infotyp -> TpParsing.token list -> Term.term * TpParsing.token list


(* atomic and formula parsers *)

    val alternates : ('a -> 'b -> 'c) list -> 'a -> 'b -> 'c
    val error : Termlex.tok list -> 'a
    val none : Termlex.tok list -> Termlex.tok * 'a list

    val id : Termlex.tok list -> Basic.fnident * Termlex.tok list
(*    val number : Termlex.tok list -> int * Termlex.tok list *)
    val number : Termlex.tok list -> Num.num * Termlex.tok list 
    val boolean : Termlex.tok list -> bool * Termlex.tok list
    val bool_type : Termlex.tok list -> Gtypes.gtype * Termlex.tok list
    val num_type : Termlex.tok list -> Gtypes.gtype * Termlex.tok list
    val comma_list :
      (TpParsing.token list -> 'a * TpParsing.token list) ->
      TpParsing.token list -> 'a list * TpParsing.token list
    val listof :
      (TpParsing.token list -> 'a * TpParsing.token list) ->
      TpParsing.token list -> 'a list * TpParsing.token list

    val short_id :
      'a -> TpParsing.token list ->  string * TpParsing.token list
    val long_id : 'a -> TpParsing.token list -> 
      (string* string) * TpParsing.token list

    val types :
      infotyp -> TpParsing.token list -> Gtypes.gtype * TpParsing.token list

    val inner_types :
      infotyp -> TpParsing.token list -> Gtypes.gtype * TpParsing.token list
    val atomic :
      infotyp -> TpParsing.token list -> Gtypes.gtype * TpParsing.token list

    val typedef :
      infotyp ->
      TpParsing.token list ->
      (string * string list option * Gtypes.gtype option) *
      TpParsing.token list


    val form :
      infotyp -> TpParsing.token list -> Term.term * TpParsing.token list
    val formula :
      infotyp -> TpParsing.token list -> Term.term * TpParsing.token list

    val possible_type :
      infotyp ->
      TpParsing.token list -> Gtypes.gtype option * TpParsing.token list
    val typed_primary :
      infotyp -> TpParsing.token list -> Term.term * TpParsing.token list
    val primary :
      infotyp -> TpParsing.token list -> Term.term * TpParsing.token list
    val id_type_op :
      (infotyp -> TpParsing.token list 
	-> 'a * TpParsing.token list) ->
	  infotyp ->
      TpParsing.token list -> ('a * Gtypes.gtype) * TpParsing.token list


    val defn :
      infotyp ->
      TpParsing.token list ->
      ((string * (string * Gtypes.gtype) list) * Term.term) *
      TpParsing.token list

    val defeq :
      infotyp ->
      TpParsing.token list ->
      ((string * (string * Gtypes.gtype) list) * Term.term) *
      TpParsing.token list

    val read_typedef :
      Corepp.pp_state ->
      string -> string * string list option * Gtypes.gtype option

    val read_defeq :
      Corepp.pp_state ->
      string -> (string * (string * Gtypes.gtype) list) * Term.term

    val identifier : Corepp.pp_state -> string -> Basic.fnident

    val read_type : Corepp.pp_state -> string -> Gtypes.gtype
    val read_fulltype : Corepp.pp_state -> string -> Gtypes.gtype
    val read_term : Corepp.pp_state -> string -> Term.term
    val read : Corepp.pp_state -> string -> Term.term
  end

module TpParser: TPPARSER

