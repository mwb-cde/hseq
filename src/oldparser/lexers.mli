(* Simple lexer (alpha quality) *)

  module type TOKENS=
    sig 
    type tok

    exception Error
      val add_keyword : string -> tok -> unit
      val remove_keyword: string -> unit
      val add_symbol: string -> tok -> unit
      val remove_symbol : string -> unit
    val keywords : unit -> (string  * tok) list
    val syms : unit -> (string * tok) list
    val set_other: (string -> int -> tok * int) -> unit
    val other : unit -> string -> int -> tok * int
    val white_space: char list
    val special: char list
    val mk_id: string -> tok


    val string_of_token: tok -> string
    val print_token: tok -> unit
    end

module type LEX=
  functor (Tokens: TOKENS) -> 
  sig 
    type tok = Tokens.tok

    val lex: string -> tok list

      val add_keyword : string -> tok -> unit
      val remove_keyword: string -> unit
      val add_symbol: string -> tok -> unit
      val remove_symbol : string -> unit
  end

module Lex: LEX
