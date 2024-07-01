(*----
  Copyright (c) 2020-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** Functor arguments *)
module type TOKENS =
  sig
    (** Tokens *)
    type t
    val equals: t -> t -> bool

    (** [string_of_token]: Used for error reporting only.  If necessary
      use [(fun x _ -> "")].  *)
    val string_of_token: t -> string

  end

module type INPUT =
  sig
    (** Input type *)
    type char_t      (** Basic character type *)
    type input  (** Stream of [t] *)

    (** [is_empty inp]: true iff [inp] is empty. *)
    val is_empty : input -> bool

    (** [look inp]: get first element in input inp but don't remove it from
       input.  *)
    val look: input -> ((char_t)option * input)

    (** [accept inp]: Get a new input, formed by dropping the first element
       of [inp].  This is non-destructive, the first element will still be
       available in [inp].  *)
    val accept: input -> input
  end

module SeqInput(A: sig type char_t end):
(INPUT
 with type char_t = A.char_t
  and type input = (A.char_t)ListSeq.t)

(** {5 Lexers} *)
module type T =
sig
  type token
  type char_t
  type input

  (** The type of matchers on the input. A matcher returns the constructed
      token, or [None] if no match, and the input with the matched elements
      removed. *)
  type ('a)matcher = input -> (('a)option * input)

  (** [matched rslt] True iff the result is the result of sucessful match *)
  val matched: (('a)option * input) -> bool

  (** [result_of rstl] Get the result of the matcher *)
  val result_of: (('a)option * input) -> ('a)option

  (** [value_of rstl] Get the value returned by the matcher, if there is one *)
  val value_of: (('a)option * input) -> 'a

  (** [input_of rstl] Get the input returned by the matcher *)
  val input_of: (('a)option * input) -> input

  (** [drop n inp] Drop the first [n] elments from the input *)
  val drop: int -> input -> input

  (** [fail inp] Return a failure result *)
  val fail: input -> (('a)option * input)

  (** {5 Constructors} *)

  (** [take taker maker inp]: Construct a value from one or more elements at the
      front of the input.

      [taker] Check for valid elements at the start of the input. Remove the
       valid elements from the input, return a data for [maker]. Return [None]
       if the first input elements are not valid.

      [maker] Construct a token from the data constructed by [taker]

      Test the input using [pred inp]. If false, return [(None,
      inp)]. Otherwise, apply [make inp] and return the constructed value,
      wrapped in [Some], and the updated input.
   *)
  val take:
    (input -> (('a)option * input)) -> ('a -> 'b) -> ('b)matcher

  (** [cond pred mt mf inp] Apply predictate [pred] to [inp]. If it is true,
      return [mt inp] otherwise [mf inp] *)
  val cond: (input -> bool) -> ('a)matcher -> ('a)matcher -> ('a)matcher

  (** [alt mchs inp] Apply each of the matchers [mchs] to [inp], returning the
      result of the first that succeeds (doesn't return a value of [None]) *)
  val alt: (('a)matcher)list -> ('a)matcher

  (** [seq mchs inp] Apply each of the matchers [mchs] to [inp], returning
     the list of generated tokens. Return a value of [None] if any matcher
     fails *)
  val seq: (('a)matcher)list -> (('a)list)matcher

  (** [fold ms z inp]: Fold the matchers [ms], starting with initial value [z]
   *)
  val fold: ('a -> ('a)matcher)list -> 'a -> ('a)matcher

  (** [apply m f inp]: Apply [f] to the value constructed by matcher [m] *)
  val apply: ('a -> 'b) -> ('a)matcher -> ('b)matcher

  (** [m >> f] is [apply f m] *)
  val (>>): ('a)matcher -> ('a -> 'b) -> ('b)matcher

  (** [chain f m inp]: Pass the value constructed by [m inp] to [f] *)
  val chain: ('a)matcher -> ('a -> ('b)matcher) -> ('b)matcher

  (** [m +> f] is [chain f m] *)
  val (+>): ('a)matcher -> ('a -> ('b)matcher) -> ('b)matcher

  (** [pass m inp v]: Pass through a value [v], if matcher [m] succeeds,
       discarding the result of [m]. This is intended for use with [chain] *)
  val pass: ('a)matcher -> 'b -> ('b)matcher

  (** [opt m default inp]: Try [m], returning the constructed value or
      [default] if [m] fails *)
  val opt: ('a)matcher -> 'a -> ('a)matcher

  (** [scanner skip matcher]: Generate a sequence by applying [matcher] to
      the input. Silently drop any value for which [skip] is true.  The
      sequence ends when [matcher] returns [None]. *)
  val scanner: ('a -> bool) -> ('a)matcher -> input -> ('a)ListSeq.t
end

module Make (I: INPUT) (T: TOKENS):
(T with type token = T.t
    and type input = I.input)
(** Generic lexer constructors *)


(** {5 Lexers for character input} *)
module Char: functor (A: TOKENS) ->
sig
  module Input: (INPUT
                 with type char_t = char
                  and type input = (char)ListSeq.t)

  module Kit: (T with type char_t = char and type input = (char)ListSeq.t)

  val result_of: (('a)option * 'b) -> ('a)option
  val value_of: (('a)option * 'b) -> 'a
  val input_of: (('a)option * 'b) -> 'b

  (** Character input *)
  type input = (char)ListSeq.t
  val look: input -> ((char)option * input)
  val accept: input -> input
  val is_empty: input -> bool

  val eof_char: char

  (** Recognizers *)
  val is_range: char -> char -> char -> bool
  val is_in: (char)list -> char -> bool
  val is_alpha: char -> bool
  val is_digit: int -> int -> char -> bool
  val digit_of_char: char -> int
  val num_of_digit_list: (int)list -> int

  val taker_nth: (int -> char -> bool) -> ((char)list)Kit.matcher
  val taker: (char -> bool) -> ((char)list)Kit.matcher
  val string: bool -> string -> (string)Kit.matcher

  val look_nth: int -> input -> (string * input)

  type ('a)symbol = (string * 'a)
  val mk_symbol: string -> 'a -> ('a)symbol

  type ('a)symbols = (int * (('a)symbol)list)list
  val symbols_of: (('a)symbol)list -> ('a)symbols

  val max_symbol_size: ('a)symbols -> int
  (** Find a string in a symbol table. Returns the symbol and the size of
     the length of the string prefix that was found *)
  val find_symbol: ('a)symbols -> string -> (int * 'a)option

  val symbol: ('a)symbols -> ('a)Kit.matcher

  val digit: int -> int -> (int)Kit.matcher
  val digits: int -> int -> ((int)list)Kit.matcher

  val is_hexdigit: char -> bool
  val hexdigits: ((int)list)Kit.matcher

  val sign: (bool)Kit.matcher

  type integer_size = Word | Long | LongLong

  (** Data about a lexed C-integer *)
  type integer_data =
    {
      size: integer_size;       (** Size of the integer type *)
      signed: bool;             (** Whether the integer type is signed *)
      base: int;                (** Base of the integer. One of 10, 16 or 8 *)
      negate: bool;             (** Whether the integer is negated *)
      digits: (int)list;        (** The digits making up the number *)
    }

  val mk_default_int: unit -> integer_data
  val set_int_size: integer_data -> integer_size -> integer_data
  val set_int_signed: integer_data -> bool -> integer_data
  val set_int_base: integer_data -> int -> integer_data
  val set_int_negate: integer_data -> bool -> integer_data
  val set_int_digits: integer_data -> (int)list -> integer_data

  val integer_suffix_one:
     (char)option -> integer_data -> (char * integer_data)Kit.matcher
  val integer_suffix: integer_data -> (integer_data)Kit.matcher

  (** Match an integer literal, returning information which can be used to
     construt the integer. The string representation accepted is intended to
     be the same as for C integer literals. Support decimal, octal,
     hexadecimal, signed and unsigned integer literal *)
  val integer: (integer_data)Kit.matcher

  (** [identifier initialp restp]: Match an identifier constructed by the
      pattern [<initialp>(<restp>+)] *)
  val identifier: (char -> bool) -> (char -> bool) -> (string)Kit.matcher

end
