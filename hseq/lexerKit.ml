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
    (** Input *)
    type char_t
    type input

    (** [is_empty inp]: true iff [inp] is empty. *)
    val is_empty : input -> bool

    (** [look inp]: get first element in input inp but don't remove it
        from input.  *)
    val look: input -> ((char_t)option * input)

    (** [accept inp]: Get a new input, formed by dropping the first element of
      [inp].  This is non-destructive, the first element will still be available
      in [inp].  *)
    val accept: input -> input
  end

module SeqInput =
functor (A: sig type char_t end) ->
struct
  type char_t = A.char_t
  type input = (char_t)ListSeq.t
  let is_empty inp = ListSeq.is_empty inp
  let look inp =
    match ListSeq.look 1 inp with
    | ([c], inp1) -> (Some(c), inp1)
    | _ -> (None, inp1)
  let accept inp = ListSeq.accept inp
end


(** Interface *)
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

    (** [result_of rstl] Get the token value returned by the matcher *)
    val result_of: (('a)option * input) -> ('a)option

    (** [value_of rstl] Get the value returned by the matcher, if there is one
       *)
    val value_of: (('a)option * input) -> 'a

    (** [input_of rstl] Get the input returned by the matcher *)
    val input_of: (('a)option * input) -> input

    (** [drop n inp] Drop the first [n] elments from the input *)
    val drop: int -> input -> input

    (** [fail inp] Return a failure result *)
    val fail: input -> (('a)option * input)

    (** {5 Constructors} *)

    (** [take taker maker inp]: Construct a value from one or more elements at
       the front of the input.

      [taker] Check for valid elements at the start of the input. Remove the
       valid elements from the input, return a data for [maker]. Return [None]
       if the first input elements are not valid.

      [maker] Construct a token from the data constructed by [taker]

      Test the input using [pred inp]. If false, return [(None,
       inp)]. Otherwise, apply [make inp] and return the constructed value,
       wrapped in [Some], and the updated input.  *)
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

    (** [apply f m inp]: Apply [f] to the value constructed by matcher [m] *)
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

    val scanner:
      ('a -> bool) -> ('a)matcher -> input -> ('a)ListSeq.t
  end

module Make (I: INPUT) (T: TOKENS) =
  struct

    (** [input] Lexer input *)
    type token = T.t

    type input = I.input
    type char_t = I.char_t
    let is_empty = I.is_empty
    let look = I.look
    let accept = I.accept

    type ('a)matcher = input -> (('a)option * input)

    let matched (x, _) = (x != None)
    let result_of (x, _) = x
    let value_of (x, _) =
      if x = None
      then failwith (__LOC__ ^ ": value_of")
      else from x
    let input_of (_, y) = y

    let fail inp = (None, inp)

    (** [drop n inp] Drop the first [n] elments from the input *)
    let rec drop n inp =
      if n = 0
      then inp
      else drop (n - 1) (accept inp)

    (** Basic constructors *)

    let get pred make inp =
      let (cond, inp0) = pred inp in
      if cond
      then
        let (rslt, inp1) = make inp0 in
        (Some(rslt), inp1)
      else (None, inp0)

    let take taker maker inp =
      match taker inp with
      | (Some(v), inp1) ->
         (Some(maker v), inp1)
      | (_, inp1) -> (None, inp1)

    (** Combinators *)

    let cond pred mt mf inp =
      if pred inp
      then mt inp
      else mf inp

    let alt matchers inp0 =
      let rec alt_aux lst inp =
        match lst with
        | [] -> (None, inp)
        | (m::ms) ->
           let r = m inp
           in
           if (result_of r) = None
           then alt_aux ms inp
           else r
      in
      alt_aux matchers inp0

    let seq matchers inp0 =
      let rec seq_aux lst rslt inp =
        match lst with
        | [] ->
           let retval = List.rev rslt in
           (Some(retval), inp)
        | (m::ms) ->
           let r = m inp
           in
           if (result_of r) = None
           then (None, inp0)
           else seq_aux ms ((from (result_of r))::rslt) (input_of r)
      in
      if matchers = []
      then (None, inp0)
      else seq_aux matchers [] inp0

    let fold matchers zero inp0 =
      let rec fold_aux lst sum inp =
        match lst with
        | [] -> (Some(sum), inp)
        | (m::ms) ->
           let r = m sum inp
           in
           if (result_of r) = None
           then (None, inp0)
           else fold_aux ms (from (result_of r)) (input_of r)
      in
      if matchers = []
      then (None, inp0)
      else fold_aux matchers zero inp0

    let apply f matcher inp =
      let r = matcher inp in
      let v = result_of r in
      if v = None
      then (None, inp)
      else (Some (f (from v)), input_of r)

    let (>>) m f = apply f m

    let chain m f inp =
      let r = m inp in
      let v = result_of r in
      if v = None
      then (None, inp)
      else (f (from v)) (input_of r)

    let (+>) = chain

    let pass m v inp =
      let r = m inp in
      if (result_of r) = None
      then (None, inp)
      else (Some(v), input_of r)

    let opt m default inp =
      let r = m inp in
      let v = result_of r in
      if v = None
      then (Some(default), inp)
      else (v, input_of r)

    (** Scanner *)

    let scanner skip (matcher: ('a)matcher) (ins: input) =
      let rec scan inp =
        let rslt = matcher inp
        in
        let value = result_of rslt
        and inp1 = input_of rslt
        in
        match value with
        | None -> None
        | Some(v) ->
           if skip v
           then scan inp1
           else Some(v, inp1)
      in
      ListSeq.of_fun scan ins

  end

(** {5 Lexers for character input} *)
module Char =
  functor (T: TOKENS) ->
  struct

    module Input = SeqInput(struct type char_t = char end)
    module Kit = Make(Input)(T)
    open Kit

    let result_of = Kit.result_of
    let value_of = Kit.value_of
    let input_of = Kit.input_of

    (** Character input *)
    type input = Kit.input
    type char_t = Kit.char_t
    let eof_char = Char.chr 0

    let look inp =
      try Input.look inp with _ -> (eof_char, inp)

    let accept = Input.accept
    let is_empty = Input.is_empty

    (** C integers *)
    let is_range first last ch =
      ((Char.code ch) >= (Char.code first))
      && ((Char.code ch) <= (Char.code last))

    let is_in lst ch = List.mem ch lst

    let is_digit f l ch =
      let char_zero = Char.code '0' in
      let bottom = Char.chr (char_zero + f)
      and top = Char.chr (char_zero + l)
      in
      is_range bottom top ch

    let digit_of_char ch = ((Char.code ch) - (Char.code '0'))

    let digit first last inp =
      let (ch, inp1) = look inp in
      if is_digit first last ch
      then (Some(digit_of_char ch), accept inp1)
      else (None, inp1)


    (** Match a symbol *)

    (** Match a sequence of characters satisfying predicate [pred]. The
        predicate is passed a character and the index of the character in the
        sequence being matched (starting at 0) *)
    let taker_nth pred inp =
      let rec taker_aux idx lst inp1 =
        if is_empty inp1
        then (lst, inp1)
        else
          begin
            let (ch, inp2) = look inp1 in
            if pred idx ch
            then taker_aux (idx + 1) (ch::lst) (accept inp2)
            else (lst, inp2)
          end
      in
      let (rslt, inp1) = taker_aux 0 [] inp
      in
      if rslt = []
      then (None, inp1)
      else (Some (List.rev rslt), inp1)

    let taker (pred: char -> bool) inp =
      taker_nth (fun (_: int) -> pred) inp

    (** [look_nth n] Get the first [n] characters from the and return them
       as a string. Doesn't change the input *)
    let look_nth n inp =
      let rec look_aux ctr lst ins =
        if is_empty ins
        then (lst, ins)
        else if ctr = 0
        then (lst, ins)
        else
          begin
            let (ch, ins1) = look ins in
            look_aux (ctr - 1) (ch::lst) (accept ins1)
          end
      in
      let (ch_lst, inp1) = look_aux n [] inp
      in
      (String.concat "" (List.map (String.make 1) (List.rev ch_lst)), inp1)

    type ('a)symbol = (string * 'a)
    let mk_symbol str tk = (str, tk)

    type ('a)symbols = (int * (('a)symbol)list)list

    let max_symbol_size (tbl:('a)symbols) =
      match tbl with
      | ((sz, _)::rest) -> sz
      | _ -> 0

    let symbols_of sym_list =
      let lst0 =
        List.map (fun (s, t) -> (String.length s, (s, t))) sym_list
      in
      let lst1 =
        List.sort
          (fun (sz1, r1) (sz2, r2) ->
            if sz1 = sz1
            then compare (fst r1) (fst r2)
            else compare sz1 sz2)
          lst0
      in
      let rec collapse lst curr_sz curr_syms rslt =
        match lst with
        | ((sz, sym)::ls) ->
           if sz = curr_sz
           then collapse ls curr_sz (sym::curr_syms) rslt
           else collapse ls sz [sym] ((curr_sz, List.rev curr_syms)::rslt)
        | _ -> (curr_sz, List.rev curr_syms)::rslt
      in
      begin
        match lst1 with
        | [] -> []
        | ((sz, sym)::rest) -> collapse rest sz [sym] []
      end

    let find_symbol (syms:('a)symbols) str =
      let str_sz = String.length str
      in
      let rec find_aux tbl =
        match tbl with
        | [] -> None
        | (sz, (sym_lst:(('a)symbol)list))::rest ->
           if sz <= str_sz
           then
             begin
               let ret_opt = List.assoc_opt (String.sub str 0 sz) sym_lst
               in
               if ret_opt = None
               then find_aux rest
               else some (sz, from ret_opt)
             end
           else find_aux rest
      in
      find_aux syms

    let symbol syms inp =
      let max_sym_size = max_symbol_size syms in
      let (str, inp1) = look_nth max_sym_size inp in
      if (String.length str) = 0
      then (None, inp1)
      else
        let (rslt_opt: (int * 'a)option) = find_symbol syms str in
        if rslt_opt = None
        then (None, inp1)
        else
          begin
            let (prefix_sz, sym) = from rslt_opt in
            (some sym, drop prefix_sz inp1)
          end

    (** Match a string of characters. Return the actual characters taken from
        the input *)
    let string case_sensitive str inp =
      let str_len = String.length str in
      let rec take_aux idx rslt inp1 =
        if not (idx < str_len)
        then (Some(str), inp1)
        else if is_empty inp1
        then (None, inp1)
        else
          let c = String.get str idx in
          begin
            let (ch, inp2) = look inp1 in
            let (lch, lc) =
              if case_sensitive
              then (ch, c)
              else (Char.lowercase_ascii ch, Char.lowercase_ascii c)
            in
            if lch = c
            then take_aux (idx + 1) (c::rslt) (accept inp2)
            else (None, inp2)
          end
      in
      take_aux 0 [] inp

    (* Make an integer from a list of integers. The least significant
           integer is at the end of the list. *)
    let rec num_of_digit_list ds =
      let rec num_digit_aux lst m rslt =
        match lst with
        | (x::xs)
          -> num_digit_aux xs (m * 10) (rslt + (x * m))
        | _ -> rslt
      in num_digit_aux (List.rev ds) 1 0

    let digits f l inp =
      let digit_taker isq = taker (is_digit f l) isq in
      let (ds_opt, inp1) = digit_taker inp in
      if ds_opt = None
      then ((None:((int)list)option), inp)
      else
        begin
          let ds = List.map digit_of_char (from ds_opt) in
          (Some (ds), inp1)
        end

    let hexdigit_of_char ch =
      if '0' <= ch && ch <= '9'
      then ((Char.code ch) - (Char.code '0'))
      else ((Char.code ch) - (Char.code 'a')) + 10

    let is_hexdigit ch =
      let lch = Char.lowercase_ascii ch
      in
      (is_digit 0 9 lch) || ('a' <= lch && lch <= 'f')

    let hexdigits inp =
      let digit_taker = taker is_hexdigit in
      let (ds_opt, inp1) = digit_taker inp in
      if ds_opt = None
      then (None, inp)
      else (Some (List.map hexdigit_of_char (from ds_opt)), inp1)

    (** [sign inp]: Check for a negative/positive sign. Return [true] for
            negative, [false] for positive. *)
    let sign inp =
      let (ch, inp1) = look inp in
      if ch = '-'
      then (Some(true), accept inp1)
      else if (ch = '+') || (is_digit 0 9 ch)
      then (Some(false), inp1)
      else (None, inp1)

    (** [integer inp]: Match an integer conforming to the C specification *)
    type integer_size = Word | Long | LongLong
    type integer_data =
      {
        size: integer_size;
        signed: bool;
        base: int;
        negate: bool;
        digits: (int)list;
      }
    let mk_int sz sn b n ds =
      { size = sz; signed = sn; base = b; negate = n; digits = ds }
    let mk_default_int () = mk_int Word true 10 false []

    let set_int_size data sz = {data with size = sz}
    let set_int_signed data sn = {data with signed = sn}
    let set_int_base data b = {data with base = b}
    let set_int_negate data n = {data with negate = n}
    let set_int_digits data ds = {data with digits = ds}

    let integer_suffix_one prev intrecord inp =
      let exclude = if prev = None then [] else [from prev] in
      let (ch1, inp0) = look inp in
      let lch1 = Char.lowercase_ascii ch1 in
      if (is_in exclude lch1) || not (is_in ['u'; 'l'] lch1)
      then (None, inp0)
      else
        begin
          let inp1 = accept inp0 in
          if lch1 = 'u'
          then (Some('u', set_int_signed intrecord false), inp1)
          else (* lch1 = 'l' *)
            begin
              let (ch2, inp2) = look inp1 in
              let lch2 = Char.lowercase_ascii ch2 in
              match lch2 with
              | 'l' -> (* 'll' *)
                 (Some('l', set_int_size intrecord LongLong),
                  accept inp2)
              | _ -> (* 'l' *)
                 (Some('l', set_int_size intrecord Long), inp2)
            end
        end

    (** Match: 'u', 'l', 'll', 'ul', 'ull', 'lu', 'llu' *)
    let integer_suffix intdata inp =
      ((integer_suffix_one None intdata)
       +>
         (fun (c, d) ->
           (opt ((integer_suffix_one (Some(c)) d) >> snd) d))) inp

    let integer inp0 =
      let intdata = mk_default_int () in
      let integer_suffix_opt idata inp =
        opt (integer_suffix idata) idata inp in
      let decimal_literal idata inp1 =
        let number_1 inp = digit 1 9 inp
        and number_2 idata n inp =
          (opt (digits 0 9) []
           >> (fun ns -> set_int_digits idata (n::ns)))
            inp
        in
        ((number_1 +> (number_2 idata))) inp1
      and hexadecimal_literal (idata: integer_data) inp1 =
        let hexnumber idata inp =
          (hexdigits >> set_int_digits idata) inp
        in
        (((string false "0x") >> (fun _ -> idata))
         +> (fun data ->
           ((hexnumber data) >> (fun d -> set_int_base d 16))))
          inp1
      and octal_literal (idata: integer_data) inp1 =
        let octalnumber idata inp =
          (digits 0 7 >> set_int_digits idata) inp
        in
        (((string false "0") >> (fun _ -> idata))
         +>
           (fun data ->
             ((octalnumber data) >> (fun d -> set_int_base d 8))))
          inp1
      in
      (sign +>
         (fun sn ->
           let idata = set_int_negate intdata sn in
           alt
             [
               decimal_literal idata;
               hexadecimal_literal idata;
               octal_literal idata
             ]
           +> integer_suffix_opt)) inp0

    (** Identifiers *)
    let is_alpha ch =
      let a_code = Char.code 'a'
      and z_code = Char.code 'z'
      and ch_code = Char.code (Char.lowercase_ascii ch)
      in
      a_code <= ch_code && ch_code <= z_code

    (** [st] must be sorted with the least element first *)
    let in_set st ch =
      let rec in_aux l =
        match l with
        | (x::xs) ->
           if ch = x then true
           else if x < ch then in_aux xs
           else false
        | _ -> false
      in
      in_aux st


    let identifier initialp restp inp =
      let pred idx ch =
        if idx = 0
        then initialp ch
        else restp ch
      and maker (lst: (char)list) =
        (String.concat "" (List.map (String.make 1) lst))
      in
      ((taker_nth pred) >> maker) inp

  end
