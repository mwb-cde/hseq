(*----
  Copyright (c) 2024 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
  ----*)

(** ListSeq: Sequences that can be used lists.

    A ListSeq is able to be queried repeatedly, a new element is only
    generated once and remembered for subsequent queries.
*)


(** The type of list sequences *)
type ('a)t = (('a)list * ('a)Seq.t)

(* [rs] must be a reversed list *)
let rec push_front_reversed rs (lst, sq) =
  match rs with
  | (x::rest) -> push_front_reversed rest (x::lst, sq)
  | _ -> (lst, sq)

let push_front ts ls =
  push_front_reversed (List.rev ts) ls

(* Read-ahead the next [n] elements. *)
let read_ahead n ls =
  let rec get_aux ctr buff (lst, sq) =
    if ctr = 0
    then push_front_reversed buff (lst, sq)
    else
      (match lst with
       | (x::xs) -> get_aux (ctr - 1) (x::buff) (xs, sq)
       | _ ->
          (match Seq.uncons sq with
           | Some(y, rest) -> get_aux (ctr - 1) (y::buff) ([], rest)
           | _ -> push_front_reversed buff (lst, sq)))
  in
  get_aux n [] ls

let get_next ls =
  match ls with
  | ([], _) -> read_ahead 1 ls
  | _ -> ls

let is_empty ls =
  match ls with
  | ([], s) -> Seq.is_empty s
  | _ -> false

let uncons ls =
  let ns = get_next ls in
  match ns with
  | (x::xs, s) -> Some(x, (xs, s))
  | _ -> None

let first n ls =
  let rec first_aux ctr buff sq =
    if ctr = 0
    then (List.rev buff, sq)
    else
      let ns = uncons sq in
      (match ns with
       | Some(x, rest) -> first_aux (ctr - 1) (x::buff) rest
       | _ -> (List.rev buff, sq))
  in
  first_aux n [] ls

let look n ls =
  let rec get_front ctr buff lsq =
    if ctr = 0
    then (0, buff, lsq)
    else
      (match lsq with
       | (x::xs, nsq) -> get_front (ctr - 1) (x::buff) (xs, nsq)
       | _ -> (ctr, buff, lsq))
  in
  let (remaining, buff, nsq) = get_front n [] ls
  in
  let rsq = read_ahead remaining nsq in
  let (_, elems, nsq) = get_front remaining buff rsq in
  (List.rev elems, push_front_reversed elems nsq)

let drop n ls =
  let (_, ret) = first n ls in
  ret

(** [accept s] Drop the first element from [s] *)
let accept ls =
  let (_, ret) = first 1 ls in
  ret

let of_fun (step: 'b -> ('a * 'b)option) start =
  ([], Seq.unfold step start)

let of_string str =
  let step idx =
    if idx < (String.length str)
    then Some((String.get str idx), idx + 1)
    else None
  in
  ([], Seq.unfold step 0)

let of_list lst =
  let step l =
    match l with
    | (x::xs) -> Some(x, xs)
    | _ -> None
  in
  ([], Seq.unfold step lst)

