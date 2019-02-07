(*----
  Name: lib.ml
  Copyright Matthew Wahab 2005-2019
  Author: Matthew Wahab <mwb.cde@googlemail.com>

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

(** Library functions *)

(** {5 Operators} **)
module Ops =
struct

  (** Function composition **)
  let (<+) f g x = f (g x)

end

let rec list_string f sep x =
  match x with
    | [] -> ""
    | (b::[]) -> (f b)
    | (b::bs) -> (f b)^sep^(list_string f sep bs);;

let insert p k v l =
  let rec add l rest =
    match l with
      |	[] -> List.rev_append rest [(k, v)]
      | (a, b)::xs ->
        if p a k
        then add xs ((a, b)::rest)
        else List.rev_append rest ((k, v)::(a, b)::xs)
  in
  add l []

let replace a b l =
  let rec rep ys =
    match ys with
      |	[] -> [(a, b)]
      | (x, y)::xs ->
        if x = a
        then (a, b)::xs
        else (x, y)::(rep xs)
  in
  rep l

let rec assocp p ls =
  match ls with
    | [] -> raise Not_found
    | (a, b)::ys -> if (p a) then b else assocp p ys

let rec filter p ls =
  match ls with
    | [] -> []
    | y::ys -> if (p y) then filter p ys else (y::filter p ys)

let index p xs =
  let rec index_aux xs i =
    match xs with
      |	[] -> raise Not_found
      | y::ys -> if (p y) then i else index_aux ys (i + 1)
  in index_aux xs 0

(** {5 Substitutions} *)

type ('a,'b)table = ('a, 'b)Hashtbl.t

let empty_env() = Hashtbl.create 13
let env_size i = Hashtbl.create i

let find x env = Hashtbl.find env x
let bind_env t r env = (Hashtbl.remove env t; Hashtbl.add env t r)
let bind t r env = (Hashtbl.remove env t; Hashtbl.add env t r; env)
let add t r env =
  try (Hashtbl.find env t)
  with Not_found -> ((Hashtbl.add env t r); r)
let member t env = try (Hashtbl.find env t; true) with Not_found -> false
let remove t env = Hashtbl.remove env t

(* Remove duplicates from a list *)
let remove_dups ls =
  let cache = Hashtbl.create (List.length ls)
  in
  let rec remove_aux xs rs =
    match xs with
      |	[] -> List.rev rs
      | (y::ys) ->
        if member y cache
        then remove_aux ys rs
        else (Hashtbl.add cache y None;
              remove_aux ys (y::rs))
  in remove_aux ls []

(*
 * String functions
 *)

let int_to_name i =
  let numchars = 26
  and codea = int_of_char 'a'
  in
  let ch = i mod numchars
  and rm = i / numchars
  in
  let ld = String.make 1 (char_of_int (codea+ch))
  in
  if (rm = 0)
  then
    ld
  else
    ld^(string_of_int rm)

(* Named Lists *)
type ('a, 'b)named_list = ('a * 'b) list

(* Position markers *)
type ('a)position =
    First | Last | Before of 'a | After of 'a | Level of 'a

(**
    [split_at s nl]: split named list nl at s name s
    returning the list upto s and the list beginning with s
*)
let split_at_name s nl =
  let rec split_aux l r =
    match l with
      | [] -> (r, [])
      | (x, y)::ls ->
        if (x = s)
        then (List.rev r, l)
        else split_aux ls ((x, y)::r)
  in split_aux nl []

let named_add l p n x =
  match p with
    | First -> (n, x)::l
    | Last -> List.rev ((n, x)::(List.rev l))
    | Before s ->
      let (lt, rt) = split_at_name s l
      in
      List.rev_append (List.rev lt) ((n, x)::rt)
    | After s ->
      let (lt, rt) = split_at_name s l
      in
      let nrt =
        (match rt with
          | [] ->  [(n, x)]
          | d::rst -> d::(n, x)::rst)
      in
      List.rev_append (List.rev lt) nrt
    | Level s ->
      let (lt, rt) = split_at_name s l
      in
      List.rev_append (List.rev lt) ((n, x)::rt)

let from_option a b =
  match a with
    | Some(x) -> x
    | None -> b

let from_some a =
  match a with
  | Some(x) -> x
  | _ -> raise (Invalid_argument "fromSome")

let apply_option f x d =
  match x with
    | None -> d
    | Some i -> f i

let dest_option err x =
  if x = None
  then raise err
  else from_some x

let set_int_option i = Some(i)
let get_int_option x =
  match x with
    | None -> raise (Invalid_argument "get_int_option")
    | Some i -> i
let compare_int_option x n =
  match x with
    | None -> false
    | Some i -> i = n
let dec_int_option x =
  match x with
    | None -> x
    | Some i -> Some (i - 1)

let date () = Unix.time()
let nice_date f =
  let tm = Unix.localtime f
  in
  (tm.Unix.tm_year + 1900, tm.Unix.tm_mon, tm.Unix.tm_mday,
   tm.Unix.tm_hour, tm.Unix.tm_min)

let get_one ls err =
  match ls with
    | x::_ -> x
    | _ -> raise err

let get_two ls err =
  match ls with
    | x::y::_ -> (x, y)
    | _ -> raise err

(**
   [full_split_at_index i x]: Split [x] into [(l, c, r)] so that
   [x = List.revappend x (c::r)] and [c] is the [i]th element of [x]
   (counting from [0]).

   @raise Not_found if [i] > = [length x].
*)
let full_split_at_index i x =
  let rec split_aux ctr l rst =
    match l with
      |	[] -> raise Not_found
      | (y::ys) ->
        if (ctr = 0) then (rst, y, ys)
        else split_aux (ctr - 1) ys (y::rst)
  in
  split_aux (abs i) x []

(**
   [full_split_at p x]:
   Split [x] into [(l, c, r)] so that [x = List.revappend x (c::r)]
   and [c] is the first element of [x] such that [p x] is true.

   @raise Not_found if [p] is false for all elements of x.
*)
let full_split_at p x =
  let rec split_aux l rst =
    match l with
      |	[] -> raise Not_found
      | (y::ys) ->
        if (p y) then (rst, y, ys)
        else split_aux ys (y::rst)
  in split_aux  x []

let split_at_index num lst =
  let rec split_aux ctr rs ls =
    match rs with
      | [] ->
        if ctr = 0
        then (List.rev ls, rs)
        else raise (Invalid_argument "split_at")
      | (x::xs) ->
        if ctr = 0 then (List.rev ls, rs)
        else
          split_aux (ctr-1) xs (x::ls)
  in
  split_aux num lst []

let rotate_left num lst =
  if num = 0 then lst
  else
    let size = List.length lst
    in
    let n =
      if num > size
      then (num mod size)
      else num
    in
    let (ls, rs) = split_at_index n lst
    in
    List.append rs ls

let rotate_right num lst =
  if num = 0 then lst
  else
    let size = List.length lst
    in
    let n =
      if num > size
      then size - (num mod size)
      else size - num
    in
    let (ls, rs) = split_at_index n lst
    in
    List.append rs ls

let apply_nth n f l d =
  match l with
    | [] -> d
    | _ -> f (List.nth l n)

let map_find f lst =
  let rec map_aux l rslt =
    match l with
      |	[] -> List.rev rslt
      | (x::xs) ->
        let rslt1 =
          try ((f x)::rslt)
          with Not_found -> rslt
        in map_aux xs rslt1
  in
  map_aux lst []

let try_find f p =
  try (Some (f p))
  with Not_found -> None

let try_app f p =
  try (Some (f p))
  with _ -> None

let find_first f lst =
  let rec find_aux ls =
    match ls with
      |	[] -> raise Not_found
      | x::xs ->
        (try f x
         with _ -> find_aux xs)
  in
  find_aux lst

let first p lst =
  let rec find_aux ls =
    match ls with
      |	[] -> raise Not_found
      | x::xs -> if p x then x else find_aux xs
  in
  find_aux lst

let rec apply_first lst x =
  match lst with
    | [] -> raise (Failure "apply_first")
    | f::ts ->
      try (f x)
      with _ -> apply_first ts x

let apply_flatten f lst =
  let rec app_aux xs rl =
    match xs with
      |	[] -> List.rev rl
      | (y::ys) -> app_aux ys (List.rev_append (f y) rl)
  in
  app_aux lst []

let apply_split f lst =
  let rec app_aux xs (bl, cl)  =
    match xs with
      |	[] -> (List.rev bl, List.rev cl)
      | (y::ys) ->
        let (b, c) = f y
        in
        app_aux ys (b::bl, c::cl)
  in
  app_aux lst ([], [])

(*
 * Sets of strings
 *)

let string_compare x y =
  if x == y
  then 0
  else Pervasives.compare x y

module StringSet =
  Set.Make
    (struct
      type t = string
      let compare = string_compare
     end)

(**
   [stringify str]: Make [str] suitable for passing to OCaml on the
   command line.  Escapes the string using [String.escaped] then
   replaces ' ' with '\ '.
*)
let stringify (arg: string) =
  let str = String.escaped arg in
  let str_len = String.length str
  in
  let rec stringify_aux first pos (rslt: (string)list) =
    if pos >= str_len
    then
      begin
        if pos = first
        then rslt
        else
          let len =pos - first in
          (String.sub str first len)::rslt
      end
    else
      begin
        let chr = str.[pos]
        in
        if (chr = ' ')
        then
          let len = pos - first in
          let rslt0 = (String.sub str first len)::rslt in
          let rslt1 = "\\ "::rslt0
          in
          stringify_aux (pos + 1) (pos + 1) rslt1
        else stringify_aux first (pos + 1) rslt
      end
  in
  let rlst = stringify_aux 0 0 [] in
  let lst = List.rev rlst
  in
  String.concat "" lst
