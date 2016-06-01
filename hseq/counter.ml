(*----
  Name: counter.ml 
  Copyright M Wahab 2005-2014
  Author: M Wahab  <mwb.cde@gmail.com>

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

(** counter: keep track of how many instances of an item there are
    Order is largest item first.  *)

(* counter: list of items and the number of times they've been added *)
type ('a)t = ('a * int) list

(* Constructor, recogniser. *)
let empty () = []
let is_empty l =
  match l with
  | [] -> true
  | _ -> false

(* add x to list lst:
   if x is in lst, increment the count,
   other wise add x to lst, set count to 1
*)
let add x lst =
  let rec add_aux ls =
    match ls with 
    | [] -> [(x, 1)]
    |	(s, nm)::xs -> 
      if s = x
      then (s, nm + 1)::xs
      else 
	if s > x
	then (s, nm)::(add_aux xs)
	else (x, 1)::(s, nm)::xs
  in 
  add_aux lst

(* [update x lst]: if x is in lst, increment the count and return
   the previous value with the new list other wise add x to lst, set
   count to 1, return 0 with the new list *)
let update x lst =
  let rec update_aux ls rst =
    match ls with 
    | [] -> (0, List.rev_append rst [(x, 1)])
    |	(s, nm)::xs -> 
      if s = x
      then (nm, (s, nm + 1)::xs)
      else 
	if s > x
	then update_aux xs ((s, nm)::rst)
	else (0, (List.rev_append rst ((x, 1)::(s, nm)::xs)))
  in 
  update_aux lst []


(* remove x from list lst: if x is not in lst, do nothing

   if x is in lst, decrement the count,
   if new count is 0, remove x from the list
*)
let remove x lst =
  let rec remove_aux ls =
    match ls with 
    | [] -> []
    |	(s, nm)::xs -> 
      if s = x 
      then 
	if nm = 1
        then xs
	else (s, nm-1)::xs
      else 
	if s > x 
	then (s, nm)::(remove_aux xs)
	else (s, nm)::xs
  in 
  remove_aux lst

(* find x in list lst: if x is not in lst, raise Not_found

   if x is in lst, return the size
*)
let find x lst =
  let rec find_aux ls =
    match ls with 
    | [] -> raise Not_found
    | (s, nm)::xs ->
      if s = x
      then nm
      else
	if s > x
	then find_aux xs
	else raise Not_found
  in 
  find_aux lst

(* [find_after x lst]: if x is not in lst, raise Not_found if x is
   in lst, return the next in the list if x is last, return None *)

let find_after x lst =
  let rec find_aux ls =
    match ls with 
    | [] -> raise Not_found
    | (s, _)::[] -> 
      if x = s
      then None 
      else raise Not_found
    | (s1, _)::(s2, x2)::xs ->
      if s1 = x 
      then Some(s2)
      else
	if s1 < x
	then raise Not_found
	else find_aux ((s2, x2)::xs)
  in 
  find_aux lst

(* [find_before x lst]: if x is not in lst, raise Not_found if x is in
   lst, return the previous in the list if x is first, return None
*)
let find_before x lst =
  let rec find_aux p ls =
    match ls with 
    | [] -> raise Not_found
    | (s, _)::xs ->
      if s = x
      then Some(p)
      else
	if s < x
	then raise Not_found
	else find_aux s xs
  in 
  match lst with
  | [] -> raise Not_found
  | (s, _)::[] -> 
    if x = s
    then None 
    else raise Not_found
  | (s, _)::xs -> find_aux s xs
