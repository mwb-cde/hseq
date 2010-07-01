(*----
  Name: lib.ml
  Copyright M Wahab 2005-2010
  Author: M Wahab  <mwb.cde@googlemail.com>

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

(* Array iteration *)
let iteri fn gt =
  let rec iteri_aux i =
    (fn i gt); 
    if i > 0 then iteri_aux (i - 1) else ()
  in iteri_aux ((Array.length gt) - 1);;

let rec list_string f sep x = 
  match x with 
    | [] -> ""
    | (b::[]) -> (f b)
    | (b::bs) -> (f b)^sep^(list_string f sep bs);;


let rec take x = 
  match x with
    | (0, bs) -> []
    | (n, []) -> []
    | (n, (b::bs)) -> b::(take (n - 1, bs))

let rec drop x = 
  match x with
    | (0, bs) -> bs
    | (n, []) -> []
    | (n,  b::bs) -> drop (n - 1, bs)

let delete_nth i x = 
  let j = if (i - 1) < 0 then 0 else (i - 1)
  in 
  take (j, x) @ (drop(j + 1, x))

let replace_nth i x y = 
  let j = if (i - 1) < 0 then 0 else (i - 1)
  in 
  take (j, x) @ (y::(drop (j + 1, x)))

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

let splice_nth i x y = 
  let j = if (i - 1) < 0 then 0 else (i - 1)
  in 
  (take (j, x) @ y @ (drop (j + 1, x)))

let rec assocp p ls = 
  match ls with
    | [] -> raise Not_found
    | (a, b)::ys -> if (p a) then b else assocp p ys

let rec filter p ls = 
  match ls with
    | [] -> []
    | y::ys -> if (p y) then filter p ys else (y::filter p ys)

(* Get nth with wraparound to [0]. *)
let wrap_around x i  = 
  match x with 
    | [] -> raise (Failure "get_nth: empty list")
    | _ ->   i mod (List.length x)

let get_nth x i  = 
  match x with 
    | [] -> raise (Failure "get_nth: empty list")
    | _ -> 
      if (List.length x) > (i + 1) 
      then List.nth x i 
      else List.nth x 0

let rec move_right al = 
  match al with
    | ([], []) -> al
    | (bl, []) -> move_right ([], List.rev bl)
    | (bl, c::cl) -> (c::bl, cl)

let rec move_left al = 
  match al with
    | ([], []) -> al
    | ([], cl) -> (move_left (List.rev cl, []))
    | (b::bl, cl) -> (bl, b::cl)


let index p xs = 
  let rec index_aux xs i = 
    match xs with 
      |	[] -> raise Not_found
      | y::ys -> if (p y) then i else index_aux ys (i + 1)
  in index_aux xs 0


(** {5 Substitutions} *)

type ('a,'b)substype = ('a, 'b)Hashtbl.t

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

let rec chase varp x env = 
  try 
    let t = Hashtbl.find env x
    in if (varp t) then (chase varp t env) else t
  with Not_found -> x

let fullchase varp x env = 
  let t1 = chase varp x env 
  in if varp t1 then x else t1

let table_to_list tbl = 
  let tmp = ref []
  in 
  (Hashtbl.iter (fun x y -> tmp := ((x, y)::!tmp)) tbl;
   !tmp)

let table_from_list xs = 
  let tenv = Hashtbl.create 1 
  in
  let rec from_list_aux ys = 
    match ys with
      |	[] -> tenv
      | (x, y)::xss -> Hashtbl.add tenv x y; from_list_aux xss
  in ignore(from_list_aux xs); tenv

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
let find_char c max x = 
  let rec find_aux i = 
    if i >= max 
    then max
    else 
      (if x.[i] = c 
       then i
       else find_aux (i + 1))
  in find_aux 0

let chop_at c x = 
  let max = String.length x
  in let indx = find_char c max x
     in 
     if indx < max 
     then (String.sub x 0 indx, 
           String.sub x (indx + 1) (((max - 1)- indx)))
     else ("", x)

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

let num_to_name (i:Num.num) = 
  let numchars = Num.num_of_int 26
  and codea = Num.num_of_int (int_of_char 'a')
  and zero = Num.num_of_int 0
  in 
  let ch = Num.mod_num i numchars
  and rm = Num.div_num i numchars
  in 
  let ld = 
    String.make 1 
      (char_of_int (Num.int_of_num (Num.add_num codea ch)))
  in
  if (Num.eq_num rm zero)
  then
    ld
  else 
    ld^(Num.string_of_num rm)

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

let get_option x d = 
  match x with
    | None -> d
    | Some(y) -> y

let set_option x data = x := Some(data)

let dest_option ?err x = 
  match x with
    | (Some x) -> x
    | _ -> 
      (match err with
	| None -> failwith "dest_option"
	| (Some e) -> raise e)


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

let apply_option f x d = 
  match x with
    | None -> d
    | Some i -> f i

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

let fold_map f a bs = 
  let rec fold_aux e ls rst = 
    match ls with 
      |	[] -> (e, List.rev rst)
      | (x::xs) -> 
	let (e1, c) = f e x
	in 
	fold_aux e1 xs (c::rst)
  in 
  fold_aux a bs []


let swap (a, b) = (b, a)

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

let extract p ls = 
  let rec extract_aux ys rs = 
    match ys with 
      |	[] -> raise Not_found
      | (x::xs) -> 
	if (p x)
	then (x, List.rev_append xs rs)
	else extract_aux xs (x::rs)
  in 
  let (x, bs) = extract_aux ls []
  in 
  (x, List.rev bs)

let least lt ls = 
  let rec ord_aux curr ys = 
    match ys with
      |	[] -> curr
      | (x::xs) -> 
	if (lt x curr) 
	then ord_aux x xs
	else ord_aux curr xs
  in 
  match ls with 
    | [] -> raise (Invalid_argument "least")
    | (f::fs) -> ord_aux f fs


let try_find f p = 
  try (Some (f p))
  with Not_found -> None

let try_app f p = 
  try (Some (f p))
  with _ -> None

let test_app f p = 
  try ignore (f p); true
  with _ -> false

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


(*
 * Lazy evaluation
 *)

type ('a)deferred_t = 
    Val of ('a * (unit -> 'a)) 
  | Fn of (unit -> 'a)

type ('a)deferred = ('a)deferred_t ref

let freeze fn = ref (Fn fn)

let thaw ?fresh var = 
  let mk_val f = 
    (let x = f()
     in 
     var := Val(x, f); x)
  in
  let pred x = 
    match fresh with 
      | None -> true
      | Some(p) -> p x
  in 
  match !var with
    | Fn fn -> mk_val fn
    | Val(x, fn) -> if pred x then x else mk_val fn

(**
   [stringify str]: Make [str] suitable for passing to OCaml on the
   command line.  Escapes the string using [String.escaped] then
   replaces ' ' with '\ '.
*)
let stringify str = 
  let rec stringify_aux str idx (len, rslt) = 
    if idx = 0 
    then (len, rslt)
    else
      let chr = String.get str (idx - 1)
      in 
      if (chr = ' ')
      then stringify_aux str (idx - 1) (len + 2, ('\\'::' '::rslt))
      else stringify_aux str (idx - 1) (len + 1 , (chr::rslt))
  in
  let implode str chars = 
    let len = String.length str
    in 
    let rec imp_aux idx lst = 
      if (idx < len)
      then 
	(match lst with 
	  | [] -> str
	  | (c::rst) -> 
	    (String.set str idx c;
	     imp_aux (idx+1) rst))
      else
	str
    in 
    imp_aux 0 chars
  in 
  let str1 = String.escaped str
  in 
  let strlen = String.length str1
  in 
  let (len, lst) = stringify_aux str1 strlen (0, [])
  in 
  implode (String.make len '#') lst


