(*----
 Name: lib.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* library functions *)

(* Array iteration *)

let iteri fn gt =
  let rec iteri_aux i =
    (fn i gt); if i>0 then iteri_aux (i-1) else ()
  in iteri_aux ((Array.length gt) -1);;

let rec list_string f sep x =
  match x with 
    [] -> ""
  | (b::[]) -> (f b)
  | (b::bs) -> (f b)^sep^(list_string f sep bs);;


let rec take x =
  match x with
    (0, bs) -> []
  | (n, []) -> []
  | (n, (b::bs)) -> b::(take(n-1, bs))

let rec drop x =
  match x with
    (0, bs) -> bs
  | (n, []) -> []
  | (n,  b::bs) -> (drop(n-1, bs))

let delete_nth i x =
  let j=if i-1<0 then 0 else (i-1)
  in 
  (take (j, x)@ (drop(j+1, x)))

let replace_nth i x y=
  let j=if i-1<0 then 0 else (i-1)
  in 
  (take (j, x)@ (y::(drop(j+1, x))))

let replace a b l=
  let rec rep ys=
    match ys with
      [] -> [(a, b)]
    | (x, y)::xs -> 
	if x=a
	then (a, b)::xs
	else (x, y)::(rep xs)
  in 
  rep l

let splice_nth i x y=
  let j=if i-1<0 then 0 else (i-1)
  in 
  (take (j, x)@ y@ (drop(j+1, x)))

let rec assoc p ls =
  match ls with
    [] -> raise Not_found
  | (y::ys)  -> if p y then y else assoc p ls

let rec assocp p x ls = 
  match ls with
    [] -> raise Not_found
  | (y::ys) -> if (p x (fst y)) then (snd y) else assocp p x ys

let rec filter p ls =
  match ls with
    [] -> []
  | y::ys -> if (p y) then filter p ys else (y::filter p ys)

(* get nth with wraparound to 0 *)

let wrap_around x i  =
  match x with 
    [] -> raise (Failure "get_nth: empty list")
  | _ ->   i mod (List.length x)

let get_nth x i  =
  match x with 
    [] -> raise (Failure "get_nth: empty list")
  | _ ->   if (List.length x) > i+1 then List.nth x i else List.nth x 0

let rec move_right al =
  match al with
    ([], []) -> al
  | (bl, []) -> move_right ([], List.rev bl)
  | (bl, c::cl) -> (c::bl, cl)

let rec move_left al =
  match al with
    ([], []) -> al
  | ([], cl) -> (move_left (List.rev cl, []))
  | (b::bl, cl) -> (bl, b::cl)


(*
let ($) x y = fun a -> x (y a)
*)

let index p xs =
  let rec index_aux xs i =
    match xs with 
      [] -> raise Not_found
    | y::ys -> if (p y) then i else index_aux ys (i+1)
  in index_aux xs 0


(* substitution types *)

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


(* remove duplicates from a list *)

let remove_dups ls =
  let cache = Hashtbl.create (List.length ls)
  in 
  let rec remove_aux xs rs =
    match xs with
      [] -> List.rev rs
    | (y::ys) -> 
	if member y cache 
    	then remove_aux ys rs
	else ((Hashtbl.add cache y None); remove_aux ys (y::rs))
  in remove_aux ls []

(* string functions *)

let find_char c max x=
  let rec find_aux i =
    if i >= max 
    then max
    else 
      (if x.[i]=c 
      then i
      else find_aux (i+1))
  in find_aux 0
      

let chop_at c x =
  let max = String.length x
  in let indx= find_char c max x
  in 
  if indx < max 
  then ((String.sub x 0 indx), (String.sub x (indx+1) (((max -1)- indx))))
  else ("", x)

let int_to_name i =
  let numchars=26
  and codea=int_of_char 'a'
  in 
  let ch=i mod numchars
  and rm=i / numchars
  in 
  let ld= String.make 1 (char_of_int (codea+ch))
  in
  if (rm=0)
  then
    ld
  else 
    ld^(string_of_int rm)

(* Named Lists *)

type ('a)named_list = (string * 'a) list

type position = 
    First | Last | Before of string | After of string

(* 
   split_at s nl: split named list nl at s name s
   returning the list upto s and the list beginning with s
*)

let split_at s nl=
  let rec split_aux l r=
    match l with 
      [] -> (r, [])
    | (x, y)::ls -> 
	if(x=s) then (List.rev r, l)
	else split_aux ls ((x, y)::r)
  in split_aux nl []

let named_add l p n x =
  match p with 
    First -> (n, x)::l
  | Last -> List.rev ((n, x)::(List.rev l))
  | Before s -> 
      let (lt, rt)=split_at s l
      in 
      List.rev_append (List.rev lt) ((n, x)::rt)
  | After s -> 
      let (lt, rt)=split_at s l
      in 
      let nrt=
	(match rt with
	  [] ->  [(n, x)]
	| d::rst -> d::(n, x)::rst)
      in 
      List.rev_append (List.rev lt) rt

let get_option x d=
  match x with
    None -> d
  | Some(y) -> y

let set_option x data= x:=Some(data)
let dest_option ?err x = 
  match x with
    (Some x) -> x
  | _ -> 
      (match err with
	None -> failwith "dest_option"
      | (Some e) -> raise e)


let set_int_option i = Some(i)
let get_int_option x =
  match x with 
    None -> raise (Invalid_argument "get_int_option")
  | Some i -> i
let compare_int_option x n =
  match x with
    None -> false
  | Some i -> i=n

let apply_option f x d=
  match x with
    None -> d
  | Some i -> f i

let date ()= Unix.time()
let nice_date f= 
  let tm = Unix.localtime f
  in 
  (tm.Unix.tm_year + 1900, tm.Unix.tm_mon, tm.Unix.tm_mday, 
   tm.Unix.tm_hour, tm.Unix.tm_min)

let get_one ls err=
  match ls with
    x::_ -> x
  | _ -> raise err
	
let get_two ls err=
  match ls with
    x::y::_ -> (x, y)
  | _ -> raise err

let split_at num lst=
  let rec split_aux ctr rs ls =
    match rs with
      [] -> 
	if ctr = 0 
	then (List.rev ls, rs)
	else raise (Invalid_argument "split_at")
    | (x::xs) ->
	if ctr = 0 then (List.rev ls, rs)
	else 
	  split_aux (ctr-1) xs (x::ls)
  in 
  split_aux num lst []

let rotate_left num lst=
  if num = 0 then lst
  else 
    let size = List.length lst
    in 
    let n = 
      if num > size
      then (num mod size)
      else num
    in 
    let ls, rs = split_at n lst
    in 
    List.append rs ls


let rotate_right num lst=
  if num = 0 then lst
  else 
    let size = List.length lst
    in 
    let n = 
      if num > size
      then size - (num mod size)
      else size - num
    in 
    let ls, rs = split_at n lst
    in 
    List.append rs ls
      
let apply_nth n f l d =
  match l with 
    [] -> d
  | _ -> f (List.nth l n)

