(* Balanced Binary Trees from Bird & Wadler *)

type tree=
    Nil
  | Br of string * tree * tree * int

let is_empty tr = 
  match tr with 
    Nil -> true
  | _ -> false

let depth tr = 
  match tr with
    Nil -> 0
  | Br(_, _, _, n) -> n

let incr tr x=
  match tr with
    Nil -> tr
  | Br(d, l, r, n) -> Br(d, l, r, n+x)

let decr tr x=
  match tr with
    Nil -> tr
  | Br(d, l, r, n) -> Br(d, l, r, n-x)

let slope tr = 
  match tr with
    Nil -> 0
  | Br(_, l, r, sl) -> (depth l)-(depth r)

let rotr tr = 
  match tr with
    Br(d, Br(dl, ll, rl, nl), r, n) ->
      Br(dl, ll, Br(d, rl, r, nl), n)
  | _ -> tr

let rotl tr=
  match tr with 
    Br(d, l, Br(dr, lr, rr, nr), n)->
      Br(dr, Br(d, l, lr, nr), rr, n)
  | _ -> tr

let shiftr tr = 
  match tr with
    Nil -> tr
  | Br(d, l, r, n) ->
      let sl = slope l
      in 
      if sl=(-1)
      then rotr (Br(d, (rotl l), r, n))
      else rotr tr

let shiftl tr = 
  match tr with
    Nil -> tr
  | Br(d, l, r, n) ->
      let sl = slope r
      in 
      if sl=(-1)
      then rotl (Br(d, l, (rotr r), n))
      else rotl tr


let rebal t = 
  let sl=slope t
  in 
  if sl=2 then shiftr t
  else 
    if sl=(-2) then shiftl t
    else t

let rec insert tr x =
  match tr with
    Nil -> Br(x, Nil, Nil, 1)
  | Br(y, l, r, n) ->
      let nbr =
	if x<y 
	then Br(y, insert l x, r, n+1)
	else 
	  if x>y 
	  then Br(y, l, insert r x, n+1)
	  else 
	    Br(x, tr, Nil, n+1)
      in 
      rebal nbr

let rec split tr=
  match tr with
    Nil -> raise (Invalid_argument "split")
  | Br(d, l, Nil, _) -> (d, l)
  | Br(d, l, r, n) -> 
      let y, t= split r
      in (y, Br(d, l, t, n))


let join t1 t2=
  match t1 with
    Nil -> t2
  | Br(_, _, _, n) ->
      let d, t=split t1
      in Br(d, t, t2, (max (depth t) (depth t2))+1)

let rec delete tr x =
  match tr with
    Nil -> raise Not_found
  | Br(y, l, r, n) ->
      let nbr =
	if x=y 
	then join l r
	else 
	  if x<y 
	  then Br(y, delete l x, r, n)
	  else Br(y, l, delete r x, n)
      in 
      rebal nbr
      


(* tests *)

let t1=insert Nil "a";;
let t2=insert t1 "a";;
let t3=insert t2 "c";;
let t4=insert t3 "b";;
let t4=insert t3 "b";;
