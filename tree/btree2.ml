(* Balanced Binary Trees *)
(* simple implementation which may be wrong *)

type tree=
    Nil
  | Br of string * tree * tree * int

(* utility functions *)

let is_empty tr = 
  match tr with 
    Nil -> true
  | _ -> false

let left tr=
  match tr with
    Br(_, l, _, _) -> l
  | _ -> raise (Invalid_argument "left")

let right tr=
  match tr with
    Br(_, _, r, _) -> r
  | _ -> raise (Invalid_argument "right")

(* depth and slope *)

(* depth: larger of number of levels on left and right *)

let depth tr = 
  match tr with
    Nil -> 0
  | Br(_, _, _, d) -> d

let max_depth t1 t2=
  max (depth t1) (depth t2)

(* slope: difference between depth on left and right *)

let slope tr = 
  match tr with
    Nil -> 0
  | Br(_, l, r, _) -> (depth l)-(depth r)

(* rotation right:

   Br(x, 
      Br(x1, l1, r1, d1), 
      r, 
      d)
 -->
   Br(x1, 
      l1, 
      Br(x, r1, r, (max_depth r1 r)+1), 
      (depth l1)+1)
*)

let rotr tr = 
  match tr with
   Br(x, Br(x1, l1, r1, d1), r, d)
   ->
   let nbr=Br(x, r1, r, (max_depth r1 r)+1)
   in 
   Br(x1, l1, nbr, (max_depth l1 nbr)+1)
  | _ -> tr

let rec rotrn tr n =
  match n with 
    0 -> tr
  | _ -> rotrn (rotr tr) (n-1)

(* rotation left:

   Br(x,
      l, 
      Br(x1, l1, r1, d1), 
      d)
 -->
   Br(x1,
      Br(x, l, l1, (max_depth l l1)+1),
      r1
      (depth r1)+1)
*)

let rotl tr=
  match tr with 
   Br(x, l, Br(x1, l1, r1, d1), d) 
   ->
   let nbr=Br(x, l, l1, (max_depth l l1)+1)
   in 
   Br(x1, nbr, r1, (max_depth nbr r1)+1)
  | _ -> tr

let rec rotln tr n=
  match n with 
    0 -> tr
  | _ ->  rotln (rotl tr) (n-1)


let shiftr tr = 
  match tr with
    Nil -> tr
  | Br(d, l, r, n) ->
      let sl = slope l
      in 
      if sl<0
      then rotrn (Br(d, (rotl l), r, n)) (-sl)
      else rotrn tr sl

let shiftl tr = 
  match tr with
    Nil -> tr
  | Br(d, l, r, n) ->
      let sl = slope r
      in 
      if sl>0
      then rotln (Br(d, l, (rotr r), n)) sl
      else rotln tr (-sl)


(* rebal: rebalance a tree *)

let rebal t = 
  let sl=slope t
  in 
  if sl>1 then shiftr t
  else 
    if sl<(-1) then shiftl t
    else t

let rec insert tr x =
  match tr with
    Nil -> Br(x, Nil, Nil, 1)
  | Br(y, l, r, n) ->
      let nbr =
	if x<y 
	then 
	  let nlt =insert l x
	  in 
	  Br(y, nlt, r, (max_depth nlt r)+1)
	else 
	  if x>y 
	  then 
	    let nrt = insert r x
	    in 
	    Br(y, l, nrt, (max_depth l nrt)+1 )
	  else 
	    tr
      in 
      rebal nbr

let rec find tr x=
   match tr with
     Nil -> raise Not_found
   | Br(y, l, r, _) ->
       if x=y 
       then y
       else
	 if(x<y) 
	 then find l x
	 else find r x
   

(* deletion *)

  let rec add_rightmost tr t =
    match tr with
      Nil -> t
    | Br(y, l, Nil, d) ->
	rebal (Br(y, l, t, (max_depth l t)+1))
    | Br(y, l, r, d) ->
	Br(y, l, add_rightmost r t, d+(depth t))


let rec delete tr x=
  match tr with
    Nil -> Nil
  | Br(y, l, r, d) -> 
      if x<y 
      then 
	(let nlt = delete l x
	in 
	Br(y, nlt, r, max_depth nlt r))
      else
	if x>y
	then 
	  (let nrt = delete r x
	  in 
	  Br(y, l, nrt, max_depth nrt r))
	else 
	  rebal (add_rightmost l r)

(* tests *)

let t1=insert Nil "a";;
let t2=insert t1 "a";;
let t3=insert t2 "c";;
let t4=insert t3 "b";;
let t5=insert t4 "a";;
let t6=insert t5 "a1";;
let t7=insert t6 "b3";;
let t8=insert t7 "hg";;

let a1=insert Nil "e";;
let a2=insert a1 "d";;

let b1=insert Nil "g";;
let b2=insert b1 "h";;
