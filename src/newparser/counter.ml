(* counter: keep track of how many instances of a thing there are *)

module Counter=
struct

(* counter: list of items and the number of times they've been added *)

  type ('a)t=('a * int) list


(* construct, recogniser *)

   let empty () = []

   let is_empty l = match l with [] -> true | _ -> false

(* add x to list lst:
   if x is in lst, increment the count,
   other wise add x to lst, set count to 1
*)

  let add x lst=
    let rec add_aux ls=
      match ls with 
	[] -> [(x, 1)]
      |	(s, nm)::xs -> 
	  if s=x
	  then (s, nm+1)::xs
	  else 
	    if s<x
	    then (x, 1)::(s, nm)::xs
	    else (s, nm)::(add_aux xs)
    in 
    add_aux lst



(* remove x from list lst:
   if x is not in lst, do nothing

   if x is in lst, decrement the count,
   if new count is 0, remove x from the list
*)

  let remove x lst=
    let rec remove_aux ls=
      match ls with 
	[] -> []
      |	(s, nm)::xs -> 
	  if s=x 
	  then 
	    (if nm=1 then xs
	    else (s, nm-1)::xs)
	  else 
	    if s<x 
	    then (s, nm)::xs
	    else (s, nm)::(remove_aux xs)
    in 
    remove_aux lst

(* find x in list lst:
   if x is not in lst, raise Not_found

   if x is in lst, return the size
*)

  let find x lst=
    let rec find_aux ls =
      match ls with 
	[] -> raise Not_found
      | (s, nm) :: xs ->
	  if(s=x)
	  then 
	    nm
	  else
	    if(s>x)
	    then 
	      raise Not_found
	    else 
	      find_aux xs
    in 
    find_aux lst

end
