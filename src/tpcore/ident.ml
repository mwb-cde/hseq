(*-----
 Name: basic.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2006
----*)

(***
* Identifiers for functions and types
***)

(** The type of theory identifiers *)
type thy_id = string

type t = (thy_id * string)

let null_thy = ""

let null = ("", "")

let is_null x = x=null
let is_short (t, _) = t=null_thy

(*** Constructors ***)

let mk_long t n = (t, n)
let mk_name n = (null_thy, n)
let dest_fnid (t, n) = (t, n)

(*** Destructors ***)

let dest x = x
let thy_of (t, _) = t
let name_of (_, n) = n

(*** Utility functions ***)

let string_of n =
  if is_short n
  then name_of n
  else (thy_of n)^"."^(name_of n)

