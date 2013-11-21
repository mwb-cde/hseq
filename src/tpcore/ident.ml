(*----
  Name: ident.ml
  Copyright M Wahab 2005-2009, 2010
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

(*
 * Identifiers for functions and types
 *)

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

(*** Comparisons ***)

let equals x y = 
  ((x == y) or (Pervasives.compare x y) = 0)
let lessthan x y = 
  if (x == y) then false
  else 
    begin
      let cmp = Pervasives.compare (thy_of x) (thy_of y) in
      if (cmp < 0) then true
      else if cmp = 0
      then (Pervasives.compare (name_of x) (name_of y)) < 0
      else false
    end

(*** Utility functions ***)

let string_of n =
  if is_short n
  then name_of n
  else (thy_of n)^"."^(name_of n)

(* [('a)tree]: Balanced trees indexed by identifiers *)
module IdentTreeData =
struct
  type key = t
  let equals x y = ((x == y) or (Pervasives.compare x y) = 0)
  let lessthan x y = lessthan x y
end
module Tree = Treekit.BTree(IdentTreeData)
type ('a)tree = ('a) Tree.t
