(* 
   Tags: 
   unique identifiers with which to identify
   sequents in a goal and formulas in a sequent
 *)
type t=string ref

let named x = ref x
let name x = !x

let null=ref ""
let create() = ref ""

let equal x y = x==y
