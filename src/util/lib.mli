(* Simple utility functions *)

val iteri : (int -> 'a array -> 'b) -> 'a array -> unit 
val list_string : ('a -> string) -> string -> 'a list -> string 

val take : int * 'a list -> 'a list 
val drop : int * 'a list -> 'a list 
val delete_nth : int -> 'a list -> 'a list
val replace_nth : int -> 'a list -> 'a -> 'a list
val replace : 'a -> 'b -> ('a*'b) list -> ('a*'b) list
val wrap_around: 'a list -> int -> int
val get_nth: 'a list -> int -> 'a
val splice_nth : int -> 'a list -> 'a list -> 'a list
val move_right : 'a list * 'a list -> 'a list * 'a list 
val move_left : 'a list * 'a list -> 'a list * 'a list 
(* val ($) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b*)
val index: ('a -> bool) -> 'a list -> int
val filter: ('a -> bool) -> 'a list -> 'a list
val assocp: ('a -> 'a -> bool) -> 'a -> ('a* 'b) list -> 'b
val assoc: ('a -> bool ) -> 'a list -> 'a

type ('a, 'b) substype = ('a, 'b)Hashtbl.t
val empty_env : unit -> ('a, 'b) Hashtbl.t 
val env_size : int -> ('a, 'b) Hashtbl.t 
val find : 'a -> ('a, 'b) Hashtbl.t -> 'b 
val bind_env : 'a -> 'b -> ('a, 'b) Hashtbl.t -> unit 
val bind : 'a -> 'b -> ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t 
val add : 'a -> 'b -> ('a, 'b) Hashtbl.t -> 'b 
val chase : ('a -> bool) -> 'a -> ('a, 'a) Hashtbl.t -> 'a
val fullchase : ('a -> bool) -> 'a -> ('a, 'a) Hashtbl.t -> 'a 
val member : 'a -> ('a, 'b)Hashtbl.t -> bool
val remove: 'a -> ('a, 'b)Hashtbl.t -> unit
val remove_dups: 'a list -> 'a list

val find_char : char -> int -> string -> int 

(* chop_at c str:
   cut str into pair of strings (x, y)
   with x the substring before char c, y the substring after c
   if no char c in str, then x is the empty string
*)

val chop_at : char -> string -> string * string

(* int_to_name i: convert i to type name
   0 -> a
   25-> z
   26 -> a1
*)
val int_to_name: int -> string

(* 
   Named Lists 
   lists in which elements are named and data can be
   added by relative position

   Use association list functions for operations other than add.
*)

type ('a)named_list = (string * 'a) list

type position = 
    First | Last | Before of string | After of string

(* named_add l p n x: 
   add (n, x) to named list l at position p 
*)

val named_add: 
    ('a)named_list->position 
      -> string -> 'a -> ('a) named_list


(* 
   [get_option x default] 
   if [x] is [Some(y)] then return [y]
   if [x] is [None] then return [default]
*)
val get_option : 'a option -> 'a -> 'a

val set_int_option : int -> int option
val get_int_option : int option -> int
val compare_int_option: int option -> int -> bool

(** [apply_option f a d]
   
   Apply to [f] to option [a].
   if [a] is [Some i] then return [f i] else return [d].
*)
val apply_option: ('a -> 'b) -> 'a option -> 'b -> 'b

(** [date]: used to ensure dependencies among theory files *)
val date: unit -> float

(* 
   [nice_date f]
   return date [f] in form [(year, month, day, hour, min)]
*)
val nice_date: float -> (int * int * int * int * int)

(**
   [get_one l e]: Get first element of list [l].
   raise exception [e] if length of [l] is empty.

   [get_two l e]: Get first two elements of list [l].
   raise exception [e] if length of [l] < 2.
*)
val get_one : 'a list -> exn -> 'a
val get_two : 'a list -> exn -> ('a * 'a)


val split_at : int -> 'a list -> ('a list * 'a list)

(**
   [rotate_left n l]: Rotate list [l] [n] places left 

   [rotate_right n l]: Rotate list [l] [n] places right 
*)
val rotate_left : int -> 'a list -> 'a list
val rotate_right : int -> 'a list -> 'a list


(** 
   [apply_nth n f l d]: Apply [f] to [n]th element of list.
   If list [l] is empty, return [d].
*)
val apply_nth : int -> ('a -> 'b) -> 'a list -> 'b -> 'b
