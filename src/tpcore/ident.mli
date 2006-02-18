(*-----
   Name: ident.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2006
   ----*)

(** 
   Identifiers for functions and types 

   An identifier is made up of a theory identifier and a name. A {e
   short} identifier is an identifier without a theory identifier
   (equivalently, the theory identifier is null).
*)

type thy_id = string
(** The type of theory identifiers *)

type t = (thy_id * string)
(** 
   General, qualified identifiers. Made up of a theory identifier and
   name.
*)

val null_thy: thy_id
(** The empty theory identifier *)

val null: t
(** The empty identifier *)

val is_null: t -> bool
(** test for the empty identifier *)

val is_short: t -> bool
(** 
   [is_short i] is true if [i] is a short identifier (having an
   empty theory part). 
*)

(** {7 Constructors} *)

val mk_long: thy_id -> string -> t
(** 
   [mk_long t n] makes a long identifier with theory part [t] and name
   part [n].
*)

val mk_name: string -> t
(**
   Make an identifier with an empty theory part. These are called
   short identifiers.
*)

(** {7 Destructors} *)

val dest: t -> (thy_id * string)
(** 
   [dest id]: Destructor for identifiers. Returns [(th, n)] where [th]
   is the theory identifier and [n] is the name. 
*)

val thy_of : t -> thy_id
(** The theory identifier of long identifier [i]. *)

val name_of : t -> string
(** The name portion of identifier [i]. *)

(** {7 Utility functions} *)

val string_of: t -> string
(** String representation of identifier [i]. *)

