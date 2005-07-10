(*-----
   Name: basic.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(** Basic constants and data structures. *)

(** {5 Identifiers for functions and types} *)

type thy_id = string
(** The name of a theory *)

type ident = (thy_id * string)
(** 
   General, qualified identifiers.
   Made up of a theory identifier and name.
*)

val null_thy: thy_id
(** The empty theory identifier *)
val null_id: ident
(** The empty identifier *)
val is_null_id: ident -> bool
(** test for the empty identifier *)

val thy_of_id : ident -> thy_id
(** The theory identifier of long identifier [i]. *)
val name: ident -> string
(** The name portion of identifier [i]. *)
val mk_long: string -> string -> ident
(** 
   [mk_long t n] makes a long identifier with theory part [t] and name
   part [n].
*)
val mk_name: string -> ident
(**
   Make a long identifier with an empty theory part. These are called
   short identifiers.
*)
val dest_fnid: ident -> (string * string)
(** Destructor for [ident]. *)
val is_short_id: ident -> bool
(** 
   [is_short_id i] is true if [i] is a short identifier (having an
   empty theory part). 
*)
val string_fnid: ident -> string
(** String representation of identifier [i]. *)

(**
   [id_selector]: Choose whether to interpret an identifier as a
   function or type identifier. (Should be removed).
*)
type id_selector = bool
val fn_id: id_selector
val type_id: id_selector

(** {5 Base Representation of logic types} *)

type base_typ = Bool | Num | Ind 
(** 
   The built-in types (should be removed) 
*) 

type typ_const = Defined of ident
(** 
   Representation of user-defined type constructors
   (could merged into [pre_typ] 
*)

(** The base representation of types *)
type ('idtyp, 'tfun, 'tcons) pre_typ =
    Var of 'idtyp 
(** Type variables. *)
  | Constr of 'tfun * ('idtyp, 'tfun, 'tcons) pre_typ list
(** User defined type constructors. *)
  | Base of 'tcons
(** The built-in type constructors. *)
  | WeakVar of 'idtyp
(**
 Weak type variables. These bind to anything except a
   variable and aren't (normally) renamed. They are used in a sequent
   calculus when a variable type 'x can occur in more than one
   sequent. If 'x is bound in one sequent, then it must have that
   binding in every sequent in which it occurs. (Like weak types in
   ML).
*)

type gtype = ((string ref, typ_const, base_typ)pre_typ)
(** The actual representation of types *)

(** String representation of types *)
val string_btype : base_typ -> string
val string_tconst : typ_const -> string list -> string


(** {5 Base Representation of logic terms} *)

(** Built-in constants that can appear in terms  *)
type const_ty =  
    Cnum of Num.num    (* big numbers *)
  | Cbool of bool

val const_lt: const_ty -> const_ty -> bool
(** Less-than ordering on constants. *)
val const_leq: const_ty -> const_ty -> bool
(** Less-than-equal ordering on constants. *)
val string_const: const_ty -> string
(** String representation of a constant. *)

(** {7 Basis of quantified terms} *)

(** Quantifiers for terms. *)
type quant_ty =  
    All | Ex | Lambda 
  | Meta  (** Meta: not used *)

val quant_string : quant_ty -> string
(** The string representation of quantifiers *)

(** {7 Binders} *)

type binders
(** Associating bound variables with their binding term. *)
val mk_binding : quant_ty -> string -> gtype -> binders
(** 
   [mk_binding k n ty] makes a binder of kind [k], with name [n] and
   type [ty]. This binder will be distinct from any other under
   [binder_equality].
*)
val dest_binding : binders -> (quant_ty * string * gtype)
(** Destructor for binders. *)
val binder_kind: binders -> quant_ty
(** [binder_kind b]: The kind of binder binding variable [b]. *)
val binder_name: binders -> string
(** [binder_name b]: The name of bound variable [b]. *)
val binder_type: binders -> gtype
(** [binder_type b]: The type of bound variable [b]. *)
val binder_equality: binders -> binders -> bool
(** Equality of binders. *)

(** {7 Terms} *)

(** The representation of a term *)
type term =
    Id of ident* gtype   (** Identifiers *)
  | Bound of binders     (** Bound variables *)
  | Free of string * gtype  (** Free variables *)
  | App of term * term    (** Function application *)
  | Qnt of binders * term (** Binding terms *)
  | Const of const_ty     (** Constants *)
  | Typed of term * gtype  (** Typed terms *)

