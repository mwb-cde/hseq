(*-----
   Name: basic.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(* Basic constants and data structures*)

(* function and type identifiers *)
type thy_id = string
type ident = (thy_id * string)

val null_thy: thy_id
val null_id: ident
val is_null_id: ident -> bool
val thy_of_id : ident -> thy_id
val name: ident -> string
val mk_long: string -> string -> ident
val mk_name: string -> ident
val is_short_id: ident -> bool
val string_fnid: ident -> string
val dest_fnid: ident -> (string * string)

type id_selector = bool
val fn_id: id_selector
val type_id: id_selector

(* primitive logical constructs *)
type quant_ty =  
    All | Ex | Lambda 
  | Meta  (* Meta: not used *)
type const_ty =  
    Null_const of int (* needed to satisfy conditions in Dequals *)
  |	Cnum of Num.num    (* big numbers *)
  | Cbool of bool

(* ordering on constants *)
val const_lt: const_ty -> const_ty -> bool
val const_leq: const_ty -> const_ty -> bool

val quant_string : quant_ty -> string

(* primitive types *)

type base_typ = | Bool | Num | Ind
type typ_const = Defined of ident

val string_btype : base_typ -> string
val string_tconst : typ_const -> string list -> string
val string_const: const_ty -> string


type ('idtyp, 'tfun, 'tcons) pre_typ =
    Var of 'idtyp
  | Constr of 'tfun * ('idtyp, 'tfun, 'tcons) pre_typ list
  | Base of 'tcons
  | WeakVar of 'idtyp

(** 
   WeekVar x: binds to anything except a variable.

   Isn't (usually) renamed.

   Is used in a sequent calculus when a variable type x can occur
   in more than one sequent. If x is bound in one sequent, it must
   be have that binding in every sequent in which it occurs. (Like 
   week types in ML)
 *)
(* representation of types *)
type gtype = 
    ((string ref, typ_const, base_typ)pre_typ)

(* Records for quantifiers *)
type q_type =
    { quant: quant_ty;
      qvar: string;
      qtyp: gtype}

(**
   [binders]: The type of binding quantifers.
   Primitive quanitifiers are All, Exists and Lambda 
*)
type binders

(** [term]: The representation of a term *)
type term =
    Id of ident* gtype   (* Identifiers *)
  | Bound of binders     (* Bound variables *)
  | Free of string * gtype      (* Free variables *)
  | App of term * term    (* Function application *)
  | Qnt of quant_ty * binders * term (* Binding terms *)
  | Const of const_ty     (* Constants *)
  | Typed of term * gtype  (* Typed terms *)

val mk_binding : quant_ty -> string -> gtype
  -> binders
val dest_binding : binders -> 
  (quant_ty * string * gtype)
val binder_kind: binders -> quant_ty
val binder_name: binders -> string
val binder_type: binders -> gtype
val binder_equality: binders -> binders -> bool

