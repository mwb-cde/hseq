(*-----
 Name: basic.mli
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

(* Basic constants and data structures*)

(* function and type identifiers *)
type thy_id = string

(* fnident renamed to indent *)
type ident = (thy_id* string)

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
    type conns_ty = | Not | And | Or | Implies | Iff | Equal
    type quant_ty =  
	All | Ex | Lambda 
      | Meta  (* Meta: used for skolem constants *)
    type const_ty =  
	Null_const of int (* needed to satisfy conditions in Dequals *)
      |	Cnum of Num.num    (* big numbers *)
      | Cbool of bool
    type fns = | Name of ident

(* ordering on constants *)
val const_lt: const_ty -> const_ty -> bool
val const_leq: const_ty -> const_ty -> bool

(* precedence of constructs and quantifiers *)
    val prec_con : conns_ty -> int
(*
    val prec_qnt : quant_ty -> int
*)

    val conns_string : conns_ty -> string
    val connc_string : conns_ty -> string list -> string
    val quant_string : quant_ty -> string
    val fns_string : fns -> string

(* primitive types *)

    type base_typ = | Bool | Num | Ind
    type typ_const = (* Func | *)  Defined of ident
    val string_btype : base_typ -> string
    val string_tconst : typ_const -> string list -> string
    val string_const: const_ty -> string

(*     val std_prec : string -> int *)


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
(* representation of types for storage on disk *)
(*
type stype = 
    ((string * int), typ_const, base_typ) pre_typ
*)

(* conversion to and from disk representation *)
(*
val from_save: stype -> gtype
val from_save_env : 
    ((string * int)* (string ref)) list ref
  -> stype -> gtype
val to_save: gtype -> stype
val to_save_env: (string ref* (string *int)) list ref 
  -> gtype -> stype
*)

(* Records for quantifiers *)
type q_type =
    { quant: quant_ty;
      qvar: string;
      qtyp: gtype}

(* the type of binding quantifers *)
(* primitive quanitifiers are All, Exists and Lambda *)

type binders

(* the representation of a term *)
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

(*
(* date: used to ensure dependencies among theory files *)

val date: unit -> float

(* 
   [nice_date f]
   return date [f] in form [(year, month, day, hour, min)]
*)
val nice_date: float -> (int * int * int * int * int)
*)


(* Pretty printer *)

(*
module PP :
    sig

(* pretty printer information records *)

      exception Error of string

      type fixity=Parserkit.Info.fixity
      val nonfix : Parserkit.Info.fixity
      val infix : Parserkit.Info.fixity
      val prefix : Parserkit.Info.fixity
      val suffix : Parserkit.Info.fixity

(* 
   Default precedence, fixity and associativity 
   for parsing and printing terms and types
*)
      val default_term_prec: int
      val default_term_assoc: Parserkit.Info.associativity
      val default_term_fixity: Parserkit.Info.fixity

      val default_type_prec: int
      val default_type_assoc: Parserkit.Info.associativity
      val default_type_fixity: Parserkit.Info.fixity

(* string representation of fixity (for printing) *)
      val assoc_to_string : Parserkit.Info.associativity -> string
      val fixity_to_string: Parserkit.Info.fixity -> string
(*
      val prec_of: pp_state -> id_selector -> ident ->  int
      val fixity_of: pp_state -> id_selector -> ident ->  fixity
*)
      val is_infix: fixity -> bool
      val is_prefix: fixity -> bool
      val is_suffix: fixity -> bool

(*  pretty printer information for function and type identifiers *)

      type record = 
	  {
	   prec: int; 
	   fixity: fixity;
	   repr: string option 
	  }

      type info = 
	{
	 term_info: (ident, record)Hashtbl.t;
	 type_info: (ident, record)Hashtbl.t
        }

      val mk_record :  int -> fixity -> string option -> record
      val empty_record : unit ->  record

(**
   [mk_info sz]:
   make an PP info store of size [sz].
   PP information is stored in two hashtables, one for terms, the other
   for types
*)
      val mk_info: int-> info

(** 
   [default_info_size]
   The size of the hashtables created by [empty_info].
   This is a reference and can be changed by assignment
*)
      val default_info_size: int ref
(**
   [empty_info]
   create a PP information store using the default size given
   by [default_info_size].
*)
      val empty_info: unit-> info

(*
      val mk_base_info: unit-> pp_state
*)

(* get/set/remove PP information *)

(**
   [get_term_info info id]
   get pretty printing information for identifer occuring in a term.
   @param info PP information.
   @param id identifier to look up.

   @return [(prec, fixity, repr)]
   where 
   [prec] is precedence
   [fixity] is fixity
   [repr] is representation to use (if any)

   @return [(default_term_prec, default_term_fixity, None)] if id is not found.
*)
      val get_term_info : info -> ident -> (int * fixity * string option)

(**
   [add_term_info info id prec fixity repr]
   add pretty printing information for identifer occuring in a term.
   @param info PP information.
   @param id identifier to add.
   @param prec precedence.
   @param fixity fixity.
   @param repr representation (if any).
*)
      val add_term_info : 
	  info -> ident -> int -> fixity 
	    -> string option -> unit

(**
   [add_term_record info id record]
   add pretty printing record for identifer occuring in a term.
   @param info PP information.
   @param id identifier to add.
   @param record PP record
*)
      val add_term_record : 
	  info -> ident -> record -> unit

(**
   [remove_term_info info id]
   remove pretty printing information for identifer occuring in a term.
   @param info PP information.
   @param id identifier to remove.
   @param prec precedence.
   @param fixity fixity.
   @param repr representation (if any).
*)
      val remove_term_info : info ->  ident -> unit

(**
   [get_type_info info id]
   get pretty printing information for identifer occuring in a type.
   @param info PP information.
   @param id identifier to look up.

   @return [(prec, fixity, repr)]
   where 
   [prec] is precedence
   [fixity] is fixity
   [repr] is representation to use (if any)
*)
      val get_type_info : info -> ident -> (int * fixity * string option)

(**
   [add_type_info info id prec fixity repr]
   add pretty printing information for identifer occuring in a type.
   @param info PP information.
   @param id identifier to add.
   @param prec precedence.
   @param fixity fixity.
   @param repr representation (if any).
*)
      val add_type_info : 
	  info -> ident -> int -> fixity -> string option -> unit

(**
   [add_type_record info id record]
   add pretty printing record for identifer occuring in a type.
   @param info PP information.
   @param id identifier to add.
   @param record PP record
*)
      val add_type_record:
	  info -> ident -> record -> unit

(**
   [remove_type_info info id]
   remove pretty printing information for identifer occuring in a type.
   @param info PP information.
   @param id identifier to remove.
   @param prec precedence.
   @param fixity fixity.
   @param repr representation (if any).
*)
      val remove_type_info : info -> ident -> unit


(* utility functions for printing *)

(**
   [list_print pr sep l]
   Print elements of list [l] using printer [pr]. 
   Printer [sep] prints the separator.
*)
      val list_print : ('a -> unit) -> (unit -> 'b) -> 'a list -> unit

(** 
   [print_bracket pr cpr br]
   print bracket [br] if priority [pr] is less than current 
   priority [cpr]
*)
      val print_bracket : int -> int -> string -> unit 

(** 
   [string_identifier id r]
   Convert an identifier to a string, using its PP representation if any.
*)
      val string_identifier : ident -> record -> string

(**
   [print_ident id]
   Print identifier [id] as is (without alternative representation)
*)
      val print_ident: ident -> unit

(**
   [print_identifier id repr]
   Print identifier [id] using representation [repr].
   If [repr] is [None] then just print the identifier.
*)
      val print_identifier: ident -> string option -> unit

    end
*)
