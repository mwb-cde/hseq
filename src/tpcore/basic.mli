(* pre-logical constants *)

(* function and type identifiers *)
type thy_id = string
type fnident = (thy_id* string)

val null_thy: thy_id
val thy_of_id : fnident -> thy_id
val name: fnident -> string
val mklong: string -> string -> fnident
val mkname: string -> fnident
val string_fnid: fnident -> string
val dest_fnid: fnident -> (string * string)

type id_selector = bool
val fn_id: id_selector
val type_id: id_selector

(* primitive logical constructs *)
    type conns_ty = | Not | And | Or | Implies | Iff | Equal
    type quant_ty = | All | Ex | Lambda
    type const_ty =  
	Null_const of int (* needed to satisfy conditions in Dequals *)
(*       | Cnum of int  *)
      |	Cnum of Num.num    (* big numbers *)
      | Cbool of bool
    type fns = | Name of fnident

(* ordering on constants *)
val const_lt: const_ty -> const_ty -> bool
val const_leq: const_ty -> const_ty -> bool

(* precedence of constructs and quantifiers *)
    val prec_con : conns_ty -> int
    val prec_qnt : quant_ty -> int

    val conns_string : conns_ty -> string
    val connc_string : conns_ty -> string list -> string
    val quant_string : quant_ty -> string
    val fns_string : fns -> string

(* primitive types *)

    type base_typ = | Bool | Num | Ind
    type typ_const = | Func | Defined of fnident
    val string_btype : base_typ -> string
    val string_tconst : typ_const -> string list -> string
    val string_const: const_ty -> string

(*     val std_prec : string -> int *)

(* date: used to ensure dependencies among theory files *)

val date: unit -> float
