(* theories: a record of identifier and type definitions and declarations,
   their theorems and the parent theories *)

(*
    exception Error of string
*)

(* identifier records *)

    type id_record =
      { typ: Gtypes.gtype;
        def: Logic.thm option;
        infix: bool;
        prec: int }
    type save_record =
      { sty: Gtypes.gtype;
        sdef: Logic.saved_thm option;
        sinfix: bool;
        sprec: int }

(* The type of theories *)
(* a theory has a name and zero or more parent theories *)
(* if a theory is protected then it cannot be extended *)

    type thy =
      { name: string;
	mutable protection: bool;
	mutable date: float;
        mutable parents: string list;
        axioms: (string, Logic.thm) Hashtbl.t;
        theorems: (string, Logic.thm) Hashtbl.t;
        defns: (string, id_record) Hashtbl.t;
      	typs: (string, Gtypes.typedef_record) Hashtbl.t;
      	mutable type_pps: (string * Corepp.pp_rec) list;
      	mutable id_pps: (string * Corepp.pp_rec) list }

(* make a theory and get its properties *)

    val mk_thy : string -> thy
    val get_name : thy -> string
    val get_parents: thy -> string list
    val add_parents: string list -> thy -> unit
    val get_protection : thy -> bool
    val get_date : thy -> float

(*
    val set_protection : thy -> unit
*)

(* add/remove PP record *)

val add_pp_rec: Basic.id_selector -> string -> Corepp.pp_rec
  -> thy -> unit
val get_pp_rec: Basic.id_selector -> string  -> thy  -> Corepp.pp_rec
val remove_pp_rec : Basic.id_selector -> string -> thy -> unit
val get_pplist: Basic.id_selector -> thy
  -> (Basic.fnident * Corepp.pp_rec) list

(* add/get/test components *)
    val add_axiom : string -> Logic.thm -> thy -> unit
    val add_thm : string -> Logic.thm -> thy -> unit
(*
    val add_type_rec : string -> Gtypes.typedef_record -> thy -> unit
*)
    val add_type_rec : Logic.cdefn -> thy -> unit

    val get_type_rec : string -> thy -> Gtypes.typedef_record 
    val get_defn_rec : string -> thy -> id_record
    val get_defn : string -> thy -> Logic.thm
    val get_id_type : string -> thy -> Gtypes.gtype
    val id_is_infix : string -> thy -> bool
    val get_id_prec : string -> thy -> int
    val id_exists : string -> thy -> bool


    val add_defn_rec :
      string -> Gtypes.gtype 
	-> Logic.thm option -> bool -> int -> thy -> unit
    val add_defn : string -> Gtypes.gtype -> Logic.thm -> thy -> unit
    val add_decln_rec : string  -> Gtypes.gtype -> int -> thy -> unit
    val get_axiom : string -> thy -> Logic.thm
    val get_theorem : string -> thy -> Logic.thm

    val to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
    val from_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
    val to_save : id_record -> save_record
    val from_save : save_record -> id_record

(* primitive input/output of theories *)
    val output_theory : out_channel -> thy -> unit
    val input_theory : in_channel -> thy

(* load a theory from disc *)
    val load_theory : string -> thy
    val save_theory: thy -> bool -> string -> unit

(* save theory to disc setting protection to prot *)
(* once protected, the protection can't be changed *)

val export_theory: out_channel -> thy-> bool -> unit
