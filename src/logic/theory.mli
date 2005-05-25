(*-----
 Name: theory.mli
 Author: M Wahab <Mwahab@Users.Sourceforge.Net>
 Copyright M Wahab 2005
----*)


(* theories: a record of identifier and type definitions and declarations,
   their theorems and the parent theories *)

(**
   [type property]: Properties of theorems. Properties are exceptions
   ([property=exn]), to allow new properties to be declared. 

   [simp: property]: Mark as of interest to the simplifier.
*)
type property = string
val simp_property: property

(* identifier records *)

type id_record =
    {
     typ: Basic.gtype;
     def: Logic.thm option;
     dprops : property list
   }
type id_save_record =
    {
     sty: Basic.gtype;
     sdef: Logic.saved_thm option;
     sdprops: property list
   }

type thm_record =
    {
     thm: Logic.thm;
     props: property list
   }
type thm_save_record =
    {
     sthm: Logic.saved_thm;
     sprops: property list
   }

(**
   [thy] The type of theories 
   a theory has a name and zero or more parent theories 
   if a theory is protected then it cannot be extended 
 *)

type thy

(*
   [ithy] The type of theory information.
   Information stored in a theory.
   Used to directly query parts of a theory (such as for printing)
   without being able to alter the theory.
*)
type contents=
    {
     cname: string;
     cprotection: bool;
     cdate: float;
     cparents: string list;
     cfiles: string list;
     caxioms: (string * thm_record) list;
     ctheorems: (string * thm_record) list;
     cdefns: (string * id_record) list;
     ctyps: (string * Gtypes.typedef_record) list;
     ctype_pps: (string * Printer.record) list;
     cid_pps: (string * Printer.record) list 
   }

(* make a theory and get its properties *)

val mk_thy : string -> thy
val get_name : thy -> string
val get_parents: thy -> string list
val add_parents: string list -> thy -> unit
val get_protection : thy -> bool
val set_protection : thy -> unit
val get_date : thy -> float

val get_files: thy -> string list
val set_files: string list -> thy -> unit
val add_file: string -> thy -> unit
val remove_file: string -> thy -> unit

(* add/remove PP record *)

val add_pp_rec: 
    Basic.id_selector -> string -> Printer.record
      -> thy -> unit
val get_pp_rec: 
    Basic.id_selector -> string  -> thy -> Printer.record
val remove_pp_rec : Basic.id_selector -> string -> thy -> unit
val get_pplist: 
    Basic.id_selector -> thy
      -> (Basic.ident * Printer.record) list

(*
   val add_pp_rec: Basic.id_selector -> string -> Corepp.pp_rec
   -> thy -> unit
   val get_pp_rec: Basic.id_selector -> string  -> thy  -> Corepp.pp_rec
   val remove_pp_rec : Basic.id_selector -> string -> thy -> unit
   val get_pplist: Basic.id_selector -> thy
   -> (Basic.ident * Corepp.pp_rec) list
 *)

(* add/get/test components *)
val add_axiom : string -> Logic.thm -> property list -> thy -> unit
val add_thm : string -> Logic.thm -> property list -> thy -> unit

val add_type_rec : Logic.cdefn -> thy -> unit

val get_type_rec : string -> thy -> Gtypes.typedef_record 
val get_defn_rec : string -> thy -> id_record
val get_defn : string -> thy -> Logic.thm
val get_id_type : string -> thy -> Basic.gtype
val id_exists : string -> thy -> bool
val type_exists : string -> thy -> bool

val add_defn_rec :
    string -> Basic.gtype 
      -> Logic.thm option -> bool -> int 
	-> property list -> thy -> unit
val add_defn : 
    string -> Basic.gtype -> Logic.thm 
	-> property list
	  -> thy -> unit
val add_decln_rec : 
    string  -> Basic.gtype -> int -> property list -> thy -> unit

val get_axiom_rec : string -> thy -> thm_record
val get_theorem_rec : string -> thy -> thm_record

val get_axiom : string -> thy -> Logic.thm
val get_theorem : string -> thy -> Logic.thm

val set_defn_props: string -> property list -> thy -> unit
val set_theorem_props: string -> property list -> thy -> unit
val set_axiom_props: string -> property list -> thy -> unit

val to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val from_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
val to_save : id_record -> id_save_record
val from_save : id_save_record -> id_record

(* primitive input/output of theories *)
val output_theory : out_channel -> thy -> unit
val input_theory : in_channel -> thy

(* load a theory from disc *)
val load_theory : string -> thy
val save_theory: thy -> bool -> string -> unit

(**
   [end_theory thy prot]: Mark end of session with theory [thy].
   Update date of theory, set protection to [prot].
   Note that once set, theory protection can't be removed.
   Does nothing if theory [thy] is protected.

   [export_thy thy prot]: Mark end of theory [thy], (with [end_theory
   thy prot]), and save theory to disk.
*)
val end_theory: thy -> bool -> unit
val export_theory: out_channel -> thy -> bool -> unit

(* [contents thy]
   Get contents of theory [thy]
*)
val contents : thy -> contents

val print_property: Printer.ppinfo -> property -> unit
val print_properties: Printer.ppinfo -> property list -> unit

val print: Printer.ppinfo -> thy -> unit
