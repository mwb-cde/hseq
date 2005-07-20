(*-----
 Name: theory.mli
 Author: M Wahab <Mwahab@Users.Sourceforge.Net>
 Copyright M Wahab 2005
----*)

(** Theories.

   A theory is a record of term and type declarations and definitions
   theorems. A theory can have parents, provide an additional source
   of declarations, definition and theorems. A theory must be younger
   then all of its parents. A theory to which additions can be made is
   {e writable} and none of the parents of a theory can be writable. A
   {e protected} theory is not writable. A theory also stores a,
   possibly empty, list of files to be loaded when the theory is first
   used.

   A theorem (or definition) stored in a theory can have {e
   properties}, to indicate that it should be used by some proof
   tool. Each proof tool should have its own unique property. The
   record of the theorem can be interogated at very points during the
   program execution, to allow the different proof tools to find the
   theorems to use. A property [simp_property] is provided, to mark
   theorems to be used by the simplifier.

   Theories are not concerned with loading files and parent theories
   or passing information to proof tools. They are only used to store
   information.
*)

(** {5 Identifier and theorem records} *)

type property = string
(**
   [type property]: Properties of theorems. Properties are string
   to allow arbitrary new properties to be declared.
*)

val simp_property: property
(**
   Indicate that the theorem is of interest to the simplifier.
*)

(** Records of identifier declarations and definitions. *)
type id_record =
    {
     typ: Basic.gtype;   (** The type of the identifier *)
     def: Logic.thm option;  (** The optional definition *)
     dprops : property list  (** Properties of the identifier *)
   }

(** Records of theorems *)
type thm_record =
    {
     thm: Logic.thm;   (** The theorem *)
     props: property list (** Properties of the theorem *)
   }

(** {5 Theory Representation} *)

(**
   The type of theories. A theory has a name and zero or more parent
   theories if a theory is protected then it is not writable
 *)
type thy

(**
   Information stored in a theory.
   
   Used to directly query parts of a theory (such as for printing or
   setting up proof tools) without being able to alter the theory.
*)
type contents=
    {
     cname: string;     (** The theory name *)
     cprotection: bool;  (** Whether it is protected *)
     cdate: float;       (** The date of the theory *)
     cparents: string list;   (** The parents *)
     cfiles: string list;     (** Files to load *)
     caxioms: (string * thm_record) list;
     (** Axioms *)
     ctheorems: (string * thm_record) list;
     (** theorems *)
     cdefns: (string * id_record) list;
     (** Term definitions and declarations *)
     ctyps: (string * Gtypes.typedef_record) list;
     (** Type definitions and declarations *)
     ctype_pps: (string * Printer.record) list;
     (** Type printer and parser information *)
     cid_pps: (string * Printer.record) list 
     (** Term printer and parser information *)
   }

(** {7 Basic Theory Operations} *)

val mk_thy : string -> thy
(** Make a named theory. *)

val contents : thy -> contents
(** Get the contents of a theory. *)

val get_name : thy -> string
(** Get the name of a theory. *)
val get_date : thy -> float
(** Get the date of a theory. *)
val get_parents: thy -> string list
(** Get the parents of a theory *)
val get_protection : thy -> bool
(** Get the protection of a theory. *)
val get_files: thy -> string list
(** Get the files of a theory. *)

val add_parents: string list -> thy -> unit
(** Add parents to a theory *)

val set_protection : thy -> unit
(** Set the protection of a theory. *)

val set_files: string list -> thy -> unit
(** Set the files of a theory *)
val add_file: string -> thy -> unit
(** Add a file to the files of a theory. *)
val remove_file: string -> thy -> unit
(** Remove a file from a theory. *)

(** {5 Theory Components} *)

(** {7 Axioms and theorems} *)

val get_axiom_rec : string -> thy -> thm_record
(** Get an axioms' record, raise [Not_found] on failure. *)
val get_axiom : string -> thy -> Logic.thm
(** Get an axiom, raise [Not_found] on failure. *)

val add_axiom : string -> Logic.thm -> property list -> thy -> unit
(** Add an axiom to a theory. *)
val set_axiom_props: string -> property list -> thy -> unit
(** Set the properties of an axiom. *)

val get_theorem_rec : string -> thy -> thm_record
(** Get a theorems' record, raise [Not_found] on failure. *)
val get_theorem : string -> thy -> Logic.thm
(** Get a theorem, raise [Not_found] on failure. *)
val add_thm : string -> Logic.thm -> property list -> thy -> unit
(** Add a theorem to a theory, raise [Not_found] on failure. *)
val set_theorem_props: string -> property list -> thy -> unit
(** Set the properties of a theorem. *)

(** {7 Type declarations and definitions} *)

val get_type_rec : string -> thy -> Gtypes.typedef_record 
(** Get the record of a type, raise [Not_found] on failure. *)
val add_type_rec : Logic.Defns.cdefn -> thy -> unit
(** Add a type declaration or definition. *)
val type_exists : string -> thy -> bool
(** Test whether a type exists in the theory. *)

(** {7 Term declarations and definitions} *)

val get_defn_rec : string -> thy -> id_record
(** Get the definition record of a term, raise [Not_found] on failure. *)
val get_defn : string -> thy -> Logic.thm
(** Get the definition of a term, raise [Not_found] on failure. *)
val get_id_type : string -> thy -> Basic.gtype
(** Get the type of an identifier, raise [Not_found] on failure. *)
val id_exists : string -> thy -> bool
(** Test whether an identifier is declared in the theory. *)
val set_defn_props: string -> property list -> thy -> unit
(** Set the properties of a definition. *)

val add_defn_rec :
   string -> Basic.gtype 
      -> Logic.thm option 
	-> property list -> thy -> unit
(** Add a term definition record. *)

val add_decln_rec : 
    string  -> Basic.gtype -> property list -> thy -> unit
(** Add the declaration of a term. *)

(** {7 Printer-Parser records} *)

val get_term_pp_rec: 
    string  -> thy -> Printer.record
(** Get a term Printer-Parser record to a theory. *)
val add_term_pp_rec: 
    string -> Printer.record
      -> thy -> unit
(** Add a term Printer-Parser record to a theory. *)
val remove_term_pp_rec : string -> thy -> unit
(** Remove term Printer-Parser record to a theory. *)
val get_term_pplist: 
    thy -> (Basic.ident * Printer.record) list
(** Get all term Printer-Parser records of a theory. *)

val get_type_pp_rec: 
    string  -> thy -> Printer.record
(** Get a type Printer-Parser record to a theory. *)
val add_type_pp_rec: 
    string -> Printer.record
      -> thy -> unit
(** Add a type Printer-Parser record to a theory. *)
val remove_type_pp_rec : string -> thy -> unit
(** Remove type Printer-Parser record to a theory. *)
val get_type_pplist: 
    thy -> (Basic.ident * Printer.record) list
(** Get all type Printer-Parser records of a theory. *)

(** {5 Theory Storage} *)

(** {7 Primitive input/output of theories} *)

(** The representation of an identifier record for disk storage. *)
type id_save_record =
    {
     sty: Basic.gtype;
     sdef: Logic.saved_thm option;
     sdprops: property list
   }

val to_save : id_record -> id_save_record
(** Convert identifier record to permanent storage record. *)
val from_save : id_save_record -> id_record
(** Convert identifier record from permanent storage record. *)

(** The representation of a theorem record for disk storage. *)
type thm_save_record =
    {
     sthm: Logic.saved_thm;
     sprops: property list
   }

val thm_to_save : thm_record -> thm_save_record
(** Convert theorem record to permanent storage record. *)
val thm_from_save : thm_save_record -> thm_record
(** Convert theorem record from permanent storage record. *)

val output_theory : out_channel -> thy -> unit
(** 
   Convert a theory to permanent storage representation and emit to an
   output channel.
*)
val input_theory : in_channel -> thy
(** 
   Convert a theory from the permanent storage representation read from 
   an channel.
*)

(** {7 Toplevel input/output of theories} *)

val load_theory : string -> thy
(** Load a theory from disc *)
val save_theory: thy -> bool -> string -> unit
(** Save a theory to disc *)

val end_theory: thy -> bool -> unit
(**
   [end_theory thy prot]: End theory [thy].

   Update date of theory, set protection to [prot].
   Note that once set, theory protection can't be removed.
   Does nothing if theory [thy] is protected.
*)

val export_theory: out_channel -> thy -> bool -> unit
(**
   [export_thy thy prot]: Mark end of theory [thy], (with [end_theory
   thy prot]), and save theory to disk.
*)

(** {5 Pretty-Printer} *)

val print_property: Printer.ppinfo -> property -> unit
(** Print a property. *)
val print_properties: Printer.ppinfo -> property list -> unit
(** Print a list of property. *)

val print: Printer.ppinfo -> thy -> unit
(** Print a theory. *)


