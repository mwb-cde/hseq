(*-----
   Name: global.mli
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)

(** The global environment *)


(** {5 Theories} *)
module Thys:
    sig

      val empty_thy_name : string
(** The name of the anonymous theory *)

      val anon_thy : unit -> Theory.thy
(** Make an anonymous theory *)

      val base_name : (string)option ref
(** The name of the base theory [[default: base]] *)

      val get_base_name: unit -> string
(** 
   get the name of the base theory. Raise Not_Found if no base theory
   loaded or set.
 *)

      val set_base_name: string -> unit
(**
   [set_base_name s]: set the name of the base theory to [s]
 *)

      val clear_base_name: unit -> unit
(** clear the base name. *)

(** {5 The theory database} *)

      val theoryDB : Thydb.thydb ref
(** The theory database *)

(** get/set theoryDB and current theory *)
      val get_theories : unit -> Thydb.thydb
      val set_theories : Thydb.thydb -> unit

      val current : unit -> Theory.thy
      val current_name : unit -> string
      val set_current : Theory.thy -> unit
    end

(** {7 Toplevel theory functions} *)

val theories: unit -> Thydb.thydb
(** The theory database *)
val current: unit -> Theory.thy
(** The current theory. Raises Not_found if no theory set as current. *)
val current_name: unit -> string
(** 
   The name of the current theory. Raises Not_found if no theory set
   as current. 
 *)
val scope : unit -> Scope.t
(** The global scope. *)


(** {5 Pretty Printer} *)
module PP:
    sig

      val tp_pp_info: Printer.ppinfo ref
(**
   [tp_pp_info]: The system PP information store (of size
   Printer.default_info_size).
 *)

      val info : unit -> Printer.ppinfo
(** [info()]: Get the system PP information store *)


      val pp_reset: unit -> unit
(** [pp_reset()]: Reset the system PP information store *)

      val pp_set : Printer.ppinfo -> unit
(** [pp_set info]: Set the system PP information store to [info] *)


(** PP information for terms *)

(*
   [get_term_pp id]
   get parsing and pretty printing information for identifer occuring 
   in a term.
   @param id identifier to look up.

   @return [(prec, fixity, repr)]
   where 
   [prec] is precedence
   [fixity] is fixity
   [repr] is representation to use (if any)

   @return [(default_term_prec, default_term_fixity, None)] if id is not found.
 *)
      val get_term_pp : Basic.ident -> (int * Printer.fixity * string option)   

(**
   [add_term_pp id prec fixity repr]
   add parsing and pretty printing information for identifer 
   occuring in a term.
   @param id identifier to add.
   @param prec precedence.
   @param fixity fixity.
   @param repr representation (if any).
 *)
      val add_term_pp : 
	  Basic.ident -> int -> Printer.fixity 
	    -> string option -> unit

(**
   [add_term_pp_record id rcrd]
   add parsing and pretty printing record for identifer occuring in a term.
   @param id identifier to add.
   @param rcrd PP record
 *)
      val add_term_pp_record : 
	  Basic.ident -> Printer.record -> unit

(**
   [remove_term_pp id]
   remove parsing and pretty printing information for identifer 
   occuring in a term.
   @param id identifier to remove.
   @param prec precedence.
   @param fixity fixity.
   @param repr representation (if any).
 *)
      val remove_term_pp : Basic.ident -> unit

(**
   [get_type_pp id]
   get parsing and pretty printing information for identifer occuring 
   in a type.
   @param id identifier to look up.

   @return [(prec, fixity, repr)]
   where 
   [prec] is precedence
   [fixity] is fixity
   [repr] is representation to use (if any)
 *)
      val get_type_pp : Basic.ident -> (int * Printer.fixity * string option)

(**
   [add_type_pp id prec fixity repr]
   add parsing and pretty printing information for identifer 
   occuring in a type.
   @param id identifier to add.
   @param prec precedence.
   @param fixity fixity.
   @param repr representation (if any).
 *)

      val add_type_pp : 
	  Basic.ident -> int -> Printer.fixity -> string option -> unit

(**
   [add_type_pp_record id rcrd]
   add parsing and pretty printing record for identifer occuring in a type.
   @param id identifier to add.
   @param rcrd PP record
 *)
      val add_type_pp_record : 
	  Basic.ident -> Printer.record -> unit

(**
   [remove_type_pp id]
   remove parsing and pretty printing information for identifer 
   occuring in a type.
   @param id identifier to remove.
   @param prec precedence.
   @param fixity fixity.
   @param repr representation (if any).
 *)
      val remove_type_pp : Basic.ident -> unit

(**
   [get/add/remove_term_printer]

   get/add/remove a term printer
 *)
      val get_term_printer:
	  Basic.ident -> 
	    (Printer.fixity * int 
	     -> (Basic.term * (Basic.term list)) Printer.printer)
      val add_term_printer : 
	  Basic.ident -> 
	    (Printer.ppinfo 
	     -> (Printer.fixity * int) 
	       -> (Basic.term * (Basic.term list)) Printer.printer) -> unit
      val remove_term_printer : Basic.ident -> unit

(**
   [get/add/remove_type_printer]

   get/add/remove a type printer
 *)
      val get_type_printer:
	  Basic.ident 
	-> (Printer.fixity * int 
	    -> (Basic.ident * (Basic.gtype list)) Printer.printer)
      val add_type_printer : 
	  Basic.ident -> 
	    (Printer.ppinfo 
	     -> (Printer.fixity * int) 
	       -> (Basic.ident * (Basic.gtype list)) Printer.printer) -> unit
      val remove_type_printer : Basic.ident -> unit

    end


(** {5 File-Handling} *)

(** Filenames and paths for theory files *)
module Files : 
    sig

      val thy_suffix: string
	  (** 
	     The suffix to add to the name of a theory to get the
	     name of its data file. 
	   *)

(** {7 Paths} *)

      val get_thy_path : unit -> string list
(** The path for theory files. *)
      val add_thy_path : string -> unit
(** Add a directory to the theory path. *)
      val set_thy_path: string list -> unit
(** Set the theory path. *)
      val remove_from_path: string -> unit
(** Remove a directory from the theory path. *)

      val get_cdir : unit -> string
(** The current working directory. *)

      val find_file : string -> string
(** 
   [find_file x]: Find file [x] in the theory path. 
   raise [Not_found] if not found
 *)

      val find_thy_file: string -> string
(** Find the a theory file. *)

(** {7 Theory loading and building} *)

      val build_thy_file: Thydb.thydb -> string -> Thydb.thydb
(** Function to build a theory from a script. *)
      val load_thy_file: Thydb.Loader.info -> Theory.saved_thy
(** Function to load a theory from a file. *)

(* Functions to invoke when loading theories from disk *)
      val load_use_theory_files: Theory.contents -> unit
      val load_functions : (Theory.contents -> unit) list ref
      val init_load_functions: unit -> unit
      val add_load_fn : (Theory.contents -> unit) -> unit
      val on_load_thy: Theory.contents -> unit

      val loader_data : Thydb.Loader.data
(** Default information needed for the theory database loader. *)
    end

(** {7 Toplevel file functions} *)
val get_thy_path: unit -> string list
val add_thy_path: string -> unit


(** Parsing 

   Parsing with error handling.  All calls to the parser should go
   through here.
 *)
module Parser:
    sig

      val expand_term : 
	  Scope.t -> Basic.term -> Basic.term
      val expand_type_names: 
	  Scope.t -> Basic.gtype -> Basic.gtype
      val expand_typedef_names: 
	  Scope.t -> Parser.typedef_data -> Parser.typedef_data

      val mk_term : Scope.t -> Basic.term -> Basic.term
(** [mk_term scp trm]: typecheck term [trm] in scope [scp] *)
      val mk_term_raw : Scope.t -> Basic.term -> Basic.term
      val mk_term_unchecked : Scope.t -> Basic.term -> Basic.term

      val read : string -> Basic.term
      val read_unchecked : string -> Basic.term
      val read_defn :
	  string -> (string * (string * Basic.gtype) list) * Basic.term
      val read_type_defn : string -> Parser.typedef_data

      val read_type : string -> Basic.gtype
      val read_fulltype : string -> Basic.gtype
      val read_identifier: string -> Basic.ident

    end


(** {5 Initialising functions} *)

module Init:
    sig

(*
   [load_base_thy()]
   try to load the base theory 
   if successful:
   make it the current theory.
   set the base theory name ([set_base_name(...)])

   if unsuccessful:
   use an empty theory as the current theory.
   if [!base_thy_builder=Some(f)] 
   then call [f]
   otherwise clear the base theory name ([clear_base_name()])

   [get_base_thy_builder()]
   [set_base_thy_builder f]

   Set/get value of base_thy_builder.
 *)
      val load_base_thy: unit -> unit

      val set_base_thy_builder : (unit -> unit) -> unit
(** Set the function to use if the base theory must be rebuilt. *)
      val get_base_thy_builder : unit -> (unit -> unit) option
(** Get the function to use if the base theory must be rebuilt. *)

      val init_theoryDB : unit -> unit

(* list of initialising functions *)
      val init_list: (unit -> unit) list ref
      val add_init: (unit-> unit) -> unit

(* function to call to initialise system *)
      val init : unit -> unit

(* list of reset functions *)

      val reset_list: (unit -> unit) list ref
      val add_reset: (unit-> unit) -> unit

(**
   reset(): function to call to reset system 
   call all functions in reset_list then
   call init()
*)
      val reset : unit -> unit
    end

(** {7 Toplevel initialising functions} *)


val init : unit -> unit
(** Initialise the system *)
val reset : unit -> unit
(** Reset the system. (This is the same as init()). *)
