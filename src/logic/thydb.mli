(*-----
Name: thydb.mli
   Author: M Wahab <Mwahab@Users.Sourceforge.Net>
   Copyright M Wahab 2005
   ----*)

(** Theory databases 

   A theory database is a mutable table of theories, indexed by theory
   names, the current theory and a list of theory names (the {e
   importing list}). The list of names is the scope of the current
   theory: each name is the name of an ancestor theory and occurs in
   the order it was imported. When the database is searched, the
   theories are searched in the order they appear in the importing
   list (unless the theory name is explicitly given).
 *)

(** {5 Error Reporting} *)

class dbError : string -> string list ->
  object
    inherit Result.error 
    val names : string list
    method get : unit -> string list
  end
val error : string -> string list -> exn
val add_error : string -> string list -> exn -> 'a

(** {5 Databases} *)

type thydb 
(** 
   The type of theory databases. 

   A database has a {e table} of theories, a {e current theory} and an
   {e importing list}.
 *)

val empty : unit ->  thydb 
(** 
   [empty thy]: Make a database with initial theory [thy], which is
   made the current theory. Fails if [thy] has parents.
 *)

val current : thydb -> Theory.thy
(** Get the current theory. Raises [Failure] if no current theory. *)

val imported : thydb -> string list
(** Get the names of the imported theories. *)

val thys : thydb -> Lib.StringSet.t
(** The names of the theories which are in scope. *)

(** {5 Operations on Theories} *)

val current_name: thydb -> string
(** Get the name of the current theory. *)

val is_imported : string -> thydb -> bool
(** Test whether a theory is in the importing list. *)

val thy_in_scope: string -> thydb -> bool
(**
   [thy_in_scope thy db]: test whether theory [thy] is in the importing list.
 *)

val is_loaded : string -> thydb -> bool 
(** 
   Test whether a theory is loaded in the database (not necessarily
   in scope). 
*)

val add_thy : thydb -> Theory.thy -> thydb
(** 
   Add a theory to the table of theories. Fails if the theory is
   already present. Doesn't change the current theory.
*)

val remove_thy : thydb -> string -> thydb
(** 
   Remove a theory from the table of theories. Fails if the theory is
   the current theory or in scope.
 *)

val get_thy : thydb -> string -> Theory.thy
(** Get the theory. *)

val get_parents : thydb -> string -> string list
(** Get the parents of a theory. *)

(** {5 Operations on the current theory} *)

val add_importing : thydb -> string list -> thydb
(** 
   Add a list of names to the front of the importing list. Duplicates are
   removed. Fails if any theory is not loaded into the database.
*)

(*
val mk_importing : thydb -> string list
(** Build the importing list of the current theory *)
*)

(*
val set_importing : thydb -> thydb
(** Build the importing list of the current theory. *)
*)

val set_current : thydb -> Theory.thy -> thydb
(** 
   Set the current theory to the given theory. The theory is added to
   the table of theories if it not already present. The importing list
   is rebuilt from the theorys' parents. Fails if any of the theorys'
   parents are not loaded.
*)

(** {7 Types} *)

val add_type_rec: Logic.Defns.cdefn -> thydb -> thydb
(** 
   [add_type_rec r db]: Store type record [r] in the current theory.
 *)

val get_type_rec: string -> string -> thydb -> Gtypes.typedef_record
(** 
   [get_type_rec th n db]
   Get the type definition of type named [n] in theory [th]
 *)

val thy_of_type: string -> string -> thydb -> string 
(** 
   [thy_of_type th n db]: Beginning with theory [th], try to find the
   theory containing a type declaration for name [n].
 *)

(** {7 Definitions and Declarations} *)

val add_decln_rec: 
    Logic.Defns.cdefn -> Theory.property list
      -> thydb -> thydb
(** 
   [add_decln_rec d ps db]: Store declaration [d] with properties [ps] in
   the current theory.
 *)
val add_decln: 
    Logic.Defns.cdefn
  -> Theory.property list -> thydb -> thydb
(** 
   [add_decln d ps db]: Store declaration [d] with properties [ps] in
   the current theory.
 *)

val add_defn_rec : string-> Basic.gtype -> Logic.thm option 
  -> Theory.property list -> thydb -> thydb
(** 
   [add_defn_rec n ty th ps db]: Store definition [th] of name [n], typed
   [ty] with properties [ps] in the current theory.
 *)

val add_defn : 
    string -> Basic.gtype -> Logic.thm -> Theory.property list 
      -> thydb -> thydb
(** 
   [add_defn n ty th ps db]: Store definition [th] of name [n], typed
   [ty] with properties [ps] in the current theory.
 *)

val get_defn_rec : string -> string -> thydb -> Theory.id_record
(** 
   [get_defn_rec n th db]: Get the definition of the term named [n] in
   theory [th].
*)

val get_defn : string -> string -> thydb -> Logic.thm
(** 
   [get_defn n th db]: Get the definition of the term named [n] in
   theory [th].
*)

val get_id_type : string -> string -> thydb -> Basic.gtype
(** 
   [get_defn n th db]: Get the type of the term named [n] in theory
   [th].
*)

val get_id_options : string -> thydb -> (Basic.ident * Basic.gtype) list
(**
   [get_id_options n db]: Get list of term identifiers with the name
   [n], together with their types.  The list is in the order of
   appearence in the importing list.
 *)

val id_exists : string -> string -> thydb -> bool
(** 
   [id_exists th n db]: Test whether a definition for name [n] in
   theory [th] exists.
 *)

val thy_of: string -> string -> thydb -> string 
(** 
   [thy_of th n db]: Beginning with theory [th], try to find the
   theory containing a term declaration for name [n].
 *)

(** {7 Theorems} *)

val add_axiom : 
    string -> Logic.thm -> Theory.property list -> thydb -> thydb
(** 
   [add_axiom n th ps db]: Store axiom [th] under name [n] with
   properties [ps] in the current theory.
 *)
val add_thm : 
    string -> Logic.thm -> Theory.property list -> thydb -> thydb
(** 
   [add_thm n th ps db]: Store theorem [th] under name [n] with
   properties [ps] in the current theory.
 *)

val get_axiom : string -> string -> thydb -> Logic.thm
(** 
   [get_axiom th n db]: Get the axiom named [n] from theory [th]. If
   [th] is "", the theory used is the first in [db.importing] with an
   axiom named [n].
*)
val get_theorem : string -> string -> thydb -> Logic.thm
(** 
   [get_theorem th n db]: Get the theorem named [n] from theory [th].
   If [th] is "", the theory used is the first in [db.importing] with
   a theorem named [n].
*)
val get_lemma : string -> string -> thydb -> Logic.thm
(** 
   [get_lemma th n db]: Get the axiom, theorem or definition named [n]
   from theory [th].
   If [th] is "", the theory used is the first in [db.importing] with
   an axiom, theorem or definition named [n].
*)

(** {7 Type Printer-Parser records} *)

val add_type_pp_rec: 
    string -> Printer.record -> thydb  -> thydb
(** 
   [add_type_pp_rec n r db]: Add PP record [r] for type identifier [n]
   in the current theory. 
 *)

val get_type_pp_rec: 
    string  -> string -> thydb -> Printer.record
(** 
   [get_type_pp_rec th n db]: Get the PP record [r] for type identifier [n]
   in theory [th]. 
 *)

val remove_type_pp_rec : 
    string -> string -> thydb -> unit
(** 
   [remove_type_pp_rec th n db]: Remove the PP record [r] for type
   identifier [n] in theory [th].
 *)

val get_type_pplist: 
    string -> thydb -> (Basic.ident * Printer.record) list
(** 
   [get_type_pplist n db]: Get the list of PP records for identifiers
   with name [n].
 *)

	
(** {7 Term Printer-Parser records} *)

val add_term_pp_rec: 
    string -> Printer.record -> thydb  -> thydb
(** 
   [add_term_pp_rec n r db]: Add PP record [r] for term identifier [n]
   in the current theory. 
 *)

val get_term_pp_rec: 
    string  -> string -> thydb -> Printer.record
(** 
   [get_term_pp_rec th n db]: Get the PP record [r] for term identifier [n]
   in theory [th]. 
 *)

val remove_term_pp_rec : string -> string -> thydb -> unit
(** 
   [remove_term_pp_rec th n db]: Remove the PP record [r] for term
   identifier [n] in theory [th].
 *)

val get_term_pplist: 
    string -> thydb -> (Basic.ident * Printer.record) list
(** 
   [get_term_pplist n db]: Get the list of PP records for identifiers
   with name [n].
 *)


(** {5 Scopes from databases} *)


val mk_scope: thydb -> Scope.t
(** Make a scope from a theory database. *)


(** {5 Theory loader} *)

(** The theory loader.

   Loads theories from permanent storage, rebuilding them if necessary.
 *)
module Loader :
    sig

    (** Information about a theory passed to file-handling functions. *)
    type info =
	{ 
	  name: string; 
(** The name of the theory *)
	  date : float option; 
(** The maximum date of the theory (ignored if [=0.0]) *)
	  prot : bool option  
(** Whether the theory is protected *)
	}

      val mk_info : string -> float option -> bool option -> info

      (** 
	 Data needed for loading a theory. [file_fn] constructs the
	 filename of the file to be loaded from the name of a
	 theory. [build_fn] constructs the theory (e.g. by loading a
	 script), if the theory file can't be found. [thy_fn] is
	 applied to a successfully loaded theory and can be used to
	 access data from theories as they are used.
       *)
      type data = 
	  {
	   thy_fn : (Theory.contents -> unit);
	   (** 
	      Function to apply to a successfully loaded theory.
	    *)
	 load_fn : (info -> Theory.thy);
	 (** Function to find and load a theory file. *)
	   build_fn: thydb -> string -> thydb
	       (** 
		  Function to build the theory if it can't be
		  loaded. The function should take the database in which
		  the theory is to be built and return the database with
		  the new theory as the current theory.
		*)
	 }

      val mk_data : 
	  (Theory.contents -> unit)
	  -> (info -> Theory.thy)
	    -> (thydb -> string -> thydb)
		-> data


      val load_theory : thydb -> data -> info -> thydb
(**
   [load_theory db n data]

   Load a theory from disc into the database.
   Return the list of importings for the new theory.
 *)

      val make_current :  thydb -> data -> Theory.thy -> thydb
(** 
   [make_current db thy]: Make theory [thy] the current theory, 
   making sure that all the parents of [thy] are loaded.
*)

(** {7 Debugging information} *)

      val load_parents : thydb -> data -> info -> string list -> thydb
	  
    end



(** {5 Debugging information} *)
