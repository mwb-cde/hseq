(*----
  Name: thydb.mli
  Copyright M Wahab 2005-2010
  Author: M Wahab  <mwb.cde@googlemail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under
  the terms of the Lesser GNU General Public License as published by
  the Free Software Foundation; either version 3, or (at your option)
  any later version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
  License for more details.

  You should have received a copy of the Lesser GNU General Public
  License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
  ----*)

(** Theory databases 

    A theory database is a table of theories, indexed by theory names,
    the current theory and a list of theory names (the {e importing
    list}). The list of names is the scope of the current theory: each
    name is the name of an ancestor theory and occurs in the order it
    was imported. When the database is searched, the theories are
    searched in the order they appear in the importing list (unless
    the theory name is explicitly given).
*)

(** {5 Error Reporting} *)

class dbError: string -> string list ->
object
  inherit Report.error 
  val names: string list
  method get: unit -> string list
end
val error: string -> string list -> exn
val add_error: string -> string list -> exn -> 'a

(** {5 Databases} *)

type table_t = (Theory.thy)Treekit.StringTree.t

type thydb 
(** The type of theory databases.

    A database has a {e table} of theories, a {e current theory} and
    an {e importing list}.
*)

val empty: unit ->  thydb 
(** [empty thy]: Make a database with initial theory [thy], which is
    made the current theory. Fails if [thy] has parents.
*)

val table: thydb -> table_t
(** Get the table of theories. *)

val current: thydb -> Theory.thy
(** Get the current theory. @raise [Failure] if there is no current
    theory. *)

val imported: thydb -> string list
(** Get the names of the imported theories. *)

val thys: thydb -> Lib.StringSet.t
(** The names of the theories which are in scope. *)

val expunge: thydb -> thydb
(** Delete unused theories from the theory database. A theory is
    unused iff it is not in the importing list of the database.
*)

(** {5 Operations on Theories} *)

val current_name: thydb -> string
(** Get the name of the current theory. *)

val is_imported: string -> thydb -> bool
(** Test whether a theory is in the importing list (and therefore in
    scope).
*)

val is_loaded: string -> thydb -> bool 
(** Test whether a theory is loaded in the database (not necessarily
    in scope).
*)

val add_thy: thydb -> Theory.thy -> thydb
(** Add a theory to the table of theories. Fails if the theory is
    already present. Doesn't change the current theory.
*)

val remove_thy: thydb -> string -> thydb
(** Remove a theory from the table of theories. Fails if the theory is
    the current theory or in scope.
*)

val get_thy: thydb -> string -> Theory.thy
(** Get the theory. *)

val get_parents: thydb -> string -> string list
(** Get the parents of a theory. *)

val end_current: thydb -> bool -> thydb
(** Mark the current theory as closed, setting the date and read-only flag. *)

(** {5 Pretty Printer} *)

val print: thydb -> unit
(** Printer for databases. *)

(** {5 Operations on the current theory} *)

val set_current: thydb -> Theory.thy -> thydb
(** Set the current theory to the given theory. The theory is added to
    the table of theories if not already present. The importing list
    is rebuilt from the theory parents. Fails if any of the theorys'
    parents are not loaded.

    Use {!Thydb.Loader.make_current} instead.
*)

(** {7 Types} *)

val add_type_rec: Logic.Defns.cdefn -> thydb -> thydb
(** [add_type_rec r db]: Store type record [r] in the current theory.
*)

val get_type_rec: string -> string -> thydb -> Gtypes.typedef_record
(** [get_type_rec th n db]: Get the type definition of type named [n]
    in theory [th]
*)

val thy_of_type: string -> string -> thydb -> string 
(** [thy_of_type th n db]: Beginning with theory [th], try to find the
    theory containing a type declaration for name [n].
*)

(** {7 Definitions and Declarations} *)

val add_decln_rec: 
  Logic.Defns.cdefn -> Theory.property list
  -> thydb -> thydb
(** [add_decln_rec d ps db]: Store declaration [d] with properties
    [ps] in the current theory.
*)

val add_decln: 
  Logic.Defns.cdefn
  -> Theory.property list -> thydb -> thydb
(** [add_decln d ps db]: Store declaration [d] with properties [ps] in
    the current theory.
*)

val add_defn_rec: string-> Basic.gtype -> Logic.thm option 
  -> Theory.property list -> thydb -> thydb
(** [add_defn_rec n ty th ps db]: Store definition [th] of name [n],
    typed [ty] with properties [ps] in the current theory.
*)

val add_defn: 
  string -> Basic.gtype -> Logic.thm -> Theory.property list 
  -> thydb -> thydb
(** [add_defn n ty th ps db]: Store definition [th] of name [n], typed
    [ty] with properties [ps] in the current theory.
*)

val get_defn_rec: string -> string -> thydb -> Theory.id_record
(** [get_defn_rec n th db]: Get the definition of the term named [n]
    in theory [th].
*)

val get_defn: string -> string -> thydb -> Logic.thm
(** [get_defn n th db]: Get the definition of the term named [n] in
    theory [th].
*)

val get_id_type: string -> string -> thydb -> Basic.gtype
(** [get_defn n th db]: Get the type of the term named [n] in theory
    [th].
*)

val get_id_options: string -> thydb -> (Ident.t * Basic.gtype) list
(** [get_id_options n db]: Get list of term identifiers with the name
    [n], together with their types.  The list is in the order of
    appearence in the importing list.
*)

val id_exists: string -> string -> thydb -> bool
(** [id_exists th n db]: Test whether a definition for name [n] in
    theory [th] exists.
*)

val thy_of: string -> string -> thydb -> string 
(** [thy_of th n db]: Beginning with theory [th], try to find the
    theory containing a term declaration for name [n].
*)

(** {7 Theorems} *)

val add_axiom: 
  string -> Logic.thm -> Theory.property list -> thydb -> thydb
(** [add_axiom n th ps db]: Store axiom [th] under name [n] with
    properties [ps] in the current theory.
*)

val add_thm: 
  string -> Logic.thm -> Theory.property list -> thydb -> thydb
(** [add_thm n th ps db]: Store theorem [th] under name [n] with
    properties [ps] in the current theory.
*)

val get_axiom: string -> string -> thydb -> Logic.thm
(** [get_axiom th n db]: Get the axiom named [n] from theory [th]. If
    [th] is "", the theory used is the first in [db.importing] with an
    axiom named [n].
*)

val get_theorem: string -> string -> thydb -> Logic.thm
(** [get_theorem th n db]: Get the theorem named [n] from theory [th].
    If [th] is "", the theory used is the first in [db.importing] with
    a theorem named [n].
*)

val get_lemma: string -> string -> thydb -> Logic.thm
(** [get_lemma th n db]: Get the axiom, theorem or definition named
    [n] from theory [th].  If [th] is "", the theory used is the first
    in [db.importing] with an axiom, theorem or definition named [n].
*)

(** {7 Type Printer-Parser records} *)

val add_type_pp_rec: 
  string -> Printer.record -> thydb  -> thydb
(** [add_type_pp_rec n r db]: Add PP record [r] for type identifier
    [n] in the current theory.
*)

val get_type_pp_rec: 
  string  -> string -> thydb -> Printer.record
(** [get_type_pp_rec th n db]: Get the PP record [r] for type
    identifier [n] in theory [th].
*)

val remove_type_pp_rec: 
  string -> string -> thydb -> thydb
(** [remove_type_pp_rec th n db]: Remove the PP record [r] for type
    identifier [n] in theory [th].
*)

val get_type_pplist: 
  string -> thydb -> (Ident.t * Printer.record) list
(** [get_type_pplist n db]: Get the list of PP records for identifiers
    with name [n].
*)
  
(** {7 Term Printer-Parser records} *)

val add_term_pp_rec: 
  string -> (Printer.record * Theory.sym_pos) -> thydb  -> thydb
(** [add_term_pp_rec n r db]: Add PP record [r] for term identifier
    [n] in the current theory.
*)

val get_term_pp_rec: 
  string  -> string -> thydb -> (Printer.record * Theory.sym_pos)
(** [get_term_pp_rec th n db]: Get the PP record [r] for term
    identifier [n] in theory [th].
*)

val remove_term_pp_rec: string -> string -> thydb -> thydb
(** [remove_term_pp_rec th n db]: Remove the PP record [r] for term
    identifier [n] in theory [th].
*)

val get_term_pplist: 
  string -> thydb 
  -> (Ident.t * (Printer.record * Theory.sym_pos)) list
(** [get_term_pplist n db]: Get the list of PP records for identifiers
    with name [n].
*)

val add_pp_symbol: (string * string) -> thydb -> thydb
(** [add_pp_symbol (str, tok) db]: Add a symbol to the current theory *)

val get_pp_symbols: string -> thydb -> (string * string) list
(**[get_pp_symbols th db]: Get the PP symbols for theory [th] *)

(** {5 Files } *)

val add_file: string -> string -> thydb -> thydb
(** [add_file thyname filename]: add file [filename] to the list to be loaded
    for theory [thyname].
*)

val remove_file: string -> string -> thydb -> thydb
(** [remove_file thyname filename]: remove file [filename] from the list to be
    loaded for theory [thyname].
*)

(** {5 Scopes from databases} *)

val marker_in_scope: Scope.marker -> thydb -> bool
(** Test whether the theory identified by a marker is in the importing
    list (and therefore in scope).
*)

val mk_scope: thydb -> Scope.t
(** Make a scope from a theory database. *)

(** {5 Theory loader} *)

(** The theory loader.

    Loads theories from permanent storage, rebuilding them if
    necessary.
*)
module Loader:
sig

  (** Information about a theory passed to file-handling functions. *)
  type info =
      { 
	name: string; 
        (** The name of the theory. *)
	date: float option; 
        (** The maximum date of the theory (optional). *)
	prot: bool option ;
        (** Whether the theory is protected (optional). *)
	childn: Lib.StringSet.t 
      (** 
          Names of the theories of which name is a parent. 
          (If [name] is in [childn] then it is a circular importing.)
      *)
      }

  val mk_info: string -> float option -> bool option -> info
  (** Constructor for [info]. *)
  val info_add_child: info -> string -> info
  (** [info_add info n]: Add n to [info.childn]. *)

  (** Data needed for loading a theory. [load_fn] loads a theory
      from a file. [build_fn] constructs the theory (e.g. by
      running a script), if the theory file can't be
      found. [thy_fn] is applied to a successfully loaded theory
      and can be used to access data from theories as they are
      used.

      [load_fn info]: Load the theory named [info.name]. The theory
      should satisfy the contraints in [info] (e.g. of date and
      whether it is protected.)

      [build_fn db name]: Build the theory named [name], return the
      database with the newly built theory as the current
      theory. The result ({e db'}) of [build_fn db name] will be
      rejected if the theory ({e thy}) named [name] is not in the
      database, if any of the parents of [thy] are not in the
      importing list of [db'], if [thy] is not the first theory in
      the importing list or if any theory in the importing list of
      [db] is not in the table of [db'].
  *)
  type data = 
      {
	load_fn: info -> Theory.saved_thy;
	(** Function to find and load a theory file. *)
	build_fn: thydb -> string -> (thydb * Theory.thy list)
      (** Function to build the theory if it can't be loaded. The function
	  should take the database in which the theory is to be built and return
	  the database with the new theory as the current theory and the list of
	  all theories that the newly built theory depends on.  *)
      }

  val mk_data: 
    (info -> Theory.saved_thy)
    -> (thydb -> string -> (thydb * Theory.thy list))
    -> data
  (** Constructor for [data]. *)

  val mk_empty: unit -> data
  (** Make empty loader data. *)

   val make_current:  thydb -> data -> Theory.thy 
   -> (thydb * (Theory.contents list))
  (** [make_current db thy]: Load the parents of [thy] into [db] and
      make theory [thy] the current theory.  *)

  val load: thydb -> data -> info -> (thydb * (Theory.contents list))
  (** [load db info data]: Load a theory and, if neccessary, it parents from
      disc into the database. Make it the current theory. If no theory file is
      available, try to build it from a theory script. 

      Uses [data.load_fn] to try to load the theory and [data.build_fn] to try
      to build the theory. Returns the updated thydb and the contents of the
      loaded theories *)

  (** {7 Debugging information} *)

  val load_theory: 
    thydb -> data -> info -> (thydb * Theory.contents list)
  val load_parents: 
    thydb -> data -> info -> string list -> (thydb * Theory.contents list)


  val check_build: thydb -> thydb -> Theory.thy -> unit
  val set_curr: thydb -> Theory.thy -> thydb
  val test_protection: string -> bool option -> bool -> unit
  val test_date: string -> float option -> float -> unit

  val latest_time: float -> float -> float

  module New:
  sig
    val build_thy: 
      data -> thydb -> info -> (Theory.thy list * Theory.thy * thydb)
    val load_aux: data -> thydb -> info -> bool -> (Theory.thy * thydb)
    val load_theory:
      thydb -> data -> info -> (thydb * (Theory.contents list))

    val get_loaded_thy: data -> thydb -> Theory.thy list -> info 
      -> (Theory.thy list * Theory.thy * thydb)
    val get_saved_thy: data -> thydb -> Theory.thy list -> info
      -> (Theory.thy list * Theory.thy * thydb)
    val load_aux: data -> thydb -> Theory.thy list -> info
      -> (Theory.thy list * Theory.thy * thydb)
    val load_deps: data -> thydb -> Theory.thy list -> float -> info list
      -> (Theory.thy list * thydb * float)

    val load_theory: 
      thydb -> data -> info -> (thydb * Theory.thy * Theory.contents list)
    val load_parents: 
      thydb -> data -> info -> string list -> (thydb * Theory.contents list)

    val load_main: 
      thydb -> data -> info -> Theory.contents list
      -> (thydb * Theory.contents list)
    val load_parents_main: 
      thydb -> data -> info -> string list -> Theory.contents list
      -> (thydb * Theory.contents list)

    val load: 
      thydb -> data -> info
      -> (thydb * Theory.contents list)

    val load_parents:
      thydb -> data -> info -> string list
      -> (thydb * Theory.contents list)

  end

    (** For Debugging. *)
  val check_importing : info -> string -> unit
end

(** {5 Debugging information} *)

module NameSet:
sig
  type t = 
      {
        list: string list ; 
	set: Lib.StringSet.t
      }

  val empty: t
  val add: t -> string -> t 
  val mem: t -> string -> bool
  val filter: (string -> bool) -> t -> t
  val to_list: t -> string list
  val to_set: t -> Lib.StringSet.t
  val from_list: string list -> t

  val print: t -> unit
end

val add_importing: thydb -> string list -> thydb
val mk_importing: thydb -> NameSet.t

val get_thy: thydb -> string -> Theory.thy
val find: (Theory.thy -> 'a) -> thydb -> 'a
val quick_find: (Theory.thy -> 'a) -> string -> thydb -> 'a
