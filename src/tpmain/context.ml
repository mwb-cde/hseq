(*----
  Name: context.ml
  Copyright M Wahab 2012
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

open Basic
open Parser
open Lib.Ops

(** Default values. *)
module Default = 
struct

  (** {6 File handling} *)

  let load f = Report.warning ("Failed to load file "^f)
  let use ?silent f = Report.warning ("Failed to use file "^f)
  let build ?silent f = raise (Failure("Can't build theory "^f))

  (** {6 Theories} *)

  let empty_thy_name = Ident.null_thy
  let base_thy_name = "Main"
end

(** Global context *)
(***
module Context =
struct
***)

  (** File handling functions *)
  type file_t =
      {
        (** [load_file f]: Load a byte-code file [f] into memory. *)
        load_f: string -> unit;

        (** [use_file ?silent f]: Read file [f] as a script.  If
            [silent=true], do not report any information. *)
        use_f: ?silent:bool -> string -> unit;

        (** [build ?silent th]: Build theory [th] from a script.
            [silent=true], do not report any information.  @raise
            Failure on failure. *)
        build_f: ?silent:bool -> string -> unit;
        
        (** [path]: List of directories to search for theories,
            libraries and scripts.*)
        path_f: string list;

        (** obj_suffix: List of possible suffixes for an object file. *)
        obj_suffix_f: string list;

        (** thy_suffix: Suffix for a theory file. *)
        thy_suffix_f: string;

        (** script_suffix: Suffix for a script file. *)
        script_suffix_f: string;
      }

  let empty_file_t ()= 
    { 
      load_f = Default.load;
      use_f = Default.use;
      build_f = Default.build;
      path_f = [];
      obj_suffix_f = [];
      thy_suffix_f = "";
      script_suffix_f = "";
    }

  (** Theory data *)
  type thy_t =
    {
      (** Name of the theory on which all user theories are based *)
      base_name_f: string option;

      (** The theory data base. *)
      thydb_f: Thydb.thydb;

      (** Information needed for the theory database loader. *)
      loader_data_f: Thydb.Loader.data;
    }

  let empty_thy_t () =
    {
      base_name_f = None;
      thydb_f = Thydb.empty();
      loader_data_f = Thydb.Loader.mk_empty();
    }

  (** Printer info *)
  type pp_t =
      {
        pp_info_f: Printer.ppinfo ref;
      }

  let empty_pp_t () = 
    {
      pp_info_f = ref(Printer.empty_ppinfo())
    }

  (** Parser info *)
  type parser_t =
      {
        parser_info_f: Parser.Table.t ref;
      }

  let empty_parser_t () = 
    {
      parser_info_f = ref(Parser.Table.empty Parser.Table.default_size)
    }

  (** Top-level context *)
  type t = 
      {
        (** File handling functions *)
        file_f: file_t;

        (** Theory data *)
        thys_f: thy_t;

        (** Pretty Printer *)
        pp_f: pp_t;

        (** Parsers *)
        parser_f: parser_t;

        (** A list of functions to invoke on a theory when it is added
            to the data-base. *)
        load_functions_f: (t -> Theory.contents -> t) list;

        (** Theorems caches *)
        thm_cache: (Ident.t, Logic.thm) Hashtbl.t;
      }

  let empty() = 
    {
      file_f = empty_file_t();
      thys_f = empty_thy_t();
      pp_f = empty_pp_t();
      parser_f = empty_parser_t();
      load_functions_f = [];
      thm_cache = Hashtbl.create(13);
    }

  (** {6 Scoped contexts} *)
  type scoped = (t * Scope.t)
  (** The type of scoped contexts *)

  let scoped ctxt scp = (ctxt, scp)
  (** Constructor for scoped contexts *)
  let scope_of (_, scp) = scp
  (** Get the scope *)
  let context_of (ctxt, _) = ctxt

  (** Constructor for scoped contexts *)
  let set_scope sctxt scp = scoped (context_of sctxt) scp
  let set_context sctxt ctxt = scoped ctxt (scope_of sctxt)

  (** {5 Accessor Functions} *)

  (** {6 File handling} *)

  let set_load t f = 
    let file1 = {t.file_f with load_f = f} in
    { t with file_f = file1 }

  let load t = t.file_f.load_f

  let set_use t f = 
    let file1 = {t.file_f with use_f = f} in
    { t with file_f = file1 }
 
  let use t = t.file_f.use_f

  let set_build t f = 
    let file1 = {t.file_f with build_f = f} in
    { t with file_f = file1 }

  let build t = t.file_f.build_f

  let set_path t p = 
    let file1 = {t.file_f with path_f = p} in
    { t with file_f = file1 }

  let path t = t.file_f.path_f

  let set_obj_suffix t sl = 
    let file1 = {t.file_f with obj_suffix_f = sl} in
    { t with file_f = file1 }

  let obj_suffix t = t.file_f.obj_suffix_f

  let set_thy_suffix t sl = 
    let file1 = {t.file_f with thy_suffix_f = sl} in
    { t with file_f = file1 }

  let thy_suffix t = t.file_f.thy_suffix_f

  let set_script_suffix t sl = 
    let file1 = {t.file_f with script_suffix_f = sl} in
    { t with file_f = file1 }

  let script_suffix t = t.file_f.script_suffix_f

  (** {6 Theory handling} *)

  let set_base_name t n = 
    let thys1 = {t.thys_f with base_name_f = Some(n)}
    in 
    { t with thys_f = thys1 }

  let base_name t = 
    match t.thys_f.base_name_f with
      | Some(x) -> x
      | _ -> ""

  let has_base_name t = 
    not ((t.thys_f.base_name_f) = None)

  let clear_base_name t = 
    let thys1 = {t.thys_f with base_name_f = None}
    in 
    { t with thys_f = thys1 }

  let set_thydb t db =
    let thys1 = { t.thys_f with thydb_f = db }
    in 
    { t with thys_f = thys1 }

  let thydb t = t.thys_f.thydb_f

  let set_loader_data t lf =
    let thys1 = { t.thys_f with loader_data_f = lf }
    in 
    { t with thys_f = thys1 }

  let loader_data t = t.thys_f.loader_data_f

  let set_load_functions t fl =
    { t with load_functions_f = fl }

  let load_functions t = t.load_functions_f

  (** Pretty printer information *)
  let set_ppinfo t inf =
    t.pp_f.pp_info_f := inf; t

  let ppinfo t = !(t.pp_f.pp_info_f)

  (** Parser information *)
  let set_parsers t inf =
    t.parser_f.parser_info_f := inf; t

  let parsers t = !(t.parser_f.parser_info_f)

  (** Theorem cache *)
  let cache_thm t id thm = 
    if not (Hashtbl.mem t.thm_cache id)
    then
      (Hashtbl.add t.thm_cache id thm; t)
    else t

  let remove_cached_thm t id = 
    if Hashtbl.mem t.thm_cache id
    then
      (Hashtbl.remove t.thm_cache id; t)
    else t

  let lookup_thm sctxt id = 
    let ctxt = context_of sctxt 
    and scp = scope_of sctxt in
    let thm = Hashtbl.find ctxt.thm_cache id
    in 
    if Logic.is_fresh scp thm
    then thm
    else
      begin
        ignore(remove_cached_thm ctxt id);
        raise Not_found
      end
        
  let find_thm sctxt id fn =  
   try lookup_thm sctxt id
    with Not_found ->
      begin
        let thm = fn sctxt in 
        let _ = cache_thm (context_of sctxt) id thm
        in 
        thm
      end

(***
end
**)

module Thys =
struct

  (*
   * Theories
   *)

  let empty_thy_name = Ident.null_thy
  let anon_thy() = Theory.mk_thy empty_thy_name []

  let get_base_name ctxt = base_name ctxt
  let set_base_name ctxt x = set_base_name ctxt x 
  let clear_base_name ctxt = clear_base_name ctxt 

  (** The theory database *)
  let theories ctxt = thydb ctxt
  let get_theories = theories
  let set_theories thydb ctxt = set_thydb thydb ctxt
  let init_theories ctxt = set_theories ctxt (Thydb.empty())

  (** The current theory *)
  let current ctxt = Thydb.current (get_theories ctxt)
  let curr_theory= current
  let current_name ctxt = Theory.get_name (current ctxt)
  let set_current ctxt thy = 
    let thydb = Thydb.set_current (get_theories ctxt) thy in
    set_theories ctxt thydb
end

(** {5 File-Handling} *)

module Files =
struct 

  let get_cdir () = Sys.getcwd ()

  let load_use_file ?silent ctxt f =
    try
      if List.exists (Filename.check_suffix f) (obj_suffix ctxt)
      then load ctxt f
      else use ctxt f
    with 
      | Not_found -> Report.warning ("Can't find file "^f)
      | _ -> Report.warning ("Failed to load file "^f)

  (** {7 Paths} ***)

  let set_path = set_path
  let get_path = path
  let add_path ctxt x = set_path ctxt (x::(get_path ctxt))
  let remove_path ctxt x = 
    let pth = get_path ctxt in
    set_path ctxt (Lib.filter (fun y -> x = y) pth)

  let init_thy_path ctxt = 
    set_path ctxt ["."; Settings.thys_dir()]

  let get_thy_path ctxt = path ctxt
  let add_thy_path ctxt x = add_path ctxt x
  let set_thy_path ctxt x = set_path ctxt x
  let remove_from_path ctxt x = remove_path ctxt x

  let init_paths ctxt = init_thy_path ctxt

  (*** Theory files ***)

  let file_of_thy ctxt th = th^(thy_suffix ctxt)
  let script_of_thy ctxt th = th^(script_suffix ctxt)

  let find_file f path =
    let rec find_aux ths =
      match ths with
	| [] -> raise Not_found
	| (t::ts) ->
	  let nf = Filename.concat t f in 
	  if Sys.file_exists nf then nf 
	  else find_aux ts
    in 
    if Filename.is_relative f 
    then find_aux path
    else f

  let find_thy_file ctxt f =
    try find_file (file_of_thy ctxt f) (get_thy_path ctxt)
    with Not_found -> raise (Report.error ("Can't find theory "^f))

  (** [build_thy_file f]: build a theory by running script file f.  *)
  let build_thy_file ctxt thyname = build ctxt thyname

  (** [load_thy_file info]: Load the file storing the theory named
      [info.name] with protection [info.prot] and date no later than
      [info.date]. Finds the file from the path [get_thy_path()].  *)
  let load_thy_file ctxt info = 
    let test_protection prot b =
      match prot with 
	| None -> true
	| (Some p) -> p && b
    in 
    let test_date tym d = 
      match tym with 
	| None -> true
	| (Some tim) -> d <= tim
    in 
    let name = info.Thydb.Loader.name
    and date = info.Thydb.Loader.date
    and prot = info.Thydb.Loader.prot
    in 
    let thyfile =file_of_thy ctxt name
    in 
    let rec load_aux ths =
      match ths with
	| [] -> raise Not_found
	| (t::ts) ->
	  let filename = Filename.concat t thyfile
	  in 
	  if Sys.file_exists filename
	  then 
	    let sthy = Theory.load_theory filename
	    in 
	    if (test_protection prot (Theory.saved_prot sthy))
	      && (test_date date (Theory.saved_date sthy))
	    then sthy
	    else load_aux ts
	  else load_aux ts
    in 
    load_aux (get_thy_path ctxt)

  (** [load_use_theory thy]: Load or use each of the files named in
      theory [thy].  *)
  let load_use_theory_files ctxt thy = 
    let files = thy.Theory.cfiles in
    let path = get_thy_path ctxt in 
    let find_load f = load_use_file ctxt (find_file f path)
    in 
    List.iter find_load files

  (*** Theory inspection functions ***)

  (** [default_load_function]: The default list of functions to call
      on a newly-loaded theory.  *)
  let default_load_functions = 
    [
      load_use_theory_files;    (* load files *)
(*
      PP.add_loaded_type_pp;    (* add type PP information *)
      PP.add_loaded_term_pp;    (* add term PP information *)
*)
    ]

  let load_functions = ref default_load_functions
  let add_load_fn f = load_functions := (f::!load_functions)
  let init_load_functions () = 
    load_functions := default_load_functions

  (** [on_load_thy]: The toplevel function that is passed a newly
      loaded theory. This just passes the theory to the functions in
      [load_functions].  *)
  let on_load_thy db th =
    List.iter (fun f -> f (empty()) th) (List.rev !load_functions)

end
