(*----
  Name: global.ml
  Copyright M Wahab 2005-2010,2012
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

(******

open Basic
open Parser
open Lib.Ops

(** {7 Toplevel state} *)

let default_context() = 
  let ctxt = Context.empty() in
  let ctxt1 = Context.set_obj_suffix ctxt [".cmo"; "cmi"] in
  let ctxt2 = Context.set_script_suffix ctxt1 (Settings.script_suffix) in
  let ctxt3 = Context.set_thy_suffix ctxt2 (Settings.thy_suffix) in  
  ctxt3

(** Variables *)
let state_var = ref (default_context())

let state () = !state_var
let set_state ctxt = state_var := ctxt


(** Short cut to {!Thys.theories.} *)
let theories() = Context.Thys.get_theories (state())

(** Short cut to {!Thys.current.} *)
let current() = Context.Thys.current (state())

(** Short cut to {!Thys.current_name.} *)
let current_name () = Context.Thys.current_name (state())

(** The global scope. Constructed from the theory database. *)
let scope () = Thydb.mk_scope(theories())

(*** Pretty printing ***)
module PP =
struct

  (*** Printer tables ***)
  let info() = BoolPP.ppinfo()

(****
  (*** Parser tables ***)
  let sym_init() = Parser.init()
  let sym_info() = Parser.symtable()
  let sym_reset () = Parser.init()
***)
  (*** Terms ***)
  let get_term_pp id = Printer.get_term_info (info()) id

  (*** Types ***)

  let get_type_pp id = Printer.get_type_info (info()) id

  (*** User-defined printers ***)

  let get_term_printer id =
    Printer.get_term_printer (info()) id

  let add_term_printer id printer =
    ignore(Printer.add_term_printer (info()) id (printer (info())))

  let remove_term_printer id =
    ignore(Printer.remove_term_printer (info()) id)

  let get_type_printer id =
    Printer.get_type_printer (info()) id

  let add_type_printer id printer =
    ignore(Printer.add_type_printer (info()) id (printer (info())))

  let remove_type_printer id =
    ignore(Printer.remove_type_printer (info()) id)

  (** Functions to add PP information when a theory is loaded *)

  let add_id_record id rcrd = ()
  let add_type_record id rcrd = ()

(***
  let add_loaded_term_pp th =
    let thy_name = th.Theory.cname
    and pp_list = List.rev th.Theory.cid_pps
    in 
    let add_pp (id, (rcrd, pos)) = 
      add_id_record (Ident.mk_long thy_name id) rcrd;
      let repr = rcrd.Printer.repr
      in 
      match repr with
	| None -> ()
	| Some(sym) -> 
	  try
	    let id_record = List.assoc id th.Theory.cdefns in 
	    let id_type = id_record.Theory.typ
	    in 
	    ignore(Parser.add_overload sym pos
                     (Ident.mk_long thy_name id, id_type))
	  with _ -> ()
    in 
    List.iter add_pp pp_list

  let add_loaded_type_pp th =
    let thy_name = th.Theory.cname
    and pp_list = List.rev th.Theory.ctype_pps
    in 
    let add_pp (id, rcrd) = 
      add_type_record (Ident.mk_long thy_name id) rcrd
    in 
    List.iter add_pp pp_list
***)
      
  (*** Parsing ***)

  let catch_parse_error e a = 
    try (e a)
    with 
      | Parser.ParsingError x -> raise (Report.error x)
      | Lexer.Lexing _ -> raise (Report.error ("Lexing error: "^a))
        
(****
  let overload_lookup s = 
    let thydb s = Thydb.get_id_options s (theories())
    and parserdb s = Parser.get_overload_list s
    in 
    try parserdb s
    with Not_found -> thydb s

  let expand_term scp t = 
    let lookup = Pterm.Resolver.make_lookup scp overload_lookup in 
    let (new_term, env) = Pterm.Resolver.resolve_term scp lookup t
    in 
    new_term

  let expand_type_names scp t =  Gtypes.set_name ~strict:false scp t

  let expand_typedef_names scp t=
    match t with
      | Grammars.NewType (n, args) -> 
	Defn.Parser.NewType (n, args) 
      | Grammars.TypeAlias (n, args, def) ->
	Defn.Parser.TypeAlias(n, args, expand_type_names scp def)
      | Grammars.Subtype (n, args, def, set) ->
	Defn.Parser.Subtype(n, args, 
			    expand_type_names scp def, 
			    expand_term scp set)

  let expand_defn scp (plhs, prhs) =
    let rhs = expand_term scp prhs
    and ((name, ty), pargs) = plhs
    in 
    let args = List.map Pterm.to_term pargs
    in 
    (((name, ty), args), rhs)

  let mk_term scp pt = expand_term scp pt

  let read str= 
    mk_term (scope()) (catch_parse_error Parser.read_term str)

  let read_unchecked x =
    catch_parse_error (Pterm.to_term <+ Parser.read_term) x

  let read_defn x =
    let (lhs, rhs) = catch_parse_error (Parser.read defn_parser) x
    in 
    expand_defn (scope()) (lhs, rhs)

  let read_type_defn x =
    let pdefn = catch_parse_error (Parser.read Parser.typedef_parser) x
    in 
    expand_typedef_names (scope()) pdefn
      
  let read_type x = 
    expand_type_names (scope()) (catch_parse_error Parser.read_type x)

  let read_identifier x = 
    catch_parse_error (Parser.read Parser.identifier_parser) x
***)
end

(****
let read = PP.read
let read_type = PP.read_type
let read_identifier = PP.read_identifier
let read_defn = PP.read_defn
let read_type_defn = PP.read_type_defn

let mk_term t = PP.mk_term (scope()) t
****)

(*****
(** Convenience module, so that readers are available *)
module Read = 
struct
  let term = PP.read
  let ltype = PP.read_type
  let identifier = PP.read_identifier
  let defn = PP.read_defn
  let typedef = PP.read_type_defn
end
****)


******)
